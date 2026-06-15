# risk_uncertainty.R
# -----------------------------------------------------------------------------
# Foundation layer for V2.5: #6 Uncertainty Quantification + #4 Risk Prediction.
#
# These are pure post-processing over the Monte Carlo OUTPUT distribution -- no
# new sampling. They read the per-iteration `summary` and `resource_utilization`
# tibbles returned by simulate_campaign_detailed() and answer:
#   #6  How likely are we to finish by a target / under a budget? What is the
#       P10/P50/P90 spread? How often is a resource overloaded?
#   #4  For each resource/failure mode: probability of being the binding
#       constraint, the expected schedule delay it contributes, and its impact.
#
# Design notes
# ------------
# * Group by operation_mode throughout (matches summarise_* house style).
# * Resource names and cost-rate defaults match build_cost_impact() exactly.
# * "Binding" per iteration = the resource with the highest
#   fleet_days_after_resources (the critical-path resource that iteration).
# * "Expected delay" for a resource = mean excess of its fleet-days over the
#   next-tightest resource = the schedule days that would be recovered if that
#   single constraint were relieved. This is the quantity recommendations and
#   the narrative engine (#1, #12) consume.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
})

.RU_COST_RATES <- c(
  "Frac fleet"    = 250000,
  "Wireline"      = 15000,
  "CT / cleanout" = 25000,
  "Milling"       = 18000,
  "Testing unit"  = 12000
)

# Human-readable risk label per resource, plus an impact description.
.RISK_LABELS <- c(
  "Frac fleet"    = "Frac fleet binding",
  "Wireline"      = "Wireline backlog",
  "CT / cleanout" = "CT / cleanout backlog",
  "Milling"       = "Milling backlog",
  "Testing unit"  = "Testing bottleneck"
)
.RISK_IMPACT <- c(
  "Frac fleet"    = "Frac pace governs the campaign; add fleet capacity or accelerate cycle time.",
  "Wireline"      = "Wireline readiness gates frac; add a wireline unit or run SCMT offline.",
  "CT / cleanout" = "CT prep/cleanout paces wells; add CT capacity or offload to milling support.",
  "Milling"       = "Plug milling queues post-frac; add a milling unit or enable CT support.",
  "Testing unit"  = "Testing/flowback unit shared with milling; add a testing unit."
)

.second_max <- function(x) {
  x <- sort(x[!is.na(x)], decreasing = TRUE)
  if (length(x) >= 2) x[2] else if (length(x) == 1) 0 else 0
}

.conf_band <- function(p) dplyr::case_when(
  p >= 0.90 ~ "High",
  p >= 0.70 ~ "Moderate",
  p >= 0.50 ~ "Low",
  TRUE      ~ "Unlikely"
)

# ===========================================================================
# #6  UNCERTAINTY QUANTIFICATION
# ===========================================================================
# Returns one row per operation_mode. target_days / budget are optional; the
# probabilities that depend on them are NA when not supplied.
quantify_uncertainty <- function(
    summary,
    resource_utilization = NULL,
    target_days = NULL,
    budget = NULL,
    util_threshold = 0.90,
    frac_fleet_cost_per_day = 250000,
    wireline_cost_per_day   = 15000,
    ct_cost_per_day         = 25000,
    milling_cost_per_day    = 18000,
    testing_unit_cost_per_day = 12000
) {
  if (is.null(summary) || nrow(summary) == 0) return(tibble())
  rates <- c("Frac fleet" = frac_fleet_cost_per_day, "Wireline" = wireline_cost_per_day,
             "CT / cleanout" = ct_cost_per_day, "Milling" = milling_cost_per_day,
             "Testing unit" = testing_unit_cost_per_day)

  # Per-mode spread rate ($/day for the whole fleet) from resource_utilization.
  spread <- NULL
  if (!is.null(resource_utilization) && nrow(resource_utilization) > 0) {
    spread <- resource_utilization %>%
      group_by(operation_mode, resource) %>%
      summarise(units = mean(units, na.rm = TRUE), .groups = "drop") %>%
      mutate(day_cost = units * unname(rates[resource])) %>%
      group_by(operation_mode) %>%
      summarise(spread_rate_per_day = sum(day_cost, na.rm = TRUE), .groups = "drop")
  }

  # Per-iteration cost (mobilisation + frac-idle penalty), if we can price it.
  cost_tbl <- NULL
  if (!is.null(spread)) {
    cost_tbl <- summary %>%
      select(operation_mode, simulation_id, estimated_campaign_days,
             total_wireline_readiness_delay_days, frac_fleets) %>%
      left_join(spread, by = "operation_mode") %>%
      mutate(campaign_cost = spread_rate_per_day * estimated_campaign_days +
               total_wireline_readiness_delay_days * frac_fleet_cost_per_day * frac_fleets)
  }

  # Per-iteration max utilization across resources (for overload probability).
  overload <- NULL
  if (!is.null(resource_utilization) && nrow(resource_utilization) > 0) {
    overload <- resource_utilization %>%
      group_by(operation_mode, simulation_id) %>%
      summarise(max_util = max(utilization, na.rm = TRUE), .groups = "drop") %>%
      group_by(operation_mode) %>%
      summarise(prob_resource_overload = mean(max_util > util_threshold), .groups = "drop")
  }

  out <- summary %>%
    group_by(operation_mode) %>%
    summarise(
      n_simulations = n(),
      p10_days = as.numeric(quantile(estimated_campaign_days, 0.10, na.rm = TRUE)),
      p50_days = as.numeric(quantile(estimated_campaign_days, 0.50, na.rm = TRUE)),
      p90_days = as.numeric(quantile(estimated_campaign_days, 0.90, na.rm = TRUE)),
      mean_days = mean(estimated_campaign_days, na.rm = TRUE),
      sd_days   = sd(estimated_campaign_days, na.rm = TRUE),
      prob_finish_by_target = if (is.null(target_days)) NA_real_
                              else mean(estimated_campaign_days <= target_days),
      prob_schedule_overrun = if (is.null(target_days)) NA_real_
                              else mean(estimated_campaign_days > target_days),
      expected_overrun_days = if (is.null(target_days)) NA_real_
                              else mean(pmax(estimated_campaign_days - target_days, 0)),
      p90_overrun_days = if (is.null(target_days)) NA_real_
                         else as.numeric(quantile(pmax(estimated_campaign_days - target_days, 0), 0.90, na.rm = TRUE)),
      .groups = "drop"
    )

  if (!is.null(cost_tbl)) {
    cost_agg <- cost_tbl %>%
      group_by(operation_mode) %>%
      summarise(
        p50_cost = as.numeric(quantile(campaign_cost, 0.50, na.rm = TRUE)),
        p90_cost = as.numeric(quantile(campaign_cost, 0.90, na.rm = TRUE)),
        prob_within_budget = if (is.null(budget)) NA_real_ else mean(campaign_cost <= budget),
        .groups = "drop"
      )
    out <- left_join(out, cost_agg, by = "operation_mode")
  } else {
    out <- out %>% mutate(p50_cost = NA_real_, p90_cost = NA_real_, prob_within_budget = NA_real_)
  }

  if (!is.null(overload)) out <- left_join(out, overload, by = "operation_mode")
  else out <- out %>% mutate(prob_resource_overload = NA_real_)

  out %>%
    mutate(
      target_days = if (is.null(target_days)) NA_real_ else target_days,
      budget = if (is.null(budget)) NA_real_ else budget,
      util_threshold = util_threshold,
      uncertainty_note = sprintf(
        "P50 %.0f d (P10 %.0f / P90 %.0f).%s%s",
        p50_days, p10_days, p90_days,
        if (is.null(target_days)) "" else sprintf(" P(finish by %d d) = %.0f%%.", as.integer(target_days), 100 * prob_finish_by_target),
        if (is.null(budget) || all(is.na(prob_within_budget))) "" else sprintf(" P(under budget) = %.0f%%.", 100 * prob_within_budget)
      )
    ) %>%
    arrange(operation_mode)
}

# ===========================================================================
# #4  RISK PREDICTION
# ===========================================================================
# One row per resource/failure mode per operation_mode:
#   probability        = P(this resource is the binding constraint per iteration)
#   expected_delay_days= mean schedule days attributable to it (excess over the
#                        next-tightest resource); unconditional over iterations
#   p90_delay_days     = 90th pct of that contribution
#   impact             = operator-facing description
# Plus derived rows: schedule overrun (if target_days) and frac-fleet idle.
predict_campaign_risks <- function(summary, resource_utilization, target_days = NULL) {
  if (is.null(resource_utilization) || nrow(resource_utilization) == 0) return(tibble())

  per_iter <- resource_utilization %>%
    group_by(operation_mode, simulation_id) %>%
    mutate(
      max_fd    = max(fleet_days_after_resources, na.rm = TRUE),
      second_fd = .second_max(fleet_days_after_resources),
      is_binding = fleet_days_after_resources >= max_fd - 1e-9,
      excess_days = ifelse(is_binding, pmax(fleet_days_after_resources - second_fd, 0), 0)
    ) %>%
    ungroup()

  resource_risks <- per_iter %>%
    group_by(operation_mode, resource) %>%
    summarise(
      probability = mean(is_binding),
      expected_delay_days = mean(excess_days),
      mean_delay_when_binding = sum(excess_days) / pmax(sum(is_binding), 1),
      p90_delay_days = as.numeric(quantile(excess_days, 0.90, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(
      risk   = unname(.RISK_LABELS[resource]),
      impact = unname(.RISK_IMPACT[resource])
    ) %>%
    select(operation_mode, risk, resource, probability, expected_delay_days,
           mean_delay_when_binding, p90_delay_days, impact)

  # Frac-fleet idle waiting on wireline (a distinct "starvation" signal).
  frac_idle <- summary %>%
    group_by(operation_mode) %>%
    summarise(
      probability = mean(total_wireline_readiness_delay_days > 0),
      expected_delay_days = mean(total_wireline_readiness_delay_days),
      mean_delay_when_binding = sum(total_wireline_readiness_delay_days) /
        pmax(sum(total_wireline_readiness_delay_days > 0), 1),
      p90_delay_days = as.numeric(quantile(total_wireline_readiness_delay_days, 0.90, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(
      risk = "Frac fleet idle (waiting on wireline)",
      resource = "Frac fleet",
      impact = "Frac crew on standby while wireline completes readiness; costs idle spread."
    ) %>%
    select(operation_mode, risk, resource, probability, expected_delay_days,
           mean_delay_when_binding, p90_delay_days, impact)

  risks <- bind_rows(resource_risks, frac_idle)

  if (!is.null(target_days)) {
    overrun <- summary %>%
      group_by(operation_mode) %>%
      summarise(
        probability = mean(estimated_campaign_days > target_days),
        expected_delay_days = mean(pmax(estimated_campaign_days - target_days, 0)),
        mean_delay_when_binding = sum(pmax(estimated_campaign_days - target_days, 0)) /
          pmax(sum(estimated_campaign_days > target_days), 1),
        p90_delay_days = as.numeric(quantile(pmax(estimated_campaign_days - target_days, 0), 0.90, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      mutate(
        risk = sprintf("Schedule overrun (> %d d)", as.integer(target_days)),
        resource = NA_character_,
        impact = "Campaign exceeds target completion date."
      ) %>%
      select(operation_mode, risk, resource, probability, expected_delay_days,
             mean_delay_when_binding, p90_delay_days, impact)
    risks <- bind_rows(risks, overrun)
  }

  risks %>%
    mutate(
      probability = round(probability, 3),
      likelihood  = .conf_band(probability),
      expected_delay_days = round(expected_delay_days, 1),
      p90_delay_days = round(p90_delay_days, 1),
      risk_note = sprintf("%s: %.0f%% likely, ~%.0f d expected delay (P90 %.0f d).",
                          risk, 100 * probability, expected_delay_days, p90_delay_days)
    ) %>%
    arrange(operation_mode, desc(probability), desc(expected_delay_days))
}

# ===========================================================================
# Convenience wrapper: run both on a simulate_campaign_detailed() result.
# ===========================================================================
build_risk_uncertainty_report <- function(sim_result, target_days = NULL, budget = NULL,
                                          util_threshold = 0.90, ...) {
  stopifnot(is.list(sim_result), "summary" %in% names(sim_result))
  list(
    uncertainty = quantify_uncertainty(
      sim_result$summary, sim_result$resource_utilization,
      target_days = target_days, budget = budget, util_threshold = util_threshold, ...),
    risks = predict_campaign_risks(
      sim_result$summary, sim_result$resource_utilization, target_days = target_days)
  )
}
