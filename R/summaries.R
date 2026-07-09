# summaries.R
# Split out of simulation_engine_fast.R (see docs/architecture_cleanup_plan.md).
# Pure post-processing of a completed simulate_campaign_detailed() result into
# UI-ready tables: summarise_*()/build_*() functions covering delay
# contributors, resource utilization, bottlenecks, traffic lights, readiness
# score, cost impact, executive KPIs/summary, investment ranking, and risk
# consequences. Depends on engine_core.R's output shape only -- never on its
# internals -- so source engine_core.R first.

# ---------------------------------------------------------------------------
# Fixed empty-case schema in summarise_delay_contributors (v11 bug: missing
# columns crashed the PDF report when zero risks triggered).
# ---------------------------------------------------------------------------

summarise_delay_contributors <- function(risk_event_log) {
  if (is.null(risk_event_log) || nrow(risk_event_log) == 0) {
    return(tibble(
      operation_mode = character(), category = character(), risk_event = character(),
      event_count = integer(), total_delay_days = numeric(), mean_delay_days = numeric(),
      total_extra_plugs = numeric(), total_extra_stages = numeric()
    ))
  }

  risk_event_log %>%
    group_by(operation_mode, category, risk_event) %>%
    summarise(
      event_count = n(),
      total_delay_days = sum(delay_days, na.rm = TRUE),
      mean_delay_days = mean(delay_days, na.rm = TRUE),
      total_extra_plugs = sum(extra_plugs, na.rm = TRUE),
      total_extra_stages = sum(extra_stages, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(total_delay_days))
}

# ---------------------------------------------------------------------------
# Summaries, scoring, and reporting (unchanged from v11 except where noted)
# ---------------------------------------------------------------------------

summarise_simulation <- function(results) {
  if (is.list(results) && "summary" %in% names(results)) results <- results$summary
  group_cols <- intersect(c("operation_mode"), names(results))

  results %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      simulations = n(),
      p10_days = as.numeric(quantile(estimated_campaign_days, 0.10, na.rm = TRUE)),
      p50_days = as.numeric(quantile(estimated_campaign_days, 0.50, na.rm = TRUE)),
      p90_days = as.numeric(quantile(estimated_campaign_days, 0.90, na.rm = TRUE)),
      mean_days = mean(estimated_campaign_days, na.rm = TRUE),
      mean_stages = mean(total_stages, na.rm = TRUE),
      mean_extra_plugs = mean(total_extra_plugs, na.rm = TRUE),
      mean_extra_stages = mean(total_extra_stages, na.rm = TRUE),
      mean_risk_delay_days = mean(total_risk_delay_days, na.rm = TRUE),
      .groups = "drop"
    )
}

summarise_resource_utilization <- function(resource_utilization) {
  if (is.null(resource_utilization) || nrow(resource_utilization) == 0) {
    return(tibble())
  }

  resource_utilization %>%
    group_by(operation_mode, resource) %>%
    summarise(
      mean_units = mean(units, na.rm = TRUE),
      mean_workload_days = mean(workload_days, na.rm = TRUE),
      mean_fleet_days_after_resources = mean(fleet_days_after_resources, na.rm = TRUE),
      mean_utilization = mean(utilization, na.rm = TRUE),
      p90_utilization = as.numeric(quantile(utilization, 0.90, na.rm = TRUE)),
      .groups = "drop"
    )
}


summarise_wireline_constraint <- function(summary, well_details = NULL) {
  if (is.null(summary) || nrow(summary) == 0) return(tibble())

  out <- summary %>%
    group_by(operation_mode) %>%
    summarise(
      simulations = n(),
      mean_wireline_stage_operation_days = mean(total_wireline_base_stage_days, na.rm = TRUE),
      mean_wireline_rig_up_down_days = mean(total_wireline_rig_up_down_days, na.rm = TRUE),
      mean_wireline_contingency_days = mean(total_wireline_contingency_days, na.rm = TRUE),
      mean_temperature_logging_days = mean(total_temperature_logging_days, na.rm = TRUE),
      mean_frac_settling_days = mean(total_frac_settling_days, na.rm = TRUE),
      mean_wireline_risk_delay_days = mean(total_wireline_risk_delay_days, na.rm = TRUE),
      mean_wireline_stage_readiness_days = mean(total_wireline_workload_days, na.rm = TRUE),
      mean_wireline_readiness_delay_days = mean(total_wireline_readiness_delay_days, na.rm = TRUE),
      p90_wireline_readiness_delay_days = as.numeric(quantile(total_wireline_readiness_delay_days, 0.90, na.rm = TRUE)),
      mean_campaign_days = mean(estimated_campaign_days, na.rm = TRUE),
      pct_campaign_waiting_on_wireline = mean_wireline_readiness_delay_days / pmax(mean_campaign_days, 1e-9),
      .groups = "drop"
    ) %>%
    mutate(
      interpretation = case_when(
        mean_wireline_readiness_delay_days > 10 ~ "Wireline is materially constraining frac readiness.",
        mean_wireline_readiness_delay_days > 0 ~ "Wireline creates some readiness delay.",
        TRUE ~ "Wireline does not create readiness delay in this setup."
      )
    )

  out
}

summarise_bottlenecks <- function(resource_summary) {
  if (is.null(resource_summary) || nrow(resource_summary) == 0) {
    return(tibble())
  }

  resource_summary %>%
    mutate(
      bottleneck_status = case_when(
        p90_utilization >= 0.85 ~ "Critical",
        p90_utilization >= 0.60 ~ "Moderate",
        TRUE ~ "Available capacity"
      ),
      priority = case_when(
        bottleneck_status == "Critical" ~ 1L,
        bottleneck_status == "Moderate" ~ 2L,
        TRUE ~ 3L
      ),
      recommendation = case_when(
        bottleneck_status == "Critical" ~ paste0("Add one additional ", tolower(resource), " unit or review workflow."),
        bottleneck_status == "Moderate" ~ paste0("Monitor ", tolower(resource), " capacity during planning."),
        TRUE ~ paste0("No immediate additional ", tolower(resource), " capacity required.")
      )
    ) %>%
    arrange(operation_mode, priority, desc(p90_utilization))
}

summarise_stage_level_risks <- function(risk_event_log, summary = NULL) {
  if (is.null(risk_event_log) || nrow(risk_event_log) == 0) {
    return(tibble())
  }

  mode_iterations <- if (!is.null(summary) && nrow(summary) > 0) {
    summary %>% count(operation_mode, name = "simulation_count")
  } else {
    risk_event_log %>% distinct(operation_mode, simulation_id) %>% count(operation_mode, name = "simulation_count")
  }

  stage_keywords <- "screen|plug|perforation|misfire|upct|gun|cement"

  risk_event_log %>%
    filter(stage_id != "Well-level" | stringr::str_detect(normalise_text(risk_event), stage_keywords)) %>%
    group_by(operation_mode, category, risk_event) %>%
    summarise(
      total_events = n(),
      total_delay_days = sum(delay_days, na.rm = TRUE),
      total_extra_plugs = sum(extra_plugs, na.rm = TRUE),
      total_extra_stages = sum(extra_stages, na.rm = TRUE),
      mean_delay_when_occurs = mean(delay_days, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(mode_iterations, by = "operation_mode") %>%
    mutate(
      expected_events_per_campaign = total_events / pmax(simulation_count, 1),
      expected_delay_days_per_campaign = total_delay_days / pmax(simulation_count, 1),
      expected_extra_plugs_per_campaign = total_extra_plugs / pmax(simulation_count, 1),
      expected_extra_stages_per_campaign = total_extra_stages / pmax(simulation_count, 1)
    ) %>%
    arrange(desc(expected_delay_days_per_campaign), desc(expected_events_per_campaign))
}


build_traffic_lights <- function(summary, risk_event_log, resource_utilization) {
  if (is.null(summary) || nrow(summary) == 0) return(tibble())

  # Single named source for every cutoff below. The "reason" text is built
  # from these same constants, so it can never drift out of sync with the
  # case_when() that actually assigns the colour -- same pattern as
  # REC_DECISION_THRESHOLDS in recommendations.R.
  TL_THRESHOLDS <- list(
    schedule_red = 0.25, schedule_amber = 0.15,
    resource_red = 0.85, resource_amber = 0.60,
    operational_red = 0.10, operational_amber = 0.05,
    wireline_red = 0.10, wireline_amber = 0.02
  )

  sim_stats <- summarise_simulation(summary)
  resource_summary <- summarise_resource_utilization(resource_utilization)
  bottlenecks <- summarise_bottlenecks(resource_summary)
  delay_summary <- summarise_delay_contributors(risk_event_log)

  risk_by_mode <- summary %>%
    group_by(operation_mode) %>%
    summarise(
      mean_risk_delay_days = mean(total_risk_delay_days, na.rm = TRUE),
      mean_campaign_days = mean(estimated_campaign_days, na.rm = TRUE),
      mean_wireline_wait_days = mean(total_wireline_readiness_delay_days, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      risk_delay_ratio = mean_risk_delay_days / pmax(mean_campaign_days, 1e-9),
      wireline_wait_ratio = mean_wireline_wait_days / pmax(mean_campaign_days, 1e-9)
    )

  max_bottleneck <- bottlenecks %>%
    group_by(operation_mode) %>%
    summarise(
      max_p90_utilization = max(p90_utilization, na.rm = TRUE),
      max_p90_resource = resource[which.max(p90_utilization)],
      .groups = "drop"
    )

  out <- sim_stats %>%
    left_join(risk_by_mode, by = "operation_mode") %>%
    left_join(max_bottleneck, by = "operation_mode") %>%
    mutate(
      uncertainty_ratio = (p90_days - p50_days) / pmax(p50_days, 1e-9),
      schedule_risk = case_when(
        uncertainty_ratio >= TL_THRESHOLDS$schedule_red ~ "Red",
        uncertainty_ratio >= TL_THRESHOLDS$schedule_amber ~ "Amber",
        TRUE ~ "Green"
      ),
      resource_risk = case_when(
        max_p90_utilization >= TL_THRESHOLDS$resource_red ~ "Red",
        max_p90_utilization >= TL_THRESHOLDS$resource_amber ~ "Amber",
        TRUE ~ "Green"
      ),
      operational_risk = case_when(
        risk_delay_ratio >= TL_THRESHOLDS$operational_red ~ "Red",
        risk_delay_ratio >= TL_THRESHOLDS$operational_amber ~ "Amber",
        TRUE ~ "Green"
      ),
      wireline_constraint = case_when(
        wireline_wait_ratio >= TL_THRESHOLDS$wireline_red ~ "Red",
        wireline_wait_ratio > TL_THRESHOLDS$wireline_amber ~ "Amber",
        TRUE ~ "Green"
      ),
      # Quantitative justification for each light, built from the exact
      # ratio + threshold that produced its colour above -- so a Red/Amber/
      # Green call is never shown without the number behind it.
      schedule_risk_reason = sprintf(
        "P90 duration is %.0f%% above P50 (threshold: Red ≥%.0f%%, Amber ≥%.0f%%).",
        100 * uncertainty_ratio, 100 * TL_THRESHOLDS$schedule_red, 100 * TL_THRESHOLDS$schedule_amber
      ),
      resource_risk_reason = sprintf(
        "%s P90 utilization is %.0f%% (threshold: Red ≥%.0f%%, Amber ≥%.0f%%).",
        ifelse(is.na(max_p90_resource), "The busiest resource", max_p90_resource),
        100 * max_p90_utilization, 100 * TL_THRESHOLDS$resource_red, 100 * TL_THRESHOLDS$resource_amber
      ),
      operational_risk_reason = sprintf(
        "Risk events contribute %.0f%% of expected campaign duration (threshold: Red ≥%.0f%%, Amber ≥%.0f%%).",
        100 * risk_delay_ratio, 100 * TL_THRESHOLDS$operational_red, 100 * TL_THRESHOLDS$operational_amber
      ),
      wireline_constraint_reason = sprintf(
        "Frac fleet waits on wireline for %.0f%% of campaign duration (threshold: Red ≥%.0f%%, Amber >%.0f%%).",
        100 * wireline_wait_ratio, 100 * TL_THRESHOLDS$wireline_red, 100 * TL_THRESHOLDS$wireline_amber
      )
    ) %>%
    select(operation_mode,
           schedule_risk, schedule_risk_reason,
           resource_risk, resource_risk_reason,
           operational_risk, operational_risk_reason,
           wireline_constraint, wireline_constraint_reason,
           uncertainty_ratio, max_p90_utilization, risk_delay_ratio, wireline_wait_ratio)

  out
}

build_readiness_score <- function(summary, risk_event_log, resource_utilization) {
  if (is.null(summary) || nrow(summary) == 0) return(tibble())

  # Transparent readiness model.
  # Weights and penalties are documented here and exported in the readiness table.
  # Component scores are bounded 0-100. Higher is better.
  weights <- list(
    schedule = 0.30,
    resource = 0.30,
    risk = 0.25,
    wireline = 0.15
  )
  penalties <- list(
    schedule_uncertainty = 250,  # 20% P90/P50 spread -> 50 schedule score
    risk_delay = 250,            # 20% risk delay ratio -> 50 risk score
    wireline_wait = 300,         # 10% wireline wait ratio -> 70 wireline score
    resource_high_util = 80      # 100% utilization on the busiest non-frac
                                  # resource -> 20 resource score
  )

  sim_stats <- summarise_simulation(summary)
  resource_summary <- summarise_resource_utilization(resource_utilization)
  bottlenecks <- summarise_bottlenecks(resource_summary)

  # Overall peak utilization (incl. frac fleet) is kept for context/reporting.
  resource_max <- bottlenecks %>%
    group_by(operation_mode) %>%
    summarise(max_p90_utilization = max(p90_utilization, na.rm = TRUE), .groups = "drop")

  # The frac fleet's utilization is structurally close to 100% by
  # construction (estimated_campaign_days is built around frac_fleet_days),
  # so it isn't an actionable spare-capacity signal on its own. The resource
  # score instead measures slack in the OTHER resources (Wireline,
  # CT/cleanout, Milling, Testing unit) - the ones the constraint cascade can
  # actually relieve.
  non_frac_max <- bottlenecks %>%
    filter(resource != "Frac fleet") %>%
    group_by(operation_mode) %>%
    slice_max(p90_utilization, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(operation_mode, non_frac_p90_utilization = p90_utilization, non_frac_bottleneck = resource)

  risk_by_mode <- summary %>%
    group_by(operation_mode) %>%
    summarise(
      risk_delay_days_for_score = mean(total_risk_delay_days, na.rm = TRUE),
      wireline_wait_days_for_score = mean(total_wireline_readiness_delay_days, na.rm = TRUE),
      campaign_days_for_score = mean(estimated_campaign_days, na.rm = TRUE),
      .groups = "drop"
    )

  sim_stats %>%
    left_join(resource_max, by = "operation_mode") %>%
    left_join(non_frac_max, by = "operation_mode") %>%
    left_join(risk_by_mode, by = "operation_mode") %>%
    mutate(
      max_p90_utilization = ifelse(is.na(max_p90_utilization), 0, max_p90_utilization),
      non_frac_p90_utilization = ifelse(is.na(non_frac_p90_utilization), 0, non_frac_p90_utilization),
      non_frac_bottleneck = ifelse(is.na(non_frac_bottleneck), "None", non_frac_bottleneck),
      campaign_days_for_score = ifelse(is.na(campaign_days_for_score), mean_days, campaign_days_for_score),
      risk_delay_days_for_score = ifelse(is.na(risk_delay_days_for_score), 0, risk_delay_days_for_score),
      wireline_wait_days_for_score = ifelse(is.na(wireline_wait_days_for_score), 0, wireline_wait_days_for_score),
      uncertainty_ratio = (p90_days - p50_days) / pmax(p50_days, 1e-9),
      risk_delay_ratio = risk_delay_days_for_score / pmax(campaign_days_for_score, 1e-9),
      wireline_wait_ratio = wireline_wait_days_for_score / pmax(campaign_days_for_score, 1e-9),
      schedule_score = pmax(0, 100 - penalties$schedule_uncertainty * uncertainty_ratio),
      resource_score = pmax(0, 100 - penalties$resource_high_util * pmax(non_frac_p90_utilization - 0.60, 0) / 0.40),
      risk_score = pmax(0, 100 - penalties$risk_delay * risk_delay_ratio),
      wireline_score = pmax(0, 100 - penalties$wireline_wait * wireline_wait_ratio),
      schedule_weight = weights$schedule,
      resource_weight = weights$resource,
      risk_weight = weights$risk,
      wireline_weight = weights$wireline,
      readiness_score = round(
        weights$schedule * schedule_score +
          weights$resource * resource_score +
          weights$risk * risk_score +
          weights$wireline * wireline_score,
        1
      ),
      readiness_status = case_when(
        readiness_score >= 80 ~ "Ready",
        readiness_score >= 60 ~ "Caution",
        readiness_score >= 40 ~ "At Risk",
        TRUE ~ "Critical"
      ),
      scoring_note = "Score = 30% schedule + 30% resource + 25% risk + 15% wireline. Resource score reflects the busiest non-frac resource (Wireline/CT/Milling/Testing) - frac-fleet utilization is structurally near 100% by design and is reported separately. Higher is better."
    ) %>%
    select(operation_mode, readiness_score, readiness_status,
           schedule_score, resource_score, risk_score, wireline_score,
           schedule_weight, resource_weight, risk_weight, wireline_weight,
           uncertainty_ratio, max_p90_utilization, non_frac_p90_utilization, non_frac_bottleneck,
           risk_delay_ratio, wireline_wait_ratio,
           scoring_note)
}

build_cost_impact <- function(
    summary,
    resource_utilization,
    frac_fleet_cost_per_day = 250000,
    wireline_cost_per_day = 15000,
    ct_cost_per_day = 25000,
    milling_cost_per_day = 18000,
    testing_unit_cost_per_day = 12000
) {
  if (is.null(summary) || nrow(summary) == 0) return(tibble())

  cost_map <- tibble(
    resource = c("Frac fleet", "Wireline", "CT / cleanout", "Milling", "Testing unit"),
    cost_per_day = c(frac_fleet_cost_per_day, wireline_cost_per_day, ct_cost_per_day,
                     milling_cost_per_day, testing_unit_cost_per_day)
  )

  resource_cost <- resource_utilization %>%
    left_join(cost_map, by = "resource") %>%
    group_by(operation_mode, resource) %>%
    summarise(
      mean_fleet_days = mean(fleet_days_after_resources, na.rm = TRUE),
      cost_per_day = first(cost_per_day),
      estimated_resource_cost = mean_fleet_days * cost_per_day,
      .groups = "drop"
    )

  wireline_idle <- summary %>%
    group_by(operation_mode) %>%
    summarise(
      mean_wireline_wait_days = mean(total_wireline_readiness_delay_days, na.rm = TRUE),
      p90_wireline_wait_days = as.numeric(quantile(total_wireline_readiness_delay_days, 0.90, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(
      resource = "Frac fleet idle while waiting on wireline",
      mean_fleet_days = mean_wireline_wait_days,
      cost_per_day = frac_fleet_cost_per_day,
      estimated_resource_cost = mean_wireline_wait_days * frac_fleet_cost_per_day
    ) %>%
    select(operation_mode, resource, mean_fleet_days, cost_per_day, estimated_resource_cost)

  bind_rows(resource_cost, wireline_idle) %>%
    arrange(operation_mode, desc(estimated_resource_cost))
}

# ---------------------------------------------------------------------------
# Total campaign cost summary (v17.6)
# Answers: "what does this campaign actually cost in total?"
#
# Total cost = Σ (units × day_rate × P50_campaign_days) for all resources
#            + wireline idle cost
#
# Broken into:
#   - Productive cost: units × day_rate × active_days  (work actually done)
#   - Standby cost:    units × day_rate × (campaign - active_days)  (on-site but idle)
#   - Idle penalty:    frac_fleet × day_rate × wireline_readiness_delay  (waiting on WL)
#
# This lets the user see: going faster with zipper costs the same or less in
# total because the campaign is shorter (fewer standby days), even though
# the per-day resource mix may be similar.
# ---------------------------------------------------------------------------
build_total_campaign_cost <- function(
    summary,
    resource_utilization,
    frac_fleets = 1, wireline_units = 1, ct_units = 1,
    milling_units = 1, testing_units = 1,
    frac_fleet_cost_per_day  = 250000,
    wireline_cost_per_day    = 15000,
    ct_cost_per_day          = 25000,
    milling_cost_per_day     = 18000,
    testing_unit_cost_per_day = 12000
) {
  if (is.null(summary) || nrow(summary) == 0) return(NULL)

  units_map <- tibble::tibble(
    resource     = c("Frac fleet","Wireline","CT / cleanout","Milling","Testing unit"),
    units        = c(frac_fleets, wireline_units, ct_units, milling_units, testing_units),
    day_rate     = c(frac_fleet_cost_per_day, wireline_cost_per_day, ct_cost_per_day,
                     milling_cost_per_day, testing_unit_cost_per_day)
  ) %>%
    dplyr::mutate(total_day_rate = units * day_rate)

  ru <- summarise_resource_utilization(resource_utilization)

  agg <- summary %>%
    dplyr::group_by(operation_mode) %>%
    dplyr::summarise(
      p50_days   = as.numeric(stats::quantile(estimated_campaign_days, 0.5, na.rm = TRUE)),
      p10_days   = as.numeric(stats::quantile(estimated_campaign_days, 0.1, na.rm = TRUE)),
      p90_days   = as.numeric(stats::quantile(estimated_campaign_days, 0.9, na.rm = TRUE)),
      idle_days  = mean(total_wireline_readiness_delay_days, na.rm = TRUE),
      .groups = "drop"
    )

  # Active fleet-days per resource from resource_utilization
  active <- ru %>%
    dplyr::left_join(units_map, by = "resource") %>%
    dplyr::select(operation_mode, resource, units, day_rate, total_day_rate,
                  mean_active_days = mean_utilization)  # utilization * campaign = active days

  # Build full cost table per mode
  purrr::map_dfr(unique(agg$operation_mode), function(mode) {
    a <- agg %>% dplyr::filter(operation_mode == mode)
    r <- ru   %>% dplyr::filter(operation_mode == mode)

    # Active days from utilization
    active_by_res <- r %>%
      dplyr::left_join(units_map, by = "resource") %>%
      dplyr::mutate(
        active_days    = mean_utilization * a$p50_days,
        standby_days   = pmax(a$p50_days - active_days, 0),
        productive_cost = active_days  * total_day_rate,
        standby_cost    = standby_days * total_day_rate
      )

    total_productive <- sum(active_by_res$productive_cost, na.rm = TRUE)
    total_standby    <- sum(active_by_res$standby_cost,    na.rm = TRUE)
    idle_penalty     <- a$idle_days * frac_fleet_cost_per_day * frac_fleets
    total_cost       <- total_productive + total_standby + idle_penalty

    tibble::tibble(
      operation_mode   = mode,
      p50_days         = a$p50_days,
      p10_days         = a$p10_days,
      p90_days         = a$p90_days,
      productive_cost  = total_productive,
      standby_cost     = total_standby,
      idle_penalty     = idle_penalty,
      total_cost       = total_cost,
      cost_per_day     = total_cost / a$p50_days,
      resource_detail  = list(active_by_res %>%
        dplyr::select(resource, units, day_rate, total_day_rate,
                      active_days, standby_days, productive_cost, standby_cost))
    )
  })
}

# ---------------------------------------------------------------------------
# Recommendation confidence (v17.6)
# Answers: "how often does zipper beat conventional across all simulations?"
# Uses paired simulation IDs (same seed, same random draws) for a fair
# like-for-like comparison. When both modes use a common seed, each iteration
# ID represents the same "state of the world" -- same risk events, same
# historical draws -- so the comparison is controlled.
# ---------------------------------------------------------------------------
build_recommendation_confidence <- function(summary) {
  if (is.null(summary) || nrow(summary) == 0) return(NULL)
  modes <- unique(summary$operation_mode)
  if (!all(c("Conventional","Zipper") %in% modes)) return(NULL)

  conv <- summary %>%
    dplyr::filter(operation_mode == "Conventional") %>%
    dplyr::select(simulation_id, conv_days = estimated_campaign_days)
  zip  <- summary %>%
    dplyr::filter(operation_mode == "Zipper") %>%
    dplyr::select(simulation_id, zip_days  = estimated_campaign_days)

  paired <- dplyr::inner_join(conv, zip, by = "simulation_id")
  if (nrow(paired) == 0) return(NULL)

  n        <- nrow(paired)
  zip_wins <- mean(paired$zip_days < paired$conv_days)
  conv_wins<- mean(paired$zip_days > paired$conv_days)
  tied     <- mean(paired$zip_days == paired$conv_days)
  mean_sav <- mean(paired$conv_days - paired$zip_days)
  p10_sav  <- as.numeric(quantile(paired$conv_days - paired$zip_days, 0.10))
  p90_sav  <- as.numeric(quantile(paired$conv_days - paired$zip_days, 0.90))

  best     <- if (zip_wins >= conv_wins) "Zipper" else "Conventional"
  best_pct <- max(zip_wins, conv_wins)

  confidence <- dplyr::case_when(
    best_pct >= 0.90 ~ "High",
    best_pct >= 0.75 ~ "Moderate",
    best_pct >= 0.60 ~ "Low",
    TRUE             ~ "Inconclusive"
  )

  tibble::tibble(
    best_option       = best,
    confidence        = confidence,
    pct_best_wins     = round(100 * best_pct, 1),
    pct_other_wins    = round(100 * min(zip_wins, conv_wins), 1),
    pct_tied          = round(100 * tied, 1),
    mean_saving_days  = round(mean_sav, 1),
    p10_saving_days   = round(p10_sav, 1),
    p90_saving_days   = round(p90_sav, 1),
    n_simulations     = n,
    confidence_note   = sprintf(
      "%s is faster in %.1f%% of the %d simulations (mean saving %.0f d, range %.0f to %.0f d)",
      best, 100 * best_pct, n, mean_sav, p10_sav, p90_sav
    )
  )
}

build_resource_recommendations <- function(summary, resource_utilization, cost_impact = NULL) {
  if (is.null(resource_utilization) || nrow(resource_utilization) == 0) return(tibble())

  resource_summary <- summarise_resource_utilization(resource_utilization)
  bottlenecks <- summarise_bottlenecks(resource_summary)
  sim_stats <- summarise_simulation(summary) %>% select(operation_mode, p50_days)

  # Approximate saving: workload is divided by one additional unit for the bottleneck resource.
  # This is a screening estimate, not a full re-simulation.
  out <- bottlenecks %>%
    left_join(sim_stats, by = "operation_mode") %>%
    mutate(
      proposed_units = mean_units + 1,
      current_fleet_days = mean_fleet_days_after_resources,
      estimated_fleet_days_with_extra_unit = mean_workload_days / pmax(proposed_units, 1),
      estimated_days_released = pmax(current_fleet_days - estimated_fleet_days_with_extra_unit, 0),
      estimated_campaign_saving_days = case_when(
        bottleneck_status == "Critical" ~ pmin(estimated_days_released, p50_days * 0.25),
        bottleneck_status == "Moderate" ~ pmin(estimated_days_released, p50_days * 0.10),
        TRUE ~ 0
      ),
      recommendation_rank = case_when(
        bottleneck_status == "Critical" ~ 1L,
        bottleneck_status == "Moderate" ~ 2L,
        TRUE ~ 3L
      ),
      recommended_action = case_when(
        bottleneck_status == "Critical" ~ paste0("Add 1 ", tolower(resource), " unit or redesign execution sequence."),
        bottleneck_status == "Moderate" ~ paste0("Monitor ", tolower(resource), " capacity and keep contingency access."),
        TRUE ~ paste0("No additional ", tolower(resource), " unit recommended at this stage.")
      )
    ) %>%
    arrange(operation_mode, recommendation_rank, desc(estimated_campaign_saving_days)) %>%
    select(operation_mode, resource, bottleneck_status, mean_units, proposed_units, p90_utilization,
           estimated_campaign_saving_days, recommended_action)

  out
}

build_executive_kpis <- function(summary, risk_event_log, resource_utilization, frac_fleet_cost_per_day = 250000) {
  if (is.null(summary) || nrow(summary) == 0) return(tibble())

  sim_stats <- summarise_simulation(summary)
  bottlenecks <- summarise_bottlenecks(summarise_resource_utilization(resource_utilization))
  delay_summary <- summarise_delay_contributors(risk_event_log)
  readiness <- build_readiness_score(summary, risk_event_log, resource_utilization)

  best <- sim_stats %>% arrange(p50_days) %>% slice(1)
  conventional <- sim_stats %>% filter(operation_mode == "Conventional") %>% slice(1)
  zipper <- sim_stats %>% filter(operation_mode == "Zipper") %>% slice(1)

  saving_days <- if (nrow(conventional) == 1 && nrow(zipper) == 1) conventional$p50_days - zipper$p50_days else NA_real_
  saving_pct <- if (!is.na(saving_days) && conventional$p50_days > 0) saving_days / conventional$p50_days else NA_real_

  primary_bottleneck <- bottlenecks %>% arrange(priority, desc(p90_utilization)) %>% slice(1)
  top_risk <- delay_summary %>%
    group_by(risk_event) %>%
    summarise(total_delay_days = sum(total_delay_days, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total_delay_days)) %>%
    slice(1)

  mean_wireline_wait <- mean(summary$total_wireline_readiness_delay_days, na.rm = TRUE)
  idle_cost <- mean_wireline_wait * frac_fleet_cost_per_day

  tibble(
    kpi = c("Best option", "P50 duration", "P90 duration", "Zipper P50 saving", "Primary bottleneck", "Top risk", "Wireline waiting", "Idle frac fleet cost", "Readiness score"),
    value = c(
      best$operation_mode,
      paste0(round(best$p50_days, 1), " days"),
      paste0(round(best$p90_days, 1), " days"),
      ifelse(is.na(saving_days), "N/A", paste0(round(saving_days, 1), " days (", round(100 * saving_pct, 1), "%)")),
      ifelse(nrow(primary_bottleneck) == 0, "N/A", paste(primary_bottleneck$operation_mode, primary_bottleneck$resource, sep = " - ")),
      ifelse(nrow(top_risk) == 0, "No triggered risks", top_risk$risk_event),
      paste0(round(mean_wireline_wait, 1), " days"),
      paste0("$", format(round(idle_cost, 0), big.mark = ",")),
      paste0(round(mean(readiness$readiness_score, na.rm = TRUE), 1), " / 100")
    )
  )
}


# ---------------------------------------------------------------------------
# NEW v12.1: Investment ranking - answers "where should I spend next?"
# Economics:
#   schedule_value      = saving_days x total daily spread rate (cost of every
#                         campaign day avoided, using the rates entered in-app)
#   incremental_cost    = added unit's day rate x resulting P50 duration
#   net_benefit         = schedule_value - incremental_cost
# These are planning-level estimates; review against contract rates.
# ---------------------------------------------------------------------------

build_investment_ranking <- function(summary, recommendations,
                                     frac_fleet_cost_per_day = 250000,
                                     wireline_cost_per_day = 15000,
                                     ct_cost_per_day = 25000,
                                     milling_cost_per_day = 18000,
                                     testing_unit_cost_per_day = 12000) {
  empty <- tibble(
    operation_mode = character(), resource = character(), proposed_change = character(),
    p50_saving_days = numeric(), new_p50_days = numeric(), incremental_unit_cost = numeric(),
    schedule_value = numeric(), net_benefit = numeric(), benefit_cost_ratio = numeric(),
    roi_tier = character(), recommended_action = character()
  )
  if (is.null(recommendations) || nrow(recommendations) == 0) return(empty)
  if (is.null(summary) || nrow(summary) == 0) return(empty)

  # Named thresholds for the ROI tier label, so "Excellent"/"Good"/"Marginal"
  # can never drift out of sync with the cutoffs that assign them.
  ROI_TIER_THRESHOLDS <- list(excellent = 5, good = 2)

  rate_lookup <- c(
    "Frac fleet" = frac_fleet_cost_per_day,
    "Wireline" = wireline_cost_per_day,
    "CT / cleanout" = ct_cost_per_day,
    "Milling" = milling_cost_per_day,
    "Testing unit" = testing_unit_cost_per_day
  )

  mode_stats <- summary %>%
    group_by(operation_mode) %>%
    summarise(
      p50_days = quantile(estimated_campaign_days, 0.5, na.rm = TRUE),
      spread_rate =
        mean(frac_fleets, na.rm = TRUE) * frac_fleet_cost_per_day +
        mean(wireline_units, na.rm = TRUE) * wireline_cost_per_day +
        mean(ct_units, na.rm = TRUE) * ct_cost_per_day +
        mean(milling_units, na.rm = TRUE) * milling_cost_per_day,
      .groups = "drop"
    )

  recommendations %>%
    left_join(mode_stats, by = "operation_mode") %>%
    mutate(
      unit_day_rate = unname(rate_lookup[resource]),
      unit_day_rate = ifelse(is.na(unit_day_rate), 0, unit_day_rate),
      p50_saving_days = pmax(estimated_campaign_saving_days, 0),
      new_p50_days = pmax(p50_days - p50_saving_days, 0),
      incremental_unit_cost = unit_day_rate * new_p50_days,
      schedule_value = p50_saving_days * spread_rate,
      net_benefit = schedule_value - incremental_unit_cost,
      benefit_cost_ratio = ifelse(incremental_unit_cost > 0,
                                  schedule_value / incremental_unit_cost, NA_real_),
      roi_tier = case_when(
        is.na(benefit_cost_ratio)                       ~ NA_character_,
        benefit_cost_ratio > ROI_TIER_THRESHOLDS$excellent ~ "Excellent",
        benefit_cost_ratio >= ROI_TIER_THRESHOLDS$good     ~ "Good",
        TRUE                                                ~ "Marginal"
      ),
      proposed_change = paste0(resource, ": ", mean_units, " -> ", proposed_units)
    ) %>%
    filter(p50_saving_days > 0) %>%
    arrange(desc(net_benefit)) %>%
    select(
      operation_mode, resource, proposed_change, p50_saving_days, new_p50_days,
      incremental_unit_cost, schedule_value, net_benefit, benefit_cost_ratio, roi_tier,
      recommended_action
    )
}

# NEW v12.1: one-row narrative for the most critical bottleneck per run.
build_bottleneck_narrative <- function(bottlenecks, recommendations) {
  if (is.null(bottlenecks) || nrow(bottlenecks) == 0) {
    return(tibble(operation_mode = character(), resource = character(),
                  p90_utilization = numeric(), bottleneck_status = character(),
                  p50_saving_days = numeric(), recommended_action = character()))
  }

  top <- bottlenecks %>%
    arrange(priority, desc(p90_utilization)) %>%
    slice(1)

  saving <- 0
  action <- top$recommendation
  if (!is.null(recommendations) && nrow(recommendations) > 0) {
    match_row <- recommendations %>%
      filter(operation_mode == top$operation_mode, resource == top$resource) %>%
      slice(1)
    if (nrow(match_row) == 1) {
      saving <- match_row$estimated_campaign_saving_days
      action <- match_row$recommended_action
    }
  }

  tibble(
    operation_mode = top$operation_mode,
    resource = top$resource,
    p90_utilization = top$p90_utilization,
    bottleneck_status = top$bottleneck_status,
    p50_saving_days = saving,
    recommended_action = action
  )
}

# ---------------------------------------------------------------------------
# NEW v14: Scenario optimiser - finds the resource configuration minimising
# total mobilisation cost (all contracted units x day rate x P50 duration,
# which embeds both schedule and idle time). Two-stage:
#   Stage 1 (screen): all configs at screen_iterations with a COMMON SEED so
#     rankings compare like-for-like (common random numbers).
#   Stage 2 (refine): top_n_refine configs re-run at refine_iterations.
# Pareto flag marks configs not dominated on (P50 duration, total cost).
# ---------------------------------------------------------------------------


summarise_risk_consequences <- function(risk_event_log, summary = NULL) {
  empty <- tibble(
    operation_mode = character(), risk_event = character(), event_count = integer(),
    direct_delay_days = numeric(), induced_wireline_days = numeric(),
    induced_ct_days = numeric(), induced_milling_days = numeric(),
    induced_testing_days = numeric(), induced_frac_days = numeric(),
    total_induced_days = numeric(), total_impact_days = numeric(),
    induced_share = numeric(), expected_impact_per_campaign = numeric()
  )
  if (is.null(risk_event_log) || nrow(risk_event_log) == 0) return(empty)

  n_sims <- if (!is.null(summary) && nrow(summary) > 0) {
    summary %>% count(operation_mode, name = "n_sims")
  } else {
    risk_event_log %>% distinct(operation_mode, simulation_id) %>%
      count(operation_mode, name = "n_sims")
  }

  risk_event_log %>%
    group_by(operation_mode, risk_event) %>%
    summarise(
      event_count = n(),
      direct_delay_days = sum(delay_days, na.rm = TRUE),
      induced_wireline_days = sum(extra_wireline_days, na.rm = TRUE),
      induced_ct_days = sum(extra_ct_days, na.rm = TRUE),
      induced_milling_days = sum(extra_milling_days, na.rm = TRUE),
      induced_testing_days = sum(extra_testing_days, na.rm = TRUE),
      induced_frac_days = sum(extra_frac_days, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(n_sims, by = "operation_mode") %>%
    mutate(
      total_induced_days = induced_wireline_days + induced_ct_days +
        induced_milling_days + induced_testing_days + induced_frac_days,
      total_impact_days = direct_delay_days + total_induced_days,
      induced_share = ifelse(total_impact_days > 0,
                             total_induced_days / total_impact_days, 0),
      expected_impact_per_campaign = total_impact_days / pmax(n_sims, 1)
    ) %>%
    select(-n_sims) %>%
    arrange(desc(expected_impact_per_campaign))
}

