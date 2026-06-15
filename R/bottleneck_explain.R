# bottleneck_explain.R
# -----------------------------------------------------------------------------
# V2.5 #2 Bottleneck Explainability. Turns the per-iteration resource picture
# into the spec's evidence chain:
#   Campaign bottleneck: <primary>
#   Evidence: active work, campaign duration, utilization, queue-delay contribution
#   Secondary bottleneck: <secondary>  ("would become binding if primary relieved")
#   + a constraint cascade (relieve rank-1 -> rank-2 emerges -> ...).
#
# Method
# ------
# Within each Monte Carlo iteration, resources are ranked by
# fleet_days_after_resources (the critical-path load). The GAP between
# consecutive ranks is the schedule time recoverable by relieving that rank:
#   gap_1 = days the primary adds over the next-tightest resource
#   gap_2 = days the (then-)secondary adds once the primary is relieved
# Averaging these over iterations gives expected, decision-ready contributions.
#
# This deliberately classifies severity by DELAY CONTRIBUTION relative to the
# P50 campaign, not by "is it the argmax" -- so a resource that is always the
# tightest but only by a day reads as Minor, not Critical (fixes the
# "100% binding but harmless" artifact in raw argmax probability).
#
# Consistency: delay_as_primary_days here equals expected_delay_days from
# predict_campaign_risks() (risk_uncertainty.R) by construction.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({ library(dplyr) })

# Severity from the primary-delay contribution as a share of P50 duration.
.bottleneck_status <- function(contribution_pct, delay_days) dplyr::case_when(
  contribution_pct >= 0.10 ~ "Critical",
  contribution_pct >= 0.03 ~ "Moderate",
  delay_days       >  0.5  ~ "Minor",
  TRUE                     ~ "Available capacity"
)

explain_bottlenecks <- function(summary, resource_utilization) {
  if (is.null(resource_utilization) || nrow(resource_utilization) == 0) {
    return(list(ranking = tibble(), cascade = tibble(), roles = tibble(), narrative = character()))
  }

  # Per-iteration ranking by critical-path load; gap = recoverable-if-relieved.
  ranked <- resource_utilization %>%
    group_by(operation_mode, simulation_id) %>%
    arrange(desc(fleet_days_after_resources), .by_group = TRUE) %>%
    mutate(
      pos     = row_number(),
      lead_fd = dplyr::lead(fleet_days_after_resources, default = 0),
      gap     = pmax(fleet_days_after_resources - lead_fd, 0)
    ) %>%
    ungroup()

  p50 <- summary %>%
    group_by(operation_mode) %>%
    summarise(p50_days = as.numeric(quantile(estimated_campaign_days, 0.50, na.rm = TRUE)),
              .groups = "drop")

  # Resource-level evidence.
  ranking <- ranked %>%
    group_by(operation_mode, resource) %>%
    summarise(
      mean_active_days        = mean(fleet_days_after_resources, na.rm = TRUE),
      mean_utilization        = mean(utilization, na.rm = TRUE),
      p90_utilization         = as.numeric(quantile(utilization, 0.90, na.rm = TRUE)),
      prob_primary            = mean(pos == 1),
      prob_secondary          = mean(pos == 2),
      delay_as_primary_days   = mean(ifelse(pos == 1, gap, 0)),
      delay_as_secondary_days = mean(ifelse(pos == 2, gap, 0)),
      .groups = "drop"
    ) %>%
    left_join(p50, by = "operation_mode") %>%
    mutate(
      queue_delay_contribution_days = delay_as_primary_days,
      contribution_pct = delay_as_primary_days / pmax(p50_days, 1e-9),
      status = .bottleneck_status(contribution_pct, delay_as_primary_days)
    ) %>%
    arrange(operation_mode, desc(delay_as_primary_days), desc(prob_primary)) %>%
    group_by(operation_mode) %>%
    mutate(bottleneck_rank = row_number()) %>%
    ungroup()

  # Constraint cascade (constraint tree): position-ordered relief waterfall.
  cascade <- ranked %>%
    group_by(operation_mode, pos) %>%
    summarise(
      mean_gap_days   = mean(gap, na.rm = TRUE),
      modal_resource  = names(sort(table(resource), decreasing = TRUE))[1],
      modal_prob      = as.numeric(max(table(resource)) / dplyr::n()),
      .groups = "drop"
    ) %>%
    group_by(operation_mode) %>%
    arrange(pos, .by_group = TRUE) %>%
    mutate(cumulative_recoverable_days = cumsum(mean_gap_days)) %>%
    ungroup()

  # Primary / secondary roles per mode.
  roles <- ranking %>%
    group_by(operation_mode) %>%
    summarise(
      primary               = resource[which.max(delay_as_primary_days)],
      primary_delay_days    = max(delay_as_primary_days),
      primary_status        = status[which.max(delay_as_primary_days)],
      primary_p90_util      = p90_utilization[which.max(delay_as_primary_days)],
      primary_active_days   = mean_active_days[which.max(delay_as_primary_days)],
      p50_days              = first(p50_days),
      .groups = "drop"
    )
  # Secondary = highest delay_as_secondary among non-primary resources.
  sec <- ranking %>%
    left_join(roles %>% select(operation_mode, primary), by = "operation_mode") %>%
    filter(resource != primary) %>%
    group_by(operation_mode) %>%
    summarise(
      secondary               = resource[which.max(delay_as_secondary_days)],
      secondary_delay_days    = max(delay_as_secondary_days),
      secondary_p90_util      = p90_utilization[which.max(delay_as_secondary_days)],
      .groups = "drop"
    )
  roles <- left_join(roles, sec, by = "operation_mode") %>%
    mutate(
      secondary_contribution_pct = secondary_delay_days / pmax(p50_days, 1e-9),
      # A secondary only "matters" if relieving the primary actually exposes a
      # constraint of consequence. Otherwise the campaign is balanced below the
      # primary and naming a 4-day "secondary" would mislead.
      secondary_material = secondary_delay_days >= 5 & secondary_contribution_pct >= 0.03
    )

  # Spec-format narrative per mode.
  narrative <- vapply(seq_len(nrow(roles)), function(i) {
    r <- roles[i, ]
    secondary_block <- if (isTRUE(r$secondary_material)) {
      sprintf(
"Secondary bottleneck: %s
  * Utilization (P90) = %.0f%%
  * Would become the binding constraint if %s is relieved (adds ~%.0f days)",
        r$secondary, 100 * r$secondary_p90_util, r$primary, r$secondary_delay_days)
    } else {
      sprintf(
"Secondary bottleneck: none material
  * Once %s is relieved the campaign is balanced (next constraint adds only ~%.0f days)",
        r$primary, r$secondary_delay_days)
    }
    sprintf(
"Campaign bottleneck: %s (%s)
Evidence:
  * Active work = %.0f fleet-days
  * Campaign duration (P50) = %.0f days
  * Utilization (P90) = %.0f%%
  * Queue-delay contribution = %.0f days

%s",
      r$primary, r$primary_status, r$primary_active_days, r$p50_days,
      100 * r$primary_p90_util, r$primary_delay_days, secondary_block
    )
  }, character(1))
  names(narrative) <- roles$operation_mode

  list(ranking = ranking, cascade = cascade, roles = roles, narrative = narrative)
}

# Convenience: print the narrative block(s).
print_bottleneck_explanation <- function(explain_result, mode = NULL) {
  nv <- explain_result$narrative
  if (!is.null(mode)) nv <- nv[mode]
  for (m in names(nv)) { cat("====", m, "====\n"); cat(nv[[m]], "\n\n") }
  invisible(explain_result)
}
