# risk_heatmap.R
# Issue #12: Schedule Risk Heatmap
#
# Aggregates risk_event_log to well × risk-type level and produces:
#   - A well × risk-event expected-delay matrix (for the tile heatmap)
#   - Per-well risk scores with Low/Medium/High/Critical classification
#   - Per-pad risk scores
#
# Dependencies (source first): simulation_engine[_fast].R
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

#' Build the schedule risk heatmap data structures.
#'
#' @param risk_event_log  Tibble from simulate_campaign_detailed()$risk_event_log.
#' @param summary         Tibble from simulate_campaign_detailed()$summary
#'                        (used to get n_iterations per mode).
#'
#' @return List:
#'   well_matrix  — well × risk_event expected delay (for tile heatmap)
#'   well_scores  — per-well total expected delay + risk classification
#'   pad_scores   — per-pad aggregate
build_schedule_risk_heatmap <- function(risk_event_log, summary) {
  if (is.null(risk_event_log) || nrow(risk_event_log) == 0) {
    return(list(well_matrix = tibble(), well_scores = tibble(), pad_scores = tibble()))
  }

  n_iter <- summary %>%
    dplyr::count(operation_mode, name = "n_iter")

  # ---- Well × risk-event matrix --------------------------------------------
  well_matrix <- risk_event_log %>%
    group_by(operation_mode, well_id, pad_id, risk_event, category) %>%
    summarise(
      total_delay  = sum(delay_days,  na.rm = TRUE),
      total_events = n(),
      .groups = "drop"
    ) %>%
    left_join(n_iter, by = "operation_mode") %>%
    mutate(
      expected_delay_days  = total_delay  / pmax(n_iter, 1),
      expected_events      = total_events / pmax(n_iter, 1)
    ) %>%
    select(-total_delay, -total_events, -n_iter)

  # ---- Well risk scores ----------------------------------------------------
  well_scores <- well_matrix %>%
    group_by(operation_mode, well_id, pad_id) %>%
    summarise(
      total_expected_delay = sum(expected_delay_days, na.rm = TRUE),
      n_risk_types         = n_distinct(risk_event),
      top_risk             = risk_event[which.max(expected_delay_days)],
      top_risk_delay       = max(expected_delay_days, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(operation_mode) %>%
    mutate(
      risk_rank  = rank(-total_expected_delay, ties.method = "first"),
      risk_level = dplyr::case_when(
        total_expected_delay >= quantile(total_expected_delay, 0.75, na.rm = TRUE) ~ "Critical",
        total_expected_delay >= quantile(total_expected_delay, 0.50, na.rm = TRUE) ~ "High",
        total_expected_delay >= quantile(total_expected_delay, 0.25, na.rm = TRUE) ~ "Medium",
        TRUE ~ "Low"
      ),
      risk_level = factor(risk_level, levels = c("Low", "Medium", "High", "Critical"))
    ) %>%
    ungroup() %>%
    arrange(operation_mode, risk_rank)

  # ---- Pad risk scores -----------------------------------------------------
  pad_scores <- well_scores %>%
    group_by(operation_mode, pad_id) %>%
    summarise(
      n_wells              = n_distinct(well_id),
      total_expected_delay = sum(total_expected_delay, na.rm = TRUE),
      max_well_delay       = max(total_expected_delay,  na.rm = TRUE),
      n_critical_wells     = sum(risk_level == "Critical"),
      n_high_wells         = sum(risk_level %in% c("High", "Critical")),
      .groups = "drop"
    ) %>%
    group_by(operation_mode) %>%
    mutate(
      pad_risk_level = dplyr::case_when(
        total_expected_delay >= quantile(total_expected_delay, 0.75, na.rm = TRUE) ~ "Critical",
        total_expected_delay >= quantile(total_expected_delay, 0.50, na.rm = TRUE) ~ "High",
        total_expected_delay >= quantile(total_expected_delay, 0.25, na.rm = TRUE) ~ "Medium",
        TRUE ~ "Low"
      ),
      pad_risk_level = factor(pad_risk_level, levels = c("Low", "Medium", "High", "Critical"))
    ) %>%
    ungroup() %>%
    arrange(operation_mode, desc(total_expected_delay))

  list(
    well_matrix = well_matrix,
    well_scores = well_scores,
    pad_scores  = pad_scores
  )
}
