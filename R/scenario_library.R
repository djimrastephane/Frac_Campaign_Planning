# scenario_library.R
# -----------------------------------------------------------------------------
# "Scenario library": lets the user save a snapshot of the current run's
# configuration + key results, run a different configuration, and compare
# saved scenarios side by side (table + duration S-curve overlay).
#
# Session-scoped only (held in a reactiveVal in app.R) -- no disk persistence.
# When "Compare both" is active, saving adds one record PER operation mode.
#
# Dependencies (source first): simulation_engine[_fast].R, bottleneck_explain.R,
# recommendations.R
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

SCENARIO_LIBRARY_MAX <- 6

# Builds one scenario-library record from a completed simulation result for a
# single operation mode. `args` is the exact args list used for that mode
# (sim_results()$args_by_mode[[mode]]).
build_scenario_record <- function(
    sim_result, args, label = NULL,
    frac_fleet_cost_per_day = 250000, wireline_cost_per_day = 15000,
    ct_cost_per_day = 25000, milling_cost_per_day = 18000, testing_unit_cost_per_day = 12000
) {
  mode <- args$operation_mode
  sm <- sim_result$summary %>% filter(operation_mode == mode)

  rd <- build_readiness_score(sim_result$summary, sim_result$risk_event_log, sim_result$resource_utilization)
  rd_m <- rd[rd$operation_mode == mode, ]

  rec <- recommend_action(sim_result, sim_args = args, verify = FALSE,
    frac_fleet_cost_per_day = frac_fleet_cost_per_day, wireline_cost_per_day = wireline_cost_per_day,
    ct_cost_per_day = ct_cost_per_day, milling_cost_per_day = milling_cost_per_day,
    testing_unit_cost_per_day = testing_unit_cost_per_day)

  rates <- c("Frac fleet" = frac_fleet_cost_per_day, "Wireline" = wireline_cost_per_day,
             "CT / cleanout" = ct_cost_per_day, "Milling" = milling_cost_per_day,
             "Testing unit" = testing_unit_cost_per_day)
  units <- sim_result$resource_utilization %>% filter(operation_mode == mode) %>%
    group_by(resource) %>% summarise(units = mean(units, na.rm = TRUE), .groups = "drop")
  spread_rate <- sum(units$units * unname(rates[units$resource]), na.rm = TRUE)

  config_label <- sprintf(
    "%s | FF:%s WL:%s CT:%s ML:%s TU:%s Trees:%s%s",
    mode, args$frac_fleets, args$wireline_units, args$ct_units,
    args$milling_units, args$testing_units, args$frac_trees,
    if (isTRUE(args$allow_ct_for_milling)) " +CTmill" else ""
  )

  list(
    id = sprintf("%s_%s", mode, format(Sys.time(), "%Y%m%d%H%M%OS3")),
    label = if (is.null(label) || !nzchar(label)) config_label else label,
    timestamp = Sys.time(),
    operation_mode = mode,
    config_label = config_label,
    p50_days = as.numeric(quantile(sm$estimated_campaign_days, 0.50, na.rm = TRUE)),
    p90_days = as.numeric(quantile(sm$estimated_campaign_days, 0.90, na.rm = TRUE)),
    readiness_score = rd_m$readiness_score[1],
    readiness_status = rd_m$readiness_status[1],
    non_frac_bottleneck = rd_m$non_frac_bottleneck[1],
    non_frac_p90_utilization = rd_m$non_frac_p90_utilization[1],
    recommendation = rec$recommendation,
    spread_rate_per_day = spread_rate,
    duration = sm$estimated_campaign_days
  )
}

# Appends `new_records` to `current`, dropping the oldest entries if the
# combined list would exceed `max_n`.
add_scenario_records <- function(current, new_records, max_n = SCENARIO_LIBRARY_MAX) {
  combined <- c(current, new_records)
  if (length(combined) > max_n) combined <- utils::tail(combined, max_n)
  combined
}

# Removes the record with the given id.
remove_scenario_record <- function(current, id) {
  Filter(function(r) !identical(r$id, id), current)
}

# Tidies the saved records into a comparison table (one row per record).
scenario_library_to_df <- function(records) {
  if (length(records) == 0) return(tibble())
  bind_rows(lapply(records, function(r) {
    tibble(
      id = r$id,
      Label = r$label,
      Config = r$config_label,
      `P50 (d)` = round(r$p50_days, 1),
      `P90 (d)` = round(r$p90_days, 1),
      Readiness = sprintf("%.0f (%s)", r$readiness_score, r$readiness_status),
      Bottleneck = sprintf("%s @ %.0f%%", r$non_frac_bottleneck, 100 * r$non_frac_p90_utilization),
      Recommendation = r$recommendation,
      `Spread $/d` = sprintf("$%.0fk", r$spread_rate_per_day / 1000)
    )
  }))
}
