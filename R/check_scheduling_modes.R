# check_scheduling_modes.R
# -----------------------------------------------------------------------------
# Side-by-side comparison of pre_frac_scheduling = "formula" (default,
# workload-accounting) vs "event" (real resource-availability-vector
# scheduler, see schedule_pre_frac() in simulation_engine_fast.R).
#
# Unlike check_regression.R, this is NOT a bit-identical check -- the two
# models are EXPECTED to diverge; that divergence is the whole point of the
# new path. This script:
#   1. Runs both modes on the same seed across a small representative grid
#      and prints the campaign-day delta per config, for human review.
#   2. Asserts physical lower bounds that must hold under EITHER model.
#   3. Asserts 2 hand-reasoned directional properties of the event model.
#
# Run (from R/):  Rscript check_scheduling_modes.R
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  source("simulation_engine_fast.R")
  source("risk_library_engine.R")
})

ASSUMPTIONS <- dplyr::bind_rows(
  tibble::tribble(
    ~variable,                  ~category, ~type,   ~probability, ~min_days, ~most_likely_days, ~max_days, ~simulation_impact, ~scope,
    "Stages per well",          "Param",   "param", NA,           8,         10,                14,        NA,                 NA,
    "Temperature log stages",   "Param",   "param", NA,           1,         2,                 3,         NA,                 NA,
    "Wells per pad",            "Param",   "param", NA,           2,         3,                 4,         NA,                 NA,
    "Cement eval duration",     "Param",   "param", NA,           0.5,       1.0,               2.0,       NA,                 NA,
    "Scraper / cleanout run",   "Param",   "param", NA,           0.3,       0.5,               1.0,       NA,                 NA,
    "Temperature log duration", "Param",   "param", NA,           0.2,       0.3,               0.5,       NA,                 NA,
    "Isolation plug duration",  "Param",   "param", NA,           0.3,       0.5,               1.0,       NA,                 NA,
    "Cement eval offline",      "Param",   "param", 0.8,          0,         0,                 0,         NA,                 NA
  ),
  tibble::tribble(
    ~variable,            ~category, ~type,  ~probability, ~min_days, ~most_likely_days, ~max_days, ~simulation_impact, ~scope,
    "Screenout",          "Frac",    "risk", 0.08,         0.5,       1.0,               3.0,       "extra stage",     "stage"
  )
)
set.seed(1)
HISTORICAL <- tibble::tibble(
  well_id = paste0("HW_", 1:30), pad_id = paste0("Pad_", ((1:30 - 1) %/% 3) + 1),
  stages_completed = sample(8:14, 30, TRUE), plugs_installed = sample(8:14, 30, TRUE),
  contingency_plugs = sample(0:2, 30, TRUE), frac_days = round(runif(30, 8, 18), 1),
  cement_eval_days = round(runif(30, 0.5, 2), 2), milling_days = round(runif(30, 5, 12), 1),
  frac_days_per_stage = round(triangle_sample(0.5, 0.8, 2.5, 30), 3),
  milling_days_per_plug = round(triangle_sample(0.4, 0.7, 1.5, 30), 3)
)

run_mode <- function(mode, frac_fleets, wireline_units, operation_mode, frac_trees) {
  simulate_campaign_detailed(
    HISTORICAL, ASSUMPTIONS, n_wells = 12, n_iterations = 300,
    milling_units = 2, testing_units = 1,
    frac_fleets = frac_fleets, wireline_units = wireline_units,
    operation_mode = operation_mode, frac_trees = frac_trees,
    seed = 42, pre_frac_scheduling = mode
  )
}

# ---- 1. Grid comparison (printed for human review) --------------------------
GRID <- expand.grid(
  frac_fleets = c(1, 2, 3), wireline_units = c(1, 2),
  operation_mode = c("Conventional", "Zipper"), stringsAsFactors = FALSE
)
GRID$frac_trees <- ifelse(GRID$operation_mode == "Zipper", 2, 1)

cat("==== Grid comparison: formula vs event mean campaign days ====\n")
ok <- TRUE
chk <- function(c, m) { cat(sprintf("  [%s] %s\n", if (isTRUE(c)) "PASS" else "FAIL", m)); ok <<- ok && isTRUE(c) }

grid_results <- lapply(seq_len(nrow(GRID)), function(i) {
  cfg <- GRID[i, ]
  rf <- run_mode("formula", cfg$frac_fleets, cfg$wireline_units, cfg$operation_mode, cfg$frac_trees)
  re <- run_mode("event",   cfg$frac_fleets, cfg$wireline_units, cfg$operation_mode, cfg$frac_trees)
  f_days <- mean(rf$summary$estimated_campaign_days)
  e_days <- mean(re$summary$estimated_campaign_days)
  cat(sprintf(
    "  %-12s fleets=%d wireline=%d  formula=%6.2f  event=%6.2f  delta=%+6.2f\n",
    cfg$operation_mode, cfg$frac_fleets, cfg$wireline_units, f_days, e_days, e_days - f_days
  ))
  list(cfg = cfg, formula_days = f_days, event_days = e_days,
       formula_summary = rf$summary, event_summary = re$summary)
})

# ---- 2. Physical lower bound: campaign days can never be shorter than the
# total frac workload spread across the fleets, under EITHER model -- a
# violation here would mean a scheduling bug, not a modeling choice.
for (gr in grid_results) {
  total_frac_workload <- mean(gr$event_summary$total_frac_workload_days)
  lower_bound <- total_frac_workload / gr$cfg$frac_fleets
  chk(gr$event_days >= lower_bound - 1e-6,
      sprintf("%s fleets=%d wireline=%d: event campaign days (%.2f) >= physical lower bound (%.2f)",
              gr$cfg$operation_mode, gr$cfg$frac_fleets, gr$cfg$wireline_units, gr$event_days, lower_bound))
}

# ---- 3. Hand-reasoned directional checks on the event model -----------------
# Dropping wireline_units (holding everything else fixed) must never DECREASE
# campaign days under the event model -- less wireline capacity can only add
# contention, never remove it. The formula path has no such guarantee in
# conventional mode (it hardcodes wireline_readiness_delay_days to 0 there),
# which is exactly the gap this scheduler closes.
conv_1u <- Filter(function(g) g$cfg$operation_mode == "Conventional" && g$cfg$wireline_units == 1, grid_results)
conv_2u <- Filter(function(g) g$cfg$operation_mode == "Conventional" && g$cfg$wireline_units == 2, grid_results)
for (i in seq_along(conv_1u)) {
  fleets <- conv_1u[[i]]$cfg$frac_fleets
  match2u <- Filter(function(g) g$cfg$frac_fleets == fleets, conv_2u)[[1]]
  chk(conv_1u[[i]]$event_days >= match2u$event_days - 1e-6,
      sprintf("Conventional, fleets=%d: event mode with 1 wireline unit (%.2f d) is never faster than with 2 (%.2f d)",
              fleets, conv_1u[[i]]$event_days, match2u$event_days))
}

# Increasing frac_fleets (holding wireline fixed) must never INCREASE event
# campaign days -- more frac capacity can only help or be neutral.
for (wl in c(1, 2)) {
  rows <- Filter(function(g) g$cfg$operation_mode == "Conventional" && g$cfg$wireline_units == wl, grid_results)
  rows <- rows[order(sapply(rows, function(g) g$cfg$frac_fleets))]
  for (i in seq_len(length(rows) - 1)) {
    chk(rows[[i]]$event_days >= rows[[i + 1]]$event_days - 1e-6,
        sprintf("Conventional, wireline=%d: event days non-increasing as frac_fleets goes %d -> %d (%.2f -> %.2f)",
                wl, rows[[i]]$cfg$frac_fleets, rows[[i + 1]]$cfg$frac_fleets,
                rows[[i]]$event_days, rows[[i + 1]]$event_days))
  }
}

cat(sprintf("\n==== %s ====\n", if (ok) "ALL CHECKS PASS" else "FAILURES ABOVE"))
if (!ok) quit(status = 1)
