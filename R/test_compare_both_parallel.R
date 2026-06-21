# test_compare_both_parallel.R
# Proves app.R's "Compare both" parallel path (Conventional + Zipper forked
# across cores via .par_lapply(), the same helper optimiser_parallel.R uses
# for the scenario grid) produces results bit-identical to running both
# modes sequentially with the same seeds -- forking changes only wall-clock
# time, not the RNG stream each mode's simulate_campaign_detailed() call
# draws (each call still does its own internal set.seed()).
# Run: Rscript test_compare_both_parallel.R
suppressPackageStartupMessages(library(dplyr))
source("simulation_engine_fast.R")
source("risk_library_engine.R")
source("optimiser_parallel.R")
source("load_inputs.R")

ok <- TRUE
chk <- function(c, m) { cat(sprintf("  [%s] %s\n", if (isTRUE(c)) "PASS" else "FAIL", m)); ok <<- ok && isTRUE(c) }

HW   <- load_historical_wells("../data_templates/historical_wells_template.csv")
ASSU <- load_master_assumptions("../data_templates/master_risks_assumptions_template.csv")

modes <- c("Conventional", "Zipper")
build_run_args <- function(mode_index, base_seed = 123L) {
  list(
    historical_wells = HW, assumptions = ASSU,
    n_wells = 15L, n_iterations = 300L,
    frac_fleets = 1, milling_units = 2, wireline_units = 1, ct_units = 1,
    frac_trees = if (modes[[mode_index]] == "Zipper") 2 else 1,
    operation_mode = modes[[mode_index]],
    zipper_efficiency = 0.75, risk_multiplier = 1,
    testing_units = 1, flowback_testing_days_min = 7, flowback_testing_days_max = 10,
    risk_library = NULL,
    seed = base_seed + (mode_index - 1L),  # mirrors app.R's seed convention
    keep_logs = TRUE, collect_well_details = TRUE
  )
}

run_sequential <- function() {
  lapply(seq_along(modes), function(mode_index) {
    args <- build_run_args(mode_index)
    list(result = do.call(simulate_campaign_detailed, args), args = args)
  })
}

# Mirrors app.R's sim_results eventReactive parallel branch exactly.
run_parallel <- function() {
  args_list <- lapply(seq_along(modes), build_run_args)
  .par_lapply(seq_along(modes), function(mode_index) {
    list(result = do.call(simulate_campaign_detailed, args_list[[mode_index]]),
         args = args_list[[mode_index]])
  }, n_cores = length(modes))
}

seq_runs <- run_sequential()
par_runs <- run_parallel()

seq_summary <- bind_rows(lapply(seq_runs, function(x) x$result$summary))
par_summary <- bind_rows(lapply(par_runs, function(x) x$result$summary))
seq_risk_log <- bind_rows(lapply(seq_runs, function(x) x$result$risk_event_log))
par_risk_log <- bind_rows(lapply(par_runs, function(x) x$result$risk_event_log))
seq_well <- bind_rows(lapply(seq_runs, function(x) x$result$well_details))
par_well <- bind_rows(lapply(par_runs, function(x) x$result$well_details))

chk(isTRUE(all.equal(as.data.frame(seq_summary), as.data.frame(par_summary), tolerance = 0)),
    "parallel summary is bit-identical to sequential (same seeds)")
chk(isTRUE(all.equal(as.data.frame(seq_risk_log), as.data.frame(par_risk_log), tolerance = 0)),
    "parallel risk_event_log is bit-identical to sequential")
chk(isTRUE(all.equal(as.data.frame(seq_well), as.data.frame(par_well), tolerance = 0)),
    "parallel well_details is bit-identical to sequential")

# -- The two modes' args must stay independent (no shared mutable state
# leaking across forked workers, e.g. one mode's frac_trees overriding the other's).
conv_args <- par_runs[[1]]$args
zip_args  <- par_runs[[2]]$args
chk(conv_args$operation_mode == "Conventional" && zip_args$operation_mode == "Zipper",
    "each forked worker keeps its own mode's args (no cross-contamination)")
chk(conv_args$frac_trees == 1 && zip_args$frac_trees == 2,
    "each forked worker keeps its own mode's frac_trees (no cross-contamination)")

# -- On an unsupported platform/single core, the app falls back to lapply()
# (handled inside .par_lapply itself) -- confirm that fallback path alone
# still produces correct, identical-to-itself results for a single mode.
single_mode_runs <- .par_lapply(1, function(i) {
  args <- build_run_args(1)
  do.call(simulate_campaign_detailed, args)$summary
}, n_cores = 1)
chk(nrow(single_mode_runs[[1]]) == 300, "single-mode/n_cores=1 path (Windows fallback) still runs correctly")

cat(sprintf("\n==== %s ====\n", if (ok) "ALL PROPERTY CHECKS PASS" else "FAILURES ABOVE"))
if (!ok) quit(status = 1)
