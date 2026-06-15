# check_regression.R
# -----------------------------------------------------------------------------
# Proves simulation_engine_fast.R is numerically identical to the original
# simulation_engine.R, then reports the speedup. Round-1 perf changes only
# touched output *construction* (per-iteration tibble -> list/data.frame +
# columnar assembly) and added keep_logs/collect_well_details skip flags; no
# arithmetic and no RNG draw was altered, so results must match to fp precision.
#
# Run (with BOTH files + this script in the same folder):
#   Rscript check_regression.R
#
# PASS criterion: every line prints IDENTICAL and the final line says ALL PASS.
# Do NOT adopt the fast engine in app.R unless this passes.
# -----------------------------------------------------------------------------

stopifnot(file.exists("simulation_engine.R"), file.exists("simulation_engine_fast.R"))

orig <- new.env(); sys.source("simulation_engine.R",      envir = orig)
fast <- new.env(); sys.source("simulation_engine_fast.R", envir = fast)

# ---- synthetic inputs (same schema the engine queries) ----------------------
ASSUMPTIONS <- dplyr::bind_rows(
  tibble::tribble(
    ~variable,                  ~category, ~type,   ~probability, ~min_days, ~most_likely_days, ~max_days, ~simulation_impact, ~scope,
    "Stages per well",          "Param",   "param", NA,           8,         10,                14,        NA,                 NA,
    "Temperature log stages",   "Param",   "param", NA,           1,         2,                 3,         NA,                 NA,
    "Wells per pad",            "Param",   "param", NA,           2,         3,                 4,         NA,                 NA,
    "SCMT duration",            "Param",   "param", NA,           0.5,       1.0,               2.0,       NA,                 NA,
    "Scraper / cleanout run",   "Param",   "param", NA,           0.3,       0.5,               1.0,       NA,                 NA,
    "Temperature log duration", "Param",   "param", NA,           0.2,       0.3,               0.5,       NA,                 NA,
    "Isolation plug duration",  "Param",   "param", NA,           0.3,       0.5,               1.0,       NA,                 NA,
    "SCMT offline",             "Param",   "param", 0.8,          0,         0,                 0,         NA,                 NA
  ),
  tibble::tribble(
    ~variable,            ~category,              ~type,  ~probability, ~min_days, ~most_likely_days, ~max_days, ~simulation_impact, ~scope,
    "Screenout",          "Frac",                 "risk", 0.08,         0.5,       1.0,               3.0,       "extra stage",      "stage",
    "Gun misfire",        "Wireline/Perforation", "risk", 0.05,         0.2,       0.5,               1.0,       "additional run",   "stage",
    "Isolation plug fail","Plug",                 "risk", 0.04,         0.3,       0.8,               2.0,       "replacement plug", "well",
    "Cement issue",       "CT Unit / cleanout",   "risk", 0.06,         0.5,       1.0,               2.0,       "ct intervention",  "well",
    "Milling difficulty", "Milling",              "risk", 0.10,         0.5,       1.0,               3.0,       "extra milling",    "well",
    "Weather delay",      "Weather",              "risk", 0.15,         1.0,       2.0,               5.0,       "schedule delay",   "campaign"
  )
)
set.seed(1)
HISTORICAL <- tibble::tibble(
  well_id = paste0("HW_", 1:30), pad_id = paste0("Pad_", ((1:30 - 1) %/% 3) + 1),
  stages_completed = sample(8:14, 30, TRUE), plugs_installed = sample(8:14, 30, TRUE),
  contingency_plugs = sample(0:2, 30, TRUE), frac_days = round(runif(30, 8, 18), 1),
  scmt_days = round(runif(30, 0.5, 2), 2), milling_days = round(runif(30, 5, 12), 1),
  frac_days_per_stage = round(orig$triangle_sample(0.5, 0.8, 2.5, 30), 3),
  milling_days_per_plug = round(orig$triangle_sample(0.4, 0.7, 1.5, 30), 3)
)
GRID <- expand.grid(operation_mode = c("Conventional", "Zipper"), milling_units = 1:3,
                    stringsAsFactors = FALSE)
GRID$testing_units <- 1; GRID$frac_fleets <- 1; GRID$wireline_units <- 1; GRID$ct_units <- 1
GRID$frac_trees <- ifelse(GRID$operation_mode == "Zipper", 2, 1)
GRID$allow_ct_for_milling <- FALSE

# ---- comparison helper -------------------------------------------------------
all_ok <- TRUE
cmp <- function(a, b, what, tol = 1e-12) {
  r <- all.equal(as.data.frame(a), as.data.frame(b), tolerance = tol)
  ok <- isTRUE(r)
  cat(sprintf("  %-26s %s\n", what, if (ok) "IDENTICAL" else "*** DIFF ***"))
  if (!ok) cat("     ", paste(utils::head(r, 6), collapse = "\n      "), "\n")
  all_ok <<- all_ok && ok
  invisible(ok)
}

run_args <- list(historical_wells = HISTORICAL, assumptions = ASSUMPTIONS,
                 n_wells = 30, n_iterations = 500, milling_units = 2, testing_units = 1,
                 operation_mode = "Zipper", frac_trees = 2, seed = 42)

cat("\n[1] Full single sim (keep_logs=TRUE) -- fast must match original\n")
o <- do.call(orig$simulate_campaign_detailed, run_args)
f <- do.call(fast$simulate_campaign_detailed, run_args)
cmp(o$summary,              f$summary,              "summary")
cmp(o$resource_utilization, f$resource_utilization, "resource_utilization")
cmp(o$well_details,         f$well_details,         "well_details")
cmp(o$risk_event_log,       f$risk_event_log,       "risk_event_log")

cat("\n[2] Skip-flags path: fast(keep_logs=FALSE) $summary must equal original $summary\n")
f2 <- do.call(fast$simulate_campaign_detailed,
              c(run_args, list(keep_logs = FALSE, collect_well_details = FALSE)))
cmp(o$summary, f2$summary, "summary (flags off)")
cat(sprintf("  %-26s %s\n", "well_details empty?",
            if (nrow(f2$well_details) == 0) "yes (expected)" else "*** no ***"))

cat("\n[3] Optimiser end-to-end -- scored results must match\n")
opt_args <- list(historical_wells = HISTORICAL, assumptions = ASSUMPTIONS, n_wells = 30,
                 scenario_grid = GRID, screen_iterations = 80, refine_iterations = 200,
                 top_n_refine = 3, seed = 123)
oo <- do.call(orig$optimise_campaign_scenarios, opt_args)
ff <- do.call(fast$optimise_campaign_scenarios, opt_args)
keep <- c("config_label", "p50_days", "p90_days", "idle_days", "total_mobilisation_cost")
cmp(oo[order(oo$config_label), keep], ff[order(ff$config_label), keep], "optimiser scores")

# ---- speedup -----------------------------------------------------------------
cat("\n[4] Speedup (single core)\n")
t_o  <- system.time(do.call(orig$simulate_campaign_detailed, run_args))[["elapsed"]]
t_f  <- system.time(do.call(fast$simulate_campaign_detailed, run_args))[["elapsed"]]
t_oo <- system.time(do.call(orig$optimise_campaign_scenarios, opt_args))[["elapsed"]]
t_ff <- system.time(do.call(fast$optimise_campaign_scenarios, opt_args))[["elapsed"]]
cat(sprintf("  single sim : %.2fs -> %.2fs  (%.2fx)\n", t_o, t_f, t_o / t_f))
cat(sprintf("  optimiser  : %.2fs -> %.2fs  (%.2fx)\n", t_oo, t_ff, t_oo / t_ff))

cat(sprintf("\n==== %s ====\n", if (all_ok) "ALL PASS - fast engine is numerically identical" else
                                "FAILURES ABOVE - do not adopt; send me the DIFF output"))
if (!all_ok) quit(status = 1)
