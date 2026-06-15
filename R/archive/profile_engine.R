# profile_engine.R
# -----------------------------------------------------------------------------
# Self-contained profiling harness for the frac campaign Monte Carlo engine.
# Builds VALID synthetic inputs (no CSVs needed) and measures where time goes,
# so the perf-refactor decision (parallelise vs base-R rewrite of the hot body)
# is driven by data, not guesswork.
#
# Usage:
#   Rscript profile_engine.R                 # bench timings + Rprof summary
#   (interactive) source("profile_engine.R") # also writes profvis HTML if avail
#
# Place next to simulation_engine.R, or set ENGINE_PATH below.
# -----------------------------------------------------------------------------

ENGINE_PATH <- Sys.getenv("ENGINE_PATH", "simulation_engine.R")
stopifnot(file.exists(ENGINE_PATH))
suppressPackageStartupMessages(source(ENGINE_PATH))

# ---- Synthetic inputs (schema matches what the engine actually queries) -----

make_assumptions <- function() {
  # Non-risk parameter rows the engine looks up by name (normalise_text-matched).
  params <- tibble::tribble(
    ~variable,                  ~category, ~type,    ~probability, ~min_days, ~most_likely_days, ~max_days, ~simulation_impact, ~scope,
    "Stages per well",          "Param",   "param",  NA,           8,         10,                14,        NA,                 NA,
    "Temperature log stages",   "Param",   "param",  NA,           1,         2,                 3,         NA,                 NA,
    "Wells per pad",            "Param",   "param",  NA,           2,         3,                 4,         NA,                 NA,
    "SCMT duration",            "Param",   "param",  NA,           0.5,       1.0,               2.0,       NA,                 NA,
    "Scraper / cleanout run",   "Param",   "param",  NA,           0.3,       0.5,               1.0,       NA,                 NA,
    "Temperature log duration", "Param",   "param",  NA,           0.2,       0.3,               0.5,       NA,                 NA,
    "Isolation plug duration",  "Param",   "param",  NA,           0.3,       0.5,               1.0,       NA,                 NA,
    "SCMT offline",             "Param",   "param",  0.8,          0,         0,                 0,         NA,                 NA
  )
  # Risk rows across resource classes + scopes (well / stage / campaign).
  risks <- tibble::tribble(
    ~variable,            ~category,               ~type,  ~probability, ~min_days, ~most_likely_days, ~max_days, ~simulation_impact,        ~scope,
    "Screenout",          "Frac",                  "risk", 0.08,         0.5,       1.0,               3.0,       "extra stage",             "stage",
    "Gun misfire",        "Wireline/Perforation",  "risk", 0.05,         0.2,       0.5,               1.0,       "additional run",          "stage",
    "Isolation plug fail","Plug",                  "risk", 0.04,         0.3,       0.8,               2.0,       "replacement plug",        "well",
    "Cement issue",       "CT Unit / cleanout",    "risk", 0.06,         0.5,       1.0,               2.0,       "ct intervention",         "well",
    "Milling difficulty", "Milling",               "risk", 0.10,         0.5,       1.0,               3.0,       "extra milling",           "well",
    "Weather delay",      "Weather",               "risk", 0.15,         1.0,       2.0,               5.0,       "schedule delay",          "campaign",
    "Permit delay",       "Regulatory/permit",     "risk", 0.10,         2.0,       3.0,               7.0,       "schedule delay",          "campaign"
  )
  dplyr::bind_rows(params, risks)
}

make_historical <- function(n = 30, seed = 1) {
  set.seed(seed)
  tibble::tibble(
    well_id              = paste0("HW_", seq_len(n)),
    pad_id               = paste0("Pad_", ((seq_len(n) - 1) %/% 3) + 1),
    stages_completed     = sample(8:14, n, TRUE),
    plugs_installed      = sample(8:14, n, TRUE),
    contingency_plugs    = sample(0:2, n, TRUE),
    frac_days            = round(runif(n, 8, 18), 1),
    scmt_days            = round(runif(n, 0.5, 2), 2),
    milling_days         = round(runif(n, 5, 12), 1),
    frac_days_per_stage  = round(triangle_sample(0.5, 0.8, 2.5, n), 3),
    milling_days_per_plug= round(triangle_sample(0.4, 0.7, 1.5, n), 3)
  )
}

ASSUMPTIONS <- make_assumptions()
HISTORICAL  <- make_historical(30)

SCENARIO_GRID <- expand.grid(
  operation_mode      = c("Conventional", "Zipper"),
  milling_units       = 1:3,
  testing_units       = 1:2,
  stringsAsFactors    = FALSE
)
SCENARIO_GRID$frac_fleets   <- 1
SCENARIO_GRID$wireline_units<- 1
SCENARIO_GRID$ct_units      <- 1
SCENARIO_GRID$frac_trees    <- ifelse(SCENARIO_GRID$operation_mode == "Zipper", 2, 1)
SCENARIO_GRID$allow_ct_for_milling <- FALSE

# ---- Harness wrappers --------------------------------------------------------

run_single <- function(n_iter = 1000) {
  simulate_campaign_detailed(
    historical_wells = HISTORICAL, assumptions = ASSUMPTIONS,
    n_wells = 30, n_iterations = n_iter,
    milling_units = 2, testing_units = 1, operation_mode = "Zipper",
    frac_trees = 2, seed = 42
  )
}

run_optimiser <- function() {
  optimise_campaign_scenarios(
    historical_wells = HISTORICAL, assumptions = ASSUMPTIONS, n_wells = 30,
    scenario_grid = SCENARIO_GRID,
    screen_iterations = 150, refine_iterations = 600, top_n_refine = 5,
    seed = 123
  )
}

# ---- Timing ------------------------------------------------------------------

cat(sprintf("\nGrid configs: %d | cores detected: %d\n\n",
            nrow(SCENARIO_GRID), parallel::detectCores()))

if (requireNamespace("bench", quietly = TRUE)) {
  cat("== bench::mark ==\n")
  b <- bench::mark(
    single_1k   = run_single(1000),
    optimiser   = run_optimiser(),
    iterations  = 3, check = FALSE, filter_gc = FALSE
  )
  print(b[, c("expression", "min", "median", "mem_alloc", "n_gc")])
} else {
  cat("== system.time (install 'bench' for better stats) ==\n")
  cat("single_1k : "); print(system.time(run_single(1000)))
  cat("optimiser : "); print(system.time(run_optimiser()))
}

# ---- Where the time goes (Rprof; this is the decision-maker) -----------------
cat("\n== Rprof line profile of one optimiser run ==\n")
pf <- tempfile(fileext = ".out")
Rprof(pf, line.profiling = TRUE, interval = 0.005)
invisible(run_optimiser())
Rprof(NULL)
print(head(summaryRprof(pf, lines = "show")$by.self, 20))
cat("\n-- by function --\n")
print(head(summaryRprof(pf)$by.self, 20))

# ---- Optional interactive flame graph ---------------------------------------
if (interactive() && requireNamespace("profvis", quietly = TRUE)) {
  pv <- profvis::profvis(run_optimiser())
  htmlwidgets::saveWidget(pv, "profile_optimiser.html", selfcontained = TRUE)
  cat("\nWrote profile_optimiser.html\n")
}

cat("\nInterpretation:\n",
    " - If 'mutate'/'case_when'/'eval_tidy'/tibble construction dominate by.self,\n",
    "   the per-iteration dplyr body (rank 2) is the real CPU cost -> base-R rewrite.\n",
    " - If self-time is spread thin and total runtime scales ~linearly with\n",
    "   nrow(scenario_grid), you are I/O-light and CPU-bound on independent work\n",
    "   -> parallelism (rank 1) is the cheapest win. Usually both are true.\n", sep = "")
