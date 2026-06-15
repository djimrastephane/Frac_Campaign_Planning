# profile_engine_fast.R
# Quick version of profile_engine.R: 1 bench pass, smaller workloads.
# Same tables, same conclusion, finishes in seconds.
# Run:  Rscript profile_engine_fast.R   (next to simulation_engine.R)

ENGINE_PATH <- Sys.getenv("ENGINE_PATH", "simulation_engine.R")
stopifnot(file.exists(ENGINE_PATH))
suppressPackageStartupMessages(source(ENGINE_PATH))

# ---- Synthetic inputs (identical schema to profile_engine.R) ----------------
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
  well_id = paste0("HW_", 1:30),
  pad_id  = paste0("Pad_", ((1:30 - 1) %/% 3) + 1),
  stages_completed = sample(8:14, 30, TRUE), plugs_installed = sample(8:14, 30, TRUE),
  contingency_plugs = sample(0:2, 30, TRUE), frac_days = round(runif(30, 8, 18), 1),
  scmt_days = round(runif(30, 0.5, 2), 2), milling_days = round(runif(30, 5, 12), 1),
  frac_days_per_stage = round(triangle_sample(0.5, 0.8, 2.5, 30), 3),
  milling_days_per_plug = round(triangle_sample(0.4, 0.7, 1.5, 30), 3)
)

# Smaller grid: 6 configs instead of 12
SCENARIO_GRID <- expand.grid(
  operation_mode = c("Conventional", "Zipper"), milling_units = 1:3,
  stringsAsFactors = FALSE)
SCENARIO_GRID$testing_units <- 1
SCENARIO_GRID$frac_fleets <- 1; SCENARIO_GRID$wireline_units <- 1; SCENARIO_GRID$ct_units <- 1
SCENARIO_GRID$frac_trees <- ifelse(SCENARIO_GRID$operation_mode == "Zipper", 2, 1)
SCENARIO_GRID$allow_ct_for_milling <- FALSE

# Lighter iteration counts
run_single <- function(n_iter = 300)
  simulate_campaign_detailed(HISTORICAL, ASSUMPTIONS, n_wells = 30, n_iterations = n_iter,
    milling_units = 2, testing_units = 1, operation_mode = "Zipper", frac_trees = 2, seed = 42)

run_optimiser <- function()
  optimise_campaign_scenarios(HISTORICAL, ASSUMPTIONS, n_wells = 30,
    scenario_grid = SCENARIO_GRID, screen_iterations = 40, refine_iterations = 120,
    top_n_refine = 3, seed = 123)

cat(sprintf("\nGrid configs: %d | cores: %d\n\n", nrow(SCENARIO_GRID), parallel::detectCores()))

cat("== timings (1 pass) ==\n")
cat("single_300 : "); print(system.time(run_single(300))["elapsed"])
cat("optimiser  : "); print(system.time(run_optimiser())["elapsed"])

cat("\n== Rprof by.self (the decision-maker) ==\n")
pf <- tempfile(fileext = ".out")
Rprof(pf, line.profiling = TRUE, interval = 0.005)
invisible(run_optimiser())
Rprof(NULL)
cat("-- by function --\n")
print(head(summaryRprof(pf)$by.self, 15))

cat("\nRead it:\n",
    " mutate/case_when/eval_tidy/[.tbl/tibble at top -> dplyr loop body is the\n",
    " cost -> base-R rewrite pays off. Spread thin across many funcs -> it's just\n",
    " volume -> parallelism is the win. Usually both.\n", sep = "")
