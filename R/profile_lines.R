# profile_lines.R
# Names the exact hot LINES in the engine's per-iteration body, so the Round-2
# rewrite targets precisely what costs time (well_df tibble? the mutates? the
# left_join? the scheduler?). Run:  Rscript profile_lines.R
options(keep.source = TRUE, keep.source.pkgs = TRUE)

ENGINE <- Sys.getenv("ENGINE_PATH", "simulation_engine_fast.R")
stopifnot(file.exists(ENGINE))
suppressPackageStartupMessages(source(ENGINE, keep.source = TRUE))

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
  frac_days_per_stage = round(triangle_sample(0.5, 0.8, 2.5, 30), 3),
  milling_days_per_plug = round(triangle_sample(0.4, 0.7, 1.5, 30), 3)
)

run <- function(keep = TRUE)
  simulate_campaign_detailed(HISTORICAL, ASSUMPTIONS, n_wells = 30, n_iterations = 800,
    milling_units = 2, testing_units = 1, operation_mode = "Zipper", frac_trees = 2, seed = 42,
    keep_logs = keep, collect_well_details = keep)

pf <- tempfile(fileext = ".out")
Rprof(pf, line.profiling = TRUE, interval = 0.004)
invisible(run(TRUE))    # full path: includes risk_log + well_details
Rprof(NULL)

cat("\n== Hottest LINES (engine file only), full keep_logs=TRUE path ==\n")
sr <- summaryRprof(pf, lines = "show")$by.self
eng <- basename(ENGINE)
sr_eng <- sr[grepl(eng, rownames(sr), fixed = TRUE), , drop = FALSE]
print(head(sr_eng, 30))

cat("\n== Same, by function ==\n")
print(head(summaryRprof(pf)$by.self, 15))

cat("\nMap the top line numbers back to simulation_engine_fast.R and send them.\n",
    "Likely suspects: the well_df tibble(...) (~L1146), the 5 mutate() blocks,\n",
    "arrange()+left_join() (~L1344/1397), or schedule_post_frac_milling's tibble.\n", sep = "")
