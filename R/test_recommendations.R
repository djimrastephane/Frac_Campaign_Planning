# test_recommendations.R
# Property checks for recommendations.R. Run: Rscript test_recommendations.R
ENGINE_FILES <- if (file.exists("engine_core.R")) {
  c("engine_core.R", "summaries.R", "report_pdf.R", "optimiser_cascade.R")
} else "archive/simulation_engine.R"
suppressPackageStartupMessages({
  for (.ef in ENGINE_FILES) source(.ef)
  source("risk_library_engine.R")
  source("bottleneck_explain.R"); source("recommendations.R")
})

ASSUMPTIONS <- dplyr::bind_rows(
  tibble::tribble(
    ~variable,                  ~category, ~type,   ~probability, ~min_days, ~most_likely_days, ~max_days, ~simulation_impact, ~scope,
    "Stages per well",          "Param",   "param", NA,           8,         10,                14,        NA,                 NA,
    "Temperature log stages",   "Param",   "param", NA,           1,         2,                 3,         NA,                 NA,
    "Wells per pad",            "Param",   "param", NA,           2,         3,                 4,         NA,                 NA,
    "Cement eval duration",            "Param",   "param", NA,           0.5,       1.0,               2.0,       NA,                 NA,
    "Scraper / cleanout run",   "Param",   "param", NA,           0.3,       0.5,               1.0,       NA,                 NA,
    "Temperature log duration", "Param",   "param", NA,           0.2,       0.3,               0.5,       NA,                 NA,
    "Isolation plug duration",  "Param",   "param", NA,           0.3,       0.5,               1.0,       NA,                 NA,
    "Cement eval offline",             "Param",   "param", 0.8,          0,         0,                 0,         NA,                 NA
  ),
  tibble::tribble(
    ~variable,            ~category, ~type,  ~probability, ~min_days, ~most_likely_days, ~max_days, ~simulation_impact, ~scope,
    "Milling difficulty", "Milling", "risk", 0.10,         0.5,       1.0,               3.0,       "extra milling",    "well"
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

args <- list(historical_wells = HISTORICAL, assumptions = ASSUMPTIONS, n_wells = 30,
             n_iterations = 400, milling_units = 2, testing_units = 1,
             operation_mode = "Zipper", frac_trees = 2, seed = 42)
result <- do.call(simulate_campaign_detailed, args)

rec <- recommend_action(result, sim_args = args, verify = TRUE)
cat("\n== Verified recommendation ==\n"); print_recommendation(rec)

ok <- TRUE
chk <- function(c, m) { cat(sprintf("  [%s] %s\n", if (isTRUE(c)) "PASS" else "FAIL", m)); ok <<- ok && isTRUE(c) }
chk(rec$bottleneck == "Testing unit", "primary bottleneck = Testing unit")
chk(grepl("VERIFIED", rec$basis), "reduction is verified by re-simulation")
chk(rec$confidence >= 0 && rec$confidence <= 1, "confidence in [0,1]")
chk(rec$new_p50_days <= rec$base_p50_days + 1e-9, "adding the binding unit does not increase P50")
chk(is.finite(rec$expected_value), "expected value computed")
chk(nchar(rec$panel) > 0 && grepl("Recommendation:", rec$panel), "panel rendered")

# Analytic fallback path (no re-sim)
rec2 <- recommend_action(result, sim_args = args, verify = FALSE)
chk(grepl("ESTIMATED", rec2$basis), "fallback path is analytic")
chk(rec2$bottleneck == rec$bottleneck, "fallback picks same bottleneck")

# -- Decision thresholds are named and single-sourced (Issue #41)
chk(REC_DECISION_THRESHOLDS$min_p50_reduction_days == 0.5, "min_p50_reduction_days default is 0.5")
chk(REC_DECISION_THRESHOLDS$confidence_moderate_win_rate == 0.75, "confidence_moderate_win_rate default is 0.75")
chk(nchar(rec$decision_reason) > 0, "decision_reason is populated")
if (rec$decision_status == "Recommended") {
  chk(grepl("Both gates cleared", rec$decision_reason), "Recommended: decision_reason cites both gates cleared")
} else if (rec$decision_status == "Optional") {
  chk(grepl("below the", rec$decision_reason), "Optional: decision_reason cites the confidence shortfall")
} else {
  chk(grepl("does not cover|does not clear", rec$decision_reason), "Not justified: decision_reason cites the failing gate")
}

cat(sprintf("\n==== %s ====\n", if (ok) "ALL PROPERTY CHECKS PASS" else "FAILURES ABOVE"))
if (!ok) quit(status = 1)
