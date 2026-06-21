# test_bottleneck.R
# Property checks for bottleneck_explain.R. Run: Rscript test_bottleneck.R
ENGINE <- if (file.exists("simulation_engine_fast.R")) "simulation_engine_fast.R" else "archive/simulation_engine.R"
suppressPackageStartupMessages({ source(ENGINE); source("risk_library_engine.R"); source("bottleneck_explain.R") })

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
r <- simulate_campaign_detailed(HISTORICAL, ASSUMPTIONS, n_wells = 30, n_iterations = 500,
       milling_units = 1, testing_units = 1, operation_mode = "Zipper", frac_trees = 2, seed = 42)

ex <- explain_bottlenecks(r$summary, r$resource_utilization)
ok <- TRUE
chk <- function(c, m) { cat(sprintf("  [%s] %s\n", if (isTRUE(c)) "PASS" else "FAIL", m)); ok <<- ok && isTRUE(c) }

cat("\n== Ranking ==\n")
print(as.data.frame(ex$ranking[, c("resource","bottleneck_rank","mean_active_days",
      "p90_utilization","queue_delay_contribution_days","status")]), digits = 4, row.names = FALSE)
cat("\n== Constraint cascade ==\n")
print(as.data.frame(ex$cascade[, c("pos","modal_resource","modal_prob","mean_gap_days",
      "cumulative_recoverable_days")]), digits = 4, row.names = FALSE)
cat("\n== Narrative ==\n"); print_bottleneck_explanation(ex)

chk(all(diff(ex$cascade$cumulative_recoverable_days) >= -1e-9), "cascade cumulative non-decreasing")
chk(all(ex$ranking$queue_delay_contribution_days >= -1e-9), "delay contributions non-negative")
chk(nrow(ex$roles) == 1 && !is.na(ex$roles$primary), "primary identified")
chk(ex$roles$secondary != ex$roles$primary, "secondary differs from primary")
chk(ex$ranking$bottleneck_rank[which.max(ex$ranking$queue_delay_contribution_days)] == 1,
    "rank 1 == largest delay contribution")
chk(all(ex$ranking$p90_utilization >= -1e-9), "utilizations present")

cat(sprintf("\n==== %s ====\n", if (ok) "ALL PROPERTY CHECKS PASS" else "FAILURES ABOVE"))
if (!ok) quit(status = 1)
