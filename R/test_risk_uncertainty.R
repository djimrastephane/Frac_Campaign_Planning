# test_risk_uncertainty.R
# Property checks for risk_uncertainty.R (no bit-identity oracle for net-new
# code, so we assert invariants instead). Run:  Rscript test_risk_uncertainty.R
ENGINE <- if (file.exists("simulation_engine_fast.R")) "simulation_engine_fast.R" else "simulation_engine.R"
suppressPackageStartupMessages({ source(ENGINE); source("risk_library_engine.R"); source("risk_uncertainty.R") })

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
    ~variable,            ~category,              ~type,  ~probability, ~min_days, ~most_likely_days, ~max_days, ~simulation_impact, ~scope,
    "Screenout",          "Frac",                 "risk", 0.08,         0.5,       1.0,               3.0,       "extra stage",      "stage",
    "Milling difficulty", "Milling",              "risk", 0.10,         0.5,       1.0,               3.0,       "extra milling",    "well",
    "Weather delay",      "Weather",              "risk", 0.15,         1.0,       2.0,               5.0,       "schedule delay",   "campaign"
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

# Run two modes; combine to exercise per-mode grouping (milling_units=1 -> tight)
sim <- function(mode, trees) simulate_campaign_detailed(
  HISTORICAL, ASSUMPTIONS, n_wells = 30, n_iterations = 400, milling_units = 1,
  testing_units = 1, operation_mode = mode, frac_trees = trees, seed = 42)
a <- sim("Conventional", 1); b <- sim("Zipper", 2)
res <- list(summary = dplyr::bind_rows(a$summary, b$summary),
            resource_utilization = dplyr::bind_rows(a$resource_utilization, b$resource_utilization))

target <- 200; budget <- 60e6
U <- quantify_uncertainty(res$summary, res$resource_utilization, target_days = target, budget = budget)
R <- predict_campaign_risks(res$summary, res$resource_utilization, target_days = target)

ok <- TRUE
chk <- function(cond, msg) { cat(sprintf("  [%s] %s\n", if (isTRUE(cond)) "PASS" else "FAIL", msg)); ok <<- ok && isTRUE(cond) }

cat("\n== Uncertainty (#6) ==\n")
print(as.data.frame(U[, c("operation_mode","p10_days","p50_days","p90_days",
                          "prob_finish_by_target","prob_schedule_overrun",
                          "prob_within_budget","prob_resource_overload","p50_cost")]), digits = 4)
chk(all(U$p10_days <= U$p50_days + 1e-9) && all(U$p50_days <= U$p90_days + 1e-9), "P10 <= P50 <= P90")
pr <- c(U$prob_finish_by_target, U$prob_schedule_overrun, U$prob_within_budget, U$prob_resource_overload)
chk(all(pr >= -1e-9 & pr <= 1 + 1e-9, na.rm = TRUE), "all probabilities in [0,1]")
chk(all(abs(U$prob_finish_by_target + U$prob_schedule_overrun - 1) < 1e-6), "P(finish) + P(overrun) == 1")
chk(all(U$n_simulations == 400), "n_simulations == 400 per mode")
chk(all(U$p50_cost > 0), "P50 cost priced (> 0)")

cat("\n== Risk prediction (#4) ==\n")
print(as.data.frame(R[R$operation_mode == "Zipper",
      c("risk","probability","expected_delay_days","p90_delay_days","likelihood")]), row.names = FALSE)
chk(all(R$probability >= -1e-9 & R$probability <= 1 + 1e-9), "all risk probabilities in [0,1]")
chk(all(R$expected_delay_days >= -1e-9), "expected delays non-negative")
# exactly-one-binding-per-iteration => resource binding probs sum to ~1 (ties can nudge >1)
bind_sum <- R %>% dplyr::filter(!is.na(resource), !grepl("idle|overrun", risk)) %>%
  dplyr::group_by(operation_mode) %>% dplyr::summarise(s = sum(probability), .groups = "drop")
chk(all(bind_sum$s >= 0.98 & bind_sum$s <= 1.20), "binding probabilities sum ~1 per mode")
chk(nrow(R) >= 6, "rows for 5 resources + frac-idle (+overrun)")

cat(sprintf("\n==== %s ====\n", if (ok) "ALL PROPERTY CHECKS PASS" else "FAILURES ABOVE"))
if (!ok) quit(status = 1)
