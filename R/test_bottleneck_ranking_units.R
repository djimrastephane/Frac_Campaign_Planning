# test_bottleneck_ranking_units.R -- run: Rscript test_bottleneck_ranking_units.R
#
# Regression test for the event-mode bottleneck-ranking units fix.
#
# Defect this pins (reproduced before the fix): in event mode (the default),
# per-well frac/wireline/CT fleet_days held raw pool workload (unit-count-
# blind) while milling/testing were workload/units -- so at a multi-unit
# config the ranking compared incommensurate numbers. At FF=2/WL=2/CT=1/
# ML=2/TU=3, explain_bottlenecks() named "Frac fleet" (analytic ~5 d, Minor)
# while paired ground-truth re-simulation showed +1 testing unit saves ~35 d
# P50 (100% win rate) and +1 frac fleet ~0.1 d. This test FAILS on the old
# code (checks 5-8) and passes on the fix.
ENGINE_FILES <- if (file.exists("engine_core.R")) {
  c("engine_core.R", "summaries.R", "report_pdf.R", "optimiser_cascade.R")
} else "archive/simulation_engine.R"
suppressPackageStartupMessages({
  library(dplyr)
  for (.ef in ENGINE_FILES) source(.ef)
  source("risk_library_engine.R"); source("bottleneck_explain.R")
  source("load_inputs.R")
})

ok <- TRUE
chk <- function(c, m) { cat(sprintf("  [%s] %s\n", if (isTRUE(c)) "PASS" else "FAIL", m)); ok <<- ok && isTRUE(c) }

HW <- synthetic_historical_wells(n = 30, seed = 42)
A  <- load_master_assumptions("../data_templates/master_risks_assumptions_template.csv")

base_args <- list(
  historical_wells = HW, assumptions = A, n_wells = 30L, n_iterations = 300L,
  frac_fleets = 1, wireline_units = 1, ct_units = 1, milling_units = 1,
  testing_units = 1, frac_trees = 2, operation_mode = "Conventional",
  pre_frac_scheduling = "event", seed = 123L,
  keep_logs = FALSE, collect_well_details = FALSE
)
run <- function(a) do.call(simulate_campaign_detailed, a)
res_arg <- c("Frac fleet"="frac_fleets", "Wireline"="wireline_units",
             "CT / cleanout"="ct_units", "Milling"="milling_units",
             "Testing unit"="testing_units")

paired_p50_saving <- function(base, args, resource) {
  a <- args; a[[res_arg[[resource]]]] <- a[[res_arg[[resource]]]] + 1
  new <- run(a)
  paired <- inner_join(base$summary %>% select(simulation_id, b = estimated_campaign_days),
                       new$summary  %>% select(simulation_id, n = estimated_campaign_days),
                       by = "simulation_id")
  as.numeric(quantile(paired$b - paired$n, 0.5))
}

# ---- Semantic invariant: fleet_days == workload / units ----------------------
# Exact for Frac fleet / Wireline / Milling / Testing unit. CT is a documented
# pre-existing exception when wireline_units >= 2: workload_days[CT] is summed
# from the PRE-override ct_workload_days (engine_core.R ~1552, before the
# 2+-wireline cement-eval-offline rule zeroes online cement eval at ~1582-1585),
# while fleet_days uses the post-override value -- in BOTH scheduling modes
# (the formula path is pinned bit-identical to the archive engine with this
# behavior). So for CT: fleet_days <= workload_days / units, with equality
# when wireline_units < 2. The cross-mode check below proves the exception
# is not introduced by the event-mode fix.
invariant_holds <- function(res) {
  ru <- res$resource_utilization
  non_ct <- ru[ru$resource != "CT / cleanout", ]
  ct     <- ru[ru$resource == "CT / cleanout", ]
  max(abs(non_ct$fleet_days_after_resources - non_ct$workload_days / non_ct$units)) < 1e-6 &&
    all(ct$fleet_days_after_resources <= ct$workload_days / ct$units + 1e-6)
}

# ---- 1. One-unit control (all units = 1) -------------------------------------
base1 <- run(base_args)
ex1 <- explain_bottlenecks(base1$summary, base1$resource_utilization)
chk(invariant_holds(base1),
    "control: fleet_days_after_resources == workload_days / units for every resource (units all 1)")
# At wireline_units = 1 the CT exception is inactive: exact equality for CT too.
ct1 <- base1$resource_utilization %>% filter(resource == "CT / cleanout")
chk(max(abs(ct1$fleet_days_after_resources - ct1$workload_days / ct1$units)) < 1e-6,
    "control: CT invariant is exact when wireline_units < 2 (cement-eval override inactive)")
chk(ex1$roles$primary == "Testing unit",
    sprintf("control: primary at all-units-1 is Testing unit (got %s) -- unchanged from pre-fix behavior", ex1$roles$primary))
sv_test1 <- paired_p50_saving(base1, base_args, "Testing unit")
chk(sv_test1 > 50,
    sprintf("control: ground truth agrees -- +1 testing unit saves %.1f d P50 (> 50 d)", sv_test1))

# ---- 2. Pinned multi-unit defect fixture -------------------------------------
mu_args <- modifyList(base_args, list(frac_fleets = 2, wireline_units = 2,
                                      ct_units = 1, milling_units = 2, testing_units = 3))
base_mu <- run(mu_args)
ex_mu <- explain_bottlenecks(base_mu$summary, base_mu$resource_utilization)
rk <- ex_mu$ranking

chk(invariant_holds(base_mu),
    "fixture: fleet_days == workload / units (exact for 4 resources; CT bounded by the documented pre-existing cement-eval quirk)")

# Cross-mode proof the CT exception is pre-existing, not introduced by this
# fix: with the same seed the sampled well data is identical in both modes,
# so CT stream days must match formula mode (whose fleet_days semantics are
# pinned bit-identical to the archive engine by check_regression.R).
base_mu_formula <- run(modifyList(mu_args, list(pre_frac_scheduling = "formula")))
ct_ev <- base_mu$resource_utilization %>% filter(resource == "CT / cleanout") %>%
  summarise(m = mean(fleet_days_after_resources)) %>% pull(m)
ct_fo <- base_mu_formula$resource_utilization %>% filter(resource == "CT / cleanout") %>%
  summarise(m = mean(fleet_days_after_resources)) %>% pull(m)
chk(abs(ct_ev - ct_fo) < 1e-6,
    sprintf("fixture: event-mode CT stream days (%.2f) match formula mode (%.2f) at the same seed", ct_ev, ct_fo))

fd <- setNames(rk$mean_active_days, rk$resource)
chk(fd[["Testing unit"]] > fd[["Frac fleet"]],
    sprintf("fixture: unit-adjusted testing stream (%.1f d) ranks above frac stream (%.1f d)",
            fd[["Testing unit"]], fd[["Frac fleet"]]))
chk(ex_mu$roles$primary != "Frac fleet",
    sprintf("fixture: primary is not Frac fleet named off raw workload (got %s)", ex_mu$roles$primary))
chk(ex_mu$roles$primary == "Testing unit",
    sprintf("fixture: primary is Testing unit (got %s)", ex_mu$roles$primary))

# ---- 3. Ground truth: analytic pick must match measured best relief ----------
sv_testing <- paired_p50_saving(base_mu, mu_args, "Testing unit")
sv_frac    <- paired_p50_saving(base_mu, mu_args, "Frac fleet")
chk(sv_testing > sv_frac + 10,
    sprintf("fixture: measured +1-unit saving: testing %.1f d vs frac %.1f d (testing wins by > 10 d)",
            sv_testing, sv_frac))
chk(ex_mu$roles$primary == "Testing unit" && sv_testing >= sv_frac,
    "fixture: selected bottleneck matches the highest measured-saving candidate")

cat(sprintf("\n==== %s ====\n", if (ok) "ALL PROPERTY CHECKS PASS" else "FAILURES ABOVE"))
if (!ok) quit(status = 1)
