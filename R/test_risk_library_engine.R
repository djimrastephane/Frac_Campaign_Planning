# test_risk_library_engine.R
# Property checks for wiring risk_consequence_library into the simulation
# engine (issue #44). Run: Rscript test_risk_library_engine.R
suppressPackageStartupMessages({
  source("load_inputs.R")
  source("validate_risk_consequence_library.R")
  source("engine_core.R")
  source("summaries.R")
  source("report_pdf.R")
  source("optimiser_cascade.R")
  source("risk_library_engine.R")
})

HW   <- load_historical_wells("../data_templates/historical_wells_template.csv")
ASSU <- load_master_assumptions("../data_templates/master_risks_assumptions_template.csv")
LIB  <- read.csv("../data_templates/risk_consequence_library_template_simple_severity.csv",
                  stringsAsFactors = FALSE) %>% validate_risk_consequence_library()

ok <- TRUE
chk <- function(c, m) { cat(sprintf("  [%s] %s\n", if (isTRUE(c)) "PASS" else "FAIL", m)); ok <<- ok && isTRUE(c) }

run <- function(risk_library = NULL, assumptions = ASSU, n_wells = 12, n_iterations = 1000, seed = 42) {
  simulate_campaign_detailed(
    historical_wells = HW, assumptions = assumptions,
    n_wells = n_wells, n_iterations = n_iterations,
    risk_library = risk_library, seed = seed,
    collect_well_details = FALSE
  )
}

res_no_lib  <- run()
res_lib     <- run(risk_library = LIB)

events_no_lib <- unique(res_no_lib$risk_event_log$risk_event)
events_lib    <- unique(res_lib$risk_event_log$risk_event)

# -- 1. A library-only risk now occurs when the library is supplied, and not otherwise
chk("Forklift failure" %in% events_lib, "library-only risk 'Forklift failure' occurs when library is supplied")
chk(!("Forklift failure" %in% events_no_lib), "'Forklift failure' does not occur without the library (no occurrence row)")

# -- 2. The ~5 matched risks (present in both files) still occur with the library present
matched_risks <- c("Plug pressure test failure", "Premature plug set", "Perforation gun misfire",
                    "Isolation plug failure", "CT unit unavailable")
chk(all(matched_risks %in% events_lib), "all matched risks still occur with the library present")
chk(all(matched_risks %in% events_no_lib), "all matched risks occur without the library too (unchanged baseline)")

# -- 3. Severity proportions for a frequent library-only risk track scenario_probability
# "Truck delivery delay": base_probability 0.08, campaign scope, scenario_probability 0.6/0.3/0.1
res_sev <- run(risk_library = LIB, n_wells = 10, n_iterations = 3000, seed = 7)
td <- res_sev$risk_event_log[res_sev$risk_event_log$risk_event == "Truck delivery delay", ]
prop <- table(td$severity) / nrow(td)
chk(nrow(td) > 50, sprintf("enough 'Truck delivery delay' occurrences for a severity-proportion check (n=%d)", nrow(td)))
chk(abs(unname(prop["Minor"])    - 0.6) < 0.12, sprintf("Minor proportion ~0.6 (got %.2f)", prop["Minor"]))
chk(abs(unname(prop["Moderate"]) - 0.3) < 0.12, sprintf("Moderate proportion ~0.3 (got %.2f)", prop["Moderate"]))
chk(abs(unname(prop["Major"])    - 0.1) < 0.12, sprintf("Major proportion ~0.1 (got %.2f)", prop["Major"]))

# -- 4. Determinism: doubling a library-only risk's wireline_runs exactly doubles its
# total sampled extra_wireline_runs under the same seed (same draws, only magnitude changes).
# "Screenout" (library) does not name-match "Screen out" (assumptions) under exact
# normalised-text matching, so it is library-only -- a deliberate property of this design.
LIB_DOUBLED <- LIB
sel <- LIB_DOUBLED$risk_name == "Screenout"
LIB_DOUBLED$wireline_runs[sel] <- LIB_DOUBLED$wireline_runs[sel] * 2

res_base    <- run(risk_library = LIB, n_iterations = 1500, seed = 99)
res_doubled <- run(risk_library = LIB_DOUBLED, n_iterations = 1500, seed = 99)

wl_base    <- sum(res_base$risk_event_log$extra_wireline_runs[res_base$risk_event_log$risk_event == "Screenout"])
wl_doubled <- sum(res_doubled$risk_event_log$extra_wireline_runs[res_doubled$risk_event_log$risk_event == "Screenout"])
chk(wl_base > 0, "baseline 'Screenout' run has nonzero wireline_runs to double (sanity check)")
chk(isTRUE(all.equal(wl_doubled, wl_base * 2)), sprintf("doubling 'Screenout' wireline_runs exactly doubles the total (base=%.3f, doubled=%.3f)", wl_base, wl_doubled))

# -- 5. Precedence: a master_risks_assumptions.csv override still wins over the library
# for a matched risk, regardless of its severity-sampled value.
ASSU_OVR <- ASSU
ovr_row <- which(normalise_text(ASSU_OVR$variable) == "plug pressure test failure")
ASSU_OVR$extra_wireline_runs[ovr_row] <- 99

res_ovr <- run(risk_library = LIB, assumptions = ASSU_OVR, n_iterations = 1000, seed = 11)
pp_events <- res_ovr$risk_event_log[res_ovr$risk_event_log$risk_event == "Plug pressure test failure", ]
chk(nrow(pp_events) > 0, "'Plug pressure test failure' occurred at least once (sanity check)")
chk(all(pp_events$extra_wireline_runs == 99), "CSV override (extra_wireline_runs=99) wins over the library's severity-sampled value")

# -- 6. For a matched risk, occurrence probability still tracks master_risks_assumptions.csv,
# not the library's base_probability, even if the library's value is perturbed far away.
LIB_PERTURBED <- LIB
sel2 <- LIB_PERTURBED$risk_name == "CT unit unavailable"
LIB_PERTURBED$base_probability[sel2] <- 0.95  # assumptions has this risk at 0.05 (campaign scope)

res_perturbed <- run(risk_library = LIB_PERTURBED, n_iterations = 1000, seed = 21)
n_ct <- sum(res_perturbed$risk_event_log$risk_event == "CT unit unavailable")
# Campaign-scope, one draw per iteration: assumptions p=0.05 -> ~50 occurrences in 1000 iters.
# If the engine wrongly used the library's perturbed 0.95, we'd see ~950.
chk(n_ct < 150, sprintf("'CT unit unavailable' occurrence count (%d / 1000) still tracks assumptions' probability (0.05), not the library's perturbed 0.95", n_ct))

# -- 7. logistics_days reaches both the risk_event_log and the campaign-level
# delay rollup (PR 2: "Truck delivery delay" has nonzero logistics_days in
# all three severity tiers: 0.5 / 1.5 / 3).
res_log <- run(risk_library = LIB, n_iterations = 1000, seed = 33)
td_log <- res_log$risk_event_log[res_log$risk_event_log$risk_event == "Truck delivery delay", ]
chk(nrow(td_log) > 0, "'Truck delivery delay' occurred at least once (sanity check)")
chk(sum(td_log$extra_logistics_days) > 0, "'Truck delivery delay' events carry nonzero extra_logistics_days in the risk_event_log")
chk(sum(res_log$summary$total_induced_logistics_days, na.rm = TRUE) > 0,
    "total_induced_logistics_days is positive in the campaign summary rollup")

# Removing the logistics consequence should lower total_frac_workload_days by
# exactly that amount, confirming it reaches the delay rollup, not just the log.
LIB_NO_LOGISTICS <- LIB
LIB_NO_LOGISTICS$logistics_days <- 0
res_no_log <- run(risk_library = LIB_NO_LOGISTICS, n_iterations = 1000, seed = 33)
workload_diff <- sum(res_log$summary$total_frac_workload_days) - sum(res_no_log$summary$total_frac_workload_days)
chk(isTRUE(all.equal(workload_diff, sum(res_log$summary$total_induced_logistics_days), tolerance = 1e-6)),
    sprintf("zeroing logistics_days lowers total_frac_workload_days by exactly the induced-logistics total (diff=%.3f)", workload_diff))

cat(sprintf("\n==== %s ====\n", if (ok) "ALL PROPERTY CHECKS PASS" else "FAILURES ABOVE"))
if (!ok) quit(status = 1)
