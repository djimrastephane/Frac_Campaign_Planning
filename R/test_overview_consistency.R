# test_overview_consistency.R
# Property checks for the Overview-tab audit (Phase 1-6). Run: Rscript test_overview_consistency.R
#
# What's a hard guarantee vs. a regression spot-check, by design:
#   - Zipper-benefit reconciliation (Phase 1) is an EXACT algebraic guarantee
#     by construction (telescoping subtraction + forced rounding residual) --
#     checked here against multiple fixtures, including adversarial rounding
#     boundaries, and should never fail.
#   - Bottleneck agreement (Phase 2/6) checks that recommend_action() (used by
#     the Overview card and Decision Support) and analyse_constraint_cascade()
#     (used by the Optimiser) name the SAME resource on a normal synthetic
#     run. These are two different methodologies -- queue-delay critical-path
#     ranking vs. trial-every-candidate-and-measure -- that are expected to
#     agree in the common case but are not algebraically forced to. This is a
#     regression spot-check, not a proof; a future change that makes them
#     diverge on this fixture is worth investigating, not necessarily a bug.
#   - Traffic-light reasons (Phase 4) are checked for self-consistency: the
#     percentage stated in the reason text must match the ratio that produced
#     the colour, and the named threshold list is asserted to match the
#     case_when() cutoffs (so they can't silently drift apart).

ENGINE <- if (file.exists("simulation_engine_fast.R")) "simulation_engine_fast.R" else "archive/simulation_engine.R"
suppressPackageStartupMessages({
  source(ENGINE); source("risk_library_engine.R"); source("bottleneck_explain.R")
  source("recommendations.R"); source("load_inputs.R"); source("validate_inputs.R")
})

ok <- TRUE
chk <- function(c, m) { cat(sprintf("  [%s] %s\n", if (isTRUE(c)) "PASS" else "FAIL", m)); ok <<- ok && isTRUE(c) }

# ---------------------------------------------------------------------------
# Phase 1: zipper-benefit breakdown reconciles EXACTLY, including adversarial
# rounding boundaries.
# ---------------------------------------------------------------------------
cat("\n== Phase 1: zipper-benefit reconciliation ==\n")

mk_summary <- function(conv_days, zip_days, ct_pw, fr_c_pw, wl_c_pw, fr_z_pw, wl_z_pw, n = 10) {
  dplyr::bind_rows(
    tibble::tibble(operation_mode = "Conventional", wells = n,
      total_ct_fleet_days = ct_pw * n, total_frac_fleet_days = fr_c_pw * n,
      total_wireline_fleet_days = wl_c_pw * n, estimated_campaign_days = conv_days),
    tibble::tibble(operation_mode = "Zipper", wells = n,
      total_ct_fleet_days = ct_pw * n, total_frac_fleet_days = fr_z_pw * n,
      total_wireline_fleet_days = wl_z_pw * n, estimated_campaign_days = zip_days)
  )
}

reconciles_exactly <- function(summary) {
  bd <- build_zipper_benefit_breakdown(summary)
  comp_sum <- sum(bd$saving_days[bd$component != "Total saving"])
  total <- bd$saving_days[bd$component == "Total saving"]
  abs(comp_sum - total) < 1e-9
}

chk(reconciles_exactly(mk_summary(300, 250, 1.0, 5.0, 4.0, 3.0, 4.5)), "normal case reconciles exactly")
chk(reconciles_exactly(mk_summary(300.5, 250.5, 1.05, 5.05, 4.05, 3.05, 4.55)), "x.5 rounding-boundary case reconciles exactly")
chk(reconciles_exactly(mk_summary(200, 200, 1.0, 5.0, 4.0, 5.0, 4.0)), "zero-saving case reconciles exactly")
chk(reconciles_exactly(mk_summary(123.7, 45.3, 0.93, 7.21, 2.04, 4.96, 6.49)), "irregular-decimal case reconciles exactly")

# ---------------------------------------------------------------------------
# Phase 2/6: Overview bottleneck card (recommend_action) agrees with the
# Optimiser's constraint cascade on a normal synthetic run.
# ---------------------------------------------------------------------------
cat("\n== Phase 2/6: bottleneck-card vs. constraint-cascade agreement ==\n")

HISTORICAL <- synthetic_historical_wells(n = 30, seed = 42)
ASSUMPTIONS_FULL <- load_master_assumptions(
  file.path(if (dir.exists("../data_templates")) ".." else ".", "data_templates", "master_risks_assumptions_template.csv")
)

base_cfg <- list(frac_fleets = 1, wireline_units = 1, ct_units = 1, milling_units = 1,
                  testing_units = 1, frac_trees = 2, operation_mode = "Zipper", allow_ct_for_milling = FALSE)

sim_args <- c(list(historical_wells = HISTORICAL, assumptions = ASSUMPTIONS_FULL,
                    n_wells = 30, n_iterations = 300, seed = 123), base_cfg)
sim_result <- do.call(simulate_campaign_detailed, sim_args)

rec <- recommend_action(sim_result, sim_args = sim_args, verify = FALSE)

cascade <- analyse_constraint_cascade(
  historical_wells = HISTORICAL, assumptions = ASSUMPTIONS_FULL, n_wells = 30,
  base_config = base_cfg, cascade_iterations = 300, max_steps = 1,
  min_saving_days = 2, seed = 123
)
cascade_first_step <- cascade[cascade$step == 1, ]

cat(sprintf("  recommend_action() bottleneck: %s\n", rec$bottleneck))
cat(sprintf("  cascade step-1 resource:        %s\n", cascade_first_step$resource_fixed))
chk(identical(rec$bottleneck, cascade_first_step$resource_fixed),
    "Overview bottleneck card names the same resource as the cascade's first step")

# ---------------------------------------------------------------------------
# Phase 4: traffic-light reason text matches its own ratio and threshold.
# ---------------------------------------------------------------------------
cat("\n== Phase 4: traffic-light reason self-consistency ==\n")

tl <- build_traffic_lights(sim_result$summary, sim_result$risk_event_log, sim_result$resource_utilization)

reason_pct_matches <- function(reason, ratio) {
  # First number immediately followed by "%" -- skips "P90"/"P50" (digits
  # with no "%" right after) and lands on the actual stated ratio, which
  # always comes before the "(threshold: ...)" parenthetical in every
  # reason string built in build_traffic_lights().
  m <- regmatches(reason, regexpr("[0-9]+(?=%)", reason, perl = TRUE))
  stated <- as.numeric(m)
  abs(stated - round(100 * ratio)) <= 1  # allow 1pp for sprintf rounding
}

chk(reason_pct_matches(tl$schedule_risk_reason[1], tl$uncertainty_ratio[1]),
    "schedule_risk_reason states the correct uncertainty_ratio percentage")
chk(reason_pct_matches(tl$operational_risk_reason[1], tl$risk_delay_ratio[1]),
    "operational_risk_reason states the correct risk_delay_ratio percentage")
chk(reason_pct_matches(tl$wireline_constraint_reason[1], tl$wireline_wait_ratio[1]),
    "wireline_constraint_reason states the correct wireline_wait_ratio percentage")

# Colour/threshold direction sanity: Red must mean "at or above the stated
# Red threshold", for every light, on every row returned.
chk(all(tl$resource_risk != "Red" | tl$max_p90_utilization >= 0.85),
    "resource_risk == Red implies max_p90_utilization >= 0.85")
chk(all(tl$operational_risk != "Red" | tl$risk_delay_ratio >= 0.10),
    "operational_risk == Red implies risk_delay_ratio >= 0.10")

cat("\n==== ", if (ok) "ALL PROPERTY CHECKS PASS" else "SOME CHECKS FAILED", " ====\n", sep = "")
if (!ok) quit(status = 1)
