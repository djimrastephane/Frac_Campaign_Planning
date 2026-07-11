# test_optimiser_explain.R -- run: Rscript test_optimiser_explain.R
#
# Phase 6.C of the optimiser auditability pass: automated scenario
# explanations (explain_optimiser_scenario() / annotate_optimiser_explanations()).
#
# Two kinds of fixture are used, deliberately:
#  - REAL simulate_campaign_detailed()/optimise_campaign_scenarios() runs for
#    the explanation types that occur naturally and often in this engine's
#    output: "governing_relieved" (testing/milling additions on the binding
#    post-frac path), "queue_only" (wireline additions off that path), and
#    "tie" (byte-identical P50 under common random numbers). These are the
#    types the original CSV investigation actually found.
#  - Hand-built synthetic `results` tibbles (same pattern already used for
#    the pure classifier in test_optimiser_binding_path.R and the manifest
#    in test_optimiser_manifest.R) for "never_used", "negligible", and
#    "baseline". explain_optimiser_scenario() is a pure function of `results`
#    -- it never calls the simulator -- so a synthetic fixture is not a
#    weaker test than a real one, and it's the only reliable way to hit
#    these three branches on demand: real grids only produce them when a
#    resource's marginal effect happens to fall in a narrow, hard-to-target
#    band (small-but-nonzero P50 delta, or missing/near-zero utilization
#    evidence), as confirmed by exploratory grid search during this pass.
ENGINE_FILES <- if (file.exists("engine_core.R")) {
  c("engine_core.R", "summaries.R", "report_pdf.R")
} else "archive/simulation_engine.R"
suppressPackageStartupMessages({
  library(dplyr); library(tibble)
  for (.ef in ENGINE_FILES) source(.ef)
  source("constants.R"); source("optimiser_explain.R")
  source("risk_library_engine.R"); source("load_inputs.R")
  source("optimiser_cascade.R")
})

ok <- TRUE
chk <- function(c, m) { cat(sprintf("  [%s] %s\n", if (isTRUE(c)) "PASS" else "FAIL", m)); ok <<- ok && isTRUE(c) }

# =====================================================================
# Part A -- real simulation grid: governing_relieved / queue_only / tie
# =====================================================================
HW <- synthetic_historical_wells(n = 30, seed = 42)
A  <- load_master_assumptions("../data_templates/master_risks_assumptions_template.csv")

grid <- expand.grid(
  operation_mode = "Zipper", frac_fleets = 1, wireline_units = c(1, 2, 3),
  ct_units = 1, milling_units = c(1, 2, 3), testing_units = c(1, 2, 3),
  frac_trees = 2, allow_ct_for_milling = FALSE, stringsAsFactors = FALSE
)
res <- optimise_campaign_scenarios(
  historical_wells = HW, assumptions = A, n_wells = 30, scenario_grid = grid,
  screen_iterations = 150, refine_iterations = 400, top_n_refine = 6, seed = 123
)
res <- annotate_optimiser_explanations(res)
details <- lapply(seq_len(nrow(res)), function(i) explain_optimiser_scenario(res, i))
types <- vapply(details, function(d) d$type, character(1))

chk("governing_relieved" %in% types,
    "grid produces at least one 'governing_relieved' scenario (a resource addition on the binding path shortened P50)")
chk("queue_only" %in% types,
    "grid produces at least one 'queue_only' scenario (utilization dropped but the OTHER path still governed, so P50 barely moved)")
chk("tie" %in% types,
    "grid produces at least one exact-tie scenario (byte-identical P50 under common random numbers)")
chk("baseline" %in% types,
    "grid's floor configuration (all resources at 1, frac_trees at its floor) has no single-resource parent -> 'baseline'")

gi <- which(types == "governing_relieved")[1]
chk(details[[gi]]$detail$p50_delta_days > 0,
    sprintf("governing_relieved example: p50_delta_days = %.2f (positive = the added unit shortened the campaign)",
            details[[gi]]$detail$p50_delta_days))
chk(grepl("bound in", details[[gi]]$short) && grepl("reduced P50", details[[gi]]$short),
    "governing_relieved 'why' text names the path that was governing and states the P50 reduction")

qi <- which(types == "queue_only")[1]
chk(details[[qi]]$detail$utilization_drop_pp >= 100 * OPTIMISER_UNIT_USE_UTIL_DROP_PP,
    sprintf("queue_only example: utilization_drop_pp = %.1f (>= the %.0f pp 'meaningfully used' threshold)",
            details[[qi]]$detail$utilization_drop_pp, 100 * OPTIMISER_UNIT_USE_UTIL_DROP_PP))
chk(abs(details[[qi]]$detail$p50_delta_days) < OPTIMISER_NEGLIGIBLE_DAYS,
    "queue_only example: P50 barely moved, below the negligible-change threshold -- consistent with 'queued locally, didn't change the campaign'")

ti <- which(types == "tie")[1]
chk(!is.null(details[[ti]]$detail$tie_group_size) && details[[ti]]$detail$tie_group_size > 1,
    "tie example carries a tie_group_size > 1 in its evidence detail")
chk(grepl("Same P50", details[[ti]]$short) && grepl("more expensive", details[[ti]]$short),
    "tie 'why' text names the cheaper equivalent and the cost difference")
chk(details[[ti]]$detail$cost_delta_M > 0,
    "tie example's cost_delta_M is positive (this row IS the more expensive of the pair, by construction of which() picked it)")

# ---- Every annotated row has non-empty short text, and 'why' matches the
#      per-row explain_optimiser_scenario() call (one source of truth) ------
chk(all(nzchar(res$why)), "annotate_optimiser_explanations() leaves no row with a blank 'why'")
chk(identical(res$why, vapply(details, function(d) d$short, character(1))),
    "the vectorised 'why' column matches explain_optimiser_scenario()$short row-for-row (single source of truth)")

# =====================================================================
# Part B -- synthetic fixtures: never_used / negligible / exact-tie proof
# =====================================================================
# Minimal columns explain_optimiser_scenario()/find_single_resource_parent()
# actually read. mode/testing/milling/wireline/ct/frac_fleets/frac_trees/
# allow_ct_for_milling identify grid adjacency; the rest carry the evidence.
mk_row <- function(testing_units = 2, p50_days = 100, total_mobilisation_cost = 1e6,
                    binding_path_primary = "Post-frac path",
                    frac_path_bind_pct = 0, post_frac_bind_pct = 1,
                    p90_util_testing = 0.5, config_label = NULL) {
  tibble(
    operation_mode = "Zipper", frac_fleets = 1, wireline_units = 1, ct_units = 1,
    milling_units = 1, testing_units = testing_units, frac_trees = 2,
    allow_ct_for_milling = FALSE,
    p50_days = p50_days, p90_days = p50_days + 20,
    total_mobilisation_cost = total_mobilisation_cost,
    binding_path_primary = binding_path_primary,
    frac_path_bind_pct = frac_path_bind_pct, post_frac_bind_pct = post_frac_bind_pct,
    p90_util_frac_fleet = NA_real_, p90_util_wireline = NA_real_, p90_util_ct = NA_real_,
    p90_util_milling = NA_real_, p90_util_testing = p90_util_testing,
    config_label = config_label %||% sprintf("Zipper | FF:1 WL:1 CT:1 ML:1 TU:%d Trees:2", testing_units)
  )
}

# never_used: child has one more testing unit than parent, P50 essentially
# unchanged (well under the negligible-days threshold, and NOT an exact tie
# so the tie-check branch doesn't intercept it first), utilization barely
# moved (well under the 'meaningfully used' pp threshold).
never_fixture <- bind_rows(
  mk_row(testing_units = 2, p50_days = 100.00, total_mobilisation_cost = 1e6, p90_util_testing = 0.42),
  mk_row(testing_units = 3, p50_days = 100.05, total_mobilisation_cost = 1.2e6, p90_util_testing = 0.41)
)
d_never <- explain_optimiser_scenario(never_fixture, 2)
chk(d_never$type == "never_used",
    sprintf("synthetic fixture (P50 +0.05 d, utilization -1 pp): classified 'never_used' (got '%s')", d_never$type))
chk(grepl("not meaningfully used", d_never$short), "never_used 'why' text states the capacity was not meaningfully used")

# negligible: same near-zero P50 delta, but this time the utilization column
# is NA for both rows (no evidence available) -- the weaker, distinct claim
# that only speaks to P50, not to whether the unit was used.
negligible_fixture <- bind_rows(
  mk_row(testing_units = 2, p50_days = 100.00, total_mobilisation_cost = 1e6, p90_util_testing = NA_real_),
  mk_row(testing_units = 3, p50_days = 100.05, total_mobilisation_cost = 1.2e6, p90_util_testing = NA_real_)
)
d_neg <- explain_optimiser_scenario(negligible_fixture, 2)
chk(d_neg$type == "negligible",
    sprintf("synthetic fixture (P50 +0.05 d, NO utilization evidence): classified 'negligible' (got '%s')", d_neg$type))
chk(grepl("No measurable campaign improvement", d_neg$short),
    "negligible 'why' text makes the weaker P50-only claim, without asserting anything about utilization")
chk(is.null(d_neg$detail$utilization_drop_pp),
    "negligible evidence detail carries no utilization_drop_pp field (none was available)")

# baseline: single row, no single-resource parent exists in its own results set.
baseline_fixture <- mk_row(testing_units = 1, p50_days = 150, total_mobilisation_cost = 2e6)
d_base <- explain_optimiser_scenario(baseline_fixture, 1)
chk(d_base$type == "baseline", "a row with no single-resource parent in its result set classifies as 'baseline'")

# exact-tie cheaper representative: two rows, identical P50 to full precision,
# different cost -- the pricier one must point at the cheaper one by name.
tie_fixture <- bind_rows(
  mk_row(testing_units = 2, p50_days = 120, total_mobilisation_cost = 3e6, config_label = "cheap-config"),
  mk_row(testing_units = 3, p50_days = 120, total_mobilisation_cost = 4e6, config_label = "expensive-config")
)
d_tie <- explain_optimiser_scenario(tie_fixture, 2)
chk(d_tie$type == "tie" && d_tie$detail$baseline == "cheap-config",
    "exact-tie fixture: the pricier row's explanation names the cheaper tied config as the reference")
d_tie_cheap <- explain_optimiser_scenario(tie_fixture, 1)
chk(d_tie_cheap$type != "tie",
    "exact-tie fixture: the CHEAPEST row in the tie group is not itself reported as a dominated tie (it falls through to the normal neighbor explanation)")

cat(sprintf("\n==== %s ====\n", if (ok) "ALL PROPERTY CHECKS PASS" else "FAILURES ABOVE"))
if (!ok) quit(status = 1)
