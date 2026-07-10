# test_optimiser_flags.R -- run: Rscript test_optimiser_flags.R
#
# Pins two export-quality fixes for the scenario optimiser:
#
# 1. `fastest` must flag exactly ONE row. Byte-identical P50 ties are
#    expected under common random numbers whenever an added unit is
#    non-binding (found in a real export: ML2 vs ML3 configs with identical
#    P50 to full float precision were BOTH flagged fastest). Tie-break:
#    cheapest total mobilisation cost among the tied set.
# 2. optimiser_export_headers() (app/app.R) renames idle_days/idle_cost to
#    scope-carrying names in the downloaded CSV -- the in-app footnote that
#    "idle" covers only frac-fleet-awaiting-wireline does not travel with
#    the bare file.
ENGINE_FILES <- if (file.exists("engine_core.R")) {
  c("engine_core.R", "summaries.R", "report_pdf.R", "optimiser_cascade.R")
} else "archive/simulation_engine.R"
suppressPackageStartupMessages({
  library(dplyr)
  for (.ef in ENGINE_FILES) source(.ef)
  source("risk_library_engine.R"); source("load_inputs.R")
})

ok <- TRUE
chk <- function(c, m) { cat(sprintf("  [%s] %s\n", if (isTRUE(c)) "PASS" else "FAIL", m)); ok <<- ok && isTRUE(c) }

HW <- synthetic_historical_wells(n = 30, seed = 42)
A  <- load_master_assumptions("../data_templates/master_risks_assumptions_template.csv")

# Grid engineered to produce a genuine P50 tie: the two Zipper configs differ
# only in milling units (2 vs 3); with testing_units = 3, milling never binds,
# so under common random numbers both configs reproduce identical draws and
# identical P50s -- the exact tie shape seen in the real export.
GRID <- tibble::tibble(
  operation_mode = c("Zipper", "Zipper", "Conventional"),
  frac_fleets = c(1, 1, 1), wireline_units = c(1, 1, 1), ct_units = c(1, 1, 1),
  milling_units = c(2, 3, 2), testing_units = c(3, 3, 3),
  frac_trees = c(2, 2, 2), allow_ct_for_milling = c(FALSE, FALSE, FALSE)
)

# refine_iterations == screen_iterations deliberately: refinement re-runs the
# top-cost config at the same seed and iteration count, reproducing its
# screened result exactly, so the engineered ML2/ML3 tie survives the refine
# stage (with a higher refine count the refined row's P50 shifts and the tie
# dissolves -- which is what happens in real runs, where ties live among
# same-stage rows).
res <- optimise_campaign_scenarios(
  historical_wells = HW, assumptions = A, n_wells = 20,
  scenario_grid = GRID, screen_iterations = 100, refine_iterations = 100,
  top_n_refine = 1, seed = 123
)

n_tied <- sum(res$p50_days == min(res$p50_days))
chk(n_tied >= 2,
    sprintf("fixture produces a genuine P50 tie (%d rows share the minimum) -- the tie-break is actually exercised", n_tied))
chk(sum(res$fastest) == 1,
    sprintf("exactly one row flagged fastest (got %d)", sum(res$fastest)))
tied_rows <- res[res$p50_days == min(res$p50_days), ]
chk(res$total_mobilisation_cost[res$fastest] == min(tied_rows$total_mobilisation_cost),
    "the flagged row is the cheapest of the tied set (ML2, not ML3)")
chk(res$milling_units[res$fastest] == 2,
    sprintf("cheapest tied config is the 2-milling one (got ML%d)", res$milling_units[res$fastest]))
chk(sum(res$recommended) == 1, "recommended still flags exactly one row")

# ---- optimiser_export_headers(), parse-extracted from app/app.R --------------
app_path <- if (file.exists("../app/app.R")) "../app/app.R" else "app/app.R"
exprs <- parse(app_path)
env <- new.env()
for (e in as.list(exprs)) {
  if (is.call(e) && identical(e[[1]], as.name("<-")) &&
      is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function")) &&
      identical(deparse(e[[2]]), "optimiser_export_headers")) {
    eval(e, envir = env)
  }
}
chk(exists("optimiser_export_headers", envir = env), "optimiser_export_headers() found in app.R")
oeh <- get("optimiser_export_headers", envir = env)

out <- oeh(res)
chk(all(c("frac_idle_awaiting_wireline_days", "frac_idle_awaiting_wireline_cost") %in% names(out)),
    "export renames idle_days/idle_cost to scope-carrying names")
chk(!any(c("idle_days", "idle_cost") %in% names(out)), "old ambiguous names absent from export")
chk(identical(out$frac_idle_awaiting_wireline_days, res$idle_days),
    "rename is name-only -- values untouched")
chk(identical(names(oeh(out)), names(out)),
    "helper is a no-op on already-renamed data (tolerates missing columns)")

cat(sprintf("\n==== %s ====\n", if (ok) "ALL PROPERTY CHECKS PASS" else "FAILURES ABOVE"))
if (!ok) quit(status = 1)
