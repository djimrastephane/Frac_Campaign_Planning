# constants.R
# -----------------------------------------------------------------------------
# Single source of truth for values that were previously duplicated as
# literal defaults across many function signatures (recommendations.R,
# scenario_library.R, robustness.R, whatif_builder.R, risk_uncertainty.R,
# optimiser_parallel.R, report_decision_page.R, summaries.R, report_pdf.R,
# optimiser_cascade.R -- 15+ occurrences, all currently
# 250000 / 15000 / 25000 / 18000 / 12000) and
# the app's own sidebar numericInput() defaults.
#
# Deliberately NOT wired into those existing function signatures: R evaluates
# default arguments lazily in the function's own defining environment, so
# swapping a literal default for DEFAULT_DAY_RATES$xxx there would require
# this file to be sourced before every script that calls those functions with
# no explicit rate arguments -- including the standalone test_*.R scripts
# that source only recommendations.R/scenario_library.R directly. That's a
# wide blast radius for a value that is, today, already identical everywhere
# it appears. Instead: this file is the canonical value, the app's sidebar
# reads from it (removing the one duplication that's actually visible to the
# user), and test_default_day_rates.R (R/test_default_day_rates.R) greps
# every "_cost_per_day = <number>" default across the codebase and asserts
# it still matches DEFAULT_DAY_RATES -- so if someone changes one occurrence
# without changing the rest, CI catches the drift instead of the user seeing
# silently inconsistent $ figures across tabs.
# -----------------------------------------------------------------------------

`%||%` <- function(x, y) if (is.null(x)) y else x

DEFAULT_DAY_RATES <- list(
  frac_fleet    = 250000,
  wireline      = 15000,
  ct            = 25000,
  milling       = 18000,
  testing_unit  = 12000
)

# -----------------------------------------------------------------------------
# Optimiser auditability/explainability thresholds (R/optimiser_manifest.R,
# R/optimiser_explain.R, R/optimiser_cascade.R, R/optimiser_parallel.R).
# Named here, not inline, so every consumer reads the same cutoff and the
# app's explanatory text can quote these numbers directly without risking
# drift (same pattern as REC_DECISION_THRESHOLDS / BAYES_DECISION_THRESHOLDS).
# -----------------------------------------------------------------------------

# Binding-path classification (Phase 2). A scenario's campaign duration is
# max(frac-path days, post-frac completion days) -- computed once per
# iteration inside simulate_campaign_detailed() and already summarised per
# scenario by score_run(). "Dominant" means that side governs completion in
# at least this fraction of the scenario's iterations; "tie band" flags a
# near-50/50 split as genuinely contested rather than arbitrarily Mixed.
OPTIMISER_BINDING_DOMINANT_THRESHOLD <- 0.70
OPTIMISER_BINDING_TIE_BAND <- 0.05

# Scenario-explanation thresholds (Phase 3).
#   TIE_EPS: two P50s are "the same result" (not just close) below this gap,
#     in days -- used for both exact-tie detection (Phase 4) and the "added
#     resource never used" explanation's P50-unchanged test.
#   NEGLIGIBLE_DAYS: a P50 change smaller than this is not called a
#     "campaign improvement" even when it is technically nonzero (guards
#     against reading Monte Carlo noise as a real result).
#   UNIT_USE_UTIL_DROP_PP: minimum P90-utilization drop (percentage points)
#     for an added unit to count as "meaningfully used" -- below this, the
#     extra capacity's utilization signature looks the same as if it had
#     never been added.
OPTIMISER_TIE_EPS <- 1e-6
OPTIMISER_NEGLIGIBLE_DAYS <- 0.5
OPTIMISER_UNIT_USE_UTIL_DROP_PP <- 0.05
