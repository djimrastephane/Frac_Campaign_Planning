# constants.R
# -----------------------------------------------------------------------------
# Single source of truth for values that were previously duplicated as
# literal defaults across many function signatures (recommendations.R,
# scenario_library.R, robustness.R, whatif_builder.R, risk_uncertainty.R,
# optimiser_parallel.R, report_decision_page.R, simulation_engine_fast.R --
# 15+ occurrences, all currently 250000 / 15000 / 25000 / 18000 / 12000) and
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
