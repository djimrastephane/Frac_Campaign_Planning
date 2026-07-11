# test_optimiser_ties.R -- run: Rscript test_optimiser_ties.R
#
# Phase 6.D of the optimiser auditability pass: exact-tie grouping
# (group_optimiser_ties()). group_optimiser_ties() is a pure function of a
# completed optimise_campaign_scenarios() results tibble -- these tests use
# hand-built synthetic fixtures (precise control over which values are
# "the same") plus one real grid to confirm the property holds against
# actual engine output, not just constructed numbers.
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

mk <- function(operation_mode, p50_days, total_mobilisation_cost, pareto = TRUE,
               recommended = FALSE, fastest = FALSE) {
  tibble(operation_mode = operation_mode, p50_days = p50_days,
         total_mobilisation_cost = total_mobilisation_cost,
         pareto = pareto, recommended = recommended, fastest = fastest)
}

# =====================================================================
# Part A -- exact full-precision ties are grouped
# =====================================================================
fx <- bind_rows(
  mk("Zipper", 100.000000, 1e6),                    # group with row 2 (exact match)
  mk("Zipper", 100.000000, 1.5e6),
  mk("Zipper", 100.0000005, 2e6),                    # within OPTIMISER_TIE_EPS (1e-6) of rows 1/2
  mk("Zipper", 150.000000, 3e6),                      # singleton
  mk("Conventional", 100.000000, 1e6)                 # different mode -- must NOT group with row 1
)
g <- group_optimiser_ties(fx)

chk(g$tie_group_id[1] == g$tie_group_id[2] && g$tie_group_id[2] == g$tie_group_id[3],
    "rows within OPTIMISER_TIE_EPS of each other (100.000000 / 100.000000 / 100.0000005) share one tie_group_id")
chk(all(g$tie_group_size[1:3] == 3),
    "that group's tie_group_size is 3")
chk(g$tie_group_id[4] != g$tie_group_id[1],
    "a row 50 days away is its own group, not merged in")
chk(g$tie_group_size[4] == 1,
    "a singleton row has tie_group_size == 1")
chk(g$tie_group_id[5] != g$tie_group_id[1],
    "identical P50 in a DIFFERENT operation_mode is never grouped with the first mode's rows (grouping is scoped per operation_mode)")

# ---- cheapest member of the group is the representative ------------------
chk(isTRUE(g$is_tie_representative[1]) && !isTRUE(g$is_tie_representative[2]) && !isTRUE(g$is_tie_representative[3]),
    "within the 3-row tie group, only the cheapest ($1.0M) is flagged is_tie_representative")
chk(isTRUE(g$is_tie_representative[4]),
    "a singleton group's sole row is its own representative (TRUE even though it's alone)")

# =====================================================================
# Part B -- rounded-only ties (differ beyond display precision, i.e. beyond
# OPTIMISER_TIE_EPS) are NOT grouped, even though a 2-decimal display would
# show them as identical
# =====================================================================
fx2 <- bind_rows(
  mk("Zipper", 100.001, 1e6),
  mk("Zipper", 100.004, 1.2e6)   # both round to "100.00" at 2dp, but differ by 0.003 >> 1e-6
)
g2 <- group_optimiser_ties(fx2)
chk(g2$tie_group_id[1] != g2$tie_group_id[2],
    sprintf("two rows that round to the same 2-decimal display value (100.001 vs 100.004, diff = %.6f) are NOT grouped -- grouping uses full-precision OPTIMISER_TIE_EPS (%.0e), not the rounded display value",
            abs(100.004 - 100.001), OPTIMISER_TIE_EPS))
chk(all(g2$tie_group_size == 1),
    "consequently both rows above are singleton groups")

# Conversely: two rows that DO differ in their full-precision value by less
# than OPTIMISER_TIE_EPS, but would look identical even at high display
# precision, ARE grouped (the tolerance exists for floating-point noise, not
# to widen the grouping criterion).
fx3 <- bind_rows(
  mk("Zipper", 100.0000001, 1e6),
  mk("Zipper", 100.0000004, 1.1e6)   # diff = 3e-7 < OPTIMISER_TIE_EPS (1e-6)
)
g3 <- group_optimiser_ties(fx3)
chk(g3$tie_group_id[1] == g3$tie_group_id[2],
    "two rows differing by less than OPTIMISER_TIE_EPS (floating-point noise) ARE grouped")

# =====================================================================
# Part C -- no rows removed; pareto/recommended/fastest untouched
# =====================================================================
fx4 <- bind_rows(
  mk("Zipper", 100, 1e6, pareto = TRUE,  recommended = TRUE,  fastest = FALSE),
  mk("Zipper", 100, 1.5e6, pareto = FALSE, recommended = FALSE, fastest = FALSE),
  mk("Zipper", 80,  5e6, pareto = TRUE,  recommended = FALSE, fastest = TRUE)
)
g4 <- group_optimiser_ties(fx4)
chk(nrow(g4) == nrow(fx4),
    "group_optimiser_ties() removes no rows -- input and output row counts match")
chk(identical(g4$pareto, fx4$pareto),
    "pareto column is untouched by group_optimiser_ties()")
chk(identical(g4$recommended, fx4$recommended),
    "recommended column is untouched by group_optimiser_ties()")
chk(identical(g4$fastest, fx4$fastest),
    "fastest column is untouched by group_optimiser_ties()")
chk(setequal(names(g4), c(names(fx4), "tie_group_id", "tie_group_size", "is_tie_representative")),
    "group_optimiser_ties() only ADDS the three tie columns -- no existing column is dropped or renamed")

# ---- NULL / empty input handled without error -----------------------------
chk(is.null(group_optimiser_ties(NULL)), "NULL input returns NULL")
chk(nrow(group_optimiser_ties(fx4[0, ])) == 0, "zero-row input returns zero rows, no error")

# =====================================================================
# Part D -- real grid: property holds against actual engine output
# =====================================================================
HW <- synthetic_historical_wells(n = 30, seed = 42)
A  <- load_master_assumptions("../data_templates/master_risks_assumptions_template.csv")
grid <- expand.grid(
  operation_mode = "Zipper", frac_fleets = 1, wireline_units = c(1, 2, 3),
  ct_units = 1, milling_units = c(1, 2), testing_units = 2,
  frac_trees = 2, allow_ct_for_milling = FALSE, stringsAsFactors = FALSE
)
res <- optimise_campaign_scenarios(
  historical_wells = HW, assumptions = A, n_wells = 30, scenario_grid = grid,
  screen_iterations = 150, refine_iterations = 400, top_n_refine = 3, seed = 123
)
res_g <- group_optimiser_ties(res)

chk(nrow(res_g) == nrow(res), "real grid: row count preserved through group_optimiser_ties()")
chk(identical(res_g$pareto, res$pareto) && identical(res_g$recommended, res$recommended) &&
    identical(res_g$fastest, res$fastest),
    "real grid: pareto/recommended/fastest identical before and after grouping")

# every row within a tie group must be within OPTIMISER_TIE_EPS of every
# other row in the SAME group (transitive closeness check, not just
# closeness to the group's anchor row).
within_group_ok <- res_g %>%
  group_by(tie_group_id) %>%
  summarise(spread = max(p50_days) - min(p50_days), .groups = "drop") %>%
  pull(spread) %>%
  { all(. <= OPTIMISER_TIE_EPS * 2) }   # 2x eps: worst case, two rows both eps away from a shared anchor
chk(within_group_ok,
    "real grid: every tie group's internal P50 spread is within tolerance of OPTIMISER_TIE_EPS")

chk(any(res_g$tie_group_size > 1),
    "real grid produces at least one genuine multi-row tie group (expected: unused resource additions reproduce identical P50 under common random numbers)")

cat(sprintf("\n==== %s ====\n", if (ok) "ALL PROPERTY CHECKS PASS" else "FAILURES ABOVE"))
if (!ok) quit(status = 1)
