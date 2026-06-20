# test_risk_multiplier.R
# Validates compute_adjusted_risk_probability(): the multiplier must scale the
# per-stage probability BEFORE compounding across stages, not the already-
# compounded per-well probability (the bug this fixes). Run: Rscript test_risk_multiplier.R
suppressPackageStartupMessages({
  source("simulation_engine_fast.R")
  source("risk_library_engine.R")
})

ok <- TRUE
chk <- function(c, m) { cat(sprintf("  [%s] %s\n", if (isTRUE(c)) "PASS" else "FAIL", m)); ok <<- ok && isTRUE(c) }

# -- Worked example from the spec: base_probability = 0.05, n_stages = 5, multiplier = 3
p_stage_adj <- 0.05 * 3
expected <- 1 - (1 - p_stage_adj)^5  # 0.556, NOT 1-(1-0.05)^5 * 3 = 0.679 (old, wrong)

got <- compute_adjusted_risk_probability(probability = 0.05, scope = "stage",
                                          risk_multiplier = 3, n_stages = 5)
chk(isTRUE(all.equal(got, expected)), sprintf("stage-scope: multiplier applied before compounding (got %.4f, expected %.4f)", got, expected))
chk(abs(got - 0.679) > 0.05, "result is not the old over-amplified value (0.679)")

# -- Well-scope: direct scaling, capped at 1
chk(isTRUE(all.equal(compute_adjusted_risk_probability(0.2, "well", 3, 5), 0.6)),
    "well-scope: adjusted = min(p * multiplier, 1)")
chk(isTRUE(all.equal(compute_adjusted_risk_probability(0.5, "well", 3, 5), 1)),
    "well-scope: capped at 1 when p * multiplier exceeds 1")

# -- Campaign-scope: direct scaling, same formula as well-scope
chk(isTRUE(all.equal(compute_adjusted_risk_probability(0.08, "campaign", 2, 5), 0.16)),
    "campaign-scope: adjusted = min(p * multiplier, 1)")

# -- NA probability -> 0
chk(identical(compute_adjusted_risk_probability(NA, "well", 2, 5), 0),
    "NA probability resolves to 0")

# -- Vectorised over multiple rows/scopes at once (as used in build_risk_table)
v <- compute_adjusted_risk_probability(
  probability     = c(0.05, 0.2, 0.08, NA),
  scope           = c("stage", "well", "campaign", "well"),
  risk_multiplier = 3,
  n_stages        = 5
)
chk(isTRUE(all.equal(v, c(expected, 0.6, 0.24, 0))), "vectorised call matches scalar calls element-wise")

cat(sprintf("\n==== %s ====\n", if (ok) "ALL PROPERTY CHECKS PASS" else "FAILURES ABOVE"))
if (!ok) quit(status = 1)
