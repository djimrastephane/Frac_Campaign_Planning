# test_validate_inputs.R
# Property checks for validate_assumptions()'s hard-error gates: empty risk
# names, duplicate risk names, and invalid scope values must all reject the
# file rather than silently passing through to the engine. Run:
#   Rscript test_validate_inputs.R
suppressPackageStartupMessages({
  library(dplyr)
  source("load_inputs.R")
  source("validate_inputs.R")
})

ok <- TRUE
chk <- function(c, m) { cat(sprintf("  [%s] %s\n", if (isTRUE(c)) "PASS" else "FAIL", m)); ok <<- ok && isTRUE(c) }

mk <- function(...) {
  tibble::tribble(
    ~category, ~variable, ~type, ~probability, ~min_days, ~most_likely_days, ~max_days, ~simulation_impact, ~scope,
    ...
  )
}

err <- function(df) tryCatch({ validate_assumptions(df); NULL }, error = function(e) conditionMessage(e))

# -- 1. The real template passes cleanly (no false positives).
TEMPLATE <- load_master_assumptions("../data_templates/master_risks_assumptions_template.csv")
chk(is.null(err(TEMPLATE)), "master_risks_assumptions_template.csv passes validation cleanly")

# -- 2. Empty / blank risk name is a hard error.
df_empty <- mk(
  "Frac", "Screenout",  "risk", 0.08, 0.5, 1.0, 3.0, "extra stage",    "stage",
  "Frac", "  ",         "risk", 0.05, 0.2, 0.5, 1.0, "additional run", "stage"
)
e1 <- err(df_empty)
chk(!is.null(e1) && grepl("non-empty", e1), "blank risk name is rejected with a clear message")
chk(!is.null(e1) && grepl("row 2", e1), "blank-name error names the offending row number")

# -- 3. Duplicate risk names (case/whitespace-insensitive) are a hard error.
df_dup <- mk(
  "Frac", "Screenout",   "risk", 0.08, 0.5, 1.0, 3.0, "extra stage",    "stage",
  "Frac", " screenout ", "risk", 0.05, 0.2, 0.5, 1.0, "additional run", "stage"
)
e2 <- err(df_dup)
chk(!is.null(e2) && grepl("duplicate", e2, ignore.case = TRUE), "case/whitespace-insensitive duplicate risk name is rejected")
chk(!is.null(e2) && grepl("row 1", e2) && grepl("row 2", e2), "duplicate-name error names both offending rows")

# -- 4. A non-blank, unrecognised scope is a hard error (not just a warning).
df_scope <- mk(
  "Frac", "Screenout", "risk", 0.08, 0.5, 1.0, 3.0, "extra stage", "Stge"
)
e3 <- err(df_scope)
chk(!is.null(e3) && grepl("invalid scope", e3, ignore.case = TRUE), "typo'd scope value is rejected, not silently defaulted")

# -- 5. Blank/NA scope is allowed (defaults to well) and only warns, but the
# warning must name the offending row(s) -- not just a bare count -- like
# every other check in this function does.
df_blank_scope <- mk(
  "Frac", "Screenout",   "risk", 0.08, 0.5, 1.0, 3.0, "extra stage",    "stage",
  "Frac", "Gun misfire", "risk", 0.05, 0.2, 0.5, 1.0, "additional run", NA
)
res <- tryCatch(validate_assumptions(df_blank_scope), error = function(e) e)
chk(!inherits(res, "error"), "blank/NA scope does not error")
scope_warning <- attr(res, "input_warnings")[grepl("defaulting to 'well'", attr(res, "input_warnings"))]
chk(length(scope_warning) == 1, "blank/NA scope produces a 'defaulting to well' warning")
chk(grepl("row 2", scope_warning) && grepl("Gun misfire", scope_warning),
    "missing-scope warning names the offending row number and risk, not just a count")
chk(!grepl("row 1", scope_warning), "missing-scope warning does not name the row that has a valid scope")

# -- 6. A normal, fully valid two-risk file passes with no errors or warnings.
df_ok <- mk(
  "Frac", "Screenout",   "risk", 0.08, 0.5, 1.0, 3.0, "extra stage",    "stage",
  "Frac", "Gun misfire", "risk", 0.05, 0.2, 0.5, 1.0, "additional run", "stage"
)
res_ok <- tryCatch(validate_assumptions(df_ok), error = function(e) e)
chk(!inherits(res_ok, "error"), "two distinct, valid risk rows pass validation")
chk(length(attr(res_ok, "input_warnings")) == 0, "valid file produces no warnings")

# -- 7. validate_historical_wells(): the real template passes cleanly.
HIST_TEMPLATE <- load_historical_wells("../data_templates/historical_wells_template.csv")
err_hist <- function(df) tryCatch({ validate_historical_wells(df); NULL }, error = function(e) conditionMessage(e))
chk(is.null(err_hist(HIST_TEMPLATE)), "historical_wells_template.csv passes validation cleanly")

# -- 8. A non-numeric value in a numeric column is a hard error naming the
# column, row, well, and offending value -- not an opaque downstream R error
# (and not a silent string-comparison miscount in n_pos_frac/n_pos_mill).
hist_bad <- HIST_TEMPLATE
hist_bad$frac_days_per_stage[2] <- "abc"
e4 <- err_hist(hist_bad)
chk(!is.null(e4) && grepl("non-numeric", e4, ignore.case = TRUE), "non-numeric value in a numeric column is rejected with a clear message")
chk(!is.null(e4) && grepl("frac_days_per_stage", e4), "non-numeric error names the offending column")
chk(!is.null(e4) && grepl("row 2", e4), "non-numeric error names the offending row")
chk(!is.null(e4) && grepl("abc", e4), "non-numeric error quotes the offending value")

# -- 9. Blank/NA values in a numeric column are NOT flagged as non-numeric
# (that's a separate, pre-existing soft-warning path for missing data).
hist_blank <- HIST_TEMPLATE
hist_blank$milling_days_per_plug[3] <- NA
chk(is.null(err_hist(hist_blank)), "NA in a numeric column does not trigger the non-numeric check")

cat(sprintf("\n==== %s ====\n", if (ok) "ALL PROPERTY CHECKS PASS" else "FAILURES ABOVE"))
if (!ok) quit(status = 1)
