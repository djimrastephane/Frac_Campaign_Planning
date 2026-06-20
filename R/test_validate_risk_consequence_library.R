# test_validate_risk_consequence_library.R
# Property checks for validate_risk_consequence_library()'s scope gate.
# Library-only risks (no row in master_risks_assumptions.csv) get their scope
# from this CSV alone via build_risk_table() (R/risk_library_engine.R), so an
# invalid value here must be rejected the same way validate_assumptions()
# rejects one in master_risks_assumptions.csv -- otherwise it silently falls
# through to flat, non-compounding probability scaling. Run:
#   Rscript test_validate_risk_consequence_library.R
suppressPackageStartupMessages(library(dplyr))
source("validate_risk_consequence_library.R")

ok <- TRUE
chk <- function(c, m) { cat(sprintf("  [%s] %s\n", if (isTRUE(c)) "PASS" else "FAIL", m)); ok <<- ok && isTRUE(c) }

TEMPLATE <- read.csv("../data_templates/risk_consequence_library_template_simple_severity.csv", stringsAsFactors = FALSE)

# -- 1. The real template passes cleanly.
res <- tryCatch({ validate_risk_consequence_library(TEMPLATE); "OK" }, error = function(e) conditionMessage(e))
chk(identical(res, "OK"), "risk_consequence_library_template_simple_severity.csv passes validation cleanly")

# -- 2. A typo'd scope value on a library row is a hard error naming the risk and the bad value.
df_bad <- TEMPLATE
df_bad$scope[1] <- "Stge"
e1 <- tryCatch({ validate_risk_consequence_library(df_bad); NULL }, error = function(e) conditionMessage(e))
chk(!is.null(e1) && grepl("invalid scope", e1, ignore.case = TRUE), "typo'd scope value is rejected")
chk(!is.null(e1) && grepl("Stge", e1), "error names the offending scope value")
chk(!is.null(e1) && grepl(df_bad$risk_name[1], e1, fixed = TRUE), "error names the offending risk")
chk(!is.null(e1) && grepl("row 1", e1), "error names the offending row number")

# -- 2b. A NA risk_name on the offending row does not crash the error message
# (renders as a labelled placeholder, not the literal string "NA").
df_bad_na_name <- TEMPLATE
df_bad_na_name$scope[2] <- "Stge"
df_bad_na_name$risk_name[2] <- NA
e1b <- tryCatch({ validate_risk_consequence_library(df_bad_na_name); NULL }, error = function(e) conditionMessage(e))
chk(!is.null(e1b) && grepl("row 2", e1b), "NA risk_name on the offending row does not crash the scope error")
chk(!is.null(e1b) && grepl("unnamed row", e1b), "NA risk_name renders as a labelled placeholder, not the literal string 'NA'")

# -- 3. Blank/NA scope is allowed (build_risk_table() defaults it to 'well', same as the
# master assumptions CSV) -- only a non-blank, unrecognised value is an error.
df_blank <- TEMPLATE
df_blank$scope[1] <- ""
res_blank <- tryCatch({ validate_risk_consequence_library(df_blank); "OK" }, error = function(e) conditionMessage(e))
chk(identical(res_blank, "OK"), "blank scope does not error")

df_na <- TEMPLATE
df_na$scope[1] <- NA
res_na <- tryCatch({ validate_risk_consequence_library(df_na); "OK" }, error = function(e) conditionMessage(e))
chk(identical(res_na, "OK"), "NA scope does not error")

# -- 4. A valid, differently-cased/whitespace scope value is accepted.
df_case <- TEMPLATE
df_case$scope[1] <- " STAGE "
res_case <- tryCatch({ validate_risk_consequence_library(df_case); "OK" }, error = function(e) conditionMessage(e))
chk(identical(res_case, "OK"), "case/whitespace-insensitive valid scope value is accepted")

cat(sprintf("\n==== %s ====\n", if (ok) "ALL PROPERTY CHECKS PASS" else "FAILURES ABOVE"))
if (!ok) quit(status = 1)
