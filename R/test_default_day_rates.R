# test_default_day_rates.R -- run: Rscript test_default_day_rates.R
#
# DEFAULT_DAY_RATES (constants.R) is the canonical value for the frac fleet /
# wireline / CT / milling / testing-unit day rates. It is NOT wired into
# every function signature that has its own "..._cost_per_day = <literal>"
# default (see constants.R for why -- lazy-eval scoping would require every
# standalone test_*.R script to source constants.R too). Instead, this test
# greps every such default across R/*.R and app/app.R and asserts it still
# equals DEFAULT_DAY_RATES -- so if a future edit changes one occurrence
# without changing the rest, this fails loudly instead of the user silently
# seeing inconsistent $ figures across tabs.
root <- if (file.exists("constants.R")) "." else "R"
source(file.path(root, "constants.R"))
app_path <- if (file.exists("../app/app.R")) "../app/app.R" else "app/app.R"

files <- c(list.files(root, pattern = "^[^.].*\\.R$", full.names = TRUE), app_path)
files <- files[!grepl("archive/|/archive/|test_default_day_rates\\.R$|constants\\.R$", files)]

.PARAM_RATE <- c(
  frac_fleet_cost_per_day   = "frac_fleet",
  wireline_cost_per_day     = "wireline",
  ct_cost_per_day           = "ct",
  milling_cost_per_day      = "milling",
  testing_unit_cost_per_day = "testing_unit"
)

ok <- TRUE
chk <- function(c, m) { cat(sprintf("  [%s] %s\n", if (isTRUE(c)) "PASS" else "FAIL", m)); ok <<- ok && isTRUE(c) }

n_checked <- 0
for (f in files) {
  lines <- readLines(f, warn = FALSE)
  for (param in names(.PARAM_RATE)) {
    # Matches "param = 250000" / "param=250000" as a default-argument literal
    # (not a call-site override like frac_fleet_cost_per_day = input$frac_fleet_cost).
    hits <- grep(sprintf("%s\\s*=\\s*[0-9]", param), lines, value = TRUE)
    hits <- hits[grepl(sprintf("%s\\s*=\\s*[0-9]+\\s*[,)]", param), hits)]
    for (h in hits) {
      m <- regmatches(h, regexpr(sprintf("%s\\s*=\\s*[0-9]+", param), h))
      val <- as.numeric(sub(".*=\\s*", "", m))
      expected <- DEFAULT_DAY_RATES[[.PARAM_RATE[[param]]]]
      n_checked <- n_checked + 1
      chk(val == expected,
          sprintf("%s: %s default (%d) matches DEFAULT_DAY_RATES$%s (%d)",
                  basename(f), param, val, .PARAM_RATE[[param]], expected))
    }
  }
}

chk(n_checked >= 15, sprintf("found and checked a plausible number of default-rate occurrences (%d)", n_checked))

# UI defaults must read from DEFAULT_DAY_RATES, not a re-duplicated literal.
app_lines <- readLines(app_path, warn = FALSE)
chk(any(grepl('numericInput\\("frac_fleet_cost".*DEFAULT_DAY_RATES\\$frac_fleet', app_lines)),
    "sidebar frac_fleet_cost numericInput reads its default from DEFAULT_DAY_RATES")
chk(any(grepl('numericInput\\("testing_unit_cost".*DEFAULT_DAY_RATES\\$testing_unit', app_lines)),
    "sidebar testing_unit_cost numericInput reads its default from DEFAULT_DAY_RATES")

cat(sprintf("\n==== %s ====\n", if (ok) "ALL PROPERTY CHECKS PASS" else "FAILURES ABOVE"))
if (!ok) quit(status = 1)
