# test_csv_sanitizer.R -- run: Rscript test_csv_sanitizer.R
#
# Regression test for the CSV/formula-injection guard (sanitize_csv_cell() /
# sanitize_csv_text_cols() in app/app.R). Loads only those two function
# definitions out of app.R -- by selectively eval()ing top-level function
# expressions whose name starts with "sanitize" -- rather than sourcing the
# whole file, since app.R ends with shinyApp(ui, server) and requires the
# full Shiny package stack (bslib, DT, rhandsontable, future, ...) just to
# parse the UI tree.
app_path <- if (file.exists("../app/app.R")) "../app/app.R" else "app/app.R"
stopifnot(file.exists(app_path))

exprs <- parse(app_path)
env <- new.env()
n_loaded <- 0
for (e in as.list(exprs)) {
  if (is.call(e) && identical(e[[1]], as.name("<-")) &&
      is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function")) &&
      grepl("^sanitize", deparse(e[[2]]))) {
    eval(e, envir = env)
    n_loaded <- n_loaded + 1
  }
}

ok <- TRUE
chk <- function(c, m) { cat(sprintf("  [%s] %s\n", if (isTRUE(c)) "PASS" else "FAIL", m)); ok <<- ok && isTRUE(c) }

chk(n_loaded == 2, sprintf("loaded sanitize_csv_cell() and sanitize_csv_text_cols() from app.R (found %d)", n_loaded))

sanitize_csv_cell <- get("sanitize_csv_cell", envir = env)
sanitize_csv_text_cols <- get("sanitize_csv_text_cols", envir = env)

chk(identical(sanitize_csv_cell("=cmd|'/c calc'!A1"), "'=cmd|'/c calc'!A1"),
    "formula starting with = gets a safe apostrophe prefix")
chk(identical(sanitize_csv_cell("=SUM(1,1)"), "'=SUM(1,1)"), "=SUM(1,1) is neutralised")
chk(identical(sanitize_csv_cell("+1234"), "'+1234"), "leading + is neutralised")
chk(identical(sanitize_csv_cell("-1234"), "'-1234"), "leading - is neutralised")
chk(identical(sanitize_csv_cell("@SUM(A1)"), "'@SUM(A1)"), "leading @ is neutralised")
chk(identical(sanitize_csv_cell("Screenout risk"), "Screenout risk"), "ordinary text passes through unchanged")
chk(identical(sanitize_csv_cell(NA_character_), NA_character_), "NA is preserved as NA, not stringified")
chk(identical(sanitize_csv_cell(c(1, 2, 3)), c(1, 2, 3)), "numeric vectors pass through untouched (not character)")

df <- data.frame(
  variable = c("Screenout", "=cmd|calc"),
  simulation_impact = c("extra stage", "+HYPERLINK(\"http://evil\")"),
  probability = c(0.1, 0.2),
  stringsAsFactors = FALSE
)
out <- sanitize_csv_text_cols(df, c("variable", "simulation_impact"))
chk(identical(out$variable, c("Screenout", "'=cmd|calc")), "only targeted text column 'variable' is sanitized")
chk(identical(out$simulation_impact, c("extra stage", "'+HYPERLINK(\"http://evil\")")),
    "targeted text column 'simulation_impact' is sanitized")
chk(is.numeric(out$probability) && identical(out$probability, c(0.1, 0.2)),
    "numeric column 'probability' is untouched and stays numeric")

out_missing_col <- sanitize_csv_text_cols(df, c("variable", "not_a_real_column"))
chk(identical(out_missing_col$variable, c("Screenout", "'=cmd|calc")),
    "sanitize_csv_text_cols tolerates a requested column that isn't present in df")

cat(sprintf("\n==== %s ====\n", if (ok) "ALL PROPERTY CHECKS PASS" else "FAILURES ABOVE"))
if (!ok) quit(status = 1)
