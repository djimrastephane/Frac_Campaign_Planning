# validate_risk_consequence_library.R
# Standalone checks for data_templates/risk_consequence_library_template_simple_severity.csv
# Run with: Rscript R/validate_risk_consequence_library.R [path/to/file.csv]

validate_risk_consequence_library <- function(df) {
  errors <- character(0)

  # 1. severity values limited to Minor/Moderate/Major
  allowed_severity <- c("Minor", "Moderate", "Major")
  bad_severity <- df[!df$severity %in% allowed_severity, ]
  if (nrow(bad_severity) > 0) {
    errors <- c(errors, sprintf(
      "Invalid severity values: %s",
      paste(unique(bad_severity$severity), collapse = ", ")
    ))
  }

  # 2. duplicate risk_name/severity combinations
  combo <- paste(df$risk_name, df$severity, sep = "|")
  dupes <- unique(combo[duplicated(combo)])
  if (length(dupes) > 0) {
    errors <- c(errors, sprintf(
      "Duplicate risk_name/severity combinations: %s",
      paste(dupes, collapse = ", ")
    ))
  }

  # 3. base_probability in [0, 1] and consistent within each risk_name
  out_of_range <- df[is.na(df$base_probability) | df$base_probability < 0 | df$base_probability > 1, ]
  if (nrow(out_of_range) > 0) {
    errors <- c(errors, sprintf(
      "base_probability out of [0,1] range for: %s",
      paste(unique(out_of_range$risk_name), collapse = ", ")
    ))
  }
  inconsistent <- aggregate(base_probability ~ risk_name, df, function(x) length(unique(x)))
  inconsistent <- inconsistent[inconsistent$base_probability > 1, ]
  if (nrow(inconsistent) > 0) {
    errors <- c(errors, sprintf(
      "Inconsistent base_probability within risk_name: %s",
      paste(inconsistent$risk_name, collapse = ", ")
    ))
  }

  # 4. scenario_probability sums to 1.0 per risk_name
  totals <- aggregate(scenario_probability ~ risk_name, df, sum)
  bad_totals <- totals[abs(totals$scenario_probability - 1.0) > 1e-9, ]
  if (nrow(bad_totals) > 0) {
    detail <- paste0(bad_totals$risk_name, " = ", bad_totals$scenario_probability)
    errors <- c(errors, sprintf(
      "scenario_probability does not sum to 1.0 for: %s",
      paste(detail, collapse = ", ")
    ))
  }

  # 5. consequence fields must not be negative
  consequence_fields <- c(
    "wireline_runs", "ct_days", "milling_plugs", "testing_days",
    "pump_days", "extra_stages", "logistics_days"
  )
  for (fld in consequence_fields) {
    bad <- df[!is.na(df[[fld]]) & df[[fld]] < 0, ]
    if (nrow(bad) > 0) {
      errors <- c(errors, sprintf(
        "Negative %s values for: %s",
        fld, paste(unique(bad$risk_name), collapse = ", ")
      ))
    }
  }

  # 6. scope must be stage / well / campaign. build_risk_table() (see
  # R/risk_library_engine.R) feeds this column straight into
  # compute_adjusted_risk_probability() for any risk that exists ONLY in this
  # library (no row in master_risks_assumptions.csv); an unrecognised value
  # there silently falls through to flat, non-compounding scaling with no
  # error -- exactly the bug class master_risks_assumptions.csv's scope
  # column is hard-validated against in validate_inputs.R::validate_assumptions().
  if ("scope" %in% names(df)) {
    allowed_scope <- c("stage", "well", "campaign")
    bad_rows_idx <- which(!is.na(df$scope) & trimws(df$scope) != "" &
                           !tolower(trimws(df$scope)) %in% allowed_scope)
    if (length(bad_rows_idx) > 0) {
      risk_label <- ifelse(is.na(df$risk_name[bad_rows_idx]) | trimws(df$risk_name[bad_rows_idx]) == "",
                           "(unnamed row)", df$risk_name[bad_rows_idx])
      detail <- sprintf("row %d (%s) = '%s'", bad_rows_idx, risk_label, df$scope[bad_rows_idx])
      errors <- c(errors, sprintf(
        "Invalid scope values (must be stage / well / campaign) for: %s",
        paste(detail, collapse = ", ")
      ))
    }
  }

  if (length(errors) > 0) {
    stop("Risk consequence library validation failed:\n", paste(errors, collapse = "\n"))
  }

  invisible(df)
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  path <- if (length(args) >= 1) args[1] else file.path(
    "data_templates", "risk_consequence_library_template_simple_severity.csv"
  )
  df <- read.csv(path, stringsAsFactors = FALSE)
  validate_risk_consequence_library(df)
  cat("All checks passed for", path, "\n")
}
