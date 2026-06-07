# validate_inputs.R
# Basic input checks before running simulation.

validate_historical_wells <- function(df) {
  required <- c(
    "well_id", "pad_id", "stages_completed", "plugs_installed",
    "contingency_plugs", "frac_days", "scmt_days", "milling_days",
    "frac_days_per_stage", "milling_days_per_plug"
  )

  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop("Historical wells file is missing columns: ", paste(missing, collapse = ", "))
  }

  df
}

validate_assumptions <- function(df) {
  required <- c(
    "category", "variable", "type", "probability",
    "min_days", "most_likely_days", "max_days", "simulation_impact"
  )

  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop("Assumptions file is missing columns: ", paste(missing, collapse = ", "))
  }

  bad_rows <- df %>%
    dplyr::filter(
      !is.na(min_days), !is.na(most_likely_days), !is.na(max_days),
      !(min_days <= most_likely_days & most_likely_days <= max_days)
    )

  if (nrow(bad_rows) > 0) {
    stop("Some assumption rows do not satisfy Min <= Most Likely <= Max.")
  }

  df
}
