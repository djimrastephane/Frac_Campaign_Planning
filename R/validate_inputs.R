# validate_inputs.R
# Input checks before running the simulation.
# Version 12: row-level diagnostics so users see exactly WHICH rows fail,
# plus probability range checks for risk rows.

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

  if (nrow(df) == 0) {
    stop("Historical wells file contains no data rows.")
  }

  n_pos_frac <- sum(!is.na(df$frac_days_per_stage) & df$frac_days_per_stage > 0)
  n_pos_mill <- sum(!is.na(df$milling_days_per_plug) & df$milling_days_per_plug > 0)
  if (n_pos_frac == 0) stop("Historical wells file has no positive FracDaysPerStage values.")
  if (n_pos_mill == 0) stop("Historical wells file has no positive MillingDaysPerPlug values.")

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

  # Triangle ordering check: report the offending rows by name, not just count.
  bad_rows <- df %>%
    dplyr::mutate(.row = dplyr::row_number()) %>%
    dplyr::filter(
      !is.na(min_days), !is.na(most_likely_days), !is.na(max_days),
      !(min_days <= most_likely_days & most_likely_days <= max_days)
    )

  if (nrow(bad_rows) > 0) {
    detail <- paste0(
      "row ", bad_rows$.row, " (", bad_rows$variable, "): ",
      bad_rows$min_days, " / ", bad_rows$most_likely_days, " / ", bad_rows$max_days
    )
    stop(
      "Assumption rows violate Min <= Most Likely <= Max:\n",
      paste(utils::head(detail, 10), collapse = "\n"),
      if (nrow(bad_rows) > 10) paste0("\n... and ", nrow(bad_rows) - 10, " more.") else ""
    )
  }

  # Probability range check for risk rows.
  risk_rows <- df %>%
    dplyr::mutate(.row = dplyr::row_number()) %>%
    dplyr::filter(tolower(trimws(type)) == "risk")

  bad_prob <- risk_rows %>%
    dplyr::filter(is.na(probability) | probability < 0 | probability > 1)

  if (nrow(bad_prob) > 0) {
    detail <- paste0("row ", bad_prob$.row, " (", bad_prob$variable, "): probability = ", bad_prob$probability)
    stop(
      "Risk rows must have probability between 0 and 1:\n",
      paste(utils::head(detail, 10), collapse = "\n")
    )
  }

  df
}
