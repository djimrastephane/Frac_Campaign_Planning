# validate_inputs.R
# Input checks before running the simulation.
# Version 12: row-level diagnostics so users see exactly WHICH rows fail,
# plus probability range checks for risk rows.
# Version 13: reject non-numeric values in historical_wells' numeric columns
# up front (mirrors strict_as_numeric()'s diagnostics in
# engine_core.R, kept as a local copy so this validation file has
# no dependency on the engine). Without this, a stray text value silently
# turns the whole column character, and `df$frac_days_per_stage > 0` below
# does a string comparison instead of erroring -- producing a wrong
# n_pos_frac count rather than a readable failure.

# well_id / pad_id are identifiers, not used arithmetically downstream, so
# they're excluded.
.HISTORICAL_NUMERIC_COLS <- c(
  "stages_completed", "plugs_installed", "contingency_plugs", "frac_days",
  "cement_eval_days", "milling_days", "frac_days_per_stage", "milling_days_per_plug"
)

.check_historical_numeric <- function(df) {
  bad <- character(0)
  for (col in .HISTORICAL_NUMERIC_COLS) {
    raw <- df[[col]]
    coerced <- suppressWarnings(as.numeric(as.character(raw)))
    is_bad <- is.na(coerced) & !is.na(raw) & nchar(trimws(as.character(raw))) > 0
    if (any(is_bad)) {
      rows <- which(is_bad)
      well_label <- if ("well_id" %in% names(df)) df$well_id[rows] else rows
      detail <- paste0("row ", rows, " (", well_label, "): \"", as.character(raw)[rows], "\"")
      bad <- c(bad, sprintf("Column '%s' has non-numeric value(s): %s", col, paste(detail, collapse = "; ")))
    }
  }
  if (length(bad) > 0) {
    stop(
      "Historical wells file has non-numeric values in numeric columns:\n",
      paste(bad, collapse = "\n")
    )
  }
}

validate_historical_wells <- function(df) {
  required <- c(
    "well_id", "pad_id", "stages_completed", "plugs_installed",
    "contingency_plugs", "frac_days", "cement_eval_days", "milling_days",
    "frac_days_per_stage", "milling_days_per_plug"
  )

  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop("Historical wells file is missing columns: ", paste(missing, collapse = ", "))
  }

  if (nrow(df) == 0) {
    stop("Historical wells file contains no data rows.")
  }

  .check_historical_numeric(df)

  n_pos_frac <- sum(!is.na(df$frac_days_per_stage) & df$frac_days_per_stage > 0)
  n_pos_mill <- sum(!is.na(df$milling_days_per_plug) & df$milling_days_per_plug > 0)
  if (n_pos_frac == 0) stop("Historical wells file has no positive FracDaysPerStage values.")
  if (n_pos_mill == 0) stop("Historical wells file has no positive MillingDaysPerPlug values.")

  # Soft warnings — attached as an attribute so callers can surface them without
  # changing the return type.
  w <- character(0)
  n_bad_frac <- nrow(df) - n_pos_frac
  n_bad_mill <- nrow(df) - n_pos_mill
  if (n_bad_frac > 0)
    w <- c(w, sprintf("%d well(s) missing/zero FracDaysPerStage — excluded from bootstrap.", n_bad_frac))
  if (n_bad_mill > 0)
    w <- c(w, sprintf("%d well(s) missing/zero MillingDaysPerPlug — excluded from bootstrap.", n_bad_mill))
  if (nrow(df) < 10)
    w <- c(w, sprintf("Only %d wells — recommend 20+ for reliable distribution fitting.", nrow(df)))
  attr(df, "input_warnings") <- w

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

  # Empty risk name is a hard error: an unnamed risk row can't be referenced
  # by a risk_consequence_library upload or a Bayesian risk observation, and
  # silently sails through every downstream join as a blank label.
  empty_name <- risk_rows %>% dplyr::filter(is.na(variable) | trimws(variable) == "")
  if (nrow(empty_name) > 0) {
    stop(
      "Risk rows must have a non-empty 'Variable / Risk Event' name:\n",
      paste("row", utils::head(empty_name$.row, 10), collapse = "\n")
    )
  }

  # Duplicate risk names (case/whitespace-insensitive) is a hard error: the
  # engine treats every assumptions row as an independent risk, so two rows
  # for the "same" risk fire and propagate consequences independently,
  # silently double-counting that risk's probability and impact.
  dup_keys <- tolower(trimws(risk_rows$variable))
  dup_name <- risk_rows %>% dplyr::filter(dup_keys %in% dup_keys[duplicated(dup_keys)])
  if (nrow(dup_name) > 0) {
    detail <- paste0("row ", dup_name$.row, " (", dup_name$variable, ")")
    stop(
      "Risk rows have duplicate names (case/whitespace-insensitive) — each risk must appear once:\n",
      paste(utils::head(detail, 10), collapse = "\n"),
      if (nrow(dup_name) > 10) paste0("\n... and ", nrow(dup_name) - 10, " more.") else ""
    )
  }

  # Invalid (non-blank, unrecognised) scope is a hard error: the engine
  # silently treats any unmatched scope value as "well", so a typo like
  # "Stge" would otherwise change a stage-compounding risk to a flat
  # per-well probability with no error and no warning visible at run time.
  valid_scopes <- c("stage", "well", "campaign")
  if ("scope" %in% names(risk_rows)) {
    bad_scope <- risk_rows %>%
      dplyr::filter(!is.na(scope), trimws(scope) != "", !trimws(tolower(scope)) %in% valid_scopes)
    if (nrow(bad_scope) > 0) {
      detail <- paste0("row ", bad_scope$.row, " (", bad_scope$variable, "): scope = '", bad_scope$scope, "'")
      stop(
        "Risk rows have an invalid scope (must be stage / well / campaign):\n",
        paste(utils::head(detail, 10), collapse = "\n"),
        if (nrow(bad_scope) > 10) paste0("\n... and ", nrow(bad_scope) - 10, " more.") else ""
      )
    }
  }

  # Soft warnings
  w <- character(0)
  high_p <- risk_rows %>% dplyr::filter(!is.na(probability) & probability > 0.5)
  if (nrow(high_p) > 0)
    w <- c(w, sprintf("%d risk row(s) have probability > 50%% — check if intentional: %s.",
                      nrow(high_p), paste(utils::head(high_p$variable, 3), collapse = ", ")))
  no_scope <- if ("scope" %in% names(risk_rows)) {
    risk_rows %>% dplyr::filter(is.na(scope) | trimws(scope) == "")
  } else {
    risk_rows
  }
  if (nrow(no_scope) > 0) {
    detail <- paste0("row ", no_scope$.row, " (", no_scope$variable, ")")
    w <- c(w, sprintf(
      "%d risk row(s) missing scope — defaulting to 'well': %s%s.",
      nrow(no_scope), paste(utils::head(detail, 5), collapse = ", "),
      if (nrow(no_scope) > 5) sprintf(", and %d more", nrow(no_scope) - 5) else ""
    ))
  }
  attr(df, "input_warnings") <- w

  df
}
