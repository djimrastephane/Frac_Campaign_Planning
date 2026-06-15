# load_inputs.R
# Read user-supplied CSV files and standardise column names.
# Version 12: defensive column checks with clear error messages, BOM-safe,
# whitespace trimming on character columns.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(janitor)
})

trim_chr_cols <- function(df) {
  df %>% mutate(across(where(is.character), ~ trimws(.x)))
}

load_historical_wells <- function(path) {
  # Skip lines beginning with # (template guidance comments)
  raw <- readLines(path, warn = FALSE)
  data_lines <- raw[!grepl("^\\s*#", raw)]
  suppressWarnings(
    readr::read_csv(paste(data_lines, collapse = "\n"),
                    show_col_types = FALSE, na = c("", "NA", "N/A"))
  ) %>%
    clean_names() %>%
    trim_chr_cols() %>%
    dplyr::filter(!is.na(well_id))   # drop any NA sentinel rows
}

load_master_assumptions <- function(path) {
  # Skip lines beginning with # (template guidance comments)
  raw <- readLines(path, warn = FALSE)
  data_lines <- raw[!grepl("^\\s*#", raw)]
  df <- suppressWarnings(
    readr::read_csv(paste(data_lines, collapse = "\n"),
                    show_col_types = FALSE, na = c("", "NA", "N/A"))
  ) %>%
    clean_names() %>%
    trim_chr_cols() %>%
    dplyr::filter(!is.na(variable_risk_event) |
                  "variable" %in% names(.))  # drop NA sentinel rows

  # clean_names() turns "Variable / Risk Event" into "variable_risk_event".
  # Fail with a clear message if the expected column is absent, rather than
  # letting rename() throw a cryptic error.
  if (!"variable_risk_event" %in% names(df)) {
    stop(
      "Assumptions file: expected a 'Variable / Risk Event' column. ",
      "Columns found: ", paste(names(df), collapse = ", ")
    )
  }

  required_numeric <- c("probability", "min_days", "most_likely_days", "max_days")
  missing <- setdiff(c(required_numeric, "category", "type", "simulation_impact"), names(df))
  if (length(missing) > 0) {
    stop(
      "Assumptions file is missing columns: ", paste(missing, collapse = ", "),
      ". Columns found: ", paste(names(df), collapse = ", ")
    )
  }

  df %>%
    rename(variable = variable_risk_event) %>%
    mutate(
      probability = suppressWarnings(as.numeric(probability)),
      min_days = suppressWarnings(as.numeric(min_days)),
      most_likely_days = suppressWarnings(as.numeric(most_likely_days)),
      max_days = suppressWarnings(as.numeric(max_days))
    )
}
