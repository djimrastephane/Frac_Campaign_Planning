# load_inputs.R
# Read user-supplied CSV files and standardise column names.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(janitor)
})

load_historical_wells <- function(path) {
  read_csv(path, show_col_types = FALSE) %>%
    clean_names()
}

load_master_assumptions <- function(path) {
  read_csv(path, show_col_types = FALSE, na = c("", "NA", "N/A")) %>%
    clean_names() %>%
    rename(
      variable = variable_risk_event,
      min_days = min_days,
      most_likely_days = most_likely_days,
      max_days = max_days,
      simulation_impact = simulation_impact
    ) %>%
    mutate(
      probability = suppressWarnings(as.numeric(probability)),
      min_days = suppressWarnings(as.numeric(min_days)),
      most_likely_days = suppressWarnings(as.numeric(most_likely_days)),
      max_days = suppressWarnings(as.numeric(max_days))
    )
}
