# Run this from the project root:
# source("run_local.R")

required <- c("shiny", "readr", "dplyr", "ggplot2", "DT", "janitor", "purrr", "stringr", "tibble")
missing <- required[!required %in% rownames(installed.packages())]
if (length(missing) > 0) {
  install.packages(missing)
}

shiny::runApp("app")
