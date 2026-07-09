# Run this from the project root:
# source("run_local.R")
#
# DESCRIPTION (Imports field) is the single source of truth for required
# packages -- read it here instead of duplicating the list, so run_local.R
# can never drift out of sync with what the app actually loads.

deps <- tryCatch(
  as.data.frame(read.dcf("DESCRIPTION")),
  error = function(e) stop("Could not read DESCRIPTION from the project root. ",
                            "Run source(\"run_local.R\") from the repository root.")
)

.parse_pkg_field <- function(field) {
  if (!field %in% names(deps) || is.na(deps[[field]][1])) return(character(0))
  raw <- strsplit(deps[[field]][1], ",")[[1]]
  trimws(gsub("\\(.*\\)", "", raw))  # drop version constraints, e.g. "dplyr (>= 1.0.0)"
}

required <- .parse_pkg_field("Imports")
suggested <- .parse_pkg_field("Suggests")

missing_required <- required[!required %in% rownames(installed.packages())]
if (length(missing_required) > 0) {
  message("Installing required packages: ", paste(missing_required, collapse = ", "))
  install.packages(missing_required)
}

# Suggests are optional (report_decision_page.R / plots.R / the audit-zip
# handler already degrade gracefully via requireNamespace() when absent) --
# just flag what's missing, don't install automatically.
missing_suggested <- suggested[!suggested %in% rownames(installed.packages())]
if (length(missing_suggested) > 0) {
  message("Optional packages not installed (some features will degrade gracefully): ",
          paste(missing_suggested, collapse = ", "))
}

still_missing <- required[!required %in% rownames(installed.packages())]
if (length(still_missing) > 0) {
  stop("Required package(s) failed to install: ", paste(still_missing, collapse = ", "))
}

shiny::runApp("app")
