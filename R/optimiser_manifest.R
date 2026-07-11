# optimiser_manifest.R
# -----------------------------------------------------------------------------
# Phase 1 of the optimiser auditability pass (see docs/architecture.md and
# the root-cause investigation this follows up on): every optimiser export
# should carry enough information to reconstruct the run later. Before this
# file, the only optimiser export was a bare results CSV -- no seed, no
# assumptions snapshot, no risk tables, no historical data, no day rates.
# Nothing here changes optimiser SCORING, Pareto classification, the
# recommendation rule, cost calculations, or random-number generation --
# this module only assembles and hashes values the app already computed.
#
# Dependencies (source first): constants.R (thresholds), digest package.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

MANIFEST_SCHEMA_VERSION <- "1.0"
MANIFEST_HASH_ALGORITHM <- "sha256"

# ---- Hashing --------------------------------------------------------------
#
# Method (documented per the task's requirement to state the hashing method):
#   1. Data frames are canonicalised by sorting COLUMNS alphabetically by
#      name (row order is preserved -- row order can be semantically
#      meaningful, e.g. a user's risk-row ordering, and we don't want to
#      silently treat a reordered table as identical to a differently
#      substantiated one).
#   2. The canonicalised frame is coerced to a character matrix (NA -> the
#      literal token "<NA>", to distinguish a real empty string from a
#      missing value) and flattened into one string: columns separated by
#      U+001F (unit separator), rows by U+001E (record separator) -- control
#      characters chosen so they can never collide with real CSV/user text.
#   3. That string is hashed with digest::digest(..., algo = "sha256",
#      serialize = FALSE) -- i.e. SHA-256 of the UTF-8 bytes of the
#      canonical string, not of R's internal serialization format (which is
#      sensitive to R version and is not what "stable serialization order"
#      should mean here).
#   4. NULL / zero-row input hashes the literal token "<empty>" so an empty
#      table has a defined, stable hash instead of erroring.
.canonical_table_string <- function(df) {
  if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) return("<empty>")
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  df <- df[, sort(names(df)), drop = FALSE]
  m <- as.matrix(df)
  m[is.na(m)] <- "<NA>"
  header <- paste(sort(names(df)), collapse = "")
  rows <- apply(m, 1, paste, collapse = "")
  paste(c(header, rows), collapse = "")
}

#' Deterministic SHA-256 hash of a data frame's content, independent of
#' column order. See .canonical_table_string() for the exact method.
hash_optimiser_table <- function(df) {
  digest::digest(.canonical_table_string(df), algo = MANIFEST_HASH_ALGORITHM, serialize = FALSE)
}

#' Deterministic SHA-256 hash of an arbitrary (possibly nested) list, via
#' jsonlite with sorted names at every level -- so key order in R code can
#' never change the hash, only actual content changes can.
.canonical_list_json <- function(x) {
  sort_keys <- function(v) {
    if (is.list(v)) {
      if (!is.null(names(v)) && length(v) > 0) v <- v[order(names(v))]
      v <- lapply(v, sort_keys)
    }
    v
  }
  jsonlite::toJSON(sort_keys(x), auto_unbox = TRUE, digits = NA, null = "null")
}

hash_optimiser_payload <- function(x) {
  digest::digest(as.character(.canonical_list_json(x)), algo = MANIFEST_HASH_ALGORITHM, serialize = FALSE)
}

# ---- Git/version helpers ---------------------------------------------------
# Never allowed to fail the manifest build: a deployed environment may have
# no .git directory (e.g. a zipped release) or no git binary at all.
.git_info <- function(project_root) {
  # `git -C <dir>` runs git as if invoked from `dir`, without touching this
  # process's working directory (system2() has no `dir` argument -- passing
  # one is a silent no-op/error, not a cd).
  safe_git <- function(args) {
    tryCatch({
      out <- suppressWarnings(system2("git", c("-C", project_root, args),
                                      stdout = TRUE, stderr = FALSE))
      status <- attr(out, "status")
      if (!is.null(status) && status != 0) return(NA_character_)
      if (length(out) == 0 || !nzchar(out[1])) return(NA_character_)
      out[1]
    }, error = function(e) NA_character_)
  }
  or_unknown <- function(x) if (is.null(x) || is.na(x) || !nzchar(x)) "unknown" else x
  list(
    commit = or_unknown(safe_git(c("rev-parse", "--short", "HEAD"))),
    branch = or_unknown(safe_git(c("rev-parse", "--abbrev-ref", "HEAD")))
  )
}

#' Reads Version: from DESCRIPTION (the project's existing single source of
#' truth for the app version -- see DESCRIPTION and run_local.R). Falls back
#' to "unknown" rather than erroring if DESCRIPTION is missing or malformed.
.app_version <- function(project_root) {
  tryCatch({
    desc_path <- file.path(project_root, "DESCRIPTION")
    if (!file.exists(desc_path)) return("unknown")
    d <- as.data.frame(read.dcf(desc_path))
    v <- d[["Version"]][1]
    if (is.null(v) || is.na(v) || !nzchar(v)) "unknown" else v
  }, error = function(e) "unknown")
}

# ---- Manifest builder -------------------------------------------------------
#
#' Build a reproducibility manifest for one optimiser run.
#'
#' Pure function: every value the manifest describes is passed in explicitly
#' rather than read from Shiny reactives, so it can be unit-tested without a
#' running app and can never silently pick up a stale value from a different
#' reactive tick than the one that produced `results`.
#'
#' @param project_root      Project root (for DESCRIPTION + git lookup).
#' @param seed               Integer seed used for the run.
#' @param operation_modes    Character vector of modes evaluated.
#' @param scheduling_mode    "event" or "formula" (pre_frac_scheduling).
#' @param n_wells            Campaign size.
#' @param screen_iterations  Screening iteration count.
#' @param refine_iterations  Refinement iteration count.
#' @param top_n_refine       How many top-cost configs were refined.
#' @param results            The optimise_campaign_scenarios() output.
#' @param resource_search_ranges Named list of c(min, max) per resource arg.
#' @param day_rates          Named list, one entry per resource's $/day.
#' @param active_parameters_df  current_locked_rows() (Campaign Setup / Base
#'   Operation rows) -- the ACTIVE, possibly-edited grid, not the template.
#' @param active_risks_df    current_risk_rows() -- ditto for risk rows.
#' @param risk_consequence_df The active risk_consequence_library table.
#' @param historical_wells   The historical wells data actually used.
#' @param using_synthetic    TRUE if no historical_wells.csv was uploaded.
#' @param historical_filename Uploaded filename (basename only -- see
#'   redact_filename()), or NA if synthetic.
#' @param excluded_well_ids  Character vector of excluded well IDs (may be
#'   empty).
#' @param bayesian_applied   TRUE if a Bayesian-merged well set was used.
#'
#' @return A nested list, ready for jsonlite::toJSON(), with a `hashes`
#'   section computed from the tables actually supplied.
build_optimiser_run_manifest <- function(
    project_root,
    seed, operation_modes, scheduling_mode, n_wells,
    screen_iterations, refine_iterations, top_n_refine,
    results, scenario_grid,
    resource_search_ranges, day_rates,
    active_parameters_df, active_risks_df, risk_consequence_df,
    historical_wells, using_synthetic, historical_filename = NA_character_,
    excluded_well_ids = character(0), bayesian_applied = FALSE
) {
  git <- .git_info(project_root)
  now <- Sys.time()

  # These 5 are PURE content hashes -- a function of the table's data only,
  # nothing else. Two runs with identical inputs (parameters, risks,
  # consequences, historical data, scenario grid) produce identical values
  # here, regardless of when either run happened. This is what "identical
  # inputs produce identical hashes" means and is safe to assert in tests.
  hashes <- list(
    active_parameters_hash = hash_optimiser_table(active_parameters_df),
    active_risks_hash      = hash_optimiser_table(active_risks_df),
    risk_consequence_hash  = hash_optimiser_table(risk_consequence_df),
    historical_data_hash   = hash_optimiser_table(historical_wells),
    scenario_grid_hash     = hash_optimiser_table(scenario_grid)
  )

  manifest <- list(
    run_identity = list(
      app_version = .app_version(project_root),
      git_commit = git$commit,
      source_branch = git$branch,
      run_timestamp = format(now, "%Y-%m-%dT%H:%M:%S%z"),
      manifest_schema_version = MANIFEST_SCHEMA_VERSION,
      run_id = paste0(format(now, "%Y%m%dT%H%M%OS3"), "-",
                      substr(hash_optimiser_payload(list(seed = seed, t = as.numeric(now))), 1, 8))
    ),
    simulation_controls = list(
      seed = as.integer(seed),
      operation_modes = as.character(operation_modes),
      scheduling_mode = as.character(scheduling_mode),
      n_wells = as.integer(n_wells),
      screen_iterations = as.integer(screen_iterations),
      refine_iterations = as.integer(refine_iterations),
      top_n_refine = as.integer(top_n_refine),
      n_scenarios_generated = if (is.null(results)) NA_integer_ else length(unique(results$scenario_id)),
      n_scenarios_refined = if (is.null(results)) NA_integer_ else sum(results$stage == "refined"),
      common_random_numbers = TRUE,
      optimiser_objective = "Minimise total mobilisation cost (spread_rate_per_day x P50 days) among Pareto-efficient scenarios",
      recommendation_rule = "Cheapest scenario on the Pareto frontier (lowest total_mobilisation_cost where pareto == TRUE)",
      pareto_dominance_rule = "Scenario A dominates B if A.p50_days <= B.p50_days AND A.total_mobilisation_cost <= B.total_mobilisation_cost, with at least one strict inequality; non-dominated scenarios are Pareto-efficient"
    ),
    resource_configuration = list(
      search_ranges = resource_search_ranges
    ),
    economic_assumptions = list(
      day_rates = day_rates,
      currency = "USD",
      mobilisation_cost_formula = "total_mobilisation_cost = spread_rate_per_day x p50_days, where spread_rate_per_day = sum(units_i x day_rate_i) across all 5 resources",
      idle_cost_note = "idle_cost/idle_days price ONLY frac-fleet idle time spent waiting on wireline stage readiness -- not testing-gated post-frac idle, and not other resources' idle time"
    ),
    active_inputs = list(
      active_parameters_rows = nrow(active_parameters_df %||% tibble()),
      active_risks_rows = nrow(active_risks_df %||% tibble()),
      risk_consequence_rows = nrow(risk_consequence_df %||% tibble())
    ),
    historical_data = list(
      source = if (isTRUE(using_synthetic)) "bundled_synthetic" else "uploaded",
      row_count = nrow(historical_wells %||% tibble()),
      filename = if (isTRUE(using_synthetic)) NA_character_ else redact_filename(historical_filename),
      outlier_policy = if (length(excluded_well_ids) > 0) {
        "Extreme outliers (>P99 or >2x P90 on frac_days_per_stage) excluded"
      } else {
        "No outlier exclusion applied"
      },
      excluded_well_ids = as.character(excluded_well_ids),
      excluded_count = length(excluded_well_ids),
      bayesian_update_applied = isTRUE(bayesian_applied)
    ),
    hashes = hashes
  )

  # manifest_hash, by contrast, is computed over the WHOLE manifest,
  # including run_timestamp/run_id -- it identifies this specific run, not
  # just its inputs, so it legitimately differs between two runs even when
  # every one of the 5 input hashes above is identical. Use the input hashes
  # (or compare them individually) to test input reproducibility; use
  # manifest_hash only to identify/verify one specific exported file.
  manifest$hashes$manifest_hash <- hash_optimiser_payload(manifest)
  manifest
}

#' Strip any path component from a user-supplied filename, keeping only the
#' basename -- so an uploaded file's local path never reaches an export.
redact_filename <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(x)) return(NA_character_)
  basename(as.character(x))
}

#' Flattens the scalar (non-table) sections of a manifest into a key/value
#' tibble for optimiser_run_inputs.csv -- a CSV rendering of run_identity +
#' simulation_controls + resource_configuration + economic_assumptions +
#' active_inputs + hashes, for readers who don't want to parse JSON.
manifest_to_flat_df <- function(manifest) {
  flatten <- function(x, prefix = "") {
    out <- list()
    for (nm in names(x)) {
      key <- if (nzchar(prefix)) paste0(prefix, ".", nm) else nm
      v <- x[[nm]]
      if (is.list(v) && !is.data.frame(v)) {
        out <- c(out, flatten(v, key))
      } else {
        out[[key]] <- paste(v, collapse = "; ")
      }
    }
    out
  }
  flat <- flatten(manifest[setdiff(names(manifest), "active_inputs")])
  flat <- c(flat, flatten(manifest$active_inputs, "active_inputs"))
  tibble(key = names(flat), value = vapply(flat, function(v) as.character(v), character(1)))
}
