# engine_core.R
# Split out of simulation_engine_fast.R (see docs/architecture_cleanup_plan.md).
# The Monte Carlo engine itself: sampling, scheduling, risk-grid mechanics, and
# the simulate_campaign_detailed()/simulate_one_campaign() entry points. Must be
# sourced BEFORE summaries.R / report_pdf.R / optimiser_cascade.R -- they call
# into this file but not vice versa.
#
# Version 16
#
# === Architecture ============================================================
# This is a Monte Carlo WORKLOAD AGGREGATOR, not a discrete-event simulator.
# Each iteration samples per-well durations, propagates risk consequences into
# resource workloads, and computes campaign duration as the critical path across
# resource streams. It does NOT model intra-well or inter-well sequencing at
# calendar resolution.
#
# === Scheduler note (greedy limitation) =====================================
# schedule_post_frac_milling() assigns wells to milling/testing resources in
# release-time order ("first-come, first-served"). This is an operational
# sequence approximation. It does NOT solve for the globally optimal assignment
# that minimises post-frac completion time. In multi-pad campaigns with uneven
# milling workloads, reordering could reduce the critical path. This is
# intentional: a full optimal scheduler is roadmap v3.0 (discrete-event engine).
# Users should treat campaign duration estimates as planning-level estimates,
# not execution schedules.
#
# === Performance (v12) =======================================================
#   1. Parameter cache: assumption lookups resolved ONCE before the loop.
#   2. Static risk grid: wells × risks cross-join built once; only random draws
#      per iteration.
#   3. Lazy delay sampling: delays sampled only for occurred events (~5% of grid).
#   4. Vectorised stage assignment: ceiling(runif(n)*stages) replaces per-row loop.
#   5. rowsum() aggregation replaces per-iteration dplyr group_by/summarise.
#
# === Robustness improvements (v16) ==========================================
#   6. <<- eliminated from draw_risks_on_grid: accumulation now uses a pure
#      function (accumulate_sums) that returns the updated matrix, making the
#      code safe for future parallelisation with future/mirai.
#   7. Strict numeric validation in build_param_cache: suppressWarnings removed;
#      non-numeric values in critical columns now raise row-level errors
#      immediately rather than silently producing NA distributions.
#   8. Config-driven resource/consequence classification: regex patterns in
#      risk_resource_class() and consequence_library() are now defined in
#      named tibble configs (RESOURCE_CLASS_CONFIG, CONSEQUENCE_CONFIG) that
#      can be inspected, extended, and overridden at runtime.
#
# === Memory note (risk grid) ================================================
# build_risk_grid() stores n_wells * n_risks vectors. For the modelled campaign
# sizes (20-40 wells, ~20 risks), peak grid size is ~800 rows and memory
# consumption is negligible (<1 MB). For significantly larger campaigns
# (>200 wells), a data.table-based grid would reduce memory pressure; the
# current list approach is retained for simplicity and compatibility.
#
# NOTE: seed reproducibility changed at v12. A given seed will not reproduce
# v11 results bit-for-bit; distributions are statistically identical.

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(tibble)
  library(tidyr)
})

normalise_text <- function(x) {
  str_squish(str_to_lower(as.character(x)))
}

triangle_sample <- function(min_val, mode_val, max_val, n = 1) {
  min_val <- rep(as.numeric(min_val), length.out = n)
  mode_val <- rep(as.numeric(mode_val), length.out = n)
  max_val <- rep(as.numeric(max_val), length.out = n)

  out <- rep(NA_real_, n)
  ok <- !is.na(min_val) & !is.na(mode_val) & !is.na(max_val) &
    max_val >= min_val & mode_val >= min_val & mode_val <= max_val

  if (!any(ok)) return(out)

  deterministic <- ok & (max_val == min_val)
  out[deterministic] <- min_val[deterministic]

  stochastic <- ok & !deterministic
  if (any(stochastic)) {
    u <- runif(sum(stochastic))
    mn <- min_val[stochastic]
    md <- mode_val[stochastic]
    mx <- max_val[stochastic]
    c_val <- (md - mn) / (mx - mn)

    out[stochastic] <- ifelse(
      u < c_val,
      mn + sqrt(u * (mx - mn) * (md - mn)),
      mx - sqrt((1 - u) * (mx - mn) * (mx - md))
    )
  }

  out
}

# ---------------------------------------------------------------------------
# 1. Parameter cache: normalise the assumptions table ONCE, then O(1) lookups.
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Strict numeric coercion helper.
# Raises a clear row-level error if any value cannot be parsed as numeric.
# This replaces suppressWarnings(as.numeric(...)) throughout the cache, which
# would silently produce NA and cause distributions to be truncated or absent.
# ---------------------------------------------------------------------------
strict_as_numeric <- function(x, col_name, variable_names) {
  # Try to coerce; suppress only the standard coercion warning so we can
  # detect and report failures ourselves.
  vals <- suppressWarnings(as.numeric(as.character(x)))
  bad <- which(is.na(vals) & !is.na(x) & nchar(trimws(as.character(x))) > 0)
  if (length(bad) > 0) {
    detail <- paste0(
      '"', as.character(x)[bad], '" (row "', variable_names[bad], '")',
      collapse = "; "
    )
    stop(sprintf(
      "Column '%s' has non-numeric values that cannot be used in the simulation: %s
",
      col_name, detail
    ))
  }
  vals
}

build_param_cache <- function(assumptions) {
  keys <- normalise_text(assumptions$variable)
  # keep first occurrence of each key, matching v11 slice(1) behaviour
  first_idx <- !duplicated(keys)

  cache <- assumptions[first_idx, , drop = FALSE]
  cache_keys <- keys[first_idx]
  vnames <- as.character(cache$variable)

  # Strict coercion: fail loudly on malformed values (e.g. "10 days", "n/a").
  # Only probability is allowed to be NA for non-risk rows (it has no default).
  list(
    keys = cache_keys,
    probability    = suppressWarnings(as.numeric(cache$probability)),  # NA allowed
    min_days       = strict_as_numeric(cache$min_days,          "min_days",         vnames),
    most_likely_days = strict_as_numeric(cache$most_likely_days, "most_likely_days", vnames),
    max_days       = strict_as_numeric(cache$max_days,           "max_days",         vnames)
  )
}

cache_lookup <- function(cache, variable_name) {
  match(normalise_text(variable_name), cache$keys)
}

sample_param_cached <- function(cache, variable_name, n = 1) {
  i <- cache_lookup(cache, variable_name)
  if (is.na(i)) stop("Missing parameter in assumptions file: ", variable_name)
  triangle_sample(cache$min_days[i], cache$most_likely_days[i], cache$max_days[i], n)
}

sample_integer_param_cached <- function(cache, variable_name, n = 1) {
  i <- cache_lookup(cache, variable_name)
  if (is.na(i)) stop("Missing parameter in assumptions file: ", variable_name)

  mn <- as.integer(round(cache$min_days[i]))
  mx <- as.integer(round(cache$max_days[i]))

  if (is.na(mn) || is.na(mx)) stop("Parameter has missing min/max values: ", variable_name)
  if (mx < mn) stop("Parameter has Max lower than Min: ", variable_name)

  sample(seq(mn, mx), size = n, replace = TRUE)
}

get_param_prob_cached <- function(cache, variable_name, default = NA_real_) {
  i <- cache_lookup(cache, variable_name)
  if (is.na(i) || is.na(cache$probability[i])) return(default)
  cache$probability[i]
}

# ---------------------------------------------------------------------------
# Legacy helpers kept for backwards compatibility (slow path; not used in the
# main loop any more, but retained in case external scripts call them).
# ---------------------------------------------------------------------------

get_param_row <- function(assumptions, variable_name) {
  assumptions %>%
    mutate(.variable_key = normalise_text(variable)) %>%
    filter(.variable_key == normalise_text(variable_name)) %>%
    slice(1)
}

sample_param <- function(assumptions, variable_name, n = 1) {
  row <- get_param_row(assumptions, variable_name)
  if (nrow(row) == 0) stop("Missing parameter in assumptions file: ", variable_name)
  triangle_sample(row$min_days, row$most_likely_days, row$max_days, n)
}

sample_param_default <- function(assumptions, variable_name, n = 1, min_val, mode_val, max_val) {
  row <- get_param_row(assumptions, variable_name)
  if (nrow(row) == 0) return(triangle_sample(min_val, mode_val, max_val, n))
  mn <- suppressWarnings(as.numeric(row$min_days))
  md <- suppressWarnings(as.numeric(row$most_likely_days))
  mx <- suppressWarnings(as.numeric(row$max_days))
  if (is.na(mn) || is.na(md) || is.na(mx)) return(triangle_sample(min_val, mode_val, max_val, n))
  triangle_sample(mn, md, mx, n)
}

sample_integer_param <- function(assumptions, variable_name, n = 1) {
  row <- get_param_row(assumptions, variable_name)
  if (nrow(row) == 0) stop("Missing parameter in assumptions file: ", variable_name)
  mn <- as.integer(round(row$min_days))
  mx <- as.integer(round(row$max_days))
  if (is.na(mn) || is.na(mx)) stop("Parameter has missing min/max values: ", variable_name)
  if (mx < mn) stop("Parameter has Max lower than Min: ", variable_name)
  sample(seq(mn, mx), size = n, replace = TRUE)
}

build_pad_assignment <- function(n_wells, assumptions) {
  build_pad_assignment_cached(n_wells, build_param_cache(assumptions))
}

# ---------------------------------------------------------------------------
# 2. Pad assignment: batch-sample pad sizes instead of one lookup per pad.
# ---------------------------------------------------------------------------

build_pad_assignment_cached <- function(n_wells, cache) {
  i <- cache_lookup(cache, "wells per pad")
  if (is.na(i)) stop("Missing parameter in assumptions file: Wells per pad")
  mn <- max(1L, as.integer(round(cache$min_days[i])))
  mx <- as.integer(round(cache$max_days[i]))
  if (is.na(mn) || is.na(mx) || mx < mn) stop("Invalid 'Wells per pad' min/max.")

  # Worst case every pad has the minimum size: at most ceiling(n_wells / mn) pads.
  max_pads <- ceiling(n_wells / mn)
  sizes <- sample(seq(mn, mx), size = max_pads, replace = TRUE)
  csum <- cumsum(sizes)
  n_pads <- which(csum >= n_wells)[1]
  sizes <- sizes[seq_len(n_pads)]
  sizes[n_pads] <- sizes[n_pads] - (csum[n_pads] - n_wells)  # trim overshoot

  tibble(
    well_index = seq_len(n_wells),
    pad_id = rep(paste0("Pad_", str_pad(seq_len(n_pads), 2, pad = "0")), sizes)
  )
}

# ---------------------------------------------------------------------------
# Config-driven resource classification.
# Patterns are defined in a named tibble (RESOURCE_CLASS_CONFIG) rather than
# hardcoded in case_when() branches. This means:
#   - The mapping is inspectable: print(RESOURCE_CLASS_CONFIG)
#   - It can be extended without modifying the classification function
#   - Missing patterns fall through to the "frac" default with a clear audit
#     trail rather than silently landing in the wrong bucket
#
# To extend: add rows to RESOURCE_CLASS_CONFIG before sourcing this file, or
# pass a custom config to risk_resource_class(). Patterns are matched in order;
# first match wins.
# ---------------------------------------------------------------------------

RESOURCE_CLASS_CONFIG <- tibble::tribble(
  ~pattern,                                                           ~resource_class,
  "wireline|perforation|upct|gun|plug",                              "wireline",
  "ct unit|cement|cleanout|scraper",                                 "ct",
  "milling",                                                         "milling",
  "weather|regulatory|permit|security|lockdown|camp|access|vendor",  "external"
  # default: "frac" (see function below)
)

risk_resource_class <- function(category, variable,
                                config = RESOURCE_CLASS_CONFIG) {
  key <- normalise_text(paste(category, variable))
  result <- rep("frac", length(key))  # default
  # Apply in reverse so first row in config takes precedence
  for (j in rev(seq_len(nrow(config)))) {
    hit <- str_detect(key, config$pattern[j])
    hit[is.na(hit)] <- FALSE
    result[hit] <- config$resource_class[j]
  }
  result
}

# ---------------------------------------------------------------------------
# NEW v15: Risk consequence propagation.
# Technical risks cascade into operational workload, not just direct delay:
#   Risk -> operational consequence -> resource workload -> schedule impact.
# Defaults below are deliberately conservative planning values; every one can
# be overridden per-risk by adding these columns to the assumptions CSV:
#   extra_wireline_runs, extra_ct_days, extra_milling_plugs,
#   extra_testing_days, extra_frac_days
# (extra plugs / extra stages remain driven by the Simulation Impact text.)
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Config-driven consequence library.
# Replaces the closure-returning function with a module-level tibble
# (CONSEQUENCE_CONFIG) so the mapping is:
#   - Inspectable at runtime: print(CONSEQUENCE_CONFIG)
#   - Extensible without modifying this file (add rows before sourcing)
#   - Passed explicitly to derive_risk_consequences() for testability
#
# Consequence values are conservative planning defaults.
# Every column can be overridden per-risk by adding the corresponding column
# to master_risks_assumptions.csv; CSV values take precedence over library.
#
# Column meanings (per occurred event):
#   extra_wireline_runs  : additional wireline trips (each = wireline_run_days)
#   extra_ct_days        : additional CT unit-days (e.g. cleanout, intervention)
#   extra_milling_plugs  : additional plugs to mill (each at well's milling rate)
#   extra_testing_days   : additional testing unit-days (re-test, verification)
#   extra_frac_days      : additional pumping time (re-frac stage, screenout pump)
# ---------------------------------------------------------------------------

CONSEQUENCE_CONFIG <- tibble::tribble(
  ~pattern,                          ~extra_wireline_runs, ~extra_ct_days, ~extra_milling_plugs, ~extra_testing_days, ~extra_frac_days,
  "screen ?out",                      1,                    0.50,           0,                    0,                   0.25,
  "plug pressure test",               1,                    0,              0,                    0.15,                0,
  "premature plug",                   0,                    0.25,           0,                    0.30,                0,
  "perforation|gun misfire|misfire",  1,                    0,              0,                    0,                   0,
  "isolation plug fail",              1,                    0.50,           1,                    0.25,                0,
  "upct",                             1,                    0.25,           0,                    0,                   0,
  "cement in casing",                 0,                    1.00,           0,                    0,                   0,
  "cement above plug",                0,                    0.50,           0,                    0,                   0
)

derive_risk_consequences <- function(risk_table, config = CONSEQUENCE_CONFIG) {
  lib <- config
  cons_cols <- c("extra_wireline_runs", "extra_ct_days", "extra_milling_plugs",
                 "extra_testing_days", "extra_frac_days")

  key <- normalise_text(risk_table$risk_event)
  defaults <- matrix(0, nrow = nrow(risk_table), ncol = length(cons_cols),
                     dimnames = list(NULL, cons_cols))
  for (j in seq_len(nrow(lib))) {
    hit <- str_detect(key, lib$pattern[j])
    if (any(hit)) {
      for (cc in cons_cols) defaults[hit, cc] <- lib[[cc]][j]
    }
  }

  # *_is_override flags record whether each cell came from a real CSV value
  # (highest precedence) vs. a filled-in default, so callers that also have a
  # risk_consequence_library (lower precedence than CSV, higher than the
  # pattern default) can resolve the three-way precedence correctly instead of
  # the CSV/default distinction being lost once both are coalesced here.
  for (cc in cons_cols) {
    override_col <- paste0(cc, "_is_override")
    if (cc %in% names(risk_table)) {
      # CSV value wins where supplied; library default fills the gaps.
      raw <- risk_table[[cc]]
      csv_val <- suppressWarnings(as.numeric(raw))
      # Warn if any CSV override value could not be parsed (don't fail the whole
      # run, but alert the user so they can fix their CSV).
      bad <- which(is.na(csv_val) & !is.na(raw) & nchar(trimws(as.character(raw))) > 0)
      if (length(bad) > 0) {
        warning(sprintf(
          "Column '%s' in assumptions CSV has non-numeric values for risks: %s. Library defaults used.",
          cc, paste(risk_table$risk_event[bad], collapse = ", ")
        ))
      }
      risk_table[[override_col]] <- !is.na(csv_val)
      risk_table[[cc]] <- ifelse(is.na(csv_val), defaults[, cc], csv_val)
    } else {
      risk_table[[override_col]] <- FALSE
      risk_table[[cc]] <- defaults[, cc]
    }
  }
  risk_table
}

# ---------------------------------------------------------------------------
# 3. Static risk grid: built once, reused for every iteration.
# ---------------------------------------------------------------------------

build_risk_grid <- function(risk_table, n_wells) {
  if (nrow(risk_table) == 0 || n_wells == 0) return(NULL)

  n_risks <- nrow(risk_table)
  grid_n <- n_wells * n_risks

  well_index <- rep(seq_len(n_wells), each = n_risks)
  risk_idx <- rep(seq_len(n_risks), times = n_wells)

  list(
    n = grid_n,
    well_index = well_index,
    risk_idx = risk_idx,
    # static per-row risk attributes (recycled from risk_table by risk_idx)
    category = risk_table$category[risk_idx],
    risk_event = risk_table$risk_event[risk_idx],
    resource_class = risk_table$resource_class[risk_idx],
    simulation_impact = risk_table$simulation_impact[risk_idx],
    adjusted_probability = risk_table$adjusted_probability[risk_idx],
    adds_plug = risk_table$adds_plug[risk_idx],
    adds_stage = risk_table$adds_stage[risk_idx],
    min_days = risk_table$min_days[risk_idx],
    most_likely_days = risk_table$most_likely_days[risk_idx],
    max_days = risk_table$max_days[risk_idx],
    extra_wireline_runs = risk_table$extra_wireline_runs[risk_idx],
    extra_ct_days = risk_table$extra_ct_days[risk_idx],
    extra_milling_plugs = risk_table$extra_milling_plugs[risk_idx],
    extra_testing_days = risk_table$extra_testing_days[risk_idx],
    extra_frac_days = risk_table$extra_frac_days[risk_idx],
    extra_wireline_runs_is_override = risk_table$extra_wireline_runs_is_override[risk_idx],
    extra_ct_days_is_override = risk_table$extra_ct_days_is_override[risk_idx],
    extra_milling_plugs_is_override = risk_table$extra_milling_plugs_is_override[risk_idx],
    extra_testing_days_is_override = risk_table$extra_testing_days_is_override[risk_idx],
    extra_frac_days_is_override = risk_table$extra_frac_days_is_override[risk_idx],
    lib_key = if ("lib_key" %in% names(risk_table)) risk_table$lib_key[risk_idx] else NA_character_,
    stage_eligible = risk_table$resource_class[risk_idx] %in% c("wireline", "frac"),
    is_campaign_scope = risk_table$is_campaign_scope[risk_idx]
  )
}

empty_risk_event_log <- function() {
  tibble(
    simulation_id = integer(), operation_mode = character(), well_id = character(), pad_id = character(),
    stage_id = character(), category = character(), risk_event = character(), resource_class = character(),
    probability_used = numeric(), delay_days = numeric(), extra_plugs = numeric(), extra_stages = numeric(),
    extra_wireline_runs = numeric(), extra_wireline_days = numeric(), extra_ct_days = numeric(),
    extra_milling_plugs = numeric(), extra_milling_days = numeric(), extra_testing_days = numeric(),
    extra_frac_days = numeric(),
    min_delay_days = numeric(), most_likely_delay_days = numeric(), max_delay_days = numeric(),
    simulation_impact = character(), severity = character(), extra_logistics_days = numeric()
  )
}

# Draw one iteration of risk outcomes on the precomputed grid.
# Returns: risk_log (occurred events only) + per-well numeric summary matrix.
# v15: consequence propagation - induced wireline/CT/milling/testing/frac
# workload is accumulated alongside the direct delay.
SUM_COLS <- c("frac", "wireline", "ct", "milling", "external", "total",
              "plugs", "stages", "wl_runs", "wl_run_days", "ct_x",
              "mill_x_plugs", "test_x", "frac_x", "logistics_x")

empty_sums_matrix <- function(n_wells) {
  matrix(0, nrow = n_wells, ncol = length(SUM_COLS),
         dimnames = list(NULL, SUM_COLS))
}

draw_risks_on_grid <- function(grid, well_df, iter_id, operation_mode,
                               wireline_run_days = 0.25, collect_log = TRUE,
                               lib_wide = NULL) {
  # Standard per-well draw
  occurs <- stats::rbinom(grid$n, 1, grid$adjusted_probability) == 1

  # Campaign-scope risks: draw once per unique risk (not per well).
  # If the risk fires, apply it to ONE randomly selected well to represent
  # a campaign-wide event (e.g. crew absence, weather, permit delay).
  # This prevents inflating high-frequency resource/external risks across all
  # n_wells independently, which overweights them vs technical risks.
  if (any(grid$is_campaign_scope)) {
    n_wells_grid <- nrow(well_df)
    # For each unique campaign-scope risk, determine if it fires this iteration
    camp_risk_indices <- which(grid$is_campaign_scope)
    # Identify unique risks by their probability pattern (one per risk type per campaign)
    # Group by risk_idx: use the grid's risk_idx if available, else by risk_event
    unique_camp <- unique(grid$risk_event[camp_risk_indices])
    for (re in unique_camp) {
      # Positions of this risk in the grid (one per well)
      positions <- which(grid$risk_event == re & grid$is_campaign_scope)
      if (length(positions) == 0) next
      # Use first position's probability for the single draw
      fires <- stats::rbinom(1, 1, grid$adjusted_probability[positions[1]]) == 1
      if (fires) {
        # Assign to one randomly chosen well; suppress all other positions
        chosen <- sample(positions, 1)
        occurs[positions] <- FALSE
        occurs[chosen] <- TRUE
      } else {
        occurs[positions] <- FALSE
      }
    }
  }

  n_occ <- sum(occurs)

  n_wells <- nrow(well_df)
  sums <- empty_sums_matrix(n_wells)

  if (n_occ == 0) {
    return(list(risk_log = empty_risk_event_log(), sums = sums))
  }

  w <- grid$well_index[occurs]
  delay <- triangle_sample(grid$min_days[occurs], grid$most_likely_days[occurs],
                           grid$max_days[occurs], n_occ)
  delay[is.na(delay)] <- 0
  rclass <- grid$resource_class[occurs]
  extra_plugs <- as.numeric(grid$adds_plug[occurs])

  # Consequence quantities for the occurred events.
  # extra_* here is already CSV-override-or-CONSEQUENCE_CONFIG-default
  # (resolved by derive_risk_consequences()); this is the lowest two tiers of
  # the three-way precedence chain.
  c_wl_runs <- as.numeric(grid$extra_wireline_runs[occurs])
  c_ct_days <- as.numeric(grid$extra_ct_days[occurs])
  c_mill_plugs <- as.numeric(grid$extra_milling_plugs[occurs])
  c_test_days <- as.numeric(grid$extra_testing_days[occurs])
  c_frac_days <- as.numeric(grid$extra_frac_days[occurs])

  # Severity-tier overlay: for occurred events whose risk is covered by an
  # uploaded risk_consequence_library, sample a severity tier (Minor/Moderate/
  # Major, weighted by the library's scenario_probability) and use that tier's
  # consequence magnitude in place of the CONSEQUENCE_CONFIG default -- but
  # never in place of a real CSV override on master_risks_assumptions.csv,
  # which keeps top precedence. Fully vectorised (no per-event loop) to stay
  # in line with this engine's performance requirements.
  severity_event <- rep(NA_character_, n_occ)
  ev_lib_key <- grid$lib_key[occurs]
  has_lib <- !is.null(lib_wide) && any(!is.na(ev_lib_key))
  if (has_lib) {
    lib_idx <- which(!is.na(ev_lib_key))
    m <- lib_wide[match(ev_lib_key[lib_idx], lib_wide$key), ]
    u <- runif(length(lib_idx))
    # Per-event thresholds differ by risk, so this is a direct elementwise
    # comparison rather than findInterval() (whose breakpoint vector must be
    # fixed across all observations, not vary per row). cum_moderate is
    # already the cumulative threshold (minor + moderate), not the moderate
    # share alone.
    sev_tier <- ifelse(u <= m$cum_minor, 1L,
                       ifelse(u <= m$cum_moderate, 2L, 3L))
    severity_event[lib_idx] <- c("Minor", "Moderate", "Major")[sev_tier]

    pick <- function(field) {
      m[[paste0(field, "_minor")]]    * (sev_tier == 1) +
      m[[paste0(field, "_moderate")]] * (sev_tier == 2) +
      m[[paste0(field, "_major")]]    * (sev_tier == 3)
    }
    lib_val <- function(field) {
      out <- rep(NA_real_, n_occ)
      out[lib_idx] <- pick(field)
      out
    }
    is_ovr_wl   <- as.logical(grid$extra_wireline_runs_is_override[occurs])
    is_ovr_ct   <- as.logical(grid$extra_ct_days_is_override[occurs])
    is_ovr_mill <- as.logical(grid$extra_milling_plugs_is_override[occurs])
    is_ovr_test <- as.logical(grid$extra_testing_days_is_override[occurs])
    is_ovr_frac <- as.logical(grid$extra_frac_days_is_override[occurs])

    overlay <- function(default_val, is_override, lib_field) {
      lv <- lib_val(lib_field)
      ifelse(is_override, default_val, ifelse(!is.na(lv), lv, default_val))
    }
    c_wl_runs    <- overlay(c_wl_runs,    is_ovr_wl,   "wireline_runs")
    c_ct_days    <- overlay(c_ct_days,    is_ovr_ct,   "ct_days")
    c_mill_plugs <- overlay(c_mill_plugs, is_ovr_mill, "milling_plugs")
    c_test_days  <- overlay(c_test_days,  is_ovr_test, "testing_days")
    c_frac_days  <- overlay(c_frac_days,  is_ovr_frac, "pump_days")

    lib_extra_stages <- lib_val("extra_stages")
    extra_stages_lib <- ifelse(is.na(lib_extra_stages), 0, lib_extra_stages)

    # logistics_days has no master_risks_assumptions.csv override column (that
    # file predates this concept), so it's library value or zero -- no 3-way
    # precedence to resolve, unlike the 5 fields above.
    lib_logistics_days <- lib_val("logistics_days")
    c_logistics_days <- ifelse(is.na(lib_logistics_days), 0, lib_logistics_days)
  } else {
    extra_stages_lib <- rep(0, n_occ)
    c_logistics_days <- rep(0, n_occ)
  }
  c_wl_days <- c_wl_runs * wireline_run_days
  c_mill_days <- c_mill_plugs * well_df$milling_days_per_plug[w]
  extra_stages <- as.numeric(grid$adds_stage[occurs]) + extra_stages_lib

  # Vectorised stage assignment
  ws <- well_df$stages[w]
  eligible <- grid$stage_eligible[occurs] & !is.na(ws) & ws > 0
  stage_num <- rep(NA_integer_, n_occ)
  stage_num[eligible] <- as.integer(ceiling(runif(sum(eligible)) * ws[eligible]))
  stage_id <- ifelse(is.na(stage_num), "Well-level",
                     paste0("Stage_", str_pad(stage_num, 2, pad = "0")))

  # Pure-function accumulation: no <<- / non-local assignment.
  # accumulate_into() returns a new matrix with the aggregated values added.
  # This eliminates the closure side-effect and makes the function safe for
  # future parallelisation (e.g. future_lapply, mirai workers).
  accumulate_into <- function(mat, col, idx, val) {
    agg <- rowsum(val, group = idx)
    mat[as.integer(rownames(agg)), col] <- mat[as.integer(rownames(agg)), col] + agg[, 1]
    mat
  }
  for (cls in c("frac", "wireline", "ct", "milling", "external")) {
    sel <- rclass == cls
    if (any(sel)) sums <- accumulate_into(sums, cls, w[sel], delay[sel])
  }
  sums <- accumulate_into(sums, "total",       w, delay)
  sums <- accumulate_into(sums, "plugs",       w, extra_plugs)
  sums <- accumulate_into(sums, "stages",      w, extra_stages)
  sums <- accumulate_into(sums, "wl_runs",     w, c_wl_runs)
  sums <- accumulate_into(sums, "wl_run_days", w, c_wl_days)
  sums <- accumulate_into(sums, "ct_x",        w, c_ct_days)
  sums <- accumulate_into(sums, "mill_x_plugs",w, c_mill_plugs)
  sums <- accumulate_into(sums, "test_x",      w, c_test_days)
  sums <- accumulate_into(sums, "frac_x",      w, c_frac_days)
  sums <- accumulate_into(sums, "logistics_x", w, c_logistics_days)

  # risk_log is a pure output artifact (no random draws here), so skipping it
  # when the caller does not need it changes nothing numerically and leaves the
  # RNG stream untouched.
  if (!collect_log) {
    return(list(risk_log = empty_risk_event_log(), sums = sums))
  }

  risk_log <- tibble(
    simulation_id = iter_id,
    operation_mode = operation_mode,
    well_id = well_df$well_id[w],
    pad_id = well_df$pad_id[w],
    stage_id = stage_id,
    category = grid$category[occurs],
    risk_event = grid$risk_event[occurs],
    resource_class = rclass,
    probability_used = grid$adjusted_probability[occurs],
    delay_days = delay,
    extra_plugs = extra_plugs,
    extra_stages = extra_stages,
    extra_wireline_runs = c_wl_runs,
    extra_wireline_days = c_wl_days,
    extra_ct_days = c_ct_days,
    extra_milling_plugs = c_mill_plugs,
    extra_milling_days = c_mill_days,
    extra_testing_days = c_test_days,
    extra_frac_days = c_frac_days,
    min_delay_days = grid$min_days[occurs],
    most_likely_delay_days = grid$most_likely_days[occurs],
    max_delay_days = grid$max_days[occurs],
    simulation_impact = grid$simulation_impact[occurs],
    severity = severity_event,
    extra_logistics_days = c_logistics_days
  )

  list(risk_log = risk_log, sums = sums)
}


# ---------------------------------------------------------------------------
# Post-frac milling scheduler
# ---------------------------------------------------------------------------
# Operational rule captured here:
#   milling cannot start until a well has been totally fracced and released.
#   milling also requires a testing unit to be available; the testing unit cannot
#   be occupied on post-frac flowback/testing at the same time.
#   optional CT support can only be used after the well is released for milling
#   and only up to the CT spare-capacity budget.
#
# This is intentionally a lightweight resource scheduler, not a full discrete-
# event simulator. It fixes the earlier campaign-level CT credit logic where a
# long conventional frac path could create unrealistic CT credit before wells
# were actually eligible for milling.
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Zipper benefit breakdown (v17.5)
# Decomposes the P50 campaign saving into its contributing sources.
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# Synthetic historical wells for use when no historical CSV is available.
# Values represent a reasonable plug-and-perf campaign baseline.
# The app warns users when synthetic data is active.
#
# Calibration basis (conservative planning estimates):
#   FracDaysPerStage:  triangular(0.5, 0.8, 2.5) -- covers slow and fast wells
#   MillingDaysPerPlug: triangular(0.4, 0.7, 1.5) -- covers standard and difficult
# ---------------------------------------------------------------------------
synthetic_historical_wells <- function(n = 30, seed = 42) {
  set.seed(seed)
  # Triangular sampler
  rtri <- function(n, mn, ml, mx) {
    u <- runif(n)
    fc <- (ml - mn) / (mx - mn)
    ifelse(u < fc,
           mn + sqrt(u * (mx - mn) * (ml - mn)),
           mx - sqrt((1 - u) * (mx - mn) * (mx - ml)))
  }
  stages <- sample(5:8, n, replace = TRUE)
  plugs  <- stages - 1L
  frac_dps   <- rtri(n, 0.50, 0.80, 2.50)
  mill_dpp   <- rtri(n, 0.40, 0.68, 1.50)
  frac_days  <- round(frac_dps * stages, 2)
  mill_days  <- round(mill_dpp * plugs,  2)
  pad_ids    <- rep(paste0("SynPad_", seq_len(ceiling(n / 5))),
                   each = 5)[seq_len(n)]

  tibble::tibble(
    well_id             = paste0("SynWell_", stringr::str_pad(seq_len(n), 3, pad = "0")),
    pad_id              = pad_ids,
    stages_planned      = stages,
    stages_completed    = stages,
    plugs_installed     = plugs,
    contingency_plugs   = 0L,
    frac_days           = frac_days,
    cement_eval_days           = round(runif(n, 0.5, 1.5), 2),
    milling_days        = mill_days,
    frac_days_per_stage = round(frac_dps, 3),
    milling_days_per_plug = round(mill_dpp, 3)
  )
}

# ---------------------------------------------------------------------------
# Pre-frac scheduler (v18, opt-in via pre_frac_scheduling = "event"):
# CT cleanout, wireline readiness, and frac pumping as real resource-
# availability-vector queues, instead of the workload-accounting formulas
# in simulate_campaign_detailed()'s two-pass duration calc. Same pattern as
# schedule_post_frac_milling() below (preallocated vectors, one pass over
# wells in arrival order, earliest-available-unit assignment).
#
# Per well, in order: CT -> Wireline -> Frac.
#   CT:       independent timeline -- naturally runs in parallel with the
#             previous well's frac, since nothing ties ct_avail to frac_avail.
#   Wireline: assigned to whichever unit (across the whole pool) is
#             earliest-free, NOT tied to "this well's own pace" -- a unit
#             that finished well i-1 early can start well i+1 immediately.
#             This is the actual capability gap this scheduler closes.
#   Frac:     assigned to whichever frac fleet is earliest-free; a well's
#             frac cannot FINISH before its own wireline finishes (the
#             well-level approximation of stage-by-stage pacing -- frac and
#             wireline still can't be modeled as two independent sequential
#             blocks at this granularity, see plan).
#
# mode_factor (zipper pump-speed multiplier) and frac_tree_constraint_delay_days
# (zipper wellhead-swap overhead) are NOT modeled here as queueing effects --
# they're baked into frac_workload_days before it reaches this function,
# exactly as in the formula path. well_transition_days is likewise already
# folded into frac_workload_days (frac-fleet downtime).
#
# Returns one row per well (in well_index order) with start/finish times for
# each resource, plus per-resource busy-time totals for the summary rollup.
schedule_pre_frac <- function(well_order_index,
                              ct_workload_days,
                              wireline_workload_days,
                              frac_workload_days,
                              ct_units,
                              wireline_units,
                              frac_fleets) {
  n <- length(well_order_index)
  if (n == 0) {
    return(list(
      well_schedule = tibble(),
      total_ct_busy_days = 0,
      total_wireline_busy_days = 0,
      total_frac_busy_days = 0,
      total_wireline_readiness_delay_days = 0,
      total_wireline_capacity_wait_days = 0,
      total_ct_caused_wait_days = 0,
      total_ct_queueing_wait_days = 0,
      total_ct_duration_floor_wait_days = 0
    ))
  }

  ct_units       <- max(1L, as.integer(round(ct_units)))
  wireline_units <- max(1L, as.integer(round(wireline_units)))
  frac_fleets    <- max(1L, as.integer(round(frac_fleets)))

  ct_avail       <- rep(0, ct_units)
  wireline_avail <- rep(0, wireline_units)
  frac_avail     <- rep(0, frac_fleets)

  v_well_index    <- integer(n)
  v_ct_start      <- numeric(n)
  v_ct_finish     <- numeric(n)
  v_wireline_start  <- numeric(n)
  v_wireline_finish <- numeric(n)
  v_frac_start    <- numeric(n)
  v_frac_finish   <- numeric(n)
  v_wireline_unit <- integer(n)
  v_frac_fleet    <- integer(n)
  v_wireline_wait_days <- numeric(n)
  v_wireline_capacity_wait_days <- numeric(n)
  v_ct_caused_wait_days <- numeric(n)
  v_ct_queueing_wait_days <- numeric(n)
  v_ct_duration_floor_wait_days <- numeric(n)

  for (pos in seq_len(n)) {
    i <- well_order_index[pos]

    # --- CT: earliest-available unit, independent of frac/wireline timelines.
    ct_unit <- which.min(ct_avail)
    ct_start <- ct_avail[ct_unit]
    ct_finish <- ct_start + ct_workload_days[pos]
    ct_avail[ct_unit] <- ct_finish

    # --- Wireline: earliest-available unit across the whole pool, gated only
    # on this well's own CT finishing (cleanout/cement-eval must clear first).
    wl_unit <- which.min(wireline_avail)
    wl_avail_before <- wireline_avail[wl_unit]
    wl_start <- max(wl_avail_before, ct_finish)
    wl_finish <- wl_start + wireline_workload_days[pos]
    wireline_avail[wl_unit] <- wl_finish
    # Counterfactual: what wireline would have finished at if CT had been
    # instant (i.e. wireline's OWN pool queueing only, dropping the max()
    # with ct_finish above). Used below to attribute frac's wait between
    # "wireline capacity" and "CT gating" -- see schedule_pre_frac()'s
    # ct_caused_wait_days comment for why these are the only two possible
    # causes in this pipeline.
    wl_finish_no_ct <- wl_avail_before + wireline_workload_days[pos]
    # Second counterfactual: what wireline would have finished at if THIS
    # well's CT had run on a fully dedicated unit (zero queueing) instead of
    # the real, possibly-shared unit -- i.e. start CT at t=0 for this well
    # alone. This isolates ct_caused_wait_days into:
    #   - ct_queueing_wait_days:       removable by adding CT units
    #   - ct_duration_floor_wait_days: NOT removable by adding units -- CT's
    #     own per-well task simply takes longer than wireline+frac's pace,
    #     so even an instantly-available CT unit would still gate wireline.
    # Found by direct investigation: a synthetic check with ct_units raised
    # all the way to n_wells (zero queueing possible) still left a nonzero,
    # flat residual -- conflating that floor with queueing-driven delay would
    # have told a user "add CT capacity" in a case where no amount of CT
    # capacity fixes it; only a shorter CT task would.
    ct_finish_dedicated <- ct_workload_days[pos]
    wl_start_dedicated <- max(wl_avail_before, ct_finish_dedicated)
    wl_finish_dedicated <- wl_start_dedicated + wireline_workload_days[pos]

    # --- Frac: earliest-available fleet; can't finish before this well's
    # wireline does (well-level pacing approximation).
    #
    # wireline_wait_days is the LOCAL extra time this well's frac finish was
    # pushed out by waiting on wireline, i.e. how much the max() below bites
    # -- NOT frac_finish - wireline_finish, which would compare two unrelated
    # cumulative-queue positions (the frac fleet may be many wells behind the
    # wireline pool's position purely from FIFO queueing, independent of any
    # real pacing effect) and blow up meaninglessly as wireline_units grows.
    fleet <- which.min(frac_avail)
    frac_start <- frac_avail[fleet]
    frac_finish_unpaced <- frac_start + frac_workload_days[pos]
    frac_finish <- max(frac_finish_unpaced, wl_finish)
    frac_avail[fleet] <- frac_finish

    v_well_index[pos]      <- i
    v_ct_start[pos]        <- ct_start
    v_ct_finish[pos]       <- ct_finish
    v_wireline_start[pos]  <- wl_start
    v_wireline_finish[pos] <- wl_finish
    v_frac_start[pos]      <- frac_start
    v_frac_finish[pos]     <- frac_finish
    v_wireline_unit[pos]   <- wl_unit
    v_frac_fleet[pos]      <- fleet
    total_wait <- max(0, wl_finish - frac_finish_unpaced)
    capacity_wait <- max(0, wl_finish_no_ct - frac_finish_unpaced)
    dedicated_wait <- max(0, wl_finish_dedicated - frac_finish_unpaced)
    v_wireline_wait_days[pos] <- total_wait
    v_wireline_capacity_wait_days[pos] <- capacity_wait
    # wl_finish >= wl_finish_no_ct always (the max() with ct_finish can only
    # push it up), so total_wait >= capacity_wait and this is never negative.
    v_ct_caused_wait_days[pos] <- total_wait - capacity_wait
    # duration_floor: the part of the CT-caused wait that survives even with
    # zero CT queueing (dedicated unit). queueing: the rest -- specifically
    # the part that goes away once CT stops being shared/contended.
    # wl_finish_dedicated <= wl_finish (real CT position is never earlier
    # than a dedicated unit's), so dedicated_wait <= total_wait and the
    # subtraction below is never negative.
    v_ct_duration_floor_wait_days[pos] <- max(0, dedicated_wait - capacity_wait)
    v_ct_queueing_wait_days[pos] <- v_ct_caused_wait_days[pos] - v_ct_duration_floor_wait_days[pos]
  }

  sched <- tibble(
    well_index = v_well_index,
    ct_start_day = v_ct_start,
    ct_finish_day = v_ct_finish,
    wireline_unit = v_wireline_unit,
    wireline_start_day = v_wireline_start,
    wireline_finish_day = v_wireline_finish,
    frac_fleet = v_frac_fleet,
    frac_start_day = v_frac_start,
    frac_finish_day = v_frac_finish,
    wireline_wait_days = v_wireline_wait_days,
    wireline_capacity_wait_days = v_wireline_capacity_wait_days,
    ct_caused_wait_days = v_ct_caused_wait_days,
    ct_queueing_wait_days = v_ct_queueing_wait_days,
    ct_duration_floor_wait_days = v_ct_duration_floor_wait_days
  )

  list(
    well_schedule = sched,
    total_ct_busy_days = sum(ct_workload_days),
    total_wireline_busy_days = sum(wireline_workload_days),
    total_frac_busy_days = sum(frac_workload_days),
    # Sum of the local per-well wireline_wait_days above -- replaces the
    # formula path's pmax(wireline_fleet_days - frac_fleet_days_est, 0)
    # *estimate* with an actual value read off the schedule.
    total_wireline_readiness_delay_days = sum(v_wireline_wait_days),
    # Attribution split: how much of the total is wireline's own capacity
    # vs CT gating wireline's start. These two always sum exactly to
    # total_wireline_readiness_delay_days above -- see wireline_capacity_wait_days'
    # definition in the loop for why these are the only two possible causes
    # in a CT -> Wireline -> Frac pipeline.
    total_wireline_capacity_wait_days = sum(v_wireline_capacity_wait_days),
    total_ct_caused_wait_days = sum(v_ct_caused_wait_days),
    # Second-level split of total_ct_caused_wait_days: how much is removable
    # by adding CT units (queueing) vs a floor that survives even with a
    # dedicated CT unit per well (CT's own task duration exceeding
    # wireline+frac's pace). These two sum exactly to total_ct_caused_wait_days.
    total_ct_queueing_wait_days = sum(v_ct_queueing_wait_days),
    total_ct_duration_floor_wait_days = sum(v_ct_duration_floor_wait_days)
  )
}

schedule_post_frac_milling <- function(release_times,
                                       milling_workload_days,
                                       flowback_testing_days,
                                       milling_units,
                                       testing_units,
                                       ct_units,
                                       allow_ct_for_milling = FALSE,
                                       ct_milling_efficiency = 0.65,
                                       ct_support_budget_days = 0) {
  n <- length(release_times)
  if (n == 0) {
    return(list(
      post_frac_completion_day = 0,
      milling_completion_day = 0,
      testing_completion_day = 0,
      dedicated_milling_workload_days = 0,
      ct_milling_support_equiv_days = 0,
      ct_milling_support_ct_days = 0,
      testing_occupied_for_milling_days = 0,
      testing_flowback_workload_days = 0,
      testing_total_workload_days = 0,
      well_schedule = tibble()
    ))
  }

  release_times <- as.numeric(release_times)
  milling_workload_days <- pmax(as.numeric(milling_workload_days), 0)
  flowback_testing_days <- pmax(as.numeric(flowback_testing_days), 0)

  milling_units <- max(1L, as.integer(round(milling_units)))
  testing_units <- max(1L, as.integer(round(testing_units)))
  ct_units <- max(0L, as.integer(round(ct_units)))
  ct_milling_efficiency <- max(as.numeric(ct_milling_efficiency), 1e-9)
  ct_support_budget_days <- pmax(as.numeric(ct_support_budget_days), 0)

  mill_avail <- rep(0, milling_units)
  test_avail <- rep(0, testing_units)
  ct_avail <- if (isTRUE(allow_ct_for_milling) && ct_units > 0 && ct_support_budget_days > 0) rep(0, ct_units) else numeric(0)
  ct_budget_remaining <- ct_support_budget_days

  order_idx <- order(release_times, seq_along(release_times))

  # perf (round 2): collect schedule columns into preallocated typed vectors and
  # build ONE tibble after the loop, instead of a tibble() per well per iteration
  # (was line ~783, 61% of total runtime). Bit-identical: same values, same
  # column order, same types, same row order (well_index 1..n).
  v_well_index     <- integer(n)
  v_frac_release   <- numeric(n)
  v_mill_work      <- numeric(n)
  v_mill_start     <- numeric(n)
  v_mill_finish    <- numeric(n)
  v_mill_resource  <- character(n)
  v_mill_res_id    <- rep(NA_integer_, n)
  v_test_for_mill  <- rep(NA_integer_, n)
  v_ct_days        <- numeric(n)
  v_dedicated_days <- numeric(n)
  v_flow_work      <- numeric(n)
  v_flow_start     <- numeric(n)
  v_flow_finish    <- numeric(n)
  v_flow_test_id   <- integer(n)
  dedicated_mill_busy <- 0
  ct_equiv_done <- 0
  ct_busy_days <- 0
  testing_for_mill_busy <- 0
  testing_flowback_busy <- 0

  for (pos in seq_along(order_idx)) {
    i <- order_idx[pos]
    rel <- release_times[i]
    mill_work <- milling_workload_days[i]

    mill_start <- rel
    mill_finish <- rel
    mill_resource <- "None"
    mill_resource_id <- NA_integer_
    test_for_mill_id <- NA_integer_
    ct_days_used <- 0
    dedicated_days_used <- 0

    if (!is.na(mill_work) && mill_work > 0) {
      best <- list(finish = Inf, start = NA_real_, type = NA_character_, res_id = NA_integer_,
                   test_id = NA_integer_, duration = NA_real_, ct_days = 0, dedicated_days = 0)

      # Dedicated milling units. Milling occupies one milling unit and one testing unit.
      for (m in seq_len(milling_units)) {
        for (t in seq_len(testing_units)) {
          st <- max(rel, mill_avail[m], test_avail[t])
          fin <- st + mill_work
          if (fin < best$finish) {
            best <- list(finish = fin, start = st, type = "Milling unit", res_id = m,
                         test_id = t, duration = mill_work, ct_days = 0,
                         dedicated_days = mill_work)
          }
        }
      }

      # Optional CT support. CT is slower/faster according to efficiency, and still
      # requires an available testing unit during the milling operation.
      if (length(ct_avail) > 0 && ct_budget_remaining > 0) {
        ct_duration <- mill_work / ct_milling_efficiency
        if (ct_duration <= ct_budget_remaining + 1e-9) {
          for (c in seq_along(ct_avail)) {
            for (t in seq_len(testing_units)) {
              st <- max(rel, ct_avail[c], test_avail[t])
              fin <- st + ct_duration
              if (fin < best$finish) {
                best <- list(finish = fin, start = st, type = "CT support", res_id = c,
                             test_id = t, duration = ct_duration, ct_days = ct_duration,
                             dedicated_days = 0)
              }
            }
          }
        }
      }

      mill_start <- best$start
      mill_finish <- best$finish
      mill_resource <- best$type
      mill_resource_id <- best$res_id
      test_for_mill_id <- best$test_id
      ct_days_used <- best$ct_days
      dedicated_days_used <- best$dedicated_days

      if (identical(mill_resource, "Milling unit")) {
        mill_avail[mill_resource_id] <- mill_finish
        dedicated_mill_busy <- dedicated_mill_busy + dedicated_days_used
      } else if (identical(mill_resource, "CT support")) {
        ct_avail[mill_resource_id] <- mill_finish
        ct_budget_remaining <- max(ct_budget_remaining - ct_days_used, 0)
        ct_busy_days <- ct_busy_days + ct_days_used
        ct_equiv_done <- ct_equiv_done + mill_work
      }

      test_avail[test_for_mill_id] <- mill_finish
      testing_for_mill_busy <- testing_for_mill_busy + (mill_finish - mill_start)
    }

    # Post-frac flowback/testing starts only after milling is complete for that well
    # and after a testing unit is available.
    flow_work <- flowback_testing_days[i]
    flow_test_id <- which.min(test_avail)
    flow_start <- max(mill_finish, test_avail[flow_test_id])
    flow_finish <- flow_start + flow_work
    test_avail[flow_test_id] <- flow_finish
    testing_flowback_busy <- testing_flowback_busy + flow_work

    v_well_index[i]     <- i
    v_frac_release[i]   <- rel
    v_mill_work[i]      <- mill_work
    v_mill_start[i]     <- mill_start
    v_mill_finish[i]    <- mill_finish
    v_mill_resource[i]  <- mill_resource
    v_mill_res_id[i]    <- mill_resource_id
    v_test_for_mill[i]  <- test_for_mill_id
    v_ct_days[i]        <- ct_days_used
    v_dedicated_days[i] <- dedicated_days_used
    v_flow_work[i]      <- flow_work
    v_flow_start[i]     <- flow_start
    v_flow_finish[i]    <- flow_finish
    v_flow_test_id[i]   <- flow_test_id
  }

  sched <- tibble(
    well_index = v_well_index,
    frac_release_day = v_frac_release,
    milling_workload_days = v_mill_work,
    milling_start_day = v_mill_start,
    milling_finish_day = v_mill_finish,
    milling_resource = v_mill_resource,
    milling_resource_id = v_mill_res_id,
    testing_unit_for_milling = v_test_for_mill,
    ct_milling_days_used = v_ct_days,
    dedicated_milling_days_used = v_dedicated_days,
    flowback_testing_days = v_flow_work,
    flowback_start_day = v_flow_start,
    flowback_finish_day = v_flow_finish,
    testing_unit_for_flowback = v_flow_test_id
  )
  list(
    post_frac_completion_day = max(c(test_avail, mill_avail, ct_avail, release_times), na.rm = TRUE),
    milling_completion_day = if (nrow(sched) > 0) max(sched$milling_finish_day, na.rm = TRUE) else 0,
    testing_completion_day = max(test_avail, na.rm = TRUE),
    dedicated_milling_workload_days = dedicated_mill_busy,
    ct_milling_support_equiv_days = ct_equiv_done,
    ct_milling_support_ct_days = ct_busy_days,
    testing_occupied_for_milling_days = testing_for_mill_busy,
    testing_flowback_workload_days = testing_flowback_busy,
    testing_total_workload_days = testing_for_mill_busy + testing_flowback_busy,
    well_schedule = sched
  )
}

# ===========================================================================
# WORKFLOW CONFIGURATION (v17.1)
# ===========================================================================
#
# The operational sequence is defined in WORKFLOW_CONFIG rather than
# hardcoded in the main simulation loop. This tibble controls:
#
#   - Which activities exist for each phase (pre_frac, frac_stage, post_frac)
#   - Which resource each activity draws from
#   - Whether the activity is ALWAYS present or conditional
#   - Which CSV parameter row provides the duration
#   - Whether it is "sequential" (adds to path) or "parallel" (absorbed into
#     available time -- does not extend the path if < campaign)
#
# To CHANGE the workflow:
#   Option A (no code): modify WORKFLOW_CONFIG before sourcing this file.
#   Option B (app UI): the Workflow editor tab exposes this tibble as an
#             editable DT table; changes propagate to all downstream calcs.
#   Option C (CSV): supply a workflow_config.csv in the same directory as
#             the assumptions CSV; load_workflow_config() reads it.
#
# Column definitions:
#   activity        : short name used in audit output
#   phase           : pre_frac | frac_stage | post_frac
#   resource        : Wireline | Frac fleet | CT / cleanout | Milling | Testing unit
#   duration_source : "param:<name>" -> look up in assumptions CSV
#                   : "formula:<expr>" -> evaluated per-well (n_stages, n_plugs available)
#                   : "historical" -> sampled from historical_wells
#   conditional     : "" = always active
#                   : "!cement_eval_offline" = only if cement eval is online
#                   : "is_zipper" = only in zipper mode
#   path_type       : "sequential" = adds to resource workload path
#                   : "parallel"   = absorbed if < available campaign time
#   notes           : free-text description
# ===========================================================================

# ---------------------------------------------------------------------------
# WORKFLOW_CONFIG (v17.4)
# One row per modelled activity. This is the authoritative sequence reference.
#
# Key scheduling rules NOT visible in this table (see Workflow tab):
#
#   CT PARALLELISM (conventional):
#     CT cleanout runs in PARALLEL with frac execution on the previous well.
#     frac_related_per_well = max(ct_workload, frac_workload + wireline_workload)
#     CT only gates campaign if ct_workload > (frac + wireline) per well.
#
#   CT PARALLELISM (zipper):
#     CT precedes each well (less parallel time available).
#     frac_related_per_well = ct_workload + max(frac_workload, wireline_workload)
#
#   cement eval OFFLINE RULE:
#     If wireline_units >= 2: cement eval always runs offline (spare unit available).
#     If wireline_units == 1: Cement eval offline probability from assumptions CSV.
#
#   WELL TRANSITION TYPE:
#     First well on a new pad: pad_to_pad_move_hours (full mobilisation).
#     Subsequent wells on same pad: well_to_well_transition_hours (skid/move).
#     In zipper: within-pad transition halved (swap delay covers most of it).
#
#   POST-FRAC SCHEDULING:
#     Milling starts as soon as well is frac-released AND milling + testing
#     units are both free. Runs in parallel with ongoing frac on other wells.
#     Testing unit is held during both milling and flowback phases.
# ---------------------------------------------------------------------------
WORKFLOW_CONFIG <- tibble::tribble(
  ~activity,                   ~phase,       ~resource,        ~duration_source,                            ~conditional,         ~path_type,    ~notes,
  # --- Pre-frac (per well, before first stage) ----------------------------
  "Pad-to-pad move",           "pre_frac",   "Frac fleet",     "formula:pad_to_pad_move_days",              "is_first_on_pad",    "sequential",  "Full rig-down + transport + rig-up when moving to a new pad. Applies once per pad (first well only). Set in sidebar: Pad-to-pad move, h.",
  "Within-pad well transition","pre_frac",   "Frac fleet",     "formula:well_to_well_transition_days",      "!is_first_on_pad",   "sequential",  "Short skid or lateral move to next wellhead on same pad. Applies to every non-first well. Set in sidebar: Within-pad well transition, h.",
  "CT cleanout / scraper",     "pre_frac",   "CT / cleanout",  "param:Scraper / cleanout run",              "",                   "parallel",    "CT scraper run. Runs in PARALLEL with frac on previous well (conventional). Only gates campaign if CT is slower than frac+wireline. See CT parallelism rule.",
  "Cement evaluation",          "pre_frac",   "CT / cleanout",  "param:Cement eval duration",                       "!cement_eval_offline",      "parallel",    "Cement evaluation log via CT. Skipped if cement eval runs offline. AUTO-OFFLINE when wireline_units >= 2 (spare unit available). Otherwise: CSV probability row.",
  # --- Frac stage loop (per stage, repeats N_stages times) ----------------
  "Perforate stage",           "frac_stage", "Wireline",       "formula:wireline_time_per_stage_days",      "",                   "sequential",  "Wireline perforation run per stage. Time set in sidebar: Wireline time per stage, h.",
  "Set isolation plug",        "frac_stage", "Wireline",       "param:Isolation plug duration",             "",                   "sequential",  "Wireline plug-setting run after frac. Separate trip unless combo gun-plug tool used.",
  "Temperature log",           "frac_stage", "Wireline",       "formula:temp_log_days/n_stages",            "temp_log",           "sequential",  "Optional temperature survey. Only for stages selected in campaign setup. Adds one wireline trip.",
  "Pump stage",                "frac_stage", "Frac fleet",     "formula:frac_time_per_stage_days",          "",                   "sequential",  "FULL STAGE CYCLE TIME: pump time + fluid loading + iron rig-up/down + pressure test + flush. Set in sidebar: Stage cycle time, h.",
  "Frac settling",             "frac_stage", "Frac fleet",     "formula:frac_settling_time_days",           "",                   "sequential",  "Pressure bleed-off before wireline re-entry. Set in sidebar: Settling before next wireline run, h.",
  # --- Post-frac (per well, discrete scheduler) ---------------------------
  "Mill out plugs",            "post_frac",  "Milling",        "formula:plugs * milling_days_per_plug",     "",                   "sequential",  "Drill out all isolation plugs. Starts as soon as well is frac-released AND milling + testing units are both free. Runs in parallel with ongoing frac.",
  "Flowback + well test",      "post_frac",  "Testing unit",   "formula:flowback_days",                     "",                   "sequential",  "Post-frac flowback and pressure build-up test. Testing unit held from milling start until flowback complete."
)

# Load a user-supplied workflow config from CSV (optional override).
# The CSV must have the same columns as WORKFLOW_CONFIG.
load_workflow_config <- function(path = NULL) {
  if (is.null(path) || !file.exists(path)) return(WORKFLOW_CONFIG)
  df <- readr::read_csv(path, show_col_types = FALSE)
  required <- c("activity","phase","resource","duration_source","conditional","path_type")
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    warning("workflow_config.csv missing columns: ", paste(missing, collapse=", "),
            ". Using built-in WORKFLOW_CONFIG.")
    return(WORKFLOW_CONFIG)
  }
  message("Loaded custom workflow config: ", nrow(df), " activities from ", path)
  df
}

# Derive resource-phase mapping from workflow config (used in summary/charts).
# Returns named list: resource -> list of phases it participates in.
workflow_resource_phases <- function(cfg = WORKFLOW_CONFIG) {
  cfg %>%
    dplyr::group_by(resource) %>%
    dplyr::summarise(phases = paste(unique(phase), collapse = "+"), .groups = "drop") %>%
    dplyr::mutate(
      starts_at = dplyr::case_when(
        grepl("pre_frac", phases)  ~ "Day 0",
        grepl("frac_stage", phases) ~ "After pre-frac",
        TRUE ~ "Post-frac"
      ),
      description = paste0(resource, " (", phases, ")")
    )
}

# Human-readable workflow summary for the Workflow tab in the app.
summarise_workflow <- function(cfg = WORKFLOW_CONFIG) {
  cfg %>%
    dplyr::mutate(
      Conditional = ifelse(conditional == "", "Always", conditional),
      `Path type` = path_type,
      Duration = duration_source
    ) %>%
    dplyr::select(
      Phase = phase, Activity = activity, Resource = resource,
      Duration, Conditional, `Path type`, Notes = notes
    ) %>%
    dplyr::arrange(
      factor(Phase, levels = c("pre_frac","frac_stage","post_frac")),
      Activity
    )
}

# ---------------------------------------------------------------------------
# Main simulation (drop-in replacement for simulate_campaign_detailed)
# ---------------------------------------------------------------------------

simulate_campaign_detailed <- function(
    historical_wells,
    assumptions,
    n_wells = 30,
    n_iterations = 1000,
    frac_fleets = 1,
    milling_units = 1,
    wireline_units = 1,
    ct_units = 1,
    frac_trees = 2,
    operation_mode = "Conventional",
    zipper_efficiency = 0.75,
    risk_multiplier = 1,
    wireline_time_per_stage_hours = 6,
    wireline_rig_up_down_hours = 12,
    wireline_contingency_pct = 10,
    frac_time_per_stage_hours = 12,      # full stage-to-stage CYCLE time: pump + fluid
                                          # loading + iron rig-up/down + pressure test + flush
    frac_settling_time_hours = 2,         # pressure bleed-off before wireline re-entry
    well_to_well_transition_hours = 4,    # within-pad transition: skid or short move to
                                          # adjacent wellhead on the same pad
    pad_to_pad_move_hours = 24,           # inter-pad move: full rig-down, transport, rig-up
                                          # at a different pad location (applies once at the
                                          # first well of each new pad)
    # Frac tree constraint (v13)
    frac_tree_swap_delay_hours = 4,      # transition delay per well when frac_trees == 2
    # CT support for milling (v13)
    allow_ct_for_milling = FALSE,        # CT spare capacity can assist milling
    ct_milling_efficiency = 0.65,        # 1 CT unit = 0.65 milling units when reassigned
    # Testing unit (v13)
    testing_units = 1,                   # dedicated testing units for flowback + milling
    flowback_testing_days_min = 7,       # post-frac flowback + testing window
    flowback_testing_days_max = 10,
    risk_library = NULL,                 # optional risk_consequence_library tibble (severity tiers + consequence magnitudes; see R/risk_library_engine.R)
    seed = NULL,
    progress_callback = NULL,  # optional function(i, n) for Shiny progress
    # --- perf (round 1): skip building per-iteration output frames the caller
    # does not need. Defaults preserve the original return value exactly.
    keep_logs = TRUE,            # build risk_event_log (FALSE for screening runs)
    collect_well_details = TRUE, # build per-well details (FALSE for screening runs)
    # Pre-frac scheduling mode:
    #   "event" (default) -- real resource-availability-vector scheduler for
    #             CT, wireline, and frac via schedule_pre_frac(). Correctly
    #             models cross-well contention and produces the attribution
    #             split (wireline_capacity_wait vs ct_caused_wait) that the
    #             formula path cannot. Validated by test_schedule_pre_frac.R
    #             (42 property checks) and check_scheduling_modes.R.
    #   "formula" -- original workload-accounting two-pass calc. Kept for
    #             check_regression.R (proves fast engine = archive engine).
    pre_frac_scheduling = c("event", "formula")
) {
  pre_frac_scheduling <- match.arg(pre_frac_scheduling)
  if (!is.null(seed)) set.seed(seed)

  n_wells <- as.integer(n_wells)
  n_iterations <- as.integer(n_iterations)
  frac_fleets <- as.numeric(frac_fleets)
  milling_units <- as.numeric(milling_units)
  wireline_units <- as.numeric(wireline_units)
  ct_units <- as.numeric(ct_units)
  frac_trees <- as.numeric(frac_trees)
  zipper_efficiency <- as.numeric(zipper_efficiency)
  risk_multiplier <- as.numeric(risk_multiplier)
  wireline_time_per_stage_hours <- as.numeric(wireline_time_per_stage_hours)
  wireline_rig_up_down_hours <- as.numeric(wireline_rig_up_down_hours)
  wireline_contingency_pct <- as.numeric(wireline_contingency_pct)
  frac_time_per_stage_hours <- as.numeric(frac_time_per_stage_hours)
  frac_settling_time_hours <- as.numeric(frac_settling_time_hours)
  well_to_well_transition_hours <- as.numeric(well_to_well_transition_hours)
  pad_to_pad_move_hours         <- as.numeric(pad_to_pad_move_hours)
  operation_mode <- as.character(operation_mode)
  frac_tree_swap_delay_hours <- as.numeric(frac_tree_swap_delay_hours)
  ct_milling_efficiency       <- as.numeric(ct_milling_efficiency)
  testing_units               <- as.numeric(testing_units)
  flowback_testing_days_min   <- as.numeric(flowback_testing_days_min)
  flowback_testing_days_max   <- as.numeric(flowback_testing_days_max)

  if (frac_tree_swap_delay_hours < 0) stop("frac_tree_swap_delay_hours cannot be negative.")
  if (ct_milling_efficiency <= 0 || ct_milling_efficiency > 1)
    stop("ct_milling_efficiency must be between 0 and 1.")
  if (testing_units < 0) stop("testing_units cannot be negative.")
  if (flowback_testing_days_min < 0 || flowback_testing_days_max < flowback_testing_days_min)
    stop("flowback_testing_days_min/max must be non-negative and min <= max.")

  if (n_wells <= 0) stop("n_wells must be positive.")
  if (n_iterations <= 0) stop("n_iterations must be positive.")
  if (frac_fleets <= 0) stop("frac_fleets must be positive.")
  if (milling_units <= 0) stop("milling_units must be positive.")
  if (wireline_units <= 0) stop("wireline_units must be positive.")
  if (ct_units <= 0) stop("ct_units must be positive.")
  if (frac_trees <= 0) stop("frac_trees must be positive.")
  if (wireline_time_per_stage_hours < 0) stop("wireline_time_per_stage_hours cannot be negative.")
  if (wireline_rig_up_down_hours < 0) stop("wireline_rig_up_down_hours cannot be negative.")
  if (wireline_contingency_pct < 0) stop("wireline_contingency_pct cannot be negative.")
  if (frac_time_per_stage_hours <= 0) stop("frac_time_per_stage_hours must be positive.")
  if (frac_settling_time_hours < 0) stop("frac_settling_time_hours cannot be negative.")
  if (well_to_well_transition_hours < 0) stop("well_to_well_transition_hours cannot be negative.")
  if (pad_to_pad_move_hours < 0) stop("pad_to_pad_move_hours cannot be negative.")

  wireline_time_per_stage_days <- wireline_time_per_stage_hours / 24
  wireline_rig_up_down_days <- wireline_rig_up_down_hours / 24
  wireline_contingency_factor <- 1 + wireline_contingency_pct / 100
  frac_time_per_stage_days <- frac_time_per_stage_hours / 24
  frac_settling_time_days  <- frac_settling_time_hours / 24
  well_to_well_transition_days <- well_to_well_transition_hours / 24
  pad_to_pad_move_days         <- pad_to_pad_move_hours / 24

  hist_milling <- historical_wells$milling_days_per_plug
  hist_milling <- hist_milling[!is.na(hist_milling) & hist_milling > 0]
  hist_frac <- historical_wells$frac_days_per_stage
  hist_frac <- hist_frac[!is.na(hist_frac) & hist_frac > 0]

  if (length(hist_frac) == 0) stop("No positive FracDaysPerStage values available in historical_wells.csv.")
  if (length(hist_milling) == 0) stop("No positive MillingDaysPerPlug values available in historical_wells.csv.")

  mode_key <- normalise_text(operation_mode)
  is_zipper <- mode_key == "zipper"
  if (is_zipper && frac_trees < 2) stop("Zipper frac requires at least 2 frac trees.")

  mode_factor <- if (is_zipper) zipper_efficiency else 1
  mode_factor <- max(0.4, min(1.2, mode_factor))

  # Frac tree efficiency: more trees reduce inter-well transition waiting.
  # Applied to transition time only, not pump time. Diminishing returns above 3.
  # 1 tree  -> conventional only (already blocked above for zipper)
  # 2 trees -> baseline zipper (factor = 1.00, no swap bonus)
  # 3 trees -> one spare reduces swap delay (~5% benefit)
  # 4+      -> further reduction (~10% benefit, diminishing)
  frac_tree_swap_delay_days <- frac_tree_swap_delay_hours / 24
  # The zipper + frac_trees < 2 guard already runs above; this is just the
  # efficiency lookup. ifelse() rather than case_when() since these are
  # scalar inputs, not vectors.
  tree_efficiency_factor <- if (!is_zipper) {
    1.0
  } else if (frac_trees == 2) {
    1.00
  } else if (frac_trees == 3) {
    0.95
  } else {   # 4+
    0.90
  }

  # --- One-time precomputation (was inside / per-call in v11) ---------------
  param_cache <- build_param_cache(assumptions)

  # --- Risk table with scope-aware probability adjustment -------------------
  # scope column in assumptions CSV controls how probability is interpreted:
  #   "well"     (default): probability applies independently per well.
  #              Expected occurrences per campaign = p * n_wells.
  #   "stage"  : probability applies per stage. Effective per-well probability
  #              = 1 - (1 - p)^stages_per_well, applied independently per well.
  #              Correct for technical risks like screenout, gun misfire.
  #   "campaign": probability of occurring once during the whole campaign,
  #              regardless of well count. Correct for resource/external risks
  #              (crew unavailable, weather, permit delay) that affect the
  #              whole operation, not each well independently.
  #              Modelled as a single Bernoulli draw; delay applied to one
  #              representative well (campaign-level delay pool).

  base_stages <- median(
    sample_integer_param_cached(param_cache, "Stages per well", 10),
    na.rm = TRUE
  )

  rt <- build_risk_table(assumptions, base_stages, risk_multiplier, risk_library)
  risk_table <- rt$table
  lib_wide <- rt$lib_wide

  risk_grid <- build_risk_grid(risk_table, n_wells)

  cement_eval_offline_prob <- get_param_prob_cached(param_cache, "Cement eval offline", default = 0.8)
  cement_eval_offline_prob <- max(0, min(1, cement_eval_offline_prob))

  well_ids <- paste0("SimWell_", str_pad(seq_len(n_wells), width = 3, pad = "0"))

  summary_list <- vector("list", n_iterations)
  well_list <- vector("list", n_iterations)
  risk_log_list <- vector("list", n_iterations)
  resource_list <- vector("list", n_iterations)

  resource_names <- c("Frac fleet", "Wireline", "CT / cleanout", "Milling", "Testing unit")
  resource_units <- c(frac_fleets, wireline_units, ct_units, milling_units, testing_units)

  for (iter_id in seq_len(n_iterations)) {
    if (!is.null(progress_callback) && iter_id %% 50 == 0) {
      progress_callback(iter_id, n_iterations)
    }

    pad_assignment <- build_pad_assignment_cached(n_wells, param_cache)
    stage_count <- sample_integer_param_cached(param_cache, "Stages per well", n_wells)
    temp_log_count <- sample_integer_param_cached(param_cache, "Temperature log stages", n_wells)

    milling_days_per_plug <- sample(hist_milling, n_wells, replace = TRUE)
    cement_eval_days <- sample_param_cached(param_cache, "Cement eval duration", n_wells)
    cleanout_days <- sample_param_cached(param_cache, "Scraper / cleanout run", n_wells)
    temp_log_duration <- sample_param_cached(param_cache, "Temperature log duration", n_wells)
    isolation_plug_days <- sample_param_cached(param_cache, "Isolation plug duration", n_wells)

    well_df <- tibble(
      simulation_id = iter_id,
      operation_mode = operation_mode,
      well_index = pad_assignment$well_index,
      well_id = well_ids,
      pad_id = pad_assignment$pad_id,
      stages = stage_count,
      temp_log_stages = temp_log_count,
      cement_eval_offline = rbinom(n_wells, 1, cement_eval_offline_prob) == 1,
      frac_days_per_stage = frac_time_per_stage_days,
      milling_days_per_plug = milling_days_per_plug,
      cement_eval_days = cement_eval_days,
      cleanout_days = cleanout_days,
      temp_log_days = temp_log_count * temp_log_duration,
      isolation_plug_days = isolation_plug_days,
      wireline_time_per_stage_days = wireline_time_per_stage_days,
      wireline_rig_up_down_days = wireline_rig_up_down_days,
      wireline_contingency_pct = wireline_contingency_pct,
      frac_settling_days = stage_count * frac_settling_time_days,
      base_frac_days = stage_count * frac_time_per_stage_days
    )

    if (!is.null(risk_grid)) {
      drawn <- draw_risks_on_grid(risk_grid, well_df, iter_id, operation_mode,
                                  wireline_run_days = wireline_time_per_stage_days,
                                  collect_log = keep_logs, lib_wide = lib_wide)
      risk_log <- drawn$risk_log
      s <- drawn$sums
    } else {
      risk_log <- empty_risk_event_log()
      s <- empty_sums_matrix(n_wells)
    }

    # Per-well risk effects come straight from the summary matrix: no joins.
    well_df <- well_df %>%
      mutate(
        frac_risk_delay_days = s[, "frac"],
        wireline_risk_delay_days = s[, "wireline"],
        ct_risk_delay_days = s[, "ct"],
        milling_risk_delay_days = s[, "milling"],
        # Logistics consequence days have no dedicated fleet, so they're
        # folded into the external bucket (same treatment as weather/permit
        # delays) while remaining separately reportable via logistics_consequence_days.
        external_risk_delay_days = s[, "external"] + s[, "logistics_x"],
        logistics_consequence_days = s[, "logistics_x"],
        risk_delay_days = s[, "total"],
        extra_plugs = s[, "plugs"],
        extra_stages = s[, "stages"],
        # v15 consequence propagation: induced workload from technical risks
        wireline_rework_days = s[, "wl_run_days"],
        extra_wireline_runs = s[, "wl_runs"],
        ct_consequence_days = s[, "ct_x"],
        extra_milling_plugs = s[, "mill_x_plugs"],
        risk_testing_days = s[, "test_x"],
        frac_consequence_days = s[, "frac_x"],
        final_stages = stages + extra_stages,
        # Plugs to mill = isolation plugs per final stage + replacement plugs
        # (adds_plug) + consequence plugs (e.g. isolation plug failure).
        plugs = final_stages + extra_plugs + extra_milling_plugs,
        online_cement_eval_days = ifelse(cement_eval_offline, 0, cement_eval_days),
        milling_days = plugs * milling_days_per_plug,
        wireline_base_stage_days = final_stages * wireline_time_per_stage_days,
        wireline_contingency_days = (wireline_base_stage_days + wireline_rig_up_down_days) * (wireline_contingency_factor - 1),
        wireline_stage_readiness_days =
          wireline_base_stage_days + wireline_rig_up_down_days + temp_log_days +
          wireline_contingency_days + wireline_risk_delay_days +
          wireline_rework_days,
        # Frac execution: mode_factor handles zipper pumping benefit.
        # frac_tree_constraint_delay: when frac_trees == 2 each inter-well swap
        # costs frac_tree_swap_delay_days (zero if conventional or trees >= 3).
        frac_tree_constraint_delay_days = if (is_zipper) {
          frac_tree_swap_delay_days * final_stages * (1 - tree_efficiency_factor)
        } else 0,
        # Pad-aware transition cost (v17.4):
        #   Within-pad:  wells share a pad; transition = short skid/move to adjacent wellhead
        #   Pad-to-pad:  first well on a new pad = full rig-down + transport + rig-up
        #
        # is_first_on_pad = TRUE for the first well of each pad (detected via pad_id change).
        # In zipper the swap delay already covers much of the inter-well cost, so within-pad
        # transition is halved; pad-to-pad is not halved (it is a mobilisation event).
        is_first_on_pad = c(TRUE, pad_id[-1] != pad_id[-length(pad_id)]),
        well_transition_days = dplyr::case_when(
          is_first_on_pad & !is_zipper ~ pad_to_pad_move_days,
          is_first_on_pad &  is_zipper ~ pad_to_pad_move_days,   # full move regardless of mode
          !is_first_on_pad & !is_zipper ~ well_to_well_transition_days,
          !is_first_on_pad &  is_zipper ~ well_to_well_transition_days * 0.5,
          TRUE ~ 0
        ),
        frac_execution_days = (
          final_stages * frac_days_per_stage +
          frac_settling_days + isolation_plug_days +
          frac_risk_delay_days +
          frac_consequence_days +
          frac_tree_constraint_delay_days +
          well_transition_days
        ) * mode_factor,
        # CT workload = primary duties + risk-induced interventions
        # (screenout cleanout, cement drillout, premature plug response).
        ct_workload_days = online_cement_eval_days + cleanout_days + ct_risk_delay_days +
          ct_consequence_days,
        frac_workload_days = frac_execution_days + external_risk_delay_days,
        # Milling workload after optional CT support.
        # CT spare capacity = campaign_days * ct_units - primary ct workload.
        # (campaign_days not yet known per-well, so use frac-related estimate.)
        milling_days_gross = milling_days + milling_risk_delay_days
      ) %>%
      mutate(
        frac_fleet_days_est = frac_workload_days / frac_fleets,
        wireline_fleet_days = wireline_stage_readiness_days / wireline_units,
        ct_fleet_days = ct_workload_days / ct_units
      ) %>%
      mutate(
        wireline_readiness_delay_days = if (is_zipper) pmax(wireline_fleet_days - frac_fleet_days_est, 0) else 0
      )

    # =========================================================================
    # Two-pass campaign duration calculation
    #
    # Pass 1: frac-path campaign duration (CT primary workload only, no milling).
    #   This gives the true calendar span available to CT for spare-capacity work.
    #   Milling runs in parallel on its own resource, so it does not extend the
    #   frac path unless it becomes the critical path.
    #
    # Pass 2: CT spare capacity resolved against Pass-1 duration; adjust milling
    #   workload; recompute final campaign duration as max(frac_path, milling,
    #   testing).
    # =========================================================================

    total_ct_primary_days <- sum(well_df$ct_workload_days, na.rm = TRUE)
    total_milling_gross   <- sum(well_df$milling_days_gross, na.rm = TRUE)

    # --- Pass 1: frac-path duration -------------------------------------------
    #
    # CT workload parallelism (v17.3):
    # ---------------------------------
    # CT cleanout runs before each well's wireline/frac, but for multi-well
    # conventional campaigns the CT unit can prep well N+1 DURING well N's
    # frac execution (CT is idle during pumping). This means CT is on the
    # critical path only when ct_fleet_days > frac_fleet_days (i.e. CT is
    # slower than the frac cycle and becomes the pacing resource).
    #
    # Conventional: frac_related = max(ct_fleet_days, frac+wireline)
    #   CT and frac/wireline run in parallel; whichever takes longer governs.
    #   If ct_fleet_days < (frac+wireline), CT completes within the frac window
    #   and does NOT add to the sequential well-to-well time.
    #
    # Zipper: CT still precedes each well's wireline start. With 2 wells active
    #   simultaneously, CT must keep pace with both wells. The existing zipper
    #   formula (ct + max(frac, wireline)) is retained as CT is more likely to
    #   be on the critical path in zipper where frac pace is faster.
    #
    # Cement eval offline rule (v17.3):
    # ---------------------------
    # If wireline_units >= 2, a spare wireline unit is available during perforation,
    # so cement eval can always run offline. Override cement_eval_offline for those wells.
    well_df <- well_df %>%
      mutate(
        # With 2+ wireline units: cement eval always runs offline (spare unit available)
        cement_eval_offline = cement_eval_offline | (wireline_units >= 2),
        online_cement_eval_days = ifelse(cement_eval_offline, 0, cement_eval_days),
        ct_workload_days = online_cement_eval_days + cleanout_days + ct_risk_delay_days +
          ct_consequence_days,
        ct_fleet_days = ct_workload_days / ct_units
      ) %>%
      mutate(
        frac_fleet_days = frac_workload_days / frac_fleets,
        frac_related_days = if (is_zipper) {
          # Zipper: CT precedes each well; retained as-is since frac pace is
          # faster and CT is more likely to constrain.
          ct_fleet_days + pmax(frac_fleet_days, wireline_fleet_days)
        } else {
          # Conventional: CT runs in parallel with frac execution on the
          # previous well. Only adds to the path if CT is the pacing resource.
          pmax(ct_fleet_days, frac_fleet_days + wireline_fleet_days)
        }
      )

    # --- Event-mode override (pre_frac_scheduling == "event") -----------------
    # Replaces the formula-based ct_fleet_days/frac_fleet_days/frac_related_days
    # above with values read off a real resource-availability-vector schedule
    # (schedule_pre_frac(), same pattern as schedule_post_frac_milling()).
    # well_df's row order here already matches campaign order -- pad_id/well_id
    # are assigned in well_index order by build_pad_assignment_cached(), so the
    # arrange(pad_id, well_id) below is a defensive no-op reorder, not a real
    # one -- safe to schedule against the current row order directly.
    event_frac_finish_day <- NULL
    if (pre_frac_scheduling == "event") {
      pf <- schedule_pre_frac(
        well_order_index = seq_len(nrow(well_df)),
        ct_workload_days = well_df$ct_workload_days,
        wireline_workload_days = well_df$wireline_stage_readiness_days,
        frac_workload_days = well_df$frac_workload_days,
        ct_units = ct_units,
        wireline_units = wireline_units,
        frac_fleets = frac_fleets
      )
      event_frac_finish_day <- pf$well_schedule$frac_finish_day
      well_df <- well_df %>%
        mutate(
          # Each well's actual busy-time on the resource that handled it,
          # replacing the workload/units division estimate.
          frac_fleet_days = well_df$frac_workload_days,
          wireline_fleet_days = well_df$wireline_stage_readiness_days,
          ct_fleet_days = well_df$ct_workload_days,
          # Local per-well wait, read off the schedule -- not the formula's
          # zipper-only pmax() estimate. See schedule_pre_frac()'s
          # wireline_wait_days for why this must not be frac_finish minus
          # wireline_finish (those are unrelated cumulative queue positions).
          wireline_readiness_delay_days = pf$well_schedule$wireline_wait_days,
          # Attribution split (see schedule_pre_frac()): how much of the wait
          # above is genuinely wireline capacity vs CT gating wireline's
          # start. Without this, a slow CT unit shows up entirely as
          # "waiting on wireline" even when wireline itself has ample units.
          wireline_capacity_wait_days = pf$well_schedule$wireline_capacity_wait_days,
          ct_caused_wireline_wait_days = pf$well_schedule$ct_caused_wait_days,
          # Second-level split of ct_caused_wireline_wait_days: how much is
          # removable by adding CT units (queueing) vs a floor that survives
          # even with a dedicated CT unit per well (see schedule_pre_frac()'s
          # ct_duration_floor_wait_days comment). Without this, "add CT
          # capacity" advice would overstate what more units can fix.
          ct_queueing_wireline_wait_days = pf$well_schedule$ct_queueing_wait_days,
          ct_duration_floor_wireline_wait_days = pf$well_schedule$ct_duration_floor_wait_days
        )
      total_frac_related_days <- max(event_frac_finish_day)
    } else {
      total_frac_related_days <- sum(well_df$frac_related_days, na.rm = TRUE)
      # The formula path never modeled this attribution (it hardcodes
      # wireline_readiness_delay_days to a single zipper-only estimate with
      # no CT/wireline split) -- NA, not 0, so it can't be misread as "CT
      # never causes wireline wait under the formula model" when really
      # the formula just doesn't compute this distinction at all.
      well_df$wireline_capacity_wait_days <- NA_real_
      well_df$ct_caused_wireline_wait_days <- NA_real_
      well_df$ct_queueing_wireline_wait_days <- NA_real_
      well_df$ct_duration_floor_wireline_wait_days <- NA_real_
    }

    # Pass-1 campaign duration = frac critical path (milling excluded here).
    # This is the actual calendar window CT is deployed over.
    frac_path_campaign_days <- total_frac_related_days

    # --- Pass 2: post-frac milling + testing dependency scheduler -------------
    # Total CT capacity available over the frac-path campaign span.
    # This is only a budget for potential CT assistance. CT still cannot support
    # milling until a well has been fracced and released to the milling queue.
    total_ct_capacity_days <- frac_path_campaign_days * ct_units
    ct_available_capacity_days <- pmax(total_ct_capacity_days - total_ct_primary_days, 0)

    # Post-frac flowback + testing window per well, plus risk-induced testing
    # interventions (premature plug set, plug pressure test failure, isolation
    # plug failure) propagated via the consequence library.
    flowback_days_per_well <- runif(n_wells,
                                   min = flowback_testing_days_min,
                                   max = flowback_testing_days_max)
    total_risk_testing_days <- sum(well_df$risk_testing_days, na.rm = TRUE)
    flowback_testing_workload_per_well <- flowback_days_per_well + well_df$risk_testing_days

    # Build well release dates. Milling is downstream of frac completion. The
    # current model estimates a frac-path workload rather than a detailed stage
    # schedule, so each well is released in campaign order at the cumulative
    # frac-path completion time for that well.
    well_df <- well_df %>%
      arrange(pad_id, well_id) %>%
      mutate(
        frac_release_day = if (pre_frac_scheduling == "event") event_frac_finish_day else cumsum(frac_related_days),
        flowback_testing_days = flowback_testing_workload_per_well
      )

    post_frac_schedule <- schedule_post_frac_milling(
      release_times = well_df$frac_release_day,
      milling_workload_days = well_df$milling_days_gross,
      flowback_testing_days = well_df$flowback_testing_days,
      milling_units = milling_units,
      testing_units = testing_units,
      ct_units = ct_units,
      allow_ct_for_milling = allow_ct_for_milling,
      ct_milling_efficiency = ct_milling_efficiency,
      ct_support_budget_days = ct_available_capacity_days
    )

    # Preserve legacy column names where possible, but with corrected semantics:
    # - ct_milling_support_days is milling-equivalent work done by CT after frac release.
    # - adjusted_milling_workload_days is work handled by dedicated milling units.
    # - total_milling_fleet_days is dedicated milling busy time after resources.
    # - total_testing_fleet_days includes both flowback/testing and testing-unit
    #   occupation during milling.
    ct_milling_support_days <- post_frac_schedule$ct_milling_support_equiv_days
    ct_milling_support_ct_days <- post_frac_schedule$ct_milling_support_ct_days
    adjusted_milling_workload_days <- post_frac_schedule$dedicated_milling_workload_days
    total_milling_fleet_days <- adjusted_milling_workload_days / pmax(milling_units, 1e-9)
    total_testing_workload_days <- post_frac_schedule$testing_total_workload_days
    testing_occupied_for_milling_days <- post_frac_schedule$testing_occupied_for_milling_days
    testing_flowback_workload_days <- post_frac_schedule$testing_flowback_workload_days
    total_testing_fleet_days <- total_testing_workload_days / pmax(testing_units, 1e-9)
    post_frac_completion_days <- post_frac_schedule$post_frac_completion_day
    milling_completion_day <- post_frac_schedule$milling_completion_day
    testing_completion_day <- post_frac_schedule$testing_completion_day

    milling_schedule_for_join <- post_frac_schedule$well_schedule %>%
      select(
        well_index,
        milling_start_day,
        milling_finish_day,
        milling_resource,
        milling_resource_id,
        testing_unit_for_milling,
        ct_milling_days_used,
        dedicated_milling_days_used,
        flowback_start_day,
        flowback_finish_day,
        testing_unit_for_flowback
      )

    well_df <- well_df %>%
      left_join(milling_schedule_for_join, by = "well_index")

    # --- Final campaign duration ---------------------------------------------
    # Campaign completion is no longer max(frac path, aggregate milling fleet-days,
    # aggregate testing fleet-days). Milling and flowback/testing are downstream of
    # frac release, so the post-frac scheduler produces the actual completion day.
    estimated_campaign_days <- max(total_frac_related_days, post_frac_completion_days, na.rm = TRUE)

    summary_list[[iter_id]] <- list(
      simulation_id = iter_id,
      operation_mode = operation_mode,
      wells = n_wells,
      frac_fleets = frac_fleets,
      milling_units = milling_units,
      wireline_units = wireline_units,
      ct_units = ct_units,
      frac_trees = frac_trees,
      zipper_efficiency = ifelse(is_zipper, mode_factor, NA_real_),
      total_pads = n_distinct(well_df$pad_id),
      total_stages = sum(well_df$final_stages, na.rm = TRUE),
      total_plugs = sum(well_df$plugs, na.rm = TRUE),
      total_extra_plugs = sum(well_df$extra_plugs, na.rm = TRUE),
      total_extra_stages = sum(well_df$extra_stages, na.rm = TRUE),
      total_risk_delay_days = sum(well_df$risk_delay_days, na.rm = TRUE),
      total_frac_workload_days = sum(well_df$frac_workload_days, na.rm = TRUE),
      total_wireline_workload_days = sum(well_df$wireline_stage_readiness_days, na.rm = TRUE),
      total_wireline_base_stage_days = sum(well_df$wireline_base_stage_days, na.rm = TRUE),
      total_wireline_rig_up_down_days = sum(well_df$wireline_rig_up_down_days, na.rm = TRUE),
      total_wireline_contingency_days = sum(well_df$wireline_contingency_days, na.rm = TRUE),
      total_wireline_risk_delay_days = sum(well_df$wireline_risk_delay_days, na.rm = TRUE),
      total_frac_settling_days = sum(well_df$frac_settling_days, na.rm = TRUE),
      total_temperature_logging_days = sum(well_df$temp_log_days, na.rm = TRUE),
      total_wireline_readiness_delay_days = sum(well_df$wireline_readiness_delay_days, na.rm = TRUE),
      # NA (not 0) under the formula model -- it doesn't compute this
      # attribution at all, see the wireline_capacity_wait_days comment above.
      total_wireline_capacity_wait_days = if (pre_frac_scheduling == "event")
        sum(well_df$wireline_capacity_wait_days, na.rm = TRUE) else NA_real_,
      total_ct_caused_wireline_wait_days = if (pre_frac_scheduling == "event")
        sum(well_df$ct_caused_wireline_wait_days, na.rm = TRUE) else NA_real_,
      total_ct_queueing_wireline_wait_days = if (pre_frac_scheduling == "event")
        sum(well_df$ct_queueing_wireline_wait_days, na.rm = TRUE) else NA_real_,
      total_ct_duration_floor_wireline_wait_days = if (pre_frac_scheduling == "event")
        sum(well_df$ct_duration_floor_wireline_wait_days, na.rm = TRUE) else NA_real_,
      total_ct_primary_workload_days = total_ct_primary_days,
      total_ct_workload_days = total_ct_primary_days + ct_milling_support_ct_days,
      total_milling_workload_days = total_milling_gross,
      total_frac_fleet_days = sum(well_df$frac_fleet_days, na.rm = TRUE),
      total_wireline_fleet_days = sum(well_df$wireline_fleet_days, na.rm = TRUE),
      total_ct_fleet_days = sum(well_df$ct_fleet_days, na.rm = TRUE) + ct_milling_support_ct_days / pmax(ct_units, 1e-9),
      total_milling_workload_gross = total_milling_gross,
      total_induced_wireline_days = sum(well_df$wireline_rework_days, na.rm = TRUE),
      total_extra_wireline_runs = sum(well_df$extra_wireline_runs, na.rm = TRUE),
      total_induced_ct_days = sum(well_df$ct_consequence_days, na.rm = TRUE),
      total_extra_milling_plugs = sum(well_df$extra_milling_plugs, na.rm = TRUE),
      total_induced_testing_days = total_risk_testing_days,
      total_induced_frac_days = sum(well_df$frac_consequence_days, na.rm = TRUE),
      ct_milling_support_days = ct_milling_support_days,
      ct_milling_support_ct_days = ct_milling_support_ct_days,
      ct_available_capacity_days = ct_available_capacity_days,
      adjusted_milling_workload_days = adjusted_milling_workload_days,
      total_milling_fleet_days = total_milling_fleet_days,
      testing_occupied_for_milling_days = testing_occupied_for_milling_days,
      testing_flowback_workload_days = testing_flowback_workload_days,
      total_testing_workload_days = total_testing_workload_days,
      total_testing_fleet_days = total_testing_fleet_days,
      post_frac_completion_days = post_frac_completion_days,
      milling_completion_day = milling_completion_day,
      testing_completion_day = testing_completion_day,
      frac_tree_efficiency_factor = tree_efficiency_factor,
      estimated_campaign_days = estimated_campaign_days,
      total_induced_logistics_days = sum(well_df$logistics_consequence_days, na.rm = TRUE)
    )

    resource_workload <- c(
      sum(well_df$frac_workload_days, na.rm = TRUE),
      sum(well_df$wireline_stage_readiness_days, na.rm = TRUE),
      total_ct_primary_days + ct_milling_support_ct_days,
      adjusted_milling_workload_days,
      total_testing_workload_days
    )
    resource_fleet_days <- c(
      sum(well_df$frac_fleet_days, na.rm = TRUE),
      sum(well_df$wireline_fleet_days, na.rm = TRUE),
      sum(well_df$ct_fleet_days, na.rm = TRUE) + ct_milling_support_ct_days / pmax(ct_units, 1e-9),
      total_milling_fleet_days,
      total_testing_fleet_days
    )

    resource_list[[iter_id]] <- data.frame(
      simulation_id = iter_id,
      operation_mode = operation_mode,
      resource = resource_names,
      units = resource_units,
      workload_days = resource_workload,
      fleet_days_after_resources = resource_fleet_days,
      utilization = resource_workload / pmax(estimated_campaign_days * resource_units, 1e-9),
      stringsAsFactors = FALSE, check.names = FALSE
    )

    if (collect_well_details) {
      well_list[[iter_id]] <- well_df %>%
        select(
          simulation_id, operation_mode, pad_id, well_id, stages, extra_stages, final_stages,
          temp_log_stages, cement_eval_offline, plugs, extra_plugs,
          frac_days_per_stage, frac_settling_days, milling_days_per_plug, cement_eval_days, cleanout_days,
          base_frac_days, frac_execution_days, wireline_time_per_stage_days, wireline_rig_up_down_days,
          wireline_contingency_pct, wireline_base_stage_days, wireline_contingency_days,
          temp_log_days, wireline_stage_readiness_days,
          wireline_fleet_days, wireline_readiness_delay_days,
          wireline_capacity_wait_days, ct_caused_wireline_wait_days,
          ct_queueing_wireline_wait_days, ct_duration_floor_wireline_wait_days, ct_workload_days,
          milling_days_gross, risk_delay_days, frac_related_days, frac_release_day,
          milling_start_day, milling_finish_day, milling_resource, flowback_start_day, flowback_finish_day,
          frac_tree_constraint_delay_days, is_first_on_pad, well_transition_days, wireline_rework_days, extra_wireline_runs,
          ct_consequence_days, extra_milling_plugs, risk_testing_days, frac_consequence_days
        )
    }

    if (keep_logs) risk_log_list[[iter_id]] <- risk_log
  }

  # Columnar assembly: summary rows were collected as plain named lists to
  # avoid per-iteration tibble() construction (the dominant runtime cost). Every
  # field is a length-1 scalar, so unlist() rebuilds each column with its native
  # type, producing output identical to the original per-row tibble()+bind_rows().
  .scols <- names(summary_list[[1]])
  summary <- tibble::as_tibble(stats::setNames(
    lapply(.scols, function(cn) unlist(lapply(summary_list, `[[`, cn), use.names = FALSE)),
    .scols
  ))
  well_details <- bind_rows(well_list)
  risk_event_log <- bind_rows(risk_log_list)
  if (ncol(risk_event_log) == 0 || nrow(risk_event_log) == 0) risk_event_log <- empty_risk_event_log()
  resource_utilization <- bind_rows(resource_list)

  # Mirror the scope-aware adjustment used to build risk_table above (via the
  # same resolve_risk_scope()/compute_adjusted_risk_probability() helpers from
  # risk_library_engine.R), so this audit column matches the probability
  # actually used to draw risk occurrences (compute_adjusted_risk_probability()
  # compounds stage-scope risk AFTER the multiplier, not before -- using the
  # old plain probability * risk_multiplier formula here would silently
  # understate the displayed/exported value for every stage-scope risk).
  assumptions_used <- assumptions %>%
    mutate(
      risk_multiplier_used = risk_multiplier,
      .scope_used = resolve_risk_scope(.),
      probability_used = ifelse(normalise_text(type) == "risk",
                                compute_adjusted_risk_probability(probability, .scope_used, risk_multiplier, base_stages),
                                as.numeric(probability))
    ) %>%
    select(-.scope_used)

  list(
    summary = summary,
    well_details = well_details,
    risk_event_log = risk_event_log,
    resource_utilization = resource_utilization,
    assumptions_used = assumptions_used
  )
}

# Compatibility wrapper for earlier app versions.
simulate_one_campaign <- function(...) {
  simulate_campaign_detailed(...)$summary
}

