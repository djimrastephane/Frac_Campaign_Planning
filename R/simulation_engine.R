# simulation_engine.R
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

  for (cc in cons_cols) {
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
      risk_table[[cc]] <- ifelse(is.na(csv_val), defaults[, cc], csv_val)
    } else {
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
    simulation_impact = character()
  )
}

# Draw one iteration of risk outcomes on the precomputed grid.
# Returns: risk_log (occurred events only) + per-well numeric summary matrix.
# v15: consequence propagation - induced wireline/CT/milling/testing/frac
# workload is accumulated alongside the direct delay.
SUM_COLS <- c("frac", "wireline", "ct", "milling", "external", "total",
              "plugs", "stages", "wl_runs", "wl_run_days", "ct_x",
              "mill_x_plugs", "test_x", "frac_x")

empty_sums_matrix <- function(n_wells) {
  matrix(0, nrow = n_wells, ncol = length(SUM_COLS),
         dimnames = list(NULL, SUM_COLS))
}

draw_risks_on_grid <- function(grid, well_df, iter_id, operation_mode,
                               wireline_run_days = 0.25) {
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
  extra_stages <- as.numeric(grid$adds_stage[occurs])

  # Consequence quantities for the occurred events
  c_wl_runs <- as.numeric(grid$extra_wireline_runs[occurs])
  c_wl_days <- c_wl_runs * wireline_run_days
  c_ct_days <- as.numeric(grid$extra_ct_days[occurs])
  c_mill_plugs <- as.numeric(grid$extra_milling_plugs[occurs])
  c_mill_days <- c_mill_plugs * well_df$milling_days_per_plug[w]
  c_test_days <- as.numeric(grid$extra_testing_days[occurs])
  c_frac_days <- as.numeric(grid$extra_frac_days[occurs])

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
    simulation_impact = grid$simulation_impact[occurs]
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
    scmt_days           = round(runif(n, 0.5, 1.5), 2),
    milling_days        = mill_days,
    frac_days_per_stage = round(frac_dps, 3),
    milling_days_per_plug = round(mill_dpp, 3)
  )
}

build_zipper_benefit_breakdown <- function(summary, zipper_efficiency = 0.75) {
  if (is.null(summary) || nrow(summary) == 0) return(NULL)
  conv <- summary %>% dplyr::filter(operation_mode == "Conventional")
  zip  <- summary %>% dplyr::filter(operation_mode == "Zipper")
  if (nrow(conv) == 0 || nrow(zip) == 0) return(NULL)

  n_wells <- median(conv$wells, na.rm = TRUE)
  ct_pw   <- mean(conv$total_ct_fleet_days,       na.rm = TRUE) / n_wells
  fr_c_pw <- mean(conv$total_frac_fleet_days,     na.rm = TRUE) / n_wells
  wl_c_pw <- mean(conv$total_wireline_fleet_days, na.rm = TRUE) / n_wells
  fr_z_pw <- mean(zip$total_frac_fleet_days,      na.rm = TRUE) / n_wells
  wl_z_pw <- mean(zip$total_wireline_fleet_days,  na.rm = TRUE) / n_wells

  conv_path_pw <- pmax(ct_pw, fr_c_pw + wl_c_pw)
  step_a_pw    <- pmax(ct_pw, fr_z_pw + wl_c_pw)
  step_b_pw    <- pmax(ct_pw, pmax(fr_z_pw, wl_z_pw))
  zip_path_pw  <- ct_pw + pmax(fr_z_pw, wl_z_pw)

  saving_efficiency <- (conv_path_pw - step_a_pw) * n_wells
  saving_wl_overlap <- (step_a_pw    - step_b_pw) * n_wells
  cost_ct_path      <- (step_b_pw    - zip_path_pw) * n_wells
  total_saving      <- median(conv$estimated_campaign_days, na.rm = TRUE) -
                       median(zip$estimated_campaign_days,  na.rm = TRUE)
  frac_path_saving  <- (conv_path_pw - zip_path_pw) * n_wells
  post_frac_diff    <- frac_path_saving - total_saving

  tibble::tibble(
    component   = c("Frac fleet efficiency (zipper ×0.75)",
                    "Wireline–frac overlap (parallel in zipper)",
                    "CT path offset (CT adds in zipper)",
                    "Post-frac & other differences",
                    "Total saving"),
    saving_days = round(c(saving_efficiency, saving_wl_overlap,
                           cost_ct_path, -post_frac_diff, total_saving), 0),
    explanation = c(
      sprintf("Frac stages pump %.0f%% faster in zipper mode", round((1-zipper_efficiency)*100)),
      "Wireline preps next well during pumping; in conventional it adds sequentially to the path",
      "CT must precede each well in zipper (partial offset to the gains above)",
      "Wireline idle cost, tree swap delays, post-frac queue differences",
      sprintf("Net P50 saving: Conv → Zip = %.0f d", total_saving)
    )
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

  rows <- vector("list", n)
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

    rows[[i]] <- tibble(
      well_index = i,
      frac_release_day = rel,
      milling_workload_days = mill_work,
      milling_start_day = mill_start,
      milling_finish_day = mill_finish,
      milling_resource = mill_resource,
      milling_resource_id = mill_resource_id,
      testing_unit_for_milling = test_for_mill_id,
      ct_milling_days_used = ct_days_used,
      dedicated_milling_days_used = dedicated_days_used,
      flowback_testing_days = flow_work,
      flowback_start_day = flow_start,
      flowback_finish_day = flow_finish,
      testing_unit_for_flowback = flow_test_id
    )
  }

  sched <- bind_rows(rows)
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
#                   : "!scmt_offline" = only if SCMT is online
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
#   SCMT OFFLINE RULE:
#     If wireline_units >= 2: SCMT always runs offline (spare unit available).
#     If wireline_units == 1: SCMT offline probability from assumptions CSV.
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
  "SCMT cement eval",          "pre_frac",   "CT / cleanout",  "param:SCMT duration",                       "!scmt_offline",      "parallel",    "Cement evaluation log via CT. Skipped if SCMT runs offline. AUTO-OFFLINE when wireline_units >= 2 (spare unit available). Otherwise: CSV probability row.",
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
    seed = NULL,
    progress_callback = NULL  # optional function(i, n) for Shiny progress
) {
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

  risk_table <- assumptions %>%
    filter(normalise_text(type) == "risk") %>%
    mutate(
      .scope = if ("scope" %in% names(.)) normalise_text(scope) else "well",
      .scope = ifelse(is.na(.scope) | .scope == "", "well", .scope),
      # Stage-scope: convert per-stage probability to effective per-well probability
      .eff_prob_well = case_when(
        .scope == "stage" ~ 1 - (1 - pmin(as.numeric(probability), 1))^base_stages,
        .scope == "campaign" ~ as.numeric(probability),  # handled separately in draw
        TRUE ~ as.numeric(probability)  # "well" scope
      ),
      adjusted_probability = pmin(.eff_prob_well * risk_multiplier, 1),
      adjusted_probability = ifelse(is.na(adjusted_probability), 0, adjusted_probability),
      is_campaign_scope = .scope == "campaign",
      adds_plug = !is.na(simulation_impact) & str_detect(normalise_text(simulation_impact), "plug"),
      adds_stage = !is.na(simulation_impact) & str_detect(normalise_text(simulation_impact), "extra stage|additional stage|re-frac|refrac|lost stage|screen out"),
      resource_class = risk_resource_class(category, variable),
      risk_event = as.character(variable)
    ) %>%
    select(-.scope, -.eff_prob_well) %>%
    derive_risk_consequences()

  risk_grid <- build_risk_grid(risk_table, n_wells)

  scmt_offline_prob <- get_param_prob_cached(param_cache, "SCMT offline", default = 0.8)
  scmt_offline_prob <- max(0, min(1, scmt_offline_prob))

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
    scmt_days <- sample_param_cached(param_cache, "SCMT duration", n_wells)
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
      scmt_offline = rbinom(n_wells, 1, scmt_offline_prob) == 1,
      frac_days_per_stage = frac_time_per_stage_days,
      milling_days_per_plug = milling_days_per_plug,
      scmt_days = scmt_days,
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
                                  wireline_run_days = wireline_time_per_stage_days)
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
        external_risk_delay_days = s[, "external"],
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
        online_scmt_days = ifelse(scmt_offline, 0, scmt_days),
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
        ct_workload_days = online_scmt_days + cleanout_days + ct_risk_delay_days +
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
    # SCMT offline rule (v17.3):
    # ---------------------------
    # If wireline_units >= 2, a spare wireline unit is available during perforation,
    # so SCMT can always run offline. Override scmt_offline for those wells.
    well_df <- well_df %>%
      mutate(
        # With 2+ wireline units: SCMT always runs offline (spare unit available)
        scmt_offline = scmt_offline | (wireline_units >= 2),
        online_scmt_days = ifelse(scmt_offline, 0, scmt_days),
        ct_workload_days = online_scmt_days + cleanout_days + ct_risk_delay_days +
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

    total_frac_related_days <- sum(well_df$frac_related_days, na.rm = TRUE)

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
        frac_release_day = cumsum(frac_related_days),
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

    summary_list[[iter_id]] <- tibble(
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
      estimated_campaign_days = estimated_campaign_days
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

    resource_list[[iter_id]] <- tibble(
      simulation_id = iter_id,
      operation_mode = operation_mode,
      resource = resource_names,
      units = resource_units,
      workload_days = resource_workload,
      fleet_days_after_resources = resource_fleet_days,
      utilization = resource_workload / pmax(estimated_campaign_days * resource_units, 1e-9)
    )

    well_list[[iter_id]] <- well_df %>%
      select(
        simulation_id, operation_mode, pad_id, well_id, stages, extra_stages, final_stages,
        temp_log_stages, scmt_offline, plugs, extra_plugs,
        frac_days_per_stage, frac_settling_days, milling_days_per_plug, scmt_days, cleanout_days,
        base_frac_days, frac_execution_days, wireline_time_per_stage_days, wireline_rig_up_down_days,
        wireline_contingency_pct, wireline_base_stage_days, wireline_contingency_days,
        temp_log_days, wireline_stage_readiness_days,
        wireline_fleet_days, wireline_readiness_delay_days, ct_workload_days,
        milling_days_gross, risk_delay_days, frac_related_days, frac_release_day,
        milling_start_day, milling_finish_day, milling_resource, flowback_start_day, flowback_finish_day,
        frac_tree_constraint_delay_days, is_first_on_pad, well_transition_days, wireline_rework_days, extra_wireline_runs,
        ct_consequence_days, extra_milling_plugs, risk_testing_days, frac_consequence_days
      )

    risk_log_list[[iter_id]] <- risk_log
  }

  summary <- bind_rows(summary_list)
  well_details <- bind_rows(well_list)
  risk_event_log <- bind_rows(risk_log_list)
  if (ncol(risk_event_log) == 0 || nrow(risk_event_log) == 0) risk_event_log <- empty_risk_event_log()
  resource_utilization <- bind_rows(resource_list)

  assumptions_used <- assumptions %>%
    mutate(
      risk_multiplier_used = risk_multiplier,
      probability_used = ifelse(normalise_text(type) == "risk",
                                pmin(as.numeric(probability) * risk_multiplier, 1),
                                as.numeric(probability))
    )

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

# ---------------------------------------------------------------------------
# Fixed empty-case schema in summarise_delay_contributors (v11 bug: missing
# columns crashed the PDF report when zero risks triggered).
# ---------------------------------------------------------------------------

summarise_delay_contributors <- function(risk_event_log) {
  if (is.null(risk_event_log) || nrow(risk_event_log) == 0) {
    return(tibble(
      operation_mode = character(), category = character(), risk_event = character(),
      event_count = integer(), total_delay_days = numeric(), mean_delay_days = numeric(),
      total_extra_plugs = numeric(), total_extra_stages = numeric()
    ))
  }

  risk_event_log %>%
    group_by(operation_mode, category, risk_event) %>%
    summarise(
      event_count = n(),
      total_delay_days = sum(delay_days, na.rm = TRUE),
      mean_delay_days = mean(delay_days, na.rm = TRUE),
      total_extra_plugs = sum(extra_plugs, na.rm = TRUE),
      total_extra_stages = sum(extra_stages, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(total_delay_days))
}

# ---------------------------------------------------------------------------
# Summaries, scoring, and reporting (unchanged from v11 except where noted)
# ---------------------------------------------------------------------------

summarise_simulation <- function(results) {
  if (is.list(results) && "summary" %in% names(results)) results <- results$summary
  group_cols <- intersect(c("operation_mode"), names(results))

  results %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      simulations = n(),
      p10_days = as.numeric(quantile(estimated_campaign_days, 0.10, na.rm = TRUE)),
      p50_days = as.numeric(quantile(estimated_campaign_days, 0.50, na.rm = TRUE)),
      p90_days = as.numeric(quantile(estimated_campaign_days, 0.90, na.rm = TRUE)),
      mean_days = mean(estimated_campaign_days, na.rm = TRUE),
      mean_stages = mean(total_stages, na.rm = TRUE),
      mean_extra_plugs = mean(total_extra_plugs, na.rm = TRUE),
      mean_extra_stages = mean(total_extra_stages, na.rm = TRUE),
      mean_risk_delay_days = mean(total_risk_delay_days, na.rm = TRUE),
      .groups = "drop"
    )
}

summarise_resource_utilization <- function(resource_utilization) {
  if (is.null(resource_utilization) || nrow(resource_utilization) == 0) {
    return(tibble())
  }

  resource_utilization %>%
    group_by(operation_mode, resource) %>%
    summarise(
      mean_units = mean(units, na.rm = TRUE),
      mean_workload_days = mean(workload_days, na.rm = TRUE),
      mean_fleet_days_after_resources = mean(fleet_days_after_resources, na.rm = TRUE),
      mean_utilization = mean(utilization, na.rm = TRUE),
      p90_utilization = as.numeric(quantile(utilization, 0.90, na.rm = TRUE)),
      .groups = "drop"
    )
}


summarise_wireline_constraint <- function(summary, well_details = NULL) {
  if (is.null(summary) || nrow(summary) == 0) return(tibble())

  out <- summary %>%
    group_by(operation_mode) %>%
    summarise(
      simulations = n(),
      mean_wireline_stage_operation_days = mean(total_wireline_base_stage_days, na.rm = TRUE),
      mean_wireline_rig_up_down_days = mean(total_wireline_rig_up_down_days, na.rm = TRUE),
      mean_wireline_contingency_days = mean(total_wireline_contingency_days, na.rm = TRUE),
      mean_temperature_logging_days = mean(total_temperature_logging_days, na.rm = TRUE),
      mean_frac_settling_days = mean(total_frac_settling_days, na.rm = TRUE),
      mean_wireline_risk_delay_days = mean(total_wireline_risk_delay_days, na.rm = TRUE),
      mean_wireline_stage_readiness_days = mean(total_wireline_workload_days, na.rm = TRUE),
      mean_wireline_readiness_delay_days = mean(total_wireline_readiness_delay_days, na.rm = TRUE),
      p90_wireline_readiness_delay_days = as.numeric(quantile(total_wireline_readiness_delay_days, 0.90, na.rm = TRUE)),
      mean_campaign_days = mean(estimated_campaign_days, na.rm = TRUE),
      pct_campaign_waiting_on_wireline = mean_wireline_readiness_delay_days / pmax(mean_campaign_days, 1e-9),
      .groups = "drop"
    ) %>%
    mutate(
      interpretation = case_when(
        mean_wireline_readiness_delay_days > 10 ~ "Wireline is materially constraining frac readiness.",
        mean_wireline_readiness_delay_days > 0 ~ "Wireline creates some readiness delay.",
        TRUE ~ "Wireline does not create readiness delay in this setup."
      )
    )

  out
}

summarise_bottlenecks <- function(resource_summary) {
  if (is.null(resource_summary) || nrow(resource_summary) == 0) {
    return(tibble())
  }

  resource_summary %>%
    mutate(
      bottleneck_status = case_when(
        p90_utilization >= 0.85 ~ "Critical",
        p90_utilization >= 0.60 ~ "Moderate",
        TRUE ~ "Available capacity"
      ),
      priority = case_when(
        bottleneck_status == "Critical" ~ 1L,
        bottleneck_status == "Moderate" ~ 2L,
        TRUE ~ 3L
      ),
      recommendation = case_when(
        bottleneck_status == "Critical" ~ paste0("Add one additional ", tolower(resource), " unit or review workflow."),
        bottleneck_status == "Moderate" ~ paste0("Monitor ", tolower(resource), " capacity during planning."),
        TRUE ~ paste0("No immediate additional ", tolower(resource), " capacity required.")
      )
    ) %>%
    arrange(operation_mode, priority, desc(p90_utilization))
}

summarise_stage_level_risks <- function(risk_event_log, summary = NULL) {
  if (is.null(risk_event_log) || nrow(risk_event_log) == 0) {
    return(tibble())
  }

  mode_iterations <- if (!is.null(summary) && nrow(summary) > 0) {
    summary %>% count(operation_mode, name = "simulation_count")
  } else {
    risk_event_log %>% distinct(operation_mode, simulation_id) %>% count(operation_mode, name = "simulation_count")
  }

  stage_keywords <- "screen|plug|perforation|misfire|upct|gun|cement"

  risk_event_log %>%
    filter(stage_id != "Well-level" | stringr::str_detect(normalise_text(risk_event), stage_keywords)) %>%
    group_by(operation_mode, category, risk_event) %>%
    summarise(
      total_events = n(),
      total_delay_days = sum(delay_days, na.rm = TRUE),
      total_extra_plugs = sum(extra_plugs, na.rm = TRUE),
      total_extra_stages = sum(extra_stages, na.rm = TRUE),
      mean_delay_when_occurs = mean(delay_days, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(mode_iterations, by = "operation_mode") %>%
    mutate(
      expected_events_per_campaign = total_events / pmax(simulation_count, 1),
      expected_delay_days_per_campaign = total_delay_days / pmax(simulation_count, 1),
      expected_extra_plugs_per_campaign = total_extra_plugs / pmax(simulation_count, 1),
      expected_extra_stages_per_campaign = total_extra_stages / pmax(simulation_count, 1)
    ) %>%
    arrange(desc(expected_delay_days_per_campaign), desc(expected_events_per_campaign))
}


build_traffic_lights <- function(summary, risk_event_log, resource_utilization) {
  if (is.null(summary) || nrow(summary) == 0) return(tibble())

  sim_stats <- summarise_simulation(summary)
  resource_summary <- summarise_resource_utilization(resource_utilization)
  bottlenecks <- summarise_bottlenecks(resource_summary)
  delay_summary <- summarise_delay_contributors(risk_event_log)

  risk_by_mode <- summary %>%
    group_by(operation_mode) %>%
    summarise(
      mean_risk_delay_days = mean(total_risk_delay_days, na.rm = TRUE),
      mean_campaign_days = mean(estimated_campaign_days, na.rm = TRUE),
      mean_wireline_wait_days = mean(total_wireline_readiness_delay_days, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      risk_delay_ratio = mean_risk_delay_days / pmax(mean_campaign_days, 1e-9),
      wireline_wait_ratio = mean_wireline_wait_days / pmax(mean_campaign_days, 1e-9)
    )

  max_bottleneck <- bottlenecks %>%
    group_by(operation_mode) %>%
    summarise(max_p90_utilization = max(p90_utilization, na.rm = TRUE), .groups = "drop")

  out <- sim_stats %>%
    left_join(risk_by_mode, by = "operation_mode") %>%
    left_join(max_bottleneck, by = "operation_mode") %>%
    mutate(
      uncertainty_ratio = (p90_days - p50_days) / pmax(p50_days, 1e-9),
      schedule_risk = case_when(
        uncertainty_ratio >= 0.25 ~ "Red",
        uncertainty_ratio >= 0.15 ~ "Amber",
        TRUE ~ "Green"
      ),
      resource_risk = case_when(
        max_p90_utilization >= 0.85 ~ "Red",
        max_p90_utilization >= 0.60 ~ "Amber",
        TRUE ~ "Green"
      ),
      operational_risk = case_when(
        risk_delay_ratio >= 0.10 ~ "Red",
        risk_delay_ratio >= 0.05 ~ "Amber",
        TRUE ~ "Green"
      ),
      wireline_constraint = case_when(
        wireline_wait_ratio >= 0.10 ~ "Red",
        wireline_wait_ratio > 0.02 ~ "Amber",
        TRUE ~ "Green"
      )
    ) %>%
    select(operation_mode, schedule_risk, resource_risk, operational_risk, wireline_constraint,
           uncertainty_ratio, max_p90_utilization, risk_delay_ratio, wireline_wait_ratio)

  out
}

build_readiness_score <- function(summary, risk_event_log, resource_utilization) {
  if (is.null(summary) || nrow(summary) == 0) return(tibble())

  # Transparent readiness model.
  # Weights and penalties are documented here and exported in the readiness table.
  # Component scores are bounded 0-100. Higher is better.
  weights <- list(
    schedule = 0.30,
    resource = 0.30,
    risk = 0.25,
    wireline = 0.15
  )
  penalties <- list(
    schedule_uncertainty = 250,  # 20% P90/P50 spread -> 50 schedule score
    risk_delay = 250,            # 20% risk delay ratio -> 50 risk score
    wireline_wait = 300,         # 10% wireline wait ratio -> 70 wireline score
    resource_high_util = 80      # 100% utilization on the busiest non-frac
                                  # resource -> 20 resource score
  )

  sim_stats <- summarise_simulation(summary)
  resource_summary <- summarise_resource_utilization(resource_utilization)
  bottlenecks <- summarise_bottlenecks(resource_summary)

  # Overall peak utilization (incl. frac fleet) is kept for context/reporting.
  resource_max <- bottlenecks %>%
    group_by(operation_mode) %>%
    summarise(max_p90_utilization = max(p90_utilization, na.rm = TRUE), .groups = "drop")

  # The frac fleet's utilization is structurally close to 100% by
  # construction (estimated_campaign_days is built around frac_fleet_days),
  # so it isn't an actionable spare-capacity signal on its own. The resource
  # score instead measures slack in the OTHER resources (Wireline,
  # CT/cleanout, Milling, Testing unit) - the ones the constraint cascade can
  # actually relieve.
  non_frac_max <- bottlenecks %>%
    filter(resource != "Frac fleet") %>%
    group_by(operation_mode) %>%
    slice_max(p90_utilization, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(operation_mode, non_frac_p90_utilization = p90_utilization, non_frac_bottleneck = resource)

  risk_by_mode <- summary %>%
    group_by(operation_mode) %>%
    summarise(
      risk_delay_days_for_score = mean(total_risk_delay_days, na.rm = TRUE),
      wireline_wait_days_for_score = mean(total_wireline_readiness_delay_days, na.rm = TRUE),
      campaign_days_for_score = mean(estimated_campaign_days, na.rm = TRUE),
      .groups = "drop"
    )

  sim_stats %>%
    left_join(resource_max, by = "operation_mode") %>%
    left_join(non_frac_max, by = "operation_mode") %>%
    left_join(risk_by_mode, by = "operation_mode") %>%
    mutate(
      max_p90_utilization = ifelse(is.na(max_p90_utilization), 0, max_p90_utilization),
      non_frac_p90_utilization = ifelse(is.na(non_frac_p90_utilization), 0, non_frac_p90_utilization),
      non_frac_bottleneck = ifelse(is.na(non_frac_bottleneck), "None", non_frac_bottleneck),
      campaign_days_for_score = ifelse(is.na(campaign_days_for_score), mean_days, campaign_days_for_score),
      risk_delay_days_for_score = ifelse(is.na(risk_delay_days_for_score), 0, risk_delay_days_for_score),
      wireline_wait_days_for_score = ifelse(is.na(wireline_wait_days_for_score), 0, wireline_wait_days_for_score),
      uncertainty_ratio = (p90_days - p50_days) / pmax(p50_days, 1e-9),
      risk_delay_ratio = risk_delay_days_for_score / pmax(campaign_days_for_score, 1e-9),
      wireline_wait_ratio = wireline_wait_days_for_score / pmax(campaign_days_for_score, 1e-9),
      schedule_score = pmax(0, 100 - penalties$schedule_uncertainty * uncertainty_ratio),
      resource_score = pmax(0, 100 - penalties$resource_high_util * pmax(non_frac_p90_utilization - 0.60, 0) / 0.40),
      risk_score = pmax(0, 100 - penalties$risk_delay * risk_delay_ratio),
      wireline_score = pmax(0, 100 - penalties$wireline_wait * wireline_wait_ratio),
      schedule_weight = weights$schedule,
      resource_weight = weights$resource,
      risk_weight = weights$risk,
      wireline_weight = weights$wireline,
      readiness_score = round(
        weights$schedule * schedule_score +
          weights$resource * resource_score +
          weights$risk * risk_score +
          weights$wireline * wireline_score,
        1
      ),
      readiness_status = case_when(
        readiness_score >= 80 ~ "Ready",
        readiness_score >= 60 ~ "Caution",
        readiness_score >= 40 ~ "At Risk",
        TRUE ~ "Critical"
      ),
      scoring_note = "Score = 30% schedule + 30% resource + 25% risk + 15% wireline. Resource score reflects the busiest non-frac resource (Wireline/CT/Milling/Testing) - frac-fleet utilization is structurally near 100% by design and is reported separately. Higher is better."
    ) %>%
    select(operation_mode, readiness_score, readiness_status,
           schedule_score, resource_score, risk_score, wireline_score,
           schedule_weight, resource_weight, risk_weight, wireline_weight,
           uncertainty_ratio, max_p90_utilization, non_frac_p90_utilization, non_frac_bottleneck,
           risk_delay_ratio, wireline_wait_ratio,
           scoring_note)
}

build_cost_impact <- function(
    summary,
    resource_utilization,
    frac_fleet_cost_per_day = 250000,
    wireline_cost_per_day = 15000,
    ct_cost_per_day = 25000,
    milling_cost_per_day = 18000,
    testing_unit_cost_per_day = 12000
) {
  if (is.null(summary) || nrow(summary) == 0) return(tibble())

  cost_map <- tibble(
    resource = c("Frac fleet", "Wireline", "CT / cleanout", "Milling", "Testing unit"),
    cost_per_day = c(frac_fleet_cost_per_day, wireline_cost_per_day, ct_cost_per_day,
                     milling_cost_per_day, testing_unit_cost_per_day)
  )

  resource_cost <- resource_utilization %>%
    left_join(cost_map, by = "resource") %>%
    group_by(operation_mode, resource) %>%
    summarise(
      mean_fleet_days = mean(fleet_days_after_resources, na.rm = TRUE),
      cost_per_day = first(cost_per_day),
      estimated_resource_cost = mean_fleet_days * cost_per_day,
      .groups = "drop"
    )

  wireline_idle <- summary %>%
    group_by(operation_mode) %>%
    summarise(
      mean_wireline_wait_days = mean(total_wireline_readiness_delay_days, na.rm = TRUE),
      p90_wireline_wait_days = as.numeric(quantile(total_wireline_readiness_delay_days, 0.90, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(
      resource = "Frac fleet idle while waiting on wireline",
      mean_fleet_days = mean_wireline_wait_days,
      cost_per_day = frac_fleet_cost_per_day,
      estimated_resource_cost = mean_wireline_wait_days * frac_fleet_cost_per_day
    ) %>%
    select(operation_mode, resource, mean_fleet_days, cost_per_day, estimated_resource_cost)

  bind_rows(resource_cost, wireline_idle) %>%
    arrange(operation_mode, desc(estimated_resource_cost))
}

# ---------------------------------------------------------------------------
# Total campaign cost summary (v17.6)
# Answers: "what does this campaign actually cost in total?"
#
# Total cost = Σ (units × day_rate × P50_campaign_days) for all resources
#            + wireline idle cost
#
# Broken into:
#   - Productive cost: units × day_rate × active_days  (work actually done)
#   - Standby cost:    units × day_rate × (campaign - active_days)  (on-site but idle)
#   - Idle penalty:    frac_fleet × day_rate × wireline_readiness_delay  (waiting on WL)
#
# This lets the user see: going faster with zipper costs the same or less in
# total because the campaign is shorter (fewer standby days), even though
# the per-day resource mix may be similar.
# ---------------------------------------------------------------------------
build_total_campaign_cost <- function(
    summary,
    resource_utilization,
    frac_fleets = 1, wireline_units = 1, ct_units = 1,
    milling_units = 1, testing_units = 1,
    frac_fleet_cost_per_day  = 250000,
    wireline_cost_per_day    = 15000,
    ct_cost_per_day          = 25000,
    milling_cost_per_day     = 18000,
    testing_unit_cost_per_day = 12000
) {
  if (is.null(summary) || nrow(summary) == 0) return(NULL)

  units_map <- tibble::tibble(
    resource     = c("Frac fleet","Wireline","CT / cleanout","Milling","Testing unit"),
    units        = c(frac_fleets, wireline_units, ct_units, milling_units, testing_units),
    day_rate     = c(frac_fleet_cost_per_day, wireline_cost_per_day, ct_cost_per_day,
                     milling_cost_per_day, testing_unit_cost_per_day)
  ) %>%
    dplyr::mutate(total_day_rate = units * day_rate)

  ru <- summarise_resource_utilization(resource_utilization)

  agg <- summary %>%
    dplyr::group_by(operation_mode) %>%
    dplyr::summarise(
      p50_days   = as.numeric(stats::quantile(estimated_campaign_days, 0.5, na.rm = TRUE)),
      p10_days   = as.numeric(stats::quantile(estimated_campaign_days, 0.1, na.rm = TRUE)),
      p90_days   = as.numeric(stats::quantile(estimated_campaign_days, 0.9, na.rm = TRUE)),
      idle_days  = mean(total_wireline_readiness_delay_days, na.rm = TRUE),
      .groups = "drop"
    )

  # Active fleet-days per resource from resource_utilization
  active <- ru %>%
    dplyr::left_join(units_map, by = "resource") %>%
    dplyr::select(operation_mode, resource, units, day_rate, total_day_rate,
                  mean_active_days = mean_utilization)  # utilization * campaign = active days

  # Build full cost table per mode
  purrr::map_dfr(unique(agg$operation_mode), function(mode) {
    a <- agg %>% dplyr::filter(operation_mode == mode)
    r <- ru   %>% dplyr::filter(operation_mode == mode)

    # Active days from utilization
    active_by_res <- r %>%
      dplyr::left_join(units_map, by = "resource") %>%
      dplyr::mutate(
        active_days    = mean_utilization * a$p50_days,
        standby_days   = pmax(a$p50_days - active_days, 0),
        productive_cost = active_days  * total_day_rate,
        standby_cost    = standby_days * total_day_rate
      )

    total_productive <- sum(active_by_res$productive_cost, na.rm = TRUE)
    total_standby    <- sum(active_by_res$standby_cost,    na.rm = TRUE)
    idle_penalty     <- a$idle_days * frac_fleet_cost_per_day * frac_fleets
    total_cost       <- total_productive + total_standby + idle_penalty

    tibble::tibble(
      operation_mode   = mode,
      p50_days         = a$p50_days,
      p10_days         = a$p10_days,
      p90_days         = a$p90_days,
      productive_cost  = total_productive,
      standby_cost     = total_standby,
      idle_penalty     = idle_penalty,
      total_cost       = total_cost,
      cost_per_day     = total_cost / a$p50_days,
      resource_detail  = list(active_by_res %>%
        dplyr::select(resource, units, day_rate, total_day_rate,
                      active_days, standby_days, productive_cost, standby_cost))
    )
  })
}

# ---------------------------------------------------------------------------
# Recommendation confidence (v17.6)
# Answers: "how often does zipper beat conventional across all simulations?"
# Uses paired simulation IDs (same seed, same random draws) for a fair
# like-for-like comparison. When both modes use a common seed, each iteration
# ID represents the same "state of the world" -- same risk events, same
# historical draws -- so the comparison is controlled.
# ---------------------------------------------------------------------------
build_recommendation_confidence <- function(summary) {
  if (is.null(summary) || nrow(summary) == 0) return(NULL)
  modes <- unique(summary$operation_mode)
  if (!all(c("Conventional","Zipper") %in% modes)) return(NULL)

  conv <- summary %>%
    dplyr::filter(operation_mode == "Conventional") %>%
    dplyr::select(simulation_id, conv_days = estimated_campaign_days)
  zip  <- summary %>%
    dplyr::filter(operation_mode == "Zipper") %>%
    dplyr::select(simulation_id, zip_days  = estimated_campaign_days)

  paired <- dplyr::inner_join(conv, zip, by = "simulation_id")
  if (nrow(paired) == 0) return(NULL)

  n        <- nrow(paired)
  zip_wins <- mean(paired$zip_days < paired$conv_days)
  conv_wins<- mean(paired$zip_days > paired$conv_days)
  tied     <- mean(paired$zip_days == paired$conv_days)
  mean_sav <- mean(paired$conv_days - paired$zip_days)
  p10_sav  <- as.numeric(quantile(paired$conv_days - paired$zip_days, 0.10))
  p90_sav  <- as.numeric(quantile(paired$conv_days - paired$zip_days, 0.90))

  best     <- if (zip_wins >= conv_wins) "Zipper" else "Conventional"
  best_pct <- max(zip_wins, conv_wins)

  confidence <- dplyr::case_when(
    best_pct >= 0.90 ~ "High",
    best_pct >= 0.75 ~ "Moderate",
    best_pct >= 0.60 ~ "Low",
    TRUE             ~ "Inconclusive"
  )

  tibble::tibble(
    best_option       = best,
    confidence        = confidence,
    pct_best_wins     = round(100 * best_pct, 1),
    pct_other_wins    = round(100 * min(zip_wins, conv_wins), 1),
    pct_tied          = round(100 * tied, 1),
    mean_saving_days  = round(mean_sav, 1),
    p10_saving_days   = round(p10_sav, 1),
    p90_saving_days   = round(p90_sav, 1),
    n_simulations     = n,
    confidence_note   = sprintf(
      "%s is faster in %.1f%% of the %d simulations (mean saving %.0f d, range %.0f to %.0f d)",
      best, 100 * best_pct, n, mean_sav, p10_sav, p90_sav
    )
  )
}

build_resource_recommendations <- function(summary, resource_utilization, cost_impact = NULL) {
  if (is.null(resource_utilization) || nrow(resource_utilization) == 0) return(tibble())

  resource_summary <- summarise_resource_utilization(resource_utilization)
  bottlenecks <- summarise_bottlenecks(resource_summary)
  sim_stats <- summarise_simulation(summary) %>% select(operation_mode, p50_days)

  # Approximate saving: workload is divided by one additional unit for the bottleneck resource.
  # This is a screening estimate, not a full re-simulation.
  out <- bottlenecks %>%
    left_join(sim_stats, by = "operation_mode") %>%
    mutate(
      proposed_units = mean_units + 1,
      current_fleet_days = mean_fleet_days_after_resources,
      estimated_fleet_days_with_extra_unit = mean_workload_days / pmax(proposed_units, 1),
      estimated_days_released = pmax(current_fleet_days - estimated_fleet_days_with_extra_unit, 0),
      estimated_campaign_saving_days = case_when(
        bottleneck_status == "Critical" ~ pmin(estimated_days_released, p50_days * 0.25),
        bottleneck_status == "Moderate" ~ pmin(estimated_days_released, p50_days * 0.10),
        TRUE ~ 0
      ),
      recommendation_rank = case_when(
        bottleneck_status == "Critical" ~ 1L,
        bottleneck_status == "Moderate" ~ 2L,
        TRUE ~ 3L
      ),
      recommended_action = case_when(
        bottleneck_status == "Critical" ~ paste0("Add 1 ", tolower(resource), " unit or redesign execution sequence."),
        bottleneck_status == "Moderate" ~ paste0("Monitor ", tolower(resource), " capacity and keep contingency access."),
        TRUE ~ paste0("No additional ", tolower(resource), " unit recommended at this stage.")
      )
    ) %>%
    arrange(operation_mode, recommendation_rank, desc(estimated_campaign_saving_days)) %>%
    select(operation_mode, resource, bottleneck_status, mean_units, proposed_units, p90_utilization,
           estimated_campaign_saving_days, recommended_action)

  out
}

build_executive_kpis <- function(summary, risk_event_log, resource_utilization, frac_fleet_cost_per_day = 250000) {
  if (is.null(summary) || nrow(summary) == 0) return(tibble())

  sim_stats <- summarise_simulation(summary)
  bottlenecks <- summarise_bottlenecks(summarise_resource_utilization(resource_utilization))
  delay_summary <- summarise_delay_contributors(risk_event_log)
  readiness <- build_readiness_score(summary, risk_event_log, resource_utilization)

  best <- sim_stats %>% arrange(p50_days) %>% slice(1)
  conventional <- sim_stats %>% filter(operation_mode == "Conventional") %>% slice(1)
  zipper <- sim_stats %>% filter(operation_mode == "Zipper") %>% slice(1)

  saving_days <- if (nrow(conventional) == 1 && nrow(zipper) == 1) conventional$p50_days - zipper$p50_days else NA_real_
  saving_pct <- if (!is.na(saving_days) && conventional$p50_days > 0) saving_days / conventional$p50_days else NA_real_

  primary_bottleneck <- bottlenecks %>% arrange(priority, desc(p90_utilization)) %>% slice(1)
  top_risk <- delay_summary %>%
    group_by(risk_event) %>%
    summarise(total_delay_days = sum(total_delay_days, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total_delay_days)) %>%
    slice(1)

  mean_wireline_wait <- mean(summary$total_wireline_readiness_delay_days, na.rm = TRUE)
  idle_cost <- mean_wireline_wait * frac_fleet_cost_per_day

  tibble(
    kpi = c("Best option", "P50 duration", "P90 duration", "Zipper P50 saving", "Primary bottleneck", "Top risk", "Wireline waiting", "Idle frac fleet cost", "Readiness score"),
    value = c(
      best$operation_mode,
      paste0(round(best$p50_days, 1), " days"),
      paste0(round(best$p90_days, 1), " days"),
      ifelse(is.na(saving_days), "N/A", paste0(round(saving_days, 1), " days (", round(100 * saving_pct, 1), "%)")),
      ifelse(nrow(primary_bottleneck) == 0, "N/A", paste(primary_bottleneck$operation_mode, primary_bottleneck$resource, sep = " - ")),
      ifelse(nrow(top_risk) == 0, "No triggered risks", top_risk$risk_event),
      paste0(round(mean_wireline_wait, 1), " days"),
      paste0("$", format(round(idle_cost, 0), big.mark = ",")),
      paste0(round(mean(readiness$readiness_score, na.rm = TRUE), 1), " / 100")
    )
  )
}

build_management_report <- function(summary, risk_event_log, resource_utilization,
                                    frac_fleet_cost_per_day = 250000,
                                    wireline_cost_per_day = 15000,
                                    ct_cost_per_day = 25000,
                                    milling_cost_per_day = 18000) {
  kpis <- build_executive_kpis(summary, risk_event_log, resource_utilization, frac_fleet_cost_per_day)
  sim_stats <- summarise_simulation(summary)
  resource_summary <- summarise_resource_utilization(resource_utilization)
  bottlenecks <- summarise_bottlenecks(resource_summary)
  recommendations <- build_resource_recommendations(summary, resource_utilization)
  readiness <- build_readiness_score(summary, risk_event_log, resource_utilization)
  cost_impact <- build_cost_impact(summary, resource_utilization, frac_fleet_cost_per_day, wireline_cost_per_day, ct_cost_per_day, milling_cost_per_day, testing_unit_cost_per_day)
  top_delays <- summarise_delay_contributors(risk_event_log) %>% slice_head(n = 10)
  traffic <- build_traffic_lights(summary, risk_event_log, resource_utilization)

  html_escape <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x
  }

  fmt_days <- function(x, digits = 1) {
    ifelse(is.na(x), "N/A", paste0(format(round(as.numeric(x), digits), big.mark = ",", trim = TRUE), " days"))
  }
  fmt_num <- function(x, digits = 1) {
    ifelse(is.na(x), "N/A", format(round(as.numeric(x), digits), big.mark = ",", trim = TRUE))
  }
  fmt_pct <- function(x, digits = 1) {
    ifelse(is.na(x), "N/A", paste0(round(100 * as.numeric(x), digits), "%"))
  }
  fmt_money <- function(x) {
    ifelse(is.na(x), "N/A", paste0("$", format(round(as.numeric(x), 0), big.mark = ",", trim = TRUE)))
  }

  html_table <- function(df, title = NULL, subtitle = NULL) {
    if (is.null(df) || nrow(df) == 0) {
      return(paste0(if (!is.null(title)) paste0("<h2>", html_escape(title), "</h2>") else "", "<p>No data available.</p>"))
    }
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    df[] <- lapply(df, function(col) {
      if (is.numeric(col)) {
        return(format(round(col, 2), big.mark = ",", trim = TRUE))
      }
      as.character(col)
    })
    header <- paste0("<tr>", paste0("<th>", html_escape(names(df)), "</th>", collapse = ""), "</tr>")
    rows <- apply(df, 1, function(row) {
      paste0("<tr>", paste0("<td>", html_escape(row), "</td>", collapse = ""), "</tr>")
    })
    paste0(
      if (!is.null(title)) paste0("<h2>", html_escape(title), "</h2>") else "",
      if (!is.null(subtitle)) paste0("<p class='section-note'>", html_escape(subtitle), "</p>") else "",
      "<table>", header, paste(rows, collapse = ""), "</table>"
    )
  }

  get_kpi <- function(name) {
    val <- kpis$value[kpis$kpi == name]
    if (length(val) == 0) "N/A" else val[1]
  }

  best_mode <- get_kpi("Best option")
  p50 <- get_kpi("P50 duration")
  p90 <- get_kpi("P90 duration")
  saving <- get_kpi("Zipper P50 saving")
  bottleneck <- get_kpi("Primary bottleneck")
  top_risk <- get_kpi("Top risk")
  wireline_wait <- get_kpi("Wireline waiting")
  idle_cost <- get_kpi("Idle frac fleet cost")
  readiness_kpi <- get_kpi("Readiness score")

  sim_report <- sim_stats %>%
    transmute(
      `Operation mode` = operation_mode,
      `Simulations` = simulations,
      `P10` = fmt_days(p10_days),
      `P50` = fmt_days(p50_days),
      `P90` = fmt_days(p90_days),
      `Mean duration` = fmt_days(mean_days),
      `Mean stages` = fmt_num(mean_stages),
      `Mean risk delay` = fmt_days(mean_risk_delay_days)
    )

  readiness_report <- readiness %>%
    transmute(
      `Operation mode` = operation_mode,
      `Overall score` = paste0(round(readiness_score, 1), " / 100"),
      `Status` = readiness_status,
      `Schedule` = round(schedule_score, 1),
      `Resource` = round(resource_score, 1),
      `Risk` = round(risk_score, 1),
      `Wireline` = round(wireline_score, 1),
      `Uncertainty` = fmt_pct(uncertainty_ratio),
      `Max P90 utilization (incl. frac)` = fmt_pct(max_p90_utilization),
      `Non-frac bottleneck` = non_frac_bottleneck,
      `Non-frac P90 utilization` = fmt_pct(non_frac_p90_utilization),
      `Risk delay ratio` = fmt_pct(risk_delay_ratio),
      `Wireline wait ratio` = fmt_pct(wireline_wait_ratio)
    )

  bottleneck_report <- bottlenecks %>%
    transmute(
      `Operation mode` = operation_mode,
      `Resource` = resource,
      `P90 utilization` = fmt_pct(p90_utilization),
      `Status` = bottleneck_status,
      `Recommendation` = recommendation
    ) %>%
    arrange(`Operation mode`, factor(`Status`, levels = c("Critical", "Moderate", "Available capacity")))

  recommendation_report <- recommendations %>%
    transmute(
      `Operation mode` = operation_mode,
      `Resource` = resource,
      `Status` = bottleneck_status,
      `Current units` = mean_units,
      `Proposed units` = proposed_units,
      `P90 utilization` = fmt_pct(p90_utilization),
      `Estimated saving` = fmt_days(estimated_campaign_saving_days),
      `Recommended action` = recommended_action
    ) %>%
    arrange(`Operation mode`, desc(as.numeric(gsub("[^0-9.-]", "", `Estimated saving`))))

  cost_report <- cost_impact %>%
    transmute(
      `Operation mode` = operation_mode,
      `Resource` = resource,
      `Mean fleet days` = fmt_num(mean_fleet_days),
      `Cost per day` = fmt_money(cost_per_day),
      `Estimated cost` = fmt_money(estimated_resource_cost)
    )

  delay_report <- top_delays %>%
    transmute(
      `Operation mode` = operation_mode,
      `Category` = category,
      `Risk event` = risk_event,
      `Event count` = event_count,
      `Total delay` = fmt_days(total_delay_days),
      `Mean delay` = fmt_days(mean_delay_days),
      `Extra plugs` = total_extra_plugs,
      `Extra stages` = total_extra_stages
    )

  # build_traffic_lights() returns one row per operation mode with risk columns.
  # Convert it to a clean long table for the report.
  traffic_report <- traffic %>%
    tidyr::pivot_longer(
      cols = c(schedule_risk, resource_risk, operational_risk, wireline_constraint),
      names_to = "Area",
      values_to = "Status"
    ) %>%
    mutate(
      Area = dplyr::recode(Area,
        schedule_risk = "Schedule risk",
        resource_risk = "Resource risk",
        operational_risk = "Operational risk",
        wireline_constraint = "Wireline constraint"
      ),
      Status = dplyr::recode(Status, Red = "Red", Amber = "Amber", Green = "Green")
    ) %>%
    select(`Operation mode` = operation_mode, Area, Status)

  kpi_cards <- paste0(
    "<div class='kpi-grid'>",
    "<div class='kpi'><span>Best option</span><strong>", html_escape(best_mode), "</strong></div>",
    "<div class='kpi'><span>P50 duration</span><strong>", html_escape(p50), "</strong></div>",
    "<div class='kpi'><span>P90 duration</span><strong>", html_escape(p90), "</strong></div>",
    "<div class='kpi'><span>Zipper saving</span><strong>", html_escape(saving), "</strong></div>",
    "<div class='kpi'><span>Primary bottleneck</span><strong>", html_escape(bottleneck), "</strong></div>",
    "<div class='kpi'><span>Top risk</span><strong>", html_escape(top_risk), "</strong></div>",
    "<div class='kpi'><span>Wireline waiting</span><strong>", html_escape(wireline_wait), "</strong></div>",
    "<div class='kpi'><span>Idle frac fleet cost</span><strong>", html_escape(idle_cost), "</strong></div>",
    "<div class='kpi'><span>Readiness</span><strong>", html_escape(readiness_kpi), "</strong></div>",
    "</div>"
  )

  paste0(
    "<!doctype html><html><head><meta charset='utf-8'><title>Frac Campaign Planning Report</title>",
    "<style>",
    "body{font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Arial,sans-serif;color:#1f2937;margin:36px;line-height:1.35;}",
    "h1{font-size:30px;margin-bottom:4px;}h2{font-size:20px;margin-top:30px;border-bottom:1px solid #d1d5db;padding-bottom:6px;}",
    ".meta{color:#6b7280;margin-bottom:22px;}.summary{background:#f8fafc;border:1px solid #e5e7eb;border-radius:10px;padding:16px;margin:18px 0;}",
    ".kpi-grid{display:grid;grid-template-columns:repeat(3,1fr);gap:10px;margin:18px 0 26px 0;}",
    ".kpi{border:1px solid #d1d5db;border-radius:8px;padding:12px;background:#ffffff;}.kpi span{display:block;color:#6b7280;font-size:12px;margin-bottom:6px;}.kpi strong{font-size:16px;}",
    "table{border-collapse:collapse;width:100%;margin:12px 0 20px 0;font-size:12px;}th{background:#f3f4f6;text-align:left;}th,td{border:1px solid #d1d5db;padding:7px;vertical-align:top;}",
    ".section-note{color:#4b5563;margin:4px 0 10px 0;}ul{margin-top:8px;} .footer{margin-top:28px;color:#6b7280;font-size:12px;}",
    "@media print{body{margin:20mm}.kpi-grid{grid-template-columns:repeat(3,1fr);}h2{break-after:avoid;}table{break-inside:auto;}tr{break-inside:avoid;break-after:auto;}}",
    "</style></head><body>",
    "<h1>Frac Campaign Planning Report</h1>",
    "<div class='meta'>Generated: ", html_escape(format(Sys.time(), "%Y-%m-%d %H:%M:%S")), "</div>",
    "<div class='summary'><p>This report summarizes simulated frac campaign outcomes using the uploaded historical well data, risk assumptions, and selected resource inputs.</p>",
    "<ul>",
    "<li>Best execution option: <strong>", html_escape(best_mode), "</strong></li>",
    "<li>Expected P50 duration: <strong>", html_escape(p50), "</strong></li>",
    "<li>Conservative P90 duration: <strong>", html_escape(p90), "</strong></li>",
    "<li>Primary bottleneck: <strong>", html_escape(bottleneck), "</strong></li>",
    "<li>Top delay risk: <strong>", html_escape(top_risk), "</strong></li>",
    "</ul></div>",
    kpi_cards,
    html_table(sim_report, "Campaign Duration Summary"),
    html_table(readiness_report, "Readiness Score Breakdown", "The readiness score combines schedule certainty, resource availability, risk exposure, and wireline readiness. Higher values indicate better readiness."),
    html_table(traffic_report, "Traffic Light Summary"),
    html_table(bottleneck_report, "Bottleneck Detection"),
    html_table(recommendation_report, "Recommended Actions"),
    html_table(cost_report, "Cost Impact"),
    html_table(delay_report, "Top Delay Contributors"),
    "<h2>Notes and Limitations</h2>",
    "<ul>",
    "<li>This is an operational planning simulation. It is not a hydraulic fracture propagation model.</li>",
    "<li>Results depend on the quality of the uploaded historical data and assumptions.</li>",
    "<li>Cost estimates use the daily rates entered in the app and should be reviewed against contract values.</li>",
    "<li>Readiness scores are decision-support indicators, not deterministic go/no-go criteria.</li>",
    "</ul>",
    "<div class='footer'>Generated by Frac Campaign Planning Simulator.</div>",
    "</body></html>"
  )
}

build_executive_summary <- function(summary, risk_event_log, resource_utilization) {
  if (is.null(summary) || nrow(summary) == 0) return(tibble())

  sim_stats <- summarise_simulation(summary)
  resource_summary <- summarise_resource_utilization(resource_utilization)
  bottlenecks <- summarise_bottlenecks(resource_summary)
  delay_summary <- summarise_delay_contributors(risk_event_log)

  best <- sim_stats %>% arrange(p50_days) %>% slice(1)
  worst <- sim_stats %>% arrange(desc(p50_days)) %>% slice(1)

  conventional <- sim_stats %>% filter(operation_mode == "Conventional") %>% slice(1)
  zipper <- sim_stats %>% filter(operation_mode == "Zipper") %>% slice(1)

  saving_days <- if (nrow(conventional) == 1 && nrow(zipper) == 1) conventional$p50_days - zipper$p50_days else NA_real_
  saving_pct <- if (!is.na(saving_days) && conventional$p50_days > 0) saving_days / conventional$p50_days else NA_real_

  primary_bottleneck <- bottlenecks %>%
    arrange(priority, desc(p90_utilization)) %>%
    slice(1)

  top_risk <- delay_summary %>%
    group_by(risk_event) %>%
    summarise(total_delay_days = sum(total_delay_days, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total_delay_days)) %>%
    slice(1)

  tibble(
    metric = c(
      "Campaign size",
      "Operation modes simulated",
      "Best execution option",
      "Best option P50 duration, days",
      "Highest P50 duration, days",
      "Estimated P50 saving from zipper, days",
      "Estimated P50 saving from zipper, %",
      "Primary bottleneck",
      "Primary bottleneck status",
      "Mean wireline readiness delay, days",
      "Top delay contributor",
      "Mean extra plugs per campaign",
      "Mean extra stages per campaign",
      "Mean risk delay per campaign, days"
    ),
    value = c(
      paste0(unique(summary$wells)[1], " wells"),
      paste(unique(summary$operation_mode), collapse = ", "),
      best$operation_mode,
      round(best$p50_days, 2),
      round(worst$p50_days, 2),
      ifelse(is.na(saving_days), "N/A", round(saving_days, 2)),
      ifelse(is.na(saving_pct), "N/A", paste0(round(100 * saving_pct, 1), "%")),
      ifelse(nrow(primary_bottleneck) == 0, "N/A", paste(primary_bottleneck$operation_mode, primary_bottleneck$resource, sep = " - ")),
      ifelse(nrow(primary_bottleneck) == 0, "N/A", primary_bottleneck$bottleneck_status),
      round(mean(summary$total_wireline_readiness_delay_days, na.rm = TRUE), 2),
      ifelse(nrow(top_risk) == 0, "No triggered risks", top_risk$risk_event),
      round(mean(summary$total_extra_plugs, na.rm = TRUE), 2),
      round(mean(summary$total_extra_stages, na.rm = TRUE), 2),
      round(mean(summary$total_risk_delay_days, na.rm = TRUE), 2)
    )
  )
}

# Build a dependency-light PDF management report using base R graphics.
# This avoids pagedown/Chrome dependency and produces a true PDF file.
# ---------------------------------------------------------------------------
# NEW v13.1: Indicative resource deployment timeline.
# The engine is a workload aggregator, not a discrete-event scheduler, so this
# is a sequence-based approximation: bar lengths are mean fleet days; start
# offsets follow the operational sequence (CT cleanout -> wireline -> frac ->
# milling -> flowback/testing), each lagged by one average well cycle.
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Resource deployment timeline - redesigned in v16.1
#
# The previous version showed fleet-days as bar length with sequential start
# offsets. This was misleading: CT ends at day 34 while the campaign runs to
# day 280, making it look like CT leaves site early.
#
# The corrected version shows TWO layers per resource:
#   1. Deployment window (light bar): the calendar period the resource is
#      mobilised on-site from the first time it is needed to when the last
#      unit of work it can do is done.
#   2. Active work (dark bar): fleet-days of actual work within that window.
#
# Deployment windows are derived from the operational sequence:
#   CT:      day 0  -> frac_path_end  (needed throughout for contingency)
#   Wireline:day 0  -> frac_path_end  (perforating and plug-setting all wells)
#   Frac:    first well CT complete -> campaign end
#   Milling: first well frac-released -> post_frac_end
#   Testing: first well milling-complete -> campaign end
#
# Start offsets use mean per-well cycle times as in the previous version.
# ---------------------------------------------------------------------------

build_resource_timeline <- function(summary) {
  empty <- tibble(
    operation_mode = character(), resource = character(),
    deploy_start = numeric(), deploy_end = numeric(),
    active_start = numeric(), active_end = numeric(),
    active_days = numeric(), deploy_days = numeric(),
    utilization_of_deployment = numeric(), utilization_of_campaign = numeric(),
    campaign_days = numeric()
  )
  if (is.null(summary) || nrow(summary) == 0) return(empty)

  agg <- summary %>%
    group_by(operation_mode) %>%
    summarise(
      n_wells = dplyr::first(wells),
      campaign_days = mean(estimated_campaign_days, na.rm = TRUE),
      ct_days = mean(total_ct_fleet_days, na.rm = TRUE),
      wl_days = mean(total_wireline_fleet_days, na.rm = TRUE),
      fr_days = mean(total_frac_fleet_days, na.rm = TRUE),
      mill_days = mean(total_milling_fleet_days, na.rm = TRUE),
      test_days = mean(total_testing_fleet_days, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      # Per-well cycle times used to estimate mobilisation start offsets
      ct_pw   = ct_days   / pmax(n_wells, 1),
      wl_pw   = wl_days   / pmax(n_wells, 1),
      fr_pw   = fr_days   / pmax(n_wells, 1),
      mill_pw = mill_days / pmax(n_wells, 1),
      # Milling starts after first wells are released (~after first well CT + WL cycle)
      mill_start = ct_pw + wl_pw + fr_pw,
      # Testing starts after first wells are milled
      test_start = mill_start + mill_pw
    )

  out <- lapply(seq_len(nrow(agg)), function(i) {
    r <- agg[i, ]

    # Deployment windows: when is this resource on-site?
    # CT and Wireline: from day 0 to frac completion
    # Frac fleet: from first well ready to campaign end
    # Milling: from first well frac-released to end of campaign
    # Testing: from first well milled to end of campaign
    deploy_starts <- c(
      "CT / cleanout" = 0,
      "Wireline"      = 0,
      "Frac fleet"    = r$ct_pw,
      "Milling"       = pmax(r$mill_start, 0),
      "Testing unit"  = pmax(r$test_start, 0)
    )
    # CT and Wireline are mobilised for the entire frac phase and need to remain
    # available for contingency interventions until frac is complete. Their
    # deployment window ends at campaign_days (the campaign boundary for the
    # frac-critical path, which equals total campaign when frac is the bottleneck).
    # Using campaign_days directly is the defensible planning assumption.
    deploy_ends <- c(
      "CT / cleanout" = r$campaign_days,
      "Wireline"      = r$campaign_days,
      "Frac fleet"    = r$campaign_days,
      "Milling"       = r$campaign_days,
      "Testing unit"  = r$campaign_days
    )
    active_days_raw <- c(
      "CT / cleanout" = r$ct_days,
      "Wireline"      = r$wl_days,
      "Frac fleet"    = r$fr_days,
      "Milling"       = r$mill_days,
      "Testing unit"  = r$test_days
    )

    deploy_days_v <- pmax(deploy_ends - deploy_starts, 1e-9)

    # Cap active bar at the deployment window so bars never exceed campaign line.
    # When active_days_raw > deploy_days the resource is overloaded (utilization
    # > 100%). We report the true utilization but clip the visual bar at the
    # window boundary — the overload is visible from utilization_of_deployment > 1.
    active_days_v <- pmin(as.numeric(active_days_raw), deploy_days_v)

    tibble(
      operation_mode = r$operation_mode,
      resource = names(deploy_starts),
      deploy_start = as.numeric(deploy_starts),
      deploy_end   = as.numeric(deploy_ends),
      deploy_days  = as.numeric(deploy_days_v),
      active_start = as.numeric(deploy_starts),
      active_end   = as.numeric(deploy_starts) + active_days_v,
      active_days  = active_days_v,
      active_days_true = as.numeric(active_days_raw),   # unclipped, used for label
      utilization_of_deployment = as.numeric(active_days_raw) / pmax(deploy_days_v, 1e-9),
      utilization_of_campaign   = as.numeric(active_days_raw) / pmax(r$campaign_days, 1e-9),
      campaign_days = r$campaign_days
    )
  })

  bind_rows(out) %>%
    mutate(resource = factor(resource, levels = rev(c(
      "CT / cleanout", "Wireline", "Frac fleet", "Milling", "Testing unit"
    ))))
}

build_management_report_pdf <- function(file, summary, risk_event_log, resource_utilization,
                                        frac_fleet_cost_per_day = 250000,
                                        wireline_cost_per_day = 15000,
                                        ct_cost_per_day = 25000,
                                        milling_cost_per_day = 18000,
                                        testing_unit_cost_per_day = 12000) {
  kpis <- build_executive_kpis(summary, risk_event_log, resource_utilization, frac_fleet_cost_per_day)
  sim_stats <- summarise_simulation(summary)
  resource_summary <- summarise_resource_utilization(resource_utilization)
  bottlenecks <- summarise_bottlenecks(resource_summary)
  recommendations <- build_resource_recommendations(summary, resource_utilization)
  readiness <- build_readiness_score(summary, risk_event_log, resource_utilization)
  cost_impact <- build_cost_impact(summary, resource_utilization, frac_fleet_cost_per_day, wireline_cost_per_day, ct_cost_per_day, milling_cost_per_day, testing_unit_cost_per_day)
  top_delays <- summarise_delay_contributors(risk_event_log) %>% slice_head(n = 10)
  traffic <- build_traffic_lights(summary, risk_event_log, resource_utilization)

  fmt_days <- function(x, digits = 1) {
    ifelse(is.na(x), "N/A", paste0(format(round(as.numeric(x), digits), big.mark = ",", trim = TRUE), " d"))
  }
  fmt_num <- function(x, digits = 1) {
    ifelse(is.na(x), "N/A", format(round(as.numeric(x), digits), big.mark = ",", trim = TRUE))
  }
  fmt_pct <- function(x, digits = 1) {
    ifelse(is.na(x), "N/A", paste0(round(100 * as.numeric(x), digits), "%"))
  }
  fmt_money <- function(x) {
    ifelse(is.na(x), "N/A", paste0("$", format(round(as.numeric(x), 0), big.mark = ",", trim = TRUE)))
  }
  compact <- function(x, width = 45) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    ifelse(nchar(x) > width, paste0(substr(x, 1, width - 3), "..."), x)
  }

  sim_report <- sim_stats %>%
    transmute(
      Mode = operation_mode,
      Sims = simulations,
      P10 = fmt_days(p10_days),
      P50 = fmt_days(p50_days),
      P90 = fmt_days(p90_days),
      Mean = fmt_days(mean_days),
      Stages = fmt_num(mean_stages),
      `Risk delay` = fmt_days(mean_risk_delay_days)
    )

  readiness_report <- readiness %>%
    transmute(
      Mode = operation_mode,
      Score = paste0(round(readiness_score, 1), "/100"),
      Status = readiness_status,
      Schedule = round(schedule_score, 1),
      Resource = round(resource_score, 1),
      Risk = round(risk_score, 1),
      Wireline = round(wireline_score, 1),
      Uncertainty = fmt_pct(uncertainty_ratio),
      `Non-frac util.` = fmt_pct(non_frac_p90_utilization)
    )

  bottleneck_report <- bottlenecks %>%
    transmute(
      Mode = operation_mode,
      Resource = resource,
      `P90 util.` = fmt_pct(p90_utilization),
      Status = bottleneck_status,
      Recommendation = compact(recommendation, 55)
    ) %>%
    arrange(Mode, factor(Status, levels = c("Critical", "Moderate", "Available capacity")))

  # FIX v12: arrange BEFORE transmute. transmute() drops
  # estimated_campaign_saving_days, so arranging on it afterwards raised
  # "In argument: `..2 = estimated_campaign_saving_days`".
  recommendation_report <- recommendations %>%
    arrange(operation_mode, desc(estimated_campaign_saving_days)) %>%
    transmute(
      Mode = operation_mode,
      Resource = resource,
      Status = bottleneck_status,
      Units = paste0(mean_units, " -> ", proposed_units),
      `P90 util.` = fmt_pct(p90_utilization),
      Saving = fmt_days(estimated_campaign_saving_days),
      Action = compact(recommended_action, 55)
    )

  cost_report <- cost_impact %>%
    transmute(
      Mode = operation_mode,
      Resource = compact(resource, 38),
      `Fleet days` = fmt_num(mean_fleet_days),
      `Cost/day` = fmt_money(cost_per_day),
      `Estimated cost` = fmt_money(estimated_resource_cost)
    )

  delay_report <- top_delays %>%
    transmute(
      Mode = operation_mode,
      Category = category,
      Risk = compact(risk_event, 42),
      Events = event_count,
      `Total delay` = fmt_days(total_delay_days),
      `Mean delay` = fmt_days(mean_delay_days),
      `Extra plugs` = total_extra_plugs,
      `Extra stages` = total_extra_stages
    )

  traffic_report <- traffic %>%
    tidyr::pivot_longer(
      cols = c(schedule_risk, resource_risk, operational_risk, wireline_constraint),
      names_to = "Area",
      values_to = "Status"
    ) %>%
    mutate(
      Area = dplyr::recode(Area,
        schedule_risk = "Schedule risk",
        resource_risk = "Resource risk",
        operational_risk = "Operational risk",
        wireline_constraint = "Wireline constraint"
      )
    ) %>%
    select(Mode = operation_mode, Area, Status)

  get_kpi <- function(name) {
    val <- kpis$value[kpis$kpi == name]
    if (length(val) == 0) "N/A" else val[1]
  }

  # ==========================================================================
  # Rendering: landscape A4, grid + ggplot2 + gridExtra. Branded layout with
  # navy header band, KPI card dashboard, charts per section, styled tables.
  # ==========================================================================
  if (!requireNamespace("gridExtra", quietly = TRUE)) {
    stop("The PDF report requires the 'gridExtra' package. Install with install.packages('gridExtra').")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The PDF report requires the 'ggplot2' package.")
  }

  navy <- "#0F2A43"; teal <- "#18A999"; amber <- "#E6A817"
  red_c <- "#D55E00"; green_c <- "#009E73"; panel_grey <- "#F4F6F8"
  mode_cols <- c("Conventional" = "#0072B2", "Zipper" = "#E69F00")
  status_cols <- c("Critical" = red_c, "Moderate" = amber, "Available capacity" = green_c,
                   "Ready" = green_c, "Caution" = amber, "At Risk" = red_c)
  resource_cols <- c("CT / cleanout" = "#56B4E9", "Wireline" = "#0072B2",
                     "Frac fleet" = "#E69F00", "Milling" = "#D55E00",
                     "Testing unit" = "#009E73")

  rpt_theme <- ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 12, colour = navy),
      plot.subtitle = ggplot2::element_text(size = 9, colour = "grey40"),
      plot.caption = ggplot2::element_text(size = 7.5, colour = "grey45", hjust = 0),
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    )

  tbl_theme <- gridExtra::ttheme_minimal(
    core = list(
      bg_params = list(fill = rep(c("white", panel_grey), length.out = 50), col = NA),
      fg_params = list(cex = 0.66, hjust = 0, x = 0.03)
    ),
    colhead = list(
      bg_params = list(fill = navy, col = NA),
      fg_params = list(col = "white", fontface = "bold", cex = 0.68, hjust = 0, x = 0.03)
    ),
    padding = grid::unit(c(5, 4), "mm")
  )

  page_no <- 0
  new_page <- function(title, subtitle = NULL) {
    page_no <<- page_no + 1
    grid::grid.newpage()
    grid::grid.rect(y = grid::unit(1, "npc"), height = grid::unit(0.09, "npc"),
                    just = "top", gp = grid::gpar(fill = navy, col = NA))
    grid::grid.text(title, x = 0.035, y = if (is.null(subtitle)) 0.955 else 0.965,
                    just = c("left", "center"),
                    gp = grid::gpar(col = "white", fontface = "bold",
                                    cex = 1.3, fontfamily = "sans"))
    if (!is.null(subtitle)) {
      grid::grid.text(subtitle, x = 0.035, y = 0.928, just = c("left", "center"),
                      gp = grid::gpar(col = "#C9D4DF", cex = 0.75))
    }
    grid::grid.text(format(Sys.time(), "%d %b %Y"), x = 0.965, y = 0.955,
                    just = c("right", "center"),
                    gp = grid::gpar(col = "#C9D4DF", cex = 0.78))
    grid::grid.lines(x = c(0.035, 0.965), y = 0.045,
                     gp = grid::gpar(col = "#D5DCE3"))
    grid::grid.text("Frac Campaign Planning Simulator", x = 0.035, y = 0.027,
                    just = "left", gp = grid::gpar(col = "grey45", cex = 0.62))
    grid::grid.text(paste("Page", page_no), x = 0.965, y = 0.027, just = "right",
                    gp = grid::gpar(col = "grey45", cex = 0.62))
  }

  draw_plot_in <- function(p, x, y, w, h) {
    print(p, vp = grid::viewport(x = x, y = y, width = w, height = h))
  }

  draw_table_in <- function(df, x, y, w, h) {
    if (is.null(df) || nrow(df) == 0) {
      grid::grid.text("No data available.", x = x, y = y,
                      gp = grid::gpar(cex = 0.8, col = "grey50"))
      return(invisible(NULL))
    }
    g <- gridExtra::tableGrob(as.data.frame(df), rows = NULL, theme = tbl_theme)
    grid::pushViewport(grid::viewport(x = x, y = y, width = w, height = h))
    # scale down if wider than viewport
    gw <- grid::convertWidth(sum(g$widths), "npc", valueOnly = TRUE)
    gh <- grid::convertHeight(sum(g$heights), "npc", valueOnly = TRUE)
    sc <- min(1, 1 / max(gw, 1e-9), 1 / max(gh, 1e-9))
    grid::pushViewport(grid::viewport(width = sc, height = sc))
    grid::grid.draw(g)
    grid::popViewport(2)
  }

  draw_kpi_card <- function(label, value, x, y, w, h, accent = teal) {
    grid::grid.roundrect(x = x, y = y, width = w, height = h, r = grid::unit(2.5, "mm"),
                         gp = grid::gpar(fill = panel_grey, col = "#DDE3E9"))
    grid::grid.rect(x = x - w / 2 + grid::unit(1.2, "mm"), y = y,
                    width = grid::unit(1.8, "mm"), height = h * 0.62,
                    gp = grid::gpar(fill = accent, col = NA))
    grid::grid.text(toupper(label), x = x - w / 2 + grid::unit(6, "mm"),
                    y = y + h * 0.22, just = c("left", "center"),
                    gp = grid::gpar(cex = 0.58, col = "grey40", fontface = "bold"))
    grid::grid.text(value, x = x - w / 2 + grid::unit(6, "mm"),
                    y = y - h * 0.14, just = c("left", "center"),
                    gp = grid::gpar(cex = 0.95, col = navy, fontface = "bold"))
  }

  # --- Inline charts ---------------------------------------------------------
  pctiles <- summary %>%
    group_by(operation_mode) %>%
    summarise(p50 = quantile(estimated_campaign_days, 0.5, na.rm = TRUE),
              p90 = quantile(estimated_campaign_days, 0.9, na.rm = TRUE),
              .groups = "drop")

  scurve_p <- ggplot2::ggplot(summary,
      ggplot2::aes(estimated_campaign_days, colour = operation_mode)) +
    ggplot2::stat_ecdf(linewidth = 1.1) +
    ggplot2::geom_vline(data = pctiles,
      ggplot2::aes(xintercept = p50, colour = operation_mode),
      linetype = "dashed", linewidth = 0.4, show.legend = FALSE) +
    ggplot2::geom_text(data = pctiles,
      ggplot2::aes(x = p50, y = 0.06, colour = operation_mode,
                   label = paste0("P50: ", round(p50), " d")),
      hjust = -0.05, size = 2.9, show.legend = FALSE) +
    ggplot2::scale_colour_manual(values = mode_cols) +
    ggplot2::scale_y_continuous(labels = scales::percent_format()) +
    ggplot2::labs(title = "Campaign duration S-curve",
                  x = "Campaign duration, days", y = "Cumulative probability",
                  colour = NULL) +
    rpt_theme

  readiness_p <- ggplot2::ggplot(readiness,
      ggplot2::aes(operation_mode, readiness_score, fill = readiness_status)) +
    ggplot2::geom_col(width = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(round(readiness_score), "/100")),
                       vjust = -0.45, size = 3.4, fontface = "bold", colour = navy) +
    ggplot2::coord_cartesian(ylim = c(0, 110)) +
    ggplot2::scale_fill_manual(values = status_cols) +
    ggplot2::labs(title = "Campaign readiness", x = NULL, y = "Score", fill = NULL) +
    rpt_theme

  bottleneck_p <- ggplot2::ggplot(bottlenecks,
      ggplot2::aes(resource, p90_utilization, fill = bottleneck_status)) +
    ggplot2::geom_col(width = 0.62) +
    ggplot2::geom_hline(yintercept = c(0.60, 0.85), linetype = "dashed",
                        colour = "grey55", linewidth = 0.35) +
    ggplot2::facet_wrap(~ operation_mode) +
    ggplot2::scale_fill_manual(values = status_cols) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::labs(title = "Bottleneck detection (P90 utilization, thresholds 60% / 85%)",
                  x = NULL, y = "P90 utilization", fill = NULL) +
    rpt_theme +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 28, hjust = 1, size = 7.5))

  timeline <- build_resource_timeline(summary)
  camp_lines <- timeline %>% distinct(operation_mode, campaign_days)

  # Two-layer gantt: light = deployment window, dark = active work
  resource_cols_dark_pdf  <- c("CT / cleanout"="#2980B9","Wireline"="#0F2A43",
                               "Frac fleet"="#D68910","Milling"="#A93226","Testing unit"="#1E8449")
  resource_cols_light_pdf <- c("CT / cleanout"="#AED6F1","Wireline"="#5D8AA8",
                               "Frac fleet"="#FAD7A0","Milling"="#F1948A","Testing unit"="#A9DFBF")

  gantt_p <- ggplot2::ggplot(timeline, ggplot2::aes(y = resource)) +
    ggplot2::geom_segment(
      ggplot2::aes(x = deploy_start, xend = deploy_end, yend = resource,
                   colour = paste0(resource, "_light")),
      linewidth = 7, lineend = "butt", alpha = 0.45) +
    ggplot2::geom_segment(
      ggplot2::aes(x = active_start, xend = active_end, yend = resource,
                   colour = paste0(resource, "_dark")),
      linewidth = 7, lineend = "butt") +
    ggplot2::geom_text(
      ggplot2::aes(x = deploy_end,
                   label = paste0(round(active_days_true), "d / ",
                                  round(100 * utilization_of_deployment), "%"),
                   colour = utilization_of_deployment > 1),
      hjust = -0.08, size = 2.5) +
    ggplot2::scale_colour_manual(
      values = c(`FALSE` = "grey25", `TRUE` = "#D55E00"),
      guide = "none") +
    ggplot2::geom_vline(data = camp_lines,
      ggplot2::aes(xintercept = campaign_days),
      linetype = "dashed", colour = "grey35", linewidth = 0.5) +
    ggplot2::facet_grid(rows = ggplot2::vars(operation_mode)) +
    ggplot2::scale_colour_manual(
      values = c(setNames(resource_cols_dark_pdf,  paste0(names(resource_cols_dark_pdf),  "_dark")),
                 setNames(resource_cols_light_pdf, paste0(names(resource_cols_light_pdf), "_light"))),
      guide = "none") +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.01, 0.20))) +
    ggplot2::labs(
      title = "Resource deployment timeline",
      subtitle = "Light = on-site window  |  Dark = active work days  |  Label: active days / utilization of deployment window",
      caption = "Indicative only: workload-based model. All bars now reach near campaign duration because resources remain mobilised throughout.",
      x = "Campaign day", y = NULL) +
    rpt_theme

  cost_plot_df <- cost_impact %>%
    mutate(resource_w = ifelse(nchar(resource) > 30,
                               paste0(substr(resource, 1, 27), "..."), resource))
  cost_p <- ggplot2::ggplot(cost_plot_df,
      ggplot2::aes(reorder(resource_w, estimated_resource_cost),
                   estimated_resource_cost, fill = operation_mode)) +
    ggplot2::geom_col(position = ggplot2::position_dodge2(preserve = "single"),
                      width = 0.7) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = mode_cols) +
    ggplot2::scale_y_continuous(
      labels = function(x) ifelse(is.na(x), NA_character_,
        ifelse(abs(x) >= 1e6, paste0("$", round(x / 1e6, 1), "M"),
        ifelse(abs(x) >= 1e3, paste0("$", round(x / 1e3), "k"),
               paste0("$", round(x)))))) +
    ggplot2::labs(title = "Estimated resource and idle cost",
                  x = NULL, y = "Estimated cost", fill = NULL) +
    rpt_theme

  delay_plot_df <- top_delays %>%
    mutate(risk_w = ifelse(nchar(risk_event) > 30,
                           paste0(substr(risk_event, 1, 27), "..."), risk_event))
  delay_p <- ggplot2::ggplot(delay_plot_df,
      ggplot2::aes(reorder(risk_w, total_delay_days), total_delay_days,
                   fill = operation_mode)) +
    ggplot2::geom_col(position = ggplot2::position_dodge2(preserve = "single"),
                      width = 0.72) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = mode_cols) +
    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    ggplot2::labs(title = "Top delay contributors",
                  x = NULL, y = "Total delay days across simulations", fill = NULL) +
    rpt_theme

  # --- Assemble pages --------------------------------------------------------
  grDevices::pdf(file, width = 11.69, height = 8.27, onefile = TRUE)
  on.exit(grDevices::dev.off(), add = TRUE)

  # Page 1: executive dashboard
  new_page("Frac Campaign Planning Report",
           "Monte Carlo campaign simulation - executive summary")
  kpi_show <- head(kpis, 9)
  accents <- rep(c(teal, navy, amber), length.out = nrow(kpi_show))
  ncol_k <- 3
  card_w <- grid::unit(0.29, "npc"); card_h <- grid::unit(0.135, "npc")
  for (i in seq_len(nrow(kpi_show))) {
    row_i <- (i - 1) %/% ncol_k
    col_i <- (i - 1) %% ncol_k
    x <- grid::unit(0.195 + col_i * 0.305, "npc")
    y <- grid::unit(0.76 - row_i * 0.165, "npc")
    draw_kpi_card(kpi_show$kpi[i], kpi_show$value[i], x, y, card_w, card_h,
                  accent = accents[i])
  }
  grid::grid.text(
    paste("Key interpretation: the best option is selected on P50 campaign duration.",
          "Readiness is a decision-support indicator and should be read together",
          "with bottlenecks, risk drivers, and cost exposure."),
    x = 0.05, y = 0.18, just = c("left", "top"),
    gp = grid::gpar(cex = 0.78, col = "grey30"))
  grid::grid.text(
    "Prepared with the uploaded historical well data, risk assumptions, and selected resource inputs.",
    x = 0.05, y = 0.12, just = c("left", "top"),
    gp = grid::gpar(cex = 0.7, col = "grey45"))

  # Page 2: campaign duration
  new_page("Campaign Duration", "Distribution of simulated outcomes")
  draw_plot_in(scurve_p, x = 0.5, y = 0.58, w = 0.9, h = 0.58)
  draw_table_in(sim_report, x = 0.5, y = 0.16, w = 0.9, h = 0.2)

  # Page 3: readiness and traffic lights
  new_page("Readiness & Traffic Lights")
  draw_plot_in(readiness_p, x = 0.27, y = 0.58, w = 0.44, h = 0.58)
  draw_table_in(traffic_report, x = 0.74, y = 0.58, w = 0.44, h = 0.55)
  draw_table_in(readiness_report, x = 0.5, y = 0.16, w = 0.92, h = 0.2)

  # Page 4: resource deployment timeline
  new_page("Resource Deployment Timeline", "Indicative sequencing of campaign resources")
  draw_plot_in(gantt_p, x = 0.5, y = 0.48, w = 0.92, h = 0.8)

  # Page 5: bottlenecks and recommended actions
  new_page("Bottlenecks & Recommended Actions")
  draw_plot_in(bottleneck_p, x = 0.5, y = 0.63, w = 0.9, h = 0.5)
  draw_table_in(recommendation_report, x = 0.5, y = 0.2, w = 0.94, h = 0.3)

  # Page 6: cost impact
  new_page("Cost Impact", "Resource deployment and idle cost estimates")
  draw_plot_in(cost_p, x = 0.5, y = 0.6, w = 0.9, h = 0.55)
  draw_table_in(head(cost_report, 12), x = 0.5, y = 0.17, w = 0.92, h = 0.26)

  # Page 7: delay contributors
  new_page("Schedule Risk Drivers", "Top delay contributors across simulations")
  draw_plot_in(delay_p, x = 0.5, y = 0.6, w = 0.9, h = 0.55)
  draw_table_in(head(delay_report, 10), x = 0.5, y = 0.17, w = 0.94, h = 0.26)

  # Page 8: notes
  new_page("Notes & Limitations")
  notes <- c(
    "This is an operational planning simulation. It is not a hydraulic fracture propagation model.",
    "Results depend on the quality of the uploaded historical data and assumptions.",
    "Cost estimates use the daily rates entered in the app and should be reviewed against contract values.",
    "Readiness scores are decision-support indicators, not deterministic go/no-go criteria.",
    "The resource deployment timeline is indicative: the simulator models workloads and capacity, not a discrete-event schedule.",
    "Wireline workload can be similar in conventional and zipper modes because the same number of stages still require plug setting, perforation, and logging. The key difference is whether wireline readiness creates frac waiting time."
  )
  y_pos <- 0.82
  for (note in notes) {
    wrapped <- strwrap(note, width = 110)
    for (ln in wrapped) {
      grid::grid.text(paste0(if (ln == wrapped[1]) "•  " else "    ", ln),
                      x = 0.06, y = y_pos, just = c("left", "top"),
                      gp = grid::gpar(cex = 0.85, col = "grey25"))
      y_pos <- y_pos - 0.045
    }
    y_pos <- y_pos - 0.02
  }

  invisible(file)
}

# ---------------------------------------------------------------------------
# NEW v12.1: Investment ranking - answers "where should I spend next?"
# Economics:
#   schedule_value      = saving_days x total daily spread rate (cost of every
#                         campaign day avoided, using the rates entered in-app)
#   incremental_cost    = added unit's day rate x resulting P50 duration
#   net_benefit         = schedule_value - incremental_cost
# These are planning-level estimates; review against contract rates.
# ---------------------------------------------------------------------------

build_investment_ranking <- function(summary, recommendations,
                                     frac_fleet_cost_per_day = 250000,
                                     wireline_cost_per_day = 15000,
                                     ct_cost_per_day = 25000,
                                     milling_cost_per_day = 18000,
                                     testing_unit_cost_per_day = 12000) {
  empty <- tibble(
    operation_mode = character(), resource = character(), proposed_change = character(),
    p50_saving_days = numeric(), new_p50_days = numeric(), incremental_unit_cost = numeric(),
    schedule_value = numeric(), net_benefit = numeric(), benefit_cost_ratio = numeric(),
    recommended_action = character()
  )
  if (is.null(recommendations) || nrow(recommendations) == 0) return(empty)
  if (is.null(summary) || nrow(summary) == 0) return(empty)

  rate_lookup <- c(
    "Frac fleet" = frac_fleet_cost_per_day,
    "Wireline" = wireline_cost_per_day,
    "CT / cleanout" = ct_cost_per_day,
    "Milling" = milling_cost_per_day,
    "Testing unit" = testing_unit_cost_per_day
  )

  mode_stats <- summary %>%
    group_by(operation_mode) %>%
    summarise(
      p50_days = quantile(estimated_campaign_days, 0.5, na.rm = TRUE),
      spread_rate =
        mean(frac_fleets, na.rm = TRUE) * frac_fleet_cost_per_day +
        mean(wireline_units, na.rm = TRUE) * wireline_cost_per_day +
        mean(ct_units, na.rm = TRUE) * ct_cost_per_day +
        mean(milling_units, na.rm = TRUE) * milling_cost_per_day,
      .groups = "drop"
    )

  recommendations %>%
    left_join(mode_stats, by = "operation_mode") %>%
    mutate(
      unit_day_rate = unname(rate_lookup[resource]),
      unit_day_rate = ifelse(is.na(unit_day_rate), 0, unit_day_rate),
      p50_saving_days = pmax(estimated_campaign_saving_days, 0),
      new_p50_days = pmax(p50_days - p50_saving_days, 0),
      incremental_unit_cost = unit_day_rate * new_p50_days,
      schedule_value = p50_saving_days * spread_rate,
      net_benefit = schedule_value - incremental_unit_cost,
      benefit_cost_ratio = ifelse(incremental_unit_cost > 0,
                                  schedule_value / incremental_unit_cost, NA_real_),
      proposed_change = paste0(resource, ": ", mean_units, " -> ", proposed_units)
    ) %>%
    filter(p50_saving_days > 0) %>%
    arrange(desc(net_benefit)) %>%
    select(
      operation_mode, resource, proposed_change, p50_saving_days, new_p50_days,
      incremental_unit_cost, schedule_value, net_benefit, benefit_cost_ratio,
      recommended_action
    )
}

# NEW v12.1: one-row narrative for the most critical bottleneck per run.
build_bottleneck_narrative <- function(bottlenecks, recommendations) {
  if (is.null(bottlenecks) || nrow(bottlenecks) == 0) {
    return(tibble(operation_mode = character(), resource = character(),
                  p90_utilization = numeric(), bottleneck_status = character(),
                  p50_saving_days = numeric(), recommended_action = character()))
  }

  top <- bottlenecks %>%
    arrange(priority, desc(p90_utilization)) %>%
    slice(1)

  saving <- 0
  action <- top$recommendation
  if (!is.null(recommendations) && nrow(recommendations) > 0) {
    match_row <- recommendations %>%
      filter(operation_mode == top$operation_mode, resource == top$resource) %>%
      slice(1)
    if (nrow(match_row) == 1) {
      saving <- match_row$estimated_campaign_saving_days
      action <- match_row$recommended_action
    }
  }

  tibble(
    operation_mode = top$operation_mode,
    resource = top$resource,
    p90_utilization = top$p90_utilization,
    bottleneck_status = top$bottleneck_status,
    p50_saving_days = saving,
    recommended_action = action
  )
}

# ---------------------------------------------------------------------------
# NEW v14: Scenario optimiser - finds the resource configuration minimising
# total mobilisation cost (all contracted units x day rate x P50 duration,
# which embeds both schedule and idle time). Two-stage:
#   Stage 1 (screen): all configs at screen_iterations with a COMMON SEED so
#     rankings compare like-for-like (common random numbers).
#   Stage 2 (refine): top_n_refine configs re-run at refine_iterations.
# Pareto flag marks configs not dominated on (P50 duration, total cost).
# ---------------------------------------------------------------------------

optimise_campaign_scenarios <- function(
    historical_wells, assumptions, n_wells,
    scenario_grid,
    fixed_args = list(),
    frac_fleet_cost_per_day = 250000,
    wireline_cost_per_day = 15000,
    ct_cost_per_day = 25000,
    milling_cost_per_day = 18000,
    testing_unit_cost_per_day = 12000,
    screen_iterations = 150,
    refine_iterations = 600,
    top_n_refine = 5,
    seed = 123,
    progress_callback = NULL
) {
  stopifnot(nrow(scenario_grid) > 0)

  required_cols <- c("operation_mode", "frac_fleets", "wireline_units", "ct_units",
                     "milling_units", "testing_units", "frac_trees", "allow_ct_for_milling")
  missing <- setdiff(required_cols, names(scenario_grid))
  if (length(missing) > 0) stop("scenario_grid missing columns: ", paste(missing, collapse = ", "))

  score_run <- function(run, cfg) {
    sm <- run$summary
    p50 <- quantile(sm$estimated_campaign_days, 0.5, na.rm = TRUE)
    p90 <- quantile(sm$estimated_campaign_days, 0.9, na.rm = TRUE)
    idle_days <- mean(sm$total_wireline_readiness_delay_days, na.rm = TRUE)
    spread_rate <- cfg$frac_fleets * frac_fleet_cost_per_day +
      cfg$wireline_units * wireline_cost_per_day +
      cfg$ct_units * ct_cost_per_day +
      cfg$milling_units * milling_cost_per_day +
      cfg$testing_units * testing_unit_cost_per_day
    tibble(
      p50_days = as.numeric(p50),
      p90_days = as.numeric(p90),
      idle_days = idle_days,
      idle_cost = idle_days * frac_fleet_cost_per_day,
      spread_rate_per_day = spread_rate,
      total_mobilisation_cost = spread_rate * as.numeric(p50)
    )
  }

  run_config <- function(cfg, n_iter) {
    args <- c(
      list(
        historical_wells = historical_wells,
        assumptions = assumptions,
        n_wells = n_wells,
        n_iterations = n_iter,
        frac_fleets = cfg$frac_fleets,
        wireline_units = cfg$wireline_units,
        ct_units = cfg$ct_units,
        milling_units = cfg$milling_units,
        testing_units = cfg$testing_units,
        frac_trees = cfg$frac_trees,
        operation_mode = cfg$operation_mode,
        allow_ct_for_milling = cfg$allow_ct_for_milling,
        seed = seed  # common random numbers across configs
      ),
      fixed_args
    )
    do.call(simulate_campaign_detailed, args)
  }

  n_cfg <- nrow(scenario_grid)
  results <- vector("list", n_cfg)

  for (i in seq_len(n_cfg)) {
    if (!is.null(progress_callback)) progress_callback(i, n_cfg, stage = "screen")
    cfg <- scenario_grid[i, , drop = FALSE]
    run <- run_config(cfg, screen_iterations)
    results[[i]] <- bind_cols(as_tibble(cfg), score_run(run, cfg)) %>%
      mutate(scenario_id = i, stage = "screened")
  }

  res <- bind_rows(results)

  # Stage 2: refine the most promising configs at full iteration count.
  refine_ids <- res %>%
    arrange(total_mobilisation_cost) %>%
    slice_head(n = min(top_n_refine, n_cfg)) %>%
    pull(scenario_id)

  for (j in seq_along(refine_ids)) {
    if (!is.null(progress_callback)) progress_callback(j, length(refine_ids), stage = "refine")
    sid <- refine_ids[j]
    cfg <- scenario_grid[sid, , drop = FALSE]
    run <- run_config(cfg, refine_iterations)
    refined <- bind_cols(as_tibble(cfg), score_run(run, cfg)) %>%
      mutate(scenario_id = sid, stage = "refined")
    res[res$scenario_id == sid, names(refined)] <- refined
  }

  # Pareto efficiency on (p50_days, total_mobilisation_cost): a config is
  # dominated if another is at least as good on both and better on one.
  res$pareto <- vapply(seq_len(nrow(res)), function(i) {
    !any(
      res$p50_days <= res$p50_days[i] &
      res$total_mobilisation_cost <= res$total_mobilisation_cost[i] &
      (res$p50_days < res$p50_days[i] |
       res$total_mobilisation_cost < res$total_mobilisation_cost[i])
    )
  }, logical(1))

  res %>%
    mutate(
      recommended = pareto & total_mobilisation_cost == min(total_mobilisation_cost[pareto]),
      fastest = p50_days == min(p50_days),
      config_label = paste0(
        operation_mode,
        " | FF:", frac_fleets, " WL:", wireline_units, " CT:", ct_units,
        " ML:", milling_units, " TU:", testing_units, " Trees:", frac_trees,
        ifelse(allow_ct_for_milling, " +CTmill", "")
      )
    ) %>%
    arrange(total_mobilisation_cost)
}

# ---------------------------------------------------------------------------
# NEW v15: Risk consequence summary - answers "are technical risks actually
# propagated through the schedule?" Direct delay vs induced workload per risk.
# ---------------------------------------------------------------------------

summarise_risk_consequences <- function(risk_event_log, summary = NULL) {
  empty <- tibble(
    operation_mode = character(), risk_event = character(), event_count = integer(),
    direct_delay_days = numeric(), induced_wireline_days = numeric(),
    induced_ct_days = numeric(), induced_milling_days = numeric(),
    induced_testing_days = numeric(), induced_frac_days = numeric(),
    total_induced_days = numeric(), total_impact_days = numeric(),
    induced_share = numeric(), expected_impact_per_campaign = numeric()
  )
  if (is.null(risk_event_log) || nrow(risk_event_log) == 0) return(empty)

  n_sims <- if (!is.null(summary) && nrow(summary) > 0) {
    summary %>% count(operation_mode, name = "n_sims")
  } else {
    risk_event_log %>% distinct(operation_mode, simulation_id) %>%
      count(operation_mode, name = "n_sims")
  }

  risk_event_log %>%
    group_by(operation_mode, risk_event) %>%
    summarise(
      event_count = n(),
      direct_delay_days = sum(delay_days, na.rm = TRUE),
      induced_wireline_days = sum(extra_wireline_days, na.rm = TRUE),
      induced_ct_days = sum(extra_ct_days, na.rm = TRUE),
      induced_milling_days = sum(extra_milling_days, na.rm = TRUE),
      induced_testing_days = sum(extra_testing_days, na.rm = TRUE),
      induced_frac_days = sum(extra_frac_days, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(n_sims, by = "operation_mode") %>%
    mutate(
      total_induced_days = induced_wireline_days + induced_ct_days +
        induced_milling_days + induced_testing_days + induced_frac_days,
      total_impact_days = direct_delay_days + total_induced_days,
      induced_share = ifelse(total_impact_days > 0,
                             total_induced_days / total_impact_days, 0),
      expected_impact_per_campaign = total_impact_days / pmax(n_sims, 1)
    ) %>%
    select(-n_sims) %>%
    arrange(desc(expected_impact_per_campaign))
}

# ---------------------------------------------------------------------------
# Constraint cascade analyser (v17)
# Answers three operational questions in order:
#   1. What is limiting me today?
#   2. What will limit me after I fix it?
#   3. Where should I spend the next dollar?
#
# Method: greedy sequential bottleneck resolution.
#   Step 0: run at current config -> identify binding constraint.
#   Step k: increment the binding resource by 1 unit -> re-run -> identify
#            next constraint. Repeat until marginal schedule saving < 
#            min_saving_days or max_steps reached.
#
# Each step records: what was fixed, days saved, cost of fix, ROI (days per
# $1M), the new constraint, and whether the fix was worthwhile.
# ---------------------------------------------------------------------------

analyse_constraint_cascade <- function(
    historical_wells,
    assumptions,
    n_wells,
    base_config,          # named list: frac_fleets, wireline_units, ct_units,
                          #             milling_units, testing_units, frac_trees,
                          #             operation_mode, allow_ct_for_milling
    fixed_args = list(),  # other simulate_campaign_detailed args (timing, etc.)
    frac_fleet_cost_per_day  = 250000,
    wireline_cost_per_day    = 15000,
    ct_cost_per_day          = 25000,
    milling_cost_per_day     = 18000,
    testing_unit_cost_per_day= 12000,
    cascade_iterations  = 300,   # fast: cascade is diagnostic not final estimate
    max_steps           = 6,
    min_saving_days     = 2,     # stop when marginal saving falls below this
    seed = 123
) {
  resource_costs <- c(
    "Frac fleet"    = frac_fleet_cost_per_day,
    "Wireline"      = wireline_cost_per_day,
    "CT / cleanout" = ct_cost_per_day,
    "Milling"       = milling_cost_per_day,
    "Testing unit"  = testing_unit_cost_per_day
  )
  resource_to_arg <- c(
    "Frac fleet"    = "frac_fleets",
    "Wireline"      = "wireline_units",
    "CT / cleanout" = "ct_units",
    "Milling"       = "milling_units",
    "Testing unit"  = "testing_units"
  )

  run_sim <- function(cfg) {
    args <- c(
      list(historical_wells = historical_wells, assumptions = assumptions,
           n_wells = n_wells, n_iterations = cascade_iterations, seed = seed),
      cfg, fixed_args
    )
    do.call(simulate_campaign_detailed, args)
  }

  score <- function(sim_result, cfg) {
    sm <- sim_result$summary
    ru <- summarise_resource_utilization(sim_result$resource_utilization)
    # Current units per resource from config
    units_map <- c(
      "Frac fleet"    = cfg$frac_fleets %||% 1,
      "Wireline"      = cfg$wireline_units %||% 1,
      "CT / cleanout" = cfg$ct_units %||% 1,
      "Milling"       = cfg$milling_units %||% 1,
      "Testing unit"  = cfg$testing_units %||% 1
    )
    bottleneck_resource <- ru %>%
      arrange(desc(p90_utilization)) %>%
      slice(1) %>%
      pull(resource)

    list(
      p50   = as.numeric(quantile(sm$estimated_campaign_days, 0.5, na.rm = TRUE)),
      p90   = as.numeric(quantile(sm$estimated_campaign_days, 0.9, na.rm = TRUE)),
      p10   = as.numeric(quantile(sm$estimated_campaign_days, 0.1, na.rm = TRUE)),
      utilization = setNames(ru$p90_utilization, ru$resource),
      bottleneck  = bottleneck_resource,
      bottleneck_util = ru$p90_utilization[ru$resource == bottleneck_resource][1],
      resource_units = units_map
    )
  }

  cfg <- base_config
  r0  <- run_sim(cfg)
  s0  <- score(r0, cfg)

  steps <- vector("list", max_steps + 1)
  steps[[1]] <- tibble(
    step                = 0L,
    action              = "Current configuration",
    resource_fixed      = NA_character_,
    units_before        = NA_integer_,
    units_after         = NA_integer_,
    p50_days            = s0$p50,
    p10_days            = s0$p10,
    p90_days            = s0$p90,
    days_saved          = 0,
    daily_rate          = NA_real_,
    incremental_cost    = NA_real_,
    schedule_value      = NA_real_,
    cost_per_day_saved  = NA_real_,
    roi_days_per_Mdollar= NA_real_,
    bottleneck_now      = s0$bottleneck,
    bottleneck_util_pct = round(s0$bottleneck_util * 100, 1),
    verdict             = "Starting point"
  )

  baseline_p50 <- s0$p50
  prev_p50     <- s0$p50
  prev_score   <- s0

  for (step in seq_len(max_steps)) {
    bn <- prev_score$bottleneck
    if (is.na(bn) || !bn %in% names(resource_to_arg)) break

    arg_name <- resource_to_arg[bn]
    units_before <- as.integer(cfg[[arg_name]] %||% 1)
    units_after  <- units_before + 1L
    cfg[[arg_name]] <- units_after

    r_new <- run_sim(cfg)
    s_new <- score(r_new, cfg)

    daily_rate <- resource_costs[bn] %||% 0
    saving     <- prev_p50 - s_new$p50
    spread_rate <- sum(mapply(function(res, units) {
      (resource_costs[res] %||% 0) * units
    }, names(prev_score$resource_units), prev_score$resource_units))
    schedule_value    <- saving * spread_rate
    incremental_cost  <- daily_rate * s_new$p50  # new unit's cost over campaign
    cost_per_day_saved <- if (saving > 0.5) incremental_cost / saving else NA_real_
    roi <- if (!is.na(cost_per_day_saved) && cost_per_day_saved > 0) {
      1e6 / cost_per_day_saved
    } else NA_real_

    verdict <- dplyr::case_when(
      saving < 0.5                     ~ "No benefit — constraint lies elsewhere",
      saving < min_saving_days         ~ "Marginal — diminishing return",
      !is.na(roi) && roi > 1           ~ sprintf("Recommended — saves %.0f days/M$ invested", roi),
      TRUE                             ~ sprintf("Consider — saves %.0f days for %s", saving,
                                           scales::dollar(incremental_cost))
    )

    steps[[step + 1]] <- tibble(
      step                = as.integer(step),
      action              = sprintf("Add 1 %s (%d \u2192 %d units)", bn, units_before, units_after),
      resource_fixed      = bn,
      units_before        = units_before,
      units_after         = units_after,
      p50_days            = s_new$p50,
      p10_days            = s_new$p10,
      p90_days            = s_new$p90,
      days_saved          = saving,
      daily_rate          = daily_rate,
      incremental_cost    = incremental_cost,
      schedule_value      = schedule_value,
      cost_per_day_saved  = cost_per_day_saved,
      roi_days_per_Mdollar= roi,
      bottleneck_now      = s_new$bottleneck,
      bottleneck_util_pct = round(s_new$bottleneck_util * 100, 1),
      verdict             = verdict
    )

    prev_p50   <- s_new$p50
    prev_score <- s_new

    if (saving < min_saving_days) break
  }

  bind_rows(Filter(Negate(is.null), steps))
}
