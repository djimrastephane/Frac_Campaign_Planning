# optimiser_explain.R
# -----------------------------------------------------------------------------
# Phases 2-4 of the optimiser auditability pass: binding-path visibility,
# automated scenario explanations, and exact-tie grouping. All pure helper
# functions -- no simulation, no scenario generation, no Pareto/recommendation
# logic here. Consumed by R/optimiser_cascade.R and R/optimiser_parallel.R's
# score_run()/score() (must be sourced BEFORE those two files) and by
# app/app.R for the results-table "Binding path"/"Why" columns and the tied-
# scenario view.
#
# Dependencies (source first): constants.R (OPTIMISER_* thresholds).
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({ library(dplyr) })

# ---- Phase 2: binding-path classification -----------------------------------
#
# A scenario's estimated_campaign_days = max(frac-path days, post-frac
# completion days) (R/engine_core.R). Per iteration, whichever side equals
# that max is "binding" for that iteration. We don't have frac-path days as
# a standalone stored column (see docs/architecture.md diagram 1), but we
# don't need it: post-frac binds iff post_frac_completion_days is (within
# floating-point tolerance) equal to estimated_campaign_days; otherwise the
# frac path binds. This is the exact test optimise_campaign_scenarios()'s
# eligibility screen already uses internally (see the `frac_path_binds`
# variable in this file's score_run() below) -- Phase 2 exposes it as a
# named, documented, per-scenario percentage instead of an internal boolean.
.BINDING_TOL <- 1e-6

#' Per-iteration binding side for a completed simulation `summary`.
#' Returns a logical vector, TRUE where the post-frac path bound.
binding_path_per_iteration <- function(summary) {
  stopifnot(all(c("post_frac_completion_days", "estimated_campaign_days") %in% names(summary)))
  summary$post_frac_completion_days >= summary$estimated_campaign_days - .BINDING_TOL
}

#' Classifies a scenario's dominant binding path from the fraction of
#' iterations where each side bound. Named constants (constants.R):
#'   post_frac_bind_pct >= OPTIMISER_BINDING_DOMINANT_THRESHOLD -> "Post-frac path"
#'   frac_path_bind_pct >= OPTIMISER_BINDING_DOMINANT_THRESHOLD -> "Frac path"
#'   |post_frac_bind_pct - 0.5| <= OPTIMISER_BINDING_TIE_BAND    -> "Tied"
#'   otherwise                                                    -> "Mixed"
classify_binding_path <- function(frac_path_bind_pct, post_frac_bind_pct,
                                   dominant_threshold = OPTIMISER_BINDING_DOMINANT_THRESHOLD,
                                   tie_band = OPTIMISER_BINDING_TIE_BAND) {
  vapply(seq_along(post_frac_bind_pct), function(i) {
    ppct <- post_frac_bind_pct[i]; fpct <- frac_path_bind_pct[i]
    if (is.na(ppct) || is.na(fpct)) return(NA_character_)
    if (ppct >= dominant_threshold) "Post-frac path"
    else if (fpct >= dominant_threshold) "Frac path"
    else if (abs(ppct - 0.5) <= tie_band) "Tied"
    else "Mixed"
  }, character(1))
}

#' Computes the three Phase-2 fields (frac_path_bind_pct, post_frac_bind_pct,
#' binding_path_primary) for one completed scenario run, from its `summary`.
#' Used by both optimiser_cascade.R and optimiser_parallel.R's score
#' functions -- the single place this computation is defined.
summarise_binding_path <- function(summary) {
  post_binds <- binding_path_per_iteration(summary)
  post_pct <- mean(post_binds, na.rm = TRUE)
  frac_pct <- 1 - post_pct
  list(
    frac_path_bind_pct = frac_pct,
    post_frac_bind_pct = post_pct,
    binding_path_primary = classify_binding_path(frac_pct, post_pct)
  )
}

#' Per-resource P90 utilization for one completed scenario run, from its
#' resource_utilization (reuses summarise_resource_utilization(), the same
#' function the Resources tab and bottleneck logic already use -- one source
#' of truth for "what counts as utilization" -- see R/summaries.R). Returns
#' a named vector, one entry per resource actually present.
summarise_scenario_utilization <- function(resource_utilization) {
  su <- summarise_resource_utilization(resource_utilization)
  if (nrow(su) == 0) return(setNames(numeric(0), character(0)))
  setNames(su$p90_utilization, su$resource)
}

# ---- Phase 3: automated scenario explanations --------------------------------
#
# Evidence scope, stated plainly: explanations here are generated entirely
# from fields optimise_campaign_scenarios() already computes (P50/P90/cost,
# Phase 2 binding-path shares, per-resource P90 utilization) -- comparing a
# scenario against its nearest single-resource grid neighbor (the row with
# one resource decremented by exactly 1, everything else identical). This
# deliberately does NOT report literal per-unit assignment counts or exact
# peak concurrency (e.g. "the 3rd milling unit was assigned 0 wells" or
# "max simultaneous demand was 2 units") -- producing those would require
# simulate_campaign_detailed() to return raw per-well schedule data it
# doesn't expose today (see engine_core.R's schedule_post_frac_milling()),
# and adding that would either change simulate_campaign_detailed()'s return
# schema (breaking check_regression.R's bit-identical `summary` comparison)
# or force collect_well_details = TRUE on every scenario (a real performance
# regression across up to 400 grid configs). A measured null result --
# "P50 did not move and utilization barely moved" -- is still rigorous
# evidence for "this addition did not help," so that's what's reported.
# See docs/architecture.md and the root-cause investigation this follows up
# on for the full reasoning.

# arg column -> (label, P90-utilization column) used to compare grid neighbors.
# Priority order for "which single resource explains this row" when more than
# one neighbor exists: testing/milling first, since the investigation found
# the post-frac (testing/milling-gated) path is the one most often binding in
# this model's default assumptions -- an ordering choice for READABILITY only,
# it does not affect which neighbors are found, only which is reported first
# when a row happens to differ from more than one immediate neighbor.
.OPT_RESOURCE_META <- list(
  testing_units  = list(label = "testing unit",  util_col = "p90_util_testing"),
  milling_units  = list(label = "milling unit",  util_col = "p90_util_milling"),
  wireline_units = list(label = "wireline unit", util_col = "p90_util_wireline"),
  ct_units       = list(label = "CT unit",       util_col = "p90_util_ct"),
  frac_fleets    = list(label = "frac fleet",    util_col = "p90_util_frac_fleet")
)
.OPT_GRID_COLS <- names(.OPT_RESOURCE_META)

#' Finds the nearest single-resource neighbor of `results[i, ]`: the row in
#' `results` with exactly one of the 5 resource columns one unit lower and
#' every other grid dimension (mode, other resources, frac_trees,
#' allow_ct_for_milling) identical. Returns NULL if none of the 5 candidate
#' decrements exists in `results` (i.e. this row is already at the grid's
#' floor on every axis that's present).
find_single_resource_parent <- function(results, i) {
  row <- results[i, ]
  for (arg in .OPT_GRID_COLS) {
    if (is.null(row[[arg]]) || row[[arg]] <= 1) next
    candidate_val <- row[[arg]] - 1
    match_cols <- setdiff(c("operation_mode", .OPT_GRID_COLS, "frac_trees", "allow_ct_for_milling"), arg)
    match_cols <- intersect(match_cols, names(results))
    is_match <- rep(TRUE, nrow(results))
    for (mc in match_cols) is_match <- is_match & (results[[mc]] == row[[mc]])
    is_match <- is_match & (results[[arg]] == candidate_val)
    parent_idx <- which(is_match)
    if (length(parent_idx) >= 1) {
      return(list(resource_arg = arg, parent = results[parent_idx[1], ]))
    }
  }
  NULL
}

#' Generates one evidence-backed explanation for `results[i, ]`.
#'
#' Returns a list: `type` (one of "tie", "never_used", "queue_only",
#' "governing_relieved", "negligible", "baseline"), `short` (manager-length
#' sentence for the results-table "Why" column), `detail` (named list of the
#' measured evidence backing `short`, for an expandable tooltip).
#'
#' @param results Completed optimise_campaign_scenarios() output (all rows,
#'   so neighbors/ties can be found within it).
#' @param i       Row index into `results` to explain.
explain_optimiser_scenario <- function(results, i) {
  row <- results[i, ]

  # Tie check first (Phase 4): unambiguous, doesn't need an adjacency search.
  same_mode <- results[results$operation_mode == row$operation_mode, ]
  tie_rows <- same_mode[abs(same_mode$p50_days - row$p50_days) <= OPTIMISER_TIE_EPS, ]
  if (nrow(tie_rows) > 1) {
    cheaper <- tie_rows[tie_rows$total_mobilisation_cost < row$total_mobilisation_cost - 1e-6, ]
    if (nrow(cheaper) > 0) {
      cheapest <- cheaper[which.min(cheaper$total_mobilisation_cost), ]
      cost_diff_M <- (row$total_mobilisation_cost - cheapest$total_mobilisation_cost) / 1e6
      return(list(
        type = "tie",
        short = sprintf("Same P50 (%.2f d) as %s, but $%.1fM more expensive.",
                        row$p50_days, cheapest$config_label, cost_diff_M),
        detail = list(
          baseline = cheapest$config_label,
          p50_delta_days = row$p50_days - cheapest$p50_days,
          cost_delta_M = cost_diff_M,
          tie_group_size = nrow(tie_rows)
        )
      ))
    }
    # This row IS the cheapest in its tie group -- not a dominated tie, fall
    # through to the normal single-resource-neighbor explanation below.
  }

  nb <- find_single_resource_parent(results, i)
  if (is.null(nb)) {
    return(list(
      type = "baseline",
      short = "Baseline configuration in the search grid -- no single-resource comparison available.",
      detail = list()
    ))
  }
  meta <- .OPT_RESOURCE_META[[nb$resource_arg]]
  parent <- nb$parent
  p50_delta <- parent$p50_days - row$p50_days   # positive = adding the unit helped
  p90_delta <- parent$p90_days - row$p90_days
  util_col <- meta$util_col
  util_before <- if (util_col %in% names(parent)) parent[[util_col]] else NA_real_
  util_after  <- if (util_col %in% names(row))    row[[util_col]]    else NA_real_
  util_drop <- if (!is.na(util_before) && !is.na(util_after)) util_before - util_after else NA_real_

  meaningfully_used <- !is.na(util_drop) && util_drop >= OPTIMISER_UNIT_USE_UTIL_DROP_PP
  improved <- !is.na(p50_delta) && p50_delta >= OPTIMISER_NEGLIGIBLE_DAYS
  # The PARENT (pre-addition) scenario's dominant binding path -- computed
  # once by Phase 2 (summarise_binding_path()/classify_binding_path()) and
  # read directly here, rather than re-derived from which side the resource
  # sits on: whichever side actually governed is a fact about the parent
  # scenario, independent of which resource we're evaluating.
  parent_dominant <- parent$binding_path_primary
  parent_dominant_pct <- dplyr::case_when(
    is.na(parent_dominant) ~ NA_real_,
    parent_dominant == "Post-frac path" ~ parent$post_frac_bind_pct,
    parent_dominant == "Frac path" ~ parent$frac_path_bind_pct,
    TRUE ~ pmax(parent$frac_path_bind_pct, parent$post_frac_bind_pct, na.rm = TRUE)
  )

  if (!improved && !meaningfully_used && !is.na(util_drop)) {
    return(list(
      type = "never_used",
      short = sprintf(
        "No measurable benefit. P50 unchanged (%+.2f d) and utilization barely moved (%s) after adding a %s -- the extra capacity was not meaningfully used.",
        p50_delta, if (is.na(util_drop)) "n/a" else sprintf("%+.1f pp", -100 * util_drop), meta$label),
      detail = list(baseline = parent$config_label, resource = meta$label,
                    p50_delta_days = p50_delta, p90_delta_days = p90_delta,
                    utilization_drop_pp = if (is.na(util_drop)) NA_real_ else 100 * util_drop,
                    binding_path_in_baseline = parent_dominant)
    ))
  }

  if (!improved && meaningfully_used) {
    return(list(
      type = "queue_only",
      short = sprintf(
        "Utilization dropped %.1f pp, but the %s controlled completion in %.0f%% of simulated runs, so P50 changed by only %.2f d.",
        100 * util_drop, tolower(parent_dominant %||% "other path"),
        100 * (if (is.na(parent_dominant_pct)) 0 else parent_dominant_pct), p50_delta),
      detail = list(baseline = parent$config_label, resource = meta$label,
                    p50_delta_days = p50_delta, p90_delta_days = p90_delta,
                    utilization_drop_pp = 100 * util_drop,
                    binding_path_in_baseline = parent_dominant)
    ))
  }

  if (improved) {
    return(list(
      type = "governing_relieved",
      short = sprintf(
        "This is on the governing path (%s bound in %.0f%% of the prior configuration's runs); adding a %s reduced P50 by %.2f d.",
        tolower(parent_dominant %||% "the campaign path"),
        100 * (if (is.na(parent_dominant_pct)) 0 else parent_dominant_pct),
        meta$label, p50_delta),
      detail = list(baseline = parent$config_label, resource = meta$label,
                    p50_delta_days = p50_delta, p90_delta_days = p90_delta,
                    utilization_drop_pp = if (is.na(util_drop)) NA_real_ else 100 * util_drop,
                    binding_path_in_baseline = parent_dominant)
    ))
  }

  # Reached only when !improved && !meaningfully_used && is.na(util_drop) --
  # i.e. P50 didn't move AND we have no utilization evidence for this
  # resource (missing from resource_utilization for this scenario). Weaker
  # claim than "never_used" on purpose: we can state the P50 result but not
  # assert anything about whether the unit was used, since we lack that
  # evidence here.
  list(
    type = "negligible",
    short = sprintf("No measurable campaign improvement (%+.2f d) within the configured tolerance (%.1f d).",
                    p50_delta, OPTIMISER_NEGLIGIBLE_DAYS),
    detail = list(baseline = parent$config_label, resource = meta$label,
                  p50_delta_days = p50_delta, p90_delta_days = p90_delta)
  )
}

#' Vectorised convenience: adds a `why` character column to `results` (short
#' explanation only -- call explain_optimiser_scenario() per-row for detail).
annotate_optimiser_explanations <- function(results) {
  if (is.null(results) || nrow(results) == 0) return(results)
  results$why <- vapply(seq_len(nrow(results)), function(i) {
    explain_optimiser_scenario(results, i)$short
  }, character(1))
  results
}

# ---- Phase 4: exact-tie grouping ---------------------------------------------
#
#' Groups scenarios with (near-)identical P50 within `operation_mode` using
#' full-precision values (OPTIMISER_TIE_EPS days), NOT rounded display
#' values. Does not touch pareto/recommended/fastest -- those are computed
#' independently by optimise_campaign_scenarios() and are asserted unchanged
#' by R/test_optimiser_ties.R. No rows are removed; every row keeps its tie
#' group membership so full exports remain complete.
#'
#' Adds columns: tie_group_id, tie_group_size, is_tie_representative
#' (cheapest member of its group -- TRUE for singleton groups too).
group_optimiser_ties <- function(results, eps = OPTIMISER_TIE_EPS) {
  if (is.null(results) || nrow(results) == 0) return(results)
  results$tie_group_id <- NA_integer_
  next_id <- 1L
  for (mode in unique(results$operation_mode)) {
    idx <- which(results$operation_mode == mode & is.na(results$tie_group_id))
    while (length(idx) > 0) {
      anchor <- idx[1]
      grp <- idx[abs(results$p50_days[idx] - results$p50_days[anchor]) <= eps]
      results$tie_group_id[grp] <- next_id
      next_id <- next_id + 1L
      idx <- setdiff(idx, grp)
    }
  }
  results %>%
    group_by(tie_group_id) %>%
    mutate(
      tie_group_size = dplyr::n(),
      is_tie_representative = total_mobilisation_cost == min(total_mobilisation_cost)
    ) %>%
    ungroup()
}
