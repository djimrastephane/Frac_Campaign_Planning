# whatif_builder.R
# Issue #11: What-If Scenario Builder
#
# Runs the base config plus up to N user-defined alternative configurations
# in parallel and returns a compact, comparison-ready result set.
#
# Each "variant" is a named list of parameter overrides applied on top of the
# base args (any key not listed inherits the base value).  Typical overrides:
#   list(frac_fleets = 2)
#   list(operation_mode = "Zipper", wireline_units = 2)
#   list(frac_fleets = 2, wireline_units = 2, ct_units = 2)
#
# Dependencies (source first): simulation_engine[_fast].R, optimiser_parallel.R,
# bottleneck_explain.R, recommendations.R
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- Internal helpers --------------------------------------------------------

.wif_spread_rate <- function(resource_utilization, mode, rates) {
  units_df <- resource_utilization %>%
    filter(operation_mode == mode) %>%
    group_by(resource) %>%
    summarise(units = mean(units, na.rm = TRUE), .groups = "drop")
  sum(units_df$units * unname(rates[units_df$resource]), na.rm = TRUE)
}

.wif_run_one <- function(args, label, rates) {
  res  <- do.call(simulate_campaign_detailed, args)
  mode <- args$operation_mode
  sm   <- res$summary[res$summary$operation_mode == mode, ]

  p10 <- as.numeric(quantile(sm$estimated_campaign_days, 0.10, na.rm = TRUE))
  p50 <- as.numeric(quantile(sm$estimated_campaign_days, 0.50, na.rm = TRUE))
  p90 <- as.numeric(quantile(sm$estimated_campaign_days, 0.90, na.rm = TRUE))

  rd   <- build_readiness_score(res$summary, res$risk_event_log, res$resource_utilization)
  rd_m <- rd[rd$operation_mode == mode, ]

  spread <- .wif_spread_rate(res$resource_utilization, mode, rates)

  list(
    label           = label,
    operation_mode  = mode,
    p10_days        = p10,
    p50_days        = p50,
    p90_days        = p90,
    readiness_score = rd_m$readiness_score[1],
    readiness_status = rd_m$readiness_status[1],
    bottleneck      = rd_m$non_frac_bottleneck[1],
    spread_rate     = spread,
    est_cost        = spread * p50,
    duration        = sm$estimated_campaign_days
  )
}

# ---- Main entry point --------------------------------------------------------

#' Run base config + a list of named variants in parallel.
#'
#' @param base_args   Simulation arg list (from sim_results()$args_by_mode).
#' @param variants    Named list of override lists, e.g.
#'   list("2 Frac Fleets" = list(frac_fleets = 2), "Zipper" = list(operation_mode = "Zipper")).
#'   An empty or NULL variants list runs base only.
#' @param n_iterations  Iterations per scenario (default 300).
#' @param frac_fleet_cost_per_day / wireline_cost_per_day / ... Day rates for cost estimate.
#' @param n_cores       Parallel workers.
#'
#' @return List: `scenarios` (list of result records), `comparison` (wide tibble).
run_whatif_batch <- function(
    base_args,
    variants            = list(),
    n_iterations        = 300L,
    frac_fleet_cost_per_day    = 250000,
    wireline_cost_per_day      = 15000,
    ct_cost_per_day            = 25000,
    milling_cost_per_day       = 18000,
    testing_unit_cost_per_day  = 12000,
    n_cores = max(1L, parallel::detectCores() - 1L)
) {
  stopifnot(is.list(base_args))
  if (is.null(variants)) variants <- list()

  rates <- c(
    "Frac fleet"    = frac_fleet_cost_per_day,
    "Wireline"      = wireline_cost_per_day,
    "CT / cleanout" = ct_cost_per_day,
    "Milling"       = milling_cost_per_day,
    "Testing unit"  = testing_unit_cost_per_day
  )

  # Build clean base args (strip UI callbacks, fix iteration count)
  clean <- function(a, overrides = list()) {
    a$progress_callback   <- NULL
    a$keep_logs           <- FALSE
    a$collect_well_details <- FALSE
    a$n_iterations        <- as.integer(n_iterations)
    for (k in names(overrides)) a[[k]] <- overrides[[k]]
    a
  }

  jobs <- c(
    list(list(label = "Base", args = clean(base_args))),
    lapply(seq_along(variants), function(i) {
      lbl <- names(variants)[i]
      if (is.null(lbl) || !nzchar(lbl)) lbl <- sprintf("Variant %d", i)
      list(label = lbl, args = clean(base_args, variants[[i]]))
    })
  )

  raw <- .par_lapply(seq_along(jobs), function(i) {
    tryCatch(
      .wif_run_one(jobs[[i]]$args, jobs[[i]]$label, rates),
      error = function(e) list(label = jobs[[i]]$label, error = conditionMessage(e))
    )
  }, n_cores = n_cores)

  # Drop errored runs but surface a warning
  ok <- vapply(raw, function(r) is.null(r$error), logical(1))
  if (any(!ok)) {
    failed <- sapply(raw[!ok], `[[`, "label")
    warning(sprintf("What-if run failed for: %s", paste(failed, collapse = ", ")))
  }
  scenarios <- raw[ok]

  # Attach deltas relative to base (first scenario)
  base_p50  <- scenarios[[1]]$p50_days
  base_cost <- scenarios[[1]]$est_cost
  scenarios <- lapply(scenarios, function(s) {
    s$delta_p50  <- s$p50_days  - base_p50
    s$delta_cost <- s$est_cost  - base_cost
    s
  })

  # Comparison tibble (one row per scenario)
  comparison <- bind_rows(lapply(scenarios, function(s) {
    tibble(
      Scenario        = s$label,
      Mode            = s$operation_mode,
      `P10 (d)`       = round(s$p10_days, 1),
      `P50 (d)`       = round(s$p50_days, 1),
      `P90 (d)`       = round(s$p90_days, 1),
      `Δ P50 vs base` = sprintf("%+.1f d", s$delta_p50),
      `Est. cost ($M)`= round(s$est_cost / 1e6, 2),
      `Δ cost vs base`= sprintf("%+.2f M", s$delta_cost / 1e6),
      `Readiness`     = sprintf("%.0f (%s)", s$readiness_score, s$readiness_status),
      `Bottleneck`    = s$bottleneck %||% "—"
    )
  }))

  list(scenarios = scenarios, comparison = comparison)
}
