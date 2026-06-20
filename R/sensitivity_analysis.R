# sensitivity_analysis.R
# Issue #8: Sensitivity Analysis Engine
#
# Identifies the variables that contribute most to campaign duration uncertainty
# by running OAT (one-at-a-time) perturbation sweeps across three variable classes:
#
#   SCALAR  — numeric sidebar params (timing, efficiency scalars).  Perturbed ±scalar_perturb_pct.
#   RISK    — per-event probabilities in the assumptions table.      Perturbed ±risk_perturb_pct.
#   RESOURCE— integer resource counts.                               Perturbed ±1 unit.
#
# Supports multi-mode runs: pass args for both Conventional and Zipper to get a
# split tornado comparing how sensitive each mode is to each driver.
#
# Dependencies (source first): simulation_engine[_fast].R, optimiser_parallel.R
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(stringr)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- Parameter registries ----------------------------------------------------

.SA_SCALARS <- list(
  frac_time_per_stage_hours     = list(label = "Frac stage cycle time",    category = "Timing"),
  wireline_time_per_stage_hours = list(label = "Wireline stage time",       category = "Timing"),
  frac_settling_time_hours      = list(label = "Frac settling time",        category = "Timing"),
  wireline_contingency_pct      = list(label = "Wireline contingency %",    category = "Timing"),
  pad_to_pad_move_hours         = list(label = "Pad-to-pad move time",      category = "Timing"),
  well_to_well_transition_hours = list(label = "Well-to-well transition",   category = "Timing"),
  risk_multiplier               = list(label = "Risk multiplier (all)",     category = "Risk"),
  zipper_efficiency             = list(label = "Zipper efficiency factor",  category = "Operations"),
  ct_milling_efficiency         = list(label = "CT milling efficiency",     category = "Operations")
)

.SA_RISK_EVENTS <- list(
  weather_delay      = list(label = "Weather delay prob.",            category = "Risk", pattern = "(?i)weather"),
  permit_delay       = list(label = "Permit delay prob.",             category = "Risk", pattern = "(?i)permit"),
  equipment_downtime = list(label = "Equipment downtime prob.",       category = "Risk", pattern = "(?i)equipment"),
  screenout          = list(label = "Screenout prob.",                category = "Risk", pattern = "(?i)screenout"),
  plug_failure       = list(label = "Plug pressure test failure prob.", category = "Risk", pattern = "(?i)plug pressure"),
  premature_plug     = list(label = "Premature plug prob.",           category = "Risk", pattern = "(?i)premature plug")
)

.SA_RESOURCES <- list(
  frac_fleets    = list(label = "Frac fleets (±1 unit)",    category = "Resource"),
  wireline_units = list(label = "Wireline units (±1 unit)", category = "Resource"),
  milling_units  = list(label = "Milling units (±1 unit)",  category = "Resource"),
  ct_units       = list(label = "CT units (±1 unit)",       category = "Resource")
)

# ---- Internal helpers --------------------------------------------------------

.sa_run_one <- function(args) {
  res  <- do.call(simulate_campaign_detailed, args)
  mode <- args$operation_mode
  sm   <- res$summary[res$summary$operation_mode == mode, ]
  tibble(
    p50_days = as.numeric(quantile(sm$estimated_campaign_days, 0.50, na.rm = TRUE)),
    p90_days = as.numeric(quantile(sm$estimated_campaign_days, 0.90, na.rm = TRUE))
  )
}

.perturb_risk_prob <- function(assumptions, pattern, direction, perturb_pct) {
  assumptions %>%
    mutate(probability = if_else(
      str_detect(risk_event, regex(pattern, ignore_case = TRUE)),
      pmin(1, pmax(0, probability * (1 + direction * perturb_pct))),
      probability
    ))
}

# ---- Main entry point --------------------------------------------------------

#' Run a full sensitivity sweep for one or more operation modes.
#'
#' @param args_by_mode Named list of sim arg lists, keyed by operation_mode
#'   (e.g. list(Conventional = ..., Zipper = ...)).
#' @param scalar_perturb_pct Fractional swing for timing/scalar params (default 0.20).
#' @param risk_perturb_pct   Fractional swing for risk event probabilities (default 0.50).
#' @param n_iterations       Reduced iteration count per perturbed run.
#' @param n_cores            Parallel workers.
#'
#' @return List with: base (tibble), detail (tibble), summary (tibble),
#'   ranking (tibble), scalars: scalar_perturb_pct, risk_perturb_pct, modes,
#'   n_iterations.
run_sensitivity_analysis <- function(
    args_by_mode,
    scalar_perturb_pct = 0.20,
    risk_perturb_pct   = 0.50,
    n_iterations       = 200L,
    n_cores            = max(1L, parallel::detectCores() - 1L),
    progress_callback  = NULL
) {
  stopifnot(is.list(args_by_mode), length(args_by_mode) >= 1)
  modes <- names(args_by_mode)

  # Strip slow/UI callbacks; fix iteration count
  base_args_list <- lapply(modes, function(m) {
    a <- args_by_mode[[m]]
    a$progress_callback   <- NULL
    a$keep_logs           <- FALSE
    a$collect_well_details <- FALSE
    a$n_iterations        <- as.integer(n_iterations)
    a
  })
  names(base_args_list) <- modes

  # Base P50/P90 per mode
  base_list <- lapply(modes, function(m) {
    r <- .sa_run_one(base_args_list[[m]])
    r$operation_mode <- m
    r
  })
  base_df <- bind_rows(base_list)

  # Build job table: variable × direction × mode
  scalar_jobs   <- expand.grid(var = names(.SA_SCALARS),     direction = c(-1L, 1L), mode = modes, type = "scalar",   stringsAsFactors = FALSE)
  risk_jobs     <- expand.grid(var = names(.SA_RISK_EVENTS), direction = c(-1L, 1L), mode = modes, type = "risk",     stringsAsFactors = FALSE)
  resource_jobs <- expand.grid(var = names(.SA_RESOURCES),   direction = c(-1L, 1L), mode = modes, type = "resource", stringsAsFactors = FALSE)
  all_jobs      <- bind_rows(scalar_jobs, risk_jobs, resource_jobs)

  n_sa_jobs <- nrow(all_jobs)
  .sa_job <- function(i) {
    row  <- all_jobs[i, ]
    v    <- row$var
    dir  <- as.integer(row$direction)
    m    <- row$mode
    type <- row$type
    args <- base_args_list[[m]]
    skip <- FALSE

    if (type == "scalar") {
      base_val <- as.numeric(args[[v]] %||% NA_real_)
      if (is.na(base_val) || base_val == 0) { skip <- TRUE } else {
        args[[v]] <- base_val * (1 + dir * scalar_perturb_pct)
      }
    } else if (type == "risk") {
      pat     <- .SA_RISK_EVENTS[[v]]$pattern
      matches <- str_detect(args$assumptions$risk_event %||% character(0), regex(pat, ignore_case = TRUE))
      if (!any(matches, na.rm = TRUE)) { skip <- TRUE } else {
        args$assumptions <- .perturb_risk_prob(args$assumptions, pat, dir, risk_perturb_pct)
      }
    } else if (type == "resource") {
      base_val <- as.integer(args[[v]] %||% 1L)
      new_val  <- base_val + dir
      if (new_val < 1L) { skip <- TRUE } else {
        args[[v]] <- new_val
      }
    }

    if (skip) {
      return(tibble(var = v, direction = dir, operation_mode = m, type = type,
                    p50_days = NA_real_, p90_days = NA_real_, skipped = TRUE))
    }
    out <- .sa_run_one(args)
    out$var           <- v
    out$direction     <- dir
    out$operation_mode <- m
    out$type          <- type
    out$skipped       <- FALSE
    out
  }

  if (!is.null(progress_callback)) {
    job_results <- lapply(seq_len(n_sa_jobs), function(i) {
      out <- .sa_job(i)
      progress_callback(i, n_sa_jobs)
      out
    })
  } else {
    job_results <- .par_lapply(seq_len(n_sa_jobs), .sa_job, n_cores = n_cores)
  }

  detail <- bind_rows(job_results)

  # ---- Summary table: one row per (variable × mode) -------------------------
  meta_df <- bind_rows(
    bind_rows(lapply(names(.SA_SCALARS), function(v) tibble(
      variable = v, label = .SA_SCALARS[[v]]$label, category = .SA_SCALARS[[v]]$category, type = "scalar"))),
    bind_rows(lapply(names(.SA_RISK_EVENTS), function(v) tibble(
      variable = v, label = .SA_RISK_EVENTS[[v]]$label, category = .SA_RISK_EVENTS[[v]]$category, type = "risk"))),
    bind_rows(lapply(names(.SA_RESOURCES), function(v) tibble(
      variable = v, label = .SA_RESOURCES[[v]]$label, category = .SA_RESOURCES[[v]]$category, type = "resource")))
  )

  summary_rows <- bind_rows(lapply(meta_df$variable, function(v) {
    bind_rows(lapply(modes, function(m) {
      base_p50 <- base_df$p50_days[base_df$operation_mode == m]
      low_row  <- detail[detail$var == v & detail$direction == -1L & detail$operation_mode == m, ]
      high_row <- detail[detail$var == v & detail$direction ==  1L & detail$operation_mode == m, ]
      if (nrow(low_row) == 0 || nrow(high_row) == 0) return(NULL)
      # If both directions skipped, variable is irrelevant for this mode
      if (isTRUE(low_row$skipped[1]) && isTRUE(high_row$skipped[1])) return(NULL)
      low_skipped  <- isTRUE(low_row$skipped[1])
      high_skipped <- isTRUE(high_row$skipped[1])
      low_p50  <- if (low_skipped)  NA_real_ else low_row$p50_days[1]
      high_p50 <- if (high_skipped) NA_real_ else high_row$p50_days[1]
      meta <- meta_df[meta_df$variable == v, ]
      tibble(
        variable       = v,
        label          = meta$label,
        category       = meta$category,
        type           = meta$type,
        operation_mode = m,
        base_p50       = base_p50,
        low_p50        = low_p50,
        high_p50       = high_p50,
        low_skipped    = low_skipped,
        high_skipped   = high_skipped,
        low_delta      = if (low_skipped)  NA_real_ else low_p50  - base_p50,
        high_delta     = if (high_skipped) NA_real_ else high_p50 - base_p50,
        swing          = abs((if (high_skipped) base_p50 else high_p50) -
                              (if (low_skipped)  base_p50 else low_p50))
      )
    }))
  }))

  # Contribution % within each mode (how much each variable accounts for total swing)
  summary_rows <- summary_rows %>%
    group_by(operation_mode) %>%
    mutate(contribution_pct = {
      tot <- sum(swing, na.rm = TRUE)
      if (tot > 0) 100 * swing / tot else rep(0, dplyr::n())
    }) %>%
    ungroup()

  # Overall ranking: average swing across modes, descending
  ranking <- summary_rows %>%
    group_by(variable, label, category, type) %>%
    summarise(mean_swing = mean(swing, na.rm = TRUE),
              max_swing  = max(swing, na.rm = TRUE),
              .groups = "drop") %>%
    arrange(desc(mean_swing)) %>%
    mutate(rank = row_number())

  list(
    base               = base_df,
    detail             = detail,
    summary            = summary_rows,
    ranking            = ranking,
    scalar_perturb_pct = scalar_perturb_pct,
    risk_perturb_pct   = risk_perturb_pct,
    modes              = modes,
    n_iterations       = as.integer(n_iterations)
  )
}
