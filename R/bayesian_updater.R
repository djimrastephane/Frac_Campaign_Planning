# bayesian_updater.R
# Issue #7: Bayesian Duration Updating
#
# Combines historical priors with newly observed campaign data to produce
# updated duration estimates and risk probabilities.
#
# DURATION UPDATE — Normal conjugate (known-variance, uninformative prior)
# -------------------------------------------------------------------------
# Prior:  empirical distribution of historical_wells (frac_days_per_stage,
#         milling_days_per_plug).
# New data: additional completed wells in the same CSV format.
# Posterior: precision-weighted combination of prior and new sample means;
#   predictive P10/P50/P90 drawn from the posterior-predictive Normal.
#   90% credible interval for the shift in the population mean.
#
# RISK PROBABILITY UPDATE — Beta-Binomial conjugate
# -------------------------------------------------------------------------
# Prior:  Beta(α₀, β₀) constructed from the assumptions-CSV probability plus
#   a user-controlled prior_strength (equivalent prior sample size).
# New data: CSV with columns risk_event, n_trials, n_events.
# Posterior: Beta(α₀+k, β₀+(n-k)); posterior mean and 90% credible interval.
#
# Dependencies (source first): simulation_engine[_fast].R
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(stringr)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- Duration update ---------------------------------------------------------

#' Normal-conjugate Bayesian update for continuous duration parameters.
#'
#' @param historical_wells  Tibble loaded from historical_wells.csv (the prior).
#' @param new_wells         Tibble of newly observed wells (same column format).
#'
#' @return Tibble with one row per parameter:
#'   parameter, label, n_prior, prior_mean, prior_sd, prior_p10, prior_p50,
#'   prior_p90, n_new, new_mean, new_sd, n_posterior, posterior_mean,
#'   posterior_sd_pred, posterior_p10, posterior_p50, posterior_p90,
#'   delta_mean, ci90_lo, ci90_hi
bayesian_update_durations <- function(historical_wells, new_wells) {
  stopifnot(is.data.frame(historical_wells), is.data.frame(new_wells))

  params <- list(
    frac_days_per_stage   = list(label = "Frac stage duration (d/stage)",  col = "frac_days_per_stage"),
    milling_days_per_plug = list(label = "Milling duration (d/plug)",       col = "milling_days_per_plug")
  )

  bind_rows(lapply(names(params), function(key) {
    meta   <- params[[key]]
    col    <- meta$col
    prior_vals <- historical_wells[[col]]
    prior_vals <- prior_vals[!is.na(prior_vals) & prior_vals > 0]
    new_vals   <- new_wells[[col]]
    new_vals   <- new_vals[!is.na(new_vals) & new_vals > 0]

    n0  <- length(prior_vals)
    n1  <- length(new_vals)
    if (n0 < 2) return(NULL)

    mu0 <- mean(prior_vals);  s0 <- sd(prior_vals)
    mu1 <- if (n1 > 0) mean(new_vals) else NA_real_
    s1  <- if (n1 > 1) sd(new_vals)   else NA_real_

    # Posterior mean (precision-weighted; shared variance assumed = s0)
    n_post  <- n0 + n1
    mu_post <- if (n1 > 0) (n0 * mu0 + n1 * mu1) / n_post else mu0

    # Posterior variance of the mean (standard error)
    se_post <- s0 / sqrt(n_post)

    # Posterior predictive SD for a single new observation
    sd_pred <- sqrt(s0^2 + se_post^2)
    sd_pred <- max(sd_pred, 1e-6)

    # 90% CI for the shift in the population mean
    delta   <- mu_post - mu0
    ci_lo   <- delta - qnorm(0.95) * se_post
    ci_hi   <- delta + qnorm(0.95) * se_post

    tibble(
      parameter       = key,
      label           = meta$label,
      n_prior         = n0,
      prior_mean      = mu0,
      prior_sd        = s0,
      prior_p10       = as.numeric(quantile(prior_vals, 0.10)),
      prior_p50       = as.numeric(quantile(prior_vals, 0.50)),
      prior_p90       = as.numeric(quantile(prior_vals, 0.90)),
      n_new           = n1,
      new_mean        = mu1,
      new_sd          = s1,
      n_posterior     = n_post,
      posterior_mean  = mu_post,
      posterior_sd_pred = sd_pred,
      posterior_p10   = mu_post + qnorm(0.10) * sd_pred,
      posterior_p50   = mu_post,
      posterior_p90   = mu_post + qnorm(0.90) * sd_pred,
      delta_mean      = delta,
      ci90_lo         = ci_lo,
      ci90_hi         = ci_hi
    )
  }))
}

# ---- Risk probability update -------------------------------------------------

#' Beta-Binomial Bayesian update for risk event probabilities.
#'
#' @param assumptions     Tibble from master_risks_assumptions.csv.
#' @param risk_obs        Tibble with columns: risk_event (chr), n_trials (int),
#'                        n_events (int). One row per risk event to update.
#' @param prior_strength  Equivalent prior sample size (default 20). Higher
#'                        values mean the assumptions-CSV probability is more
#'                        strongly anchored; lower values let new data dominate.
#'
#' @return Tibble with one row per risk event in risk_obs:
#'   risk_event, prior_prob, alpha_prior, beta_prior, n_trials, n_events,
#'   alpha_post, beta_post, posterior_mean, posterior_p05, posterior_p95,
#'   delta_prob
bayesian_update_risks <- function(assumptions, risk_obs, prior_strength = 20) {
  stopifnot(is.data.frame(assumptions), is.data.frame(risk_obs))

  # After load_master_assumptions(), the variable/risk-event column is "variable".
  # Support both raw and loaded forms.
  risk_col <- if ("variable" %in% names(assumptions)) "variable" else "variable / risk event"
  risk_rows <- assumptions %>%
    filter(!is.na(probability), tolower(type) == "risk") %>%
    select(risk_event = all_of(risk_col), prior_prob = probability) %>%
    mutate(prior_prob = as.numeric(prior_prob))

  risk_obs <- risk_obs %>%
    mutate(
      n_trials = as.integer(n_trials),
      n_events = as.integer(n_events),
      n_events = pmin(n_events, n_trials)
    )

  bind_rows(lapply(seq_len(nrow(risk_obs)), function(i) {
    obs_name <- risk_obs$risk_event[i]
    n_t      <- risk_obs$n_trials[i]
    n_e      <- risk_obs$n_events[i]

    # Match to assumptions (partial, case-insensitive)
    match_row <- risk_rows %>%
      filter(str_detect(tolower(risk_event), tolower(obs_name)) |
             str_detect(tolower(obs_name), tolower(risk_event))) %>%
      slice_head(n = 1)

    p0 <- if (nrow(match_row) > 0) match_row$prior_prob else 0.05
    p0 <- pmax(0.001, pmin(0.999, p0))

    # Jeffreys-adjusted Beta prior
    alpha0 <- p0 * prior_strength + 0.5
    beta0  <- (1 - p0) * prior_strength + 0.5

    alpha_post <- alpha0 + n_e
    beta_post  <- beta0  + (n_t - n_e)

    post_mean <- alpha_post / (alpha_post + beta_post)

    tibble(
      risk_event    = obs_name,
      prior_prob    = p0,
      alpha_prior   = alpha0,
      beta_prior    = beta0,
      n_trials      = n_t,
      n_events      = n_e,
      alpha_post    = alpha_post,
      beta_post     = beta_post,
      posterior_mean = post_mean,
      posterior_p05  = qbeta(0.05, alpha_post, beta_post),
      posterior_p95  = qbeta(0.95, alpha_post, beta_post),
      delta_prob     = post_mean - p0
    )
  }))
}

# ---- Master entry point ------------------------------------------------------

#' Run a complete Bayesian update combining duration and risk updating.
#'
#' @param historical_wells  Tibble of prior wells.
#' @param new_wells         Tibble of newly observed wells (same format).
#' @param assumptions       Tibble from assumptions CSV (for risk priors).
#'                          NULL if risk updating not desired.
#' @param risk_obs          Tibble with (risk_event, n_trials, n_events).
#'                          NULL to skip risk updating.
#' @param prior_strength    Beta prior equivalent sample size (default 20).
#'
#' @return List: duration_update (tibble), risk_update (tibble or NULL),
#'   merged_wells (historical + new combined, for sim feed-through),
#'   n_prior, n_new, prior_strength.
run_bayesian_update <- function(
    historical_wells,
    new_wells,
    assumptions   = NULL,
    risk_obs      = NULL,
    prior_strength = 20L
) {
  stopifnot(is.data.frame(historical_wells), is.data.frame(new_wells))
  if (nrow(new_wells) == 0) stop("new_wells has no rows.")

  dur <- bayesian_update_durations(historical_wells, new_wells)

  risk <- if (!is.null(assumptions) && !is.null(risk_obs) && nrow(risk_obs) > 0) {
    tryCatch(
      bayesian_update_risks(assumptions, risk_obs, prior_strength),
      error = function(e) { warning("Risk update failed: ", conditionMessage(e)); NULL }
    )
  } else NULL

  # Merge wells: combined pool feeds the simulation bootstrap directly.
  # Column-align: keep only shared columns.
  shared_cols <- intersect(names(historical_wells), names(new_wells))
  merged <- bind_rows(
    historical_wells[, shared_cols],
    new_wells[, shared_cols]
  )

  list(
    duration_update = dur,
    risk_update     = risk,
    merged_wells    = merged,
    n_prior         = nrow(historical_wells),
    n_new           = nrow(new_wells),
    prior_strength  = as.integer(prior_strength)
  )
}

# ---- Risk observation CSV loader ---------------------------------------------

#' Load a risk observations CSV (risk_event, n_trials, n_events).
load_risk_observations <- function(path) {
  df <- read.csv(path, comment.char = "#", stringsAsFactors = FALSE,
                 strip.white = TRUE)
  names(df) <- tolower(gsub("[^a-z0-9_]", "_", names(df)))
  # Accept flexible column naming
  if (!"risk_event" %in% names(df) && "risk" %in% names(df))  df$risk_event <- df$risk
  if (!"n_trials"   %in% names(df) && "trials" %in% names(df)) df$n_trials <- df$trials
  if (!"n_events"   %in% names(df) && "events" %in% names(df)) df$n_events <- df$events
  req_cols <- c("risk_event", "n_trials", "n_events")
  missing  <- setdiff(req_cols, names(df))
  if (length(missing) > 0)
    stop(sprintf("Risk observations CSV must contain: %s", paste(missing, collapse = ", ")))
  df %>%
    select(risk_event, n_trials, n_events) %>%
    filter(!is.na(risk_event), nzchar(trimws(risk_event))) %>%
    mutate(n_trials = as.integer(n_trials), n_events = as.integer(n_events))
}
