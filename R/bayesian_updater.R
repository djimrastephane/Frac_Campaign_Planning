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
#'   risk_event, matched, prior_prob, alpha_prior, beta_prior, n_trials, n_events,
#'   alpha_post, beta_post, posterior_mean, posterior_p05, posterior_p95,
#'   delta_prob
#'   `matched` is FALSE when obs_name had no fuzzy match in the assumptions
#'   CSV -- in that case prior_prob is a fabricated 0.05 default, not the
#'   user's real planning assumption, and callers MUST surface this rather
#'   than presenting the row as a normal result (see assess_risk_update()).
bayesian_update_risks <- function(assumptions, risk_obs, prior_strength = 20) {
  stopifnot(is.data.frame(assumptions), is.data.frame(risk_obs))

  # After load_master_assumptions(), the variable/risk-event column is "variable".
  # Support both raw and loaded forms.
  risk_col <- if ("variable" %in% names(assumptions)) "variable" else "variable / risk event"
  risk_rows <- assumptions %>%
    filter(!is.na(probability), tolower(type) == "risk") %>%
    mutate(prior_prob = as.numeric(probability))
  # scope (stage/well/campaign) drives which opportunities are comparable --
  # same convention as simulation_engine_fast.R: default to "well" when the
  # column is missing or blank.
  if ("scope" %in% names(risk_rows)) {
    risk_rows$scope <- tolower(trimws(risk_rows$scope))
    risk_rows$scope[is.na(risk_rows$scope) | risk_rows$scope == ""] <- "well"
  } else {
    risk_rows$scope <- "well"
  }
  risk_rows <- risk_rows %>% select(risk_event = all_of(risk_col), prior_prob, scope)

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

    # Match to assumptions (partial, case-insensitive). fixed() treats both
    # sides as literal substrings rather than regex patterns -- risk names
    # routinely contain characters like "(", ")", "." that are regex
    # metacharacters (e.g. "Plug failure (early)"); without fixed(), a name
    # with an unbalanced metacharacter throws a hard regex error that
    # propagates out of bayesian_update_risks() and silently nulls out the
    # ENTIRE risk update for every row in the upload (caught by the tryCatch
    # in run_bayesian_update(), surfaced only as a server-side warning()),
    # not just the row with the offending name.
    match_row <- risk_rows %>%
      filter(str_detect(tolower(risk_event), stringr::fixed(tolower(obs_name))) |
             str_detect(tolower(obs_name), stringr::fixed(tolower(risk_event)))) %>%
      slice_head(n = 1)

    matched <- nrow(match_row) > 0
    p0 <- if (matched) match_row$prior_prob else 0.05
    p0 <- pmax(0.001, pmin(0.999, p0))
    scope0 <- if (matched) match_row$scope else "well"

    # Jeffreys-adjusted Beta prior
    alpha0 <- p0 * prior_strength + 0.5
    beta0  <- (1 - p0) * prior_strength + 0.5

    alpha_post <- alpha0 + n_e
    beta_post  <- beta0  + (n_t - n_e)

    post_mean <- alpha_post / (alpha_post + beta_post)

    tibble(
      risk_event    = obs_name,
      matched       = matched,
      scope         = scope0,
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

# ---- Evidence strength & decision engine --------------------------------------
#
# Everything below this point is a POST-PROCESSING layer on top of the
# posterior numbers computed above. It does not touch any Bayesian
# calculation, posterior, or credible interval -- it only classifies and
# narrates results that already exist in duration_update / risk_update, so
# every recommendation stays traceable to those columns.
#
# This is a planning-level screening heuristic, not a formal hypothesis test.
# ALL decision thresholds live in this one block (BAYES_DECISION_THRESHOLDS)
# so the rule is auditable and adjustable in a single place -- nothing below
# this block hardcodes a number. The app's "Decision Thresholds" panel reads
# directly from this list, so the UI can never drift out of sync with the
# code that actually drives the recommendation.
#
# Thresholds are deliberately conservative (it takes a reasonably large
# sample AND a reasonably large shift before this engine will say "update
# the assumption") because false-positive assumption changes are more
# costly to a campaign plan than waiting one more well for more evidence.
# -----------------------------------------------------------------------------

BAYES_DECISION_THRESHOLDS <- list(
  # Evidence strength (applies to both duration parameters and risk events).
  # "n" is new wells for duration, trials for risk.
  min_n_for_strong_evidence   = 30,   # n >= this (+ narrow CI + consistent direction) => "Strong"
  min_n_for_moderate_evidence = 10,   # n >= this => "Moderate"; below => "Weak"

  # Risk "Update assumption" gate -- ALL four conditions below must hold.
  min_trials_for_update    = 30,      # MIN_TRIALS_FOR_UPDATE
  min_events_for_update    = 3,       # MIN_EVENTS_FOR_UPDATE
  min_posterior_shift_pp   = 0.02,    # MIN_POSTERIOR_SHIFT_PP -- 2 percentage points (0-1 scale)
  min_relative_shift       = 1.0,     # additional gate: posterior must move by >=100% relative
                                       # to the prior probability. Without this, two risks with
                                       # the same trial count and a similar absolute pp shift but
                                       # very different base rates (e.g. a small risk that roughly
                                       # doubled vs a larger risk that crept up modestly) would be
                                       # treated identically by the absolute-pp gate alone.

  # Risk "Monitor" gate -- a Weak-evidence event still needs SOME relative
  # movement to be worth watching; a near-zero shift with weak evidence
  # goes straight to "No action" instead.
  min_relative_shift_for_monitor = 0.5,

  # Credible-interval "narrow enough to be confident" cutoffs.
  duration_ci_narrow_rel = 0.5,   # duration: 90% CI width < 50% of |prior_mean|
  risk_ci_narrow_pp      = 0.05   # risk: 90% CI width < 5 percentage points
)

#' Evidence-strength hierarchy (Strong > Moderate > Weak).
#' Strong requires a decent sample AND a narrow posterior CI AND a direction
#' that is not just prior-anchoring noise (CI excludes the no-change point).
#' Moderate/Weak are governed by sample size alone -- with few observations
#' there usually isn't enough information to assess CI tightness or
#' direction reliably, so sample size is the deciding factor.
.evidence_strength <- function(n_obs, ci_narrow, direction_consistent) {
  th <- BAYES_DECISION_THRESHOLDS
  if (isTRUE(n_obs >= th$min_n_for_strong_evidence) && isTRUE(ci_narrow) && isTRUE(direction_consistent)) {
    "Strong"
  } else if (isTRUE(n_obs >= th$min_n_for_moderate_evidence)) {
    "Moderate"
  } else {
    "Weak"
  }
}

#' Convert small integers to English words for readable narrative text
#' ("Four events" rather than "4 events"). Falls back to the numeral for
#' anything above ten.
.num_word <- function(n) {
  words <- c("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")
  if (n >= 0 && n <= 10) words[n + 1] else as.character(n)
}

.cap1 <- function(s) paste0(toupper(substring(s, 1, 1)), substring(s, 2))

#' Human-readable sample-size label that names the opportunity unit implied
#' by a risk's scope ("53 stages", "1 campaign", "4 wells") -- prevents a
#' bare trial count from being read as if all risks shared the same unit.
.scope_unit_label <- function(scope, n) {
  unit <- switch(tolower(scope), stage = "stage", campaign = "campaign", "well")
  sprintf("%d %s%s", n, unit, if (n == 1) "" else "s")
}

#' Classify a duration_update tibble (output of bayesian_update_durations())
#' with evidence strength, a Stable/Meaningful-change statistical decision,
#' an engineering decision (Retain / Review / Update assumption), a 3-level
#' recommendation, and narrative text that separates the observed new-well
#' mean from the posterior estimate (no posterior-vs-prior language implies
#' an observed-data claim it doesn't support).
#'
#' Decision rule (see BAYES_DECISION_THRESHOLDS for the numbers):
#'   Retain assumption -- credible interval for the shift includes zero
#'                        (posterior shift is not statistically meaningful).
#'   Review assumption -- CI excludes zero, but evidence is only Moderate
#'                        (shift is approaching, but hasn't reached, the
#'                        strength needed to act on it).
#'   Update assumption -- CI excludes zero AND evidence is Strong.
assess_duration_update <- function(duration_update) {
  if (is.null(duration_update) || nrow(duration_update) == 0) return(duration_update)
  th <- BAYES_DECISION_THRESHOLDS

  duration_update %>%
    rowwise() %>%
    mutate(
      ci_width         = ci90_hi - ci90_lo,
      ci_excludes_zero = !(ci90_lo <= 0 & ci90_hi >= 0),
      ci_narrow        = ci_width < th$duration_ci_narrow_rel * abs(prior_mean),
      evidence_strength = .evidence_strength(n_new, ci_narrow, ci_excludes_zero),
      statistical_decision = if (ci_excludes_zero) "Meaningful change" else "Stable",
      direction = dplyr::case_when(
        !ci_excludes_zero            ~ "Stable",
        posterior_mean > prior_mean  ~ "Increasing",
        TRUE                         ~ "Decreasing"
      ),
      decision = dplyr::case_when(
        !ci_excludes_zero                               ~ "Retain assumption",
        ci_excludes_zero & evidence_strength == "Strong" ~ "Update assumption",
        TRUE                                             ~ "Review assumption"
      ),
      recommendation_level = dplyr::case_when(
        decision == "Update assumption" ~ "Recommended",
        decision == "Review assumption" ~ "Optional",
        TRUE                            ~ "Not justified"
      ),
      recommendation_text = dplyr::case_when(
        decision == "Retain assumption" ~
          "No update required - the shift is within the credible range of the new observations.",
        decision == "Review assumption" ~
          "A shift was detected but evidence is not yet Strong - collect more data before updating the assumption.",
        TRUE ~
          sprintf("Update planning assumption to %.3f d (from %.3f d).", posterior_mean, prior_mean)
      ),
      decision_reason = dplyr::case_when(
        decision == "Retain assumption" ~
          sprintf("90%% credible interval for the shift includes zero [%+.3f, %+.3f]. Evidence %s.",
                  ci90_lo, ci90_hi, tolower(evidence_strength)),
        decision == "Review assumption" ~
          sprintf("Credible interval excludes zero [%+.3f, %+.3f], but evidence is only %s (n=%d new wells).",
                  ci90_lo, ci90_hi, tolower(evidence_strength), n_new),
        TRUE ~
          sprintf("Credible interval excludes zero [%+.3f, %+.3f] with %s evidence (n=%d new wells). Update threshold met.",
                  ci90_lo, ci90_hi, tolower(evidence_strength), n_new)
      ),
      narrative_observed = sprintf(
        "New wells observed mean: %.3f d (n=%d) vs historical prior mean: %.3f d (n=%d).",
        new_mean, n_new, prior_mean, n_prior),
      narrative_posterior = sprintf(
        "The Bayesian posterior estimate shifted from %.3f d to %.3f d.", prior_mean, posterior_mean),
      narrative_interpretation = dplyr::case_when(
        evidence_strength == "Weak" ~
          sprintf("Insufficient evidence to conclude a meaningful change in %s.", tolower(label)),
        direction == "Increasing" ~ "Evidence suggests this duration may be taking longer than assumed.",
        direction == "Decreasing" ~ "Evidence suggests this duration may be completing faster than assumed.",
        TRUE ~ "Evidence is consistent with the current assumption."
      ),
      narrative_full = sprintf("%s. %s %s Recommendation: %s.",
        narrative_posterior, narrative_observed, decision_reason, tolower(decision))
    ) %>%
    ungroup()
}

#' Classify a risk_update tibble (output of bayesian_update_risks()) with
#' evidence strength, an engineering decision (Update assumption / Monitor /
#' No action), a 3-level recommendation, a one-line audit-ready Decision
#' Reason, and narrative text that distinguishes the raw observed frequency
#' (events/trials) from the Bayesian posterior estimate -- the posterior can
#' move due to prior-anchoring even when the raw observed rate hasn't
#' meaningfully changed, and the narrative must not conflate the two.
#'
#' Decision rule (see BAYES_DECISION_THRESHOLDS for the numbers):
#'   Update assumption -- evidence is Moderate or Strong, AND trials/events/
#'                        absolute shift all clear their minimums, AND the
#'                        shift is also large relative to the prior
#'                        (>= min_relative_shift) -- the relative-shift gate
#'                        is what separates a risk that has genuinely shifted
#'                        from one with a similar absolute pp move that is
#'                        small relative to its own (low) base rate.
#'   Monitor            -- a positive shift exists and evidence is Weak or
#'                        Moderate, but the Update gate wasn't fully cleared;
#'                        for Weak evidence specifically, the shift must
#'                        still be at least "indicative" (>= 50% relative)
#'                        to avoid flagging pure noise.
#'   No action          -- shift is negligible, or evidence is Weak and the
#'                        shift doesn't even clear the indicative bar.
#'
#' CHECKLIST for any new derived column added below: an unmatched row's
#' prior_prob (0.05) is a fabricated default, not a real assumption, so
#' every column that narrates or judges the prior/posterior MUST branch on
#' `!matched` first (see decision, recommendation_level, recommendation_text,
#' decision_reason, sample_caveat, narrative_interpretation, narrative_full
#' below for the existing pattern). A column that skips this guard will
#' silently present a fabricated prior as if it were real -- this has
#' already happened once (sample_caveat, fixed after a code review caught
#' it) and is the single most common way this function regresses.
assess_risk_update <- function(risk_update) {
  if (is.null(risk_update) || nrow(risk_update) == 0) return(risk_update)
  th <- BAYES_DECISION_THRESHOLDS

  risk_update %>%
    rowwise() %>%
    mutate(
      observed_freq     = if (n_trials > 0) n_events / n_trials else NA_real_,
      ci_width          = posterior_p95 - posterior_p05,
      ci_narrow         = ci_width < th$risk_ci_narrow_pp,
      ci_excludes_prior = !(posterior_p05 <= prior_prob & posterior_p95 >= prior_prob),
      rel_change = dplyr::case_when(
        prior_prob > 0          ~ abs(delta_prob) / prior_prob,
        abs(delta_prob) >= 0.01 ~ Inf,
        TRUE                    ~ 0
      ),
      magnitude_tier = dplyr::case_when(
        rel_change >= th$min_relative_shift           ~ "Significant",
        rel_change >= th$min_relative_shift_for_monitor ~ "Indicative",
        TRUE                                           ~ "Negligible"
      ),
      direction = dplyr::case_when(
        abs(delta_prob) < 1e-6 ~ "Stable",
        delta_prob > 0          ~ "Increasing",
        TRUE                    ~ "Decreasing"
      ),
      evidence_strength = .evidence_strength(n_trials, ci_narrow, ci_excludes_prior),
      meets_update_gate = evidence_strength %in% c("Strong", "Moderate") &
        n_trials >= th$min_trials_for_update &
        n_events >= th$min_events_for_update &
        abs(delta_prob) >= th$min_posterior_shift_pp &
        rel_change >= th$min_relative_shift,
      # An unmatched risk_event name (no fuzzy match in the assumptions CSV)
      # was scored against a fabricated 0.05 prior, not the user's real
      # planning assumption -- it must never look like a normal decision.
      decision = dplyr::case_when(
        !matched ~ "No assumption match",
        meets_update_gate ~ "Update assumption",
        delta_prob > 0 & evidence_strength %in% c("Weak", "Moderate") & magnitude_tier != "Negligible" ~ "Monitor",
        TRUE ~ "No action"
      ),
      recommendation_level = dplyr::case_when(
        decision == "No assumption match"                                     ~ "Not justified",
        decision == "Update assumption"                                       ~ "Recommended",
        decision == "Monitor" & evidence_strength %in% c("Moderate", "Strong") ~ "Optional",
        TRUE                                                                   ~ "Not justified"
      ),
      recommendation_text = dplyr::case_when(
        decision == "No assumption match" ~
          sprintf("No matching risk found in master_risks_assumptions.csv for '%s' -- check the spelling, or add this risk to the assumptions CSV before relying on this row.", risk_event),
        decision == "Update assumption" ~
          sprintf("Update risk assumption to %.1f%% (from %.1f%%).", 100 * posterior_mean, 100 * prior_prob),
        decision == "Monitor" & evidence_strength == "Weak" ~
          "Continue monitoring; collect more observations before updating this assumption.",
        decision == "Monitor" ~
          "Monitor closely - the shift is not yet large enough to justify an assumption change.",
        TRUE ~ "No update justified."
      ),
      # ---- Decision Reason: a short, audit-ready sentence naming exactly
      # which gate(s) the decision turned on, in the style requested for the
      # Risk Probability Summary table.
      decision_reason = dplyr::case_when(
        decision == "No assumption match" ~
          sprintf("'%s' did not match any risk in the assumptions CSV; prior_prob is a fabricated 5%% default, not a real planning assumption.", risk_event),
        decision == "Update assumption" ~ sprintf(
          "%d event%s in %d trials. Posterior %s by %+.1f pp. Evidence %s. Update threshold exceeded.",
          n_events, if (n_events == 1) "" else "s", n_trials,
          dplyr::if_else(delta_prob >= 0, "increased", "decreased"),
          100 * delta_prob, tolower(evidence_strength)),
        decision == "Monitor" & n_trials < th$min_trials_for_update & n_events <= 2 ~ sprintf(
          "Only %d event%s observed. Evidence %s despite %s shift.",
          n_events, if (n_events == 1) "" else "s", tolower(evidence_strength),
          dplyr::if_else(abs(100 * delta_prob) >= 4, "large", "a modest")),
        decision == "Monitor" ~ sprintf(
          "Posterior %s by %+.1f pp. Evidence %s. Continue monitoring.",
          dplyr::if_else(delta_prob >= 0, "increased", "decreased"), 100 * delta_prob, tolower(evidence_strength)),
        TRUE ~ sprintf(
          "Shift %s and evidence %s.",
          dplyr::if_else(abs(delta_prob) < th$min_posterior_shift_pp, "below threshold", "modest"),
          tolower(evidence_strength))
      ),
      narrative_observed = dplyr::case_when(
        n_trials < th$min_n_for_moderate_evidence ~ "Observed data insufficient to draw a conclusion.",
        observed_freq > prior_prob ~ sprintf(
          "Observed frequency (%.1f%%, %d/%d) exceeded the planning assumption (%.1f%%).",
          100 * observed_freq, n_events, n_trials, 100 * prior_prob),
        observed_freq < prior_prob ~ sprintf(
          "Observed frequency (%.1f%%, %d/%d) was below the planning assumption (%.1f%%).",
          100 * observed_freq, n_events, n_trials, 100 * prior_prob),
        TRUE ~ sprintf(
          "Observed frequency (%.1f%%, %d/%d) matched the planning assumption.",
          100 * observed_freq, n_events, n_trials)
      ),
      narrative_posterior = sprintf(
        "The Bayesian posterior estimate shifted from %.1f%% to %.1f%%.", 100 * prior_prob, 100 * posterior_mean),
      # Explains the counterintuitive case an engineer will otherwise flag as
      # "wrong": a Weak-evidence posterior necessarily stays anchored near
      # the prior, even when the observed frequency looks very different
      # (or, as with 0 events, looks nothing like the posterior at all).
      # !matched takes priority: that prior is a fabricated default, not a
      # real assumption anchoring anything, so the "stays close to the
      # prior" framing below would be actively misleading for those rows.
      sample_caveat = dplyr::case_when(
        !matched ~ sprintf(
          "No matching assumption for '%s' -- this prior is a fabricated default, not a real planning assumption.",
          risk_event
        ),
        evidence_strength == "Weak" ~ sprintf(
          "Posterior remains close to the prior because only %s %s available.",
          .scope_unit_label(scope, n_trials),
          if (n_trials == 1) "observation was" else "observations were"
        ),
        TRUE ~ NA_character_
      ),
      narrative_interpretation = dplyr::case_when(
        !matched ~
          sprintf("'%s' was not found in master_risks_assumptions.csv -- no real prior to compare against.", risk_event),
        evidence_strength == "Weak" ~
          sprintf("Insufficient evidence to conclude a change in %s frequency.", tolower(risk_event)),
        direction == "Increasing" ~ "Evidence suggests the event may be occurring more frequently than assumed.",
        direction == "Decreasing" ~ "Evidence suggests the event may be occurring less frequently than assumed.",
        TRUE ~ "Evidence is consistent with the current assumption."
      ),
      narrative_full = if (!matched) {
        sprintf(
          "%s: no matching risk found in master_risks_assumptions.csv. The %.1f%% prior shown is a fabricated default, not this campaign's real planning assumption -- check the spelling in the risk observations CSV or add this risk to the assumptions CSV.",
          risk_event, 100 * prior_prob)
      } else {
        sprintf(
          "%s shifted from %.1f%% to %.1f%%. %s event%s %s observed across %d opportunit%s. Evidence is %s%s. Recommendation: %s.",
          risk_event, 100 * prior_prob, 100 * posterior_mean,
          .cap1(.num_word(n_events)), if (n_events == 1) "" else "s", if (n_events == 1) "was" else "were",
          n_trials, if (n_trials == 1) "y" else "ies",
          tolower(evidence_strength),
          dplyr::case_when(
            decision == "Update assumption" ~ " and exceeds the update threshold",
            decision == "Monitor"           ~ ", which is not yet enough to update the assumption",
            TRUE                            ~ " and the shift is below the update threshold"
          ),
          tolower(decision))
      }
    ) %>%
    ungroup()
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
