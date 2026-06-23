# learning_engine.R
# Issue #6: Historical Campaign Learning Engine
#
# Automatically derives operational duration distributions from uploaded
# historical well data. Fits Normal, Lognormal, Gamma, and Weibull distributions
# to each duration parameter using MLE (via MASS::fitdistr), ranks candidates
# by AIC, and generates suggested triangular planning assumptions (P5/mode/P95)
# from the best-fitting distribution.
#
# Dependencies (source first): simulation_engine[_fast].R
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(stringr)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

# Duration parameters extracted from historical_wells
.LE_PARAMS <- list(
  frac_days_per_stage   = "Frac stage duration (d/stage)",
  milling_days_per_plug = "Milling duration (d/plug)"
)

.DIST_FAMILIES <- c("Normal", "Lognormal", "Gamma", "Weibull")

# ---- Fit quality wording -------------------------------------------------
# Single named source for the KS p-value cutoffs behind "Fit quality" and its
# accompanying note, so the wording in the fit table, the suggested-
# assumptions table, and the status summary can never drift apart.
#   Good     : p >= 0.10 -- not rejected by the KS test at the usual 0.05
#              level, with some margin.
#   Moderate : 0.01 <= p < 0.10 -- formally rejected (or borderline) but not
#              by a wide margin; still a reasonable planning input.
#   Poor     : p < 0.01 -- strongly rejected; none of the 4 candidates
#              describe this data well.
FIT_QUALITY_THRESHOLDS <- list(good = 0.10, moderate = 0.01)

.fit_quality_label <- function(ks_pvalue) {
  dplyr::case_when(
    is.na(ks_pvalue)                                 ~ NA_character_,
    ks_pvalue >= FIT_QUALITY_THRESHOLDS$good          ~ "Good",
    ks_pvalue >= FIT_QUALITY_THRESHOLDS$moderate      ~ "Moderate",
    TRUE                                               ~ "Poor"
  )
}

#' Plain-language note for a given fit quality tier. Never claims the
#' selected distribution is "correct" -- only that it's the most suitable
#' of the candidates actually evaluated.
.fit_quality_note <- function(quality, ks_pvalue) {
  if (is.na(quality)) return("Fit quality unavailable.")
  if (quality == "Good") {
    return(sprintf("Selected distribution fits the historical data well (KS p=%.3f).", ks_pvalue))
  }
  if (quality == "Moderate") {
    return(paste0(
      sprintf("No tested distribution perfectly matches the historical data (KS p=%.3f). ", ks_pvalue),
      "The selected distribution is the most suitable candidate for planning ",
      "and simulation among those evaluated."))
  }
  # Poor
  paste0(
    sprintf("None of the tested distributions adequately describe the historical data (KS p=%.3f). ", ks_pvalue),
    "The selected distribution is still the most suitable candidate evaluated, but treat the ",
    "suggested planning range with extra caution and consider gathering more historical data.")
}

# ---- Descriptive statistics --------------------------------------------------

.desc_stats <- function(x) {
  x <- x[!is.na(x) & x > 0]
  if (length(x) < 2) return(NULL)
  tibble(
    n      = length(x),
    mean   = mean(x),
    sd     = sd(x),
    cv     = sd(x) / mean(x),
    min    = min(x),
    p10    = as.numeric(quantile(x, 0.10)),
    p25    = as.numeric(quantile(x, 0.25)),
    p50    = as.numeric(quantile(x, 0.50)),
    p75    = as.numeric(quantile(x, 0.75)),
    p90    = as.numeric(quantile(x, 0.90)),
    max    = max(x),
    skew   = mean(((x - mean(x)) / sd(x))^3),
    kurt   = mean(((x - mean(x)) / sd(x))^4) - 3
  )
}

# ---- Distribution fitting (MLE) ----------------------------------------------

.fit_one <- function(x, family) {
  x <- x[!is.na(x) & x > 0]
  n <- length(x)
  tryCatch({
    fit <- switch(family,
      Normal   = MASS::fitdistr(x, "normal"),
      Lognormal = MASS::fitdistr(x, "lognormal"),
      Gamma    = MASS::fitdistr(x, "gamma"),
      Weibull  = MASS::fitdistr(x, "weibull")
    )
    ll  <- as.numeric(logLik(fit))
    k   <- length(fit$estimate)
    aic <- -2 * ll + 2 * k
    bic <- -2 * ll + k * log(n)

    # KS test against fitted distribution
    ks <- switch(family,
      Normal    = ks.test(x, "pnorm",    fit$estimate["mean"], fit$estimate["sd"]),
      Lognormal = ks.test(x, "plnorm",   fit$estimate["meanlog"], fit$estimate["sdlog"]),
      Gamma     = ks.test(x, "pgamma",   fit$estimate["shape"], fit$estimate["rate"]),
      Weibull   = ks.test(x, "pweibull", fit$estimate["shape"], fit$estimate["scale"])
    )

    # Mode and P5/P95 from fitted distribution
    params <- as.list(fit$estimate)
    mode_val <- switch(family,
      Normal    = params$mean,
      Lognormal = exp(params$meanlog - params$sdlog^2),
      Gamma     = max(0, (params$shape - 1) / params$rate),
      Weibull   = if (params$shape > 1) params$scale * ((params$shape-1)/params$shape)^(1/params$shape) else 0
    )
    p5  <- switch(family,
      Normal    = qnorm(0.05,  params$mean, params$sd),
      Lognormal = qlnorm(0.05, params$meanlog, params$sdlog),
      Gamma     = qgamma(0.05, params$shape, params$rate),
      Weibull   = qweibull(0.05, params$shape, params$scale)
    )
    p95 <- switch(family,
      Normal    = qnorm(0.95,  params$mean, params$sd),
      Lognormal = qlnorm(0.95, params$meanlog, params$sdlog),
      Gamma     = qgamma(0.95, params$shape, params$rate),
      Weibull   = qweibull(0.95, params$shape, params$scale)
    )

    list(
      family     = family,
      params     = params,
      loglik     = ll,
      aic        = aic,
      bic        = bic,
      ks_stat    = ks$statistic,
      ks_pvalue  = ks$p.value,
      mode       = max(0, mode_val),
      p5         = max(0, p5),
      p95        = max(0, p95),
      converged  = TRUE
    )
  }, error = function(e) {
    list(family = family, params = list(), loglik = -Inf, aic = Inf, bic = Inf,
         ks_stat = NA_real_, ks_pvalue = NA_real_,
         mode = NA_real_, p5 = NA_real_, p95 = NA_real_, converged = FALSE)
  })
}

# Fit all candidate families and rank by AIC.
.fit_all <- function(x) {
  fits <- lapply(.DIST_FAMILIES, function(f) .fit_one(x, f))
  names(fits) <- .DIST_FAMILIES
  fits[order(sapply(fits, `[[`, "aic"))]  # ranked best → worst
}

# ---- Density function helpers ------------------------------------------------

.dfun <- function(x_seq, family, params) {
  tryCatch(switch(family,
    Normal    = dnorm(x_seq,   params$mean,    params$sd),
    Lognormal = dlnorm(x_seq,  params$meanlog, params$sdlog),
    Gamma     = dgamma(x_seq,  params$shape,   params$rate),
    Weibull   = dweibull(x_seq, params$shape,  params$scale)
  ), error = function(e) rep(NA_real_, length(x_seq)))
}

.qfun <- function(p, family, params) {
  tryCatch(switch(family,
    Normal    = qnorm(p,    params$mean,    params$sd),
    Lognormal = qlnorm(p,   params$meanlog, params$sdlog),
    Gamma     = qgamma(p,   params$shape,   params$rate),
    Weibull   = qweibull(p, params$shape,   params$scale)
  ), error = function(e) rep(NA_real_, length(p)))
}

# ---- Main entry point --------------------------------------------------------

#' Learn duration distributions from historical well data.
#'
#' @param historical_wells  Tibble with frac_days_per_stage, milling_days_per_plug.
#'
#' @return List per parameter, each containing:
#'   raw_values, desc (descriptive stats tibble), fits (named list of fit objects,
#'   ranked by AIC), fit_table (tibble for display), best_fit (top-ranked fit),
#'   suggested_min, suggested_mode, suggested_max (triangular assumption params).
learn_from_historical <- function(historical_wells) {
  stopifnot(is.data.frame(historical_wells))

  lapply(names(.LE_PARAMS), function(key) {
    label <- .LE_PARAMS[[key]]
    x     <- historical_wells[[key]]
    x     <- x[!is.na(x) & x > 0]

    if (length(x) < 5) {
      return(list(
        parameter = key, label = label, raw_values = x,
        desc = NULL, fits = NULL, fit_table = NULL,
        best_fit = NULL,
        suggested_min = NA_real_, suggested_mode = NA_real_, suggested_max = NA_real_,
        note = sprintf("Only %d valid values — need ≥5 to fit distributions.", length(x))
      ))
    }

    desc <- .desc_stats(x)
    fits <- .fit_all(x)
    best <- fits[[1]]  # lowest AIC

    fit_table <- bind_rows(lapply(seq_along(fits), function(i) {
      f <- fits[[i]]
      tibble(
        Rank         = i,
        Distribution = f$family,
        logLik       = round(f$loglik, 2),
        AIC          = round(f$aic, 2),
        BIC          = round(f$bic, 2),
        `KS stat`    = round(f$ks_stat, 4),
        `KS p-value` = round(f$ks_pvalue, 4),
        `Fit quality` = .fit_quality_label(f$ks_pvalue),
        Converged    = f$converged,
        Best         = i == 1
      )
    }))

    best_fit_quality <- .fit_quality_label(best$ks_pvalue)
    best_fit_note     <- .fit_quality_note(best_fit_quality, best$ks_pvalue)

    list(
      parameter      = key,
      label          = label,
      raw_values     = x,
      desc           = desc,
      fits           = fits,
      fit_table      = fit_table,
      best_fit       = best,
      fit_quality    = best_fit_quality,
      fit_quality_note = best_fit_note,
      suggested_min  = round(best$p5,   3),
      suggested_mode = round(best$mode, 3),
      suggested_max  = round(best$p95,  3),
      note           = NULL
    )
  }) %>% setNames(names(.LE_PARAMS))
}

# ---- Suggested assumptions table ---------------------------------------------

#' Format suggested triangular assumption parameters from learning results.
suggested_assumptions_table <- function(learning) {
  bind_rows(lapply(learning, function(r) {
    if (is.null(r$best_fit)) {
      return(tibble(
        Parameter = r$label,
        `Selected planning distribution` = "N/A",
        `Fit quality` = "N/A",
        `Suggested min (d)` = NA_real_,
        `Suggested mode (d)` = NA_real_,
        `Suggested max (d)` = NA_real_,
        `Note` = r$note %||% "Insufficient data"
      ))
    }
    # The note never claims the selected distribution is "correct" -- only
    # that it's the most suitable candidate among those actually evaluated.
    # Same wording the status summary and fit table use (.fit_quality_note()),
    # so this table -- the one users copy into master_risks_assumptions.csv --
    # can't say something different from the rest of the tab.
    tibble(
      Parameter                        = r$label,
      `Selected planning distribution` = r$best_fit$family,
      `Fit quality`                    = r$fit_quality,
      `Suggested min (d)`              = r$suggested_min,
      `Suggested mode (d)`             = r$suggested_mode,
      `Suggested max (d)`              = r$suggested_max,
      `Note`                           = r$fit_quality_note
    )
  }))
}

# ---- Outlier well summary -----------------------------------------------------

#' Summarise outlier wells for one historical-wells duration metric.
#'
#' Flags wells whose value on `metric` sits above the `p_threshold` quantile
#' of the uploaded/synthetic historical set, and attempts to explain why
#' using only the columns actually present in historical_wells.csv. There is
#' no risk-event or "reason" field in that schema, so this is a best-effort
#' heuristic over the other recorded columns (stage overrun, contingency
#' plugs used, cement-eval/milling time well above the typical well) -- it
#' never asserts a specific cause (e.g. "screenout") the data can't support.
#' Falls back to asking the user to investigate when no recorded column
#' stands out, rather than fabricating a more specific reason.
summarise_outlier_wells <- function(historical_wells, metric = "frac_days_per_stage", p_threshold = 0.95) {
  stopifnot(is.data.frame(historical_wells), metric %in% names(historical_wells))

  df <- historical_wells %>%
    dplyr::filter(!is.na(.data[[metric]]), .data[[metric]] > 0)

  empty_outliers <- tibble(well_id = character(), value = numeric(), possible_reason = character())
  if (nrow(df) < 5) {
    return(list(
      n_wells = nrow(df), p50 = NA_real_, p90 = NA_real_, max = NA_real_,
      threshold = NA_real_, outliers = empty_outliers,
      note = sprintf("Only %d valid well(s) for this metric -- need >=5 to characterise outliers.", nrow(df))
    ))
  }

  x <- df[[metric]]
  p50 <- as.numeric(quantile(x, 0.50, na.rm = TRUE))
  p90 <- as.numeric(quantile(x, 0.90, na.rm = TRUE))
  threshold <- as.numeric(quantile(x, p_threshold, na.rm = TRUE))
  max_val <- max(x, na.rm = TRUE)

  outlier_rows <- df %>%
    dplyr::filter(.data[[metric]] > threshold) %>%
    dplyr::arrange(dplyr::desc(.data[[metric]]))

  has_stages_planned <- "stages_planned" %in% names(df)
  med_cement  <- stats::median(df$cement_eval_days, na.rm = TRUE)
  med_milling <- stats::median(df$milling_days, na.rm = TRUE)

  .factor_text <- function(row) {
    factors <- character(0)
    if (has_stages_planned &&
        !is.na(row$stages_completed) && !is.na(row$stages_planned) &&
        row$stages_completed > row$stages_planned) {
      factors <- c(factors, sprintf("%d more stage(s) completed than planned",
                                     row$stages_completed - row$stages_planned))
    }
    if (!is.na(row$contingency_plugs) && row$contingency_plugs > 0) {
      factors <- c(factors, sprintf("%d contingency plug(s) used", row$contingency_plugs))
    }
    if (!is.na(row$cement_eval_days) && !is.na(med_cement) && med_cement > 0 &&
        row$cement_eval_days > 1.5 * med_cement) {
      factors <- c(factors, sprintf("cement evaluation took %.1fx the typical well",
                                     row$cement_eval_days / med_cement))
    }
    if (!is.na(row$milling_days) && !is.na(med_milling) && med_milling > 0 &&
        row$milling_days > 1.5 * med_milling) {
      factors <- c(factors, sprintf("milling took %.1fx the typical well",
                                     row$milling_days / med_milling))
    }
    if (length(factors) == 0) {
      paste0("No contributing factor found in historical_wells.csv's recorded columns -- ",
             "recommend investigating this well's field records directly (daily reports, ",
             "screenout/intervention logs) to confirm the cause.")
    } else {
      paste0("Possible contributing factor(s) from the data: ", paste(factors, collapse = "; "),
             ". Inferred from available columns, not a confirmed cause -- verify against field records.")
    }
  }

  outliers <- outlier_rows
  if (nrow(outliers) > 0) {
    outliers$possible_reason <- vapply(seq_len(nrow(outliers)),
                                        function(i) .factor_text(outliers[i, ]), character(1))
  } else {
    outliers$possible_reason <- character(0)
  }
  outliers <- outliers %>%
    dplyr::transmute(well_id, value = .data[[metric]], possible_reason)

  list(
    n_wells = nrow(df), p50 = p50, p90 = p90, max = max_val,
    threshold = threshold, outliers = outliers, note = NULL
  )
}
