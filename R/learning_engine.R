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

    # KS_ADEQUACY_ALPHA: the standard 0.05 significance level for a KS
    # goodness-of-fit test. p < 0.05 means we can reject "the data came from
    # this distribution" -- i.e. the fit is not statistically adequate, just
    # the least bad of the candidates tried. Named here so the UI wording
    # below and the fit table's "Adequate" column can't drift apart.
    KS_ADEQUACY_ALPHA <- 0.05

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
        `Adequate (p>=0.05)` = ifelse(is.na(f$ks_pvalue), NA, f$ks_pvalue >= KS_ADEQUACY_ALPHA),
        Converged    = f$converged,
        Best         = i == 1
      )
    }))

    # No candidate distribution is a statistically adequate fit -- the KS
    # test rejects all of them at p<0.05. The "best" one is still useful as
    # a planning input, but it's the least-bad candidate among those tested,
    # not "the correct distribution" for this data.
    best_is_adequate <- !is.na(best$ks_pvalue) && best$ks_pvalue >= KS_ADEQUACY_ALPHA

    list(
      parameter      = key,
      label          = label,
      raw_values     = x,
      desc           = desc,
      fits           = fits,
      fit_table      = fit_table,
      best_fit       = best,
      best_is_adequate = best_is_adequate,
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
        `Best fit` = "N/A",
        `Suggested min (d)` = NA_real_,
        `Suggested mode (d)` = NA_real_,
        `Suggested max (d)` = NA_real_,
        `Basis` = r$note %||% "Insufficient data"
      ))
    }
    # KS p-value < 0.05 means the KS test rejects this distribution as the
    # data's true generating distribution -- so it's the least-bad
    # candidate among those tested, not a confirmed correct fit. Say so
    # explicitly here since this table is what users copy into
    # master_risks_assumptions.csv.
    basis <- if (isTRUE(r$best_is_adequate)) {
      sprintf("P5 / mode / P95 of best-fit distribution (KS p=%.3f, not rejected at p<0.05)",
              r$best_fit$ks_pvalue)
    } else {
      sprintf("P5 / mode / P95 of the LEAST-BAD candidate (KS p=%.3f -- rejected at p<0.05; no tested distribution is a statistically adequate fit)",
              r$best_fit$ks_pvalue)
    }
    tibble(
      Parameter           = r$label,
      `Best fit`          = if (isTRUE(r$best_is_adequate)) r$best_fit$family
                             else paste0(r$best_fit$family, " (least-bad)"),
      `Suggested min (d)` = r$suggested_min,
      `Suggested mode (d)` = r$suggested_mode,
      `Suggested max (d)` = r$suggested_max,
      `Basis`             = basis
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
