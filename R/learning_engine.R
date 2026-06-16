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
        Converged    = f$converged,
        Best         = i == 1
      )
    }))

    list(
      parameter      = key,
      label          = label,
      raw_values     = x,
      desc           = desc,
      fits           = fits,
      fit_table      = fit_table,
      best_fit       = best,
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
    tibble(
      Parameter           = r$label,
      `Best fit`          = r$best_fit$family,
      `Suggested min (d)` = r$suggested_min,
      `Suggested mode (d)` = r$suggested_mode,
      `Suggested max (d)` = r$suggested_max,
      `Basis`             = "P5 / mode / P95 of best-fit distribution"
    )
  }))
}
