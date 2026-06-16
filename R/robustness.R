# robustness.R
# -----------------------------------------------------------------------------
# "Recommendation robustness" check (V2.5-style decision support add-on).
#
# The Monte Carlo already samples within-run uncertainty for every assumption
# row (triangle_sample on min/most_likely/max). What it does NOT cover is
# fixed sidebar scalars: what if the planning assumption itself (stage cycle
# time, risk multiplier, zipper efficiency, ...) is off by +-15%? Does the
# recommended action (and the readiness verdict) survive that, or does it flip?
#
# This is a one-at-a-time (OAT) sensitivity sweep: perturb each candidate
# assumption +-perturb_pct, re-run a reduced-iteration simulation (common seed,
# parallelised via .par_lapply from optimiser_parallel.R), and recompute the
# readiness score + analytic recommendation for each perturbed run.
#
# Dependencies (source first): simulation_engine[_fast].R, optimiser_parallel.R,
# bottleneck_explain.R, recommendations.R
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

ROBUSTNESS_LABELS <- c(
  frac_time_per_stage_hours     = "Frac stage cycle time (hrs)",
  wireline_time_per_stage_hours = "Wireline stage time (hrs)",
  risk_multiplier                = "Risk multiplier",
  zipper_efficiency               = "Zipper efficiency factor",
  wireline_contingency_pct         = "Wireline contingency (%)"
)

# Runs one simulation and reduces it to the handful of numbers the robustness
# table needs: campaign duration, readiness verdict, and recommended action.
.robustness_run_one <- function(args, rec_cost_args) {
  res <- do.call(simulate_campaign_detailed, args)
  mode <- args$operation_mode

  rd <- build_readiness_score(res$summary, res$risk_event_log, res$resource_utilization)
  rd_m <- rd[rd$operation_mode == mode, ]

  rec <- do.call(recommend_action,
                  c(list(sim_result = res, sim_args = args, verify = FALSE), rec_cost_args))

  sm <- res$summary[res$summary$operation_mode == mode, ]

  tibble(
    p50_days = as.numeric(quantile(sm$estimated_campaign_days, 0.50, na.rm = TRUE)),
    p90_days = as.numeric(quantile(sm$estimated_campaign_days, 0.90, na.rm = TRUE)),
    readiness_score = rd_m$readiness_score[1],
    readiness_status = rd_m$readiness_status[1],
    recommendation = rec$recommendation,
    bottleneck = rec$bottleneck
  )
}

# Sweeps each `perturb_params` entry +-perturb_pct around its current sidebar
# value, holding everything else (including seed) fixed, and compares the
# resulting readiness score / recommended action against the base case.
#
# `sim_args` is the exact args list used to produce the focus-mode result
# (sim_results()$args_by_mode[[focus_mode]] in app.R) -- it already contains
# historical_wells, assumptions, n_wells, operation_mode, seed, etc.
assess_recommendation_robustness <- function(
    sim_args,
    perturb_params = names(ROBUSTNESS_LABELS),
    perturb_pct = 0.15,
    n_iterations = 150,
    frac_fleet_cost_per_day = 250000, wireline_cost_per_day = 15000,
    ct_cost_per_day = 25000, milling_cost_per_day = 18000, testing_unit_cost_per_day = 12000,
    n_cores = max(1L, parallel::detectCores() - 1L),
    progress_callback = NULL
) {
  stopifnot(is.list(sim_args))
  perturb_params <- intersect(perturb_params, names(ROBUSTNESS_LABELS))
  if (length(perturb_params) == 0) stop("No valid perturb_params supplied.")

  base_args <- sim_args
  base_args$progress_callback <- NULL
  base_args$keep_logs <- FALSE
  base_args$collect_well_details <- FALSE
  base_args$n_iterations <- as.integer(n_iterations)

  rec_cost_args <- list(
    frac_fleet_cost_per_day = frac_fleet_cost_per_day,
    wireline_cost_per_day = wireline_cost_per_day,
    ct_cost_per_day = ct_cost_per_day,
    milling_cost_per_day = milling_cost_per_day,
    testing_unit_cost_per_day = testing_unit_cost_per_day
  )

  base_res <- .robustness_run_one(base_args, rec_cost_args)

  # "ALL" jobs perturb every candidate assumption together in the same
  # direction -- a combined best-case (-1, all favourable) and stress-case
  # (+1, all unfavourable) bundle, alongside the one-at-a-time jobs.
  jobs <- bind_rows(
    expand.grid(parameter = perturb_params, direction = c(-1, 1), stringsAsFactors = FALSE),
    tibble(parameter = "ALL", direction = c(-1, 1))
  )
  n_jobs <- nrow(jobs)

  .rob_job <- function(i) {
    param <- jobs$parameter[i]
    direction <- jobs$direction[i]
    args <- base_args
    sweep_params <- if (identical(param, "ALL")) perturb_params else param
    for (p in sweep_params) {
      base_val <- as.numeric(base_args[[p]] %||% 0)
      args[[p]] <- base_val * (1 + direction * perturb_pct)
    }
    out <- .robustness_run_one(args, rec_cost_args)
    out$parameter <- param
    out$direction <- direction
    out$value <- if (identical(param, "ALL")) NA_real_ else args[[param]]
    out
  }

  if (!is.null(progress_callback)) {
    progress_callback(1L, n_jobs + 1L)
    job_results <- lapply(seq_len(n_jobs), function(i) {
      out <- .rob_job(i)
      progress_callback(i + 1L, n_jobs + 1L)
      out
    })
  } else {
    job_results <- .par_lapply(seq_len(n_jobs), .rob_job, n_cores)
  }
  all_results <- bind_rows(job_results)
  perturbed <- all_results %>% filter(parameter != "ALL")

  # Combined best/base/stress scenario summary.
  combined <- bind_rows(
    all_results %>% filter(parameter == "ALL", direction == -1) %>% mutate(scenario = "Best case"),
    base_res %>% mutate(scenario = "Base case"),
    all_results %>% filter(parameter == "ALL", direction == 1) %>% mutate(scenario = "Stress case")
  ) %>%
    mutate(
      delta_p50_days = p50_days - base_res$p50_days,
      stable = recommendation == base_res$recommendation,
      scenario = factor(scenario, levels = c("Best case", "Base case", "Stress case"))
    ) %>%
    arrange(scenario) %>%
    select(scenario, p50_days, p90_days, delta_p50_days,
           readiness_score, readiness_status, recommendation, bottleneck, stable)

  summary_tbl <- bind_rows(lapply(perturb_params, function(p) {
    low  <- perturbed %>% filter(parameter == p, direction == -1)
    high <- perturbed %>% filter(parameter == p, direction == 1)

    flips <- c()
    if (!identical(low$recommendation, base_res$recommendation)) {
      flips <- c(flips, sprintf("-%.0f%%: %s", 100 * perturb_pct, low$recommendation))
    }
    if (!identical(high$recommendation, base_res$recommendation)) {
      flips <- c(flips, sprintf("+%.0f%%: %s", 100 * perturb_pct, high$recommendation))
    }
    stable <- length(flips) == 0

    tibble(
      assumption = ROBUSTNESS_LABELS[[p]],
      parameter = p,
      base_value = as.numeric(base_args[[p]] %||% NA_real_),
      low_value = low$value,
      low_p50_days = low$p50_days,
      low_readiness_status = low$readiness_status,
      base_p50_days = base_res$p50_days,
      base_readiness_status = base_res$readiness_status,
      high_value = high$value,
      high_p50_days = high$p50_days,
      high_readiness_status = high$readiness_status,
      stable = stable,
      note = if (stable) "Recommendation unchanged" else paste(flips, collapse = "; ")
    )
  }))

  list(
    base = base_res,
    detail = perturbed,
    summary = summary_tbl,
    combined = combined,
    perturb_pct = perturb_pct,
    n_iterations = as.integer(n_iterations)
  )
}

# -----------------------------------------------------------------------------
# combine_recommendation_confidence(): folds the statistical confidence from
# recommend_action() (win-rate from paired re-simulation, or the cascade's
# prob_primary when not verified) together with the robustness sweep above
# (OAT stability + combined stress case) into a single decision-ready
# confidence level. If `robustness` is NULL (not yet run), falls back to the
# statistical band alone with a caveat to run the check.
# -----------------------------------------------------------------------------
.CONF_RANK <- c("Inconclusive" = 1L, "Low" = 2L, "Moderate" = 3L, "High" = 4L)

combine_recommendation_confidence <- function(rec, robustness = NULL) {
  stat_band <- rec$confidence_band
  stat_line <- sprintf("%.0f%% confidence in the underlying analysis (%s).",
                        100 * rec$confidence, rec$basis)

  if (is.null(robustness)) {
    return(list(
      level = stat_band,
      label = sprintf("%s confidence", stat_band),
      detail = c(
        stat_line,
        "Robustness not yet checked - run the check below to confirm this holds under +-15% planning-assumption uncertainty."
      )
    ))
  }

  n_total    <- nrow(robustness$summary)
  n_unstable <- sum(!robustness$summary$stable)
  oat_stable <- n_unstable == 0
  stress     <- robustness$combined[robustness$combined$scenario == "Stress case", ]
  stress_stable <- isTRUE(stress$stable[1])

  robustness_rank <- if (oat_stable && stress_stable) 4L
                      else if (oat_stable || stress_stable) 3L
                      else 2L
  overall_rank <- min(.CONF_RANK[[stat_band]], robustness_rank)
  overall <- names(.CONF_RANK)[.CONF_RANK == overall_rank]

  oat_line <- if (oat_stable) {
    sprintf("Holds across all %d one-at-a-time assumption swings of +-%.0f%%.",
            n_total, 100 * robustness$perturb_pct)
  } else {
    sprintf("Changes under a +-%.0f%% swing in %d of %d assumptions (see table below).",
            100 * robustness$perturb_pct, n_unstable, n_total)
  }
  stress_line <- if (stress_stable) {
    sprintf("Holds under the combined stress case (all assumptions +-%.0f%% unfavourable, P50 %+.1f d).",
            100 * robustness$perturb_pct, stress$delta_p50_days[1])
  } else {
    sprintf("Changes to \"%s\" under the combined stress case (P50 %+.1f d).",
            stress$recommendation[1], stress$delta_p50_days[1])
  }

  list(
    level = overall,
    label = sprintf("%s confidence", overall),
    detail = c(stat_line, oat_line, stress_line)
  )
}
