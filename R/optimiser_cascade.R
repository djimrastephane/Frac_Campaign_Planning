# optimiser_cascade.R
# Split out of simulation_engine_fast.R (see docs/architecture_cleanup_plan.md).
# Multi-run orchestration that calls simulate_campaign_detailed() repeatedly:
# optimise_campaign_scenarios() (grid search over resource configs) and
# analyse_constraint_cascade() (greedy sequential bottleneck resolution).
# Depends on engine_core.R's simulate_campaign_detailed() -- source
# engine_core.R first. R/optimiser_parallel.R's parallel screening path
# depends on this file's optimise_campaign_scenarios() the same way it always
# has -- unchanged by this split.

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
        seed = seed,  # common random numbers across configs
        keep_logs = FALSE,            # screening/refine use $summary only
        collect_well_details = FALSE
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
      # Exactly ONE row gets fastest = TRUE. Byte-identical P50 ties are
      # expected under common random numbers whenever an added unit is
      # non-binding (e.g. a 3rd milling unit that never gates a draw
      # reproduces the 2-unit results exactly), so `p50 == min(p50)` alone
      # can flag several configs. Tie-break: cheapest total mobilisation
      # cost among the tied set, then first in grid order -- the same row
      # the app's existing filter(fastest) %>% slice(1) displays picked,
      # so downstream plots/cards are unchanged.
      fastest = {
        tied <- which(p50_days == min(p50_days))
        dplyr::row_number() == tied[order(total_mobilisation_cost[tied])][1]
      },
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
    seed = 123,
    progress_callback   = NULL
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
    # estimated_campaign_days = max(frac-path days, post-frac completion days).
    # Raw utilization isn't a reliable signal of which resource is binding:
    # Milling/Testing can show high utilization while sitting well inside the
    # post-frac window, with zero effect on campaign length, when the frac
    # path is actually the side of the max() that's binding (and vice versa).
    # Restrict candidates to whichever side dominates across iterations so the
    # cascade doesn't chase a resource that can't move the schedule.
    frac_path_binds <- mean(sm$post_frac_completion_days < sm$estimated_campaign_days, na.rm = TRUE) > 0.5
    eligible <- if (frac_path_binds) {
      c("Frac fleet", "Wireline", "CT / cleanout")
    } else {
      c("Milling", "Testing unit", "CT / cleanout")
    }

    ranked_eligible <- ru %>%
      filter(resource %in% eligible) %>%
      arrange(desc(p90_utilization))
    bottleneck_resource <- ranked_eligible$resource[1]
    # Utilization is supporting evidence only, not the gate: it can mis-rank a
    # resource that's actually gated by a shared dependency (e.g. milling
    # needs a free testing unit too, so testing can be the true limiter even
    # when milling's own utilization reads higher). The caller trials every
    # eligible candidate and decides by measured saving instead; utilization
    # is only used there to break near-ties.
    candidates <- ranked_eligible$resource

    list(
      p50   = as.numeric(quantile(sm$estimated_campaign_days, 0.5, na.rm = TRUE)),
      p90   = as.numeric(quantile(sm$estimated_campaign_days, 0.9, na.rm = TRUE)),
      p10   = as.numeric(quantile(sm$estimated_campaign_days, 0.1, na.rm = TRUE)),
      utilization = setNames(ru$p90_utilization, ru$resource),
      bottleneck  = bottleneck_resource,
      candidates  = candidates,
      bottleneck_util = ru$p90_utilization[ru$resource == bottleneck_resource][1],
      resource_units = units_map
    )
  }

  cfg <- base_config
  if (!is.null(progress_callback)) progress_callback(0L, max_steps)
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
    candidates <- prev_score$candidates
    candidates <- candidates[!is.na(candidates) & candidates %in% names(resource_to_arg)]
    if (length(candidates) == 0) break

    # Trial EVERY eligible candidate (+1 unit) and measure its real schedule
    # saving -- utilization is supporting evidence only (used below to break
    # near-ties), never a gate on which candidates get trialled. See score()
    # above for why utilization alone can mis-rank the true limiter.
    trials <- lapply(candidates, function(cand) {
      arg_name <- resource_to_arg[cand]
      trial_cfg <- cfg
      units_before <- as.integer(cfg[[arg_name]] %||% 1)
      trial_cfg[[arg_name]] <- units_before + 1L
      r_trial <- run_sim(trial_cfg)
      s_trial <- score(r_trial, trial_cfg)
      list(resource = cand, cfg = trial_cfg, units_before = units_before,
           units_after = units_before + 1L, score = s_trial,
           saving = prev_p50 - s_trial$p50,
           prior_utilization = unname(prev_score$utilization[cand]))
    })
    savings <- vapply(trials, function(x) x$saving, numeric(1))
    best_saving <- max(savings)

    # No eligible resource materially shortens the campaign: stop instead of
    # picking an essentially-arbitrary "winner" among noise-level savings.
    no_bottleneck_threshold <- max(min_saving_days, 0.01 * prev_p50)
    if (best_saving < no_bottleneck_threshold) {
      steps[[step + 1]] <- tibble(
        step                = as.integer(step),
        action              = "No further resource materially shortens the campaign",
        resource_fixed      = NA_character_,
        units_before        = NA_integer_,
        units_after         = NA_integer_,
        p50_days            = prev_p50,
        p10_days            = prev_score$p10,
        p90_days            = prev_score$p90,
        days_saved          = best_saving,
        daily_rate          = NA_real_,
        incremental_cost    = NA_real_,
        schedule_value      = NA_real_,
        cost_per_day_saved  = NA_real_,
        roi_days_per_Mdollar= NA_real_,
        bottleneck_now      = prev_score$bottleneck,
        bottleneck_util_pct = round(prev_score$bottleneck_util * 100, 1),
        verdict             = sprintf(
          "No material bottleneck — best option saves only %.1f days (< %.1f day / 1%% threshold)",
          best_saving, min_saving_days)
      )
      if (!is.null(progress_callback)) progress_callback(step, max_steps)
      break
    }

    # Tie-break candidates within 0.5 days of the best measured saving by
    # PRIOR utilization (supporting evidence, not the primary signal) -- this
    # only matters when two candidates are genuinely close; it never
    # overrides a materially larger saving elsewhere.
    near_best <- which(savings >= best_saving - 0.5)
    best <- if (length(near_best) > 1) {
      utils <- vapply(trials[near_best], function(x) x$prior_utilization %||% -Inf, numeric(1))
      trials[[near_best[which.max(utils)]]]
    } else {
      trials[[near_best[1]]]
    }

    bn           <- best$resource
    units_before <- best$units_before
    units_after  <- best$units_after
    cfg          <- best$cfg
    s_new        <- best$score

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

    if (!is.null(progress_callback)) progress_callback(step, max_steps)
    if (saving < min_saving_days) break
  }

  bind_rows(Filter(Negate(is.null), steps))
}
