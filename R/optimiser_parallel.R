# optimiser_parallel.R
# -----------------------------------------------------------------------------
# Drop-in parallel replacement for optimise_campaign_scenarios().
#
# Why this is safe (bit-identical to the sequential version):
#   simulate_campaign_detailed() calls set.seed(seed) internally (engine L982),
#   and the optimiser passes the SAME seed to every config (common random
#   numbers for variance reduction). Each config's RNG stream is therefore
#   fully determined by `seed` and INDEPENDENT of evaluation order or of any
#   fork/worker RNG state. Parallelising the config loop changes nothing
#   numerically -- only wall-clock. (No L'Ecuyer-CMRG streams required.)
#
# Backend:
#   - Unix/macOS: parallel::mclapply (fork; zero setup, shares memory).
#   - Windows / mc.cores==1: falls back to lapply (sequential, still correct).
#   - If future.apply is installed and options(optimiser.use_future=TRUE),
#     uses future_lapply for a cross-platform multiprocess backend.
#
# Source AFTER simulation_engine.R. Then call optimise_campaign_scenarios_par()
# with the same arguments you pass to optimise_campaign_scenarios().
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

.par_lapply <- function(X, FUN, n_cores) {
  use_future <- isTRUE(getOption("optimiser.use_future", FALSE)) &&
    requireNamespace("future.apply", quietly = TRUE)
  if (use_future) {
    return(future.apply::future_lapply(X, FUN, future.seed = FALSE))
  }
  if (.Platform$OS.type == "windows" || n_cores <= 1L) {
    return(lapply(X, FUN))
  }
  out <- parallel::mclapply(X, FUN, mc.cores = n_cores, mc.preschedule = FALSE)
  failed <- vapply(out, function(z) inherits(z, "try-error"), logical(1))
  if (any(failed)) {
    stop("Parallel worker error(s):\n",
         paste(unique(unlist(out[failed])), collapse = "\n"))
  }
  out
}

optimise_campaign_scenarios_par <- function(
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
    n_cores = max(1L, parallel::detectCores() - 1L),
    progress_callback = NULL   # accepted for signature parity; not called per-item
) {
  stopifnot(nrow(scenario_grid) > 0)

  required_cols <- c("operation_mode", "frac_fleets", "wireline_units", "ct_units",
                     "milling_units", "testing_units", "frac_trees", "allow_ct_for_milling")
  missing <- setdiff(required_cols, names(scenario_grid))
  if (length(missing) > 0) stop("scenario_grid missing columns: ", paste(missing, collapse = ", "))

  # --- identical scoring + run logic to the sequential version ---------------
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
        historical_wells = historical_wells, assumptions = assumptions,
        n_wells = n_wells, n_iterations = n_iter,
        frac_fleets = cfg$frac_fleets, wireline_units = cfg$wireline_units,
        ct_units = cfg$ct_units, milling_units = cfg$milling_units,
        testing_units = cfg$testing_units, frac_trees = cfg$frac_trees,
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

  # --- Stage 1: screen (PARALLELISED across configs) -------------------------
  screen_one <- function(i) {
    cfg <- scenario_grid[i, , drop = FALSE]
    run <- run_config(cfg, screen_iterations)
    bind_cols(as_tibble(cfg), score_run(run, cfg)) %>%
      mutate(scenario_id = i, stage = "screened")
  }
  results <- .par_lapply(seq_len(n_cfg), screen_one, n_cores)
  res <- bind_rows(results)

  # --- Stage 2: refine top-N (PARALLELISED across the few survivors) ---------
  refine_ids <- res %>%
    arrange(total_mobilisation_cost) %>%
    slice_head(n = min(top_n_refine, n_cfg)) %>%
    pull(scenario_id)

  refine_one <- function(sid) {
    cfg <- scenario_grid[sid, , drop = FALSE]
    run <- run_config(cfg, refine_iterations)
    bind_cols(as_tibble(cfg), score_run(run, cfg)) %>%
      mutate(scenario_id = sid, stage = "refined")
  }
  refined_list <- .par_lapply(as.list(refine_ids),
                              function(s) refine_one(s),
                              min(n_cores, length(refine_ids)))
  for (refined in refined_list) {
    sid <- refined$scenario_id
    res[res$scenario_id == sid, names(refined)] <- refined
  }

  # --- Pareto + labelling (identical to sequential) --------------------------
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
      fastest = p50_days == min(p50_days),
      config_label = paste0(
        operation_mode,
        " | FF:", frac_fleets, " WL:", wireline_units, " CT:", ct_units,
        " ML:", milling_units, " TU:", testing_units, " Trees:", frac_trees,
        ifelse(allow_ct_for_milling, " +CTmill", "")
      )
    ) %>%
    arrange(total_mobilisation_cost)
}

# -----------------------------------------------------------------------------
# Regression check: prove parallel == sequential, bit-for-bit, then report
# speedup. Run:  Rscript optimiser_parallel.R   (requires simulation_engine.R
# and profile_engine.R generators alongside, or it builds its own inputs).
# -----------------------------------------------------------------------------
verify_parallel_matches <- function(engine_path = Sys.getenv("ENGINE_PATH", "simulation_engine_fast.R")) {
  if (!file.exists(engine_path)) engine_path <- "simulation_engine.R"
  source(engine_path)

  # Build synthetic inputs inline (self-contained; does NOT source the profiler).
  assumptions <- dplyr::bind_rows(
    tibble::tribble(
      ~variable,                  ~category, ~type,   ~probability, ~min_days, ~most_likely_days, ~max_days, ~simulation_impact, ~scope,
      "Stages per well",          "Param",   "param", NA,           8,         10,                14,        NA,                 NA,
      "Temperature log stages",   "Param",   "param", NA,           1,         2,                 3,         NA,                 NA,
      "Wells per pad",            "Param",   "param", NA,           2,         3,                 4,         NA,                 NA,
      "Cement eval duration",            "Param",   "param", NA,           0.5,       1.0,               2.0,       NA,                 NA,
      "Scraper / cleanout run",   "Param",   "param", NA,           0.3,       0.5,               1.0,       NA,                 NA,
      "Temperature log duration", "Param",   "param", NA,           0.2,       0.3,               0.5,       NA,                 NA,
      "Isolation plug duration",  "Param",   "param", NA,           0.3,       0.5,               1.0,       NA,                 NA,
      "Cement eval offline",             "Param",   "param", 0.8,          0,         0,                 0,         NA,                 NA
    ),
    tibble::tribble(
      ~variable,            ~category,              ~type,  ~probability, ~min_days, ~most_likely_days, ~max_days, ~simulation_impact, ~scope,
      "Screenout",          "Frac",                 "risk", 0.08,         0.5,       1.0,               3.0,       "extra stage",      "stage",
      "Gun misfire",        "Wireline/Perforation", "risk", 0.05,         0.2,       0.5,               1.0,       "additional run",   "stage",
      "Isolation plug fail","Plug",                 "risk", 0.04,         0.3,       0.8,               2.0,       "replacement plug", "well",
      "Cement issue",       "CT Unit / cleanout",   "risk", 0.06,         0.5,       1.0,               2.0,       "ct intervention",  "well",
      "Milling difficulty", "Milling",              "risk", 0.10,         0.5,       1.0,               3.0,       "extra milling",    "well",
      "Weather delay",      "Weather",              "risk", 0.15,         1.0,       2.0,               5.0,       "schedule delay",   "campaign"
    )
  )
  set.seed(1)
  historical <- tibble::tibble(
    well_id = paste0("HW_", 1:30), pad_id = paste0("Pad_", ((1:30 - 1) %/% 3) + 1),
    stages_completed = sample(8:14, 30, TRUE), plugs_installed = sample(8:14, 30, TRUE),
    contingency_plugs = sample(0:2, 30, TRUE), frac_days = round(runif(30, 8, 18), 1),
    cement_eval_days = round(runif(30, 0.5, 2), 2), milling_days = round(runif(30, 5, 12), 1),
    frac_days_per_stage = round(triangle_sample(0.5, 0.8, 2.5, 30), 3),
    milling_days_per_plug = round(triangle_sample(0.4, 0.7, 1.5, 30), 3)
  )
  grid <- expand.grid(operation_mode = c("Conventional", "Zipper"), milling_units = 1:3,
                      stringsAsFactors = FALSE)
  grid$testing_units <- 1; grid$frac_fleets <- 1; grid$wireline_units <- 1; grid$ct_units <- 1
  grid$frac_trees <- ifelse(grid$operation_mode == "Zipper", 2, 1)
  grid$allow_ct_for_milling <- FALSE

  t_seq <- system.time(
    seq_res <- optimise_campaign_scenarios(
      historical, assumptions, n_wells = 30, scenario_grid = grid,
      screen_iterations = 150, refine_iterations = 600, top_n_refine = 5, seed = 123)
  )[["elapsed"]]

  t_par <- system.time(
    par_res <- optimise_campaign_scenarios_par(
      historical, assumptions, n_wells = 30, scenario_grid = grid,
      screen_iterations = 150, refine_iterations = 600, top_n_refine = 5, seed = 123)
  )[["elapsed"]]

  num_cols <- c("p50_days", "p90_days", "idle_days", "idle_cost",
                "spread_rate_per_day", "total_mobilisation_cost")
  seq_o <- seq_res[order(seq_res$config_label), num_cols]
  par_o <- par_res[order(par_res$config_label), num_cols]

  ident <- isTRUE(all.equal(as.data.frame(seq_o), as.data.frame(par_o), tolerance = 0))
  cat(sprintf("\nbit-identical results: %s\n", ident))
  cat(sprintf("sequential: %.2fs | parallel: %.2fs | speedup: %.2fx (cores-1=%d)\n",
              t_seq, t_par, t_seq / t_par, max(1L, parallel::detectCores() - 1L)))
  if (!ident) {
    cat("MISMATCH -- diff:\n"); print(seq_o - par_o)
    stop("Parallel optimiser is NOT numerically identical -- do not ship.")
  }
  invisible(ident)
}

if (sys.nframe() == 0L && !interactive()) verify_parallel_matches()
