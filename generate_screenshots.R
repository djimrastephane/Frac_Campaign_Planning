# generate_screenshots.R
# Generates PNG screenshots of new analysis features for the README.
# Run from the project root: Rscript generate_screenshots.R

setwd("/Users/djimra/Oil_and_Gas_Projects/Zipper_Frac/frac_campaign_simulator_project_v14")

suppressPackageStartupMessages({
  library(MASS)        # load MASS first so dplyr::select wins below
  library(dplyr); library(ggplot2); library(tibble); library(tidyr)
  library(stringr); library(readr); library(janitor)
  library(scales); library(gridExtra); library(patchwork)
})
select <- dplyr::select   # prevent MASS::select masking dplyr::select

cat("Sourcing R modules...\n")
source("R/load_inputs.R")
source("R/validate_inputs.R")
source("R/simulation_engine_fast.R")
source("R/risk_library_engine.R")
source("R/optimiser_parallel.R")
source("R/risk_uncertainty.R")
source("R/sensitivity_analysis.R")
source("R/whatif_builder.R")
source("R/bayesian_updater.R")
source("R/learning_engine.R")
source("R/risk_heatmap.R")
source("R/plots.R")

OUT  <- "docs/images/screenshots"
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

W <- 1400; H <- 780   # pixel dimensions for all screenshots
RES <- 140             # ppi — gives crisp output at README width

save_png <- function(p, name, w = W, h = H) {
  path <- file.path(OUT, name)
  ragg::agg_png(path, width = w, height = h, res = RES, background = "white")
  print(p); dev.off()
  cat(sprintf("  saved %s\n", path))
}

# ---------------------------------------------------------------------------
# Synthetic data
# ---------------------------------------------------------------------------
cat("Building synthetic data...\n")
set.seed(42)
ALL_WELLS <- synthetic_historical_wells(n = 50, seed = 42)
HISTORICAL <- ALL_WELLS[1:35, ]
NEW_WELLS  <- ALL_WELLS[36:50, ]

ASSUMPTIONS <- load_master_assumptions(
  "data_templates/master_risks_assumptions_template.csv"
)

BASE_CONV <- list(
  historical_wells = HISTORICAL, assumptions = ASSUMPTIONS,
  n_wells = 24L, n_iterations = 400L,
  frac_fleets = 1L, wireline_units = 1L, ct_units = 1L,
  milling_units = 2L, testing_units = 2L, frac_trees = 1L,
  operation_mode = "Conventional", seed = 42L,
  keep_logs = TRUE, collect_well_details = TRUE
)
BASE_ZIP <- modifyList(BASE_CONV, list(
  operation_mode = "Zipper", frac_trees = 2L, seed = 42L
))

# ---------------------------------------------------------------------------
# 07 — Historical Learning
# ---------------------------------------------------------------------------
cat("\n[07] Historical Learning...\n")
learning <- learn_from_historical(HISTORICAL)

p_density <- plot_learning_density(learning) +
  labs(subtitle = "Fitted distributions to FracDaysPerStage and MillingDaysPerPlug\nAIC-ranked — solid line = best fit")
p_qq      <- plot_learning_qq(learning) +
  labs(subtitle = "Q-Q plot: observed vs theoretical quantiles for best-fit distribution")

save_png(
  (p_density | p_qq) +
    plot_annotation(
      title   = "Historical Learning Engine — distribution fitting from historical wells",
      theme   = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
    ),
  "07_historical_learning.png", w = 1600, h = 820
)

# ---------------------------------------------------------------------------
# 08 — Sensitivity Analysis
# ---------------------------------------------------------------------------
cat("\n[08] Sensitivity Analysis (OAT sweep — takes ~60 s)...\n")
sensitivity <- run_sensitivity_analysis(
  args_by_mode        = list(Conventional = BASE_CONV, Zipper = BASE_ZIP),
  scalar_perturb_pct  = 0.20,
  risk_perturb_pct    = 0.50,
  n_iterations        = 120L,
  n_cores             = max(1L, parallel::detectCores() - 1L)
)

p_tornado  <- plot_sensitivity_tornado(sensitivity, top_n = 12)
p_bymode   <- plot_sensitivity_by_mode(sensitivity, top_n = 10)

save_png(
  (p_tornado / p_bymode) +
    plot_annotation(
      title   = "Sensitivity Analysis — OAT perturbation sweep across timing, risk and resource variables",
      theme   = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
    ),
  "08_sensitivity.png", w = 1400, h = 1100
)

# ---------------------------------------------------------------------------
# 09 — Bayesian Update
# ---------------------------------------------------------------------------
cat("\n[09] Bayesian Update...\n")
risk_obs <- tibble::tibble(
  risk_event = c("Screen out", "Plug pressure test failure", "Perforation gun misfire"),
  n_trials   = c(120L, 120L, 120L),
  n_events   = c(6L,   9L,   4L)
)
bayes <- run_bayesian_update(HISTORICAL, NEW_WELLS,
                             assumptions = ASSUMPTIONS, risk_obs = risk_obs)

p_dur  <- plot_bayesian_duration_update(bayes, HISTORICAL, NEW_WELLS)
p_risk <- plot_bayesian_risk_update(bayes$risk_update)

# Build a narrative text panel mirroring the new bayes_status UI output
.bayes_narrative_panel <- function(br) {
  dur <- br$duration_update
  lines <- c(
    sprintf("Update: %d prior + %d new = %d observations",
            br$n_prior, br$n_new, br$n_prior + br$n_new),
    sprintf("Duration priors: bootstrap from %d historical wells", br$n_prior),
    sprintf("Risk priors: Beta(assumptions CSV, strength = %d)", br$prior_strength),
    ""
  )
  for (i in seq_len(nrow(dur))) {
    r         <- dur[i, ]
    has_new   <- !is.na(r$new_mean) && r$n_new > 0
    pct_diff  <- if (has_new) 100 * abs(r$new_mean / r$prior_mean - 1) else NA
    dir_word  <- if (has_new && r$new_mean > r$prior_mean) "above" else "below"
    post_dir  <- if (r$delta_mean > 0) "up" else if (r$delta_mean < 0) "down" else "unchanged"
    ci_zero   <- r$ci90_lo <= 0 && r$ci90_hi >= 0
    short_label <- substr(r$label, 1, 28)
    lines <- c(lines,
      sprintf("  %s", short_label),
      if (has_new) sprintf("    New data: %d wells, mean %.3f d", r$n_new, r$new_mean)
      else          "    New data: none",
      sprintf("    Shift: %+.4f d", r$delta_mean),
      sprintf("    90%% CI [%+.4f, %+.4f]", r$ci90_lo, r$ci90_hi),
      if (has_new) sprintf("    New %.1f%% %s prior (%.3f d) -> %s",
                           pct_diff, dir_word, r$prior_mean, post_dir)
      else          "    Posterior unchanged.",
      if (ci_zero) "    [CI incl. zero: within noise]"
      else          "    [CI excl. zero: consistent shift]",
      ""
    )
  }
  if (!is.null(br$risk_update) && nrow(br$risk_update) > 0) {
    lines <- c(lines, "  Risk updates:")
    for (i in seq_len(nrow(br$risk_update))) {
      r        <- br$risk_update[i, ]
      obs_rate <- if (r$n_trials > 0L) r$n_events / r$n_trials else NA_real_
      dir      <- if (!is.na(obs_rate)) {
        if (obs_rate > r$prior_prob + 0.005) "above prior"
        else if (obs_rate < r$prior_prob - 0.005) "below prior"
        else "matches prior"
      } else "--"
      ci_w <- r$posterior_p95 - r$posterior_p05
      note <- if (ci_w < 0.05) "constrained" else "uncertain"
      nm   <- substr(r$risk_event, 1, 20)
      lines <- c(lines,
        sprintf("    %-20s: obs %.1f%% (%s)", nm,
                if (!is.na(obs_rate)) 100*obs_rate else 0, dir),
        sprintf("      post %.1f%% [%.1f%%-%.1f%%] %s",
                100*r$posterior_mean, 100*r$posterior_p05, 100*r$posterior_p95, note))
    }
  }
  ggplot() +
    xlim(0, 1) + ylim(0, 1) +
    annotate("text", x = 0.04, y = 0.97,
             label    = paste(lines, collapse = "\n"),
             hjust    = 0, vjust    = 1,
             family   = "mono", size = 2.35, lineheight = 1.4,
             colour   = "#212529") +
    theme_void() +
    theme(
      panel.background = element_rect(fill = "#f8f9fa", colour = "#ced4da", linewidth = 0.4),
      plot.margin      = margin(10, 4, 10, 10)
    )
}

p_narrative <- .bayes_narrative_panel(bayes)

save_png(
  (p_narrative | (p_dur / p_risk)) +
    plot_layout(widths = c(1, 2)) +
    plot_annotation(
      title   = "Bayesian Update — diagnostic narrative and prior vs posterior distributions",
      theme   = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
    ),
  "09_bayesian_update.png", w = 1600, h = 900
)

# ---------------------------------------------------------------------------
# 10 — What-If Builder
# ---------------------------------------------------------------------------
cat("\n[10] What-If Scenario Builder...\n")
whatif <- run_whatif_batch(
  base_args  = BASE_CONV,
  variants   = list(
    "Add wireline unit"   = list(wireline_units = 2L),
    "Add milling unit"    = list(milling_units = 3L),
    "Switch to Zipper"    = list(operation_mode = "Zipper", frac_trees = 2L),
    "Reduce frac time 10%" = list(frac_time_per_stage_hours = 11.0),
    "High risk scenario"  = list(risk_multiplier = 1.5)
  ),
  n_iterations = 300L
)

p_bars   <- plot_whatif_bars(whatif)
p_scurve <- plot_whatif_scurve(whatif)

save_png(
  (p_bars | p_scurve) +
    plot_annotation(
      title   = "What-If Scenario Builder — P10/P50/P90 and S-curve comparison across named variants",
      theme   = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
    ),
  "10_whatif.png", w = 1600, h = 820
)

# ---------------------------------------------------------------------------
# 11 — Schedule Risk Heatmap (needs full simulation with risk log)
# ---------------------------------------------------------------------------
cat("\n[11] Schedule Risk Heatmap (running simulation)...\n")
sim_conv <- do.call(simulate_campaign_detailed, BASE_CONV)
sim_zip  <- do.call(simulate_campaign_detailed, BASE_ZIP)

combined_log <- dplyr::bind_rows(sim_conv$risk_event_log, sim_zip$risk_event_log)
combined_sum <- dplyr::bind_rows(sim_conv$summary,        sim_zip$summary)

heatmap_data <- build_schedule_risk_heatmap(combined_log, combined_sum)

p_heat <- plot_schedule_risk_heatmap(heatmap_data, top_n_risks = 8)
p_rank <- plot_well_risk_ranking(heatmap_data)

save_png(
  (p_heat / p_rank) +
    plot_annotation(
      title   = "Schedule Risk Heatmap — expected delay by well and risk type; well risk ranking with classification",
      theme   = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
    ),
  "11_risk_heatmap.png", w = 1400, h = 1100
)

cat("\nAll screenshots generated successfully.\n")
