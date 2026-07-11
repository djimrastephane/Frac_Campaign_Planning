# generate_screenshots_tabs.R
# Regenerates screenshots 02-05 with fully populated simulation data.
# Run from the project root: Rscript generate_screenshots_tabs.R

setwd("/Users/djimra/Oil_and_Gas_Projects/Zipper_Frac/frac_campaign_simulator_project_v14")

suppressPackageStartupMessages({
  library(MASS)
  library(dplyr); library(ggplot2); library(tibble); library(tidyr)
  library(stringr); library(readr); library(janitor)
  library(scales); library(gridExtra); library(patchwork)
})
select <- dplyr::select

cat("Sourcing R modules...\n")
source("R/load_inputs.R")
source("R/validate_inputs.R")
source("R/engine_core.R")
source("R/summaries.R")
source("R/report_pdf.R")
source("R/constants.R")
source("R/optimiser_explain.R")
source("R/optimiser_cascade.R")
source("R/optimiser_manifest.R")
source("R/risk_library_engine.R")
source("R/optimiser_parallel.R")
source("R/risk_uncertainty.R")
source("R/bottleneck_explain.R")
source("R/recommendations.R")
source("R/robustness.R")
source("R/sensitivity_analysis.R")
source("R/whatif_builder.R")
source("R/bayesian_updater.R")
source("R/learning_engine.R")
source("R/risk_heatmap.R")
source("R/scenario_library.R")
source("R/narrative_engine.R")
source("R/report_decision_page.R")
source("R/plots.R")

OUT <- "docs/images/screenshots"
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

save_png <- function(p, name, w = 1400, h = 820) {
  path <- file.path(OUT, name)
  ragg::agg_png(path, width = w, height = h, res = 140, background = "white")
  print(p); dev.off()
  cat(sprintf("  saved %s\n", path))
}

# ---------------------------------------------------------------------------
# Shared simulation data (same seed as generate_screenshots.R)
# ---------------------------------------------------------------------------
cat("Building synthetic data and running simulation...\n")
set.seed(42)
HISTORICAL  <- synthetic_historical_wells(n = 50, seed = 42)
ASSUMPTIONS <- load_master_assumptions("data_templates/master_risks_assumptions_template.csv")

BASE_CONV <- list(
  historical_wells = HISTORICAL, assumptions = ASSUMPTIONS,
  n_wells = 24L, n_iterations = 600L,
  frac_fleets = 1L, wireline_units = 1L, ct_units = 1L,
  milling_units = 2L, testing_units = 2L, frac_trees = 1L,
  operation_mode = "Conventional", seed = 42L,
  keep_logs = TRUE, collect_well_details = TRUE
)
BASE_ZIP <- modifyList(BASE_CONV, list(operation_mode = "Zipper", frac_trees = 2L))

sim_conv <- do.call(simulate_campaign_detailed, BASE_CONV)
sim_zip  <- do.call(simulate_campaign_detailed, BASE_ZIP)

summary_both  <- bind_rows(sim_conv$summary,        sim_zip$summary)
log_both      <- bind_rows(sim_conv$risk_event_log, sim_zip$risk_event_log)
res_both      <- bind_rows(sim_conv$resource_utilization, sim_zip$resource_utilization)
detail_both   <- bind_rows(sim_conv$well_details,   sim_zip$well_details)

# Analytics derived from conventional results (primary mode)
resource_summary <- summarise_resource_utilization(sim_conv$resource_utilization)
bottlenecks      <- summarise_bottlenecks(resource_summary)
delay            <- summarise_delay_contributors(sim_conv$risk_event_log)
stage_risk       <- summarise_stage_level_risks(sim_conv$risk_event_log, sim_conv$summary)
consequences     <- summarise_risk_consequences(sim_conv$risk_event_log, sim_conv$summary)
timeline         <- build_resource_timeline(sim_conv$summary)
recommendations  <- build_resource_recommendations(sim_conv$summary, sim_conv$resource_utilization)
readiness        <- build_readiness_score(sim_conv$summary, sim_conv$risk_event_log,
                                          sim_conv$resource_utilization)
cost_impact      <- build_cost_impact(
  sim_conv$summary, sim_conv$resource_utilization,
  frac_fleet_cost_per_day = 250000, wireline_cost_per_day = 15000,
  ct_cost_per_day = 25000, milling_cost_per_day = 18000,
  testing_unit_cost_per_day = 12000
)
heatmap_data <- build_schedule_risk_heatmap(log_both, summary_both)

# ---------------------------------------------------------------------------
# 02 — Decision Support (robustness tornado + scenario library)
# ---------------------------------------------------------------------------
cat("\n[02] Decision Support — robustness check...\n")
# assess_recommendation_robustness uses %||% 0 for missing params, so supply
# the simulation defaults explicitly to avoid zero-value failures
CONV_ROB <- modifyList(BASE_CONV, list(
  frac_time_per_stage_hours     = 12,
  wireline_time_per_stage_hours = 6,
  wireline_contingency_pct      = 10,
  risk_multiplier               = 1,
  zipper_efficiency             = 0.75,
  keep_logs = FALSE, collect_well_details = FALSE
))
robustness <- assess_recommendation_robustness(
  sim_args     = CONV_ROB,
  perturb_pct  = 0.15,
  n_iterations = 100L,
  n_cores      = max(1L, parallel::detectCores() - 1L)
)

# Build 3 scenario records from real simulation runs
sim_zip2  <- do.call(simulate_campaign_detailed, modifyList(BASE_CONV, list(
  operation_mode = "Zipper", frac_trees = 2L, wireline_units = 2L, seed = 99L,
  n_iterations = 300L, keep_logs = FALSE, collect_well_details = FALSE)))
sim_conv2 <- do.call(simulate_campaign_detailed, modifyList(BASE_CONV, list(
  milling_units = 3L, seed = 77L,
  n_iterations = 300L, keep_logs = FALSE, collect_well_details = FALSE)))

scenario_records <- list(
  list(label = "Baseline — 1 frac fleet / conventional",
       duration = sim_conv$summary$estimated_campaign_days),
  list(label = "Zipper + 2 wireline units",
       duration = sim_zip2$summary$estimated_campaign_days),
  list(label = "Baseline + 3 milling units",
       duration = sim_conv2$summary$estimated_campaign_days)
)

p_rob  <- plot_robustness_tornado(robustness) +
  labs(subtitle = "±15 % OAT perturbation — green bars = recommendation stable under this assumption swing")
p_scen <- plot_scenario_comparison(scenario_records)

save_png(
  (p_rob / p_scen) +
    plot_annotation(
      title = "Decision Support — robustness check and scenario library",
      theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
    ),
  "02_decision_support.png", w = 1400, h = 1100
)

# ---------------------------------------------------------------------------
# 03 — Risks (heatmap + ranking only -- README-facing version. The tornado
# and consequence-propagation panels were dropped from this image: cramming
# 4 dense panels into one PNG made it hard to read at README width. They're
# still available individually via plot_risk_tornado()/plot_risk_consequences()
# for anyone regenerating the full per-tab set, just not in this composite.)
# ---------------------------------------------------------------------------
cat("\n[03] Risks tab...\n")
p_heat  <- plot_schedule_risk_heatmap(heatmap_data, top_n_risks = 8)
p_rank  <- plot_well_risk_ranking(heatmap_data)

save_png(
  (p_heat / p_rank) +
    plot_layout(heights = c(1.5, 1)) +
    plot_annotation(
      title = "Risks — schedule risk heatmap and well risk ranking",
      theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
    ),
  "03_risks.png", w = 1400, h = 1250
)

# ---------------------------------------------------------------------------
# 04 — Resources (Gantt + utilization + bottlenecks)
# ---------------------------------------------------------------------------
cat("\n[04] Resources tab...\n")
p_gantt <- plot_resource_gantt(timeline)
# Shorter titles for the side-by-side composite row: the full in-app titles
# are wider than half a PNG and collide across panels.
p_bot   <- plot_bottlenecks(bottlenecks) +
  labs(title = "Bottleneck detection (P90 utilization)",
       subtitle = "Dashed lines: 60% / 85% thresholds")
p_rec   <- plot_resource_recommendations(recommendations) +
  labs(title = "Schedule improvement from +1 unit")

save_png(
  (p_gantt / (p_bot | p_rec)) +
    plot_annotation(
      title = "Resources — deployment timeline, utilization, bottleneck detection and recommendations",
      theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
    ),
  "04_resources.png", w = 1400, h = 1100
)

# ---------------------------------------------------------------------------
# 05 — Optimiser (constraint cascade + Pareto frontier)
# ---------------------------------------------------------------------------
cat("\n[05] Optimiser — constraint cascade...\n")
cascade <- analyse_constraint_cascade(
  historical_wells = HISTORICAL,
  assumptions      = ASSUMPTIONS,
  n_wells          = 24L,
  base_config      = list(
    frac_fleets = 1L, wireline_units = 1L, ct_units = 1L,
    milling_units = 2L, testing_units = 2L, frac_trees = 1L,
    operation_mode = "Conventional", allow_ct_for_milling = FALSE
  ),
  cascade_iterations = 200L,
  max_steps          = 5L,
  seed               = 42L
)

cat("\n[05] Optimiser — Pareto grid search...\n")
scenario_grid <- expand.grid(
  operation_mode      = c("Conventional", "Zipper"),
  frac_fleets         = 1L,
  wireline_units      = 1:2,
  ct_units            = 1L,
  milling_units       = 2:3,
  testing_units       = 2L,
  frac_trees          = c(1L, 2L),
  allow_ct_for_milling = FALSE,
  stringsAsFactors    = FALSE
) %>%
  filter(!(operation_mode == "Conventional" & frac_trees == 2L)) %>%
  filter(!(operation_mode == "Zipper"        & frac_trees == 1L))

optim_results <- optimise_campaign_scenarios(
  historical_wells  = HISTORICAL,
  assumptions       = ASSUMPTIONS,
  n_wells           = 24L,
  scenario_grid     = scenario_grid,
  screen_iterations = 100L,
  refine_iterations = 300L,
  top_n_refine      = 4L,
  seed              = 42L
)

# Angled step labels for the composite: at half-PNG width the horizontal
# labels ("Add 1 Testing unit (-> 3 units)") run into each other.
p_cascade <- plot_constraint_cascade(cascade) +
  theme(axis.text.x = element_text(angle = 18, hjust = 1))
p_cas_util <- plot_cascade_utilization(cascade)
p_pareto  <- plot_pareto_frontier(optim_results)

save_png(
  ((p_cascade | p_cas_util) / p_pareto) +
    plot_annotation(
      title = "Optimiser — constraint cascade analyser and Pareto scenario search",
      theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
    ),
  "05_optimiser.png", w = 1400, h = 1100
)

cat("\nAll tab screenshots generated successfully.\n")
