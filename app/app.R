# app.R
# Version 16: decision-support additions on top of v15's readability fixes.
#   - Readiness value box now explains itself: the two weakest scoring drivers
#     are shown under the score.
#   - "Critical bottleneck" narrative card: which resource, P90 utilization,
#     expected schedule impact, recommended action.
#   - "Where to spend next" investment ranking: net benefit and benefit/cost
#     of each proposed resource addition (schedule value vs incremental unit
#     cost), answering "if I spend more, where?"
#   - Idle frac fleet cost box shows the underlying idle days.
#   - Audit & Data gains an input-fidelity check (simulated vs historical
#     milling distribution; sidebar frac rate vs historical mean).

library(shiny)
library(bslib)
library(readr)
library(dplyr)
library(ggplot2)
library(DT)
library(janitor)

`%||%` <- function(x, y) if (is.null(x)) y else x

if (basename(getwd()) == "app") {
  project_root <- normalizePath(file.path(getwd(), ".."), mustWork = FALSE)
} else {
  project_root <- normalizePath(getwd(), mustWork = FALSE)
}

source(file.path(project_root, "R", "load_inputs.R"))
source(file.path(project_root, "R", "validate_inputs.R"))
source(file.path(project_root, "R", "simulation_engine_fast.R"))
source(file.path(project_root, "R", "optimiser_parallel.R"))
source(file.path(project_root, "R", "risk_uncertainty.R"))
source(file.path(project_root, "R", "bottleneck_explain.R"))
source(file.path(project_root, "R", "recommendations.R"))
source(file.path(project_root, "R", "robustness.R"))
source(file.path(project_root, "R", "scenario_library.R"))
source(file.path(project_root, "R", "narrative_engine.R"))
source(file.path(project_root, "R", "report_decision_page.R"))
source(file.path(project_root, "R", "plots.R"))

safe_round_df <- function(df, digits = 2) {
  df %>% mutate(across(where(is.numeric), ~ round(.x, digits)))
}

engine_has_progress <- "progress_callback" %in% names(formals(simulate_campaign_detailed))

# Compact display formats for value boxes.
fmt_days_short <- function(x) {
  if (length(x) == 0 || is.na(x)) return("N/A")
  paste0(format(round(x, 1), big.mark = ","), " d")
}
fmt_money_short <- function(x) {
  if (length(x) == 0 || is.na(x)) return("N/A")
  ax <- abs(x)
  if (ax >= 1e9) paste0("$", round(x / 1e9, 2), "B")
  else if (ax >= 1e6) paste0("$", round(x / 1e6, 2), "M")
  else if (ax >= 1e3) paste0("$", round(x / 1e3, 0), "k")
  else paste0("$", round(x, 0))
}

plot_card <- function(header, output_id, height = "440px") {
  card(
    full_screen = TRUE,
    card_header(header),
    card_body(plotOutput(output_id, height = height), padding = 8)
  )
}

# Guaranteed fix for blank DT tables in bslib fill containers:
# wrap every DTOutput in a div with explicit height so the flex layout
# cannot collapse it to zero. card_body fill=FALSE alone is insufficient
# on bslib 0.6.x when navset_card_underline is itself a fill container.
dt_wrap <- function(output_id, height = "400px") {
  tags$div(style = paste0("height:", height, "; overflow-y:auto; overflow-x:auto;"),
    DTOutput(output_id))
}
table_card <- function(header, ..., height = "400px") {
  args <- list(...)
  card(
    full_screen = TRUE,
    card_header(header),
    card_body(fill = FALSE, ...)
  )
}

# --- UI ----------------------------------------------------------------------

ui <- page_sidebar(
  title = "Frac Campaign Planning Simulator",
  theme = bs_theme(version = 5, preset = "shiny"),
  fillable = FALSE,

  sidebar = sidebar(
    width = 340,
    actionButton("run", "Run simulation", class = "btn-primary btn-lg w-100"),
    uiOutput("status_message"),
    uiOutput("download_ui"),
    accordion(
      open = c("Input files", "Scenario"),
      accordion_panel(
        "Input files",
        fileInput("historical_file", "historical_wells.csv (optional)", accept = ".csv"),
        helpText(class = "text-muted small mt-0",
          "If omitted, synthetic baseline data is used (clearly flagged). ",
          "Recommended: 20+ historical wells for calibrated estimates."),
        fileInput("assumption_file", "master_risks_assumptions.csv", accept = ".csv")
      ),
      accordion_panel(
        "Scenario",
        selectInput("n_wells", "Campaign size", choices = c(20, 30, 40), selected = 30),
        selectInput("operation_mode", "Operation mode",
                    choices = c("Compare both", "Conventional", "Zipper"),
                    selected = "Compare both"),
        selectInput("execution_mode", "Execution mode",
                    choices = c("Fast", "Standard", "Audit"), selected = "Standard"),
        helpText("Fast: 300 runs, no per-well audit detail. Standard: 1,000 runs. Audit: 2,000 runs with full traceability."),
        numericInput("n_iter", "Simulation runs", value = 1000, min = 100, max = 10000, step = 100),
        numericInput("seed", "Simulation seed", value = 123, min = 1, max = 999999, step = 1),
        sliderInput("risk_multiplier", "Risk multiplier", min = 0.25, max = 3, value = 1, step = 0.25),
        numericInput("target_days", "Target duration (days, optional)", value = NA, min = 1, step = 1),
        numericInput("budget", "Budget ceiling ($, optional)", value = NA, min = 0, step = 1000000)
      ),
      accordion_panel(
        "Resources",
        numericInput("frac_fleets", "Frac fleets", value = 1, min = 1, max = 5, step = 1),
        numericInput("wireline_units", "Wireline units", value = 1, min = 1, max = 5, step = 1),
        numericInput("ct_units", "CT / cleanout units", value = 1, min = 1, max = 5, step = 1),
        numericInput("milling_units", "Milling units", value = 1, min = 1, max = 5, step = 1),
        numericInput("frac_trees", "Frac trees available", value = 2, min = 1, max = 10, step = 1),
        helpText("2 = basic zipper. 3 = lower swap delay (~5%). 4+ = further reduction (~10%)."),
        sliderInput("zipper_efficiency", "Zipper execution factor",
                    min = 0.5, max = 1.0, value = 0.75, step = 0.05),
        helpText("0.75 means frac execution is 25% faster than conventional."),
        numericInput("frac_tree_swap_delay_hours", "Frac tree swap delay, h", value = 4, min = 0, max = 48, step = 0.5),
        helpText("Transition delay per well between zipper pairs when only 2 trees available."),
        numericInput("testing_units", "Testing units (flowback / plug milling)", value = 1, min = 1, max = 5, step = 1),
        numericInput("flowback_testing_days_min", "Flowback + testing, min days", value = 7, min = 1, max = 30, step = 1),
        numericInput("flowback_testing_days_max", "Flowback + testing, max days", value = 10, min = 1, max = 30, step = 1),
        checkboxInput("allow_ct_for_milling", "Allow CT units to support milling", value = FALSE),
        sliderInput("ct_milling_efficiency", "CT milling efficiency", min = 0.3, max = 0.9, value = 0.65, step = 0.05),
        helpText("Fraction of CT capacity equivalent to one milling unit when reassigned.")
      ),
      accordion_panel(
        "Operation timing",
        numericInput("wireline_time_per_stage_hours", "Wireline time per stage, h", value = 6, min = 0, max = 72, step = 0.5),
        numericInput("wireline_rig_up_down_hours", "Wireline rig up/down, h", value = 12, min = 0, max = 120, step = 1),
        numericInput("wireline_contingency_pct", "Wireline contingency, %", value = 10, min = 0, max = 100, step = 5),
        numericInput("frac_time_per_stage_hours", "Stage cycle time, h",
          value = 12, min = 1, max = 120, step = 0.5),
        helpText(class = "text-muted small mt-0 mb-1",
          "Full stage-to-stage cycle: pump time + fluid loading + iron rig-up/down + pressure test + flush."),
        numericInput("frac_settling_time_hours", "Settling before next wireline run, h",
          value = 2, min = 0, max = 72, step = 0.5),
        numericInput("well_to_well_transition_hours", "Within-pad well transition, h",
          value = 4, min = 0, max = 48, step = 0.5),
        helpText(class = "text-muted small mt-0 mb-1",
          "Short move between adjacent wellheads on the same pad. Applied for every non-first well on a pad."),
        numericInput("pad_to_pad_move_hours", "Pad-to-pad move, h",
          value = 24, min = 0, max = 168, step = 1),
        helpText(class = "text-muted small mt-0 mb-1",
          "Full rig-down, transport and rig-up when moving to a new pad. Applied once for the first well on each new pad.")
      ),
      accordion_panel(
        "Daily costs",
        numericInput("frac_fleet_cost", "Frac fleet $/day", value = 250000, min = 0, step = 10000),
        numericInput("wireline_cost", "Wireline $/day", value = 15000, min = 0, step = 1000),
        numericInput("ct_cost", "CT / cleanout $/day", value = 25000, min = 0, step = 1000),
        numericInput("milling_cost", "Milling $/day", value = 18000, min = 0, step = 1000),
        numericInput("testing_unit_cost", "Testing unit $/day", value = 12000, min = 0, step = 500)
      )
    )
  ),

  navset_card_underline(
    id = "main_nav",

    nav_panel(
      "Overview",
      layout_columns(
        fill = FALSE, col_widths = c(4, 4, 4),
        value_box(
          title = "Best option",
          value = textOutput("vb_best"),
          theme = "primary"
        ),
        value_box(
          title = "P50 duration (best option)",
          value = textOutput("vb_p50"),
          p(textOutput("vb_p90", inline = TRUE))
        ),
        value_box(
          title = "Zipper P50 saving",
          value = textOutput("vb_saving"),
          p(textOutput("vb_saving_pct", inline = TRUE)),
          theme = "info"
        )
      ),
      layout_columns(
        fill = FALSE, col_widths = c(4, 4, 4),
        value_box(
          title = "Readiness (lowest mode)",
          value = textOutput("vb_readiness"),
          uiOutput("vb_readiness_status"),
          uiOutput("vb_readiness_drivers")
        ),
        value_box(
          title = "Campaign bottleneck",
          value = textOutput("vb_bottleneck"),
          uiOutput("vb_bottleneck_sub"),
          theme = "secondary"
        ),
        value_box(
          title = "Frac fleet idle cost",
          value = textOutput("vb_idle_cost"),
          uiOutput("vb_idle_context"),
          theme = "warning"
        )
      ),
      # Zipper benefit breakdown card (only shown when both modes simulated)
      uiOutput("synthetic_data_banner"),
      uiOutput("total_cost_card"),
      uiOutput("zipper_benefit_card"),

      layout_columns(
        col_widths = c(5, 7),
        card(
          full_screen = TRUE,
          card_header("Critical bottleneck"),
          uiOutput("bottleneck_narrative")
        ),
        card(
          full_screen = TRUE,
          card_header("Where to spend next"),
          card_body(fill = FALSE, dt_wrap("investment_table", "300px")),
          card_footer(tags$small(class = "text-muted",
            "Schedule value = P50 days saved x total daily spread rate. ",
            "Incremental cost = added unit's day rate x resulting P50 duration. ",
            "Planning-level estimates - review against contract rates."))
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        plot_card("Campaign duration S-curve", "scurve_plot", "430px"),
        plot_card("Duration distribution", "duration_plot", "430px")
      ),
      layout_columns(
        col_widths = c(6, 6),
        table_card("Traffic lights", dt_wrap("traffic_light_table", "200px")),
        table_card("Executive summary", dt_wrap("executive_summary_table", "380px"))
      )
    ),

    nav_panel(
      "Decision support",
      layout_columns(
        col_widths = c(7, 5),
        card(full_screen = TRUE, card_header("Management summary"),
             card_body(uiOutput("decision_narrative"))),
        card(full_screen = TRUE, card_header("Recommendation"),
             card_body(
               actionButton("verify_rec", "Verify by re-simulation", class = "btn-sm btn-primary mb-2"),
               uiOutput("recommendation_confidence"),
               verbatimTextOutput("recommendation_panel")))
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(full_screen = TRUE, card_header("Risk prediction"),
             card_body(DT::DTOutput("risk_table"))),
        card(full_screen = TRUE, card_header("Uncertainty (P-values)"),
             card_body(DT::DTOutput("uncertainty_table")))
      ),
      card(full_screen = TRUE, card_header("Constraint cascade - what to relieve next"),
           card_body(DT::DTOutput("bottleneck_cascade_table"))),

      card(
        full_screen = TRUE,
        card_header("Recommendation robustness - is it sensitive to the planning assumptions?"),
        card_body(
          p(class = "text-muted",
            "One-at-a-time check: nudges each assumption below by +-15% (holding everything ",
            "else fixed) and re-runs a reduced-iteration simulation to see whether the ",
            "recommended action and readiness verdict still hold."),
          actionButton("run_robustness", "Check robustness",
                       class = "btn-sm btn-primary mb-2", icon = icon("arrows-left-right")),
          uiOutput("robustness_summary"),
          tags$h6("Best / base / stress case", class = "mt-3"),
          p(class = "text-muted",
            "All assumptions shifted together: \"Best case\" = all -15% (favourable), ",
            "\"Stress case\" = all +15% (unfavourable)."),
          DT::DTOutput("combined_scenario_table"),
          tags$h6("P50 sensitivity by assumption", class = "mt-3"),
          plotOutput("robustness_tornado_plot", height = "300px"),
          tags$h6("Per-assumption detail (one-at-a-time)", class = "mt-3"),
          DT::DTOutput("robustness_table")
        )
      )
    ),

    nav_panel(
      "Scenario library",
      card(
        card_header("Save current scenario"),
        card_body(
          p(class = "text-muted",
            "Snapshots the current run's configuration and key results so you can ",
            "compare it against other configurations. If \"Compare both\" is selected, ",
            "saves one entry per operation mode. Library holds up to ",
            sprintf("%d scenarios", SCENARIO_LIBRARY_MAX), " (oldest is dropped once full)."),
          layout_columns(
            col_widths = c(8, 4),
            textInput("scenario_label", NULL, placeholder = "Optional label (default: config summary)"),
            actionButton("save_scenario", "Save scenario",
                         class = "btn-sm btn-primary", icon = icon("bookmark"))
          )
        )
      ),
      card(
        full_screen = TRUE,
        card_header("Saved scenarios"),
        card_body(
          actionButton("remove_scenario", "Remove selected",
                       class = "btn-sm btn-outline-danger mb-2", icon = icon("trash")),
          actionButton("clear_scenarios", "Clear all",
                       class = "btn-sm btn-outline-secondary mb-2", icon = icon("xmark")),
          DT::DTOutput("scenario_library_table")
        )
      ),
      plot_card("Saved scenarios - duration comparison", "scenario_comparison_plot", "430px")
    ),

    nav_panel(
      "Risks",
      plot_card("Expected schedule impact per campaign (tornado)", "tornado_plot", "460px"),
      plot_card("Consequence propagation: direct delay vs induced workload", "consequence_plot", "480px"),
      table_card("Consequence detail by risk", dt_wrap("consequence_table", "400px")),
      plot_card("Top delay contributors", "delay_plot", "460px"),
      plot_card("Expected stage-level risk events", "stage_risk_plot", "460px"),
      table_card("Stage-level risk detail", dt_wrap("stage_risk_table", "400px")),
      table_card("Delay contributor detail", dt_wrap("delay_table", "400px"))
    ),

    nav_panel(
      "Resources",
      plot_card("Resource utilization: active days and utilization by mode", "gantt_plot", "420px"),
      plot_card("Mean resource utilization", "resource_plot", "420px"),
      plot_card("Bottleneck detection (P90 utilization)", "bottleneck_plot", "420px"),
      plot_card("Estimated schedule improvement from additional resources", "recommendation_plot", "420px"),
      plot_card("Estimated resource and idle cost impact", "cost_impact_plot", "420px"),
      table_card("Recommended actions", dt_wrap("recommendation_table", "320px")),
      table_card("Cost impact detail", dt_wrap("cost_impact_table", "400px")),
      table_card("Utilization detail", dt_wrap("resource_table", "300px")),
      table_card("Bottleneck detail", dt_wrap("bottleneck_table", "300px"))
    ),

    nav_panel(
      "Wireline & Readiness",
      plot_card("Wireline stage-readiness constraint", "wireline_constraint_plot", "460px"),
      plot_card("Campaign readiness score", "readiness_plot", "380px"),
      table_card("Wireline constraint detail", dt_wrap("wireline_constraint_table", "220px")),
      table_card("Readiness breakdown", dt_wrap("readiness_table", "220px"))
    ),

    nav_panel(
      "Optimiser",

      # --- Constraint cascade (primary) ---
      card(
        card_header("Constraint cascade — what limits you and where to spend next"),
        p(class = "text-muted",
          "Starts from your current sidebar settings, identifies the binding constraint, ",
          "adds one unit of that resource, re-runs, and repeats. ",
          "Answers: (1) what limits you today, (2) what limits you after you fix it, ",
          "(3) where each additional dollar generates the most schedule return."),
        layout_columns(
          col_widths = c(3, 9),
          numericInput("cascade_max_steps", "Max steps", value = 6, min = 2, max = 10, step = 1),
          helpText("Each step adds one unit of the binding resource and re-runs 300 iterations. 6 steps ≈ 1-2 min.")
        ),
        actionButton("run_cascade", "Run constraint cascade",
                     class = "btn-warning btn-lg w-100", icon = icon("arrow-right"))
      ),
      layout_columns(
        col_widths = c(8, 4),
        plot_card("P50 after each fix (error bars = P10/P90)", "cascade_waterfall", "440px"),
        plot_card("Binding constraint at each step", "cascade_util_plot", "440px")
      ),
      card(
        full_screen = TRUE,
        card_header("Cascade detail — cost and ROI of each fix"),
        card_body(fill = FALSE, dt_wrap("cascade_table", "320px")),
        card_footer(tags$small(class = "text-muted",
          "ROI = days saved per $1M invested. Schedule value = days saved × total daily spread rate. ",
          "Cascade uses 300 iterations per step; run the full simulation after applying settings."))
      ),

      tags$hr(),
      tags$h5("Grid-search optimiser (Pareto frontier)", class = "mt-3 mb-2"),
      tags$p(class = "text-muted",
        "Exhaustive search over all resource combinations. Use this to explore non-obvious ",
        "multi-resource combinations beyond the greedy cascade path."),

      card(
        card_header("Find the optimum scenario"),
        p(class = "text-muted mb-2",
          "Grid-search over resource configurations. Ranked on total mobilisation cost ",
          "(all contracted units × day rate × P50 duration), which embeds both ",
          "schedule and idle time. Two stages: fast screen of every configuration, ",
          "then refinement of the top candidates."),
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          sliderInput("opt_frac_range", "Frac fleets", min = 1, max = 4, value = c(1, 2), step = 1),
          sliderInput("opt_wl_range", "Wireline units", min = 1, max = 4, value = c(1, 2), step = 1),
          sliderInput("opt_ct_range", "CT units", min = 1, max = 4, value = c(1, 1), step = 1),
          sliderInput("opt_mill_range", "Milling units", min = 1, max = 4, value = c(1, 3), step = 1)
        ),
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          sliderInput("opt_test_range", "Testing units", min = 1, max = 4, value = c(1, 3), step = 1),
          checkboxGroupInput("opt_modes", "Operation modes",
                             choices = c("Conventional", "Zipper"),
                             selected = c("Conventional", "Zipper"), inline = TRUE),
          checkboxInput("opt_ct_milling", "Also test CT-supports-milling variants", value = FALSE),
          numericInput("opt_screen_iter", "Screening iterations", value = 150, min = 50, max = 500, step = 50)
        ),
        uiOutput("opt_grid_size"),
        actionButton("run_optimiser", "Run optimiser", class = "btn-success btn-lg")
      ),
      layout_columns(
        col_widths = c(5, 7),
        card(
          full_screen = TRUE,
          card_header("Recommended scenario"),
          uiOutput("opt_recommendation"),
          uiOutput("opt_apply_ui")
        ),
        plot_card("Trade-off frontier", "pareto_plot", "440px")
      ),
      card(
        full_screen = TRUE,
        card_header("All scenarios (sorted by total cost)"),
        card_body(fill = FALSE, dt_wrap("opt_results_table", "480px")),
        card_footer(downloadButton("download_optimiser", "Download optimiser results CSV"))
      )
    ),

    nav_panel(
      "Workflow",

      card(
        card_header("Operational sequence — what the simulator models"),
        p(class = "text-muted",
          "Every modelled activity, its resource, duration source, and scheduling rule. ",
          "This is the authoritative reference for what the engine computes."),
        card_body(fill = FALSE, dt_wrap("workflow_table", "460px")),
        card_footer(tags$small(class = "text-muted",
          "Phase colours: ",
          tags$span(style="color:#2980B9;font-weight:bold","pre_frac"), " | ",
          tags$span(style="color:#E6A817;font-weight:bold","frac_stage"), " | ",
          tags$span(style="color:#D55E00;font-weight:bold","post_frac"),
          " — parallel = absorbed into available time if not the pacing resource"))
      ),

      card(
        card_header("Key scheduling rules"),
        card_body(fill = FALSE,
          layout_columns(
            col_widths = c(6, 6),
            div(
              tags$h6(class = "fw-bold text-primary", "CT parallelism rule"),
              tags$p(class = "small mb-1", tags$strong("Conventional: "),
                "CT cleanout runs in parallel with frac on the previous well. ",
                tags$code("frac_related = max(CT, frac + wireline)"), ". ",
                "CT only gates campaign if CT workload > frac + wireline per well."),
              tags$p(class = "small", tags$strong("Zipper: "),
                "CT precedes each well (less overlap available). ",
                tags$code("frac_related = CT + max(frac, wireline)"), "."),
              tags$hr(class = "my-2"),
              tags$h6(class = "fw-bold text-primary", "SCMT offline rule"),
              tags$p(class = "small mb-1", tags$strong("wireline_units ≥ 2: "),
                "SCMT always runs offline — spare wireline unit available."),
              tags$p(class = "small", tags$strong("wireline_units = 1: "),
                "SCMT offline probability from assumptions CSV (default 80%). ",
                "Set to 1.0 to always run offline.")
            ),
            div(
              tags$h6(class = "fw-bold text-primary", "Well and pad transitions"),
              tags$p(class = "small mb-1", tags$strong("Within-pad move: "),
                "Short skid to adjacent wellhead. Applies to every non-first well on a pad. ",
                "Set in sidebar: Within-pad well transition, h."),
              tags$p(class = "small mb-1", tags$strong("Pad-to-pad move: "),
                "Full rig-down + transport + rig-up. Applies once per pad (first well only). ",
                "Set in sidebar: Pad-to-pad move, h."),
              tags$p(class = "small text-muted",
                "In zipper: within-pad transition is halved ",
                "(frac tree swap delay covers most of it)."),
              tags$hr(class = "my-2"),
              tags$h6(class = "fw-bold text-primary", "Post-frac discrete scheduler"),
              tags$p(class = "small",
                "Milling starts at frac release (not after all frac completes). ",
                "Testing unit held during both milling and flowback. ",
                "Runs in parallel with ongoing frac on subsequent wells.")
            )
          )
        )
      ),

      card(
        card_header("Resource deployment phases"),
        card_body(fill = FALSE, dt_wrap("workflow_resource_table", "200px"))
      ),

      card(
        card_header("How to change the workflow"),
        card_body(fill = FALSE,
          tags$table(class = "table table-sm table-bordered small",
            tags$thead(class = "table-light",
              tags$tr(
                tags$th("What to change"), tags$th("How"), tags$th("Where")
              )
            ),
            tags$tbody(
              tags$tr(tags$td("Stage cycle time (pump + turnaround)"),
                tags$td("Adjust sidebar input"),
                tags$td(tags$code("Sidebar → Stage cycle time, h"))),
              tags$tr(tags$td("Well-to-well or pad-to-pad transition time"),
                tags$td("Adjust sidebar inputs"),
                tags$td(tags$code("Sidebar → Operation timing"))),
              tags$tr(tags$td("SCMT always offline"),
                tags$td("Set SCMT offline probability to 1.0"),
                tags$td(tags$code("master_risks_assumptions.csv"))),
              tags$tr(tags$td("No CT cleanout / scraper run"),
                tags$td("Set Scraper / cleanout run durations to 0"),
                tags$td(tags$code("master_risks_assumptions.csv"))),
              tags$tr(tags$td("No temperature logging"),
                tags$td("Set temperature log duration rows to 0"),
                tags$td(tags$code("master_risks_assumptions.csv"))),
              tags$tr(tags$td("Change activity durations"),
                tags$td("Edit Min / ML / Max days for the relevant row"),
                tags$td(tags$code("master_risks_assumptions.csv"))),
              tags$tr(tags$td("Change activity-to-resource mapping"),
                tags$td("Edit resource column + update RESOURCE_CLASS_CONFIG"),
                tags$td(tags$code("WORKFLOW_CONFIG in simulation_engine.R"))),
              tags$tr(tags$td("Add a new activity to the sequence"),
                tags$td("Add row to WORKFLOW_CONFIG + update workload formula"),
                tags$td(tags$code("simulation_engine.R"))),
              tags$tr(tags$td("Custom sequence from file"),
                tags$td("Place workflow_config.csv alongside assumptions CSV"),
                tags$td(tags$code("workflow_config.csv (see template)")))
            )
          )
        )
      )
    ),


    nav_panel(
      "Audit & Data",
      plot_card("Input fidelity check", "validation_plot", "420px"),
      table_card("Executive KPIs", dt_wrap("executive_kpi_table", "280px")),
      table_card("All simulation results", dt_wrap("results_table", "480px")),
      table_card("Well details",
        uiOutput("simulation_selector_ui"),
        dt_wrap("well_details_table", "480px")
      ),
      table_card("Risk event log",
        uiOutput("risk_selector_ui"),
        dt_wrap("risk_event_table", "480px")
      ),
      table_card("Assumptions used", dt_wrap("assumptions_table", "480px")),
      card(card_header("Input check"), verbatimTextOutput("input_check"))
    )
  )
)

# --- Server ------------------------------------------------------------------

server <- function(input, output, session) {

  observeEvent(input$execution_mode, {
    target <- switch(input$execution_mode, Fast = 300, Standard = 1000, Audit = 2000)
    updateNumericInput(session, "n_iter", value = target)
  }, ignoreInit = TRUE)

  input_data <- reactive({
    req(input$assumption_file)
    tryCatch({
      # Historical file is optional: fall back to synthetic data if not supplied
      using_synthetic <- is.null(input$historical_file)
      historical <- if (using_synthetic) {
        synthetic_historical_wells(n = 30, seed = 42)
      } else {
        load_historical_wells(input$historical_file$datapath) %>%
          validate_historical_wells()
      }
      assumptions <- load_master_assumptions(input$assumption_file$datapath) %>%
        validate_assumptions()
      list(ok = TRUE, historical = historical, assumptions = assumptions,
           error = NULL, using_synthetic = using_synthetic)
    }, error = function(e) {
      list(ok = FALSE, historical = NULL, assumptions = NULL,
           error = conditionMessage(e), using_synthetic = FALSE)
    })
  })

  output$status_message <- renderUI({
    if (is.null(input$assumption_file)) {
      return(tags$small(class = "text-muted",
        "Upload master_risks_assumptions.csv to begin. Historical file is optional."))
    }
    dat <- input_data()
    if (!isTRUE(dat$ok)) {
      return(tags$small(class = "text-danger fw-bold", paste("Input error:", dat$error)))
    }
    if (isTRUE(dat$using_synthetic)) {
      tagList(
        tags$small(class = "text-warning fw-bold",
          "⚠ Using synthetic baseline data (no historical_wells.csv uploaded)."),
        tags$br(),
        tags$small(class = "text-muted",
          sprintf("%d synthetic wells | %d assumption rows. ",
                  nrow(dat$historical), nrow(dat$assumptions)),
          "Upload historical_wells.csv for calibrated estimates.")
      )
    } else {
      tags$small(class = "text-success fw-bold",
        sprintf("Inputs loaded: %d historical wells, %d assumption rows. Ready to run.",
                nrow(dat$historical), nrow(dat$assumptions)))
    }
  })

  sim_results <- eventReactive(input$run, {
    req(input$assumption_file)
    dat <- input_data()
    validate(need(isTRUE(dat$ok), paste("Fix input files first:", dat$error)))

    modes <- if (input$operation_mode == "Compare both") c("Conventional", "Zipper") else input$operation_mode
    keep_full_logs <- input$execution_mode != "Fast"

    tryCatch({
      withProgress(message = "Running simulation", value = 0, {
        detailed_runs <- lapply(seq_along(modes), function(mode_index) {
          base_frac <- (mode_index - 1) / length(modes)
          args <- list(
            historical_wells = dat$historical,
            assumptions = dat$assumptions,
            n_wells = as.integer(input$n_wells),
            n_iterations = as.integer(input$n_iter),
            frac_fleets = input$frac_fleets,
            milling_units = input$milling_units,
            wireline_units = input$wireline_units,
            ct_units = input$ct_units,
            frac_trees = input$frac_trees,
            operation_mode = modes[[mode_index]],
            zipper_efficiency = input$zipper_efficiency,
            risk_multiplier = input$risk_multiplier,
            wireline_time_per_stage_hours = input$wireline_time_per_stage_hours,
            wireline_rig_up_down_hours = input$wireline_rig_up_down_hours,
            wireline_contingency_pct = input$wireline_contingency_pct,
            frac_time_per_stage_hours = input$frac_time_per_stage_hours,
            frac_settling_time_hours = input$frac_settling_time_hours,
            well_to_well_transition_hours = input$well_to_well_transition_hours,
            pad_to_pad_move_hours = input$pad_to_pad_move_hours,
            frac_tree_swap_delay_hours = input$frac_tree_swap_delay_hours,
            allow_ct_for_milling = isTRUE(input$allow_ct_for_milling),
            ct_milling_efficiency = input$ct_milling_efficiency,
            testing_units = input$testing_units,
            flowback_testing_days_min = input$flowback_testing_days_min,
            flowback_testing_days_max = input$flowback_testing_days_max,
            seed = as.integer(input$seed) + (mode_index - 1L)  # mode 1 = base seed; aligns with optimiser
          )
          if (engine_has_progress) {
            args$progress_callback <- function(i, n) {
              setProgress(base_frac + (i / n) / length(modes),
                          detail = sprintf("%s: %d / %d", modes[[mode_index]], i, n))
            }
          } else {
            setProgress(base_frac, detail = modes[[mode_index]])
          }
          res <- do.call(simulate_campaign_detailed, args)
          args$progress_callback <- NULL  # strip closure so args can be safely re-run later
          list(result = res, args = args)
        })

        results_only <- lapply(detailed_runs, `[[`, "result")
        args_by_mode <- setNames(lapply(detailed_runs, `[[`, "args"), modes)

        list(
          summary = bind_rows(lapply(results_only, `[[`, "summary")),
          well_details = if (keep_full_logs) bind_rows(lapply(results_only, `[[`, "well_details")) else tibble(),
          risk_event_log = bind_rows(lapply(results_only, `[[`, "risk_event_log")),
          resource_utilization = bind_rows(lapply(results_only, `[[`, "resource_utilization")),
          assumptions_used = bind_rows(lapply(results_only, `[[`, "assumptions_used")) %>% distinct(),
          args_by_mode = args_by_mode
        )
      })
    }, error = function(e) {
      showNotification(paste("Simulation error:", conditionMessage(e)), type = "error", duration = NULL)
      NULL
    })
  })

  # --- Derived tables: each computed ONCE per run ----------------------------

  resource_summary_r <- reactive({ req(sim_results()); summarise_resource_utilization(sim_results()$resource_utilization) })
  bottlenecks_r      <- reactive({ summarise_bottlenecks(resource_summary_r()) })
  sim_stats_r        <- reactive({ req(sim_results()); summarise_simulation(sim_results()$summary) })
  delay_r            <- reactive({ req(sim_results()); summarise_delay_contributors(sim_results()$risk_event_log) })
  stage_risk_r       <- reactive({ req(sim_results()); summarise_stage_level_risks(sim_results()$risk_event_log, sim_results()$summary) })
  wireline_r         <- reactive({ req(sim_results()); summarise_wireline_constraint(sim_results()$summary, sim_results()$well_details) })
  traffic_r          <- reactive({ req(sim_results()); build_traffic_lights(sim_results()$summary, sim_results()$risk_event_log, sim_results()$resource_utilization) })
  readiness_r        <- reactive({ req(sim_results()); build_readiness_score(sim_results()$summary, sim_results()$risk_event_log, sim_results()$resource_utilization) })
  recommendations_r  <- reactive({ req(sim_results()); build_resource_recommendations(sim_results()$summary, sim_results()$resource_utilization) })
  cost_impact_r      <- reactive({
    req(sim_results())
    build_cost_impact(sim_results()$summary, sim_results()$resource_utilization,
                      frac_fleet_cost_per_day = input$frac_fleet_cost,
                      wireline_cost_per_day = input$wireline_cost,
                      ct_cost_per_day = input$ct_cost,
                      milling_cost_per_day = input$milling_cost,
                      testing_unit_cost_per_day = input$testing_unit_cost)
  })
  kpis_r <- reactive({
    req(sim_results())
    build_executive_kpis(sim_results()$summary, sim_results()$risk_event_log,
                         sim_results()$resource_utilization,
                         frac_fleet_cost_per_day = input$frac_fleet_cost)
  })
  investment_r <- reactive({
    req(sim_results())
    build_investment_ranking(sim_results()$summary, recommendations_r(),
                             frac_fleet_cost_per_day = input$frac_fleet_cost,
                             wireline_cost_per_day = input$wireline_cost,
                             ct_cost_per_day = input$ct_cost,
                             milling_cost_per_day = input$milling_cost,
                             testing_unit_cost_per_day = input$testing_unit_cost)
  })
  narrative_r <- reactive({ build_bottleneck_narrative(bottlenecks_r(), recommendations_r()) })

  # ---- V2.5 decision-support layer (#1 recommendation, #2 bottleneck, #4 risk, #6 uncertainty, #12 narrative) ----
  .na_to_null <- function(x) if (is.null(x) || length(x) == 0 || all(is.na(x))) NULL else x

  focus_mode_r <- reactive({
    req(sim_results())
    p <- sim_results()$summary %>% dplyr::group_by(operation_mode) %>%
      dplyr::summarise(p50 = stats::median(estimated_campaign_days), .groups = "drop")
    p$operation_mode[which.min(p$p50)]
  })
  uncertainty_v2_r <- reactive({
    req(sim_results())
    quantify_uncertainty(sim_results()$summary, sim_results()$resource_utilization,
      target_days = .na_to_null(input$target_days), budget = .na_to_null(input$budget))
  })
  risk_pred_r <- reactive({
    req(sim_results())
    predict_campaign_risks(sim_results()$summary, sim_results()$resource_utilization,
      target_days = .na_to_null(input$target_days))
  })
  bottleneck_explain_r <- reactive({
    req(sim_results()); explain_bottlenecks(sim_results()$summary, sim_results()$resource_utilization)
  })
  # Analytic recommendation (instant, no re-simulation) shown by default.
  rec_analytic_r <- reactive({
    req(sim_results())
    a <- sim_results()$args_by_mode[[focus_mode_r()]]
    recommend_action(sim_results(), sim_args = a, verify = FALSE)
  })
  # Verified recommendation is computed only when the user clicks "Verify".
  verified_rec_rv <- reactiveVal(NULL)
  observeEvent(sim_results(), { verified_rec_rv(NULL) }, ignoreNULL = FALSE)  # new run clears old verification
  observeEvent(input$verify_rec, {
    req(sim_results())
    a <- sim_results()$args_by_mode[[focus_mode_r()]]
    a$progress_callback <- NULL; a$keep_logs <- FALSE; a$collect_well_details <- FALSE
    withProgress(message = "Verifying recommendation by re-simulation", value = 0.4, {
      verified_rec_rv(recommend_action(sim_results(), sim_args = a, verify = TRUE))
    })
  })
  rec_v2_r <- reactive({
    req(sim_results())
    v <- verified_rec_rv(); if (is.null(v)) rec_analytic_r() else v
  })
  decision_narrative_r <- reactive({
    req(sim_results())
    a <- sim_results()$args_by_mode[[focus_mode_r()]]; a$progress_callback <- NULL
    generate_narrative(sim_results(), sim_args = a,
      target_days = .na_to_null(input$target_days), budget = .na_to_null(input$budget), rec = rec_v2_r())
  })

  # Recommendation robustness: OAT +-15% sweep over fixed sidebar assumptions,
  # computed only on demand (button-triggered) since it re-runs the simulation
  # ~10 times even at reduced iterations.
  robustness_rv <- reactiveVal(NULL)
  observeEvent(sim_results(), { robustness_rv(NULL) }, ignoreNULL = FALSE)  # new run clears old sweep
  observeEvent(input$run_robustness, {
    req(sim_results())
    a <- sim_results()$args_by_mode[[focus_mode_r()]]
    withProgress(message = "Checking recommendation robustness", value = 0.4, {
      robustness_rv(assess_recommendation_robustness(
        a,
        frac_fleet_cost_per_day = input$frac_fleet_cost,
        wireline_cost_per_day = input$wireline_cost,
        ct_cost_per_day = input$ct_cost,
        milling_cost_per_day = input$milling_cost,
        testing_unit_cost_per_day = input$testing_unit_cost
      ))
    })
  })
  output$robustness_summary <- renderUI({
    rb <- robustness_rv()
    if (is.null(rb)) return(tags$p(class = "text-muted",
      "Not yet run. Click \"Check robustness\" to test the current recommendation against +-15% swings in 5 key assumptions."))
    n_unstable <- sum(!rb$summary$stable)
    oat_line <- if (n_unstable == 0) {
      tags$p(class = "text-success",
        sprintf("One-at-a-time: \"%s\" holds across +-%.0f%% swings in all %d assumptions tested (n=%d iterations each).",
                rb$base$recommendation, 100 * rb$perturb_pct, nrow(rb$summary), rb$n_iterations))
    } else {
      tags$p(class = "text-warning",
        sprintf("One-at-a-time: the base recommendation is \"%s\", but it flips under a +-%.0f%% swing in %d of %d assumptions (n=%d iterations each) - see table for which ones.",
                rb$base$recommendation, 100 * rb$perturb_pct, n_unstable, nrow(rb$summary), rb$n_iterations))
    }
    stress <- rb$combined %>% dplyr::filter(scenario == "Stress case")
    stress_line <- if (isTRUE(stress$stable)) {
      tags$p(class = "text-success",
        sprintf("Combined stress case (all %d assumptions +-%.0f%% unfavourable together): recommendation unchanged, P50 %+.1f d vs base.",
                nrow(rb$summary), 100 * rb$perturb_pct, stress$delta_p50_days))
    } else {
      tags$p(class = "text-warning",
        sprintf("Combined stress case (all %d assumptions +-%.0f%% unfavourable together): recommendation changes to \"%s\", P50 %+.1f d vs base.",
                nrow(rb$summary), 100 * rb$perturb_pct, stress$recommendation, stress$delta_p50_days))
    }
    tagList(oat_line, stress_line)
  })
  output$robustness_table <- DT::renderDT({
    rb <- robustness_rv()
    if (is.null(rb)) return(DT::datatable(tibble(), options = list(dom = "t")))
    df <- rb$summary %>% dplyr::transmute(
      Assumption = assumption,
      `-15% value` = round(low_value, 3),
      `-15% P50 (d)` = round(low_p50_days, 1),
      `-15% Readiness` = low_readiness_status,
      `Base value` = round(base_value, 3),
      `Base P50 (d)` = round(base_p50_days, 1),
      `Base Readiness` = base_readiness_status,
      `+15% value` = round(high_value, 3),
      `+15% P50 (d)` = round(high_p50_days, 1),
      `+15% Readiness` = high_readiness_status,
      `Recommendation stable?` = ifelse(stable, "Yes", "No"),
      Note = note
    )
    DT::datatable(df, rownames = FALSE, options = list(dom = "t", scrollX = TRUE, pageLength = 10))
  })
  output$combined_scenario_table <- DT::renderDT({
    rb <- robustness_rv()
    if (is.null(rb)) return(DT::datatable(tibble(), options = list(dom = "t")))
    df <- rb$combined %>% dplyr::transmute(
      Scenario = as.character(scenario),
      `P50 (d)` = round(p50_days, 1),
      `P90 (d)` = round(p90_days, 1),
      `Delta P50 vs base (d)` = sprintf("%+.1f", delta_p50_days),
      Readiness = sprintf("%.0f (%s)", readiness_score, readiness_status),
      Recommendation = recommendation,
      `Vs. base` = ifelse(stable, "Unchanged", "Changed")
    )
    DT::datatable(df, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })

  output$robustness_tornado_plot <- renderPlot({
    plot_robustness_tornado(robustness_rv())
  }, res = 96)

  output$recommendation_confidence <- renderUI({
    req(sim_results())
    conf <- combine_recommendation_confidence(rec_v2_r(), robustness_rv())
    badge_class <- switch(conf$level,
      "High" = "bg-success", "Moderate" = "bg-warning text-dark",
      "Low" = "bg-danger", "Inconclusive" = "bg-secondary")
    tagList(
      tags$span(class = paste("badge", badge_class), conf$label),
      tags$ul(class = "mt-2 mb-2 small text-muted", lapply(conf$detail, tags$li))
    )
  })

  # --- Scenario library: save/compare run configurations -------------------
  scenario_library_rv <- reactiveVal(list())
  observeEvent(input$save_scenario, {
    req(sim_results())
    args_by_mode <- sim_results()$args_by_mode
    new_records <- lapply(names(args_by_mode), function(mode) {
      a <- args_by_mode[[mode]]; a$progress_callback <- NULL
      build_scenario_record(sim_results(), a, label = input$scenario_label,
        frac_fleet_cost_per_day = input$frac_fleet_cost,
        wireline_cost_per_day = input$wireline_cost,
        ct_cost_per_day = input$ct_cost,
        milling_cost_per_day = input$milling_cost,
        testing_unit_cost_per_day = input$testing_unit_cost)
    })
    scenario_library_rv(add_scenario_records(scenario_library_rv(), new_records))
    updateTextInput(session, "scenario_label", value = "")
  })
  observeEvent(input$remove_scenario, {
    sel <- input$scenario_library_table_rows_selected
    df <- scenario_library_to_df(scenario_library_rv())
    if (length(sel) > 0 && nrow(df) > 0) {
      ids_to_remove <- df$id[sel]
      current <- scenario_library_rv()
      for (id in ids_to_remove) current <- remove_scenario_record(current, id)
      scenario_library_rv(current)
    }
  })
  observeEvent(input$clear_scenarios, { scenario_library_rv(list()) })
  output$scenario_library_table <- DT::renderDT({
    df <- scenario_library_to_df(scenario_library_rv())
    if (nrow(df) == 0) return(DT::datatable(tibble(), options = list(dom = "t")))
    DT::datatable(df %>% dplyr::select(-id), rownames = FALSE,
                   options = list(dom = "t", scrollX = TRUE), selection = "multiple")
  })
  output$scenario_comparison_plot <- renderPlot({
    plot_scenario_comparison(scenario_library_rv())
  }, res = 96)

  output$decision_narrative   <- renderUI({ req(sim_results()); tags$p(decision_narrative_r()$narrative) })
  output$recommendation_panel <- renderText({ req(sim_results()); rec_v2_r()$panel })
  output$risk_table <- DT::renderDT({
    req(sim_results())
    df <- risk_pred_r() %>% dplyr::filter(operation_mode == focus_mode_r()) %>%
      dplyr::transmute(Risk = risk, Probability = sprintf("%.0f%%", 100 * probability),
                       `Expected delay (d)` = expected_delay_days, `P90 (d)` = p90_delay_days,
                       Likelihood = likelihood, Impact = impact)
    DT::datatable(df, rownames = FALSE, options = list(dom = "t", pageLength = 12))
  })
  output$uncertainty_table <- DT::renderDT({
    req(sim_results())
    df <- uncertainty_v2_r() %>% dplyr::transmute(
      Mode = operation_mode, P10 = round(p10_days), P50 = round(p50_days), P90 = round(p90_days),
      `P(by target)` = ifelse(is.na(prob_finish_by_target), "-", sprintf("%.0f%%", 100 * prob_finish_by_target)),
      `P(under budget)` = ifelse(is.na(prob_within_budget), "-", sprintf("%.0f%%", 100 * prob_within_budget)),
      `P(overload)` = ifelse(is.na(prob_resource_overload), "-", sprintf("%.0f%%", 100 * prob_resource_overload)),
      `P50 cost` = ifelse(is.na(p50_cost), "-", sprintf("$%.1fM", p50_cost / 1e6)))
    DT::datatable(df, rownames = FALSE, options = list(dom = "t"))
  })
  output$bottleneck_cascade_table <- DT::renderDT({
    req(sim_results())
    df <- bottleneck_explain_r()$cascade %>% dplyr::filter(operation_mode == focus_mode_r()) %>%
      dplyr::transmute(`Relief step` = pos, Resource = modal_resource,
                       `Recoverable (d)` = round(mean_gap_days, 1),
                       `Cumulative (d)` = round(cumulative_recoverable_days, 1))
    DT::datatable(df, rownames = FALSE, options = list(dom = "t"))
  })
  timeline_r  <- reactive({ req(sim_results()); build_resource_timeline(sim_results()$summary) })
  total_cost_r <- reactive({
    req(sim_results())
    build_total_campaign_cost(
      sim_results()$summary,
      sim_results()$resource_utilization,
      frac_fleets       = input$frac_fleets,
      wireline_units    = input$wireline_units,
      ct_units          = input$ct_units,
      milling_units     = input$milling_units,
      testing_units     = input$testing_units,
      frac_fleet_cost_per_day   = input$frac_fleet_cost,
      wireline_cost_per_day     = input$wireline_cost,
      ct_cost_per_day           = input$ct_cost,
      milling_cost_per_day      = input$milling_cost,
      testing_unit_cost_per_day = input$testing_unit_cost
    )
  })

  zipper_breakdown_r <- reactive({
    req(sim_results())
    build_zipper_benefit_breakdown(sim_results()$summary)
  })

  # Constraint cascade analyser: runs separately on demand (not auto on sim run)
  # because it requires multiple sequential simulation calls.
  cascade_results <- reactiveVal(NULL)

  observeEvent(input$run_cascade, {
    dat <- input_data()
    if (!isTRUE(dat$ok)) {
      showNotification("Fix input files before running cascade analysis.", type = "error")
      return()
    }
    base_cfg <- list(
      frac_fleets = input$frac_fleets,
      wireline_units = input$wireline_units,
      ct_units = input$ct_units,
      milling_units = input$milling_units,
      testing_units = input$testing_units,
      frac_trees = input$frac_trees,
      operation_mode = if (input$operation_mode == "Compare both") "Zipper" else input$operation_mode,
      allow_ct_for_milling = isTRUE(input$allow_ct_for_milling)
    )
    fixed <- list(
      zipper_efficiency = input$zipper_efficiency,
      risk_multiplier = input$risk_multiplier,
      wireline_time_per_stage_hours = input$wireline_time_per_stage_hours,
      wireline_rig_up_down_hours = input$wireline_rig_up_down_hours,
      wireline_contingency_pct = input$wireline_contingency_pct,
      frac_time_per_stage_hours = input$frac_time_per_stage_hours,
      frac_settling_time_hours = input$frac_settling_time_hours,
      well_to_well_transition_hours = input$well_to_well_transition_hours,
      pad_to_pad_move_hours = input$pad_to_pad_move_hours,
      frac_tree_swap_delay_hours = input$frac_tree_swap_delay_hours,
      ct_milling_efficiency = input$ct_milling_efficiency,
      flowback_testing_days_min = input$flowback_testing_days_min,
      flowback_testing_days_max = input$flowback_testing_days_max
    )
    res <- tryCatch({
      withProgress(message = "Analysing constraint cascade", value = 0, {
        setProgress(0.1, detail = "Step 0: current config")
        r <- analyse_constraint_cascade(
          historical_wells = dat$historical,
          assumptions = dat$assumptions,
          n_wells = as.integer(input$n_wells),
          base_config = base_cfg,
          fixed_args = fixed,
          frac_fleet_cost_per_day   = input$frac_fleet_cost,
          wireline_cost_per_day     = input$wireline_cost,
          ct_cost_per_day           = input$ct_cost,
          milling_cost_per_day      = input$milling_cost,
          testing_unit_cost_per_day = input$testing_unit_cost,
          cascade_iterations = 300,
          max_steps = input$cascade_max_steps,
          min_saving_days = 2,
          seed = as.integer(input$seed)
        )
        setProgress(1)
        r
      })
    }, error = function(e) {
      showNotification(paste("Cascade error:", conditionMessage(e)), type = "error", duration = NULL)
      NULL
    })
    cascade_results(res)
  })
  consequences_r <- reactive({ req(sim_results()); summarise_risk_consequences(sim_results()$risk_event_log, sim_results()$summary) })

  # Value-box data computed directly from results with compact formatting,
  # rather than parsing pre-formatted KPI strings.
  vb_data <- reactive({
    stats <- sim_stats_r()
    best <- stats %>% arrange(p50_days) %>% slice(1)
    conventional <- stats %>% filter(operation_mode == "Conventional") %>% slice(1)
    zipper <- stats %>% filter(operation_mode == "Zipper") %>% slice(1)

    saving_days <- if (nrow(conventional) == 1 && nrow(zipper) == 1) {
      conventional$p50_days - zipper$p50_days
    } else NA_real_
    saving_pct <- if (!is.na(saving_days) && conventional$p50_days > 0) {
      100 * saving_days / conventional$p50_days
    } else NA_real_

    bn <- bottlenecks_r() %>% arrange(priority, desc(p90_utilization)) %>% slice(1)
    rd <- readiness_r() %>% arrange(readiness_score) %>% slice(1)

    # Two weakest readiness drivers, explained in operational units (days,
    # %, $) rather than raw 0-100 sub-scores, so the score explains itself.
    drivers <- ""
    if (nrow(rd) == 1) {
      rd_stats <- stats %>% filter(operation_mode == rd$operation_mode) %>% slice(1)
      rd_wireline_days <- mean(
        sim_results()$summary$total_wireline_readiness_delay_days[
          sim_results()$summary$operation_mode == rd$operation_mode], na.rm = TRUE)

      comp_scores <- c(
        "Schedule certainty" = rd$schedule_score,
        "Resource capacity" = rd$resource_score,
        "Risk exposure" = rd$risk_score,
        "Wireline readiness" = rd$wireline_score
      )
      comp_desc <- c(
        "Schedule certainty" = sprintf("P90 is %.0f d above P50 (%.0f%% wider)",
          rd_stats$p90_days - rd_stats$p50_days, 100 * rd$uncertainty_ratio),
        "Resource capacity" = sprintf("%s at %.0f%% P90 utilization", rd$non_frac_bottleneck, 100 * rd$non_frac_p90_utilization),
        "Risk exposure" = sprintf("risk events add ~%.0f d (%.0f%% of campaign)",
          rd_stats$mean_risk_delay_days, 100 * rd$risk_delay_ratio),
        "Wireline readiness" = sprintf("frac fleet idle ~%.0f d waiting on wireline (~%s)",
          rd_wireline_days, fmt_money_short(rd_wireline_days * input$frac_fleet_cost))
      )
      weakest <- names(sort(comp_scores))[1:2]
      drivers <- paste(paste0(weakest, " – ", comp_desc[weakest]), collapse = "; ")
    }

    idle_days <- mean(sim_results()$summary$total_wireline_readiness_delay_days, na.rm = TRUE)

    list(
      best = best$operation_mode,
      p50 = fmt_days_short(best$p50_days),
      p90 = paste0("P90: ", fmt_days_short(best$p90_days)),
      saving = if (is.na(saving_days)) "N/A" else fmt_days_short(saving_days),
      saving_pct = if (is.na(saving_pct)) "" else paste0(round(saving_pct, 1), "% vs conventional"),
      readiness = if (nrow(rd) == 0) "N/A" else paste0(round(rd$readiness_score, 0), " / 100"),
      readiness_status = if (nrow(rd) == 0) "" else rd$readiness_status,
      readiness_drivers = drivers,
      bottleneck = if (nrow(bn) == 0) "N/A" else paste(bn$operation_mode, bn$resource, sep = " \u2013 "),
      bottleneck_type = if (nrow(bn) == 0) "post_frac" else {
        if (grepl("Milling|Testing", bn$resource)) "post_frac" else "frac_phase"
      },
      # Frac-phase constraint: wireline slower than frac fleet in zipper
      frac_phase_constrained = idle_days > 2,
      idle_days = idle_days,
      idle_cost = fmt_money_short(idle_days * input$frac_fleet_cost)
    )
  })

  output$vb_best       <- renderText(vb_data()$best)
  output$vb_p50        <- renderText(vb_data()$p50)
  output$vb_p90        <- renderText(vb_data()$p90)
  output$vb_saving     <- renderText(vb_data()$saving)
  output$vb_saving_pct <- renderText(vb_data()$saving_pct)
  output$vb_readiness  <- renderText(vb_data()$readiness)
  output$vb_bottleneck <- renderText(vb_data()$bottleneck)
  output$vb_idle_cost  <- renderText(vb_data()$idle_cost)
  output$vb_readiness_status <- renderUI({
    status <- vb_data()$readiness_status
    if (identical(status, "")) return(NULL)
    cls <- switch(status, Ready = "text-success", Caution = "text-warning",
                  "At Risk" = "text-warning", Critical = "text-danger", "text-muted")
    tags$p(class = paste("fw-bold mb-0", cls), status)
  })
  output$vb_readiness_drivers <- renderUI({
    d <- vb_data()$readiness_drivers
    if (identical(d, "")) return(NULL)
    tags$small(class = "text-muted", paste("Driven by:", d))
  })
  output$vb_idle_days <- renderText({
    sprintf("%.1f mean idle days waiting on wireline", vb_data()$idle_days)
  })

  output$vb_bottleneck_sub <- renderUI({
    d <- vb_data()
    if (identical(d$bottleneck, "N/A")) return(NULL)
    # Show scope: is this limiting campaign end date or frac efficiency?
    scope_txt <- if (d$bottleneck_type == "post_frac")
      tags$small(class = "text-muted", "Limits campaign end date")
    else
      tags$small(class = "text-muted", "Limits frac fleet utilization")
    # Also flag wireline constraint if it exists alongside a post-frac bottleneck
    wl_note <- if (d$bottleneck_type == "post_frac" && d$frac_phase_constrained)
      tags$small(class = "text-warning d-block mt-1",
        sprintf("Also: wireline slower than frac fleet — %.1f d idle cost exists", d$idle_days))
    else NULL
    tagList(scope_txt, wl_note)
  })

  output$vb_idle_context <- renderUI({
    d <- vb_data()
    if (d$idle_days < 0.5) {
      return(tags$small(class = "text-muted", "No significant frac fleet waiting time"))
    }
    # Clarify that idle cost and campaign bottleneck are different problems
    scope <- if (d$bottleneck_type == "post_frac")
      "This is separate from the campaign bottleneck — frac fleet waits on wireline during pumping operations."
    else
      "Wireline is also the campaign bottleneck."
    tagList(
      tags$small(sprintf("%.1f mean idle days waiting on wireline. ", d$idle_days)),
      tags$small(class = "text-muted d-block", scope)
    )
  })

  output$bottleneck_narrative <- renderUI({
    nb <- narrative_r()
    if (nrow(nb) == 0) return(p("No bottleneck identified."))
    status_cls <- switch(nb$bottleneck_status,
                         Critical = "text-danger", Moderate = "text-warning", "text-success")
    tagList(
      h3(class = "mb-1", paste(nb$operation_mode, "\u2013", nb$resource)),
      p(class = paste("fw-bold", status_cls),
        sprintf("%s | P90 utilization %.0f%%", nb$bottleneck_status, 100 * nb$p90_utilization)),
      if (nb$p50_saving_days > 0) {
        p(sprintf("Expected impact: ~%.0f days of campaign duration recoverable.", nb$p50_saving_days))
      },
      p(tags$strong("Recommended action: "), nb$recommended_action),
      # If milling/testing is the campaign bottleneck but wireline is also
      # causing frac idle cost, explain that these are two separate issues.
      {
        vbd <- vb_data()
        if (!is.null(vbd) && vbd$bottleneck_type == "post_frac" && vbd$frac_phase_constrained) {
          div(class = "mt-3 p-2 border-start border-warning border-3 bg-light rounded",
            tags$small(tags$strong("Note: two separate constraints are active.")),
            tags$br(),
            tags$small(
              sprintf(
                "The campaign bottleneck is %s (drives total duration). ",
                nb$resource
              )),
            tags$small(
              sprintf(
                "Separately, the frac fleet is also waiting %.1f days on wireline during pumping operations (%.0f%% of frac-phase time). ",
                vbd$idle_days,
                100 * vbd$idle_days / max(mean(sim_results()$summary$total_frac_fleet_days, na.rm=TRUE), 1)
              )),
            tags$small("Fixing the campaign bottleneck will not reduce the wireline idle cost, and vice versa.")
          )
        }
      }
    )
  })

  output$investment_table <- renderDT({
    inv <- investment_r()
    if (nrow(inv) == 0) {
      return(datatable(tibble(message = "No resource addition shows a positive schedule saving."),
                       options = list(dom = "t"), rownames = FALSE))
    }
    inv %>%
      transmute(
        Mode = operation_mode,
        Change = proposed_change,
        `P50 saving` = paste0(round(p50_saving_days, 1), " d"),
        `Incremental cost` = incremental_unit_cost,
        `Schedule value` = schedule_value,
        `Net benefit` = net_benefit,
        `Benefit / cost` = round(benefit_cost_ratio, 2)
      ) %>%
      datatable(options = list(dom = "t", scrollX = TRUE), rownames = FALSE) %>%
      formatCurrency(c("Incremental cost", "Schedule value", "Net benefit"), digits = 0)
  })

  output$validation_plot <- renderPlot({
    dat <- input_data()
    req(isTRUE(dat$ok), sim_results())
    plot_input_validation(dat$historical, sim_results()$well_details,
                          frac_time_per_stage_hours = input$frac_time_per_stage_hours)
  }, res = 96)

  # --- Selectors -------------------------------------------------------------

  selected_simulation <- reactive({
    req(sim_results())
    sim_results()$summary %>% arrange(desc(estimated_campaign_days)) %>% slice(1) %>% pull(simulation_id)
  })

  output$simulation_selector_ui <- renderUI({
    req(sim_results())
    if (nrow(sim_results()$well_details) == 0) {
      return(helpText("Well-level detail is not retained in Fast mode. Switch to Standard or Audit and re-run."))
    }
    ids <- sort(unique(sim_results()$summary$simulation_id))
    selectInput("selected_simulation", "Simulation ID (default: worst case)", choices = ids, selected = selected_simulation())
  })

  output$risk_selector_ui <- renderUI({
    req(sim_results())
    ids <- sort(unique(sim_results()$summary$simulation_id))
    selectInput("selected_risk_simulation", "Simulation ID", choices = ids, selected = selected_simulation())
  })

  # --- Plots -----------------------------------------------------------------

  output$scurve_plot   <- renderPlot({ req(sim_results()); plot_campaign_scurve(sim_results()$summary) }, res = 96)
  output$duration_plot <- renderPlot({ req(sim_results()); plot_campaign_distribution(sim_results()$summary) }, res = 96)
  output$tornado_plot  <- renderPlot({ plot_risk_tornado(stage_risk_r()) }, res = 96)
  output$consequence_plot <- renderPlot({ plot_risk_consequences(consequences_r()) }, res = 96)
  output$consequence_table <- renderDT({
    cq <- consequences_r()
    req(cq)
    cq %>%
      transmute(
        Mode = operation_mode,
        `Risk event` = risk_event,
        Events = event_count,
        `Direct delay (d)` = round(direct_delay_days, 1),
        `Wireline rework (d)` = round(induced_wireline_days, 1),
        `CT (d)` = round(induced_ct_days, 1),
        `Milling (d)` = round(induced_milling_days, 1),
        `Testing (d)` = round(induced_testing_days, 1),
        `Pumping (d)` = round(induced_frac_days, 1),
        `Induced share` = scales::percent(induced_share, accuracy = 1),
        `Per campaign (d)` = round(expected_impact_per_campaign, 1)
      ) %>%
      datatable(options = list(pageLength = 12, scrollX = TRUE), rownames = FALSE)
  })
  output$delay_plot    <- renderPlot({ plot_delay_contributors(delay_r()) }, res = 96)
  output$stage_risk_plot <- renderPlot({ plot_stage_level_risks(stage_risk_r()) }, res = 96)
  output$gantt_plot <- renderPlot({ plot_resource_gantt(timeline_r()) }, res = 96)
  output$resource_plot <- renderPlot({ plot_resource_utilization(resource_summary_r()) }, res = 96)
  output$bottleneck_plot <- renderPlot({ plot_bottlenecks(bottlenecks_r()) }, res = 96)
  output$wireline_constraint_plot <- renderPlot({ plot_wireline_constraint(wireline_r()) }, res = 96)
  output$readiness_plot <- renderPlot({ plot_readiness_score(readiness_r()) }, res = 96)
  output$recommendation_plot <- renderPlot({ plot_resource_recommendations(recommendations_r()) }, res = 96)
  output$cost_impact_plot <- renderPlot({ plot_cost_impact(cost_impact_r()) }, res = 96)

  # --- Tables ----------------------------------------------------------------

  dt_simple <- function(df, page = 10, digits = 2, rn = TRUE) {
    datatable(safe_round_df(df, digits), options = list(pageLength = page, scrollX = TRUE), rownames = rn)
  }

  output$executive_kpi_table     <- renderDT({ datatable(kpis_r(), options = list(dom = "t", scrollX = TRUE), rownames = FALSE) })
  output$executive_summary_table <- renderDT({
    req(sim_results())
    tbl <- build_executive_summary(sim_results()$summary, sim_results()$risk_event_log, sim_results()$resource_utilization)
    datatable(tbl, options = list(dom = "t", scrollX = TRUE), rownames = FALSE)
  })
  output$results_table <- renderDT({ req(sim_results()); dt_simple(sim_results()$summary) })
  output$traffic_light_table <- renderDT({
    datatable(safe_round_df(traffic_r(), 3), options = list(dom = "t", scrollX = TRUE), rownames = FALSE) %>%
      formatStyle(c("schedule_risk", "resource_risk", "operational_risk", "wireline_constraint"),
                  color = styleEqual(c("Green", "Amber", "Red"), c("#1b9e77", "#e6ab02", "#d62728")),
                  fontWeight = "bold")
  })
  output$readiness_table  <- renderDT({ datatable(safe_round_df(readiness_r(), 3), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) })
  output$recommendation_table <- renderDT({ datatable(safe_round_df(recommendations_r(), 2), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) })
  output$cost_impact_table <- renderDT({ datatable(safe_round_df(cost_impact_r(), 2), options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE) })
  output$delay_table      <- renderDT({ dt_simple(delay_r(), page = 15) })
  output$stage_risk_table <- renderDT({ dt_simple(stage_risk_r(), page = 15) })
  output$resource_table   <- renderDT({ dt_simple(resource_summary_r()) })
  output$bottleneck_table <- renderDT({ dt_simple(bottlenecks_r()) })
  output$wireline_constraint_table <- renderDT({ dt_simple(wireline_r()) })
  output$assumptions_table <- renderDT({ req(sim_results()); dt_simple(sim_results()$assumptions_used, page = 20, digits = 4) })

  output$well_details_table <- renderDT({
    req(sim_results(), input$selected_simulation, nrow(sim_results()$well_details) > 0)
    tbl <- sim_results()$well_details %>% filter(simulation_id == as.integer(input$selected_simulation))
    dt_simple(tbl, page = 15)
  })

  output$risk_event_table <- renderDT({
    req(sim_results(), input$selected_risk_simulation)
    tbl <- sim_results()$risk_event_log %>% filter(simulation_id == as.integer(input$selected_risk_simulation))
    dt_simple(tbl, page = 15)
  })

  output$input_check <- renderPrint({
    dat <- input_data()
    if (!isTRUE(dat$ok)) {
      cat("Input error:\n", dat$error, "\n")
      return(invisible(NULL))
    }
    cat("Historical wells rows:", nrow(dat$historical), "\n")
    cat("Assumption rows:", nrow(dat$assumptions), "\n")
    cat("Risk rows:", sum(dat$assumptions$type == "Risk", na.rm = TRUE), "\n\n")
    cat("Historical columns:\n"); print(names(dat$historical))
    cat("\nAssumption columns:\n"); print(names(dat$assumptions))
  })

  # --- Workflow configuration -------------------------------------------
  # Users can inspect and edit the operational sequence from the UI.
  # Changes flow into the summarise_workflow() display but NOT into the
  # simulation engine at runtime (that requires a code-level WORKFLOW_CONFIG
  # change -- see the Workflow tab for instructions).
  output$workflow_table <- renderDT({
    datatable(
      summarise_workflow(),
      options = list(dom = "t", pageLength = 20, scrollX = TRUE),
      rownames = FALSE,
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: left; color: #0F2A43; font-weight: bold;",
        "Current operational sequence (read-only view). To change: see instructions below."
      )
    ) %>%
      formatStyle("Phase",
        color = styleEqual(
          c("pre_frac","frac_stage","post_frac"),
          c("#2980B9","#E6A817","#D55E00")
        ), fontWeight = "bold")
  })

  output$workflow_resource_table <- renderDT({
    datatable(
      workflow_resource_phases(),
      options = list(dom = "t", scrollX = TRUE),
      rownames = FALSE
    )
  })

  # --- Scenario optimiser -----------------------------------------------------

  opt_grid <- reactive({
    modes <- input$opt_modes
    req(length(modes) > 0)
    ct_mill_opts <- if (isTRUE(input$opt_ct_milling)) c(FALSE, TRUE) else FALSE
    grid <- expand.grid(
      operation_mode = modes,
      frac_fleets = seq(input$opt_frac_range[1], input$opt_frac_range[2]),
      wireline_units = seq(input$opt_wl_range[1], input$opt_wl_range[2]),
      ct_units = seq(input$opt_ct_range[1], input$opt_ct_range[2]),
      milling_units = seq(input$opt_mill_range[1], input$opt_mill_range[2]),
      testing_units = seq(input$opt_test_range[1], input$opt_test_range[2]),
      stringsAsFactors = FALSE
    )
    grid$frac_trees <- ifelse(grid$operation_mode == "Zipper",
                              max(2, input$frac_trees), input$frac_trees)
    grid <- merge(grid, data.frame(allow_ct_for_milling = ct_mill_opts))
    grid
  })

  output$opt_grid_size <- renderUI({
    n <- nrow(opt_grid())
    est_min <- round(n * input$opt_screen_iter / 4000, 1)  # rough throughput guess
    cls <- if (n > 400) "text-danger fw-bold" else "text-muted"
    msg <- sprintf("%d configurations to screen (rough estimate: %s min). ", n,
                   format(est_min, nsmall = 1))
    if (n > 400) msg <- paste0(msg, "Reduce the ranges - more than 400 configs is blocked.")
    tags$small(class = cls, msg)
  })

  optim_results <- reactiveVal(NULL)

  observeEvent(input$run_optimiser, {
    dat <- input_data()
    if (!isTRUE(dat$ok)) {
      showNotification(paste("Fix input files first:", dat$error), type = "error")
      return()
    }
    grid <- opt_grid()
    if (nrow(grid) > 400) {
      showNotification("More than 400 configurations - narrow the ranges.", type = "error")
      return()
    }

    fixed_args <- list(
      zipper_efficiency = input$zipper_efficiency,
      risk_multiplier = input$risk_multiplier,
      wireline_time_per_stage_hours = input$wireline_time_per_stage_hours,
      wireline_rig_up_down_hours = input$wireline_rig_up_down_hours,
      wireline_contingency_pct = input$wireline_contingency_pct,
      frac_time_per_stage_hours = input$frac_time_per_stage_hours,
      frac_settling_time_hours = input$frac_settling_time_hours,
      well_to_well_transition_hours = input$well_to_well_transition_hours,
      pad_to_pad_move_hours = input$pad_to_pad_move_hours,
      frac_tree_swap_delay_hours = input$frac_tree_swap_delay_hours,
      ct_milling_efficiency = input$ct_milling_efficiency,
      flowback_testing_days_min = input$flowback_testing_days_min,
      flowback_testing_days_max = input$flowback_testing_days_max
    )

    res <- tryCatch({
      withProgress(message = "Optimising scenarios", value = 0, {
        optimise_campaign_scenarios(
          historical_wells = dat$historical,
          assumptions = dat$assumptions,
          n_wells = as.integer(input$n_wells),
          scenario_grid = grid,
          fixed_args = fixed_args,
          frac_fleet_cost_per_day = input$frac_fleet_cost,
          wireline_cost_per_day = input$wireline_cost,
          ct_cost_per_day = input$ct_cost,
          milling_cost_per_day = input$milling_cost,
          testing_unit_cost_per_day = input$testing_unit_cost,
          screen_iterations = as.integer(input$opt_screen_iter),
          refine_iterations = 600,
          top_n_refine = 5,
          seed = as.integer(input$seed),
          progress_callback = function(i, n, stage = "screen") {
            if (stage == "screen") {
              setProgress(0.85 * i / n, detail = sprintf("Screening %d / %d", i, n))
            } else {
              setProgress(0.85 + 0.15 * i / n, detail = sprintf("Refining top %d / %d", i, n))
            }
          }
        )
      })
    }, error = function(e) {
      showNotification(paste("Optimiser error:", conditionMessage(e)),
                       type = "error", duration = NULL)
      NULL
    })
    optim_results(res)
  })

  output$opt_recommendation <- renderUI({
    res <- optim_results()
    if (is.null(res)) return(p(class = "text-muted", "Run the optimiser to get a recommendation."))
    rec <- res %>% filter(recommended) %>% slice(1)
    if (nrow(rec) == 0) return(p("No recommendation available."))
    fast <- res %>% filter(fastest) %>% slice(1)
    tagList(
      h4(class = "mb-1", rec$config_label),
      p(tags$strong(sprintf("P50: %.0f days | Total cost: %s | Idle: %s",
                            rec$p50_days, fmt_money_short(rec$total_mobilisation_cost),
                            fmt_money_short(rec$idle_cost)))),
      p(class = "text-muted",
        sprintf("Lowest total mobilisation cost on the efficient frontier (refined at 600 iterations, seed %s). ",
                input$seed),
        sprintf("Fastest option: %s at %.0f days for %s.",
                fast$config_label, fast$p50_days, fmt_money_short(fast$total_mobilisation_cost))),
      div(class = "alert alert-info mt-2 p-2",
        tags$small(
          tags$strong("Why might the Overview P50 differ after applying these settings?"),
          tags$br(),
          "The optimiser screens at 600 iterations; the main simulation runs at your selected iteration count. ",
          "Small differences (1-3 days) are normal Monte Carlo noise. The optimiser P50 shown here ",
          sprintf("(%.0f days) is the best estimate at 600 iterations. Re-run the full simulation after ", rec$p50_days),
          "applying settings to get the definitive result at your chosen precision."
        )
      )
    )
  })

  output$opt_apply_ui <- renderUI({
    req(optim_results())
    actionButton("apply_recommended", "Apply recommended settings to sidebar",
                 class = "btn-outline-success w-100")
  })

  observeEvent(input$apply_recommended, {
    res <- optim_results()
    req(res)
    rec <- res %>% filter(recommended) %>% slice(1)
    req(nrow(rec) == 1)
    updateNumericInput(session, "frac_fleets", value = rec$frac_fleets)
    updateNumericInput(session, "wireline_units", value = rec$wireline_units)
    updateNumericInput(session, "ct_units", value = rec$ct_units)
    updateNumericInput(session, "milling_units", value = rec$milling_units)
    updateNumericInput(session, "testing_units", value = rec$testing_units)
    updateNumericInput(session, "frac_trees", value = rec$frac_trees)
    updateSelectInput(session, "operation_mode", selected = rec$operation_mode)
    updateCheckboxInput(session, "allow_ct_for_milling", value = rec$allow_ct_for_milling)
    showNotification(
      tagList(
        tags$strong("Settings applied."),
        tags$br(),
        tags$small(sprintf(
          "Optimiser P50: %.0f days (at 600 iterations, seed %s). ",
          rec$p50_days, input$seed
        )),
        tags$small(sprintf(
          "Click Run simulation for the definitive %d-iteration result. ",
          as.integer(input$n_iter)
        )),
        tags$small("A difference of 1-3 days vs the optimiser is normal Monte Carlo variation.")
      ),
      type = "message", duration = 10
    )
  })

  output$pareto_plot        <- renderPlot({ plot_pareto_frontier(optim_results()) }, res = 96)
  output$total_cost_card <- renderUI({
    tc <- total_cost_r()
    if (is.null(tc) || nrow(tc) == 0) return(NULL)

    fmt_M <- function(x) paste0("$", round(x/1e6, 1), "M")

    if (nrow(tc) == 2) {
      # Both modes: show comparison
      conv <- tc[tc$operation_mode == "Conventional",]
      zip  <- tc[tc$operation_mode == "Zipper",]
      cost_saving   <- conv$total_cost - zip$total_cost
      sched_saving  <- conv$p50_days   - zip$p50_days
      prod_pct_conv <- 100 * conv$productive_cost / conv$total_cost
      prod_pct_zip  <- 100 * zip$productive_cost  / zip$total_cost

      card(
        card_header("Total campaign cost comparison"),
        card_body(fill = FALSE,
          layout_columns(
            col_widths = c(4, 4, 4),
            # Conventional cost breakdown
            div(class = "p-2 border rounded",
              tags$h6(class = "fw-bold text-primary", "Conventional"),
              tags$p(class = "mb-1",
                tags$span(style = "font-size:1.6em;font-weight:bold;", fmt_M(conv$total_cost))),
              tags$small(class = "text-muted d-block",
                sprintf("P50: %.0f days  |  %s/day", conv$p50_days, fmt_M(conv$cost_per_day))),
              tags$hr(class="my-2"),
              tags$small(class="d-block", sprintf("Productive: %s (%.0f%%)",
                fmt_M(conv$productive_cost), prod_pct_conv)),
              tags$small(class="d-block text-muted", sprintf("Standby: %s (%.0f%%)",
                fmt_M(conv$standby_cost), 100-prod_pct_conv)),
              if (conv$idle_penalty > 10000) tags$small(class="d-block text-warning",
                sprintf("Idle penalty: %s", fmt_M(conv$idle_penalty)))
            ),
            # Zipper cost breakdown
            div(class = "p-2 border rounded",
              tags$h6(class = "fw-bold", style = "color:#E69F00;", "Zipper"),
              tags$p(class = "mb-1",
                tags$span(style = "font-size:1.6em;font-weight:bold;", fmt_M(zip$total_cost))),
              tags$small(class = "text-muted d-block",
                sprintf("P50: %.0f days  |  %s/day", zip$p50_days, fmt_M(zip$cost_per_day))),
              tags$hr(class="my-2"),
              tags$small(class="d-block", sprintf("Productive: %s (%.0f%%)",
                fmt_M(zip$productive_cost), prod_pct_zip)),
              tags$small(class="d-block text-muted", sprintf("Standby: %s (%.0f%%)",
                fmt_M(zip$standby_cost), 100-prod_pct_zip)),
              if (zip$idle_penalty > 10000) tags$small(class="d-block text-warning",
                sprintf("Idle penalty: %s", fmt_M(zip$idle_penalty)))
            ),
            # Net saving
            div(class = paste0("p-2 border rounded ",
                               if(cost_saving > 0) "border-success bg-light" else "border-danger bg-light"),
              tags$h6(class = "fw-bold", "Net saving (Zipper vs Conventional)"),
              tags$p(class = "mb-1",
                tags$span(
                  style = paste0("font-size:1.6em;font-weight:bold;color:",
                                 if(cost_saving > 0) "#009E73" else "#D55E00"),
                  paste0(if(cost_saving > 0) "+" else "", fmt_M(cost_saving)))),
              tags$small(class = "text-muted d-block",
                sprintf("%.0f days faster", sched_saving)),
              tags$small(class = "text-muted d-block",
                sprintf("Cost/day: Zip %s vs Conv %s",
                  fmt_M(zip$cost_per_day), fmt_M(conv$cost_per_day))),
              tags$hr(class="my-2"),
              tags$small(class="text-muted",
                "Saving comes from fewer standby days on a shorter campaign, ",
                "even when per-day spread rate is higher in zipper mode.")
            )
          )
        )
      )
    } else {
      # Single mode
      r <- tc[1,]
      prod_pct <- 100 * r$productive_cost / r$total_cost
      card(
        card_header(paste("Total campaign cost —", r$operation_mode)),
        card_body(fill = FALSE,
          layout_columns(col_widths = c(4, 4, 4),
            div(tags$h6("Total"), tags$strong(style="font-size:1.4em;", fmt_M(r$total_cost)),
                tags$small(class="text-muted d-block", sprintf("%.0f days  |  %s/day",
                  r$p50_days, fmt_M(r$cost_per_day)))),
            div(tags$h6("Productive"), tags$strong(fmt_M(r$productive_cost)),
                tags$small(class="text-muted", sprintf("%.0f%% of total", prod_pct))),
            div(tags$h6("Standby"), tags$strong(fmt_M(r$standby_cost)),
                tags$small(class="text-muted", sprintf("%.0f%% of total (on-site not working)",
                  100-prod_pct)))
          )
        )
      )
    }
  })

  output$synthetic_data_banner <- renderUI({
    dat <- tryCatch(input_data(), error = function(e) NULL)
    if (is.null(dat) || !isTRUE(dat$using_synthetic)) return(NULL)
    div(
      class = "alert alert-warning d-flex align-items-center gap-2 py-2 px-3 mb-2",
      tags$span(style = "font-size:1.2em;", "⚠️"),
      div(
        tags$strong("Synthetic baseline data in use."),
        tags$span(class = "ms-1",
          "No historical_wells.csv was uploaded. Duration estimates use synthetic defaults ",
          "(triangular distributions calibrated to typical plug-and-perf campaigns). ",
          "Upload your own historical data for calibrated, site-specific estimates.")
      )
    )
  })

  output$zipper_benefit_card <- renderUI({
    bd <- zipper_breakdown_r()
    if (is.null(bd) || nrow(bd) == 0) return(NULL)
    # Only show when both modes are present
    all_modes <- unique(sim_results()$summary$operation_mode)
    if (!all(c("Conventional","Zipper") %in% all_modes)) return(NULL)

    rows <- bd %>% dplyr::filter(component != "Total saving")
    total <- bd %>% dplyr::filter(component == "Total saving")

    card(
      card_header({
          bd <- zipper_breakdown_r()
          total_row <- if (!is.null(bd)) bd |> dplyr::filter(component == "Total saving") else NULL
          saving_days <- if (!is.null(total_row) && nrow(total_row) > 0) total_row$saving_days[1] else NULL
          if (!is.null(saving_days)) paste0("Where the ", saving_days, " days comes from") else "Drivers of schedule reduction"
        }),
      card_body(fill = FALSE,
        layout_columns(
          col_widths = rep(3, nrow(rows)),
          !!!lapply(1:nrow(rows), function(i) {
            r <- rows[i,]
            col <- if (r$saving_days > 0) "#009E73" else "#D55E00"
            div(class = "text-center p-2 border rounded",
              tags$small(class = "text-muted d-block", r$component),
              tags$strong(style = paste0("font-size:1.3em;color:",col),
                          paste0(ifelse(r$saving_days > 0, "+", ""), r$saving_days, " d")),
              tags$small(class = "text-muted d-block mt-1", style = "font-size:0.78em;",
                         r$explanation)
            )
          })
        ),
        tags$hr(class = "my-2"),
        div(class = "text-center",
          tags$span(class = "fw-bold me-2", "Total P50 saving:"),
          tags$span(style = "color:#009E73;font-size:1.2em;font-weight:bold;",
                    paste0(total$saving_days, " days")),
          tags$span(class = "text-muted ms-2 small", total$explanation)
        )
      )
    )
  })

  output$cascade_waterfall  <- renderPlot({ plot_constraint_cascade(cascade_results()) }, res = 96)
  output$cascade_util_plot  <- renderPlot({ plot_cascade_utilization(cascade_results()) }, res = 96)
  output$cascade_table      <- renderDT({
    res <- cascade_results()
    if (is.null(res)) return(datatable(data.frame(message = "Run cascade to see results.")))
    res %>%
      transmute(
        Step = step,
        Action = action,
        `P50 days` = round(p50_days, 1),
        `P10 days` = round(p10_days, 1),
        `P90 days` = round(p90_days, 1),
        `Days saved` = round(days_saved, 1),
        `Daily rate` = daily_rate,
        `Incremental cost` = incremental_cost,
        `Schedule value` = schedule_value,
        `Cost per day saved` = cost_per_day_saved,
        `ROI (d per M$)` = round(roi_days_per_Mdollar, 1),
        `Next constraint` = paste0(bottleneck_now, " (", bottleneck_util_pct, "%)"),
        Verdict = verdict
      ) %>%
      datatable(options = list(dom = "t", scrollX = TRUE, pageLength = 10), rownames = FALSE) %>%
      formatCurrency(c("Daily rate", "Incremental cost", "Schedule value", "Cost per day saved"), digits = 0) %>%
      formatStyle("Verdict",
        color = styleEqual(
          c("Starting point"),
          c("#0072B2")
        ),
        fontWeight = styleEqual(c("Starting point"), c("bold"))
      )
  })

  output$opt_results_table <- renderDT({
    res <- optim_results()
    req(res)
    res %>%
      transmute(
        Scenario = config_label,
        Stage = stage,
        `P50 days` = round(p50_days, 1),
        `P90 days` = round(p90_days, 1),
        `Idle days` = round(idle_days, 1),
        `Idle cost` = idle_cost,
        `Spread $/day` = spread_rate_per_day,
        `Total cost` = total_mobilisation_cost,
        Pareto = ifelse(pareto, "Yes", ""),
        Flag = case_when(recommended ~ "RECOMMENDED", fastest ~ "Fastest", TRUE ~ "")
      ) %>%
      datatable(options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE) %>%
      formatCurrency(c("Idle cost", "Spread $/day", "Total cost"), digits = 0) %>%
      formatStyle("Flag", fontWeight = "bold",
                  color = styleEqual(c("RECOMMENDED", "Fastest"), c("#009E73", "#0072B2")))
  })

  output$download_optimiser <- downloadHandler(
    filename = function() paste0("scenario_optimiser_results_", Sys.Date(), ".csv"),
    content = function(file) {
      req(optim_results())
      write_csv(optim_results(), file)
    }
  )

  # --- Downloads (shown only after a successful run) -------------------------

  output$download_ui <- renderUI({
    req(sim_results())
    tagList(
      tags$hr(),
      downloadButton("download_report", "Management report PDF", class = "w-100 mb-2"),
      downloadButton("download_all", "Audit package (zip)", class = "w-100")
    )
  })

  output$download_report <- downloadHandler(
    filename = function() paste0("frac_campaign_management_report_", Sys.Date(), ".pdf"),
    content = function(file) {
      req(sim_results())
      build_management_report_pdf(
        file = file,
        summary = sim_results()$summary,
        risk_event_log = sim_results()$risk_event_log,
        resource_utilization = sim_results()$resource_utilization,
        frac_fleet_cost_per_day = input$frac_fleet_cost,
        wireline_cost_per_day = input$wireline_cost,
        ct_cost_per_day = input$ct_cost,
        milling_cost_per_day = input$milling_cost,
        testing_unit_cost_per_day = input$testing_unit_cost,
        target_days = .na_to_null(input$target_days),
        budget = .na_to_null(input$budget),
        recommendation = rec_v2_r(),
        narrative = decision_narrative_r()$narrative,
        robustness = robustness_rv(),
        scenario_records = scenario_library_rv()
      )
    }
  )

  output$download_all <- downloadHandler(
    filename = function() paste0("frac_campaign_simulation_audit_", Sys.Date(), ".zip"),
    content = function(file) {
      req(sim_results())
      tmpdir <- tempfile("audit_package_")
      dir.create(tmpdir)

      write_csv(sim_results()$summary, file.path(tmpdir, "simulation_summary.csv"))
      if (nrow(sim_results()$well_details) > 0) {
        write_csv(sim_results()$well_details, file.path(tmpdir, "simulation_well_details.csv"))
      }
      write_csv(sim_results()$risk_event_log, file.path(tmpdir, "simulation_risk_event_log.csv"))
      write_csv(sim_results()$resource_utilization, file.path(tmpdir, "resource_utilization.csv"))
      write_csv(sim_results()$assumptions_used, file.path(tmpdir, "assumptions_used.csv"))
      write_csv(build_executive_summary(sim_results()$summary, sim_results()$risk_event_log, sim_results()$resource_utilization), file.path(tmpdir, "executive_summary.csv"))
      write_csv(kpis_r(), file.path(tmpdir, "executive_kpis.csv"))
      write_csv(delay_r(), file.path(tmpdir, "delay_contributors.csv"))
      write_csv(resource_summary_r(), file.path(tmpdir, "resource_utilization_summary.csv"))
      write_csv(bottlenecks_r(), file.path(tmpdir, "bottleneck_detection.csv"))
      write_csv(stage_risk_r(), file.path(tmpdir, "stage_level_risks.csv"))
      write_csv(wireline_r(), file.path(tmpdir, "wireline_constraint.csv"))
      write_csv(traffic_r(), file.path(tmpdir, "traffic_lights.csv"))
      write_csv(readiness_r(), file.path(tmpdir, "readiness_score.csv"))
      write_csv(recommendations_r(), file.path(tmpdir, "resource_recommendations.csv"))
      write_csv(cost_impact_r(), file.path(tmpdir, "cost_impact.csv"))
      write_csv(investment_r(), file.path(tmpdir, "investment_ranking.csv"))
      write_csv(timeline_r(), file.path(tmpdir, "resource_timeline.csv"))
      write_csv(consequences_r(), file.path(tmpdir, "risk_consequences.csv"))
      build_management_report_pdf(file.path(tmpdir, "management_report.pdf"),
                                  sim_results()$summary, sim_results()$risk_event_log,
                                  sim_results()$resource_utilization,
                                  input$frac_fleet_cost, input$wireline_cost,
                                  input$ct_cost, input$milling_cost,
                                  testing_unit_cost_per_day = input$testing_unit_cost,
                                  target_days = .na_to_null(input$target_days),
                                  budget = .na_to_null(input$budget),
                                  recommendation = rec_v2_r(),
                                  narrative = decision_narrative_r()$narrative,
                                  robustness = robustness_rv(),
                                  scenario_records = scenario_library_rv())

      files_to_zip <- list.files(tmpdir, full.names = TRUE)
      if (requireNamespace("zip", quietly = TRUE)) {
        zip::zip(zipfile = file, files = basename(files_to_zip), root = tmpdir)
      } else {
        oldwd <- getwd()
        on.exit(setwd(oldwd), add = TRUE)
        setwd(tmpdir)
        utils::zip(zipfile = file, files = list.files(tmpdir), flags = "-r9Xq")
      }
    }
  )
}

shinyApp(ui, server)
