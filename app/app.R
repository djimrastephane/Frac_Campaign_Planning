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
library(future)
library(promises)
library(rhandsontable)

# The main "Run simulation" button can take 25-55s at Audit-mode/40-well
# scale (measured). Without this, that call runs synchronously inside the
# Shiny session's single R process and the WHOLE session -- every tab, every
# other reactive -- is unresponsive for the full duration. multisession runs
# it in a separate background R process so the session stays responsive;
# see sim_results' observeEvent(input$run, ...) below.
#
# Capped at 2 workers, not future's default of availableCores(): this
# session never runs more than 2 concurrent simulate_campaign_detailed()
# calls (Compare-both's own .par_lapply() fork is a SEPARATE, nested pool
# spun up inside one of these 2 workers, not more multisession workers).
# multisession spawns its pool eagerly at plan() time, before any "Run"
# click -- on a typical Shiny Server deployment where every session gets
# its own R process, future's default would mean every single session
# idles with one persistent worker R process per CPU core on the host for
# its entire lifetime, which compounds fast under concurrent users.
future::plan(future::multisession, workers = 2)

if (basename(getwd()) == "app") {
  project_root <- normalizePath(file.path(getwd(), ".."), mustWork = FALSE)
} else {
  project_root <- normalizePath(getwd(), mustWork = FALSE)
}

# Single source of truth for %||% and DEFAULT_DAY_RATES -- see R/constants.R
# for why the scattered function-default rate literals elsewhere are left
# as-is (guarded by R/test_default_day_rates.R) rather than rewritten here.
source(file.path(project_root, "R", "constants.R"))

source(file.path(project_root, "R", "load_inputs.R"))
source(file.path(project_root, "R", "validate_inputs.R"))
source(file.path(project_root, "R", "validate_risk_consequence_library.R"))
# Engine split into 4 files (see docs/architecture_cleanup_plan.md) -- must be
# sourced in this order: engine_core.R defines simulate_campaign_detailed()
# and everything summaries.R/report_pdf.R/optimiser_cascade.R call into.
source(file.path(project_root, "R", "engine_core.R"))
source(file.path(project_root, "R", "summaries.R"))
source(file.path(project_root, "R", "report_pdf.R"))
source(file.path(project_root, "R", "optimiser_explain.R"))
source(file.path(project_root, "R", "optimiser_cascade.R"))
source(file.path(project_root, "R", "optimiser_manifest.R"))
source(file.path(project_root, "R", "risk_library_engine.R"))
source(file.path(project_root, "R", "optimiser_parallel.R"))
source(file.path(project_root, "R", "risk_uncertainty.R"))
source(file.path(project_root, "R", "bottleneck_explain.R"))
source(file.path(project_root, "R", "recommendations.R"))
source(file.path(project_root, "R", "robustness.R"))
source(file.path(project_root, "R", "sensitivity_analysis.R"))
source(file.path(project_root, "R", "whatif_builder.R"))
source(file.path(project_root, "R", "bayesian_updater.R"))
source(file.path(project_root, "R", "learning_engine.R"))
source(file.path(project_root, "R", "risk_heatmap.R"))
source(file.path(project_root, "R", "scenario_library.R"))
source(file.path(project_root, "R", "narrative_engine.R"))
source(file.path(project_root, "R", "report_decision_page.R"))
source(file.path(project_root, "R", "plots.R"))

safe_round_df <- function(df, digits = 2) {
  df %>% mutate(across(where(is.numeric), ~ round(.x, digits)))
}

# The bundled template is the default seed for the Risk Editor grid, and the
# fallback whenever no assumption_file is uploaded. "Risk" type rows (Technical
# Risk / Resource Risk / External Risk categories) are user-editable in the
# grid; everything else (Campaign Setup / Base Operation, locked-name rows
# the engine looks up by exact name) is passed through unedited.
default_assumptions_df <- load_master_assumptions(
  file.path(project_root, "data_templates", "master_risks_assumptions_template.csv")
)
split_assumptions_locked_risk <- function(df) {
  is_risk <- tolower(trimws(df$type)) == "risk"
  list(locked = df[!is_risk, , drop = FALSE], risk = df[is_risk, , drop = FALSE])
}
default_assumptions_split <- split_assumptions_locked_risk(default_assumptions_df)

# Cell-level error finder for the Risk Editor grid: mirrors the checks in
# validate_assumptions() (R/validate_inputs.R), but instead of stop()-ing on
# the first failing category, it collects every offending (row, col) pair so
# the UI can highlight them all at once. validate_assumptions() itself stays
# the single source of truth for whether the data is actually fit to run --
# this is purely for live visual feedback while editing.
find_risk_cell_errors <- function(df) {
  if (nrow(df) == 0) return(list())
  col_idx <- function(name) match(name, names(df)) - 1L  # 0-based for Handsontable
  bad <- list()
  add_bad <- function(row_idx, col_name) {
    ci <- col_idx(col_name)
    for (r in row_idx) bad[[length(bad) + 1]] <<- list(row = r - 1L, col = ci)
  }

  empty_idx <- which(is.na(df$variable) | trimws(df$variable) == "")
  if (length(empty_idx) > 0) add_bad(empty_idx, "variable")

  keys <- tolower(trimws(df$variable))
  dup_idx <- which(nzchar(keys) & keys %in% keys[duplicated(keys) & nzchar(keys)])
  if (length(dup_idx) > 0) add_bad(dup_idx, "variable")

  prob_idx <- which(is.na(df$probability) | df$probability < 0 | df$probability > 1)
  if (length(prob_idx) > 0) add_bad(prob_idx, "probability")

  tri_idx <- which(
    !is.na(df$min_days) & !is.na(df$most_likely_days) & !is.na(df$max_days) &
      !(df$min_days <= df$most_likely_days & df$most_likely_days <= df$max_days)
  )
  if (length(tri_idx) > 0) {
    add_bad(tri_idx, "min_days"); add_bad(tri_idx, "most_likely_days"); add_bad(tri_idx, "max_days")
  }

  if ("scope" %in% names(df)) {
    bad_scope_idx <- which(
      !is.na(df$scope) & trimws(df$scope) != "" &
        !tolower(trimws(df$scope)) %in% c("stage", "well", "campaign")
    )
    if (length(bad_scope_idx) > 0) add_bad(bad_scope_idx, "scope")
  }

  bad
}

# Same idea as find_risk_cell_errors(), scoped to the Parameters grid
# (Campaign Setup / Base Operation rows). Only the triangle-ordering check
# applies here -- probability range, name, and scope checks in
# validate_assumptions() are risk-row-only and don't apply to this set.
find_param_cell_errors <- function(df) {
  if (nrow(df) == 0) return(list())
  col_idx <- function(name) match(name, names(df)) - 1L
  bad <- list()
  tri_idx <- which(
    !is.na(df$min_days) & !is.na(df$most_likely_days) & !is.na(df$max_days) &
      !(df$min_days <= df$most_likely_days & df$most_likely_days <= df$max_days)
  )
  if (length(tri_idx) > 0) {
    for (col_name in c("min_days", "most_likely_days", "max_days")) {
      ci <- col_idx(col_name)
      for (r in tri_idx) bad[[length(bad) + 1]] <- list(row = r - 1L, col = ci)
    }
  }
  bad
}

# Renames the internal snake_case columns back to load_master_assumptions()'s
# expected raw CSV headers (e.g. "variable" -> "Variable / Risk Event"), so a
# downloaded grid CSV can be re-uploaded via assumption_file/params_file
# without a "missing column" error -- a round-trip load_master_assumptions()
# wouldn't otherwise tolerate, since it looks for the exact original header
# text, not the post-clean_names() name.
# CSV/formula-injection guard: a text cell that starts with =, +, -, or @ is
# interpreted as a formula by Excel/Sheets/LibreOffice when the exported CSV
# is opened -- e.g. a Risk Editor "variable" or "simulation_impact" cell
# typed as "=cmd|'/c calc'!A1" or "=SUM(1,1)". Prefixing such cells with a
# leading apostrophe forces spreadsheet apps to treat them as literal text
# (the apostrophe itself is not written into the cell value on open) without
# altering what a human or CSV-reading script sees.
# Deliberately narrow: only touches character columns, and only cells that
# start with one of the four trigger characters -- ordinary values (numbers,
# dates, plain text) pass through byte-for-byte.
sanitize_csv_cell <- function(x) {
  if (!is.character(x)) return(x)
  needs_prefix <- !is.na(x) & grepl("^[=+@-]", x)
  x[needs_prefix] <- paste0("'", x[needs_prefix])
  x
}

# Applies sanitize_csv_cell() to the named columns of `df` that are actually
# present -- callers pass only the free-text columns a user could have typed
# into (e.g. "variable", "simulation_impact"), never locked-name/lookup
# columns, so a sanitized round-trip upload can't break the engine's
# exact-name matching for Campaign Setup / Base Operation rows.
sanitize_csv_text_cols <- function(df, cols) {
  present <- intersect(cols, names(df))
  for (col in present) df[[col]] <- sanitize_csv_cell(df[[col]])
  df
}

assumptions_to_template_headers <- function(df) {
  rename_map <- c(
    category = "Category", variable = "Variable / Risk Event", type = "Type",
    probability = "Probability", min_days = "Min Days",
    most_likely_days = "Most Likely Days", max_days = "Max Days",
    simulation_impact = "Simulation Impact", scope = "Scope"
  )
  matched <- intersect(names(rename_map), names(df))
  names(df)[match(matched, names(df))] <- rename_map[matched]
  df
}

# Export-time column renames for the optimiser results CSV. Inside the app,
# the Optimiser tab's footer explains that the "idle" figure prices ONLY
# frac-fleet idle time spent waiting on wireline stage readiness -- not
# testing-gated post-frac idle, and not other resources' idle. The bare CSV
# leaves the app without that footnote, so the column names must carry the
# scope themselves (a 415-day testing-serialized campaign showing 1.7 "idle
# days" is correct but unreadable without it).
optimiser_export_headers <- function(df) {
  rename_map <- c(
    idle_days = "frac_idle_awaiting_wireline_days",
    idle_cost = "frac_idle_awaiting_wireline_cost"
  )
  matched <- intersect(names(rename_map), names(df))
  names(df)[match(matched, names(df))] <- rename_map[matched]
  df
}

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

# Flattens recommend_action()'s list output (R/recommendations.R) into a
# one-row tibble so the evidence-based 3-way verdict can be written to the
# audit package -- previously only resource_recommendations.csv (the older,
# simpler build_resource_recommendations() table) was exported, so the
# traceable recommendation shown on the Decision Support tab never made it
# into the downloadable record (Issue #42).
recommendation_verdict_to_df <- function(rec) {
  tibble(
    operation_mode          = rec$operation_mode,
    recommendation          = rec$recommendation,
    decision_status         = rec$decision_status,
    decision_reason         = rec$decision_reason,
    bottleneck              = rec$bottleneck,
    bottleneck_status       = rec$status,
    p90_utilization         = rec$p90_utilization,
    base_p50_days           = rec$base_p50_days,
    new_p50_days            = rec$new_p50_days,
    expected_reduction_days = rec$expected_reduction_days,
    spread_rate_per_day     = rec$spread_rate_per_day,
    expected_value          = rec$expected_value,
    confidence              = rec$confidence,
    confidence_band         = rec$confidence_band,
    basis                   = rec$basis,
    why                     = paste(rec$why, collapse = " | ")
  )
}

plot_card <- function(header, output_id, height = "440px", decision = NULL) {
  card(
    full_screen = TRUE,
    card_header(header),
    card_body(plotOutput(output_id, height = height), padding = 8),
    if (!is.null(decision))
      card_footer(tags$small(class = "text-muted", tags$b("Decision: "), decision))
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

  tags$head(
    tags$style(HTML(
      ".htInvalidCell { background-color: #f8d7da !important; box-shadow: inset 0 0 0 1px #dc3545; }"
    )),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('grid_cell_highlight', function(msg) {
        var widget = HTMLWidgets.find('#' + msg.id);
        if (!widget || !widget.hot) return;
        var hot = widget.hot;
        var nRows = hot.countRows();
        var nCols = hot.countCols();
        for (var r = 0; r < nRows; r++) {
          for (var c = 0; c < nCols; c++) {
            hot.setCellMeta(r, c, 'className', '');
          }
        }
        (msg.cells || []).forEach(function(cell) {
          hot.setCellMeta(cell.row, cell.col, 'className', 'htInvalidCell');
        });
        hot.render();
      });
    "))
  ),

  sidebar = sidebar(
    width = 340,
    actionButton("run", "Run simulation", class = "btn-primary btn-lg w-100"),
    uiOutput("run_status"),
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
        fileInput("assumption_file", "master_risks_assumptions.csv (optional)", accept = ".csv"),
        helpText(class = "text-muted small mt-0",
          "If omitted, the bundled template is used. Risk rows can be edited directly ",
          "in the Risk Editor tab without uploading a file at all."),
        fileInput("risk_library_file", "risk_consequence_library.csv (optional)", accept = ".csv"),
        downloadButton("download_risk_library_template", "Download template", class = "btn-sm w-100 mb-2"),
        helpText(class = "text-muted small mt-0",
          "If omitted, the bundled template is used. Expected columns: ",
          "risk_name, category, scope, base_probability, severity, scenario_probability, ",
          "wireline_runs, ct_days, milling_plugs, testing_days, pump_days, extra_stages, ",
          "logistics_days, affected_resource, risk_notes, scenario_notes."),
        tags$hr(class = "my-2"),
        tags$small(class = "fw-bold", "Bayesian update (optional)"),
        fileInput("bayes_new_wells_file", "New campaign wells CSV", accept = ".csv"),
        helpText(class = "text-muted small mt-0",
          "Same format as historical_wells.csv. Uploads new completed-well observations; ",
          "the Bayesian Update tab shows prior vs posterior. Apply merges data into the simulation bootstrap."),
        fileInput("bayes_risk_obs_file", "Risk observations CSV (optional)", accept = ".csv"),
        helpText(class = "text-muted small mt-0",
          "Columns: risk_event, n_trials, n_events. Used for Beta-Binomial risk probability updating.")
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
        sliderInput("risk_multiplier", "Risk frequency multiplier", min = 0.25, max = 3, value = 1, step = 0.25),
        helpText("Scales how often risk events occur. It does not change consequence severity once a risk occurs."),
        numericInput("target_days", "Target duration (days, optional)", value = NULL, min = 1, step = 1),
        numericInput("budget", "Budget ceiling ($, optional)", value = NULL, min = 0, step = 1000000)
      ),
      accordion_panel(
        "Resources",
        numericInput("frac_fleets", "Frac fleets", value = 1, min = 1, max = 5, step = 1),
        numericInput("wireline_units", "Wireline units", value = 1, min = 1, max = 5, step = 1),
        numericInput("ct_units", "CT / cleanout units", value = 1, min = 1, max = 5, step = 1),
        numericInput("milling_units", "Milling units", value = 1, min = 1, max = 5, step = 1),
        selectInput("pre_frac_scheduling", "Pre-frac scheduling model",
                    choices = c("Resource queue (default)" = "event",
                                "Workload formula (legacy)" = "formula"),
                    selected = "event"),
        helpText(class = "text-muted small mt-0",
          "Resource queue: schedules each well against real unit-availability ",
          "timelines -- e.g. a wireline unit that finishes a well early can ",
          "start the next one immediately. Validated against the workload-",
          "formula model across multiple resource configurations (testing-unit, ",
          "wireline, and milling bottlenecks); diverges from it specifically ",
          "where the formula could not see real resource contention. Workload ",
          "formula: divides each well's CT/wireline/frac workload by the ",
          "resource count -- kept as a faster, simpler legacy option for ",
          "comparison."),
        # Zipper-only inputs: have no effect on a Conventional-only run (the
        # engine only applies tree efficiency / swap delay when is_zipper),
        # so hide them when the user has picked Conventional specifically --
        # but keep them visible for "Compare both", since that mode still
        # runs a Zipper pass that uses them.
        conditionalPanel(
          condition = "input.operation_mode != 'Conventional'",
          numericInput("frac_trees", "Frac trees available", value = 2, min = 1, max = 10, step = 1),
          helpText("2 = basic zipper. 3 = lower swap delay (~5%). 4+ = further reduction (~10%)."),
          sliderInput("zipper_efficiency", "Zipper execution factor",
                      min = 0.5, max = 1.0, value = 0.75, step = 0.05),
          helpText("0.75 means frac execution is 25% faster than conventional."),
          numericInput("frac_tree_swap_delay_hours", "Frac tree swap delay, h", value = 4, min = 0, max = 48, step = 0.5),
          helpText("Transition delay per well between zipper pairs when only 2 trees available.")
        ),
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
        numericInput("frac_fleet_cost", "Frac fleet $/day", value = DEFAULT_DAY_RATES$frac_fleet, min = 0, step = 10000),
        numericInput("wireline_cost", "Wireline $/day", value = DEFAULT_DAY_RATES$wireline, min = 0, step = 1000),
        numericInput("ct_cost", "CT / cleanout $/day", value = DEFAULT_DAY_RATES$ct, min = 0, step = 1000),
        numericInput("milling_cost", "Milling $/day", value = DEFAULT_DAY_RATES$milling, min = 0, step = 1000),
        numericInput("testing_unit_cost", "Testing unit $/day", value = DEFAULT_DAY_RATES$testing_unit, min = 0, step = 500)
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
      uiOutput("readiness_breakdown"),
      # Zipper benefit breakdown card (only shown when both modes simulated)
      uiOutput("synthetic_data_banner"),
      uiOutput("total_cost_card"),
      uiOutput("zipper_benefit_card"),

      layout_columns(
        col_widths = c(5, 7),
        card(
          full_screen = TRUE,
          card_header(
            uiOutput("bottleneck_card_header", inline = TRUE),
            bslib::tooltip(
              tags$span(class = "text-muted ms-1", style = "cursor: help;", "(?)"),
              "Why: the limiting resource, ranked by unit-aware stream duration (workload / units), not raw utilization alone. ",
              "The analytic estimate is the relief down to the NEXT constraint -- a different question from ",
              "'what does one extra unit buy?'. Click Verify to re-simulate with +1 unit and get the measured, ",
              "paired answer to that second question; expect the verified number to be smaller.",
              placement = "right"
            )
          ),
          uiOutput("bottleneck_narrative"),
          actionButton("verify_rec_overview", "Verify by re-simulation",
                       class = "btn-sm btn-outline-primary mt-2")
        ),
        card(
          full_screen = TRUE,
          card_header("Where to spend next"),
          card_body(fill = FALSE, dt_wrap("investment_table", "300px")),
          card_footer(tags$small(class = "text-muted",
            "Schedule value = P50 days saved x total daily spread rate. ",
            "Incremental cost = added unit's day rate x resulting P50 duration. ",
            "ROI = Schedule value / Incremental cost (Excellent >5x, Good 2-5x, Marginal <2x). ",
            "Planning-level screening estimate (workload ÷ units, not re-simulated) for all 5 resources at once — ",
            "run the Optimiser's “Run constraint cascade” for a verified, re-simulated ranking."))
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
      "Historical Learning",
      card(
        card_header("Automatic distribution fitting from historical well data"),
        card_body(
          uiOutput("learning_status")
        )
      ),
      layout_columns(
        col_widths = c(12),
        card(
          full_screen = TRUE,
          card_header("Fitted distributions — density overlay"),
          card_body(plotOutput("learning_density_plot", height = "420px")),
          card_footer(tags$small(class = "text-muted",
            "Grey bars = empirical data. Solid line = selected planning distribution (lowest AIC among the 4 tested — ",
            "see Fit quality below for how well it actually matches the data). ",
            "Dashed lines = other candidate distributions."))
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          full_screen = TRUE,
          card_header("Q-Q diagnostic — observed vs theoretical quantiles"),
          card_body(plotOutput("learning_qq_plot", height = "380px")),
          card_footer(tags$small(class = "text-muted",
            "Points on the dashed line = perfect fit. Curvature = distribution mismatch."))
        ),
        card(
          full_screen = TRUE,
          card_header("Distribution fitting results — AIC / BIC / KS ranking"),
          card_body(DT::DTOutput("learning_fit_table")),
          card_footer(tags$small(class = "text-muted",
            "Rank = lowest AIC among the 4 candidates — this is the primary selection logic, not a ",
            "goodness-of-fit claim. ",
            "Fit quality (Good / Moderate / Poor) is an ", tags$b("indicative"), " check from the KS p-value: ",
            "a low p-value means the Kolmogorov-Smirnov test rejects that distribution as the data's true ",
            "generator — common for operational data with a heavier tail than Normal/Lognormal/Gamma/Weibull ",
            "can capture. ",
            "This KS result is approximate, not a formal test: its parameters were fitted from the same ",
            "data being tested, which biases the p-value toward passing. Treat Fit quality as a rough ",
            "signal alongside the AIC ranking, not proof the selected distribution is the data's true ",
            "generator. Even when rank 1's fit quality is Moderate or Poor, it's still the most suitable ",
            "AIC-ranked candidate evaluated — not a confirmed correct distribution."))
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          full_screen = TRUE,
          card_header("Descriptive statistics"),
          card_body(DT::DTOutput("learning_desc_table"))
        ),
        card(
          full_screen = TRUE,
          card_header("Suggested planning assumptions (auto-generated)"),
          card_body(DT::DTOutput("learning_suggested_table")),
          card_footer(tags$small(class = "text-muted",
            "Min / Mode / Max derived from P5 / mode / P95 of the selected (AIC-ranked) planning distribution. ",
            "See the \"Note\" column: when Fit quality is Moderate or Poor, no tested distribution ",
            "perfectly matches the historical data -- the one shown is still the most suitable candidate ",
            "evaluated, just not a precise tail-risk model. Fit quality is an indicative KS check only ",
            "(parameters were fitted from the same data), not a guarantee this is the true distribution. ",
            "Use these to populate the min_days / most_likely_days / max_days columns ",
            "in master_risks_assumptions.csv for stage-duration rows."))
        )
      ),
      card(
        full_screen = TRUE,
        card_header("Outlier well summary — frac days per stage"),
        card_body(
          DT::DTOutput("outlier_summary_table"),
          tags$hr(),
          tags$p(class = "small text-muted mb-1",
            "Wells above the P95 threshold for this metric, split into Watch-list ",
            "(above P95 but still part of the normal cluster) and Extreme (above P99 ",
            "or 2x P90). Click a row to see why."),
          DT::DTOutput("outlier_wells_table"),
          uiOutput("outlier_well_detail"),
          tags$hr(),
          checkboxInput("exclude_outlier_wells",
            "Exclude extreme outlier well(s) from the simulation and learning",
            value = FALSE),
          uiOutput("outlier_exclusion_status")
        ),
        card_footer(tags$small(class = "text-muted",
          "historical_wells.csv has no risk-event/\"reason\" field, so \"contributing factor(s)\" ",
          "is a best-effort guess from the other recorded columns (stage overrun, contingency ",
          "plugs, cement-eval/milling time well above the typical well) -- not a confirmed cause. ",
          "When nothing stands out, investigate the well's field records directly. ",
          "This table always shows ALL wells, including any currently excluded below -- click ",
          "\"Run simulation\" after checking the exclude box to re-run without them."))
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
               verbatimTextOutput("recommendation_panel"),
               tags$details(class = "mt-2",
                 tags$summary(class = "small text-muted", style = "cursor: pointer;", "Decision Rules"),
                 uiOutput("recommendation_decision_rules"))))
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
      "Sensitivity",
      card(
        card_header("Sensitivity analysis — what drives campaign duration?"),
        card_body(
          tags$p("Sensitivity analysis identifies which planning assumptions most affect campaign duration."),
          tags$p(class = "mb-1", "The tool runs one-at-a-time perturbation tests:"),
          tags$ul(class = "mb-2",
            tags$li(tags$b("±20% "), "Timing and operational duration assumptions",
              tags$span(class = "text-muted", " — moderate operational efficiency variation.")),
            tags$li(tags$b("±50% "), "Risk-event probabilities",
              tags$span(class = "text-muted", " — reflects the higher uncertainty of low-frequency operational events.")),
            tags$li(tags$b("±1 unit "), "Resource counts",
              tags$span(class = "text-muted", " — reflects real mobilisation decisions; crews and equipment are added in whole units."))
          ),
          tags$p(class = "text-muted small mb-2",
            "These ranges are planning-level stress tests, not calibrated confidence intervals. ",
            "Each perturbed case is re-simulated using 200 iterations for speed. ",
            "Results are intended for screening and ranking drivers — verify any recommended action using the full simulation count."),
          layout_columns(
            col_widths = c(4, 8),
            actionButton("run_sensitivity", "Run sensitivity analysis",
                         class = "btn-sm btn-primary", icon = icon("chart-bar")),
            uiOutput("sensitivity_status")
          )
        )
      ),
      layout_columns(
        col_widths = c(12),
        card(
          full_screen = TRUE,
          card_header("Driver ranking — P50 impact by variable (butterfly tornado)"),
          card_body(plotOutput("sensitivity_tornado_plot", height = "520px")),
          card_footer(tags$small(class = "text-muted", tags$b("Decision: "),
            "Ranks assumptions that most affect campaign duration, so planning effort goes to the highest-leverage ones. ",
            "Bar width = P50 shift when assumption is perturbed. Wider bar = stronger influence on schedule. ",
            "Faceted by operation mode when both Conventional and Zipper are simulated."))
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          full_screen = TRUE,
          card_header("Conventional vs Zipper — swing comparison (top 10 drivers)"),
          card_body(plotOutput("sensitivity_bymode_plot", height = "380px")),
          card_footer(tags$small(class = "text-muted",
            "Compares how sensitive each mode is to each driver. ",
            "Only available when 'Compare both' is selected."))
        ),
        card(
          full_screen = TRUE,
          card_header("Variable importance ranking"),
          card_body(DT::DTOutput("sensitivity_ranking_table"))
        )
      ),
      card(
        full_screen = TRUE,
        card_header("Detailed sensitivity table — P50 at low / base / high perturbation"),
        card_body(DT::DTOutput("sensitivity_detail_table"))
      )
    ),

    nav_panel(
      "Bayesian Update",
      card(
        card_header("Bayesian duration & risk probability updating"),
        card_body(
          uiOutput("bayes_status"),
          layout_columns(
            col_widths = c(6, 6),
            sliderInput("bayes_prior_strength", "Prior strength (equivalent prior sample size)",
                        min = 5, max = 100, value = 20, step = 5),
            div(
              tags$small(class = "text-muted",
                "Higher = historical assumptions anchor the estimate more strongly against new data. ",
                "Lower = new observations update faster."),
              actionButton("bayes_apply", "Apply to simulation",
                           class = "btn-sm btn-success mt-2", icon = icon("check")),
              uiOutput("bayes_apply_status")
            )
          )
        )
      ),
      layout_columns(
        col_widths = c(12),
        card(
          full_screen = TRUE,
          card_header("Duration update — prior vs posterior predictive"),
          card_body(plotOutput("bayes_duration_plot", height = "260px")),
          card_footer(
            tags$small(class = "text-muted d-block", tags$b("Decision: "),
              "Checks whether historical duration assumptions should be updated for future planning."),
            uiOutput("bayes_duration_interp"),
            tags$small(class = "text-muted mt-1 d-block",
              "Blue = prior (historical wells). Gold = posterior (updated). Dashed = P50."))
        )
      ),
      # Risk-probability updating is split into one section per `scope`
      # (stage / well / campaign): a stage-scope opportunity count (e.g. 53
      # stages) is not the same unit as a campaign-scope one (e.g. 1
      # campaign), so mixing them in one chart/table invites an invalid
      # direct comparison. Each section is only as tall as it needs to be --
      # render_risk_scope_section() (defined in the server) returns a muted
      # placeholder instead of an empty plot/table when a scope has no rows.
      layout_columns(
        col_widths = c(12),
        card(
          full_screen = TRUE,
          card_header("Stage-level risk update (opportunities = stages)"),
          card_body(plotOutput("bayes_risk_plot_stage", height = "320px")),
          card_footer(
            uiOutput("bayes_risk_interp_stage"),
            tags$small(class = "text-muted mt-1 d-block",
              "Blue = prior. Gold = posterior. Red dotted = observed frequency. ",
              "\"n\" badge shows the opportunity count and its unit."))
        )
      ),
      layout_columns(
        col_widths = c(12),
        card(
          full_screen = TRUE,
          card_header("Well-level risk update (opportunities = wells)"),
          card_body(plotOutput("bayes_risk_plot_well", height = "320px")),
          card_footer(uiOutput("bayes_risk_interp_well"))
        )
      ),
      layout_columns(
        col_widths = c(12),
        card(
          full_screen = TRUE,
          card_header("Campaign-level risk update (opportunities = campaigns)"),
          card_body(plotOutput("bayes_risk_plot_campaign", height = "320px")),
          card_footer(uiOutput("bayes_risk_interp_campaign"))
        )
      ),
      layout_columns(
        col_widths = c(12),
        card(
          full_screen = TRUE,
          card_header("Duration parameter summary"),
          card_body(DT::DTOutput("bayes_duration_table"))
        )
      ),
      layout_columns(
        col_widths = c(4, 4, 4),
        card(card_header("Stage-level risk summary"), card_body(DT::DTOutput("bayes_risk_table_stage"))),
        card(card_header("Well-level risk summary"), card_body(DT::DTOutput("bayes_risk_table_well"))),
        card(card_header("Campaign-level risk summary"), card_body(DT::DTOutput("bayes_risk_table_campaign")))
      )
    ),

    nav_panel(
      "What-If",
      card(
        card_header("Define up to 3 alternative configurations"),
        card_body(
          p(class = "text-muted",
            "Each variant inherits all current simulation parameters. ",
            "Only fill in what you want to change. Leave a variant label blank to skip it. ",
            "Runs all active variants in parallel at 300 iterations — click ",
            tags$strong("Run comparison"), " after the main simulation has completed."),

          # Variant 1
          tags$hr(),
          tags$h6("Variant 1"),
          layout_columns(
            col_widths = c(3, 2, 1, 1, 1, 1, 1),
            textInput("wif_v1_label", "Label", placeholder = "e.g. +1 Frac fleet"),
            selectInput("wif_v1_mode", "Mode",
              choices = c("(same as base)" = "", "Conventional", "Zipper"), selected = ""),
            numericInput("wif_v1_frac",  "Frac fleets", value = NULL, min = 1, max = 5),
            numericInput("wif_v1_wl",    "Wireline units", value = NULL, min = 1, max = 5),
            numericInput("wif_v1_ct",    "CT units", value = NULL, min = 1, max = 5),
            numericInput("wif_v1_ml",    "Milling units", value = NULL, min = 1, max = 5),
            numericInput("wif_v1_tu",    "Testing units", value = NULL, min = 1, max = 5)
          ),

          # Variant 2
          tags$h6("Variant 2"),
          layout_columns(
            col_widths = c(3, 2, 1, 1, 1, 1, 1),
            textInput("wif_v2_label", "Label", placeholder = "e.g. Zipper mode"),
            selectInput("wif_v2_mode", "Mode",
              choices = c("(same as base)" = "", "Conventional", "Zipper"), selected = ""),
            numericInput("wif_v2_frac",  "Frac fleets", value = NULL, min = 1, max = 5),
            numericInput("wif_v2_wl",    "Wireline units", value = NULL, min = 1, max = 5),
            numericInput("wif_v2_ct",    "CT units", value = NULL, min = 1, max = 5),
            numericInput("wif_v2_ml",    "Milling units", value = NULL, min = 1, max = 5),
            numericInput("wif_v2_tu",    "Testing units", value = NULL, min = 1, max = 5)
          ),

          # Variant 3
          tags$h6("Variant 3"),
          layout_columns(
            col_widths = c(3, 2, 1, 1, 1, 1, 1),
            textInput("wif_v3_label", "Label", placeholder = "e.g. +1 WL +1 FF"),
            selectInput("wif_v3_mode", "Mode",
              choices = c("(same as base)" = "", "Conventional", "Zipper"), selected = ""),
            numericInput("wif_v3_frac",  "Frac fleets", value = NULL, min = 1, max = 5),
            numericInput("wif_v3_wl",    "Wireline units", value = NULL, min = 1, max = 5),
            numericInput("wif_v3_ct",    "CT units", value = NULL, min = 1, max = 5),
            numericInput("wif_v3_ml",    "Milling units", value = NULL, min = 1, max = 5),
            numericInput("wif_v3_tu",    "Testing units", value = NULL, min = 1, max = 5)
          ),

          tags$hr(),
          layout_columns(
            col_widths = c(4, 8),
            actionButton("run_whatif", "Run comparison",
                         class = "btn-sm btn-primary", icon = icon("play")),
            uiOutput("whatif_status")
          )
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(full_screen = TRUE,
             card_header("Duration comparison — P50 with P10–P90 range"),
             card_body(plotOutput("whatif_bars_plot", height = "380px")),
             card_footer(tags$small(class = "text-muted", tags$b("Decision: "),
               "Compares committed planning scenarios side by side to pick the best option."))),
        card(full_screen = TRUE,
             card_header("Duration S-curve overlay"),
             card_body(plotOutput("whatif_scurve_plot", height = "380px")),
             card_footer(tags$small(class = "text-muted", tags$b("Decision: "),
               "Shows the probability of finishing by any given day across scenarios.")))
      ),
      card(full_screen = TRUE,
           card_header("Side-by-side summary"),
           card_body(DT::DTOutput("whatif_comparison_table")))
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
          ),
          tags$hr(class = "my-2"),
          p(class = "text-muted small mb-1",
            "Export the current library to a .json file to save it across sessions, ",
            "or import a previously exported file to restore it (replaces the current library)."),
          layout_columns(
            col_widths = c(6, 6),
            fileInput("upload_scenarios", NULL, accept = ".json",
                      buttonLabel = "Import .json", placeholder = "No file selected"),
            div(style = "padding-top: 6px;",
                downloadButton("download_scenarios", "Export library (.json)",
                               class = "btn-sm btn-outline-primary w-100",
                               icon = icon("download")))
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
      plot_card("Schedule risk heatmap — expected delay by well and risk type", "risk_heatmap_plot", "520px",
                decision = "Flags which specific wells and risk types need closer monitoring or mitigation."),
      plot_card("Well risk ranking — total expected delay per well", "well_risk_ranking_plot", "420px",
                decision = "Ranks wells by expected schedule impact so attention goes to the highest-risk wells first."),
      table_card("Well risk scores", dt_wrap("well_risk_table", "360px")),
      plot_card("Expected schedule impact per campaign (tornado)", "tornado_plot", "460px",
                decision = "Ranks risk events by total expected delay across the campaign, to prioritise mitigation."),
      plot_card("Consequence propagation: direct delay vs induced workload", "consequence_plot", "480px"),
      table_card("Consequence detail by risk", dt_wrap("consequence_table", "400px")),
      plot_card("Top delay contributors", "delay_plot", "460px"),
      plot_card("Expected stage-level risk events", "stage_risk_plot", "460px"),
      table_card("Stage-level risk detail", dt_wrap("stage_risk_table", "400px")),
      table_card("Delay contributor detail", dt_wrap("delay_table", "400px"))
    ),

    nav_panel(
      "Resources",
      plot_card("Resource utilization: active days and utilization by mode", "gantt_plot", "420px",
                decision = "Identifies which resources are close to becoming campaign bottlenecks."),
      plot_card("Mean resource utilization", "resource_plot", "420px",
                decision = "Identifies which resources are close to becoming campaign bottlenecks."),
      plot_card("Bottleneck detection (P90 utilization)", "bottleneck_plot", "420px",
                decision = "Shows which resource is most likely to limit the schedule in a worst-case run."),
      plot_card("Estimated schedule improvement from additional resources", "recommendation_plot", "420px",
                decision = "Quantifies the P50 days saved if an extra unit of each resource were added."),
      plot_card("Estimated resource and idle cost impact", "cost_impact_plot", "420px",
                decision = "Weighs the cost of adding a resource against the cost of leaving it idle."),
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
          helpText("Each step trials every eligible resource (300 iterations each, up to 3) and keeps whichever measurably saves the most days. 6 steps ≈ 3-5 min.")
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
        p(class = "text-muted small mb-2 fst-italic",
          "Note: the \"Idle\" figure below only prices frac-fleet idle days at the frac fleet day rate -- ",
          "it does not separately price wireline/CT/milling/testing idle time. This is a simplification, ",
          "not a bug: the frac fleet is the highest day-rate resource and usually the binding constraint, ",
          "so it dominates idle cost in most configurations, but check the other resources' utilization ",
          "on the Resources tab before treating two close-ranked scenarios as equivalent."),
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
          uiOutput("opt_apply_ui"),
          uiOutput("opt_reproducibility_panel")
        ),
        plot_card("Trade-off frontier", "pareto_plot", "440px")
      ),
      card(
        full_screen = TRUE,
        card_header("All scenarios (sorted by total cost)"),
        card_body(fill = FALSE,
          tags$p(class = "text-muted small mb-2",
            tags$strong("Binding path"), " -- which side of the schedule controlled campaign completion, ",
            "and in what share of the simulated runs. ",
            "The binding path controls the campaign finish date. Resource changes outside that path ",
            "may reduce local queueing without shortening the overall campaign."),
          dt_wrap("opt_results_table", "480px")),
        card_footer(
          downloadButton("download_optimiser", "Download optimiser results CSV", class = "me-2"),
          downloadButton("download_optimiser_manifest", "Download run manifest (zip)",
                         class = "btn-outline-primary",
                         icon = icon("file-shield")),
          tags$div(class = "small text-muted mt-2",
            "The manifest bundle includes the run's inputs, assumptions, risk tables, and hashes -- ",
            "enough to reproduce or audit this exact run later. See ", tags$strong("Run reproducibility"),
            " above for a summary.")
        )
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
              tags$h6(class = "fw-bold text-primary", "Cement evaluation offline rule"),
              tags$p(class = "small mb-1", tags$strong("wireline_units ≥ 2: "),
                "Cement evaluation always runs offline — spare wireline unit available."),
              tags$p(class = "small", tags$strong("wireline_units = 1: "),
                "Cement evaluation offline probability from assumptions CSV (default 80%). ",
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
              tags$tr(tags$td("Cement evaluation always offline"),
                tags$td("Set Cement eval offline probability to 1.0"),
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
                tags$td(tags$code("WORKFLOW_CONFIG in R/engine_core.R"))),
              tags$tr(tags$td("Add a new activity to the sequence"),
                tags$td("Add row to WORKFLOW_CONFIG + update workload formula"),
                tags$td(tags$code("R/engine_core.R"))),
              tags$tr(tags$td("Custom sequence from file"),
                tags$td("Place workflow_config.csv alongside assumptions CSV"),
                tags$td(tags$code("workflow_config.csv (see template)")))
            )
          )
        )
      )
    ),


    nav_panel(
      "Risk Editor",
      card(
        card_header("Parameters"),
        p(class = "text-muted small",
          "Campaign Setup and Base Operation rows. ", tags$strong("Category, Variable, Type"),
          ", and the risk-only ", tags$strong("Scope"), "/consequence columns are locked — the ",
          "engine looks these rows up by exact name, and renaming or deleting one makes it ",
          "silently fall back to a default. ", tags$strong("Probability, Min/Most Likely/Max Days"),
          ", and the notes column are editable. Currently sourced from the uploaded ",
          "master_risks_assumptions.csv, or the bundled template if none is uploaded."),
        layout_columns(
          col_widths = c(6, 6),
          fileInput("params_file", "Upload parameters CSV (optional)", accept = ".csv"),
          div(class = "text-end",
            actionButton("reset_params_table", "Reset to template defaults", class = "btn-sm btn-outline-secondary me-2"),
            downloadButton("download_params_table", "Download as CSV", class = "btn-sm")
          )
        ),
        helpText(class = "text-muted small mt-n2",
          "Same column layout as master_risks_assumptions.csv — rows that aren't ",
          "Campaign Setup / Base Operation type are ignored on upload."),
        rHandsontableOutput("params_table_hot"),
        uiOutput("params_table_status")
      ),
      card(
        card_header("Edit risk rows directly — no CSV required"),
        p(class = "text-muted small",
          "Technical / Resource / External risk rows from master_risks_assumptions.csv, ",
          "editable as a spreadsheet. Campaign Setup and Base Operation rows ",
          "(locked-name, looked up by exact name by the engine) are not edited here — ",
          "they still come from the uploaded file, or the bundled template if none is uploaded."),
        layout_columns(
          col_widths = c(6, 6),
          fileInput("risk_rows_file", "Upload risk rows CSV (optional)", accept = ".csv"),
          div(class = "text-end",
            actionButton("reset_risk_rows", "Reset to template defaults", class = "btn-sm btn-outline-secondary me-2"),
            downloadButton("download_risk_rows", "Download as CSV", class = "btn-sm")
          )
        ),
        helpText(class = "text-muted small mt-n2",
          "Same column layout as master_risks_assumptions.csv — rows that aren't ",
          "Technical/Resource/External Risk type are ignored on upload."),
        tags$div(class = "alert alert-info py-2 px-3 small mb-2",
          tags$strong("Tip: "),
          "right-click any row in the grid below to insert a new risk above/below it, ",
          "or to remove that row entirely."),
        rHandsontableOutput("risk_rows_hot"),
        uiOutput("risk_rows_status")
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

  output$download_risk_library_template <- downloadHandler(
    filename = function() "risk_consequence_library_template_simple_severity.csv",
    content = function(file) {
      file.copy(
        file.path(project_root, "data_templates",
                   "risk_consequence_library_template_simple_severity.csv"),
        file
      )
    }
  )

  # --- Risk Editor (master_risks_assumptions.csv risk rows) -----------------
  # Locked-name rows (Campaign Setup / Base Operation) are not edited in the
  # grid -- they come from the uploaded file if present, else the bundled
  # template -- and are tracked separately from the user-editable risk rows.
  locked_rows_rv <- reactiveVal(default_assumptions_split$locked)
  risk_rows_seed_rv <- reactiveVal(default_assumptions_split$risk)

  observeEvent(input$assumption_file, {
    split <- tryCatch(
      split_assumptions_locked_risk(load_master_assumptions(input$assumption_file$datapath)),
      error = function(e) NULL
    )
    if (!is.null(split)) {
      locked_rows_rv(split$locked)
      risk_rows_seed_rv(split$risk)
    }
  })

  # Dedicated upload/download/reset for just the Risk rows table -- unlike
  # assumption_file above, this only ever touches risk_rows_seed_rv(); any
  # locked-name rows present in the uploaded CSV are ignored.
  observeEvent(input$risk_rows_file, {
    split <- tryCatch(
      split_assumptions_locked_risk(load_master_assumptions(input$risk_rows_file$datapath)),
      error = function(e) {
        showNotification(paste("Risk rows file error:", conditionMessage(e)), type = "error", duration = 10)
        NULL
      }
    )
    if (!is.null(split)) risk_rows_seed_rv(split$risk)
  })

  observeEvent(input$reset_risk_rows, {
    risk_rows_seed_rv(default_assumptions_split$risk)
  })

  # Dedicated upload/download/reset for just the Parameters table -- unlike
  # assumption_file above, this only ever touches locked_rows_rv(); any risk
  # rows present in the uploaded CSV are ignored.
  observeEvent(input$params_file, {
    split <- tryCatch(
      split_assumptions_locked_risk(load_master_assumptions(input$params_file$datapath)),
      error = function(e) {
        showNotification(paste("Parameters file error:", conditionMessage(e)), type = "error", duration = 10)
        NULL
      }
    )
    if (!is.null(split)) locked_rows_rv(split$locked)
  })

  observeEvent(input$reset_params_table, {
    locked_rows_rv(default_assumptions_split$locked)
  })

  output$download_params_table <- downloadHandler(
    filename = function() "parameters.csv",
    content = function(file) {
      df <- current_locked_rows() %>%
        sanitize_csv_text_cols(c("simulation_impact"))
      write.csv(assumptions_to_template_headers(df), file, row.names = FALSE)
    }
  )

  # Custom "Remove row" menu item: confirm() before calling Handsontable's
  # own alter('remove_row', ...) -- the default item removes immediately
  # with no prompt, which is one accidental right-click away from losing a
  # risk row with no undo (Ctrl+Z aside).
  confirm_remove_row_js <- htmlwidgets::JS("
    function (key, selection) {
      if (!window.confirm('Remove this risk row? This cannot be undone (use \"Reset to template defaults\" to start over from the bundled defaults).')) {
        return;
      }
      var start = selection[0].start.row;
      var end = selection[0].end.row;
      for (var r = end; r >= start; r--) {
        this.alter('remove_row', r);
      }
    }
  ")

  output$params_table_hot <- renderRHandsontable({
    df <- locked_rows_rv()
    rhandsontable(df, useTypes = TRUE, stretchH = "all", contextMenu = FALSE) %>%
      hot_col("category", readOnly = TRUE) %>%
      hot_col("variable", readOnly = TRUE) %>%
      hot_col("type", readOnly = TRUE) %>%
      hot_col("probability", type = "numeric", format = "0.00") %>%
      hot_col("min_days", type = "numeric", format = "0.00") %>%
      hot_col("most_likely_days", type = "numeric", format = "0.00") %>%
      hot_col("max_days", type = "numeric", format = "0.00") %>%
      hot_col("simulation_impact", type = "text") %>%
      hot_col("scope", readOnly = TRUE) %>%
      hot_col("extra_wireline_runs", readOnly = TRUE) %>%
      hot_col("extra_ct_days", readOnly = TRUE) %>%
      hot_col("extra_milling_plugs", readOnly = TRUE) %>%
      hot_col("extra_testing_days", readOnly = TRUE) %>%
      hot_col("extra_frac_days", readOnly = TRUE)
  })

  output$risk_rows_hot <- renderRHandsontable({
    df <- risk_rows_seed_rv()
    rhandsontable(df, useTypes = TRUE, stretchH = "all", contextMenu = TRUE) %>%
      hot_col("category", type = "dropdown",
              source = c("Technical Risk", "Resource Risk", "External Risk")) %>%
      hot_col("variable", type = "text") %>%
      hot_col("type", readOnly = TRUE) %>%
      hot_col("probability", type = "numeric", format = "0.00") %>%
      hot_col("min_days", type = "numeric", format = "0.00") %>%
      hot_col("most_likely_days", type = "numeric", format = "0.00") %>%
      hot_col("max_days", type = "numeric", format = "0.00") %>%
      hot_col("simulation_impact", type = "text") %>%
      hot_col("scope", type = "dropdown", source = c("stage", "well", "campaign")) %>%
      hot_context_menu(customOpts = list(
        remove_row = list(name = "Remove row", callback = confirm_remove_row_js)
      ))
  })

  # Current risk rows as edited in the grid, falling back to the seed before
  # the widget has rendered (e.g. on a different tab during initial load).
  current_risk_rows <- reactive({
    if (!is.null(input$risk_rows_hot)) hot_to_r(input$risk_rows_hot) else risk_rows_seed_rv()
  })

  current_locked_rows <- reactive({
    if (!is.null(input$params_table_hot)) hot_to_r(input$params_table_hot) else locked_rows_rv()
  })

  # Push cell-level red highlighting to the grid on every edit, without a
  # full renderRHandsontable() re-render (which would rebuild the DOM and
  # drop the user's cursor/selection mid-edit).
  observeEvent(current_risk_rows(), {
    bad_cells <- find_risk_cell_errors(current_risk_rows())
    session$sendCustomMessage("grid_cell_highlight", list(id = "risk_rows_hot", cells = bad_cells))
  })

  observeEvent(current_locked_rows(), {
    bad_cells <- find_param_cell_errors(current_locked_rows())
    session$sendCustomMessage("grid_cell_highlight", list(id = "params_table_hot", cells = bad_cells))
  })

  output$params_table_status <- renderUI({
    df <- current_locked_rows()
    bad_cells <- find_param_cell_errors(df)
    n_bad_rows <- length(unique(vapply(bad_cells, function(c) c$row, integer(1))))
    if (n_bad_rows == 0) {
      tags$p(class = "small text-success mt-2 mb-0", sprintf("✓ %d parameter row(s) — valid.", nrow(df)))
    } else {
      tags$p(class = "small text-danger mt-2 mb-0",
        sprintf("✘ %d row(s) violate Min ≤ Most Likely ≤ Max — fix the highlighted cells.", n_bad_rows))
    }
  })

  output$risk_rows_status <- renderUI({
    res <- tryCatch(
      list(ok = TRUE, df = validate_assumptions(bind_rows(current_locked_rows(), current_risk_rows()))),
      error = function(e) list(ok = FALSE, error = conditionMessage(e))
    )
    if (isTRUE(res$ok)) {
      n_risk <- nrow(current_risk_rows())
      tags$p(class = "small text-success mt-2 mb-0", sprintf("✓ %d risk row(s) — valid.", n_risk))
    } else {
      tags$pre(class = "small text-danger mt-2 mb-0", res$error)
    }
  })

  output$download_risk_rows <- downloadHandler(
    filename = function() "risk_rows.csv",
    content = function(file) {
      df <- current_risk_rows() %>%
        sanitize_csv_text_cols(c("variable", "simulation_impact"))
      write.csv(assumptions_to_template_headers(df), file, row.names = FALSE)
    }
  )

  input_data <- reactive({
    tryCatch({
      # Historical file is optional: fall back to synthetic data if not supplied
      using_synthetic <- is.null(input$historical_file)
      historical_raw <- if (using_synthetic) {
        synthetic_historical_wells(n = 30, seed = 42)
      } else {
        load_historical_wells(input$historical_file$datapath) %>%
          validate_historical_wells()
      }

      # Optional outlier exclusion (Historical Learning tab's "Outlier well
      # summary" card): removes wells flagged "Extreme" (>P99 or >2x P90) on
      # frac_days_per_stage from the data used downstream by BOTH the
      # simulation (the milling_days_per_plug bootstrap pool inside
      # simulate_campaign_detailed()) and the learning engine's distribution
      # fits -- a single source of truth, not two datasets quietly diverging
      # between tabs. "Watch-list" wells (>P95 but below the extreme cutoff)
      # are still part of the normal cluster and are never auto-excluded --
      # only shown for visibility. The outlier card itself always analyses
      # historical_raw (see outlier_r()) so it keeps showing every flagged
      # well, with its excluded state, regardless of this toggle.
      excluded_well_ids <- character(0)
      outlier_exclusion_note <- NULL
      historical <- historical_raw
      if (isTRUE(input$exclude_outlier_wells)) {
        outliers_for_exclusion <- tryCatch({
          o <- summarise_outlier_wells(historical_raw, metric = "frac_days_per_stage")$outliers
          o[o$tier == "Extreme", , drop = FALSE]
        }, error = function(e) tibble::tibble())
        n_remaining <- nrow(historical_raw) - nrow(outliers_for_exclusion)
        if (nrow(outliers_for_exclusion) == 0) {
          outlier_exclusion_note <- "Exclude outliers is checked, but no wells are flagged as extreme outliers (>P99 or >2x P90) to exclude."
        } else if (n_remaining < 5) {
          outlier_exclusion_note <- sprintf(
            "Exclude outliers is checked, but removing the %d extreme outlier well(s) would leave only %d -- below the 5-well minimum for reliable fitting/sampling, so no wells were excluded.",
            nrow(outliers_for_exclusion), n_remaining)
        } else {
          excluded_well_ids <- outliers_for_exclusion$well_id
          historical <- historical_raw %>% dplyr::filter(!well_id %in% excluded_well_ids)
          outlier_exclusion_note <- sprintf(
            "Excluding %d extreme outlier well(s) (%s) from the simulation and learning: %s.",
            length(excluded_well_ids), "frac days/stage > P99 or > 2x P90", paste(excluded_well_ids, collapse = ", "))
        }
      }

      risk_library_path <- if (is.null(input$risk_library_file)) {
        file.path(
          project_root, "data_templates",
          "risk_consequence_library_template_simple_severity.csv"
        )
      } else {
        input$risk_library_file$datapath
      }
      risk_library <- read.csv(risk_library_path, stringsAsFactors = FALSE) %>%
        validate_risk_consequence_library()

      assumptions <- bind_rows(current_locked_rows(), current_risk_rows()) %>%
        validate_assumptions()
      input_warnings <- c(
        attr(historical, "input_warnings") %||% character(0),
        attr(assumptions, "input_warnings") %||% character(0)
      )
      list(ok = TRUE, historical = historical, historical_raw = historical_raw,
           excluded_well_ids = excluded_well_ids, outlier_exclusion_note = outlier_exclusion_note,
           assumptions = assumptions,
           risk_library = risk_library,
           error = NULL, using_synthetic = using_synthetic,
           warnings = input_warnings)
    }, error = function(e) {
      list(ok = FALSE, historical = NULL, historical_raw = NULL,
           excluded_well_ids = character(0), outlier_exclusion_note = NULL,
           assumptions = NULL, risk_library = NULL,
           error = conditionMessage(e), using_synthetic = FALSE)
    })
  })

  output$status_message <- renderUI({
    dat <- input_data()
    if (!isTRUE(dat$ok)) {
      return(tags$div(
        tags$small(class = "text-danger fw-bold", "✘ Input error"),
        tags$p(class = "small text-danger mt-1 mb-0", dat$error)
      ))
    }
    warns <- dat$warnings %||% character(0)
    hist_line <- if (isTRUE(dat$using_synthetic)) {
      tags$div(class = "small text-warning",
        "⚠ historical_wells.csv: using 30 synthetic wells (upload your own for calibrated fits)")
    } else {
      tags$div(class = "small text-success",
        sprintf("✓ historical_wells.csv: %d wells", nrow(dat$historical)))
    }
    assump_line <- tags$div(class = "small text-success",
      sprintf("✓ master_risks_assumptions.csv: %d rows (%d risks)",
              nrow(dat$assumptions),
              sum(tolower(trimws(dat$assumptions$type)) == "risk", na.rm = TRUE)))
    warn_block <- if (length(warns) > 0) {
      tags$ul(class = "small text-warning mt-1 mb-0 ps-3",
        lapply(warns, function(w) tags$li(w)))
    }
    tags$div(class = "mt-1",
      hist_line, assump_line, warn_block,
      if (length(warns) == 0)
        tags$small(class = "text-success fw-bold mt-1 d-block", "Ready to run.")
      else
        tags$small(class = "text-warning fw-bold mt-1 d-block", "Ready — review warnings above.")
    )
  })

  # --- Async "Run simulation" ------------------------------------------------
  # The heavy lifting (build_run_args/detailed_runs below) now runs inside
  # future::future() in a separate background R process (see plan() at the
  # top of this file), so this Shiny session stays responsive -- other tabs,
  # inputs, and outputs keep working -- for the full 25-55s a large/Audit-mode
  # run can take, instead of freezing the whole session.
  #
  # Trade-off, stated plainly: a future runs in a different process with no
  # access to this session, so it cannot call withProgress()/setProgress()
  # (which only work in functions like setProgress(), called on
  # this session's reactive domain) and cannot use the per-mode/per-iteration
  # progress_callback wiring the synchronous path used to have. The granular
  # progress bar is replaced by a single busy/done/error status
  # (sim_running_rv / output$run_status below). Compare-both's .par_lapply()
  # fork-based parallelism (see test_compare_both_parallel.R) is preserved
  # unchanged inside the future's body -- a future worker process forking
  # its own children works exactly like the main process doing so.
  sim_result_rv  <- reactiveVal(NULL)
  sim_running_rv <- reactiveVal(FALSE)

  observeEvent(input$run, {
    if (isTRUE(sim_running_rv())) {
      showNotification("A simulation is already running -- please wait for it to finish.",
                        type = "warning", duration = 5)
      return(invisible())
    }

    dat <- input_data()
    validate(need(isTRUE(dat$ok), paste("Fix input files first:", dat$error)))

    modes <- if (input$operation_mode == "Compare both") c("Conventional", "Zipper") else input$operation_mode
    keep_full_logs <- input$execution_mode != "Fast"

    # Everything the background process needs must be captured as a plain
    # value HERE, synchronously, on the main session -- input$..., dat$...,
    # and bayes_merged_wells_rv() are reactive and cannot be read from inside
    # future::future()'s expression, which runs in a separate R process.
    historical_for_sim <- bayes_merged_wells_rv() %||% dat$historical
    assumptions_snapshot   <- dat$assumptions
    risk_library_snapshot  <- dat$risk_library
    ui_params <- list(
      n_wells = as.integer(input$n_wells),
      n_iterations = as.integer(input$n_iter),
      frac_fleets = input$frac_fleets,
      milling_units = input$milling_units,
      wireline_units = input$wireline_units,
      ct_units = input$ct_units,
      frac_trees = input$frac_trees,
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
      pre_frac_scheduling = input$pre_frac_scheduling,
      seed = as.integer(input$seed)
    )

    sim_running_rv(TRUE)

    fut <- future::future(
      {
        # multisession workers start as fresh R processes -- explicitly
        # source the engine files rather than relying on automatic global
        # detection to find every transitively-called helper function.
        source(file.path(project_root, "R", "engine_core.R"))
        source(file.path(project_root, "R", "summaries.R"))
        source(file.path(project_root, "R", "report_pdf.R"))
        source(file.path(project_root, "R", "optimiser_explain.R"))
        source(file.path(project_root, "R", "optimiser_cascade.R"))
        source(file.path(project_root, "R", "risk_library_engine.R"))
        source(file.path(project_root, "R", "optimiser_parallel.R"))

        build_run_args <- function(mode_index) {
          list(
            historical_wells = historical_for_sim,
            assumptions = assumptions_snapshot,
            n_wells = ui_params$n_wells,
            n_iterations = ui_params$n_iterations,
            frac_fleets = ui_params$frac_fleets,
            milling_units = ui_params$milling_units,
            wireline_units = ui_params$wireline_units,
            ct_units = ui_params$ct_units,
            frac_trees = ui_params$frac_trees,
            operation_mode = modes[[mode_index]],
            zipper_efficiency = ui_params$zipper_efficiency,
            risk_multiplier = ui_params$risk_multiplier,
            wireline_time_per_stage_hours = ui_params$wireline_time_per_stage_hours,
            wireline_rig_up_down_hours = ui_params$wireline_rig_up_down_hours,
            wireline_contingency_pct = ui_params$wireline_contingency_pct,
            frac_time_per_stage_hours = ui_params$frac_time_per_stage_hours,
            frac_settling_time_hours = ui_params$frac_settling_time_hours,
            well_to_well_transition_hours = ui_params$well_to_well_transition_hours,
            pad_to_pad_move_hours = ui_params$pad_to_pad_move_hours,
            frac_tree_swap_delay_hours = ui_params$frac_tree_swap_delay_hours,
            allow_ct_for_milling = ui_params$allow_ct_for_milling,
            ct_milling_efficiency = ui_params$ct_milling_efficiency,
            testing_units = ui_params$testing_units,
            flowback_testing_days_min = ui_params$flowback_testing_days_min,
            flowback_testing_days_max = ui_params$flowback_testing_days_max,
            pre_frac_scheduling = ui_params$pre_frac_scheduling,
            risk_library = risk_library_snapshot,
            # Same seed for every mode (common random numbers), not
            # offset by position in `modes` -- offsetting by mode_index
            # made a mode's result depend on what ELSE was being run
            # alongside it: "Zipper" alone got seed+0 (mode_index=1 in its
            # own 1-element modes vector) while "Zipper" inside "Compare
            # both" got seed+1 (mode_index=2), so the same operation_mode
            # produced two different P50s depending on the sidebar's
            # Operation mode selection alone. A shared seed also makes the
            # Conventional-vs-Zipper "saving" comparison (e.g.
            # build_zipper_benefit_breakdown()) a genuine paired comparison
            # instead of two independent samples -- matching the common-
            # random-numbers convention optimise_campaign_scenarios()
            # already uses for the scenario grid.
            seed = ui_params$seed
          )
        }

        tryCatch({
          # Compare-both runs Conventional and Zipper as two fully independent
          # simulate_campaign_detailed() calls -- fork them across cores with
          # the same .par_lapply() helper optimiser_parallel.R already uses
          # for the scenario grid (regression-tested bit-identical; see
          # test_compare_both_parallel.R) rather than inventing a second
          # parallel-execution mechanism.
          use_parallel <- length(modes) > 1 &&
            .Platform$OS.type != "windows" &&
            parallel::detectCores() > 1

          if (use_parallel) {
            args_list <- lapply(seq_along(modes), build_run_args)
            detailed_runs <- .par_lapply(
              seq_along(modes),
              function(mode_index) {
                list(result = do.call(simulate_campaign_detailed, args_list[[mode_index]]),
                     args = args_list[[mode_index]])
              },
              n_cores = length(modes)
            )
          } else {
            detailed_runs <- lapply(seq_along(modes), function(mode_index) {
              args <- build_run_args(mode_index)
              res <- do.call(simulate_campaign_detailed, args)
              list(result = res, args = args)
            })
          }

          results_only <- lapply(detailed_runs, `[[`, "result")
          args_by_mode <- setNames(lapply(detailed_runs, `[[`, "args"), modes)

          list(
            ok = TRUE,
            summary = dplyr::bind_rows(lapply(results_only, `[[`, "summary")),
            well_details = if (keep_full_logs) dplyr::bind_rows(lapply(results_only, `[[`, "well_details")) else tibble::tibble(),
            risk_event_log = dplyr::bind_rows(lapply(results_only, `[[`, "risk_event_log")),
            resource_utilization = dplyr::bind_rows(lapply(results_only, `[[`, "resource_utilization")),
            assumptions_used = dplyr::bind_rows(lapply(results_only, `[[`, "assumptions_used")) %>% dplyr::distinct(),
            args_by_mode = args_by_mode
          )
        }, error = function(e) {
          list(ok = FALSE, error = conditionMessage(e))
        })
      },
      seed = FALSE,  # each simulate_campaign_detailed() call seeds itself internally (see optimiser_parallel.R)
      globals = list(
        project_root = project_root, modes = modes, keep_full_logs = keep_full_logs,
        historical_for_sim = historical_for_sim, assumptions_snapshot = assumptions_snapshot,
        risk_library_snapshot = risk_library_snapshot, ui_params = ui_params
      )
    )

    promises::then(fut,
      onFulfilled = function(value) {
        sim_running_rv(FALSE)
        if (isTRUE(value$ok)) {
          sim_result_rv(value)
        } else {
          showNotification(paste("Simulation error:", value$error), type = "error", duration = NULL)
          sim_result_rv(NULL)
        }
      },
      onRejected = function(error) {
        sim_running_rv(FALSE)
        showNotification(paste("Simulation error:", conditionMessage(error)), type = "error", duration = NULL)
        sim_result_rv(NULL)
      }
    )
    invisible(NULL)
  })

  output$run_status <- renderUI({
    if (isTRUE(sim_running_rv())) {
      tags$div(class = "small text-primary mt-1",
        tags$span(class = "spinner-border spinner-border-sm me-1", role = "status"),
        "Running simulation in the background -- the rest of the app stays responsive.")
    }
  })

  sim_results <- reactive({ sim_result_rv() })

  # --- Derived tables: each computed ONCE per run ----------------------------

  resource_summary_r <- reactive({ req(sim_results()); summarise_resource_utilization(sim_results()$resource_utilization) })
  bottlenecks_r      <- reactive({ summarise_bottlenecks(resource_summary_r()) })
  sim_stats_r        <- reactive({ req(sim_results()); summarise_simulation(sim_results()$summary) })
  delay_r            <- reactive({ req(sim_results()); summarise_delay_contributors(sim_results()$risk_event_log) })
  stage_risk_r       <- reactive({ req(sim_results()); summarise_stage_level_risks(sim_results()$risk_event_log, sim_results()$summary) })
  risk_heatmap_r     <- reactive({ req(sim_results()); build_schedule_risk_heatmap(sim_results()$risk_event_log, sim_results()$summary) })
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
  # Overview's "Critical bottleneck" card is now built from rec_v2_r()
  # (defined below, near input$verify_rec) instead of
  # build_bottleneck_narrative(), so it shares one source of truth with the
  # Decision Support tab's Recommendation card and the Optimiser's
  # constraint cascade.

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
  # Same verify button as Decision Support's Recommendation card, surfaced on
  # the Overview "Critical bottleneck" card too -- both write to the SAME
  # verified_rec_rv, so the two tabs can never show a verified vs. unverified
  # answer for the same run, and clicking either button updates both.
  observeEvent(input$verify_rec_overview, {
    req(sim_results())
    a <- sim_results()$args_by_mode[[focus_mode_r()]]
    a$progress_callback <- NULL; a$keep_logs <- FALSE; a$collect_well_details <- FALSE
    withProgress(message = "Verifying bottleneck impact by re-simulation", value = 0.4, {
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
    withProgress(message = "Checking recommendation robustness", value = 0, {
      robustness_rv(assess_recommendation_robustness(
        a,
        frac_fleet_cost_per_day = input$frac_fleet_cost,
        wireline_cost_per_day = input$wireline_cost,
        ct_cost_per_day = input$ct_cost,
        milling_cost_per_day = input$milling_cost,
        testing_unit_cost_per_day = input$testing_unit_cost,
        progress_callback = function(i, n) {
          setProgress(i / n,
            detail = if (i <= 1L) "Base case..." else sprintf("Assumption sweep: %d / %d", i - 1L, n - 1L))
        }
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

  # ---- Sensitivity Analysis (Issue #8) --------------------------------------
  sensitivity_rv <- reactiveVal(NULL)
  observeEvent(sim_results(), { sensitivity_rv(NULL) }, ignoreNULL = FALSE)

  observeEvent(input$run_sensitivity, {
    req(sim_results())
    withProgress(message = "Running sensitivity analysis", value = 0, {
      tryCatch({
        sensitivity_rv(run_sensitivity_analysis(
          args_by_mode = sim_results()$args_by_mode,
          progress_callback = function(i, n) {
            setProgress(i / n, detail = sprintf("Variable %d / %d swept", i, n))
          }
        ))
      }, error = function(e) {
        showNotification(paste("Sensitivity error:", conditionMessage(e)), type = "error", duration = 10)
      })
    })
  })

  output$sensitivity_status <- renderUI({
    sa <- sensitivity_rv()
    if (is.null(sa)) return(tags$small(class = "text-muted",
      "Not yet run. Click the button to sweep all planning variables and rank their impact on campaign duration."))
    n_vars  <- nrow(sa$ranking)
    top_lbl <- sa$ranking$label[1]
    tags$small(class = "text-success",
      sprintf("Done — %d variables swept across %s (±%s timing, ±%s risk prob., ±1 unit resources). Top driver: %s.",
              n_vars, paste(sa$modes, collapse = " & "),
              sprintf("%.0f%%", 100 * sa$scalar_perturb_pct),
              sprintf("%.0f%%", 100 * sa$risk_perturb_pct),
              top_lbl))
  })

  output$sensitivity_tornado_plot <- renderPlot({
    plot_sensitivity_tornado(sensitivity_rv())
  }, res = 96)

  output$sensitivity_bymode_plot <- renderPlot({
    plot_sensitivity_by_mode(sensitivity_rv())
  }, res = 96)

  output$sensitivity_ranking_table <- DT::renderDT({
    sa <- sensitivity_rv()
    if (is.null(sa)) return(DT::datatable(tibble(), options = list(dom = "t")))
    df <- sa$ranking %>%
      transmute(
        Rank     = rank,
        Variable = label,
        Category = category,
        Type     = type,
        `Mean swing (d)` = round(mean_swing, 2),
        `Max swing (d)`  = round(max_swing, 2)
      )
    DT::datatable(df, rownames = FALSE,
                  options = list(dom = "tp", pageLength = 15, scrollX = TRUE)) %>%
      DT::formatStyle("Rank", fontWeight = "bold") %>%
      DT::formatStyle("Category",
        backgroundColor = DT::styleEqual(
          c("Timing", "Risk", "Resource", "Operations"),
          c("#e8f4f8", "#fce8e0", "#e0f4ec", "#f4e0f0")
        ))
  })

  output$sensitivity_detail_table <- DT::renderDT({
    sa <- sensitivity_rv()
    if (is.null(sa)) return(DT::datatable(tibble(), options = list(dom = "t")))
    pct_s <- sprintf("%.0f%%", 100 * sa$scalar_perturb_pct)
    pct_r <- sprintf("%.0f%%", 100 * sa$risk_perturb_pct)
    .not_tested <- "Not tested: already at minimum feasible value"
    df <- sa$summary %>%
      left_join(sa$ranking %>% select(variable, rank = rank), by = "variable") %>%
      arrange(rank, operation_mode) %>%
      transmute(
        Rank              = rank,
        Variable          = label,
        Category          = category,
        Mode              = operation_mode,
        `Base P50 (d)`    = round(base_p50, 1),
        `Low P50 (d)`     = if_else(low_skipped,  .not_tested, sprintf("%.1f", low_p50)),
        `Low delta (d)`   = if_else(low_skipped,  .not_tested, sprintf("%+.2f", low_delta)),
        `High P50 (d)`    = if_else(high_skipped, .not_tested, sprintf("%.1f", high_p50)),
        `High delta (d)`  = if_else(high_skipped, .not_tested, sprintf("%+.2f", high_delta)),
        `Total swing (d)` = round(swing, 2),
        `Contribution %`  = round(contribution_pct, 1)
      )
    DT::datatable(df, rownames = FALSE,
                  caption = htmltools::tags$caption(
                    style = "caption-side: bottom; text-align: left; font-size: 0.82em; color: #666;",
                    sprintf("Low = -%s (timing/ops) or -%s (risk prob.) or -1 unit (resource). High = +%s / +%s / +1.",
                            pct_s, pct_r, pct_s, pct_r)),
                  options = list(dom = "tp", pageLength = 20, scrollX = TRUE)) %>%
      DT::formatStyle(
        "Total swing (d)",
        background = DT::styleColorBar(range(sa$summary$swing, na.rm = TRUE), "#d4edda"),
        backgroundSize = "95% 60%", backgroundRepeat = "no-repeat", backgroundPosition = "center"
      )
  })

  output$recommendation_confidence <- renderUI({
    req(sim_results())
    rec  <- rec_v2_r()
    conf <- combine_recommendation_confidence(rec, robustness_rv())
    badge_class <- switch(conf$level,
      "High" = "bg-success", "Moderate" = "bg-warning text-dark",
      "Low" = "bg-danger", "Inconclusive" = "bg-secondary")
    verdict_class <- switch(rec$decision_status,
      "Recommended" = "bg-success", "Optional" = "bg-warning text-dark",
      "Not justified" = "bg-secondary")
    tagList(
      tags$span(class = paste("badge", verdict_class), rec$decision_status),
      tags$span(class = paste("badge ms-1", badge_class), conf$label),
      tags$div(class = "small text-muted mt-1", rec$decision_reason),
      tags$ul(class = "mt-2 mb-2 small text-muted", lapply(conf$detail, tags$li))
    )
  })

  output$recommendation_decision_rules <- renderUI({
    th <- REC_DECISION_THRESHOLDS
    tags$div(class = "small text-muted mt-1",
      tags$p(class = "mb-1", tags$b("Not justified"), " -- net value is not positive, or P50 reduction is below ",
             sprintf("%.1f d", th$min_p50_reduction_days), "."),
      tags$p(class = "mb-1", tags$b("Optional"), " -- net value is positive, but win-rate confidence is below ",
             sprintf("%.0f%%", 100 * th$confidence_moderate_win_rate), " (Low/Inconclusive)."),
      tags$p(class = "mb-0", tags$b("Recommended"), " -- net value is positive and win-rate confidence is at least ",
             sprintf("%.0f%%", 100 * th$confidence_moderate_win_rate), " (Moderate/High)."),
      tags$p(class = "mb-0 mt-2 fst-italic",
        "Thresholds are defined once in REC_DECISION_THRESHOLDS (R/recommendations.R) and used directly by recommend_action().")
    )
  })

  # ---- What-If Scenario Builder (Issue #11) ---------------------------------
  whatif_rv <- reactiveVal(NULL)
  observeEvent(sim_results(), { whatif_rv(NULL) }, ignoreNULL = FALSE)

  observeEvent(input$run_whatif, {
    req(sim_results())

    # Build variants from the three UI rows; skip any with a blank label.
    .wif_variant <- function(label_in, mode_in, frac_in, wl_in, ct_in, ml_in, tu_in) {
      lbl <- trimws(label_in)
      if (!nzchar(lbl)) return(NULL)
      overrides <- list()
      if (nzchar(mode_in)) overrides$operation_mode <- mode_in
      if (!is.na(frac_in)) overrides$frac_fleets    <- as.integer(frac_in)
      if (!is.na(wl_in))   overrides$wireline_units <- as.integer(wl_in)
      if (!is.na(ct_in))   overrides$ct_units        <- as.integer(ct_in)
      if (!is.na(ml_in))   overrides$milling_units   <- as.integer(ml_in)
      if (!is.na(tu_in))   overrides$testing_units    <- as.integer(tu_in)
      setNames(list(overrides), lbl)
    }

    variants <- c(
      .wif_variant(input$wif_v1_label, input$wif_v1_mode, input$wif_v1_frac,
                   input$wif_v1_wl, input$wif_v1_ct, input$wif_v1_ml, input$wif_v1_tu),
      .wif_variant(input$wif_v2_label, input$wif_v2_mode, input$wif_v2_frac,
                   input$wif_v2_wl, input$wif_v2_ct, input$wif_v2_ml, input$wif_v2_tu),
      .wif_variant(input$wif_v3_label, input$wif_v3_mode, input$wif_v3_frac,
                   input$wif_v3_wl, input$wif_v3_ct, input$wif_v3_ml, input$wif_v3_tu)
    )

    base_args <- sim_results()$args_by_mode[[focus_mode_r()]]
    withProgress(message = "Running what-if comparison", value = 0, {
      tryCatch({
        whatif_rv(run_whatif_batch(
          base_args, variants,
          frac_fleet_cost_per_day   = input$frac_fleet_cost,
          wireline_cost_per_day     = input$wireline_cost,
          ct_cost_per_day           = input$ct_cost,
          milling_cost_per_day      = input$milling_cost,
          testing_unit_cost_per_day = input$testing_unit_cost,
          progress_callback = function(i, n) {
            setProgress(i / n, detail = sprintf("Scenario %d / %d", i, n))
          }
        ))
      }, error = function(e) {
        showNotification(paste("What-if error:", conditionMessage(e)), type = "error", duration = 10)
      })
    })
  })

  output$whatif_status <- renderUI({
    wif <- whatif_rv()
    if (is.null(wif)) return(tags$small(class = "text-muted",
      "Not yet run. Fill in at least one variant label and click Run comparison."))
    n_sc <- length(wif$scenarios)
    tags$small(class = "text-success",
      sprintf("Done — %d scenario%s compared (base + %d variant%s).",
              n_sc, if (n_sc != 1) "s" else "",
              n_sc - 1L, if (n_sc - 1L != 1) "s" else ""))
  })

  output$whatif_bars_plot <- renderPlot({
    plot_whatif_bars(whatif_rv())
  }, res = 96)

  output$whatif_scurve_plot <- renderPlot({
    plot_whatif_scurve(whatif_rv())
  }, res = 96)

  output$whatif_comparison_table <- DT::renderDT({
    wif <- whatif_rv()
    if (is.null(wif)) return(DT::datatable(tibble(), options = list(dom = "t")))
    DT::datatable(wif$comparison, rownames = FALSE,
                  options = list(dom = "t", scrollX = TRUE)) %>%
      DT::formatStyle("Scenario", fontWeight = "bold") %>%
      DT::formatStyle("Δ P50 vs base",
        color = DT::styleInterval(c(-0.001, 0.001), c("#009E73", "black", "#D55E00"))) %>%
      DT::formatStyle("Δ cost vs base",
        color = DT::styleInterval(c(-0.001, 0.001), c("#009E73", "black", "#D55E00")))
  })

  # ---- Historical Learning Engine (Issue #6) --------------------------------
  # Runs automatically whenever historical wells data changes; no button needed.
  learning_r <- reactive({
    dat <- input_data()
    req(isTRUE(dat$ok))
    tryCatch(
      learn_from_historical(dat$historical),
      error = function(e) { warning("Learning engine: ", conditionMessage(e)); NULL }
    )
  })

  outlier_r <- reactive({
    dat <- input_data()
    req(isTRUE(dat$ok))
    # Always analyses historical_raw (every well, unfiltered) so this card
    # keeps showing the full picture even while the exclusion checkbox below
    # is removing flagged wells from the simulation and learning engine --
    # otherwise an excluded well would vanish from its own outlier table.
    tryCatch(
      summarise_outlier_wells(dat$historical_raw, metric = "frac_days_per_stage"),
      error = function(e) { warning("Outlier summary: ", conditionMessage(e)); NULL }
    )
  })

  output$outlier_exclusion_status <- renderUI({
    dat <- tryCatch(input_data(), error = function(e) NULL)
    if (is.null(dat) || is.null(dat$outlier_exclusion_note)) return(NULL)
    cls <- if (length(dat$excluded_well_ids) > 0) "text-warning" else "text-muted"
    tags$small(class = paste("d-block mt-1", cls), dat$outlier_exclusion_note)
  })

  output$outlier_summary_table <- DT::renderDT({
    o <- tryCatch(outlier_r(), error = function(e) NULL)
    if (is.null(o)) return(DT::datatable(tibble(), options = list(dom = "t")))
    n_extreme <- if (nrow(o$outliers) == 0) 0L else sum(o$outliers$tier == "Extreme")
    n_watch <- nrow(o$outliers) - n_extreme
    df <- tibble::tibble(
      Metric = c("Wells analysed", "P50 duration", "P90 duration", "Maximum observed",
                 if (is.na(o$threshold)) "Watch-list outliers (>P95)"
                 else sprintf("Watch-list outliers (>P95, %.2f d)", o$threshold),
                 if (is.na(o$extreme_threshold)) "Extreme outliers (>P99 or >2x P90)"
                 else sprintf("Extreme outliers (>P99 or >2x P90, %.2f d)", o$extreme_threshold)),
      Value = c(as.character(o$n_wells),
                if (is.na(o$p50)) "N/A" else sprintf("%.2f d", o$p50),
                if (is.na(o$p90)) "N/A" else sprintf("%.2f d", o$p90),
                if (is.na(o$max)) "N/A" else sprintf("%.2f d", o$max),
                as.character(n_watch),
                as.character(n_extreme))
    )
    DT::datatable(df, rownames = FALSE, options = list(dom = "t"))
  })

  output$outlier_wells_table <- DT::renderDT({
    o <- tryCatch(outlier_r(), error = function(e) NULL)
    if (is.null(o) || nrow(o$outliers) == 0) {
      return(DT::datatable(tibble(message = "No wells above the P95 threshold for this metric."),
                           options = list(dom = "t"), rownames = FALSE))
    }
    dat <- tryCatch(input_data(), error = function(e) NULL)
    excluded_ids <- if (!is.null(dat)) dat$excluded_well_ids else character(0)
    df <- o$outliers %>%
      dplyr::transmute(`Well ID` = well_id, `Duration (d/stage)` = round(value, 2),
                       Tier = tier,
                       `Excluded?` = ifelse(well_id %in% excluded_ids, "Yes", "No"))
    DT::datatable(df, rownames = FALSE, selection = "single",
                  options = list(dom = "t", pageLength = 10)) %>%
      DT::formatStyle("Tier",
        color = DT::styleEqual(c("Extreme", "Watch-list"), c("#d62728", "#b8860b")), fontWeight = "bold") %>%
      DT::formatStyle("Excluded?",
        color = DT::styleEqual(c("Yes", "No"), c("#d62728", "#888888")), fontWeight = "bold")
  })

  output$outlier_well_detail <- renderUI({
    o <- tryCatch(outlier_r(), error = function(e) NULL)
    sel <- input$outlier_wells_table_rows_selected
    if (is.null(o) || is.null(sel) || nrow(o$outliers) == 0) return(NULL)
    row <- o$outliers[sel, ]
    div(class = "mt-2 p-2 border-start border-warning border-3 bg-light rounded",
      tags$strong(row$well_id),
      tags$span(sprintf(" (%s)", row$tier), class = "text-muted"),
      tags$br(),
      tags$span(sprintf("Duration: %.2f d/stage", row$value)),
      tags$br(),
      tags$strong("Possible reason: "), tags$span(row$possible_reason)
    )
  })

  output$learning_status <- renderUI({
    lr <- tryCatch(learning_r(), error = function(e) NULL)
    dat <- tryCatch(input_data(), error = function(e) NULL)
    if (is.null(lr)) {
      return(tags$p(class = "text-muted",
        "Upload historical_wells.csv to automatically fit duration distributions. ",
        "When no file is uploaded the simulator uses synthetic baseline data ",
        "(flagged in the sidebar) — distribution fitting still runs on that data."))
    }
    n_wells <- if (!is.null(dat) && isTRUE(dat$ok)) nrow(dat$historical) else 0
    using_synthetic <- isTRUE(dat$using_synthetic)

    tags$div(
      if (using_synthetic)
        tags$p(class = "text-warning fw-bold",
          sprintf("Using %d synthetic wells — upload historical_wells.csv for calibrated fits.", n_wells))
      else
        tags$p(class = "text-success fw-bold",
          sprintf("Fitted on %d historical wells.", n_wells)),
      lapply(lr, function(r) {
        if (is.null(r$best_fit)) {
          return(div(class = "mb-2", tags$strong(r$label), tags$span(class = "text-muted ms-1", r$note)))
        }
        quality_cls <- switch(r$fit_quality, Good = "text-success", Moderate = "text-warning",
                              Poor = "text-danger", "text-muted")
        div(class = "mb-3 p-2 border rounded",
          tags$div(class = "small text-muted", r$label),
          tags$div(tags$strong("Selected planning distribution: "), r$best_fit$family),
          tags$div(tags$strong("Fit quality: "), tags$span(class = paste("fw-bold", quality_cls), r$fit_quality)),
          tags$div(tags$strong("Note: "), tags$span(class = "small", r$fit_quality_note)),
          tags$div(class = "small mt-1",
            sprintf("Suggested triangular: min=%.3f d, mode=%.3f d, max=%.3f d.",
                    r$suggested_min, r$suggested_mode, r$suggested_max))
        )
      })
    )
  })

  output$learning_density_plot <- renderPlot({
    plot_learning_density(tryCatch(learning_r(), error = function(e) NULL))
  }, res = 96)

  output$learning_qq_plot <- renderPlot({
    plot_learning_qq(tryCatch(learning_r(), error = function(e) NULL))
  }, res = 96)

  output$learning_fit_table <- DT::renderDT({
    lr <- tryCatch(learning_r(), error = function(e) NULL)
    if (is.null(lr)) return(DT::datatable(tibble(), options = list(dom = "t")))
    df <- bind_rows(lapply(lr, function(r) {
      if (is.null(r$fit_table)) return(NULL)
      r$fit_table %>% mutate(Parameter = r$label) %>%
        select(Parameter, everything())
    })) %>%
      mutate(`Fit quality` = ifelse(is.na(`Fit quality`), "N/A", `Fit quality`))
    # A degenerate (e.g. zero-variance) input column can make fitdistr()
    # return an infinite AIC for one or more candidates. range() then
    # includes +-Inf, which styleColorBar() serialises as the literal token
    # "Inf" into the generated JS -- not valid JS, so the whole table
    # renders empty with a "ReferenceError: Inf is not defined" console
    # error. Fall back to the finite AIC values only.
    finite_aic <- df$AIC[is.finite(df$AIC)]
    aic_range <- if (length(finite_aic) > 0) range(finite_aic) else c(0, 1)
    DT::datatable(df %>% select(-Best), rownames = FALSE,
                  options = list(dom = "t", scrollX = TRUE, pageLength = 8)) %>%
      DT::formatStyle("Rank",
        target = "row",
        backgroundColor = DT::styleEqual(1, "#d4edda")) %>%
      DT::formatStyle("AIC",
        background = DT::styleColorBar(aic_range, "#cce5ff"),
        backgroundSize = "90% 55%", backgroundRepeat = "no-repeat",
        backgroundPosition = "center") %>%
      DT::formatStyle("Fit quality",
        color = DT::styleEqual(c("Good", "Moderate", "Poor", "N/A"),
                                c("#1b9e77", "#e6ab02", "#d62728", "#888888")),
        fontWeight = "bold")
  })

  output$learning_desc_table <- DT::renderDT({
    lr <- tryCatch(learning_r(), error = function(e) NULL)
    if (is.null(lr)) return(DT::datatable(tibble(), options = list(dom = "t")))
    df <- bind_rows(lapply(lr, function(r) {
      if (is.null(r$desc)) return(NULL)
      r$desc %>% mutate(Parameter = r$label) %>% select(Parameter, everything()) %>%
        mutate(across(where(is.numeric), ~ round(.x, 4)))
    }))
    DT::datatable(df, rownames = FALSE,
                  options = list(dom = "t", scrollX = TRUE))
  })

  output$learning_suggested_table <- DT::renderDT({
    lr <- tryCatch(learning_r(), error = function(e) NULL)
    if (is.null(lr)) return(DT::datatable(tibble(), options = list(dom = "t")))
    df <- suggested_assumptions_table(lr)
    DT::datatable(df, rownames = FALSE,
                  options = list(dom = "t", scrollX = TRUE)) %>%
      DT::formatStyle("Selected planning distribution", fontWeight = "bold") %>%
      DT::formatStyle("Fit quality",
        color = DT::styleEqual(c("Good", "Moderate", "Poor", "N/A"),
                                c("#1b9e77", "#e6ab02", "#d62728", "#888888")),
        fontWeight = "bold")
  })

  # ---- Bayesian Update (Issue #7) -------------------------------------------

  # Holds the merged (prior + new) historical wells when the user applies the update.
  bayes_merged_wells_rv <- reactiveVal(NULL)

  # Reactive: run the Bayesian update whenever new-wells file changes.
  bayes_result_r <- reactive({
    req(input$bayes_new_wells_file)
    dat <- input_data()
    validate(need(isTRUE(dat$ok), "Fix input files before running Bayesian update."))

    new_wells <- tryCatch(
      load_historical_wells(input$bayes_new_wells_file$datapath),
      error = function(e) {
        showNotification(paste("New wells file error:", conditionMessage(e)),
                         type = "error", duration = 10)
        return(NULL)
      }
    )
    req(!is.null(new_wells))

    risk_obs <- if (!is.null(input$bayes_risk_obs_file)) {
      tryCatch(
        load_risk_observations(input$bayes_risk_obs_file$datapath),
        error = function(e) {
          showNotification(paste("Risk obs file error:", conditionMessage(e)),
                           type = "warning", duration = 8)
          NULL
        }
      )
    } else NULL

    run_bayesian_update(
      historical_wells = dat$historical,
      new_wells        = new_wells,
      assumptions      = dat$assumptions,
      risk_obs         = risk_obs,
      prior_strength   = input$bayes_prior_strength
    )
  })

  # Evidence/decision/decision-reason trail for the audit zip + PDF report
  # (Issue #43). NULL when no Bayesian update has been run this session --
  # uses tryCatch rather than req() so it degrades to NULL instead of
  # propagating a silent "not ready" condition to callers like the audit
  # package handler that need a definite value.
  bayes_decision_r <- reactive({
    br <- tryCatch(bayes_result_r(), error = function(e) NULL)
    if (is.null(br)) return(NULL)
    list(
      duration = assess_duration_update(br$duration_update),
      risk     = assess_risk_update(br$risk_update)
    )
  })

  output$bayes_status <- renderUI({
    if (is.null(input$bayes_new_wells_file)) {
      return(tags$p(class = "text-muted",
        "Upload a new campaign wells CSV (same format as historical_wells.csv) to run the Bayesian update. ",
        "Optionally also upload a risk observations CSV."))
    }
    br <- tryCatch(bayes_result_r(), error = function(e) NULL)
    if (is.null(br)) return(tags$p(class = "text-danger", "Update failed — check the file format."))

    # All evidence strength / decision / recommendation / narrative fields
    # come from assess_duration_update() / assess_risk_update() in
    # bayesian_updater.R -- this render function only formats them. See
    # those functions for the thresholds and reasoning.
    dur <- assess_duration_update(br$duration_update)
    ru  <- assess_risk_update(br$risk_update)
    has_risk <- !is.null(ru) && nrow(ru) > 0

    .evidence_word <- function(strength) switch(strength, "Strong" = "strong", "Moderate" = "moderate", "weak")
    .evidence_rank <- c(weak = 1L, moderate = 2L, strong = 3L)

    # ---- 0. Campaign Learning Summary card (operational wording, TOP) --------
    dur_status <- if (all(dur$direction == "Stable")) "Stable" else dur$direction[which.max(abs(dur$delta_mean))]
    dur_evidence_words <- vapply(dur$evidence_strength, .evidence_word, character(1))
    dur_ranks <- .evidence_rank[dur_evidence_words]
    dur_evidence <- names(dur_ranks)[which.min(dur_ranks)]

    if (has_risk) {
      risk_status <- if (all(ru$direction == "Stable")) "Stable" else ru$direction[which.max(abs(ru$delta_prob))]
      risk_evidence_words <- vapply(ru$evidence_strength, .evidence_word, character(1))
      risk_ranks <- .evidence_rank[risk_evidence_words]
      risk_evidence <- names(risk_ranks)[which.min(risk_ranks)]
    } else {
      risk_status <- "Not assessed"
      risk_evidence <- "weak"
    }

    all_levels <- c(dur$recommendation_level, if (has_risk) ru$recommendation_level)
    action_text <- if (any(all_levels == "Recommended")) {
      "Update flagged assumptions — see Decision column below."
    } else if (any(all_levels == "Optional")) {
      "Review flagged items; no urgent update needed."
    } else {
      "Continue monitoring — no updates currently justified."
    }

    learning_summary_card <- card(
      class = "mb-3 border-primary",
      card_header(class = "bg-primary text-white fw-bold", "Campaign Learning Summary"),
      card_body(class = "py-2",
        tags$div(class = "row text-center g-0",
          tags$div(class = "col border-end",
            tags$div(class = "small text-muted fw-semibold", "Historical wells"),
            tags$div(class = "fs-5 fw-bold", br$n_prior)
          ),
          tags$div(class = "col border-end",
            tags$div(class = "small text-muted fw-semibold", "New wells"),
            tags$div(class = "fs-5 fw-bold", br$n_new)
          ),
          tags$div(class = "col border-end",
            tags$div(class = "small text-muted fw-semibold", "Duration assumptions"),
            tags$div(class = "fs-5 fw-bold", sprintf("%s (evidence %s)", dur_status, dur_evidence))
          ),
          tags$div(class = "col border-end",
            tags$div(class = "small text-muted fw-semibold", "Risk assumptions"),
            tags$div(class = "fs-5 fw-bold", sprintf("%s (evidence %s)", risk_status, risk_evidence))
          ),
          tags$div(class = "col",
            tags$div(class = "small text-muted fw-semibold", "Recommended action"),
            tags$div(class = "fs-6 fw-bold text-primary", action_text)
          )
        )
      )
    )

    # ---- 0b. Decision Rules card (static reference, always visible) ----------
    # Plain-English statement of the rule the engine actually runs -- see
    # assess_duration_update() / assess_risk_update() in bayesian_updater.R
    # for the code that implements exactly this. Numbers below are pulled
    # from BAYES_DECISION_THRESHOLDS so this card can never drift out of
    # sync with the logic.
    th <- BAYES_DECISION_THRESHOLDS
    decision_rules_card <- card(
      class = "mb-3",
      card_header(tags$span(class = "fw-bold", "Decision Rules")),
      card_body(
        tags$div(class = "row",
          tags$div(class = "col-md-6",
            tags$h6(class = "text-uppercase text-muted small", "Duration assumptions"),
            tags$p(class = "mb-1", tags$b("Retain assumption")),
            tags$ul(class = "small text-muted",
              tags$li("Posterior shift not statistically meaningful"),
              tags$li("Credible interval includes zero"),
              tags$li("Evidence = Weak")
            ),
            tags$p(class = "mb-1", tags$b("Review assumption")),
            tags$ul(class = "small text-muted",
              tags$li("Moderate evidence"),
              tags$li("Shift approaching operational significance")
            ),
            tags$p(class = "mb-1", tags$b("Update assumption")),
            tags$ul(class = "small text-muted",
              tags$li("Strong evidence"),
              tags$li("Credible interval excludes zero"),
              tags$li("Operationally meaningful shift")
            )
          ),
          tags$div(class = "col-md-6",
            tags$h6(class = "text-uppercase text-muted small", "Risk assumptions"),
            tags$p(class = "mb-1", tags$b("Update assumption")),
            tags$ul(class = "small text-muted",
              tags$li("Evidence = Moderate or Strong"),
              tags$li("Minimum observation count met"),
              tags$li("Posterior shift exceeds update threshold")
            ),
            tags$p(class = "mb-1", tags$b("Monitor")),
            tags$ul(class = "small text-muted",
              tags$li("Signal exists but evidence remains limited"),
              tags$li("Additional observations recommended")
            ),
            tags$p(class = "mb-1", tags$b("No action")),
            tags$ul(class = "small text-muted",
              tags$li("Shift is small or evidence remains weak")
            )
          )
        ),
        tags$p(class = "small text-muted mt-2 mb-0 fst-italic",
          "Recommendations are generated automatically from Bayesian evidence, sample size and posterior shift thresholds.")
      )
    )

    # ---- 0c. Decision Thresholds (expandable, numbers read live from code) ---
    decision_thresholds_card <- tags$details(class = "mb-3",
      tags$summary(class = "fw-bold", style = "cursor: pointer;", "Decision Thresholds"),
      tags$div(class = "small text-muted mt-2",
        tags$p(class = "mb-1", tags$b("Update assumption"), " (risk events) requires ALL of:"),
        tags$ul(
          tags$li(sprintf("≥ %d observations (trials)", th$min_trials_for_update)),
          tags$li(sprintf("≥ %d events", th$min_events_for_update)),
          tags$li(sprintf("≥ %.0f percentage-point posterior shift", 100 * th$min_posterior_shift_pp)),
          tags$li(sprintf("≥ %.0f%% relative shift from the prior probability (e.g. roughly doubling) -- ",
                          100 * th$min_relative_shift),
            "this stops a modest move on a larger base rate from being treated the same as a large move on a small one"),
          tags$li("Moderate or Strong evidence")
        ),
        tags$p(class = "mb-1 mt-2", tags$b("Monitor"), " (risk events) -- a positive shift that doesn't clear the Update bar above:"),
        tags$ul(
          tags$li(sprintf("Moderate evidence: any shift that isn't negligible is flagged for monitoring")),
          tags$li(sprintf("Weak evidence: still needs ≥ %.0f%% relative shift to be worth watching, otherwise it's treated as noise ('No action')",
                          100 * th$min_relative_shift_for_monitor))
        ),
        tags$p(class = "mb-1 mt-2", tags$b("Evidence strength"), " is driven by sample size:"),
        tags$ul(
          tags$li(sprintf("Strong: ≥ %d observations, AND a narrow posterior credible interval (duration: 90%% CI width < %.0f%% of the prior mean; risk: 90%% CI width < %.0f percentage points), AND a direction that isn't just prior-anchoring noise",
                          th$min_n_for_strong_evidence, 100 * th$duration_ci_narrow_rel, 100 * th$risk_ci_narrow_pp)),
          tags$li(sprintf("Moderate: ≥ %d observations", th$min_n_for_moderate_evidence)),
          tags$li(sprintf("Weak: < %d observations", th$min_n_for_moderate_evidence))
        ),
        tags$p(class = "mb-0 mt-2",
          tags$b("Why these are conservative: "),
          "a false-positive assumption change is more costly to a campaign plan than waiting for one more well or a few more risk-event trials, so the thresholds deliberately require a fairly large sample AND a fairly large shift before recommending an update."),
        tags$p(class = "mb-0 mt-2 fst-italic",
          "All thresholds are defined once in BAYES_DECISION_THRESHOLDS (R/bayesian_updater.R) and used directly by the code that produces every Decision and Decision Reason shown on this page.")
      )
    )

    # ---- 1. Bayesian Learning Summary (structured executive narrative) -------
    # Replaces the old blanket "observed events are more frequent than
    # assumed" statement with per-parameter / per-risk lines that are
    # explicit about direction, evidence strength, and recommendation --
    # never asserting a directional claim that the evidence doesn't support.
    .decision_badge_cls <- function(level) switch(level,
      "Recommended" = "bg-danger text-white", "Optional" = "bg-warning text-dark", "bg-secondary")

    dur_all_retain <- all(dur$decision == "Retain assumption")
    dur_summary_lines <- if (dur_all_retain) {
      list(tags$div(class = "py-1",
        tags$span(class = "fw-semibold me-2", "Stable"),
        tags$span(class = "text-muted me-2", sprintf("Evidence %s.", tolower(dur$evidence_strength[1]))),
        tags$span(class = "text-success", "No updates recommended.")
      ))
    } else {
      lapply(seq_len(nrow(dur)), function(i) {
        r <- dur[i, ]
        tags$div(class = "py-1 border-bottom",
          tags$span(class = "fw-semibold me-2", r$label),
          tags$span(class = if (r$direction == "Stable") "text-secondary" else "text-danger",
            sprintf("%s (%+.3f d)", r$direction, r$delta_mean)),
          tags$span(class = "badge ms-2", .decision_badge_cls(r$recommendation_level), r$decision),
          tags$div(class = "small text-muted mt-1", sprintf("Evidence %s. %s", tolower(r$evidence_strength), r$recommendation_text))
        )
      })
    }

    # Every risk event gets the same fully-traceable sentence structure --
    # observed counts, evidence strength, threshold outcome, recommendation
    # -- so no event's decision relies on wording the others don't get.
    risk_summary_lines <- if (!has_risk) {
      list(tags$p(class = "text-muted small mb-0",
        "Upload a risk observations CSV to update risk-event probabilities."))
    } else {
      lapply(seq_len(nrow(ru)), function(i) {
        r <- ru[i, ]
        tags$div(class = "py-1 border-bottom",
          tags$span(class = "fw-semibold me-2", r$risk_event),
          tags$span(class = "badge ms-2", .decision_badge_cls(r$recommendation_level), r$decision),
          tags$div(class = "small text-muted mt-1", r$narrative_full)
        )
      })
    }

    learning_narrative_card <- card(
      class = "mb-3",
      card_header(tags$span(class = "fw-bold", "Bayesian Learning Summary")),
      card_body(
        tags$h6(class = "text-muted small text-uppercase mb-1", "Duration assumptions"),
        do.call(tagList, dur_summary_lines),
        tags$h6(class = "text-muted small text-uppercase mb-1 mt-3", "Risk assumptions"),
        do.call(tagList, risk_summary_lines)
      )
    )

    # ---- 2a. Duration detail card (table with Evidence + Decision + Reason) --
    dur_rows <- lapply(seq_len(nrow(dur)), function(i) {
      r <- dur[i, ]
      chg_cls <- if (r$direction == "Stable") "text-secondary" else if (r$direction == "Increasing") "text-danger" else "text-success"
      tags$tr(
        tags$td(r$label),
        tags$td(sprintf("%.3f d", r$prior_mean)),
        tags$td(sprintf("%.3f d", r$posterior_mean)),
        tags$td(class = chg_cls, r$direction),
        tags$td(r$evidence_strength),
        tags$td(tags$span(class = paste("badge", .decision_badge_cls(r$recommendation_level)), r$decision)),
        tags$td(class = "small text-muted", r$decision_reason)
      )
    })

    dur_card <- card(
      card_header(
        tags$span(class = "fw-bold", "Duration Detail"),
        tags$small(class = "text-muted ms-2",
          sprintf("%d historical + %d new wells", br$n_prior, br$n_new))
      ),
      card_body(
        tags$table(class = "table table-sm table-hover mb-0",
          tags$thead(
            tags$tr(
              tags$th("Parameter"), tags$th("Prior"), tags$th("Updated"),
              tags$th("Direction"), tags$th("Evidence"), tags$th("Decision"), tags$th("Decision Reason")
            )
          ),
          tags$tbody(do.call(tagList, dur_rows))
        )
      )
    )

    # ---- 2b. Risk detail card (table with Evidence + Decision + Reason) ------
    risk_card <- if (has_risk) {
      risk_rows_ui <- lapply(seq_len(nrow(ru)), function(i) {
        r <- ru[i, ]
        chg_cls <- if (r$direction == "Stable") "text-secondary" else if (r$direction == "Increasing") "text-danger" else "text-success"
        tags$tr(
          tags$td(r$risk_event),
          tags$td(sprintf("%.1f%%", 100 * r$prior_prob)),
          tags$td(sprintf("%.1f%%", 100 * r$posterior_mean)),
          tags$td(class = chg_cls, sprintf("%s (%+.1fpp, %.0f%% rel.)", r$direction, 100 * r$delta_prob, 100 * r$rel_change)),
          tags$td(r$evidence_strength),
          tags$td(tags$span(class = paste("badge", .decision_badge_cls(r$recommendation_level)), r$decision)),
          tags$td(class = "small text-muted", r$decision_reason)
        )
      })
      card(
        card_header(tags$span(class = "fw-bold", "Risk Detail")),
        card_body(
          tags$table(class = "table table-sm table-hover mb-0",
            tags$thead(
              tags$tr(
                tags$th("Event"), tags$th("Prior"), tags$th("Updated"),
                tags$th("Change"), tags$th("Evidence"), tags$th("Decision"), tags$th("Decision Reason")
              )
            ),
            tags$tbody(do.call(tagList, risk_rows_ui))
          )
        )
      )
    } else {
      card(
        card_header(tags$span(class = "fw-bold", "Risk Detail")),
        card_body(tags$p(class = "text-muted small mb-0",
          "Upload a risk observations CSV to see Beta-Binomial risk probability updates."))
      )
    }

    # ---- Assemble ------------------------------------------------------------
    tagList(
      learning_summary_card,
      decision_rules_card,
      decision_thresholds_card,
      learning_narrative_card,
      tags$div(class = "row g-3 mt-0",
        tags$div(class = "col-md-6", dur_card),
        tags$div(class = "col-md-6", risk_card)
      )
    )
  })

  output$bayes_duration_plot <- renderPlot({
    br <- tryCatch(bayes_result_r(), error = function(e) NULL)
    dat <- tryCatch(input_data(), error = function(e) NULL)
    hw  <- if (!is.null(dat) && isTRUE(dat$ok)) dat$historical else NULL
    plot_bayesian_duration_update(br, hw)
  }, res = 96)

  output$bayes_duration_table <- DT::renderDT({
    br <- tryCatch(bayes_result_r(), error = function(e) NULL)
    if (is.null(br) || is.null(br$duration_update))
      return(DT::datatable(tibble(), options = list(dom = "t")))
    df <- assess_duration_update(br$duration_update) %>%
      transmute(
        Parameter         = label,
        `Prior wells`     = n_prior,
        `Prior P50 (d)`   = round(prior_p50,      3),
        `Prior P10–P90`   = sprintf("%.3f – %.3f", prior_p10, prior_p90),
        `New wells`       = n_new,
        `New mean (d)`    = round(new_mean,        3),
        `Post. P50 (d)`   = round(posterior_p50,   3),
        `Post. P10–P90`   = sprintf("%.3f – %.3f", posterior_p10, posterior_p90),
        `Shift (d)`       = sprintf("%+.4f", delta_mean),
        `90% CI for shift` = sprintf("[%+.4f, %+.4f]", ci90_lo, ci90_hi),
        Evidence          = evidence_strength,
        Decision          = decision,
        `Decision Reason` = decision_reason
      )
    DT::datatable(df, rownames = FALSE,
                  options = list(dom = "t", scrollX = TRUE)) %>%
      DT::formatStyle("Shift (d)",
        color = DT::styleInterval(c(-1e-9, 1e-9), c("#009E73", "black", "#D55E00"))) %>%
      DT::formatStyle("Decision",
        backgroundColor = DT::styleEqual(
          c("Retain assumption", "Review assumption", "Update assumption"),
          c("#d4edda", "#fff3cd", "#f8d7da")))
  })

  output$bayes_duration_interp <- renderUI({
    br <- tryCatch(bayes_result_r(), error = function(e) NULL)
    if (is.null(br)) return(NULL)
    dur <- assess_duration_update(br$duration_update)
    tags$p(class = "small text-primary mb-0 fw-semibold", paste(dur$narrative_full, collapse = " "))
  })

  # Shared body for the 3 scope-gated risk sections (stage/well/campaign) --
  # factored into one helper instead of tripling the table-formatting code,
  # called once per scope below. `scope_filter` matches the `scope` column
  # added to bayesian_update_risks()'s output (R/bayesian_updater.R).
  render_risk_scope_section <- function(scope_filter) {
    ru_all_r <- reactive({
      br <- tryCatch(bayes_result_r(), error = function(e) NULL)
      if (is.null(br) || is.null(br$risk_update) || nrow(br$risk_update) == 0) return(NULL)
      assess_risk_update(br$risk_update)
    })
    ru_r <- reactive({
      ru <- ru_all_r()
      if (is.null(ru)) return(NULL)
      ru %>% filter(scope == scope_filter)
    })

    list(
      interp = renderUI({
        ru <- ru_r()
        if (is.null(ru) || nrow(ru) == 0) {
          return(tags$p(class = "small text-muted mb-0",
            sprintf("No %s-level risk observations in this dataset.", scope_filter)))
        }
        any_unmatched <- any(!ru$matched)
        tags$p(class = paste("small mb-0 fw-semibold", if (any_unmatched) "text-danger" else "text-primary"),
               paste(ru$narrative_full, collapse = " "))
      }),
      plot = renderPlot({
        plot_bayesian_risk_update(ru_r())
      }, res = 96),
      table = DT::renderDT({
        ru <- ru_r()
        if (is.null(ru) || nrow(ru) == 0)
          return(DT::datatable(
            tibble(Note = sprintf("No %s-level risk observations in this dataset.", scope_filter)),
            rownames = FALSE, options = list(dom = "t")))
        df <- ru %>%
          transmute(
            `Risk event`      = risk_event,
            Matched           = ifelse(matched, "Yes", "✘ No match"),
            `Sample`          = mapply(.scope_unit_label, scope, n_trials),
            `Prior prob.`     = ifelse(matched,
                                       sprintf("%.3f (%.1f%%)", prior_prob, 100 * prior_prob),
                                       sprintf("%.3f (%.1f%%) — fabricated default, not a real assumption", prior_prob, 100 * prior_prob)),
            `Events`          = n_events,
            `Observed freq.`  = sprintf("%.1f%%", 100 * observed_freq),
            `Post. mean`      = sprintf("%.3f (%.1f%%)", posterior_mean, 100 * posterior_mean),
            `Post. 90% CI`    = sprintf("[%.3f, %.3f]", posterior_p05, posterior_p95),
            `Shift`           = sprintf("%+.4f", delta_prob),
            `Relative shift`  = sprintf("%.0f%%", 100 * rel_change),
            Evidence          = evidence_strength,
            Decision          = decision,
            `Decision Reason` = decision_reason
          )
        DT::datatable(df, rownames = FALSE,
                      options = list(dom = "t", scrollX = TRUE)) %>%
          DT::formatStyle("Shift",
            color = DT::styleInterval(c(-1e-9, 1e-9), c("#009E73", "black", "#D55E00"))) %>%
          DT::formatStyle("Matched",
            backgroundColor = DT::styleEqual("✘ No match", "#f8d7da"),
            fontWeight = DT::styleEqual("✘ No match", "bold")) %>%
          DT::formatStyle("Decision",
            backgroundColor = DT::styleEqual(
              c("No action", "Monitor", "Update assumption", "No assumption match"),
              c("#d4edda", "#fff3cd", "#f8d7da", "#f8d7da")))
      })
    )
  }

  for (sc in c("stage", "well", "campaign")) {
    local({
      scope_local <- sc
      section <- render_risk_scope_section(scope_local)
      output[[paste0("bayes_risk_interp_", scope_local)]] <- section$interp
      output[[paste0("bayes_risk_plot_", scope_local)]]   <- section$plot
      output[[paste0("bayes_risk_table_", scope_local)]]  <- section$table
    })
  }

  # Apply: merge new wells into the bootstrap pool used by the simulation.
  observeEvent(input$bayes_apply, {
    br <- tryCatch(bayes_result_r(), error = function(e) NULL)
    req(!is.null(br))
    bayes_merged_wells_rv(br$merged_wells)
    showNotification(
      sprintf("Applied: simulation will now bootstrap from %d combined wells (%d prior + %d new).",
              nrow(br$merged_wells), br$n_prior, br$n_new),
      type = "message", duration = 6)
  })

  output$bayes_apply_status <- renderUI({
    mw <- bayes_merged_wells_rv()
    if (is.null(mw)) return(NULL)
    tags$small(class = "text-success",
      sprintf("Active: simulation is using %d merged wells. Re-run to update results.", nrow(mw)))
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

  output$download_scenarios <- downloadHandler(
    filename = function() paste0("scenario_library_", Sys.Date(), ".json"),
    content = function(file) {
      writeLines(scenario_library_to_json(scenario_library_rv()), file)
    }
  )

  observeEvent(input$upload_scenarios, {
    req(input$upload_scenarios)
    tryCatch({
      json_str <- paste(readLines(input$upload_scenarios$datapath, warn = FALSE), collapse = "\n")
      imported <- scenario_library_from_json(json_str)
      scenario_library_rv(imported)
      showNotification(
        sprintf("Imported %d scenario(s).", length(imported)),
        type = "message", duration = 4
      )
    }, error = function(e) {
      showNotification(
        paste("Import failed:", conditionMessage(e)),
        type = "error", duration = 8
      )
    })
  })

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
    # Use the zipper_efficiency actually used for this run's Zipper pass
    # (not the function's 0.75 default) so the card's label/percentage can
    # never disagree with the sidebar's Zipper execution factor slider.
    zip_eff <- sim_results()$args_by_mode[["Zipper"]]$zipper_efficiency %||% 0.75
    build_zipper_benefit_breakdown(sim_results()$summary, zipper_efficiency = zip_eff)
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
      flowback_testing_days_max = input$flowback_testing_days_max,
      pre_frac_scheduling = input$pre_frac_scheduling
    )
    res <- tryCatch({
      withProgress(message = "Analysing constraint cascade", value = 0, {
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
          seed = as.integer(input$seed),
          progress_callback = function(i, n) {
            frac <- if (n == 0L) 1 else (i + 1L) / (n + 2L)
            detail <- if (i == 0L) "Current config..." else sprintf("Step %d / %d", i, n)
            setProgress(frac, detail = detail)
          }
        )
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

    # Single source of truth for "the campaign bottleneck": the same
    # explain_bottlenecks()-based recommendation the Decision Support tab's
    # Recommendation card and the Overview "Critical bottleneck" narrative
    # use (rec_v2_r() -- see output$bottleneck_narrative below), so the value
    # box headline can never name a different resource than the narrative
    # card right underneath it. Previously this used summarise_bottlenecks(),
    # a separate utilization-threshold ranking that could disagree with the
    # narrative card and the Optimiser's constraint cascade.
    rec <- tryCatch(rec_v2_r(), error = function(e) NULL)
    bn <- if (!is.null(rec)) {
      tibble::tibble(operation_mode = rec$operation_mode, resource = rec$bottleneck)
    } else {
      tibble::tibble()
    }
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

      # Full 4-component breakdown (not just the weakest 2) for the
      # expandable "Readiness score breakdown" panel, so the score is fully
      # traceable on demand, not just summarised to its two weakest drivers.
      comp_weights <- c(
        "Schedule certainty" = rd$schedule_weight,
        "Resource capacity" = rd$resource_weight,
        "Risk exposure" = rd$risk_weight,
        "Wireline readiness" = rd$wireline_weight
      )
      readiness_components <- tibble::tibble(
        driver = names(comp_scores),
        weight = unname(comp_weights[names(comp_scores)]),
        score = unname(comp_scores),
        description = unname(comp_desc[names(comp_scores)])
      ) %>% dplyr::arrange(score)
    } else {
      readiness_components <- tibble::tibble()
    }

    idle_days <- mean(sim_results()$summary$total_wireline_readiness_delay_days, na.rm = TRUE)

    # Attribution split (event mode only -- see schedule_pre_frac() in
    # R/engine_core.R): how much of the idle-days figure above is
    # genuinely wireline capacity vs CT gating wireline's start. Without
    # this, a slow CT unit shows up entirely as "waiting on wireline" even
    # when wireline itself has ample units -- mean(..., na.rm=TRUE) on the
    # formula path's all-NA column returns NaN, which is how has_attribution
    # detects that the split isn't available for this run.
    ct_caused_days <- mean(sim_results()$summary$total_ct_caused_wireline_wait_days, na.rm = TRUE)
    wireline_capacity_days <- mean(sim_results()$summary$total_wireline_capacity_wait_days, na.rm = TRUE)
    has_attribution <- !is.nan(ct_caused_days) && !is.nan(wireline_capacity_days)
    # Second-level split of ct_caused_days: queueing (removable by adding CT
    # units) vs duration_floor (CT's own per-well task simply takes longer
    # than wireline+frac's pace -- NOT removable by adding units, only by a
    # shorter CT task). Found by direct investigation: a synthetic check with
    # CT units raised to match n_wells (zero queueing left to remove) still
    # left a flat, nonzero residual -- telling a user to "add CT capacity"
    # without this split would overstate what more units can actually fix.
    ct_queueing_days <- mean(sim_results()$summary$total_ct_queueing_wireline_wait_days, na.rm = TRUE)
    ct_duration_floor_days <- mean(sim_results()$summary$total_ct_duration_floor_wireline_wait_days, na.rm = TRUE)

    list(
      best = best$operation_mode,
      p50 = fmt_days_short(best$p50_days),
      p90 = paste0("P90: ", fmt_days_short(best$p90_days)),
      saving = if (is.na(saving_days)) "N/A" else fmt_days_short(saving_days),
      saving_pct = if (is.na(saving_pct)) "" else paste0(round(saving_pct, 1), "% vs conventional"),
      readiness = if (nrow(rd) == 0) "N/A" else paste0(round(rd$readiness_score, 0), " / 100"),
      readiness_status = if (nrow(rd) == 0) "" else rd$readiness_status,
      readiness_drivers = drivers,
      readiness_components = readiness_components,
      readiness_mode = if (nrow(rd) == 0) NA_character_ else rd$operation_mode,
      readiness_scoring_note = if (nrow(rd) == 0) "" else rd$scoring_note,
      bottleneck = if (nrow(bn) == 0) "N/A" else paste(bn$operation_mode, bn$resource, sep = " \u2013 "),
      bottleneck_resource = if (nrow(bn) == 0) NA_character_ else bn$resource,
      bottleneck_type = if (nrow(bn) == 0) "post_frac" else {
        if (grepl("Milling|Testing", bn$resource)) "post_frac" else "frac_phase"
      },
      # Frac-phase constraint: wireline slower than frac fleet in zipper
      frac_phase_constrained = idle_days > 2,
      idle_days = idle_days,
      idle_cost = fmt_money_short(idle_days * input$frac_fleet_cost),
      has_attribution = has_attribution,
      ct_caused_days = ct_caused_days,
      wireline_capacity_days = wireline_capacity_days,
      ct_queueing_days = ct_queueing_days,
      ct_duration_floor_days = ct_duration_floor_days
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

  # Expandable, fully-traceable readiness breakdown: all 4 weighted
  # components (not just the 2 weakest shown on the value box), each with
  # its weight, 0-100 sub-score, and plain-English description -- reusing
  # the exact comp_desc text already built in vb_data(), just not truncated.
  output$readiness_breakdown <- renderUI({
    d <- vb_data()
    rc <- d$readiness_components
    if (is.null(rc) || nrow(rc) == 0) return(NULL)
    bslib::accordion(
      open = FALSE,
      bslib::accordion_panel(
        sprintf("Readiness score breakdown — %s (%s, %s)", d$readiness_mode, d$readiness_status, d$readiness),
        tags$table(class = "table table-sm mb-2",
          tags$thead(tags$tr(tags$th("Driver"), tags$th("Weight"), tags$th("Score"), tags$th("What's driving it"))),
          tags$tbody(
            lapply(seq_len(nrow(rc)), function(i) {
              tags$tr(
                tags$td(rc$driver[i]),
                tags$td(sprintf("%.0f%%", 100 * rc$weight[i])),
                tags$td(sprintf("%.0f / 100", rc$score[i])),
                tags$td(rc$description[i])
              )
            })
          )
        ),
        tags$small(class = "text-muted", d$readiness_scoring_note)
      )
    )
  })

  output$vb_bottleneck_sub <- renderUI({
    d <- vb_data()
    if (identical(d$bottleneck, "N/A")) return(NULL)
    # Show scope: is this limiting campaign end date or frac efficiency?
    scope_txt <- if (d$bottleneck_type == "post_frac")
      tags$small(class = "text-muted", "Limits campaign end date")
    else
      tags$small(class = "text-muted", "Limits frac fleet utilization")
    # Also flag a frac-phase idle cost alongside a post-frac bottleneck --
    # name whichever resource the attribution split says actually causes
    # it (CT or wireline), rather than always blaming wireline (see
    # vb_idle_context for why that assumption is wrong when CT gates
    # wireline's start).
    wl_note <- if (d$bottleneck_type == "post_frac" && d$frac_phase_constrained) {
      cause <- if (isTRUE(d$has_attribution) && d$ct_caused_days > d$wireline_capacity_days) "CT" else "wireline"
      tags$small(class = "text-warning d-block mt-1",
        sprintf("Also: %s slower than frac fleet — %.1f d idle cost exists", cause, d$idle_days))
    } else NULL
    tagList(scope_txt, wl_note)
  })

  output$vb_idle_context <- renderUI({
    d <- vb_data()
    if (d$idle_days < 0.5) {
      return(tags$small(class = "text-muted", "No significant frac fleet waiting time"))
    }
    # Clarify that idle cost and campaign bottleneck are different problems.
    # bottleneck_type == "frac_phase" only tells us the bottleneck isn't
    # milling/testing -- it could be wireline, CT, or the frac fleet itself,
    # so check the actual resource rather than assuming wireline (the
    # original wording here always said "Wireline is also the campaign
    # bottleneck" for ANY frac_phase bottleneck, which was wrong whenever
    # CT or the frac fleet was the real constraint).
    scope <- if (d$bottleneck_type == "post_frac") {
      "This is separate from the campaign bottleneck — frac fleet waits on wireline during pumping operations."
    } else if (isTRUE(grepl("Wireline", d$bottleneck_resource))) {
      "Wireline is also the campaign bottleneck."
    } else {
      sprintf("This is separate from the campaign bottleneck — %s is the actual constraint there.",
              d$bottleneck_resource)
    }
    # Attribution: a slow CT unit can push wireline's own finish time later
    # without wireline itself being undersized -- don't let a user read this
    # idle-days figure as "add wireline capacity" when the fix is actually
    # "add CT capacity". Only shown when the split is available (event mode)
    # and the CT-caused share is large enough to matter.
    #
    # Within the CT-caused share, distinguish queueing (genuinely fixed by
    # adding CT units) from a duration floor (CT's own per-well task simply
    # takes longer than wireline+frac's pace -- adding units does NOT fix
    # this, only a shorter CT task does). Found by direct investigation:
    # raising CT units all the way to one-per-well still left a flat,
    # nonzero residual -- telling a user to "add CT capacity" without this
    # split overstates what more units can actually achieve.
    attribution <- if (isTRUE(d$has_attribution) && d$ct_caused_days > 0.5) {
      ct_detail <- if (d$ct_queueing_days > 0.5 && d$ct_duration_floor_days > 0.5) {
        sprintf("of which %.1f d is queueing (add CT capacity) and %.1f d is CT's own task duration (shortening the CT task is the only fix)",
                d$ct_queueing_days, d$ct_duration_floor_days)
      } else if (d$ct_duration_floor_days > 0.5) {
        "all of it CT's own task duration exceeding wireline+frac's pace -- adding CT units will NOT fix this, only a shorter CT task will"
      } else {
        "essentially all of it CT queueing -- add CT capacity"
      }
      tags$small(class = "text-warning d-block mt-1",
        sprintf("Of this, %.1f d is CT gating wireline's start (%s) — %.1f d is wireline's own capacity.",
                d$ct_caused_days, ct_detail, d$wireline_capacity_days))
    } else NULL
    tagList(
      tags$small(sprintf("%.1f mean idle days waiting on wireline. ", d$idle_days)),
      tags$small(class = "text-muted d-block", scope),
      attribution
    )
  })

  # Built from rec_v2_r() -- the same Why/Evidence/Confidence object the
  # Decision Support tab's Recommendation card renders -- instead of the
  # separate build_bottleneck_narrative()/build_resource_recommendations()
  # path, whose "days recoverable" was an undisclosed workload/units screening
  # estimate (see build_resource_recommendations() in R/summaries.R) that could name a
  # different resource, with a different saving, than this same simulation's
  # constraint cascade or Decision Support recommendation.
  output$bottleneck_card_header <- renderUI({
    rec <- tryCatch(rec_v2_r(), error = function(e) NULL)
    label <- if (is.null(rec)) "Bottleneck" else switch(rec$status,
      Critical = "Critical bottleneck",
      Moderate = "Moderate bottleneck",
      Minor    = "Minor bottleneck",
      "No significant bottleneck"
    )
    span(label)
  })
  output$bottleneck_narrative <- renderUI({
    rec <- tryCatch(rec_v2_r(), error = function(e) NULL)
    if (is.null(rec)) return(p("No bottleneck identified."))
    status_cls <- switch(rec$status,
                         Critical = "text-danger", Moderate = "text-warning",
                         Minor = "text-warning", "text-success")
    verified <- grepl("VERIFIED", rec$basis)
    tagList(
      h3(class = "mb-1", paste(rec$operation_mode, "\u2013", rec$bottleneck)),
      p(class = paste("fw-bold", status_cls),
        sprintf("%s | P90 utilization %.0f%%", rec$status, 100 * rec$p90_utilization)),
      tags$div(class = "mt-2",
        tags$strong("Why: "),
        sprintf("%s is the current campaign bottleneck (%s) -- ranked by measured queue-delay contribution to P50 duration, not raw utilization alone.",
                rec$bottleneck, rec$status)),
      tags$div(class = "mt-2",
        tags$strong("Evidence: "),
        # rec$bottleneck is already a complete noun phrase (e.g. "Testing
        # unit", "Wireline", "CT / cleanout") -- don't append a second
        # "unit" or it reads "testing unit unit".
        sprintf("Adding one more %s reduces campaign P50 by %.0f days.", rec$bottleneck, rec$expected_reduction_days),
        tags$span(class = if (verified) "badge bg-success ms-1" else "badge bg-secondary ms-1",
                  if (verified) "VERIFIED by re-simulation" else "ESTIMATED")),
      tags$div(class = "mt-2",
        tags$strong("Expected impact: "),
        sprintf("Estimated duration reduction = %.0f days. ", rec$expected_reduction_days),
        tags$span(class = "text-muted", sprintf("[%s]", rec$decision_status))),
      p(class = "mt-2", tags$strong("Recommended action: "), rec$recommendation),
      tags$small(class = "text-muted d-block", rec$decision_reason),
      if (!verified) {
        tags$small(class = "text-muted d-block mt-1",
          "Click \u201cVerify by re-simulation\u201d to confirm this estimate by re-running the simulation with one extra unit and comparing paired outcomes.")
      },
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
                rec$bottleneck
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
    # Already sorted by desc(net_benefit) in build_investment_ranking() --
    # row 1 is the highest-value recommendation, highlighted below.
    df <- inv %>%
      transmute(
        Mode = operation_mode,
        Change = proposed_change,
        `P50 saving` = paste0(round(p50_saving_days, 1), " d"),
        `Incremental cost` = incremental_unit_cost,
        `Schedule value` = schedule_value,
        `Net benefit` = net_benefit,
        ROI = paste0(round(benefit_cost_ratio, 1), "x"),
        Tier = roi_tier
      )
    datatable(df, options = list(dom = "t", scrollX = TRUE), rownames = FALSE) %>%
      formatCurrency(c("Incremental cost", "Schedule value", "Net benefit"), digits = 0) %>%
      formatStyle("Tier",
                  color = styleEqual(c("Excellent", "Good", "Marginal"), c("#1b9e77", "#e6ab02", "#d62728")),
                  fontWeight = "bold") %>%
      formatStyle(names(df), target = "row",
                  backgroundColor = styleRow(1, "#FFF3CD"))
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
  output$risk_heatmap_plot <- renderPlot({ plot_schedule_risk_heatmap(risk_heatmap_r()) }, res = 96)
  output$well_risk_ranking_plot <- renderPlot({ plot_well_risk_ranking(risk_heatmap_r()) }, res = 96)
  output$well_risk_table <- renderDT({
    ws <- risk_heatmap_r()$well_scores
    req(ws, nrow(ws) > 0)
    ws %>%
      transmute(
        Mode                 = operation_mode,
        Well                 = well_id,
        Pad                  = pad_id,
        `Exp. delay (d)`     = round(total_expected_delay, 2),
        `Risk level`         = as.character(risk_level),
        `Top risk`           = top_risk,
        `Top risk delay (d)` = round(top_risk_delay, 2),
        `# risk types`       = n_risk_types,
        Rank                 = risk_rank
      ) %>%
      arrange(Mode, Rank) %>%
      DT::datatable(rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE)) %>%
      DT::formatStyle(
        "Risk level",
        backgroundColor = DT::styleEqual(
          c("Low", "Medium", "High", "Critical"),
          c("#d5f5e3", "#fef9e7", "#fdebd0", "#fadbd8")
        )
      )
  })

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
    tr <- traffic_r()
    if (is.null(tr) || nrow(tr) == 0) return(datatable(tibble(), options = list(dom = "t")))
    # One row per (mode, light) instead of one row per mode with 4 wide
    # status+reason column pairs -- a "Reason" sentence reads far better
    # in a narrow table than crammed next to 7 other columns.
    cats <- list(
      list(label = "Schedule Risk",       status = "schedule_risk",       reason = "schedule_risk_reason"),
      list(label = "Resource Risk",       status = "resource_risk",       reason = "resource_risk_reason"),
      list(label = "Operational Risk",    status = "operational_risk",    reason = "operational_risk_reason"),
      list(label = "Wireline Constraint", status = "wireline_constraint", reason = "wireline_constraint_reason")
    )
    long <- dplyr::bind_rows(lapply(cats, function(cg) {
      tibble::tibble(
        `Operation mode` = tr$operation_mode,
        `Traffic light`  = cg$label,
        Status           = tr[[cg$status]],
        Reason           = tr[[cg$reason]]
      )
    })) %>% dplyr::arrange(`Operation mode`, `Traffic light`)
    datatable(long, options = list(dom = "t", scrollX = TRUE, pageLength = 20), rownames = FALSE) %>%
      formatStyle("Status",
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
  # Phase 1 (auditability pass): snapshot of every value the reproducibility
  # manifest describes, captured in the SAME reactive tick as the run itself
  # -- not re-read from live reactives at export time, which could pick up
  # sidebar edits made after the run finished (the "stale reactive" /
  # "capturing defaults instead of active edited values" risks flagged in
  # Phase 0). Downloading the manifest later reads only this snapshot.
  optim_run_context <- reactiveVal(NULL)

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
      flowback_testing_days_max = input$flowback_testing_days_max,
      pre_frac_scheduling = input$pre_frac_scheduling
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
    if (!is.null(res)) {
      optim_run_context(list(
        seed = as.integer(input$seed),
        operation_modes = unique(grid$operation_mode),
        scheduling_mode = input$pre_frac_scheduling,
        n_wells = as.integer(input$n_wells),
        screen_iterations = as.integer(input$opt_screen_iter),
        refine_iterations = 600L,
        top_n_refine = 5L,
        scenario_grid = grid,
        resource_search_ranges = list(
          frac_fleets = input$opt_frac_range, wireline_units = input$opt_wl_range,
          ct_units = input$opt_ct_range, milling_units = input$opt_mill_range,
          testing_units = input$opt_test_range
        ),
        day_rates = list(
          frac_fleet = input$frac_fleet_cost, wireline = input$wireline_cost,
          ct = input$ct_cost, milling = input$milling_cost, testing_unit = input$testing_unit_cost
        ),
        active_parameters_df = current_locked_rows(),
        active_risks_df = current_risk_rows(),
        risk_consequence_df = dat$risk_library,
        historical_wells = dat$historical,
        using_synthetic = dat$using_synthetic,
        historical_filename = if (is.null(input$historical_file)) NA_character_ else input$historical_file$name,
        excluded_well_ids = dat$excluded_well_ids,
        # The optimiser currently runs on dat$historical directly (unlike the
        # main "Run simulation" handler, which prefers
        # bayes_merged_wells_rv() %||% dat$historical) -- a Bayesian update is
        # never applied to optimiser runs today. Recorded honestly as FALSE
        # rather than fixed here: changing which historical data feeds the
        # optimiser is an optimiser-behavior change, out of scope for this
        # auditability-only pass. See the accompanying investigation report.
        bayesian_applied = FALSE
      ))
    }
  })

  # Phase 4 (auditability pass): tie-group badge + expandable alternatives
  # for one scenario. `res_ties` must already have tie_group_id/tie_group_size/
  # is_tie_representative from group_optimiser_ties(). Returns NULL if the
  # scenario isn't part of a multi-row tie -- callers render nothing extra.
  .tie_group_detail_ui <- function(res_ties, target_row) {
    grp <- res_ties %>% filter(tie_group_id == target_row$tie_group_id) %>%
      arrange(total_mobilisation_cost)
    if (nrow(grp) <= 1) return(NULL)
    tagList(
      tags$span(class = "badge bg-secondary ms-2", sprintf("%d tied configurations", nrow(grp))),
      tags$details(class = "mt-1",
        tags$summary(class = "small text-muted", style = "cursor: pointer;",
                     "Show tied alternatives (same P50, different cost)"),
        tags$ul(class = "small mb-0 mt-1",
          lapply(seq_len(nrow(grp)), function(i) {
            r <- grp[i, ]
            e <- explain_optimiser_scenario(res_ties, which(res_ties$scenario_id == r$scenario_id &
                                                             res_ties$stage == r$stage)[1])
            tags$li(
              sprintf("%s, %s, %s", r$config_label, fmt_money_short(r$total_mobilisation_cost),
                      if (isTRUE(r$pareto)) "Pareto" else "dominated"),
              if (!isTRUE(r$is_tie_representative))
                tags$div(class = "text-muted", style = "padding-left: 1em;", paste("Reason:", e$short))
            )
          })
        )
      )
    )
  }

  output$opt_recommendation <- renderUI({
    res <- optim_results()
    if (is.null(res)) return(p(class = "text-muted", "Run the optimiser to get a recommendation."))
    rec <- res %>% filter(recommended) %>% slice(1)
    if (nrow(rec) == 0) return(p("No recommendation available."))
    fast <- res %>% filter(fastest) %>% slice(1)
    res_ties <- group_optimiser_ties(res)
    rec_ties <- res_ties %>% filter(recommended) %>% slice(1)
    fast_ties <- res_ties %>% filter(fastest) %>% slice(1)
    tagList(
      h4(class = "mb-1", rec$config_label, .tie_group_detail_ui(res_ties, rec_ties)),
      p(tags$strong(sprintf("P50: %.0f days | Total cost: %s | Idle: %s",
                            rec$p50_days, fmt_money_short(rec$total_mobilisation_cost),
                            fmt_money_short(rec$idle_cost))),
        tags$sup(class = "text-muted", title = "Frac-fleet idle days only -- other resources' idle time is not separately priced here.", " *")),
      p(class = "text-muted",
        sprintf("Lowest total mobilisation cost on the efficient frontier (refined at 600 iterations, seed %s). ",
                input$seed),
        sprintf("Fastest option: %s at %.0f days for %s.",
                fast$config_label, fast$p50_days, fmt_money_short(fast$total_mobilisation_cost))),
      div(class = "mb-2",
        tags$strong("Fastest option"), .tie_group_detail_ui(res_ties, fast_ties)),
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
    # annotate_optimiser_explanations() needs the FULL results frame (it
    # looks up tie groups and single-resource neighbors across all rows),
    # so it runs before transmute() narrows to display columns.
    annotate_optimiser_explanations(res) %>%
      transmute(
        Scenario = config_label,
        Stage = stage,
        `P50 days` = round(p50_days, 1),
        `P90 days` = round(p90_days, 1),
        `Idle days` = round(idle_days, 1),
        `Idle cost` = idle_cost,
        `Spread $/day` = spread_rate_per_day,
        `Total cost` = total_mobilisation_cost,
        `Binding path` = ifelse(
          is.na(binding_path_primary), "",
          sprintf("%s, %.0f%%", binding_path_primary,
                  100 * pmax(frac_path_bind_pct, post_frac_bind_pct, na.rm = TRUE))
        ),
        Why = why,
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
      write_csv(optimiser_export_headers(optim_results()), file)
    }
  )

  # Phase 1 (auditability pass): the manifest itself, built once per run and
  # cached by Shiny's reactive semantics -- so the "Run reproducibility"
  # panel (Phase 5) and the download handler below always describe the
  # exact same manifest (same run_id/timestamp/hashes), never two separately
  # regenerated ones with different timestamps.
  optim_manifest_r <- reactive({
    req(optim_results(), optim_run_context())
    ctx <- optim_run_context()
    build_optimiser_run_manifest(
      project_root = project_root,
      seed = ctx$seed, operation_modes = ctx$operation_modes,
      scheduling_mode = ctx$scheduling_mode, n_wells = ctx$n_wells,
      screen_iterations = ctx$screen_iterations, refine_iterations = ctx$refine_iterations,
      top_n_refine = ctx$top_n_refine,
      results = optim_results(), scenario_grid = ctx$scenario_grid,
      resource_search_ranges = ctx$resource_search_ranges, day_rates = ctx$day_rates,
      active_parameters_df = ctx$active_parameters_df, active_risks_df = ctx$active_risks_df,
      risk_consequence_df = ctx$risk_consequence_df,
      historical_wells = ctx$historical_wells, using_synthetic = ctx$using_synthetic,
      historical_filename = ctx$historical_filename,
      excluded_well_ids = ctx$excluded_well_ids, bayesian_applied = ctx$bayesian_applied
    )
  })

  # Phase 5: expandable "Run reproducibility" summary near the recommendation.
  output$opt_reproducibility_panel <- renderUI({
    m <- tryCatch(optim_manifest_r(), error = function(e) NULL)
    if (is.null(m)) return(NULL)
    complete <- m$run_identity$app_version != "unknown" && m$run_identity$git_commit != "unknown"
    tags$details(class = "mt-3",
      tags$summary(class = "fw-bold", style = "cursor: pointer;", "Run reproducibility"),
      tags$div(class = "small mt-2",
        tags$table(class = "table table-sm mb-2",
          tags$tbody(
            tags$tr(tags$td("Run ID"), tags$td(tags$code(m$run_identity$run_id))),
            tags$tr(tags$td("App version"), tags$td(m$run_identity$app_version)),
            tags$tr(tags$td("Git commit"), tags$td(tags$code(m$run_identity$git_commit))),
            tags$tr(tags$td("Seed"), tags$td(m$simulation_controls$seed)),
            tags$tr(tags$td("Screen / refine iterations"),
                    tags$td(sprintf("%s / %s", m$simulation_controls$screen_iterations,
                                    m$simulation_controls$refine_iterations))),
            tags$tr(tags$td("Historical data"),
                    tags$td(sprintf("%s (%d wells)", m$historical_data$source, m$historical_data$row_count))),
            tags$tr(tags$td("Input manifest hash"),
                    tags$td(tags$code(paste0(substr(m$hashes$manifest_hash, 1, 16), "...")))),
            tags$tr(tags$td("Export manifest"), tags$td("Available -- see \"Download run manifest\" below"))
          )
        ),
        if (complete) {
          tags$p(class = "text-success mb-0",
            "This run includes a complete input manifest. The same application version, seed, ",
            "input tables, and optimiser settings can be used to reproduce it.")
        } else {
          tags$p(class = "text-warning mb-0",
            "This run's manifest is incomplete: ",
            if (m$run_identity$git_commit == "unknown") "the git commit could not be determined (no git repository found at the app's working directory). " else "",
            if (m$run_identity$app_version == "unknown") "the app version could not be determined (DESCRIPTION not found). " else "",
            "Exact code-version reproducibility cannot be guaranteed, though the input tables and settings above are still fully captured.")
        }
      )
    )
  })

  # Phase 1 (auditability pass): reproducibility manifest bundle. A SEPARATE
  # download from download_optimiser above -- the original single-CSV export
  # is left byte-for-byte unchanged (no schema change to it at all) so
  # nothing that already parses that file breaks.
  output$download_optimiser_manifest <- downloadHandler(
    filename = function() paste0("optimiser_run_manifest_", Sys.Date(), ".zip"),
    content = function(file) {
      req(optim_results(), optim_run_context())
      ctx <- optim_run_context()
      manifest <- optim_manifest_r()

      tmpdir <- tempfile("optimiser_manifest_")
      dir.create(tmpdir)
      on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

      # JSON manifest -- fails gracefully to CSV-only if jsonlite is somehow
      # unavailable (it's a required Import -- see DESCRIPTION -- so this is
      # a defensive fallback, not an expected path).
      json_ok <- tryCatch({
        writeLines(as.character(jsonlite::toJSON(manifest, auto_unbox = TRUE, pretty = TRUE,
                                                  digits = NA, null = "null")),
                   file.path(tmpdir, "optimiser_run_manifest.json"))
        TRUE
      }, error = function(e) FALSE)
      if (!json_ok) {
        showNotification("JSON manifest unavailable -- exporting CSV files only.", type = "warning", duration = 8)
      }

      write_csv(manifest_to_flat_df(manifest), file.path(tmpdir, "optimiser_run_inputs.csv"))
      write_csv(
        (ctx$active_parameters_df %||% tibble()) %>% sanitize_csv_text_cols(c("variable", "simulation_impact")),
        file.path(tmpdir, "optimiser_active_parameters.csv"))
      write_csv(
        (ctx$active_risks_df %||% tibble()) %>% sanitize_csv_text_cols(c("variable", "simulation_impact")),
        file.path(tmpdir, "optimiser_active_risks.csv"))
      write_csv(
        (ctx$risk_consequence_df %||% tibble()) %>%
          sanitize_csv_text_cols(c("risk_name", "affected_resource", "risk_notes", "scenario_notes")),
        file.path(tmpdir, "optimiser_risk_consequences.csv"))
      if (length(ctx$excluded_well_ids) > 0) {
        write_csv(tibble(excluded_well_id = ctx$excluded_well_ids),
                  file.path(tmpdir, "optimiser_historical_exclusions.csv"))
      }
      write_csv(optimiser_export_headers(optim_results()), file.path(tmpdir, "optimiser_results.csv"))

      files_to_zip <- list.files(tmpdir, full.names = TRUE)
      if (requireNamespace("zip", quietly = TRUE)) {
        zip::zip(zipfile = file, files = basename(files_to_zip), root = tmpdir)
      } else {
        oldwd <- getwd()
        on.exit(setwd(oldwd), add = TRUE)
        setwd(tmpdir)
        utils::zip(zipfile = file, files = basename(files_to_zip))
      }
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
        scenario_records = scenario_library_rv(),
        bayesian_decision = bayes_decision_r()
      )
    }
  )

  output$download_all <- downloadHandler(
    filename = function() paste0("frac_campaign_simulation_audit_", Sys.Date(), ".zip"),
    content = function(file) {
      req(sim_results())
      tmpdir <- tempfile("audit_package_")
      dir.create(tmpdir)
      on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

      write_csv(sim_results()$summary, file.path(tmpdir, "simulation_summary.csv"))
      if (nrow(sim_results()$well_details) > 0) {
        write_csv(sim_results()$well_details, file.path(tmpdir, "simulation_well_details.csv"))
      }
      write_csv(sim_results()$risk_event_log, file.path(tmpdir, "simulation_risk_event_log.csv"))
      write_csv(sim_results()$resource_utilization, file.path(tmpdir, "resource_utilization.csv"))
      write_csv(sim_results()$assumptions_used %>%
                  sanitize_csv_text_cols(c("variable", "simulation_impact")),
                file.path(tmpdir, "assumptions_used.csv"))
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
      write_csv(recommendation_verdict_to_df(rec_v2_r()), file.path(tmpdir, "recommendation_verdict.csv"))
      write_csv(cost_impact_r(), file.path(tmpdir, "cost_impact.csv"))
      write_csv(investment_r(), file.path(tmpdir, "investment_ranking.csv"))
      write_csv(timeline_r(), file.path(tmpdir, "resource_timeline.csv"))
      write_csv(consequences_r(), file.path(tmpdir, "risk_consequences.csv"))
      bd <- bayes_decision_r()
      if (!is.null(bd) && !is.null(bd$duration)) {
        write_csv(bd$duration, file.path(tmpdir, "bayesian_duration_decision_audit.csv"))
      }
      if (!is.null(bd) && !is.null(bd$risk) && nrow(bd$risk) > 0) {
        write_csv(bd$risk, file.path(tmpdir, "bayesian_risk_decision_audit.csv"))
      }
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
                                  scenario_records = scenario_library_rv(),
                                  bayesian_decision = bd)

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
