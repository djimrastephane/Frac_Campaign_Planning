# app.R
# User-facing Shiny app. Users upload CSV files and run the simulator.
# Version 5: includes audit trail, well-level details, risk event log, resource utilization, and assumptions used.

library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(DT)
library(janitor)

`%||%` <- function(x, y) if (is.null(x)) y else x

# Robust source paths. Works whether app is launched from project root or app folder.
if (basename(getwd()) == "app") {
  project_root <- normalizePath(file.path(getwd(), ".."), mustWork = FALSE)
} else {
  project_root <- normalizePath(getwd(), mustWork = FALSE)
}

source(file.path(project_root, "R", "load_inputs.R"))
source(file.path(project_root, "R", "validate_inputs.R"))
source(file.path(project_root, "R", "simulation_engine.R"))
source(file.path(project_root, "R", "plots.R"))

safe_round_df <- function(df, digits = 2) {
  df %>% mutate(across(where(is.numeric), ~ round(.x, digits)))
}

ui <- fluidPage(
  titlePanel("Frac Campaign Planning Simulator"),
  sidebarLayout(
    sidebarPanel(
      h4("Input files"),
      fileInput("historical_file", "Upload historical_wells.csv", accept = ".csv"),
      fileInput("assumption_file", "Upload master_risks_assumptions.csv", accept = ".csv"),
      tags$hr(),
      h4("Scenario settings"),
      selectInput("n_wells", "Campaign size", choices = c(20, 30, 40), selected = 30),
      selectInput(
        "operation_mode",
        "Operation mode",
        choices = c("Conventional", "Zipper", "Compare both"),
        selected = "Compare both"
      ),
      numericInput("n_iter", "Simulation runs", value = 1000, min = 100, max = 10000, step = 100),
      numericInput("frac_fleets", "Frac fleets", value = 1, min = 1, max = 5, step = 1),
      numericInput("wireline_units", "Wireline units", value = 1, min = 1, max = 5, step = 1),
      numericInput("ct_units", "CT / cleanout units", value = 1, min = 1, max = 5, step = 1),
      numericInput("milling_units", "Milling units", value = 1, min = 1, max = 5, step = 1),
      numericInput("frac_trees", "Frac trees available", value = 2, min = 1, max = 10, step = 1),
      sliderInput(
        "zipper_efficiency",
        "Zipper execution factor",
        min = 0.5, max = 1.0, value = 0.75, step = 0.05
      ),
      helpText("Zipper factor 0.75 means the frac execution part is 25% faster than conventional."),
      sliderInput("risk_multiplier", "Risk multiplier", min = 0.25, max = 3, value = 1, step = 0.25),
      actionButton("run", "Run simulation", class = "btn-primary"),
      tags$hr(),
      downloadButton("download_all", "Download audit package")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Summary", br(), uiOutput("status_message"), DTOutput("summary_table")),
        tabPanel("Distribution", br(), plotOutput("duration_plot", height = "500px")),
        tabPanel("Results", br(), DTOutput("results_table")),
        tabPanel("Well Details", br(), uiOutput("simulation_selector_ui"), DTOutput("well_details_table")),
        tabPanel("Risk Event Log", br(), uiOutput("risk_selector_ui"), DTOutput("risk_event_table")),
        tabPanel("Delay Contributors", br(), plotOutput("delay_plot", height = "520px"), br(), DTOutput("delay_table")),
        tabPanel("Resource Utilization", br(), plotOutput("resource_plot", height = "450px"), br(), DTOutput("resource_table")),
        tabPanel("Assumptions Used", br(), DTOutput("assumptions_table")),
        tabPanel("Input Check", br(), verbatimTextOutput("input_check"))
      )
    )
  )
)

server <- function(input, output, session) {

  input_data <- reactive({
    req(input$historical_file, input$assumption_file)

    tryCatch({
      historical <- load_historical_wells(input$historical_file$datapath) %>%
        validate_historical_wells()

      assumptions <- load_master_assumptions(input$assumption_file$datapath) %>%
        validate_assumptions()

      list(ok = TRUE, historical = historical, assumptions = assumptions, error = NULL)
    }, error = function(e) {
      list(ok = FALSE, historical = NULL, assumptions = NULL, error = conditionMessage(e))
    })
  })

  output$status_message <- renderUI({
    dat <- input_data()
    if (!isTRUE(dat$ok)) {
      return(tags$div(style = "color:red; font-weight:bold;", paste("Input error:", dat$error)))
    }
    tags$div(style = "color:green; font-weight:bold;", "Inputs loaded. Click Run simulation.")
  })

  sim_results <- eventReactive(input$run, {
    dat <- input_data()
    validate(need(isTRUE(dat$ok), paste("Fix input files first:", dat$error)))

    tryCatch({
      modes <- if (input$operation_mode == "Compare both") {
        c("Conventional", "Zipper")
      } else {
        input$operation_mode
      }

      detailed_runs <- lapply(seq_along(modes), function(mode_index) {
        simulate_campaign_detailed(
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
          seed = 123 + mode_index
        )
      })

      list(
        summary = bind_rows(lapply(detailed_runs, `[[`, "summary")),
        well_details = bind_rows(lapply(detailed_runs, `[[`, "well_details")),
        risk_event_log = bind_rows(lapply(detailed_runs, `[[`, "risk_event_log")),
        resource_utilization = bind_rows(lapply(detailed_runs, `[[`, "resource_utilization")),
        assumptions_used = bind_rows(lapply(detailed_runs, `[[`, "assumptions_used")) %>% distinct()
      )
    }, error = function(e) {
      showNotification(paste("Simulation error:", conditionMessage(e)), type = "error", duration = NULL)
      NULL
    })
  })

  selected_simulation <- reactive({
    req(sim_results())
    sim_results()$summary %>% arrange(desc(estimated_campaign_days)) %>% slice(1) %>% pull(simulation_id)
  })

  output$simulation_selector_ui <- renderUI({
    req(sim_results())
    ids <- sort(unique(sim_results()$summary$simulation_id))
    selectInput("selected_simulation", "Select simulation ID", choices = ids, selected = selected_simulation())
  })

  output$risk_selector_ui <- renderUI({
    req(sim_results())
    ids <- sort(unique(sim_results()$summary$simulation_id))
    selectInput("selected_risk_simulation", "Select simulation ID", choices = ids, selected = selected_simulation())
  })

  output$summary_table <- renderDT({
    req(sim_results())
    datatable(safe_round_df(summarise_simulation(sim_results()$summary), 2), options = list(dom = "t", scrollX = TRUE))
  })

  output$duration_plot <- renderPlot({
    req(sim_results())
    plot_campaign_distribution(sim_results()$summary)
  })

  output$results_table <- renderDT({
    req(sim_results())
    datatable(safe_round_df(sim_results()$summary, 2), options = list(pageLength = 10, scrollX = TRUE))
  })

  output$well_details_table <- renderDT({
    req(sim_results(), input$selected_simulation)
    tbl <- sim_results()$well_details %>%
      filter(simulation_id == as.integer(input$selected_simulation))
    datatable(safe_round_df(tbl, 2), options = list(pageLength = 15, scrollX = TRUE))
  })

  output$risk_event_table <- renderDT({
    req(sim_results(), input$selected_risk_simulation)
    tbl <- sim_results()$risk_event_log %>%
      filter(simulation_id == as.integer(input$selected_risk_simulation))
    datatable(safe_round_df(tbl, 2), options = list(pageLength = 15, scrollX = TRUE))
  })

  output$delay_plot <- renderPlot({
    req(sim_results())
    plot_delay_contributors(summarise_delay_contributors(sim_results()$risk_event_log))
  })

  output$delay_table <- renderDT({
    req(sim_results())
    datatable(
      safe_round_df(summarise_delay_contributors(sim_results()$risk_event_log), 2),
      options = list(pageLength = 15, scrollX = TRUE)
    )
  })

  output$resource_plot <- renderPlot({
    req(sim_results())
    plot_resource_utilization(summarise_resource_utilization(sim_results()$resource_utilization))
  })

  output$resource_table <- renderDT({
    req(sim_results())
    datatable(
      safe_round_df(summarise_resource_utilization(sim_results()$resource_utilization), 2),
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })

  output$assumptions_table <- renderDT({
    req(sim_results())
    datatable(safe_round_df(sim_results()$assumptions_used, 4), options = list(pageLength = 20, scrollX = TRUE))
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
    cat("Historical columns:\n")
    print(names(dat$historical))
    cat("\nAssumption columns:\n")
    print(names(dat$assumptions))
  })

  output$download_all <- downloadHandler(
    filename = function() paste0("frac_campaign_simulation_audit_", Sys.Date(), ".zip"),
    content = function(file) {
      req(sim_results())
      tmpdir <- tempfile("audit_package_")
      dir.create(tmpdir)

      write_csv(sim_results()$summary, file.path(tmpdir, "simulation_summary.csv"))
      write_csv(sim_results()$well_details, file.path(tmpdir, "simulation_well_details.csv"))
      write_csv(sim_results()$risk_event_log, file.path(tmpdir, "simulation_risk_event_log.csv"))
      write_csv(sim_results()$resource_utilization, file.path(tmpdir, "resource_utilization.csv"))
      write_csv(sim_results()$assumptions_used, file.path(tmpdir, "assumptions_used.csv"))
      write_csv(summarise_delay_contributors(sim_results()$risk_event_log), file.path(tmpdir, "delay_contributors.csv"))
      write_csv(summarise_resource_utilization(sim_results()$resource_utilization), file.path(tmpdir, "resource_utilization_summary.csv"))

      oldwd <- getwd()
      on.exit(setwd(oldwd), add = TRUE)
      setwd(tmpdir)
      utils::zip(zipfile = file, files = list.files(tmpdir), flags = "-r9Xq")
    }
  )
}

shinyApp(ui, server)
