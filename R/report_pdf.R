# report_pdf.R
# Split out of simulation_engine_fast.R (see docs/architecture_cleanup_plan.md).
# The gridExtra/grDevices-heavy PDF report renderer: build_management_report()
# and build_management_report_pdf(). Isolated from engine_core.R/summaries.R so
# they have no plotting-device side effects. Depends on summaries.R's output
# shape (executive KPIs, readiness, etc.) -- source engine_core.R and
# summaries.R first.

build_management_report <- function(summary, risk_event_log, resource_utilization,
                                    frac_fleet_cost_per_day = 250000,
                                    wireline_cost_per_day = 15000,
                                    ct_cost_per_day = 25000,
                                    milling_cost_per_day = 18000) {
  kpis <- build_executive_kpis(summary, risk_event_log, resource_utilization, frac_fleet_cost_per_day)
  sim_stats <- summarise_simulation(summary)
  resource_summary <- summarise_resource_utilization(resource_utilization)
  bottlenecks <- summarise_bottlenecks(resource_summary)
  recommendations <- build_resource_recommendations(summary, resource_utilization)
  readiness <- build_readiness_score(summary, risk_event_log, resource_utilization)
  cost_impact <- build_cost_impact(summary, resource_utilization, frac_fleet_cost_per_day, wireline_cost_per_day, ct_cost_per_day, milling_cost_per_day, testing_unit_cost_per_day)
  top_delays <- summarise_delay_contributors(risk_event_log) %>% slice_head(n = 10)
  traffic <- build_traffic_lights(summary, risk_event_log, resource_utilization)

  html_escape <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x
  }

  fmt_days <- function(x, digits = 1) {
    ifelse(is.na(x), "N/A", paste0(format(round(as.numeric(x), digits), big.mark = ",", trim = TRUE), " days"))
  }
  fmt_num <- function(x, digits = 1) {
    ifelse(is.na(x), "N/A", format(round(as.numeric(x), digits), big.mark = ",", trim = TRUE))
  }
  fmt_pct <- function(x, digits = 1) {
    ifelse(is.na(x), "N/A", paste0(round(100 * as.numeric(x), digits), "%"))
  }
  fmt_money <- function(x) {
    ifelse(is.na(x), "N/A", paste0("$", format(round(as.numeric(x), 0), big.mark = ",", trim = TRUE)))
  }

  html_table <- function(df, title = NULL, subtitle = NULL) {
    if (is.null(df) || nrow(df) == 0) {
      return(paste0(if (!is.null(title)) paste0("<h2>", html_escape(title), "</h2>") else "", "<p>No data available.</p>"))
    }
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    df[] <- lapply(df, function(col) {
      if (is.numeric(col)) {
        return(format(round(col, 2), big.mark = ",", trim = TRUE))
      }
      as.character(col)
    })
    header <- paste0("<tr>", paste0("<th>", html_escape(names(df)), "</th>", collapse = ""), "</tr>")
    rows <- apply(df, 1, function(row) {
      paste0("<tr>", paste0("<td>", html_escape(row), "</td>", collapse = ""), "</tr>")
    })
    paste0(
      if (!is.null(title)) paste0("<h2>", html_escape(title), "</h2>") else "",
      if (!is.null(subtitle)) paste0("<p class='section-note'>", html_escape(subtitle), "</p>") else "",
      "<table>", header, paste(rows, collapse = ""), "</table>"
    )
  }

  get_kpi <- function(name) {
    val <- kpis$value[kpis$kpi == name]
    if (length(val) == 0) "N/A" else val[1]
  }

  best_mode <- get_kpi("Best option")
  p50 <- get_kpi("P50 duration")
  p90 <- get_kpi("P90 duration")
  saving <- get_kpi("Zipper P50 saving")
  bottleneck <- get_kpi("Primary bottleneck")
  top_risk <- get_kpi("Top risk")
  wireline_wait <- get_kpi("Wireline waiting")
  idle_cost <- get_kpi("Idle frac fleet cost")
  readiness_kpi <- get_kpi("Readiness score")

  sim_report <- sim_stats %>%
    transmute(
      `Operation mode` = operation_mode,
      `Simulations` = simulations,
      `P10` = fmt_days(p10_days),
      `P50` = fmt_days(p50_days),
      `P90` = fmt_days(p90_days),
      `Mean duration` = fmt_days(mean_days),
      `Mean stages` = fmt_num(mean_stages),
      `Mean risk delay` = fmt_days(mean_risk_delay_days)
    )

  readiness_report <- readiness %>%
    transmute(
      `Operation mode` = operation_mode,
      `Overall score` = paste0(round(readiness_score, 1), " / 100"),
      `Status` = readiness_status,
      `Schedule` = round(schedule_score, 1),
      `Resource` = round(resource_score, 1),
      `Risk` = round(risk_score, 1),
      `Wireline` = round(wireline_score, 1),
      `Uncertainty` = fmt_pct(uncertainty_ratio),
      `Max P90 utilization (incl. frac)` = fmt_pct(max_p90_utilization),
      `Non-frac bottleneck` = non_frac_bottleneck,
      `Non-frac P90 utilization` = fmt_pct(non_frac_p90_utilization),
      `Risk delay ratio` = fmt_pct(risk_delay_ratio),
      `Wireline wait ratio` = fmt_pct(wireline_wait_ratio)
    )

  bottleneck_report <- bottlenecks %>%
    transmute(
      `Operation mode` = operation_mode,
      `Resource` = resource,
      `P90 utilization` = fmt_pct(p90_utilization),
      `Status` = bottleneck_status,
      `Recommendation` = recommendation
    ) %>%
    arrange(`Operation mode`, factor(`Status`, levels = c("Critical", "Moderate", "Available capacity")))

  recommendation_report <- recommendations %>%
    transmute(
      `Operation mode` = operation_mode,
      `Resource` = resource,
      `Status` = bottleneck_status,
      `Current units` = mean_units,
      `Proposed units` = proposed_units,
      `P90 utilization` = fmt_pct(p90_utilization),
      `Estimated saving` = fmt_days(estimated_campaign_saving_days),
      `Recommended action` = recommended_action
    ) %>%
    arrange(`Operation mode`, desc(as.numeric(gsub("[^0-9.-]", "", `Estimated saving`))))

  cost_report <- cost_impact %>%
    transmute(
      `Operation mode` = operation_mode,
      `Resource` = resource,
      `Mean fleet days` = fmt_num(mean_fleet_days),
      `Cost per day` = fmt_money(cost_per_day),
      `Estimated cost` = fmt_money(estimated_resource_cost)
    )

  delay_report <- top_delays %>%
    transmute(
      `Operation mode` = operation_mode,
      `Category` = category,
      `Risk event` = risk_event,
      `Event count` = event_count,
      `Total delay` = fmt_days(total_delay_days),
      `Mean delay` = fmt_days(mean_delay_days),
      `Extra plugs` = total_extra_plugs,
      `Extra stages` = total_extra_stages
    )

  # build_traffic_lights() returns one row per operation mode with risk columns.
  # Convert it to a clean long table for the report.
  traffic_report <- traffic %>%
    tidyr::pivot_longer(
      cols = c(schedule_risk, resource_risk, operational_risk, wireline_constraint),
      names_to = "Area",
      values_to = "Status"
    ) %>%
    mutate(
      Area = dplyr::recode(Area,
        schedule_risk = "Schedule risk",
        resource_risk = "Resource risk",
        operational_risk = "Operational risk",
        wireline_constraint = "Wireline constraint"
      ),
      Status = dplyr::recode(Status, Red = "Red", Amber = "Amber", Green = "Green")
    ) %>%
    select(`Operation mode` = operation_mode, Area, Status)

  kpi_cards <- paste0(
    "<div class='kpi-grid'>",
    "<div class='kpi'><span>Best option</span><strong>", html_escape(best_mode), "</strong></div>",
    "<div class='kpi'><span>P50 duration</span><strong>", html_escape(p50), "</strong></div>",
    "<div class='kpi'><span>P90 duration</span><strong>", html_escape(p90), "</strong></div>",
    "<div class='kpi'><span>Zipper saving</span><strong>", html_escape(saving), "</strong></div>",
    "<div class='kpi'><span>Primary bottleneck</span><strong>", html_escape(bottleneck), "</strong></div>",
    "<div class='kpi'><span>Top risk</span><strong>", html_escape(top_risk), "</strong></div>",
    "<div class='kpi'><span>Wireline waiting</span><strong>", html_escape(wireline_wait), "</strong></div>",
    "<div class='kpi'><span>Idle frac fleet cost</span><strong>", html_escape(idle_cost), "</strong></div>",
    "<div class='kpi'><span>Readiness</span><strong>", html_escape(readiness_kpi), "</strong></div>",
    "</div>"
  )

  paste0(
    "<!doctype html><html><head><meta charset='utf-8'><title>Frac Campaign Planning Report</title>",
    "<style>",
    "body{font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Arial,sans-serif;color:#1f2937;margin:36px;line-height:1.35;}",
    "h1{font-size:30px;margin-bottom:4px;}h2{font-size:20px;margin-top:30px;border-bottom:1px solid #d1d5db;padding-bottom:6px;}",
    ".meta{color:#6b7280;margin-bottom:22px;}.summary{background:#f8fafc;border:1px solid #e5e7eb;border-radius:10px;padding:16px;margin:18px 0;}",
    ".kpi-grid{display:grid;grid-template-columns:repeat(3,1fr);gap:10px;margin:18px 0 26px 0;}",
    ".kpi{border:1px solid #d1d5db;border-radius:8px;padding:12px;background:#ffffff;}.kpi span{display:block;color:#6b7280;font-size:12px;margin-bottom:6px;}.kpi strong{font-size:16px;}",
    "table{border-collapse:collapse;width:100%;margin:12px 0 20px 0;font-size:12px;}th{background:#f3f4f6;text-align:left;}th,td{border:1px solid #d1d5db;padding:7px;vertical-align:top;}",
    ".section-note{color:#4b5563;margin:4px 0 10px 0;}ul{margin-top:8px;} .footer{margin-top:28px;color:#6b7280;font-size:12px;}",
    "@media print{body{margin:20mm}.kpi-grid{grid-template-columns:repeat(3,1fr);}h2{break-after:avoid;}table{break-inside:auto;}tr{break-inside:avoid;break-after:auto;}}",
    "</style></head><body>",
    "<h1>Frac Campaign Planning Report</h1>",
    "<div class='meta'>Generated: ", html_escape(format(Sys.time(), "%Y-%m-%d %H:%M:%S")), "</div>",
    "<div class='summary'><p>This report summarizes simulated frac campaign outcomes using the uploaded historical well data, risk assumptions, and selected resource inputs.</p>",
    "<ul>",
    "<li>Best execution option: <strong>", html_escape(best_mode), "</strong></li>",
    "<li>Expected P50 duration: <strong>", html_escape(p50), "</strong></li>",
    "<li>Conservative P90 duration: <strong>", html_escape(p90), "</strong></li>",
    "<li>Primary bottleneck: <strong>", html_escape(bottleneck), "</strong></li>",
    "<li>Top delay risk: <strong>", html_escape(top_risk), "</strong></li>",
    "</ul></div>",
    kpi_cards,
    html_table(sim_report, "Campaign Duration Summary"),
    html_table(readiness_report, "Readiness Score Breakdown", "The readiness score combines schedule certainty, resource availability, risk exposure, and wireline readiness. Higher values indicate better readiness."),
    html_table(traffic_report, "Traffic Light Summary"),
    html_table(bottleneck_report, "Bottleneck Detection"),
    html_table(recommendation_report, "Recommended Actions"),
    html_table(cost_report, "Cost Impact"),
    html_table(delay_report, "Top Delay Contributors"),
    "<h2>Notes and Limitations</h2>",
    "<ul>",
    "<li>This is an operational planning simulation. It is not a hydraulic fracture propagation model.</li>",
    "<li>Results depend on the quality of the uploaded historical data and assumptions.</li>",
    "<li>Cost estimates use the daily rates entered in the app and should be reviewed against contract values.</li>",
    "<li>Readiness scores are decision-support indicators, not deterministic go/no-go criteria.</li>",
    "</ul>",
    "<div class='footer'>Generated by Frac Campaign Planning Simulator.</div>",
    "</body></html>"
  )
}

# Build a dependency-light PDF management report using base R graphics.
# This avoids pagedown/Chrome dependency and produces a true PDF file.

build_management_report_pdf <- function(file, summary, risk_event_log, resource_utilization,
                                        frac_fleet_cost_per_day = 250000,
                                        wireline_cost_per_day = 15000,
                                        ct_cost_per_day = 25000,
                                        milling_cost_per_day = 18000,
                                        testing_unit_cost_per_day = 12000) {
  kpis <- build_executive_kpis(summary, risk_event_log, resource_utilization, frac_fleet_cost_per_day)
  sim_stats <- summarise_simulation(summary)
  resource_summary <- summarise_resource_utilization(resource_utilization)
  bottlenecks <- summarise_bottlenecks(resource_summary)
  recommendations <- build_resource_recommendations(summary, resource_utilization)
  readiness <- build_readiness_score(summary, risk_event_log, resource_utilization)
  cost_impact <- build_cost_impact(summary, resource_utilization, frac_fleet_cost_per_day, wireline_cost_per_day, ct_cost_per_day, milling_cost_per_day, testing_unit_cost_per_day)
  top_delays <- summarise_delay_contributors(risk_event_log) %>% slice_head(n = 10)
  traffic <- build_traffic_lights(summary, risk_event_log, resource_utilization)

  fmt_days <- function(x, digits = 1) {
    ifelse(is.na(x), "N/A", paste0(format(round(as.numeric(x), digits), big.mark = ",", trim = TRUE), " d"))
  }
  fmt_num <- function(x, digits = 1) {
    ifelse(is.na(x), "N/A", format(round(as.numeric(x), digits), big.mark = ",", trim = TRUE))
  }
  fmt_pct <- function(x, digits = 1) {
    ifelse(is.na(x), "N/A", paste0(round(100 * as.numeric(x), digits), "%"))
  }
  fmt_money <- function(x) {
    ifelse(is.na(x), "N/A", paste0("$", format(round(as.numeric(x), 0), big.mark = ",", trim = TRUE)))
  }
  compact <- function(x, width = 45) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    ifelse(nchar(x) > width, paste0(substr(x, 1, width - 3), "..."), x)
  }

  sim_report <- sim_stats %>%
    transmute(
      Mode = operation_mode,
      Sims = simulations,
      P10 = fmt_days(p10_days),
      P50 = fmt_days(p50_days),
      P90 = fmt_days(p90_days),
      Mean = fmt_days(mean_days),
      Stages = fmt_num(mean_stages),
      `Risk delay` = fmt_days(mean_risk_delay_days)
    )

  readiness_report <- readiness %>%
    transmute(
      Mode = operation_mode,
      Score = paste0(round(readiness_score, 1), "/100"),
      Status = readiness_status,
      Schedule = round(schedule_score, 1),
      Resource = round(resource_score, 1),
      Risk = round(risk_score, 1),
      Wireline = round(wireline_score, 1),
      Uncertainty = fmt_pct(uncertainty_ratio),
      `Non-frac util.` = fmt_pct(non_frac_p90_utilization)
    )

  bottleneck_report <- bottlenecks %>%
    transmute(
      Mode = operation_mode,
      Resource = resource,
      `P90 util.` = fmt_pct(p90_utilization),
      Status = bottleneck_status,
      Recommendation = compact(recommendation, 55)
    ) %>%
    arrange(Mode, factor(Status, levels = c("Critical", "Moderate", "Available capacity")))

  # FIX v12: arrange BEFORE transmute. transmute() drops
  # estimated_campaign_saving_days, so arranging on it afterwards raised
  # "In argument: `..2 = estimated_campaign_saving_days`".
  recommendation_report <- recommendations %>%
    arrange(operation_mode, desc(estimated_campaign_saving_days)) %>%
    transmute(
      Mode = operation_mode,
      Resource = resource,
      Status = bottleneck_status,
      Units = paste0(mean_units, " -> ", proposed_units),
      `P90 util.` = fmt_pct(p90_utilization),
      Saving = fmt_days(estimated_campaign_saving_days),
      Action = compact(recommended_action, 55)
    )

  cost_report <- cost_impact %>%
    transmute(
      Mode = operation_mode,
      Resource = compact(resource, 38),
      `Fleet days` = fmt_num(mean_fleet_days),
      `Cost/day` = fmt_money(cost_per_day),
      `Estimated cost` = fmt_money(estimated_resource_cost)
    )

  delay_report <- top_delays %>%
    transmute(
      Mode = operation_mode,
      Category = category,
      Risk = compact(risk_event, 42),
      Events = event_count,
      `Total delay` = fmt_days(total_delay_days),
      `Mean delay` = fmt_days(mean_delay_days),
      `Extra plugs` = total_extra_plugs,
      `Extra stages` = total_extra_stages
    )

  stage_risk_summary <- summarise_stage_level_risks(risk_event_log, summary)
  tornado_report <- if (nrow(stage_risk_summary) == 0) {
    tibble::tibble(
      Mode = character(), Category = character(), Risk = character(),
      `Expected events/campaign` = character(), `Expected delay (d/campaign)` = character(),
      `Mean delay when occurs` = character()
    )
  } else {
    stage_risk_summary %>%
      slice_max(expected_delay_days_per_campaign, n = 10, with_ties = FALSE) %>%
      transmute(
        Mode = operation_mode,
        Category = category,
        Risk = compact(risk_event, 42),
        `Expected events/campaign` = fmt_num(expected_events_per_campaign, 2),
        `Expected delay (d/campaign)` = fmt_days(expected_delay_days_per_campaign),
        `Mean delay when occurs` = fmt_days(mean_delay_when_occurs)
      )
  }

  traffic_report <- traffic %>%
    tidyr::pivot_longer(
      cols = c(schedule_risk, resource_risk, operational_risk, wireline_constraint),
      names_to = "Area",
      values_to = "Status"
    ) %>%
    mutate(
      Area = dplyr::recode(Area,
        schedule_risk = "Schedule risk",
        resource_risk = "Resource risk",
        operational_risk = "Operational risk",
        wireline_constraint = "Wireline constraint"
      )
    ) %>%
    select(Mode = operation_mode, Area, Status)

  get_kpi <- function(name) {
    val <- kpis$value[kpis$kpi == name]
    if (length(val) == 0) "N/A" else val[1]
  }

  # ==========================================================================
  # Rendering: landscape A4, grid + ggplot2 + gridExtra. Branded layout with
  # navy header band, KPI card dashboard, charts per section, styled tables.
  # ==========================================================================
  if (!requireNamespace("gridExtra", quietly = TRUE)) {
    stop("The PDF report requires the 'gridExtra' package. Install with install.packages('gridExtra').")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The PDF report requires the 'ggplot2' package.")
  }

  navy <- "#0F2A43"; teal <- "#18A999"; amber <- "#E6A817"
  red_c <- "#D55E00"; green_c <- "#009E73"; panel_grey <- "#F4F6F8"
  mode_cols <- c("Conventional" = "#0072B2", "Zipper" = "#E69F00")
  status_cols <- c("Critical" = red_c, "Moderate" = amber, "Available capacity" = green_c,
                   "Ready" = green_c, "Caution" = amber, "At Risk" = red_c)
  resource_cols <- c("CT / cleanout" = "#56B4E9", "Wireline" = "#0072B2",
                     "Frac fleet" = "#E69F00", "Milling" = "#D55E00",
                     "Testing unit" = "#009E73")

  rpt_theme <- ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 12, colour = navy),
      plot.subtitle = ggplot2::element_text(size = 9, colour = "grey40"),
      plot.caption = ggplot2::element_text(size = 7.5, colour = "grey45", hjust = 0),
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    )

  tbl_theme <- gridExtra::ttheme_minimal(
    core = list(
      bg_params = list(fill = rep(c("white", panel_grey), length.out = 50), col = NA),
      fg_params = list(cex = 0.66, hjust = 0, x = 0.03)
    ),
    colhead = list(
      bg_params = list(fill = navy, col = NA),
      fg_params = list(col = "white", fontface = "bold", cex = 0.68, hjust = 0, x = 0.03)
    ),
    padding = grid::unit(c(5, 4), "mm")
  )

  page_no <- 0
  new_page <- function(title, subtitle = NULL) {
    page_no <<- page_no + 1
    grid::grid.newpage()
    grid::grid.rect(y = grid::unit(1, "npc"), height = grid::unit(0.09, "npc"),
                    just = "top", gp = grid::gpar(fill = navy, col = NA))
    grid::grid.text(title, x = 0.035, y = if (is.null(subtitle)) 0.955 else 0.965,
                    just = c("left", "center"),
                    gp = grid::gpar(col = "white", fontface = "bold",
                                    cex = 1.3, fontfamily = "sans"))
    if (!is.null(subtitle)) {
      grid::grid.text(subtitle, x = 0.035, y = 0.928, just = c("left", "center"),
                      gp = grid::gpar(col = "#C9D4DF", cex = 0.75))
    }
    grid::grid.text(format(Sys.time(), "%d %b %Y"), x = 0.965, y = 0.955,
                    just = c("right", "center"),
                    gp = grid::gpar(col = "#C9D4DF", cex = 0.78))
    grid::grid.lines(x = c(0.035, 0.965), y = 0.045,
                     gp = grid::gpar(col = "#D5DCE3"))
    grid::grid.text("Frac Campaign Planning Simulator", x = 0.035, y = 0.027,
                    just = "left", gp = grid::gpar(col = "grey45", cex = 0.62))
    grid::grid.text(paste("Page", page_no), x = 0.965, y = 0.027, just = "right",
                    gp = grid::gpar(col = "grey45", cex = 0.62))
  }

  draw_plot_in <- function(p, x, y, w, h) {
    print(p, vp = grid::viewport(x = x, y = y, width = w, height = h))
  }

  draw_table_in <- function(df, x, y, w, h) {
    if (is.null(df) || nrow(df) == 0) {
      grid::grid.text("No data available.", x = x, y = y,
                      gp = grid::gpar(cex = 0.8, col = "grey50"))
      return(invisible(NULL))
    }
    g <- gridExtra::tableGrob(as.data.frame(df), rows = NULL, theme = tbl_theme)
    grid::pushViewport(grid::viewport(x = x, y = y, width = w, height = h))
    # scale down if wider than viewport
    gw <- grid::convertWidth(sum(g$widths), "npc", valueOnly = TRUE)
    gh <- grid::convertHeight(sum(g$heights), "npc", valueOnly = TRUE)
    sc <- min(1, 1 / max(gw, 1e-9), 1 / max(gh, 1e-9))
    grid::pushViewport(grid::viewport(width = sc, height = sc))
    grid::grid.draw(g)
    grid::popViewport(2)
  }

  draw_kpi_card <- function(label, value, x, y, w, h, accent = teal) {
    grid::grid.roundrect(x = x, y = y, width = w, height = h, r = grid::unit(2.5, "mm"),
                         gp = grid::gpar(fill = panel_grey, col = "#DDE3E9"))
    grid::grid.rect(x = x - w / 2 + grid::unit(1.2, "mm"), y = y,
                    width = grid::unit(1.8, "mm"), height = h * 0.62,
                    gp = grid::gpar(fill = accent, col = NA))
    grid::grid.text(toupper(label), x = x - w / 2 + grid::unit(6, "mm"),
                    y = y + h * 0.22, just = c("left", "center"),
                    gp = grid::gpar(cex = 0.58, col = "grey40", fontface = "bold"))
    grid::grid.text(value, x = x - w / 2 + grid::unit(6, "mm"),
                    y = y - h * 0.14, just = c("left", "center"),
                    gp = grid::gpar(cex = 0.95, col = navy, fontface = "bold"))
  }

  # --- Inline charts ---------------------------------------------------------
  pctiles <- summary %>%
    group_by(operation_mode) %>%
    summarise(p10 = quantile(estimated_campaign_days, 0.1, na.rm = TRUE),
              p50 = quantile(estimated_campaign_days, 0.5, na.rm = TRUE),
              p90 = quantile(estimated_campaign_days, 0.9, na.rm = TRUE),
              .groups = "drop")

  scurve_p <- ggplot2::ggplot(summary,
      ggplot2::aes(estimated_campaign_days, colour = operation_mode)) +
    ggplot2::stat_ecdf(linewidth = 1.1) +
    ggplot2::geom_vline(data = pctiles,
      ggplot2::aes(xintercept = p50, colour = operation_mode),
      linetype = "dashed", linewidth = 0.4, show.legend = FALSE) +
    ggplot2::geom_text(data = pctiles,
      ggplot2::aes(x = p50, y = 0.06, colour = operation_mode,
                   label = paste0("P50: ", round(p50), " d")),
      hjust = -0.05, size = 2.9, show.legend = FALSE) +
    ggplot2::scale_colour_manual(values = mode_cols) +
    ggplot2::scale_y_continuous(labels = scales::percent_format()) +
    ggplot2::labs(title = "Campaign duration S-curve",
                  x = "Campaign duration, days", y = "Cumulative probability",
                  colour = NULL) +
    rpt_theme

  readiness_p <- ggplot2::ggplot(readiness,
      ggplot2::aes(operation_mode, readiness_score, fill = readiness_status)) +
    ggplot2::geom_col(width = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(round(readiness_score), "/100")),
                       vjust = -0.45, size = 3.4, fontface = "bold", colour = navy) +
    ggplot2::coord_cartesian(ylim = c(0, 110)) +
    ggplot2::scale_fill_manual(values = status_cols) +
    ggplot2::labs(title = "Campaign readiness", x = NULL, y = "Score", fill = NULL) +
    rpt_theme

  bottleneck_p <- ggplot2::ggplot(bottlenecks,
      ggplot2::aes(resource, p90_utilization, fill = bottleneck_status)) +
    ggplot2::geom_col(width = 0.62) +
    ggplot2::geom_hline(yintercept = c(0.60, 0.85), linetype = "dashed",
                        colour = "grey55", linewidth = 0.35) +
    ggplot2::facet_wrap(~ operation_mode) +
    ggplot2::scale_fill_manual(values = status_cols) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::labs(title = "Bottleneck detection (P90 utilization, thresholds 60% / 85%)",
                  x = NULL, y = "P90 utilization", fill = NULL) +
    rpt_theme +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 28, hjust = 1, size = 7.5))

  timeline <- build_resource_timeline(summary)
  camp_lines <- timeline %>% distinct(operation_mode, campaign_days)

  # Two-layer gantt: light = deployment window, dark = active work
  resource_cols_dark_pdf  <- c("CT / cleanout"="#2980B9","Wireline"="#0F2A43",
                               "Frac fleet"="#D68910","Milling"="#A93226","Testing unit"="#1E8449")
  resource_cols_light_pdf <- c("CT / cleanout"="#AED6F1","Wireline"="#5D8AA8",
                               "Frac fleet"="#FAD7A0","Milling"="#F1948A","Testing unit"="#A9DFBF")

  gantt_p <- ggplot2::ggplot(timeline, ggplot2::aes(y = resource)) +
    ggplot2::geom_segment(
      ggplot2::aes(x = deploy_start, xend = deploy_end, yend = resource,
                   colour = paste0(resource, "_light")),
      linewidth = 7, lineend = "butt", alpha = 0.45) +
    ggplot2::geom_segment(
      ggplot2::aes(x = active_start, xend = active_end, yend = resource,
                   colour = paste0(resource, "_dark")),
      linewidth = 7, lineend = "butt") +
    ggplot2::geom_text(
      ggplot2::aes(x = deploy_end,
                   label = paste0(round(active_days_true), "d / ",
                                  round(100 * utilization_of_deployment), "%"),
                   colour = utilization_of_deployment > 1),
      hjust = -0.08, size = 2.5) +
    ggplot2::scale_colour_manual(
      values = c(`FALSE` = "grey25", `TRUE` = "#D55E00"),
      guide = "none") +
    ggplot2::geom_vline(data = camp_lines,
      ggplot2::aes(xintercept = campaign_days),
      linetype = "dashed", colour = "grey35", linewidth = 0.5) +
    ggplot2::facet_grid(rows = ggplot2::vars(operation_mode)) +
    ggplot2::scale_colour_manual(
      values = c(setNames(resource_cols_dark_pdf,  paste0(names(resource_cols_dark_pdf),  "_dark")),
                 setNames(resource_cols_light_pdf, paste0(names(resource_cols_light_pdf), "_light"))),
      guide = "none") +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.01, 0.20))) +
    ggplot2::labs(
      title = "Resource deployment timeline",
      subtitle = "Light = on-site window  |  Dark = active work days  |  Label: active days / utilization of deployment window",
      caption = "Indicative only: workload-based model. All bars now reach near campaign duration because resources remain mobilised throughout.",
      x = "Campaign day", y = NULL) +
    rpt_theme

  cost_plot_df <- cost_impact %>%
    mutate(resource_w = ifelse(nchar(resource) > 30,
                               paste0(substr(resource, 1, 27), "..."), resource))
  cost_p <- ggplot2::ggplot(cost_plot_df,
      ggplot2::aes(reorder(resource_w, estimated_resource_cost),
                   estimated_resource_cost, fill = operation_mode)) +
    ggplot2::geom_col(position = ggplot2::position_dodge2(preserve = "single"),
                      width = 0.7) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = mode_cols) +
    ggplot2::scale_y_continuous(
      labels = function(x) ifelse(is.na(x), NA_character_,
        ifelse(abs(x) >= 1e6, paste0("$", round(x / 1e6, 1), "M"),
        ifelse(abs(x) >= 1e3, paste0("$", round(x / 1e3), "k"),
               paste0("$", round(x)))))) +
    ggplot2::labs(title = "Estimated resource and idle cost",
                  x = NULL, y = "Estimated cost", fill = NULL) +
    rpt_theme

  delay_plot_df <- top_delays %>%
    mutate(risk_w = ifelse(nchar(risk_event) > 30,
                           paste0(substr(risk_event, 1, 27), "..."), risk_event))
  delay_p <- ggplot2::ggplot(delay_plot_df,
      ggplot2::aes(reorder(risk_w, total_delay_days), total_delay_days,
                   fill = operation_mode)) +
    ggplot2::geom_col(position = ggplot2::position_dodge2(preserve = "single"),
                      width = 0.72) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = mode_cols) +
    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    ggplot2::labs(title = "Top delay contributors",
                  x = NULL, y = "Total delay days across simulations", fill = NULL) +
    rpt_theme

  tornado_p <- plot_risk_tornado(stage_risk_summary) + rpt_theme

  # --- Assemble pages --------------------------------------------------------
  grDevices::pdf(file, width = 11.69, height = 8.27, onefile = TRUE)
  on.exit(grDevices::dev.off(), add = TRUE)

  # Helper: coloured status badge (replaces raw table cells for traffic lights)
  draw_status_badge <- function(status, xc, yc, w = 0.13, h = 0.058) {
    col <- status_cols[status]
    col <- if (is.na(col) || length(col) == 0) "grey70" else unname(col)
    grid::grid.roundrect(
      x = grid::unit(xc, "npc"), y = grid::unit(yc, "npc"),
      width = grid::unit(w, "npc"), height = grid::unit(h, "npc"),
      r = grid::unit(1.8, "mm"),
      gp = grid::gpar(fill = col, col = NA, alpha = 0.88))
    grid::grid.text(status, x = xc, y = yc,
                    gp = grid::gpar(col = "white", fontface = "bold", cex = 0.60))
  }

  # Page 1: executive dashboard + mini S-curve
  new_page("Frac Campaign Planning Report",
           "Monte Carlo campaign simulation — executive summary")
  kpi_show <- head(kpis, 9)
  accents <- rep(c(teal, navy, amber), length.out = nrow(kpi_show))
  ncol_k <- 3
  card_w <- grid::unit(0.29, "npc"); card_h <- grid::unit(0.135, "npc")
  for (i in seq_len(nrow(kpi_show))) {
    row_i <- (i - 1) %/% ncol_k
    col_i <- (i - 1) %% ncol_k
    x <- grid::unit(0.195 + col_i * 0.305, "npc")
    y <- grid::unit(0.76 - row_i * 0.165, "npc")
    draw_kpi_card(kpi_show$kpi[i], kpi_show$value[i], x, y, card_w, card_h,
                  accent = accents[i])
  }
  scurve_mini <- scurve_p +
    ggplot2::theme(legend.position = "right",
                   plot.title    = ggplot2::element_text(size = 9),
                   plot.subtitle = ggplot2::element_blank())
  draw_plot_in(scurve_mini, x = 0.5, y = 0.20, w = 0.92, h = 0.28)

  # Page 2: campaign duration (full chart + styled P10/P50/P90 summary row)
  new_page("Campaign Duration", "Cumulative probability of campaign completion")
  draw_plot_in(scurve_p, x = 0.5, y = 0.535, w = 0.92, h = 0.77)
  modes_ord <- pctiles$operation_mode
  n_modes   <- length(modes_ord)
  card_xs   <- if (n_modes == 1) 0.5 else seq(0.27, 0.73, length.out = n_modes)
  for (mi in seq_along(modes_ord)) {
    m   <- modes_ord[mi]
    row <- pctiles[pctiles$operation_mode == m, ]
    xc  <- card_xs[mi]
    grid::grid.roundrect(
      x = grid::unit(xc, "npc"), y = grid::unit(0.082, "npc"),
      width = grid::unit(0.42, "npc"), height = grid::unit(0.068, "npc"),
      r = grid::unit(2, "mm"), gp = grid::gpar(fill = panel_grey, col = "#DDE3E9"))
    grid::grid.text(
      sprintf("%s  |  P10: %.0f d   P50: %.0f d   P90: %.0f d",
              m, row$p10, row$p50, row$p90),
      x = xc, y = 0.082,
      gp = grid::gpar(cex = 0.76, col = navy, fontface = "bold"))
  }

  # Page 3: readiness and traffic lights (visual badges replace raw tables)
  new_page("Readiness & Traffic Lights")
  draw_plot_in(readiness_p, x = 0.26, y = 0.53, w = 0.44, h = 0.78)
  tl_modes <- unique(traffic_report$Mode)
  tl_areas <- unique(traffic_report$Area)
  n_tl     <- length(tl_modes)
  tl_xs    <- if (n_tl == 1) 0.77 else seq(0.68, 0.90, length.out = n_tl)
  grid::grid.text("Traffic lights", x = 0.76, y = 0.895,
                  just = c("center", "center"),
                  gp = grid::gpar(col = navy, fontface = "bold", cex = 0.90))
  for (ci in seq_along(tl_modes)) {
    grid::grid.text(tl_modes[ci], x = tl_xs[ci], y = 0.853,
                    gp = grid::gpar(cex = 0.68, col = "grey30", fontface = "bold"))
  }
  for (ri in seq_along(tl_areas)) {
    yb <- 0.808 - (ri - 1) * 0.100
    grid::grid.text(tl_areas[ri], x = 0.525, y = yb, just = c("left", "center"),
                    gp = grid::gpar(cex = 0.68, col = "grey30"))
    for (ci in seq_along(tl_modes)) {
      st <- traffic_report$Status[traffic_report$Mode == tl_modes[ci] &
                                    traffic_report$Area == tl_areas[ri]]
      if (length(st) == 0) st <- "N/A"
      draw_status_badge(st[1], xc = tl_xs[ci], yc = yb, w = 0.14, h = 0.060)
    }
  }
  grid::grid.text("Readiness breakdown", x = 0.76, y = 0.380,
                  just = c("center", "center"),
                  gp = grid::gpar(col = navy, fontface = "bold", cex = 0.90))
  for (ri in seq_len(nrow(readiness))) {
    row_r <- readiness[ri, ]
    yrd   <- 0.335 - (ri - 1) * 0.090
    grid::grid.roundrect(
      x = grid::unit(0.76, "npc"), y = grid::unit(yrd, "npc"),
      width = grid::unit(0.44, "npc"), height = grid::unit(0.072, "npc"),
      r = grid::unit(2, "mm"), gp = grid::gpar(fill = panel_grey, col = "#DDE3E9"))
    grid::grid.text(
      sprintf("%s — %s", row_r$operation_mode,
              paste0(round(row_r$readiness_score, 0), "/100")),
      x = 0.76, y = yrd + 0.013,
      gp = grid::gpar(cex = 0.72, col = navy, fontface = "bold"))
    grid::grid.text(
      sprintf("Sched %.0f  /  Resource %.0f  /  Risk %.0f  /  Wireline %.0f",
              row_r$schedule_score, row_r$resource_score,
              row_r$risk_score, row_r$wireline_score),
      x = 0.76, y = yrd - 0.016,
      gp = grid::gpar(cex = 0.64, col = "grey35"))
  }

  # Page 4: resource deployment timeline (full chart)
  new_page("Resource Deployment Timeline", "Indicative sequencing of campaign resources")
  draw_plot_in(gantt_p, x = 0.5, y = 0.48, w = 0.92, h = 0.80)

  # Page 5: bottlenecks and recommendations (chart + styled action bullets)
  new_page("Bottlenecks & Recommended Actions")
  draw_plot_in(bottleneck_p, x = 0.5, y = 0.595, w = 0.92, h = 0.66)
  grid::grid.text("Recommended actions", x = 0.04, y = 0.238,
                  just = c("left", "center"),
                  gp = grid::gpar(col = navy, fontface = "bold", cex = 0.88))
  rec_lines <- recommendations %>%
    arrange(operation_mode, desc(estimated_campaign_saving_days)) %>%
    mutate(ln = sprintf("[%s]  %s: %s  —  est. %.1f d saving, P90 util %.0f%%",
                        operation_mode, resource, recommended_action,
                        estimated_campaign_saving_days, 100 * p90_utilization))
  bullet_y <- 0.200
  for (ln in head(rec_lines$ln, 6)) {
    for (w_ln in strwrap(paste0("•  ", ln), width = 145)) {
      if (bullet_y < 0.065) break
      grid::grid.text(w_ln, x = 0.05, y = bullet_y, just = c("left", "top"),
                      gp = grid::gpar(cex = 0.72, col = "grey20"))
      bullet_y <- bullet_y - 0.034
    }
  }

  # Page 6: cost impact (full chart + total cost summary)
  new_page("Cost Impact", "Estimated resource deployment and idle cost")
  draw_plot_in(cost_p, x = 0.5, y = 0.535, w = 0.92, h = 0.80)
  total_cost <- sum(cost_impact$estimated_resource_cost, na.rm = TRUE)
  grid::grid.roundrect(
    x = grid::unit(0.5, "npc"), y = grid::unit(0.082, "npc"),
    width = grid::unit(0.52, "npc"), height = grid::unit(0.065, "npc"),
    r = grid::unit(2, "mm"), gp = grid::gpar(fill = panel_grey, col = "#DDE3E9"))
  grid::grid.text(
    sprintf("Total estimated cost (all modes combined): %s", fmt_money(total_cost)),
    x = 0.5, y = 0.082,
    gp = grid::gpar(cex = 0.80, col = navy, fontface = "bold"))

  # Page 7: delay contributors (full chart)
  new_page("Schedule Risk Drivers", "Top delay contributors across simulations")
  draw_plot_in(delay_p, x = 0.5, y = 0.535, w = 0.92, h = 0.80)

  # Page 7b: risk event tornado (full chart)
  new_page("Risk Event Tornado", "Expected schedule impact per campaign, by operation mode")
  draw_plot_in(tornado_p, x = 0.5, y = 0.535, w = 0.92, h = 0.80)

  # Page 8: notes
  new_page("Notes & Limitations")
  notes <- c(
    "This is an operational planning simulation. It is not a hydraulic fracture propagation model.",
    "Results depend on the quality of the uploaded historical data and assumptions.",
    "Cost estimates use the daily rates entered in the app and should be reviewed against contract values.",
    "Readiness scores are decision-support indicators, not deterministic go/no-go criteria.",
    "The resource deployment timeline is indicative: the simulator models workloads and capacity, not a discrete-event schedule.",
    "Wireline workload can be similar in conventional and zipper modes because the same number of stages still require plug setting, perforation, and logging. The key difference is whether wireline readiness creates frac waiting time."
  )
  y_pos <- 0.82
  for (note in notes) {
    wrapped <- strwrap(note, width = 110)
    for (ln in wrapped) {
      grid::grid.text(paste0(if (ln == wrapped[1]) "•  " else "    ", ln),
                      x = 0.06, y = y_pos, just = c("left", "top"),
                      gp = grid::gpar(cex = 0.85, col = "grey25"))
      y_pos <- y_pos - 0.045
    }
    y_pos <- y_pos - 0.02
  }

  invisible(file)
}

