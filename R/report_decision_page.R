# report_decision_page.R
# -----------------------------------------------------------------------------
# Prepends a polished "Executive Decision Summary" page to the existing
# management report, WITHOUT editing the engine. It captures the original
# build_management_report_pdf(), renders the new page to a temp PDF, renders
# the original report to another, and stitches them with qpdf/pdftools.
#
# The new page surfaces the V2.5 decision layer: the management narrative (#12),
# the verified/estimated recommendation with EV + confidence (#1), the
# constraint cascade (#2), and uncertainty P-values (#6).
#
# Source AFTER: simulation_engine[_fast].R, risk_uncertainty.R,
#               bottleneck_explain.R, recommendations.R, narrative_engine.R
# Requires (for merge): qpdf  OR  pdftools. Degrades gracefully to the original
# report if neither is installed.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({ library(dplyr) })

# Capture the engine's original exactly once (guard against re-source recursion).
if (!exists(".orig_build_management_report_pdf", inherits = TRUE)) {
  .orig_build_management_report_pdf <- build_management_report_pdf
}

.dp_money <- function(x) {
  if (is.null(x) || is.na(x)) return("n/a"); s <- if (x < 0) "-" else ""; ax <- abs(x)
  if (ax >= 1e6) sprintf("%s$%.1fM", s, ax/1e6)
  else if (ax >= 1e3) sprintf("%s$%.0fk", s, ax/1e3) else sprintf("%s$%.0f", s, ax)
}

# Is there anything to put on the optional "Robustness & scenarios" page?
.has_robustness_scenario_content <- function(robustness, scenario_records) {
  !is.null(robustness) || length(scenario_records) > 0
}

build_decision_summary_page <- function(file, summary, resource_utilization,
                                        recommendation = NULL, narrative = NULL,
                                        target_days = NULL, budget = NULL,
                                        frac_fleet_cost_per_day = 250000, wireline_cost_per_day = 15000,
                                        ct_cost_per_day = 25000, milling_cost_per_day = 18000,
                                        testing_unit_cost_per_day = 12000,
                                        robustness = NULL) {
  navy <- "#0F2A43"; teal <- "#18A999"; panel_grey <- "#F4F6F8"
  okabe <- c(green = "#009E73", amber = "#E6A817", red = "#D55E00", grey = "#8A8A8A")

  # Focus = best (lowest-P50) mode.
  p50_by_mode <- summary %>% group_by(operation_mode) %>%
    summarise(p50 = as.numeric(quantile(estimated_campaign_days, 0.5, na.rm = TRUE)), .groups = "drop")
  mode <- p50_by_mode$operation_mode[which.min(p50_by_mode$p50)]

  sim_result <- list(summary = summary, resource_utilization = resource_utilization)
  unc <- quantify_uncertainty(summary, resource_utilization, target_days = target_days, budget = budget,
           frac_fleet_cost_per_day = frac_fleet_cost_per_day, wireline_cost_per_day = wireline_cost_per_day,
           ct_cost_per_day = ct_cost_per_day, milling_cost_per_day = milling_cost_per_day,
           testing_unit_cost_per_day = testing_unit_cost_per_day)
  um  <- unc[unc$operation_mode == mode, ]
  ex  <- explain_bottlenecks(summary, resource_utilization)
  casc <- ex$cascade %>% filter(operation_mode == mode)
  if (is.null(recommendation))
    recommendation <- recommend_action(sim_result, sim_args = NULL, verify = FALSE,
      frac_fleet_cost_per_day = frac_fleet_cost_per_day, wireline_cost_per_day = wireline_cost_per_day,
      ct_cost_per_day = ct_cost_per_day, milling_cost_per_day = milling_cost_per_day,
      testing_unit_cost_per_day = testing_unit_cost_per_day)
  rec <- recommendation
  if (is.null(narrative))
    narrative <- generate_narrative(sim_result, sim_args = list(operation_mode = mode),
                   target_days = target_days, budget = budget, rec = rec)$narrative

  short_action <- if (isTRUE(rec$worthwhile)) rec$recommendation else "Hold configuration"
  conf <- combine_recommendation_confidence(rec, robustness)

  grDevices::pdf(file, width = 11.69, height = 8.27, onefile = TRUE)
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::grid.newpage()

  # Header band
  grid::grid.rect(y = grid::unit(1, "npc"), height = grid::unit(0.09, "npc"), just = "top",
                  gp = grid::gpar(fill = navy, col = NA))
  grid::grid.text("Executive Decision Summary", x = 0.035, y = 0.965, just = c("left", "center"),
                  gp = grid::gpar(col = "white", fontface = "bold", cex = 1.35))
  grid::grid.text("What to do, what it is worth, and how confident we are",
                  x = 0.035, y = 0.928, just = c("left", "center"),
                  gp = grid::gpar(col = "#C9D4DF", cex = 0.78))
  grid::grid.text(format(Sys.time(), "%d %b %Y"), x = 0.965, y = 0.955, just = c("right", "center"),
                  gp = grid::gpar(col = "#C9D4DF", cex = 0.78))

  # KPI cards
  kpi_card <- function(label, value, xc, accent = teal) {
    w <- grid::unit(0.176, "npc"); h <- grid::unit(0.115, "npc")
    x <- grid::unit(xc, "npc");    y <- grid::unit(0.80, "npc")
    grid::grid.roundrect(x = x, y = y, width = w, height = h, r = grid::unit(2.4, "mm"),
                         gp = grid::gpar(fill = panel_grey, col = "#DDE3E9"))
    grid::grid.rect(x = x - w * 0.5 + grid::unit(1.2, "mm"), y = y, width = grid::unit(1.8, "mm"),
                    height = h * 0.62, gp = grid::gpar(fill = accent, col = NA))
    grid::grid.text(toupper(label), x = x - w * 0.5 + grid::unit(6, "mm"), y = y + h * 0.24,
                    just = c("left", "center"), gp = grid::gpar(cex = 0.56, col = "grey40", fontface = "bold"))
    grid::grid.text(value, x = x - w * 0.5 + grid::unit(6, "mm"), y = y - h * 0.16, just = c("left", "center"),
                    gp = grid::gpar(cex = 0.92, col = navy, fontface = "bold"))
  }
  xs <- seq(0.13, 0.87, length.out = 5)
  conf_accent <- switch(conf$level,
    "High" = okabe["green"], "Moderate" = okabe["amber"], "Low" = okabe["red"], okabe["grey"])
  kpi_card("Best option", mode, xs[1])
  kpi_card("P50 duration", sprintf("%.0f d", um$p50_days), xs[2])
  kpi_card("Recommended action", short_action, xs[3], accent = okabe["amber"])
  kpi_card("Expected value", .dp_money(rec$expected_value), xs[4], accent = okabe["green"])
  kpi_card("Confidence", conf$level, xs[5], accent = unname(conf_accent))

  # Narrative
  grid::grid.text("Management summary", x = 0.035, y = 0.685, just = c("left", "center"),
                  gp = grid::gpar(col = navy, fontface = "bold", cex = 0.92))
  wrapped <- paste(strwrap(narrative, width = 150), collapse = "\n")
  grid::grid.text(wrapped, x = 0.035, y = 0.60, just = c("left", "top"),
                  gp = grid::gpar(cex = 0.78, col = "grey20", lineheight = 1.25))

  # Constraint cascade (left)
  grid::grid.text("Constraint cascade - what to relieve next", x = 0.035, y = 0.44,
                  just = c("left", "center"), gp = grid::gpar(col = navy, fontface = "bold", cex = 0.86))
  casc_tbl <- casc %>% transmute(`Step` = pos, Resource = modal_resource,
                  `Recoverable (d)` = round(mean_gap_days, 1),
                  `Cumulative (d)` = round(cumulative_recoverable_days, 1))
  tt <- gridExtra::ttheme_minimal(
    core = list(bg_params = list(fill = rep(c("white", panel_grey), length.out = 50), col = NA),
                fg_params = list(cex = 0.66, hjust = 0, x = 0.05)),
    colhead = list(bg_params = list(fill = navy, col = NA),
                   fg_params = list(col = "white", fontface = "bold", cex = 0.66, hjust = 0, x = 0.05)))
  g <- gridExtra::tableGrob(as.data.frame(casc_tbl), rows = NULL, theme = tt)
  grid::pushViewport(grid::viewport(x = 0.27, y = 0.27, width = 0.46, height = 0.30))
  grid::grid.draw(g); grid::popViewport()

  # Uncertainty + recommendation evidence (right)
  grid::grid.text("Uncertainty & recommendation", x = 0.55, y = 0.44, just = c("left", "center"),
                  gp = grid::gpar(col = navy, fontface = "bold", cex = 0.86))
  pct <- function(p) if (is.na(p)) "-" else sprintf("%.0f%%", 100*p)
  # combine_recommendation_confidence() phrases its caveat for the on-screen
  # layout ("run the check below"); point at the in-app location instead here.
  conf_detail <- gsub("run the check below", "see the assumption robustness check in Decision support",
                       conf$detail, fixed = TRUE)
  conf_detail <- unlist(lapply(conf_detail, strwrap, width = 72))
  lines <- c(
    sprintf("Duration: P10 %.0f / P50 %.0f / P90 %.0f d", um$p10_days, um$p50_days, um$p90_days),
    sprintf("P(finish by target): %s     P(under budget): %s", pct(um$prob_finish_by_target), pct(um$prob_within_budget)),
    sprintf("P(resource overload): %s", pct(um$prob_resource_overload)),
    "",
    sprintf("Recommendation: %s", rec$recommendation),
    sprintf("Primary constraint: %s (%s, P90 util %.0f%%)", rec$bottleneck, rec$status, 100*rec$p90_utilization),
    sprintf("Expected P50 reduction: %.0f d   |   EV: %s", rec$expected_reduction_days, .dp_money(rec$expected_value)),
    "",
    conf_detail
  )
  grid::grid.text(paste(lines, collapse = "\n"), x = 0.55, y = 0.37, just = c("left", "top"),
                  gp = grid::gpar(cex = 0.74, col = "grey20", lineheight = 1.3))

  # Footer
  grid::grid.lines(x = c(0.035, 0.965), y = 0.045, gp = grid::gpar(col = "#D5DCE3"))
  grid::grid.text("Frac Campaign Planning Simulator - decision support", x = 0.035, y = 0.027,
                  just = "left", gp = grid::gpar(col = "grey45", cex = 0.62))
  grid::grid.text("Planning-level estimates - review EV against contract rates.",
                  x = 0.965, y = 0.027, just = "right", gp = grid::gpar(col = "grey45", cex = 0.62))
  invisible(file)
}

# Optional second page: the assumption-robustness sweep (OAT + combined
# best/base/stress, from assess_recommendation_robustness()) and the saved
# scenario library, if either is available for this session. Only called
# when .has_robustness_scenario_content() is TRUE.
build_robustness_scenario_page <- function(file, robustness = NULL, scenario_records = NULL) {
  navy <- "#0F2A43"; panel_grey <- "#F4F6F8"

  grDevices::pdf(file, width = 11.69, height = 8.27, onefile = TRUE)
  on.exit(grDevices::dev.off(), add = TRUE)

  tt <- gridExtra::ttheme_minimal(
    core    = list(bg_params = list(fill = rep(c("white", panel_grey), length.out = 50), col = NA),
                   fg_params = list(cex = 0.66, hjust = 0, x = 0.05)),
    colhead = list(bg_params = list(fill = navy, col = NA),
                   fg_params = list(col = "white", fontface = "bold", cex = 0.66, hjust = 0, x = 0.05)))

  .page_header <- function(subtitle) {
    grid::grid.newpage()
    grid::grid.rect(y = grid::unit(1, "npc"), height = grid::unit(0.09, "npc"), just = "top",
                    gp = grid::gpar(fill = navy, col = NA))
    grid::grid.text("Robustness & Scenario Comparison", x = 0.035, y = 0.965, just = c("left", "center"),
                    gp = grid::gpar(col = "white", fontface = "bold", cex = 1.35))
    grid::grid.text(subtitle, x = 0.035, y = 0.928, just = c("left", "center"),
                    gp = grid::gpar(col = "#C9D4DF", cex = 0.78))
    grid::grid.text(format(Sys.time(), "%d %b %Y"), x = 0.965, y = 0.955, just = c("right", "center"),
                    gp = grid::gpar(col = "#C9D4DF", cex = 0.78))
  }
  .page_footer <- function() {
    grid::grid.lines(x = c(0.035, 0.965), y = 0.045, gp = grid::gpar(col = "#D5DCE3"))
    grid::grid.text("Frac Campaign Planning Simulator - decision support", x = 0.035, y = 0.027,
                    just = "left", gp = grid::gpar(col = "grey45", cex = 0.62))
    grid::grid.text("Planning-level estimates - review EV against contract rates.",
                    x = 0.965, y = 0.027, just = "right", gp = grid::gpar(col = "grey45", cex = 0.62))
  }

  # ---- Page: assumption robustness (always rendered) -------------------------
  .page_header("How sensitive is the recommendation to planning assumptions?")
  y <- 0.86
  grid::grid.text("Assumption robustness check", x = 0.035, y = y, just = c("left", "center"),
                  gp = grid::gpar(col = navy, fontface = "bold", cex = 0.92))

  if (!is.null(robustness)) {
    n_total    <- nrow(robustness$summary)
    n_unstable <- sum(!robustness$summary$stable)
    stress     <- robustness$combined %>% filter(scenario == "Stress case")

    oat_line <- if (n_unstable == 0) {
      sprintf("One-at-a-time: \"%s\" holds across +-%.0f%% swings in all %d assumptions tested (n=%d iterations each).",
              robustness$base$recommendation, 100 * robustness$perturb_pct, n_total, robustness$n_iterations)
    } else {
      sprintf("One-at-a-time: base recommendation \"%s\" flips under a +-%.0f%% swing in %d of %d assumptions (n=%d iterations each).",
              robustness$base$recommendation, 100 * robustness$perturb_pct, n_unstable, n_total, robustness$n_iterations)
    }
    stress_line <- if (isTRUE(stress$stable[1])) {
      sprintf("Combined stress case (all %d assumptions +-%.0f%% unfavourable together): recommendation unchanged, P50 %+.1f d vs base.",
              n_total, 100 * robustness$perturb_pct, stress$delta_p50_days[1])
    } else {
      sprintf("Combined stress case (all %d assumptions +-%.0f%% unfavourable together): recommendation changes to \"%s\", P50 %+.1f d vs base.",
              n_total, 100 * robustness$perturb_pct, stress$recommendation[1], stress$delta_p50_days[1])
    }
    grid::grid.text(paste(c(oat_line, stress_line), collapse = "\n"),
                    x = 0.035, y = y - 0.045, just = c("left", "top"),
                    gp = grid::gpar(cex = 0.74, col = "grey20", lineheight = 1.3))

    # Best/base/stress table
    combined_tbl <- robustness$combined %>% transmute(
      Scenario        = as.character(scenario),
      `P50 (d)`       = round(p50_days, 1),
      `P90 (d)`       = round(p90_days, 1),
      `Delta P50 (d)` = sprintf("%+.1f", delta_p50_days),
      Readiness       = sprintf("%.0f (%s)", readiness_score, readiness_status),
      Recommendation  = recommendation,
      `Vs. base`      = ifelse(stable, "Unchanged", "Changed")
    )
    g_tbl <- gridExtra::tableGrob(as.data.frame(combined_tbl), rows = NULL, theme = tt)
    grid::pushViewport(grid::viewport(x = 0.5, y = y - 0.195, width = 0.93, height = 0.155))
    grid::grid.draw(g_tbl); grid::popViewport()

    # Tornado chart (sensitivity by assumption) — smaller base font for PDF
    tornado <- plot_robustness_tornado(robustness) + theme_frac(base_size = 9)
    gp_tornado <- ggplotGrob(tornado)
    grid::pushViewport(grid::viewport(x = 0.5, y = y - 0.505, width = 0.93, height = 0.32))
    grid::grid.draw(gp_tornado); grid::popViewport()
  } else {
    grid::grid.text(
      "Not checked this session - run \"Check robustness\" in Decision support before generating the report to include this view.",
      x = 0.035, y = y - 0.045, just = c("left", "top"),
      gp = grid::gpar(cex = 0.74, col = "grey45", lineheight = 1.3))
  }
  .page_footer()

  # ---- Page: scenario comparison (only when records exist) -------------------
  if (length(scenario_records) > 0) {
    .page_header("How do the saved configurations compare?")
    grid::grid.text("Saved scenario comparison", x = 0.035, y = 0.86, just = c("left", "center"),
                    gp = grid::gpar(col = navy, fontface = "bold", cex = 0.92))
    sc_tbl <- scenario_library_to_df(scenario_records) %>% select(-id)
    g_sc <- gridExtra::tableGrob(as.data.frame(sc_tbl), rows = NULL, theme = tt)
    grid::pushViewport(grid::viewport(x = 0.5, y = 0.62, width = 0.93, height = 0.40))
    grid::grid.draw(g_sc); grid::popViewport()
    .page_footer()
  }

  invisible(file)
}

# Override: prepend the decision page (and, if available, a robustness/
# scenario page), then the original report; stitch.
build_management_report_pdf <- function(file, summary, risk_event_log, resource_utilization,
    frac_fleet_cost_per_day = 250000, wireline_cost_per_day = 15000, ct_cost_per_day = 25000,
    milling_cost_per_day = 18000, testing_unit_cost_per_day = 12000,
    target_days = NULL, budget = NULL, recommendation = NULL, narrative = NULL,
    robustness = NULL, scenario_records = NULL) {

  render_original <- function(out) .orig_build_management_report_pdf(
    out, summary, risk_event_log, resource_utilization,
    frac_fleet_cost_per_day, wireline_cost_per_day, ct_cost_per_day,
    milling_cost_per_day, testing_unit_cost_per_day)

  have_merge <- requireNamespace("qpdf", quietly = TRUE) || requireNamespace("pdftools", quietly = TRUE)
  if (!have_merge) {  # graceful: no merge tool -> original report only
    warning("Install 'qpdf' (or 'pdftools') to prepend the Executive Decision Summary page.")
    return(render_original(file))
  }

  pages <- c(tempfile(fileext = ".pdf"))
  on.exit(unlink(pages), add = TRUE)
  build_decision_summary_page(pages[1], summary, resource_utilization, recommendation, narrative,
    target_days, budget, frac_fleet_cost_per_day, wireline_cost_per_day, ct_cost_per_day,
    milling_cost_per_day, testing_unit_cost_per_day, robustness = robustness)

  if (.has_robustness_scenario_content(robustness, scenario_records)) {
    rb_page <- tempfile(fileext = ".pdf")
    pages <- c(pages, rb_page)
    build_robustness_scenario_page(rb_page, robustness, scenario_records)
  }

  orig_page <- tempfile(fileext = ".pdf")
  pages <- c(pages, orig_page)
  render_original(orig_page)

  if (requireNamespace("qpdf", quietly = TRUE)) qpdf::pdf_combine(pages, output = file)
  else pdftools::pdf_combine(pages, output = file)
  invisible(file)
}
