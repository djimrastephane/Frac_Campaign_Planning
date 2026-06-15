# plots.R
# Version 13: legibility pass.
#   - Shared theme_frac(): base_size 14, bottom legends so horizontal bar
#     charts get the full panel width.
#   - Long category labels wrapped with wrap_lbl() so risk names no longer
#     overlap or get clipped.
#   - Cost axis uses compact dollar labels ($5M instead of $5,000,000).
#   - Okabe-Ito colourblind-safe palettes throughout.

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(stringr)
})

mode_colours <- c(
  "Conventional" = "#0072B2",  # blue
  "Zipper" = "#E69F00"         # orange
)

status_colours <- c(
  "Critical" = "#D55E00",
  "At Risk" = "#D55E00",
  "Moderate" = "#E69F00",
  "Caution" = "#E69F00",
  "Available capacity" = "#009E73",
  "Ready" = "#009E73"
)

scale_fill_mode <- function() scale_fill_manual(values = mode_colours, na.value = "#999999")
scale_colour_mode <- function() scale_colour_manual(values = mode_colours, na.value = "#999999")
scale_fill_status <- function() scale_fill_manual(values = status_colours, na.value = "#999999")

theme_frac <- function(base_size = 14, legend = "bottom") {
  theme_minimal(base_size = base_size) +
    theme(
      legend.position = legend,
      plot.title = element_text(face = "bold", size = base_size + 1),
      plot.title.position = "plot",
      panel.grid.minor = element_blank()
    )
}

wrap_lbl <- function(x, width = 28) str_wrap(as.character(x), width = width)

# Robust compact dollar formatter. scales::cut_short_scale() crashes with
# "NAs are not allowed in subscripted assignments" on vectors mixing zeros
# and large values, so format manually.
dollar_compact <- function(x) {
  vapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    s <- if (v < 0) "-" else ""
    a <- abs(v)
    if (a >= 1e9) paste0(s, "$", round(a / 1e9, 1), "B")
    else if (a >= 1e6) paste0(s, "$", round(a / 1e6, 1), "M")
    else if (a >= 1e3) paste0(s, "$", format(round(a / 1e3), big.mark = ","), "k")
    else paste0(s, "$", round(a))
  }, character(1))
}

plot_campaign_distribution <- function(results) {
  if (is.list(results) && "summary" %in% names(results)) results <- results$summary
  if ("operation_mode" %in% names(results) && length(unique(results$operation_mode)) > 1) {
    ggplot(results, aes(x = estimated_campaign_days, fill = operation_mode)) +
      geom_histogram(bins = 40, alpha = 0.55, position = "identity") +
      scale_fill_mode() +
      labs(
        title = "Campaign duration distribution",
        x = "Estimated campaign duration, days",
        y = "Simulation count",
        fill = NULL
      ) +
      theme_frac()
  } else {
    ggplot(results, aes(x = estimated_campaign_days)) +
      geom_histogram(bins = 40, fill = "#0072B2") +
      labs(
        title = "Campaign duration distribution",
        x = "Estimated campaign duration, days",
        y = "Simulation count"
      ) +
      theme_frac()
  }
}

plot_campaign_scurve <- function(results) {
  if (is.list(results) && "summary" %in% names(results)) results <- results$summary
  if (is.null(results) || nrow(results) == 0) {
    return(ggplot() + labs(title = "No simulation results"))
  }

  pct <- results %>%
    group_by(operation_mode) %>%
    summarise(
      p10 = quantile(estimated_campaign_days, 0.10, na.rm = TRUE),
      p50 = quantile(estimated_campaign_days, 0.50, na.rm = TRUE),
      p90 = quantile(estimated_campaign_days, 0.90, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(c(p10, p50, p90), names_to = "pct", values_to = "days") %>%
    mutate(
      prob = dplyr::recode(pct, p10 = 0.10, p50 = 0.50, p90 = 0.90),
      label = paste0(toupper(pct), ": ", round(days, 0), " d")
    )

  ggplot(results, aes(estimated_campaign_days, colour = operation_mode)) +
    stat_ecdf(linewidth = 1.1) +
    geom_segment(
      data = pct,
      aes(x = days, xend = days, y = 0, yend = prob),
      linetype = "dashed", linewidth = 0.4
    ) +
    geom_point(data = pct, aes(days, prob), size = 2.6) +
    geom_text(
      data = pct, aes(days, prob, label = label),
      hjust = -0.08, vjust = 1.25, size = 3.6, show.legend = FALSE
    ) +
    scale_colour_mode() +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_x_continuous(expand = expansion(mult = c(0.02, 0.12))) +
    labs(
      title = "Campaign duration S-curve",
      x = "Estimated campaign duration, days",
      y = "Cumulative probability",
      colour = NULL
    ) +
    theme_frac()
}

plot_risk_tornado <- function(stage_risk_summary, top_n = 8) {
  if (is.null(stage_risk_summary) || nrow(stage_risk_summary) == 0) {
    return(ggplot() + labs(title = "No risk events triggered"))
  }
  df <- stage_risk_summary %>%
    group_by(risk_event) %>%
    summarise(expected_delay = sum(expected_delay_days_per_campaign, na.rm = TRUE), .groups = "drop") %>%
    slice_max(expected_delay, n = top_n) %>%
    mutate(risk_event = reorder(wrap_lbl(risk_event), expected_delay))

  ggplot(df, aes(expected_delay, risk_event)) +
    geom_col(fill = "#D55E00", width = 0.7) +
    geom_text(aes(label = paste0(round(expected_delay, 1), " d")),
              hjust = -0.15, size = 4.2, fontface = "bold") +
    scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
    labs(
      title = "Expected schedule impact per campaign",
      x = "Expected delay, days / campaign",
      y = NULL
    ) +
    theme_frac()
}

plot_delay_contributors <- function(delay_summary, top_n = 10) {
  if (is.null(delay_summary) || nrow(delay_summary) == 0) {
    return(ggplot() + labs(title = "No risk events triggered"))
  }

  plot_df <- delay_summary %>%
    group_by(operation_mode, risk_event) %>%
    summarise(total_delay_days = sum(total_delay_days, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total_delay_days)) %>%
    slice_head(n = top_n) %>%
    mutate(risk_event = reorder(wrap_lbl(risk_event), total_delay_days))

  ggplot(plot_df, aes(x = risk_event, y = total_delay_days, fill = operation_mode)) +
    geom_col(position = position_dodge2(preserve = "single"), width = 0.75) +
    coord_flip() +
    scale_fill_mode() +
    scale_y_continuous(labels = scales::label_comma()) +
    labs(
      title = "Top delay contributors",
      x = NULL,
      y = "Total delay days across simulations",
      fill = NULL
    ) +
    theme_frac()
}

plot_resource_utilization <- function(resource_summary) {
  if (is.null(resource_summary) || nrow(resource_summary) == 0) {
    return(ggplot() + labs(title = "No resource utilization data"))
  }

  ggplot(resource_summary, aes(x = resource, y = mean_utilization, fill = operation_mode)) +
    geom_col(position = position_dodge2(preserve = "single"), width = 0.7) +
    geom_text(aes(label = scales::percent(mean_utilization, accuracy = 1)),
              position = position_dodge2(width = 0.7, preserve = "single"),
              vjust = -0.4, size = 3.8) +
    scale_fill_mode() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       expand = expansion(mult = c(0, 0.12))) +
    labs(
      title = "Mean resource utilization",
      x = NULL,
      y = "Mean utilization",
      fill = NULL
    ) +
    theme_frac()
}

plot_bottlenecks <- function(bottleneck_summary) {
  if (is.null(bottleneck_summary) || nrow(bottleneck_summary) == 0) {
    return(ggplot() + labs(title = "No bottleneck data"))
  }

  ggplot(bottleneck_summary, aes(x = resource, y = p90_utilization, fill = bottleneck_status)) +
    geom_col(width = 0.65) +
    geom_hline(yintercept = c(0.60, 0.85), linetype = "dashed", colour = "grey50", linewidth = 0.4) +
    geom_text(aes(label = scales::percent(p90_utilization, accuracy = 1)),
              vjust = -0.4, size = 3.8) +
    facet_wrap(~ operation_mode) +
    scale_fill_status() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       expand = expansion(mult = c(0, 0.12))) +
    labs(
      title = "Bottleneck detection by resource (dashed lines: 60% / 85% thresholds)",
      x = NULL,
      y = "P90 utilization",
      fill = NULL
    ) +
    theme_frac() +
    theme(axis.text.x = element_text(angle = 25, hjust = 1))
}

plot_stage_level_risks <- function(stage_risk_summary, top_n = 10) {
  if (is.null(stage_risk_summary) || nrow(stage_risk_summary) == 0) {
    return(ggplot() + labs(title = "No stage-level risk events triggered"))
  }

  plot_df <- stage_risk_summary %>%
    group_by(operation_mode, risk_event) %>%
    summarise(expected_events_per_campaign = sum(expected_events_per_campaign, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(expected_events_per_campaign)) %>%
    slice_head(n = top_n) %>%
    mutate(risk_event = reorder(wrap_lbl(risk_event), expected_events_per_campaign))

  ggplot(plot_df, aes(x = risk_event, y = expected_events_per_campaign, fill = operation_mode)) +
    geom_col(position = position_dodge2(preserve = "single"), width = 0.75) +
    coord_flip() +
    scale_fill_mode() +
    labs(
      title = "Expected stage-level risk events per campaign",
      x = NULL,
      y = "Expected events per campaign",
      fill = NULL
    ) +
    theme_frac()
}

plot_wireline_constraint <- function(wireline_summary) {
  if (is.null(wireline_summary) || nrow(wireline_summary) == 0) {
    return(ggplot() + labs(title = "No wireline constraint data"))
  }

  plot_df <- wireline_summary %>%
    select(
      operation_mode,
      wireline_stage_time = mean_wireline_stage_operation_days,
      rig_up_down = mean_wireline_rig_up_down_days,
      wireline_contingency = mean_wireline_contingency_days,
      temperature_logging = mean_temperature_logging_days,
      frac_settling = mean_frac_settling_days,
      readiness_delay = mean_wireline_readiness_delay_days
    ) %>%
    tidyr::pivot_longer(
      cols = -operation_mode,
      names_to = "component",
      values_to = "days"
    ) %>%
    mutate(
      component = dplyr::recode(
        component,
        wireline_stage_time = "Wireline stage time",
        rig_up_down = "Wireline rig up/down",
        wireline_contingency = "Wireline contingency",
        temperature_logging = "Temperature logging",
        frac_settling = "Frac settling time",
        readiness_delay = "Frac waiting on wireline"
      )
    )

  ggplot(plot_df, aes(x = component, y = days, fill = operation_mode)) +
    geom_col(position = position_dodge2(preserve = "single"), width = 0.75) +
    geom_text(aes(label = round(days, 1)),
              position = position_dodge2(width = 0.75, preserve = "single"),
              hjust = -0.2, size = 3.6) +
    coord_flip() +
    scale_fill_mode() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(
      title = "Wireline stage-readiness constraint",
      x = NULL,
      y = "Mean days per campaign",
      fill = NULL
    ) +
    theme_frac()
}

plot_readiness_score <- function(readiness_score) {
  if (is.null(readiness_score) || nrow(readiness_score) == 0) {
    return(ggplot() + labs(title = "No readiness score data"))
  }

  ggplot(readiness_score, aes(x = operation_mode, y = readiness_score, fill = readiness_status)) +
    geom_col(width = 0.5) +
    geom_text(aes(label = paste0(round(readiness_score, 0), " / 100")),
              vjust = -0.5, size = 4.4, fontface = "bold") +
    coord_cartesian(ylim = c(0, 105)) +
    scale_fill_status() +
    labs(
      title = "Campaign readiness score",
      x = NULL,
      y = "Readiness score",
      fill = NULL
    ) +
    theme_frac()
}

plot_resource_recommendations <- function(recommendations) {
  if (is.null(recommendations) || nrow(recommendations) == 0) {
    return(ggplot() + labs(title = "No resource recommendation data"))
  }

  plot_df <- recommendations %>%
    dplyr::filter(estimated_campaign_saving_days > 0) %>%
    dplyr::mutate(resource = reorder(wrap_lbl(resource), estimated_campaign_saving_days))

  if (nrow(plot_df) == 0) {
    return(ggplot() + labs(title = "No material resource recommendations"))
  }

  ggplot(plot_df, aes(x = resource, y = estimated_campaign_saving_days, fill = operation_mode)) +
    geom_col(position = position_dodge2(preserve = "single"), width = 0.65) +
    geom_text(aes(label = paste0(round(estimated_campaign_saving_days, 1), " d")),
              position = position_dodge2(width = 0.65, preserve = "single"),
              hjust = -0.2, size = 3.8) +
    coord_flip() +
    scale_fill_mode() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(
      title = "Estimated schedule improvement from additional resources",
      x = NULL,
      y = "Estimated campaign saving, days",
      fill = NULL
    ) +
    theme_frac()
}

plot_cost_impact <- function(cost_impact) {
  if (is.null(cost_impact) || nrow(cost_impact) == 0) {
    return(ggplot() + labs(title = "No cost impact data"))
  }

  plot_df <- cost_impact %>%
    dplyr::group_by(operation_mode, resource) %>%
    dplyr::summarise(estimated_resource_cost = sum(estimated_resource_cost, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(desc(estimated_resource_cost)) %>%
    dplyr::slice_head(n = 12) %>%
    dplyr::mutate(resource = reorder(wrap_lbl(resource, 30), estimated_resource_cost))

  ggplot(plot_df, aes(x = resource, y = estimated_resource_cost, fill = operation_mode)) +
    geom_col(position = position_dodge2(preserve = "single"), width = 0.75) +
    geom_text(aes(label = dollar_compact(estimated_resource_cost)),
              position = position_dodge2(width = 0.75, preserve = "single"),
              hjust = -0.1, size = 3.6) +
    coord_flip() +
    scale_fill_mode() +
    scale_y_continuous(labels = dollar_compact, expand = expansion(mult = c(0, 0.18))) +
    labs(
      title = "Estimated resource and idle cost impact",
      x = NULL,
      y = "Estimated cost",
      fill = NULL
    ) +
    theme_frac()
}

# NEW v13: input fidelity check - simulated vs historical milling days/plug,
# plus the sidebar frac rate vs the historical mean. Campaign-level backtest
# requires historical campaign durations (roadmap).
# Input fidelity validation plot — redesigned v17.2
# Issues with the previous version:
#   1. geom_density() kernel extends BEYOND data range, looking like the
#      simulation samples outside the historical bounds (it doesn't).
#   2. With 13 historical wells vs 3000+ simulated draws, KDE y-scales
#      are incomparable — historical looked flat, simulated looked huge.
#   3. A genuine outlier (high milling rate on one exceptional well) caused
#      the simulated distribution to show a secondary mode, alarming users.
#
# Redesigned approach:
#   - Overlaid histograms with after_stat(density) normalisation so both
#     distributions are on the same 0-1 probability scale.
#   - Rug marks on the x-axis show individual historical data points, making
#     outliers visible and explained rather than hidden in a smooth curve.
#   - Vertical lines for historical mean and median.
#   - Data quality warning when n_historical < 20 (bootstrap unreliable).
#   - Second panel: frac rate sidebar vs historical comparison (bar chart,
#     not just a caption).
plot_input_validation <- function(historical_wells, well_details,
                                  frac_time_per_stage_hours = NULL) {
  if (is.null(historical_wells) || nrow(historical_wells) == 0) {
    return(ggplot() + labs(title = "No historical data available"))
  }

  hist_mill <- historical_wells$milling_days_per_plug
  hist_mill <- hist_mill[!is.na(hist_mill) & hist_mill > 0]
  n_hist <- length(hist_mill)

  df_hist <- tibble::tibble(source = "Historical wells", value = hist_mill)

  df_sim <- NULL
  if (!is.null(well_details) && nrow(well_details) > 0 &&
      "milling_days_per_plug" %in% names(well_details)) {
    sim_vals <- well_details$milling_days_per_plug
    sim_vals <- sim_vals[!is.na(sim_vals) & sim_vals > 0]
    # Cap at 2000 draws for rendering speed; stratified by unique value
    if (length(sim_vals) > 2000) sim_vals <- sample(sim_vals, 2000)
    df_sim <- tibble::tibble(source = "Simulated draws", value = sim_vals)
  }

  df <- dplyr::bind_rows(df_hist, df_sim)

  # Reference lines: historical mean and median
  ref <- tibble::tibble(
    metric = c("Historical mean", "Historical median"),
    value  = c(mean(hist_mill), median(hist_mill)),
    col    = c("#0072B2", "#56B4E9")
  )

  # Detect outliers (> mean + 2*sd) for annotation
  outlier_threshold <- mean(hist_mill) + 2 * sd(hist_mill)
  outliers <- hist_mill[hist_mill > outlier_threshold]

  low_n_note <- if (n_hist < 20) {
    sprintf(
      "Note: only %d historical wells. Bootstrap resampling with small n can produce spiky distributions ",
      n_hist
    )
  } else NULL

  frac_caption <- if (!is.null(frac_time_per_stage_hours) &&
                      "frac_days_per_stage" %in% names(historical_wells)) {
    hist_frac <- historical_wells$frac_days_per_stage
    hist_frac_mean <- mean(hist_frac[!is.na(hist_frac) & hist_frac > 0], na.rm = TRUE)
    sidebar_days <- frac_time_per_stage_hours / 24
    delta_pct <- round(100 * (sidebar_days - hist_frac_mean) / hist_frac_mean)
    sign_str <- if (delta_pct > 0) paste0("+", delta_pct) else as.character(delta_pct)
    sprintf(
      "Frac rate check: sidebar = %.1f h/stage (%.2f d) | historical mean = %.2f d/stage | difference: %s%%",
      frac_time_per_stage_hours, sidebar_days, hist_frac_mean, sign_str
    )
  } else NULL

  caption_text <- paste(c(low_n_note, frac_caption), collapse = "
")

  # Faceted layout: historical and simulated on separate panels with shared
  # x-axis and identical bin breaks. Fixes the n-disparity problem where
  # 13 historical wells vs 2000+ simulated draws made overlaid histograms
  # unreadable (one source always dominated the density axis).
  x_range <- range(df$value, na.rm = TRUE)
  x_pad   <- diff(x_range) * 0.06
  n_bins  <- max(15, min(35, ceiling(diff(x_range) / 0.04)))
  shared_breaks <- seq(x_range[1] - x_pad, x_range[2] + x_pad,
                       length.out = n_bins + 1)

  facet_labels <- c(
    "Historical wells" = paste0("Historical wells  (n = ", n_hist, ")"),
    "Simulated draws"  = paste0("Simulated draws  (n = ", nrow(df_sim), ")")
  )

  ggplot(df, aes(value, after_stat(density), fill = source, colour = source)) +
    geom_histogram(breaks = shared_breaks, alpha = 0.75, linewidth = 0.2) +
    facet_grid(rows = vars(source), scales = "free_y",
               labeller = labeller(source = facet_labels)) +
    # Individual well dots: one point per historical well, jittered vertically
    # within the historical panel. Much more visible than rug marks, and
    # directly shows clustering and outliers.
    geom_point(
      data = df_hist %>% dplyr::mutate(source = "Historical wells"),
      aes(x = value, y = -0.4),
      inherit.aes = FALSE,
      shape = 21, size = 3.2, colour = "#0072B2", fill = "#AED6F1",
      stroke = 0.8, position = position_jitter(height = 0.08, seed = 42)
    ) +
    # Reference lines: vertical dashed lines on both panels
    geom_vline(data = ref, aes(xintercept = value, linetype = metric),
               colour = "#0072B2", linewidth = 0.65, show.legend = TRUE) +
    # Outlier label in the historical panel
    {if (length(outliers) > 0)
      geom_text(
        data = data.frame(
          value = outliers,
          source = "Historical wells",
          label  = paste0("Outlier: ", round(outliers, 2), " d")
        ),
        aes(x = value, y = Inf, label = label),
        inherit.aes = FALSE,
        vjust = 1.5, hjust = 1.05, size = 3.0,
        colour = "#D55E00", fontface = "italic")
    } +
    scale_fill_manual(values = c("Historical wells" = "#0072B2",
                                 "Simulated draws"  = "#E69F00"),
                      guide = "none") +
    scale_colour_manual(values = c("Historical wells" = "#0072B2",
                                   "Simulated draws"  = "#E69F00"),
                        guide = "none") +
    scale_linetype_manual(values = c("Historical mean"   = "dotted",
                                     "Historical median" = "dashed"),
                          name = NULL) +
    scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
    labs(
      title = "Input fidelity: milling days per plug",
      subtitle = paste0(
        "Shared bin breaks across both panels. ",
        "Dots = individual historical wells (jittered). ",
        "Dashed lines = historical mean / median."
      ),
      x = "Milling days per plug",
      y = "Probability density",
      caption = caption_text
    ) +
    theme_frac() +
    theme(legend.position = "bottom",
          strip.text.y = element_text(size = 9.5, face = "bold", angle = 0),
          plot.caption = element_text(hjust = 0, size = 9, colour = "grey35"))
}

# Redesigned v17.5: Resource utilization comparison chart.
#
# Replaces the two-layer Gantt. New design: grouped horizontal bars,
# two panels side by side showing active fleet-days and utilization %.
# Makes the Conv vs Zipper comparison immediately readable:
#   Frac fleet: Conv 147d vs Zip 106d -> the source of the schedule saving.
#   Milling/Testing: same in both -> post-frac is not the differentiator.
plot_resource_gantt <- function(timeline) {
  if (is.null(timeline) || nrow(timeline) == 0) {
    return(ggplot() + labs(title = "No timeline data - run a simulation first"))
  }

  resource_cols_fill <- c("Conventional" = "#5D8AA8", "Zipper" = "#E69F00")

  df <- timeline %>%
    mutate(
      resource = factor(resource, levels = rev(c(
        "CT / cleanout","Wireline","Frac fleet","Milling","Testing unit"))),
      overloaded = utilization_of_campaign > 1,
      util_pct = round(100 * utilization_of_campaign),
      active_label = paste0(round(active_days_true), "d"),
      util_label   = paste0(util_pct, "%")
    )
  camp_lines <- timeline %>% distinct(operation_mode, campaign_days)

  p_days <- ggplot(df, aes(x = active_days_true, y = resource, fill = operation_mode)) +
    geom_col(position = position_dodge(width = 0.72), width = 0.65, alpha = 0.88) +
    geom_text(
      aes(label = active_label, colour = overloaded),
      position = position_dodge(width = 0.72),
      hjust = -0.1, size = 3.1, fontface = "bold"
    ) +
    geom_vline(
      data = camp_lines,
      aes(xintercept = campaign_days, colour = operation_mode),
      linetype = "dashed", linewidth = 0.65, show.legend = FALSE
    ) +
    # Stagger campaign labels vertically to prevent overlap when Conv and Zipper
    # campaign durations are similar (e.g. both ~420d with TU=1)
    geom_label(
      data = camp_lines %>%
        dplyr::mutate(y_pos = dplyr::if_else(operation_mode == "Conventional", 1.4, 0.5)),
      aes(x = campaign_days, y = y_pos,
          label = paste0(operation_mode, ": ", round(campaign_days), "d"),
          colour = operation_mode),
      inherit.aes = FALSE, size = 2.8, label.size = 0.3, fill = "white",
      vjust = 0.5, hjust = -0.05, show.legend = FALSE
    ) +
    scale_fill_manual(values = resource_cols_fill, name = NULL) +
    scale_colour_manual(
      values = c("Conventional" = "#5D8AA8", "Zipper" = "#E69F00",
                 `TRUE` = "#D55E00", `FALSE` = "grey20"),
      guide = "none"
    ) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
    labs(
      title = "Active fleet-days per resource",
      subtitle = "Dashed line = P50 campaign duration. Bars = actual work days.",
      x = "Fleet-days of active work", y = NULL
    ) +
    theme_frac() +
    theme(legend.position = "bottom", legend.key.size = unit(0.5, "cm"))

  p_util <- ggplot(df, aes(x = util_pct, y = resource, fill = operation_mode)) +
    geom_col(position = position_dodge(width = 0.72), width = 0.65, alpha = 0.88) +
    geom_text(
      aes(label = util_label, colour = overloaded),
      position = position_dodge(width = 0.72),
      hjust = -0.1, size = 3.1, fontface = "bold"
    ) +
    geom_vline(xintercept = 100, linetype = "dashed",
               colour = "#D55E00", linewidth = 0.65) +
    annotate("text", x = 100, y = 0.5, label = "100%",
             hjust = -0.1, size = 2.9, colour = "#D55E00") +
    scale_fill_manual(values = resource_cols_fill, name = NULL) +
    scale_colour_manual(
      values = c("Conventional" = "#5D8AA8", "Zipper" = "#E69F00",
                 `TRUE` = "#D55E00", `FALSE` = "grey20"),
      guide = "none"
    ) +
    scale_x_continuous(
      expand = expansion(mult = c(0, 0.18)),
      limits = c(0, NA),
      labels = function(x) paste0(x, "%")
    ) +
    labs(
      title = "Resource occupancy during campaign",
      subtitle = "Red dashed = 100% (fully utilised). Labels in red = overloaded (bottleneck).",
      x = "Active days ÷ campaign days", y = NULL
    ) +
    theme_frac() +
    theme(legend.position = "bottom",
          legend.key.size = unit(0.5, "cm"),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())

  tryCatch(
    cowplot::plot_grid(p_days, p_util, nrow = 1, rel_widths = c(1.3, 1),
                       align = "h", axis = "tb"),
    error = function(e)
      p_days + labs(caption = paste0(
        "Install cowplot for dual-panel view. ",
        "Indicative only - simulator models workloads, not a discrete-event schedule."))
  )
}

# NEW v15: direct delay vs induced workload per risk - the consequence
# propagation view. Answers whether technical risks carry real schedule weight.
plot_risk_consequences <- function(consequences, top_n = 10) {
  if (is.null(consequences) || nrow(consequences) == 0) {
    return(ggplot() + labs(title = "No risk events triggered"))
  }

  comp_cols <- c(
    "Direct delay" = "#999999",
    "Wireline rework" = "#0072B2",
    "CT intervention" = "#56B4E9",
    "Extra milling" = "#D55E00",
    "Extra testing" = "#009E73",
    "Extra pumping" = "#E69F00"
  )

  plot_df <- consequences %>%
    group_by(risk_event) %>%
    summarise(
      `Direct delay` = sum(direct_delay_days, na.rm = TRUE),
      `Wireline rework` = sum(induced_wireline_days, na.rm = TRUE),
      `CT intervention` = sum(induced_ct_days, na.rm = TRUE),
      `Extra milling` = sum(induced_milling_days, na.rm = TRUE),
      `Extra testing` = sum(induced_testing_days, na.rm = TRUE),
      `Extra pumping` = sum(induced_frac_days, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(total = rowSums(across(-risk_event))) %>%
    slice_max(total, n = top_n) %>%
    select(-total) %>%
    tidyr::pivot_longer(-risk_event, names_to = "component", values_to = "days") %>%
    mutate(
      risk_event = reorder(wrap_lbl(risk_event), days, FUN = sum),
      component = factor(component, levels = names(comp_cols))
    )

  ggplot(plot_df, aes(days, risk_event, fill = component)) +
    geom_col(width = 0.72) +
    scale_fill_manual(values = comp_cols) +
    scale_x_continuous(labels = scales::label_comma(),
                       expand = expansion(mult = c(0, 0.08))) +
    labs(
      title = "Risk impact: direct delay vs induced workload",
      subtitle = "Induced components are operational consequences propagated into resource workloads.",
      x = "Total days across simulations", y = NULL, fill = NULL
    ) +
    theme_frac()
}

# NEW v17: constraint cascade waterfall chart
plot_constraint_cascade <- function(cascade) {
  if (is.null(cascade) || nrow(cascade) == 0) {
    return(ggplot() + labs(title = "Run the cascade analyser to see constraint sequence"))
  }

  # P50 step chart
  df <- cascade %>%
    mutate(
      label = ifelse(step == 0, "Current", action),
      label = stringr::str_wrap(label, 22),
      label = factor(label, levels = label),
      fill_col = dplyr::case_when(
        step == 0           ~ "#5D8AA8",
        grepl("Recommended", verdict) ~ "#009E73",
        grepl("Marginal|Consider", verdict) ~ "#E6A817",
        TRUE                ~ "#D55E00"
      ),
      roi_label = ifelse(
        step > 0 & !is.na(roi_days_per_Mdollar),
        sprintf("%.0f d/M$", roi_days_per_Mdollar), ""
      ),
      saving_label = ifelse(
        step > 0,
        sprintf("-%.0f d\n%s", days_saved, roi_label), ""
      )
    )

  ggplot(df, aes(x = label, y = p50_days, fill = fill_col)) +
    geom_col(width = 0.65, show.legend = FALSE) +
    geom_errorbar(aes(ymin = p10_days, ymax = p90_days),
                  width = 0.25, colour = "grey40", linewidth = 0.6) +
    geom_text(aes(label = sprintf("%.0f d", p50_days)),
              vjust = -0.5, size = 3.8, fontface = "bold", colour = "#0F2A43") +
    geom_text(aes(y = p50_days / 2, label = saving_label),
              size = 3.1, colour = "white", fontface = "bold") +
    scale_fill_identity() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15)),
                       labels = scales::label_comma()) +
    labs(
      title = "Constraint cascade: P50 after each fix",
      subtitle = "Error bars = P10-P90 range. Labels on bars: days saved and ROI (days per $M invested).",
      caption = "Green = recommended action. Amber = consider. Each step resolves the binding constraint from the previous step.",
      x = NULL, y = "P50 campaign duration, days"
    ) +
    theme_frac() +
    theme(axis.text.x = element_text(size = 9, lineheight = 1.15))
}

# NEW v17: constraint utilization heatmap across cascade steps
plot_cascade_utilization <- function(cascade) {
  if (is.null(cascade) || nrow(cascade) == 0) {
    return(ggplot() + labs(title = "Run cascade analyser"))
  }

  resources <- c("Testing unit", "Milling", "Frac fleet", "Wireline", "CT / cleanout")

  # Build utilization from bottleneck and bottleneck_util for just the current bn
  # (we only track the primary bottleneck per step from the cascade)
  df_util <- cascade %>%
    mutate(step_label = ifelse(step == 0, "Current",
                               sprintf("Step %d", step))) %>%
    mutate(step_label = factor(step_label, levels = rev(unique(step_label))))

  # Add a utilization column that maps each step's bottleneck
  # We'll approximate: bottleneck resource = 100%, others decrease proportionally
  # This is indicative only; actual utilization per step comes from the run.
  # The key story is the bottleneck sequence.
  ggplot(df_util, aes(x = step_label, y = "Binding constraint")) +
    geom_tile(aes(fill = bottleneck_util_pct), colour = "white", linewidth = 0.5) +
    geom_text(aes(label = paste0(bottleneck_now, "\n",
                                 round(bottleneck_util_pct), "%")),
              size = 3.2, lineheight = 1.2, colour = "white", fontface = "bold") +
    scale_fill_gradient2(low = "#009E73", mid = "#E6A817", high = "#D55E00",
                         midpoint = 75, limits = c(0, 100),
                         breaks = c(0, 25, 50, 75, 100),
                         labels = c("0%", "25%", "50%", "75%", "100%")) +
    coord_flip() +
    labs(title = "Binding constraint at each step",
         subtitle = "Colour = P90 utilization of the binding resource after each fix",
         x = NULL, y = NULL, fill = "P90 utilization") +
    theme_frac() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid = element_blank(),
          legend.position = "bottom",
          legend.key.width = unit(2.5, "cm"),
          legend.key.height = unit(0.4, "cm"),
          legend.title = element_text(size = 9),
          legend.text = element_text(size = 8),
          plot.margin = margin(5, 25, 5, 5))
}

# Pareto frontier of scenario optimiser results.
plot_pareto_frontier <- function(optim_results) {
  if (is.null(optim_results) || nrow(optim_results) == 0) {
    return(ggplot() + labs(title = "Run the optimiser to see the trade-off frontier"))
  }

  front <- optim_results %>% filter(pareto) %>% arrange(p50_days)
  rec   <- optim_results %>% filter(recommended)
  fast  <- optim_results %>% filter(fastest) %>% slice(1)

  ggplot(optim_results,
         aes(p50_days, total_mobilisation_cost / 1e6, colour = operation_mode)) +
    geom_point(aes(shape = pareto, size = pareto), alpha = 0.75) +
    geom_line(data = front, aes(group = 1), colour = "grey40",
              linetype = "dashed", linewidth = 0.5) +
    geom_point(data = rec,  size = 5.5, shape = 18, colour = "#009E73") +
    geom_text(data = rec,  aes(label = "Recommended"), vjust = -1.2,
              size = 3.6, fontface = "bold", colour = "#009E73", show.legend = FALSE) +
    geom_text(data = fast, aes(label = "Fastest"), vjust = -1.2,
              size = 3.3, colour = "grey35", show.legend = FALSE) +
    scale_colour_mode() +
    scale_shape_manual(values = c(`TRUE` = 17, `FALSE` = 16), guide = "none") +
    scale_size_manual(values  = c(`TRUE` = 3.4, `FALSE` = 2.2), guide = "none") +
    scale_y_continuous(labels = scales::label_dollar(suffix = "M")) +
    labs(
      title    = "Scenario trade-off: duration vs total mobilisation cost",
      subtitle = "Triangles = Pareto-efficient. Screened at reduced iterations; top 5 refined. Hover for details.",
      caption  = "Total cost = units \u00d7 day rate \u00d7 P50. Re-run the full simulation after applying settings for the definitive P50.",
      x = "P50 campaign duration, days",
      y = "Total mobilisation cost",
      colour = NULL
    ) +
    theme_frac()
}
