# plots.R

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
})

plot_campaign_distribution <- function(results) {
  if (is.list(results) && "summary" %in% names(results)) results <- results$summary
  if ("operation_mode" %in% names(results) && length(unique(results$operation_mode)) > 1) {
    ggplot(results, aes(x = estimated_campaign_days, fill = operation_mode)) +
      geom_histogram(bins = 40, alpha = 0.55, position = "identity") +
      labs(
        title = "Campaign duration distribution",
        x = "Estimated campaign duration, days",
        y = "Simulation count",
        fill = "Operation mode"
      ) +
      theme_minimal()
  } else {
    ggplot(results, aes(x = estimated_campaign_days)) +
      geom_histogram(bins = 40) +
      labs(
        title = "Campaign duration distribution",
        x = "Estimated campaign duration, days",
        y = "Simulation count"
      ) +
      theme_minimal()
  }
}

plot_delay_contributors <- function(delay_summary, top_n = 12) {
  if (is.null(delay_summary) || nrow(delay_summary) == 0) {
    return(ggplot() + labs(title = "No risk events triggered"))
  }

  plot_df <- delay_summary %>%
    group_by(operation_mode, risk_event) %>%
    summarise(total_delay_days = sum(total_delay_days, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total_delay_days)) %>%
    slice_head(n = top_n) %>%
    mutate(risk_event = reorder(risk_event, total_delay_days))

  ggplot(plot_df, aes(x = risk_event, y = total_delay_days, fill = operation_mode)) +
    geom_col(position = "dodge") +
    coord_flip() +
    labs(
      title = "Top delay contributors",
      x = "Risk event",
      y = "Total delay days across simulations",
      fill = "Operation mode"
    ) +
    theme_minimal()
}

plot_resource_utilization <- function(resource_summary) {
  if (is.null(resource_summary) || nrow(resource_summary) == 0) {
    return(ggplot() + labs(title = "No resource utilization data"))
  }

  ggplot(resource_summary, aes(x = resource, y = mean_utilization, fill = operation_mode)) +
    geom_col(position = "dodge") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      title = "Mean resource utilization",
      x = "Resource",
      y = "Mean utilization",
      fill = "Operation mode"
    ) +
    theme_minimal()
}
