# simulation_engine.R
# Monte Carlo simulation engine for multi-pad frac campaign planning.
# Version 5: detailed audit trail, well-level details, risk event log, and resource utilization.

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(stringr)
  library(tibble)
})

normalise_text <- function(x) {
  str_squish(str_to_lower(as.character(x)))
}

triangle_sample <- function(min_val, mode_val, max_val, n = 1) {
  min_val <- rep(as.numeric(min_val), length.out = n)
  mode_val <- rep(as.numeric(mode_val), length.out = n)
  max_val <- rep(as.numeric(max_val), length.out = n)

  out <- rep(NA_real_, n)
  ok <- !is.na(min_val) & !is.na(mode_val) & !is.na(max_val) &
    max_val >= min_val & mode_val >= min_val & mode_val <= max_val

  if (!any(ok)) return(out)

  deterministic <- ok & (max_val == min_val)
  out[deterministic] <- min_val[deterministic]

  stochastic <- ok & !deterministic
  if (any(stochastic)) {
    u <- runif(sum(stochastic))
    mn <- min_val[stochastic]
    md <- mode_val[stochastic]
    mx <- max_val[stochastic]
    c_val <- (md - mn) / (mx - mn)

    out[stochastic] <- ifelse(
      u < c_val,
      mn + sqrt(u * (mx - mn) * (md - mn)),
      mx - sqrt((1 - u) * (mx - mn) * (mx - md))
    )
  }

  out
}

get_param_row <- function(assumptions, variable_name) {
  assumptions %>%
    mutate(.variable_key = normalise_text(variable)) %>%
    filter(.variable_key == normalise_text(variable_name)) %>%
    slice(1)
}

sample_param <- function(assumptions, variable_name, n = 1) {
  row <- get_param_row(assumptions, variable_name)
  if (nrow(row) == 0) stop("Missing parameter in assumptions file: ", variable_name)
  triangle_sample(row$min_days, row$most_likely_days, row$max_days, n)
}

sample_integer_param <- function(assumptions, variable_name, n = 1) {
  row <- get_param_row(assumptions, variable_name)
  if (nrow(row) == 0) stop("Missing parameter in assumptions file: ", variable_name)

  mn <- as.integer(round(row$min_days))
  mx <- as.integer(round(row$max_days))

  if (is.na(mn) || is.na(mx)) stop("Parameter has missing min/max values: ", variable_name)
  if (mx < mn) stop("Parameter has Max lower than Min: ", variable_name)

  sample(seq(mn, mx), size = n, replace = TRUE)
}

build_pad_assignment <- function(n_wells, assumptions) {
  pad_sizes <- integer(0)
  remaining <- n_wells

  while (remaining > 0) {
    next_size <- sample_integer_param(assumptions, "Wells per pad", 1)
    next_size <- min(next_size, remaining)
    pad_sizes <- c(pad_sizes, next_size)
    remaining <- remaining - next_size
  }

  tibble(
    well_index = seq_len(n_wells),
    pad_id = rep(paste0("Pad_", stringr::str_pad(seq_along(pad_sizes), 2, pad = "0")), pad_sizes)
  )
}

risk_resource_class <- function(category, variable) {
  key <- normalise_text(paste(category, variable))
  case_when(
    str_detect(key, "wireline|perforation|upct|gun|plug") ~ "wireline",
    str_detect(key, "ct unit|cement|cleanout|scraper") ~ "ct",
    str_detect(key, "milling") ~ "milling",
    str_detect(key, "weather|regulatory|permit|security|lockdown|camp|access|vendor") ~ "external",
    TRUE ~ "frac"
  )
}


empty_risk_event_log <- function() {
  tibble(
    simulation_id = integer(), operation_mode = character(), well_id = character(), pad_id = character(),
    stage_id = character(), category = character(), risk_event = character(), resource_class = character(),
    probability_used = numeric(), delay_days = numeric(), extra_plugs = numeric(), extra_stages = numeric(),
    min_delay_days = numeric(), most_likely_delay_days = numeric(), max_delay_days = numeric(),
    simulation_impact = character()
  )
}

simulate_campaign_detailed <- function(
    historical_wells,
    assumptions,
    n_wells = 30,
    n_iterations = 1000,
    frac_fleets = 1,
    milling_units = 1,
    wireline_units = 1,
    ct_units = 1,
    frac_trees = 2,
    operation_mode = "Conventional",
    zipper_efficiency = 0.75,
    risk_multiplier = 1,
    seed = NULL
) {
  if (!is.null(seed)) set.seed(seed)

  n_wells <- as.integer(n_wells)
  n_iterations <- as.integer(n_iterations)
  frac_fleets <- as.numeric(frac_fleets)
  milling_units <- as.numeric(milling_units)
  wireline_units <- as.numeric(wireline_units)
  ct_units <- as.numeric(ct_units)
  frac_trees <- as.numeric(frac_trees)
  zipper_efficiency <- as.numeric(zipper_efficiency)
  risk_multiplier <- as.numeric(risk_multiplier)
  operation_mode <- as.character(operation_mode)

  if (n_wells <= 0) stop("n_wells must be positive.")
  if (n_iterations <= 0) stop("n_iterations must be positive.")
  if (frac_fleets <= 0) stop("frac_fleets must be positive.")
  if (milling_units <= 0) stop("milling_units must be positive.")
  if (wireline_units <= 0) stop("wireline_units must be positive.")
  if (ct_units <= 0) stop("ct_units must be positive.")
  if (frac_trees <= 0) stop("frac_trees must be positive.")

  hist_frac <- historical_wells$frac_days_per_stage
  hist_frac <- hist_frac[!is.na(hist_frac) & hist_frac > 0]

  hist_milling <- historical_wells$milling_days_per_plug
  hist_milling <- hist_milling[!is.na(hist_milling) & hist_milling > 0]

  if (length(hist_frac) == 0) stop("No positive FracDaysPerStage values available in historical_wells.csv.")
  if (length(hist_milling) == 0) stop("No positive MillingDaysPerPlug values available in historical_wells.csv.")

  mode_key <- normalise_text(operation_mode)
  is_zipper <- mode_key == "zipper"

  if (is_zipper && frac_trees < 2) {
    stop("Zipper frac requires at least 2 frac trees.")
  }

  mode_factor <- if (is_zipper) zipper_efficiency else 1
  mode_factor <- max(0.4, min(1.2, mode_factor))

  risk_table <- assumptions %>%
    filter(normalise_text(type) == "risk") %>%
    mutate(
      adjusted_probability = pmin(as.numeric(probability) * risk_multiplier, 1),
      adjusted_probability = ifelse(is.na(adjusted_probability), 0, adjusted_probability),
      adds_plug = str_detect(normalise_text(simulation_impact), "plug"),
      adds_stage = str_detect(normalise_text(simulation_impact), "extra stage|additional stage|re-frac|refrac|lost stage|screen out"),
      resource_class = risk_resource_class(category, variable),
      risk_event = as.character(variable)
    )

  scmt_offline_row <- get_param_row(assumptions, "SCMT offline")
  scmt_offline_prob <- if (nrow(scmt_offline_row) == 0 || is.na(scmt_offline_row$probability)) {
    0.8
  } else {
    as.numeric(scmt_offline_row$probability)
  }
  scmt_offline_prob <- max(0, min(1, scmt_offline_prob))

  summary_list <- vector("list", n_iterations)
  well_list <- vector("list", n_iterations)
  risk_log_list <- vector("list", n_iterations)
  resource_list <- vector("list", n_iterations)

  for (iter_id in seq_len(n_iterations)) {
    pad_assignment <- build_pad_assignment(n_wells, assumptions)
    stage_count <- sample_integer_param(assumptions, "Stages per well", n_wells)
    temp_log_count <- sample_integer_param(assumptions, "Temperature log stages", n_wells)

    frac_days_per_stage <- sample(hist_frac, n_wells, replace = TRUE)
    milling_days_per_plug <- sample(hist_milling, n_wells, replace = TRUE)
    scmt_days <- sample_param(assumptions, "SCMT duration", n_wells)
    cleanout_days <- sample_param(assumptions, "Scraper / cleanout run", n_wells)
    temp_log_duration <- sample_param(assumptions, "Temperature log duration", n_wells)
    isolation_plug_days <- sample_param(assumptions, "Isolation plug duration", n_wells)

    well_df <- tibble(
      simulation_id = iter_id,
      operation_mode = operation_mode,
      well_index = seq_len(n_wells),
      well_id = paste0("SimWell_", str_pad(seq_len(n_wells), width = 3, pad = "0")),
      stages = stage_count,
      temp_log_stages = temp_log_count,
      scmt_offline = rbinom(n_wells, 1, scmt_offline_prob) == 1,
      frac_days_per_stage = frac_days_per_stage,
      milling_days_per_plug = milling_days_per_plug,
      scmt_days = scmt_days,
      cleanout_days = cleanout_days,
      temp_log_days = temp_log_count * temp_log_duration,
      isolation_plug_days = isolation_plug_days,
      base_frac_days = stage_count * frac_days_per_stage
    ) %>%
      left_join(pad_assignment, by = "well_index") %>%
      select(-well_index)

    if (nrow(risk_table) > 0) {
      risk_by_well_all <- map_dfr(seq_len(n_wells), function(i) {
        risk_table %>%
          mutate(
            simulation_id = iter_id,
            operation_mode = operation_mode,
            well_id = well_df$well_id[i],
            pad_id = well_df$pad_id[i],
            stage_id = case_when(
              resource_class %in% c("wireline", "frac") ~ paste0("Stage_", str_pad(sample(seq_len(well_df$stages[i]), n(), replace = TRUE), 2, pad = "0")),
              TRUE ~ "Well-level"
            ),
            occurs = rbinom(n(), 1, adjusted_probability),
            delay_days = ifelse(
              occurs == 1,
              triangle_sample(min_days, most_likely_days, max_days, n()),
              0
            ),
            extra_plugs = ifelse(occurs == 1 & adds_plug, 1, 0),
            extra_stages = ifelse(occurs == 1 & adds_stage, 1, 0),
            probability_used = adjusted_probability,
            min_delay_days = min_days,
            most_likely_delay_days = most_likely_days,
            max_delay_days = max_days
          )
      })

      risk_log <- risk_by_well_all %>%
        filter(occurs == 1) %>%
        transmute(
          simulation_id,
          operation_mode,
          well_id,
          pad_id,
          stage_id,
          category,
          risk_event,
          resource_class,
          probability_used,
          delay_days,
          extra_plugs,
          extra_stages,
          min_delay_days,
          most_likely_delay_days,
          max_delay_days,
          simulation_impact
        )

      risk_summary <- risk_by_well_all %>%
        group_by(well_id) %>%
        summarise(
          frac_risk_delay_days = sum(delay_days[resource_class == "frac"], na.rm = TRUE),
          wireline_risk_delay_days = sum(delay_days[resource_class == "wireline"], na.rm = TRUE),
          ct_risk_delay_days = sum(delay_days[resource_class == "ct"], na.rm = TRUE),
          milling_risk_delay_days = sum(delay_days[resource_class == "milling"], na.rm = TRUE),
          external_risk_delay_days = sum(delay_days[resource_class == "external"], na.rm = TRUE),
          risk_delay_days = sum(delay_days, na.rm = TRUE),
          extra_plugs = sum(extra_plugs, na.rm = TRUE),
          extra_stages = sum(extra_stages, na.rm = TRUE),
          .groups = "drop"
        )
    } else {
      risk_log <- empty_risk_event_log()
      risk_summary <- tibble(
        well_id = well_df$well_id,
        frac_risk_delay_days = 0,
        wireline_risk_delay_days = 0,
        ct_risk_delay_days = 0,
        milling_risk_delay_days = 0,
        external_risk_delay_days = 0,
        risk_delay_days = 0,
        extra_plugs = 0,
        extra_stages = 0
      )
    }

    well_df <- well_df %>%
      left_join(risk_summary, by = "well_id") %>%
      mutate(
        final_stages = stages + extra_stages,
        plugs = final_stages + extra_plugs,
        online_scmt_days = ifelse(scmt_offline, 0, scmt_days),
        milling_days = plugs * milling_days_per_plug,
        frac_execution_days = (final_stages * frac_days_per_stage + isolation_plug_days + frac_risk_delay_days) * mode_factor,
        wireline_workload_days = temp_log_days + wireline_risk_delay_days,
        ct_workload_days = online_scmt_days + cleanout_days + ct_risk_delay_days,
        frac_workload_days = frac_execution_days + external_risk_delay_days,
        milling_workload_days = milling_days + milling_risk_delay_days,
        frac_fleet_days = frac_workload_days / frac_fleets,
        wireline_fleet_days = wireline_workload_days / wireline_units,
        ct_fleet_days = ct_workload_days / ct_units,
        milling_fleet_days = milling_workload_days / milling_units,
        frac_related_days = frac_fleet_days + wireline_fleet_days + ct_fleet_days
      )

    total_frac_related_days <- sum(well_df$frac_related_days, na.rm = TRUE)
    total_milling_fleet_days <- sum(well_df$milling_fleet_days, na.rm = TRUE)
    estimated_campaign_days <- max(total_frac_related_days, total_milling_fleet_days)

    summary_list[[iter_id]] <- tibble(
      simulation_id = iter_id,
      operation_mode = operation_mode,
      wells = n_wells,
      frac_fleets = frac_fleets,
      milling_units = milling_units,
      wireline_units = wireline_units,
      ct_units = ct_units,
      frac_trees = frac_trees,
      zipper_efficiency = ifelse(is_zipper, mode_factor, NA_real_),
      total_pads = n_distinct(well_df$pad_id),
      total_stages = sum(well_df$final_stages, na.rm = TRUE),
      total_plugs = sum(well_df$plugs, na.rm = TRUE),
      total_extra_plugs = sum(well_df$extra_plugs, na.rm = TRUE),
      total_extra_stages = sum(well_df$extra_stages, na.rm = TRUE),
      total_risk_delay_days = sum(well_df$risk_delay_days, na.rm = TRUE),
      total_frac_workload_days = sum(well_df$frac_workload_days, na.rm = TRUE),
      total_wireline_workload_days = sum(well_df$wireline_workload_days, na.rm = TRUE),
      total_ct_workload_days = sum(well_df$ct_workload_days, na.rm = TRUE),
      total_milling_workload_days = sum(well_df$milling_workload_days, na.rm = TRUE),
      total_frac_fleet_days = sum(well_df$frac_fleet_days, na.rm = TRUE),
      total_wireline_fleet_days = sum(well_df$wireline_fleet_days, na.rm = TRUE),
      total_ct_fleet_days = sum(well_df$ct_fleet_days, na.rm = TRUE),
      total_milling_fleet_days = total_milling_fleet_days,
      estimated_campaign_days = estimated_campaign_days
    )

    resource_names <- c("Frac fleet", "Wireline", "CT / cleanout", "Milling")
    resource_units <- c(frac_fleets, wireline_units, ct_units, milling_units)
    resource_workload <- c(
      sum(well_df$frac_workload_days, na.rm = TRUE),
      sum(well_df$wireline_workload_days, na.rm = TRUE),
      sum(well_df$ct_workload_days, na.rm = TRUE),
      sum(well_df$milling_workload_days, na.rm = TRUE)
    )
    resource_fleet_days <- c(
      sum(well_df$frac_fleet_days, na.rm = TRUE),
      sum(well_df$wireline_fleet_days, na.rm = TRUE),
      sum(well_df$ct_fleet_days, na.rm = TRUE),
      sum(well_df$milling_fleet_days, na.rm = TRUE)
    )

    resource_list[[iter_id]] <- tibble(
      simulation_id = iter_id,
      operation_mode = operation_mode,
      resource = resource_names,
      units = resource_units,
      workload_days = resource_workload,
      fleet_days_after_resources = resource_fleet_days,
      utilization = resource_workload / pmax(estimated_campaign_days * resource_units, 1e-9)
    )

    well_list[[iter_id]] <- well_df %>%
      select(
        simulation_id, operation_mode, pad_id, well_id, stages, extra_stages, final_stages,
        temp_log_stages, scmt_offline, plugs, extra_plugs,
        frac_days_per_stage, milling_days_per_plug, scmt_days, cleanout_days,
        base_frac_days, frac_execution_days, wireline_workload_days, ct_workload_days,
        milling_workload_days, risk_delay_days, frac_related_days, milling_fleet_days
      )

    risk_log_list[[iter_id]] <- risk_log
  }

  summary <- bind_rows(summary_list)
  well_details <- bind_rows(well_list)
  risk_event_log <- bind_rows(risk_log_list)
  if (ncol(risk_event_log) == 0) risk_event_log <- empty_risk_event_log()
  resource_utilization <- bind_rows(resource_list)

  assumptions_used <- assumptions %>%
    mutate(
      risk_multiplier_used = risk_multiplier,
      probability_used = ifelse(normalise_text(type) == "risk", pmin(as.numeric(probability) * risk_multiplier, 1), as.numeric(probability))
    )

  list(
    summary = summary,
    well_details = well_details,
    risk_event_log = risk_event_log,
    resource_utilization = resource_utilization,
    assumptions_used = assumptions_used
  )
}

# Compatibility wrapper for earlier app versions.
simulate_one_campaign <- function(...) {
  simulate_campaign_detailed(...)$summary
}

summarise_simulation <- function(results) {
  if (is.list(results) && "summary" %in% names(results)) results <- results$summary
  group_cols <- intersect(c("operation_mode"), names(results))

  results %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      simulations = n(),
      p10_days = as.numeric(quantile(estimated_campaign_days, 0.10, na.rm = TRUE)),
      p50_days = as.numeric(quantile(estimated_campaign_days, 0.50, na.rm = TRUE)),
      p90_days = as.numeric(quantile(estimated_campaign_days, 0.90, na.rm = TRUE)),
      mean_days = mean(estimated_campaign_days, na.rm = TRUE),
      mean_stages = mean(total_stages, na.rm = TRUE),
      mean_extra_plugs = mean(total_extra_plugs, na.rm = TRUE),
      mean_extra_stages = mean(total_extra_stages, na.rm = TRUE),
      mean_risk_delay_days = mean(total_risk_delay_days, na.rm = TRUE),
      .groups = "drop"
    )
}

summarise_delay_contributors <- function(risk_event_log) {
  if (is.null(risk_event_log) || nrow(risk_event_log) == 0) {
    return(tibble(risk_event = character(), event_count = integer(), total_delay_days = numeric(), mean_delay_days = numeric()))
  }

  risk_event_log %>%
    group_by(operation_mode, category, risk_event) %>%
    summarise(
      event_count = n(),
      total_delay_days = sum(delay_days, na.rm = TRUE),
      mean_delay_days = mean(delay_days, na.rm = TRUE),
      total_extra_plugs = sum(extra_plugs, na.rm = TRUE),
      total_extra_stages = sum(extra_stages, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(total_delay_days))
}

summarise_resource_utilization <- function(resource_utilization) {
  if (is.null(resource_utilization) || nrow(resource_utilization) == 0) {
    return(tibble())
  }

  resource_utilization %>%
    group_by(operation_mode, resource) %>%
    summarise(
      mean_units = mean(units, na.rm = TRUE),
      mean_workload_days = mean(workload_days, na.rm = TRUE),
      mean_fleet_days_after_resources = mean(fleet_days_after_resources, na.rm = TRUE),
      mean_utilization = mean(utilization, na.rm = TRUE),
      p90_utilization = as.numeric(quantile(utilization, 0.90, na.rm = TRUE)),
      .groups = "drop"
    )
}
