# recommendations.R
# -----------------------------------------------------------------------------
# V2.5 #1 Traceable Recommendations. Produces the spec's evidence panel:
#
#   Recommendation: Add 1 testing unit
#   Why:
#     * Testing unit P90 utilization = 99%
#     * Testing unit is the current campaign bottleneck (Critical)
#     * Expected P50 reduction = 255 days   (VERIFIED by re-simulation)
#     * Expected value = $X
#     * Confidence = 98%
#
# What makes it "traceable": the expected reduction is not asserted from an
# analytic proxy -- the function re-runs the Monte Carlo with +1 of the binding
# resource at the SAME seed (paired common random numbers) and measures the
# actual P50 reduction and win-rate. The schedule-fast engine makes this cheap.
#
# Dependencies (source first): simulation_engine[_fast].R, bottleneck_explain.R
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({ library(dplyr) })

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- Decision thresholds -----------------------------------------------------
# Single named source for every cutoff recommend_action() uses to turn a
# re-simulated (or analytic-fallback) schedule/cost comparison into a
# Recommended / Optional / Not justified verdict. Mirrors the pattern used by
# BAYES_DECISION_THRESHOLDS in bayesian_updater.R: nothing in the decision
# path below hardcodes a number outside this list, so the app's "Decision
# Rules" disclosure can never drift out of sync with the code that actually
# drives the recommendation.
REC_DECISION_THRESHOLDS <- list(
  min_p50_reduction_days = 0.5,   # below this, a "win" is noise, not a real schedule gain
  confidence_high_win_rate     = 0.90,  # win-rate >= this -> "High" confidence
  confidence_moderate_win_rate = 0.75,  # win-rate >= this -> "Moderate" confidence
  confidence_low_win_rate      = 0.60   # win-rate >= this -> "Low" confidence; below -> "Inconclusive"
)

.rec_conf_band <- function(p) {
  th <- REC_DECISION_THRESHOLDS
  dplyr::case_when(
    p >= th$confidence_high_win_rate     ~ "High",
    p >= th$confidence_moderate_win_rate ~ "Moderate",
    p >= th$confidence_low_win_rate      ~ "Low",
    TRUE                                  ~ "Inconclusive"
  )
}

# Maps from engine resource name -> simulate_campaign_detailed() arg + noun + rate.
.REC_ARG  <- c("Frac fleet"="frac_fleets","Wireline"="wireline_units",
               "CT / cleanout"="ct_units","Milling"="milling_units","Testing unit"="testing_units")
.REC_NOUN <- c("Frac fleet"="frac fleet","Wireline"="wireline unit","CT / cleanout"="CT unit",
               "Milling"="milling unit","Testing unit"="testing unit")

recommend_action <- function(
    sim_result,
    sim_args = NULL,                 # the named arg list that produced sim_result (req. for verify)
    verify = TRUE,
    frac_fleet_cost_per_day = 250000, wireline_cost_per_day = 15000,
    ct_cost_per_day = 25000, milling_cost_per_day = 18000, testing_unit_cost_per_day = 12000
) {
  stopifnot(is.list(sim_result), "summary" %in% names(sim_result))
  rates <- c("Frac fleet"=frac_fleet_cost_per_day, "Wireline"=wireline_cost_per_day,
             "CT / cleanout"=ct_cost_per_day, "Milling"=milling_cost_per_day,
             "Testing unit"=testing_unit_cost_per_day)

  ex   <- explain_bottlenecks(sim_result$summary, sim_result$resource_utilization)
  mode <- sim_args$operation_mode %||% ex$roles$operation_mode[1]
  role <- ex$roles[ex$roles$operation_mode == mode, ]
  rnk  <- ex$ranking[ex$ranking$operation_mode == mode, ]
  prim <- role$primary
  arg  <- .REC_ARG[[prim]]; noun <- .REC_NOUN[[prim]]

  base_summary <- sim_result$summary %>% filter(operation_mode == mode)
  base_p50 <- as.numeric(quantile(base_summary$estimated_campaign_days, 0.50, na.rm = TRUE))

  # Fleet spread rate ($/day) for this mode from resource_utilization.
  ru <- sim_result$resource_utilization %>% filter(operation_mode == mode)
  units_before <- ru %>% group_by(resource) %>% summarise(units = mean(units, na.rm = TRUE), .groups = "drop")
  spread_rate <- sum(units_before$units * unname(rates[units_before$resource]), na.rm = TRUE)

  can_verify <- isTRUE(verify) && !is.null(sim_args) &&
    exists("simulate_campaign_detailed", mode = "function")

  if (can_verify) {
    new_args <- sim_args
    new_args[[arg]] <- as.numeric(sim_args[[arg]] %||% 1) + 1
    new_res <- do.call(simulate_campaign_detailed, new_args)
    new_summary <- new_res$summary %>% filter(operation_mode == mode)

    paired <- inner_join(
      base_summary %>% select(simulation_id, base = estimated_campaign_days),
      new_summary  %>% select(simulation_id, new  = estimated_campaign_days),
      by = "simulation_id")
    reduction <- paired$base - paired$new
    p50_reduction  <- as.numeric(quantile(reduction, 0.50, na.rm = TRUE))
    mean_reduction <- mean(reduction, na.rm = TRUE)
    new_p50  <- as.numeric(quantile(new_summary$estimated_campaign_days, 0.50, na.rm = TRUE))
    delta_p50 <- base_p50 - new_p50
    win_rate <- mean(reduction > 0, na.rm = TRUE)
    basis <- sprintf("VERIFIED by re-simulation (+1 %s, n=%d paired)", noun, nrow(paired))
  } else {
    # Analytic fallback: cascade rank-1 contribution.
    p50_reduction  <- role$primary_delay_days
    mean_reduction <- role$primary_delay_days
    delta_p50 <- role$primary_delay_days
    new_p50  <- base_p50 - delta_p50
    win_rate <- as.numeric(rnk$prob_primary[rnk$resource == prim])
    basis <- "ESTIMATED from constraint cascade (not re-simulated)"
  }

  added_rate <- unname(rates[prim])
  ev_gross   <- delta_p50 * spread_rate            # value of compressing the whole fleet's calendar
  added_cost <- added_rate * new_p50               # extra unit on-hire over the shorter campaign
  ev_net     <- ev_gross - added_cost

  p90_util  <- as.numeric(rnk$p90_utilization[rnk$resource == prim])
  status    <- rnk$status[rnk$resource == prim]

  min_reduction <- REC_DECISION_THRESHOLDS$min_p50_reduction_days
  worthwhile <- ev_net > 0 && delta_p50 > min_reduction
  conf_band  <- .rec_conf_band(win_rate)

  # Three-way verdict: economic gate first, then statistical confidence.
  #   Not justified - the schedule saving doesn't cover the added unit, or
  #                   the P50 reduction doesn't clear min_p50_reduction_days.
  #   Optional      - net positive, but win-rate confidence is Low/Inconclusive.
  #   Recommended   - net positive and Moderate/High confidence.
  decision_status <- if (!worthwhile) {
    "Not justified"
  } else if (conf_band %in% c("Low", "Inconclusive")) {
    "Optional"
  } else {
    "Recommended"
  }

  decision_reason <- if (!worthwhile) {
    if (ev_net <= 0) {
      sprintf("Net value is %s: %.0f d saved x $%.0fk/d spread does not cover the added %s's standby cost.",
              .fmt_usd(ev_net), delta_p50, spread_rate / 1000, tolower(noun))
    } else {
      sprintf("P50 reduction of %.1f d does not clear the %.1f d minimum to count as a real schedule gain.",
              delta_p50, min_reduction)
    }
  } else if (decision_status == "Optional") {
    sprintf("Net value is positive (%s), but confidence is only %s (%.0f%% win rate) - below the %.0f%% bar for Recommended.",
            .fmt_usd(ev_net), conf_band, 100 * win_rate, 100 * REC_DECISION_THRESHOLDS$confidence_moderate_win_rate)
  } else {
    sprintf("Net value is positive (%s) with %s confidence (%.0f%% win rate). Both gates cleared.",
            .fmt_usd(ev_net), conf_band, 100 * win_rate)
  }

  action <- if (worthwhile) sprintf("Add 1 %s", noun)
            else sprintf("Hold current configuration (do not add a %s)", noun)

  why <- c(
    sprintf("%s P90 utilization = %.0f%%", prim, 100 * p90_util),
    sprintf("%s is the current campaign bottleneck (%s)", prim, status),
    sprintf("Expected P50 reduction = %.0f days  [%s]", p50_reduction, basis),
    sprintf("Expected value = %s  (= %.0f d saved x $%.0fk/d fleet spread - extra unit standby)",
            .fmt_usd(ev_net), delta_p50, spread_rate / 1000),
    sprintf("Confidence = %.0f%% (%s) - duration improves in %.0f%% of paired simulations",
            100 * win_rate, conf_band, 100 * win_rate)
  )
  if (!worthwhile) why <- c(why, "Net value is not positive: the schedule saving does not cover the added unit.")
  if (decision_status == "Optional") why <- c(why, "Net value is positive, but confidence is not yet high enough to call this Recommended.")

  panel <- paste0("Recommendation: ", action, "  [", decision_status, "]\nWhy:\n",
                  paste0("  * ", why, collapse = "\n"))

  list(
    operation_mode = mode,
    recommendation = action,
    worthwhile = worthwhile,
    decision_status = decision_status,
    decision_reason = decision_reason,
    bottleneck = prim,
    status = status,
    p90_utilization = p90_util,
    base_p50_days = base_p50,
    new_p50_days = new_p50,
    expected_reduction_days = p50_reduction,
    mean_reduction_days = mean_reduction,
    spread_rate_per_day = spread_rate,
    expected_value = ev_net,
    ev_gross = ev_gross,
    added_cost = added_cost,
    confidence = win_rate,
    confidence_band = .rec_conf_band(win_rate),
    basis = basis,
    why = why,
    panel = panel
  )
}

.fmt_usd <- function(x) {
  s <- if (x < 0) "-" else ""
  ax <- abs(x)
  if (ax >= 1e6) sprintf("%s$%.1fM", s, ax / 1e6)
  else if (ax >= 1e3) sprintf("%s$%.0fk", s, ax / 1e3)
  else sprintf("%s$%.0f", s, ax)
}

print_recommendation <- function(rec) { cat(rec$panel, "\n"); invisible(rec) }
