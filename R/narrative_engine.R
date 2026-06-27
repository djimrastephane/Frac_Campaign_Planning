# narrative_engine.R
# -----------------------------------------------------------------------------
# V2.5 #12 Decision Narrative Engine. Stitches the foundation-layer outputs
# (#1 recommendation, #2 bottleneck roles, #6 uncertainty) into a single
# management-readable paragraph -- no new math, pure assembly.
#
# Example output:
#   "Recommended execution is Zipper mode with 1 frac fleet, 1 wireline unit,
#    1 CT unit, 2 milling units, 1 testing unit and 2 frac trees. The campaign
#    is expected to finish in 419 days (P50), within a P10-P90 range of
#    405-435 days, approximately 6 days faster than conventional execution.
#    Testing unit is the primary constraint at 99% P90 utilization; no other
#    resource materially constrains the campaign once it is relieved. Adding 1
#    testing unit is expected to reduce P50 duration by ~192 days and generate
#    ~$62.1M of schedule value (confidence 100%). There is a 0% probability of
#    finishing by the 170-day target."
#
# Dependencies (source first): simulation_engine[_fast].R, bottleneck_explain.R,
# recommendations.R, risk_uncertainty.R
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({ library(dplyr) })

`%||%` <- function(a, b) if (is.null(a)) b else a

.nf_usd <- function(x) {
  s <- if (is.na(x)) "" else if (x < 0) "-" else ""; ax <- abs(x)
  if (is.na(x)) "n/a"
  else if (ax >= 1e6) sprintf("%s$%.1fM", s, ax / 1e6)
  else if (ax >= 1e3) sprintf("%s$%.0fk", s, ax / 1e3)
  else sprintf("%s$%.0f", s, ax)
}
.cap1 <- function(s) { substr(s, 1, 1) <- toupper(substr(s, 1, 1)); s }

.config_phrase <- function(a) {
  parts <- c(
    sprintf("%d frac fleet%s", a$frac_fleets %||% 1, if ((a$frac_fleets %||% 1) != 1) "s" else ""),
    sprintf("%d wireline unit%s", a$wireline_units %||% 1, if ((a$wireline_units %||% 1) != 1) "s" else ""),
    sprintf("%d CT unit%s", a$ct_units %||% 1, if ((a$ct_units %||% 1) != 1) "s" else ""),
    sprintf("%d milling unit%s", a$milling_units %||% 1, if ((a$milling_units %||% 1) != 1) "s" else ""),
    sprintf("%d testing unit%s", a$testing_units %||% 1, if ((a$testing_units %||% 1) != 1) "s" else ""),
    sprintf("%d frac tree%s", a$frac_trees %||% 1, if ((a$frac_trees %||% 1) != 1) "s" else "")
  )
  paste0(paste(parts[-length(parts)], collapse = ", "), " and ", parts[length(parts)])
}

generate_narrative <- function(sim_result, sim_args = NULL, target_days = NULL, budget = NULL,
                               rec = NULL) {
  stopifnot(is.list(sim_result), "summary" %in% names(sim_result))
  mode <- sim_args$operation_mode %||% sim_result$summary$operation_mode[1]

  unc  <- quantify_uncertainty(sim_result$summary, sim_result$resource_utilization,
                               target_days = target_days, budget = budget)
  um   <- unc[unc$operation_mode == mode, ]
  ex   <- explain_bottlenecks(sim_result$summary, sim_result$resource_utilization)
  role <- ex$roles[ex$roles$operation_mode == mode, ]
  if (is.null(rec)) rec <- recommend_action(sim_result, sim_args = sim_args)

  # Mode comparison (if both modes present).
  modes_p50 <- sim_result$summary %>%
    group_by(operation_mode) %>%
    summarise(p50 = as.numeric(quantile(estimated_campaign_days, 0.50, na.rm = TRUE)), .groups = "drop")
  compare <- ""
  if (nrow(modes_p50) >= 2) {
    others <- modes_p50[modes_p50$operation_mode != mode, ]
    o <- others[which.max(others$p50), ]
    delta <- o$p50 - um$p50_days
    compare <- sprintf(", approximately %.0f days %s than %s execution",
                       abs(delta), if (delta >= 0) "faster" else "slower", tolower(o$operation_mode))
  }

  # Config + duration.
  cfg <- if (!is.null(sim_args)) .config_phrase(sim_args) else "the current configuration"
  duration <- sprintf("%.0f days (P50), within a P10-P90 range of %.0f-%.0f days",
                      um$p50_days, um$p10_days, um$p90_days)

  # Constraint sentence.
  bn <- sprintf("%s is the primary constraint at %.0f%% P90 utilization",
                role$primary, 100 * role$primary_p90_util)
  sec <- if (isTRUE(role$secondary_material))
    sprintf("; %s becomes the next constraint once it is relieved", role$secondary)
  else "; no other resource materially constrains the campaign once it is relieved"

  # Recommendation sentence.
  rs <- if (isTRUE(rec$worthwhile))
    sprintf("%s is expected to reduce P50 duration by ~%.0f days and generate ~%s of schedule value (confidence %.0f%%)",
            .cap1(tolower(rec$recommendation)), rec$expected_reduction_days,
            .nf_usd(rec$expected_value), 100 * rec$confidence)
  else
    sprintf("Adding one %s is estimated to save ~%.0f days, but does not generate sufficient net value at current rates (%s); the current configuration is preferred.",
            tolower(rec$bottleneck), rec$expected_reduction_days, .nf_usd(rec$expected_value))

  # Target / budget odds.
  tgt <- if (!is.null(target_days) && !is.na(um$prob_finish_by_target))
    sprintf("There is a %.0f%% probability of finishing by the %d-day target",
            100 * um$prob_finish_by_target, as.integer(target_days)) else ""
  bud <- if (!is.null(budget) && !is.na(um$prob_within_budget))
    sprintf("%s %.0f%% probability of staying under the %s budget",
            if (nzchar(tgt)) " and a" else "There is a",
            100 * um$prob_within_budget, .nf_usd(budget)) else ""
  odds <- paste0(tgt, bud); if (nzchar(odds)) odds <- paste0(" ", odds, ".")

  paragraph <- sprintf(
    "Recommended execution is %s mode with %s. The campaign is expected to finish in %s%s. %s%s. %s.%s",
    mode, cfg, duration, compare, bn, sec, rs, odds)

  list(
    narrative = paragraph,
    mode = mode,
    recommendation = rec$recommendation,
    components = list(uncertainty = um, roles = role, rec = rec)
  )
}

print_narrative <- function(n) { cat(n$narrative, "\n"); invisible(n) }
