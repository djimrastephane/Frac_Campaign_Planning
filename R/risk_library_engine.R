# risk_library_engine.R
# Wires an uploaded/edited risk_consequence_library into the risk_table used
# by simulate_campaign_detailed(). Two jobs:
#   1. Risks present ONLY in the library (no row in master_risks_assumptions.csv)
#      become real occurrence rows, using the library's own base_probability
#      + scope (reusing the existing scope-adjustment logic unchanged).
#   2. Risks present in BOTH files keep master_risks_assumptions.csv as the
#      probability/scope authority; the library only supplies severity tiers
#      + consequence magnitudes for those (applied in draw_risks_on_grid()).
#
# Matching is exact normalised-text equality only (no fuzzy/substring
# matching) to avoid false positives between superficially similar risk names.

normalise_risk_key <- function(x) normalise_text(x)

# Applies risk_multiplier as a *frequency* scalar (it never touches consequence
# severity/workload). The multiplier always scales the base, per-occurrence
# probability first; for stage-scope risks that means the per-stage probability
# is scaled BEFORE compounding across stages, not after. Scaling the already-
# compounded per-well probability (the old behaviour) over-amplifies stage-scope
# risks, since compounding is non-linear.
#   stage    : p_stage_adj = min(probability * risk_multiplier, 1)
#              adjusted    = 1 - (1 - p_stage_adj)^n_stages
#   well     : adjusted    = min(probability * risk_multiplier, 1)
#   campaign : adjusted    = min(probability * risk_multiplier, 1)  (single Bernoulli draw)
# NA probability -> 0.
compute_adjusted_risk_probability <- function(probability, scope, risk_multiplier, n_stages) {
  probability <- as.numeric(probability)
  p_adj <- pmin(probability * risk_multiplier, 1)
  adjusted <- ifelse(scope == "stage", 1 - (1 - p_adj)^n_stages, p_adj)
  ifelse(is.na(adjusted), 0, adjusted)
}

# Normalised scope vector for a data.frame's `scope` column: missing column,
# NA, or blank all default to "well". Shared by build_risk_table() below and
# engine_core.R's assumptions_used construction, so the audit
# column and the probability actually used to draw risk occurrences can
# never derive scope two different ways.
resolve_risk_scope <- function(df) {
  s <- if ("scope" %in% names(df)) normalise_text(df$scope) else rep("well", nrow(df))
  ifelse(is.na(s) | s == "", "well", s)
}

# Returns list(occurrence, lib_wide):
#   occurrence : one row per risk_name (key, risk_name, category, scope,
#                base_probability, affected_resource)
#   lib_wide   : one row per risk_name key, with cum_minor/cum_moderate
#                (cumulative scenario_probability thresholds for vectorised
#                severity-tier sampling) and, for each of the 7 consequence
#                fields, three columns suffixed _minor/_moderate/_major.
prepare_risk_library <- function(risk_library_df) {
  df <- risk_library_df %>%
    dplyr::mutate(key = normalise_risk_key(risk_name))

  occurrence <- df %>%
    dplyr::distinct(key, risk_name, category, scope, base_probability, affected_resource)

  fields <- c("scenario_probability", "wireline_runs", "ct_days", "milling_plugs",
              "testing_days", "pump_days", "extra_stages", "logistics_days")

  lib_wide <- df %>%
    dplyr::mutate(severity = tolower(severity)) %>%
    dplyr::select(key, severity, dplyr::all_of(fields)) %>%
    tidyr::pivot_wider(
      names_from = severity,
      values_from = dplyr::all_of(fields),
      names_glue = "{.value}_{severity}"
    ) %>%
    dplyr::mutate(
      cum_minor = scenario_probability_minor,
      cum_moderate = scenario_probability_minor + scenario_probability_moderate
    )

  list(occurrence = occurrence, lib_wide = lib_wide)
}

# Builds the risk_table used by draw_risks_on_grid(), unioning in any
# library-only risks as synthetic occurrence rows. When risk_library is NULL,
# behaviour is identical to the original inline construction it replaces.
build_risk_table <- function(assumptions, base_stages, risk_multiplier, risk_library = NULL) {
  risk_rows <- assumptions %>% dplyr::filter(normalise_text(type) == "risk")

  lib_wide <- NULL
  if (!is.null(risk_library) && nrow(risk_library) > 0) {
    lib <- prepare_risk_library(risk_library)
    lib_wide <- lib$lib_wide

    assumption_keys <- normalise_risk_key(risk_rows$variable)
    new_keys <- setdiff(lib$occurrence$key, assumption_keys)
    if (length(new_keys) > 0) {
      synth <- lib$occurrence %>%
        dplyr::filter(key %in% new_keys) %>%
        dplyr::transmute(
          category = category,
          variable = risk_name,
          type = "risk",
          probability = base_probability,
          min_days = 0, most_likely_days = 0, max_days = 0,
          simulation_impact = "From risk_consequence_library",
          scope = scope,
          extra_wireline_runs = NA_real_, extra_ct_days = NA_real_,
          extra_milling_plugs = NA_real_, extra_testing_days = NA_real_,
          extra_frac_days = NA_real_
        )
      risk_rows <- dplyr::bind_rows(risk_rows, synth)
    }
  }

  risk_table <- risk_rows %>%
    dplyr::mutate(
      .scope = resolve_risk_scope(.),
      adjusted_probability = compute_adjusted_risk_probability(probability, .scope, risk_multiplier, base_stages),
      is_campaign_scope = .scope == "campaign",
      adds_plug = !is.na(simulation_impact) & stringr::str_detect(normalise_text(simulation_impact), "plug"),
      adds_stage = !is.na(simulation_impact) & stringr::str_detect(normalise_text(simulation_impact), "extra stage|additional stage|re-frac|refrac|lost stage|screen out"),
      resource_class = risk_resource_class(category, variable),
      risk_event = as.character(variable)
    ) %>%
    dplyr::select(-.scope) %>%
    derive_risk_consequences()

  risk_table$lib_key <- if (!is.null(lib_wide)) {
    key <- normalise_risk_key(risk_table$risk_event)
    ifelse(key %in% lib_wide$key, key, NA_character_)
  } else {
    NA_character_
  }

  list(table = risk_table, lib_wide = lib_wide)
}
