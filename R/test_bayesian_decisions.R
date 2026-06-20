# test_bayesian_decisions.R
# Property checks for the evidence-strength / decision engine added to
# bayesian_updater.R. Run: Rscript test_bayesian_decisions.R
suppressPackageStartupMessages({
  source("load_inputs.R")
  source("bayesian_updater.R")
})

HW   <- load_historical_wells("../data_templates/historical_wells_template.csv")
NW   <- load_historical_wells("../data_templates/new_campaign_wells_template.csv")
ASSU <- load_master_assumptions("../data_templates/master_risks_assumptions_template.csv")
ROBS <- load_risk_observations("../data_templates/risk_observations_template.csv")

br  <- run_bayesian_update(HW, NW, ASSU, ROBS, prior_strength = 20)
dur <- assess_duration_update(br$duration_update)
ru  <- assess_risk_update(br$risk_update)

ok <- TRUE
chk <- function(c, m) { cat(sprintf("  [%s] %s\n", if (isTRUE(c)) "PASS" else "FAIL", m)); ok <<- ok && isTRUE(c) }

row <- function(df, key_col, key) df[df[[key_col]] == key, ]

# -- Duration: both parameters stable with weak evidence (8 new wells, CI includes zero)
chk(all(dur$decision == "Retain assumption"), "duration: both parameters retain assumption (CI includes zero)")
chk(all(dur$evidence_strength == "Weak"), "duration: evidence is Weak with only 8 new wells")
chk(all(dur$recommendation_level == "Not justified"), "duration: recommendation level is Not justified")

# -- Risk: Screen out has the largest relative shift (~130%) on a decent sample (53 trials)
so <- row(ru, "risk_event", "Screen out")
chk(so$evidence_strength == "Moderate", "Screen out: evidence is Moderate (53 trials, not narrow/consistent enough for Strong)")
chk(so$magnitude_tier == "Significant", "Screen out: relative shift is Significant (>=100%)")
chk(so$decision == "Update assumption", "Screen out: decision is Update assumption")
chk(so$recommendation_level == "Recommended", "Screen out: recommendation level is Recommended")
chk(grepl("exceeded", so$narrative_observed), "Screen out: observed-frequency narrative says 'exceeded'")

# -- Risk: Plug pressure test failure has a smaller relative shift on the same sample size
pp <- row(ru, "risk_event", "Plug pressure test failure")
chk(pp$evidence_strength == "Moderate", "Plug pressure: evidence is Moderate")
chk(pp$magnitude_tier == "Indicative", "Plug pressure: relative shift is Indicative (50-100%)")
chk(pp$decision == "Monitor", "Plug pressure: decision is Monitor")
chk(pp$recommendation_level == "Optional", "Plug pressure: recommendation level is Optional")

# -- Risk: Wireline crew unavailable has only 1 trial -- weak evidence despite a large posterior shift
wl <- row(ru, "risk_event", "Wireline crew unavailable")
chk(wl$evidence_strength == "Weak", "Wireline: evidence is Weak (n_trials=1)")
chk(wl$decision == "Monitor", "Wireline: decision is Monitor (some signal, but too few trials)")
chk(wl$recommendation_level == "Not justified", "Wireline: recommendation level is Not justified despite Monitor decision")
chk(grepl("insufficient", wl$narrative_observed, ignore.case = TRUE),
    "Wireline: observed-frequency narrative flags insufficient trials, not a frequency claim")

# -- Risk: Weather delay has 0 observed events -- no signal at all
wd <- row(ru, "risk_event", "Weather delay")
chk(wd$evidence_strength == "Weak", "Weather delay: evidence is Weak (n_trials=1)")
chk(wd$decision == "No action", "Weather delay: decision is No action")
chk(wd$recommendation_level == "Not justified", "Weather delay: recommendation level is Not justified")
chk(grepl("Insufficient evidence", wd$narrative_interpretation),
    "Weather delay: interpretation states insufficient evidence, not a directional claim")

# -- Narrative must never assert "more frequent" off a Weak evidence base
weak_rows <- ru[ru$evidence_strength == "Weak", ]
chk(all(grepl("Insufficient evidence", weak_rows$narrative_interpretation)),
    "all Weak-evidence risk rows use insufficient-evidence wording, never a directional claim")

# -- Decision thresholds are named, single-sourced, and match the requested defaults
chk(BAYES_DECISION_THRESHOLDS$min_trials_for_update == 30, "MIN_TRIALS_FOR_UPDATE default is 30")
chk(BAYES_DECISION_THRESHOLDS$min_events_for_update == 3, "MIN_EVENTS_FOR_UPDATE default is 3")
chk(BAYES_DECISION_THRESHOLDS$min_posterior_shift_pp == 0.02, "MIN_POSTERIOR_SHIFT_PP default is 0.02 (2pp)")

# -- Decision Reason text is audit-ready and traceable to visible numbers
chk(so$decision_reason == "4 events in 53 trials. Posterior increased by +3.9 pp. Evidence moderate. Update threshold exceeded.",
    "Screen out: Decision Reason matches the exact numbers (4 events, 53 trials, +3.9pp)")
chk(grepl("^Posterior increased by \\+3\\.8 pp\\. Evidence moderate\\. Continue monitoring\\.$", pp$decision_reason),
    "Plug pressure: Decision Reason explains Monitor outcome")
chk(wl$decision_reason == "Only 1 event observed. Evidence weak despite large shift.",
    "Wireline: Decision Reason flags the single observed event despite the shift")
chk(wd$decision_reason == "Shift below threshold and evidence weak.",
    "Weather delay: Decision Reason cites the below-threshold shift")

# -- meets_update_gate is TRUE only for Screen out (the only row clearing all four gates at once)
chk(isTRUE(so$meets_update_gate), "Screen out clears the full Update-assumption gate")
chk(!isTRUE(pp$meets_update_gate), "Plug pressure does not clear the full gate (relative shift too small)")
chk(!isTRUE(wl$meets_update_gate), "Wireline does not clear the full gate (trials/events below minimum)")
chk(!isTRUE(wd$meets_update_gate), "Weather delay does not clear the full gate")

# -- Full narrative names the evidence, the threshold outcome, and the recommendation
chk(grepl("exceeds the update threshold", so$narrative_full), "Screen out: full narrative cites the update threshold")
chk(grepl("Recommendation: update assumption\\.$", so$narrative_full), "Screen out: full narrative ends with the recommendation")

# -- Duration: renamed middle tier is "Review assumption" (was "Monitor" in an earlier revision)
chk(!any(dur$decision == "Monitor"), "duration decisions never use the risk-only label 'Monitor'")
chk(all(dur$decision %in% c("Retain assumption", "Review assumption", "Update assumption")),
    "duration decisions are one of the three documented labels")

# -- scope is attached to every risk_update row, and drives the chart/table
# split into stage/well/campaign sections.
chk(all(!is.na(ru$scope)), "every risk_update row has a non-NA scope")
chk(so$scope == "stage", "Screen out: scope is stage")
chk(pp$scope == "stage", "Plug pressure test failure: scope is stage")
chk(wl$scope == "campaign", "Wireline crew unavailable: scope is campaign")
chk(wd$scope == "campaign", "Weather delay: scope is campaign")

# -- sample_caveat explains the prior-anchoring effect (issue: "why did the
# probability move if [almost] nothing was observed") -- present only for
# Weak-evidence rows, absent for Moderate/Strong.
chk(is.na(so$sample_caveat), "Screen out (Moderate evidence): no sample_caveat")
chk(is.na(pp$sample_caveat), "Plug pressure test failure (Moderate evidence): no sample_caveat")
chk(!is.na(wl$sample_caveat) && grepl("only 1 campaign", wl$sample_caveat),
    "Wireline crew unavailable (Weak evidence): sample_caveat names the 1-campaign sample")
chk(!is.na(wd$sample_caveat) && grepl("only 1 campaign", wd$sample_caveat),
    "Weather delay (Weak evidence): sample_caveat names the 1-campaign sample")

# -- .scope_unit_label() pluralizes correctly for all 3 scopes, 1 vs N.
chk(.scope_unit_label("stage", 1) == "1 stage", ".scope_unit_label: singular stage")
chk(.scope_unit_label("stage", 53) == "53 stages", ".scope_unit_label: plural stages")
chk(.scope_unit_label("well", 1) == "1 well", ".scope_unit_label: singular well")
chk(.scope_unit_label("well", 4) == "4 wells", ".scope_unit_label: plural wells")
chk(.scope_unit_label("campaign", 1) == "1 campaign", ".scope_unit_label: singular campaign")
chk(.scope_unit_label("campaign", 3) == "3 campaigns", ".scope_unit_label: plural campaigns")


# -- Unmatched risk_event names must never look like a normal decision: the
# fuzzy-match in bayesian_update_risks() falls back to a fabricated 0.05
# prior, and that must be surfaced, not silently presented as real.
ROBS_BAD <- dplyr::bind_rows(ROBS, tibble::tibble(
  risk_event = "Frobnicator failure", n_trials = 40, n_events = 10
))
br_bad <- run_bayesian_update(HW, NW, ASSU, ROBS_BAD, prior_strength = 20)
ru_bad <- assess_risk_update(br_bad$risk_update)
unmatched <- row(ru_bad, "risk_event", "Frobnicator failure")

chk(nrow(unmatched) == 1, "unmatched risk_event still produces one output row")
chk(isFALSE(unmatched$matched), "'Frobnicator failure' is flagged matched = FALSE")
chk(unmatched$decision == "No assumption match", "unmatched row gets the distinct 'No assumption match' decision")
chk(unmatched$recommendation_level == "Not justified", "unmatched row is never 'Recommended' or 'Optional'")
chk(grepl("fabricated", unmatched$decision_reason), "decision_reason names the prior as fabricated")
chk(grepl("no matching risk", unmatched$narrative_full, ignore.case = TRUE),
    "narrative_full states no matching risk was found, not a normal posterior-shift story")

# -- A real match in the same batch is unaffected by the unmatched row.
so_bad <- row(ru_bad, "risk_event", "Screen out")
chk(isTRUE(so_bad$matched), "matched rows in the same batch keep matched = TRUE")
chk(so_bad$decision == "Update assumption", "matched row's decision is unaffected by an unmatched row elsewhere in the batch")

# -- sample_caveat must not claim an unmatched row's fabricated prior is a
# real, merely-under-sampled assumption (the "stays close to the prior"
# framing used for genuine Weak-evidence rows). plot_bayesian_risk_update()
# renders sample_caveat directly as an in-panel annotation whenever it is
# non-NA, so this is also what keeps an unmatched risk from appearing in the
# chart with no warning at all.
chk(!is.na(unmatched$sample_caveat), "unmatched row gets a non-NA sample_caveat (so the plot annotation fires)")
chk(grepl("fabricated", unmatched$sample_caveat), "unmatched row's sample_caveat names the prior as fabricated, not merely under-sampled")
chk(!grepl("remains close to the prior", unmatched$sample_caveat),
    "unmatched row's sample_caveat does not use the genuine-Weak-evidence 'stays close to prior' framing")

# -- The fuzzy-name match must use literal substring matching, not regex: a
# risk name containing a regex metacharacter (e.g. an unbalanced paren) must
# not crash the match -- a crash here is caught by run_bayesian_update()'s
# tryCatch and silently nulls out EVERY risk in the same upload, not just the
# offending row.
ROBS_REGEX <- dplyr::bind_rows(ROBS, tibble::tibble(
  risk_event = "Plug failure (early", n_trials = 30, n_events = 5
))
ASSU_REGEX <- dplyr::bind_rows(ASSU, tibble::tibble(
  category = "Plug", variable = "Plug failure (early", type = "risk",
  probability = 0.05, min_days = 0.3, most_likely_days = 0.8, max_days = 2.0,
  simulation_impact = "replacement plug", scope = "well"
))
br_regex <- tryCatch(
  run_bayesian_update(HW, NW, ASSU_REGEX, ROBS_REGEX, prior_strength = 20),
  error = function(e) e
)
chk(!inherits(br_regex, "error"), "a risk name containing an unbalanced regex metacharacter does not crash the Bayesian update")
chk(!is.null(br_regex$risk_update) && "Plug failure (early" %in% br_regex$risk_update$risk_event,
    "the regex-metacharacter risk name still produces a result row")
chk(!is.null(br_regex$risk_update) && nrow(br_regex$risk_update) == nrow(ROBS_REGEX),
    "other risks in the same upload are unaffected by the metacharacter name (not all silently nulled out)")

cat(sprintf("\n==== %s ====\n", if (ok) "ALL PROPERTY CHECKS PASS" else "FAILURES ABOVE"))
if (!ok) quit(status = 1)
