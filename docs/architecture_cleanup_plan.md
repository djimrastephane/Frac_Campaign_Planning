# Architecture Cleanup Plan

Status: **proposal, no code moved yet.** This document is the scaffold for the
next phase of work after the `v16-stable-checkpoint` tag. It exists so the
mechanical refactor can be reviewed and sequenced *before* anyone starts
moving code, not after.

Baseline: commit `67b6a1c` / tag `v16-stable-checkpoint`. At that point:
`R/simulation_engine_fast.R` is 4,114 lines, `app/app.R` is 4,079 lines,
`check_regression.R` and all 15 `R/test_*.R` scripts pass (see the
[Current Validation Status](../README.md#current-validation-status) table).

## Why this, and why now

Both files mix concerns that don't need to live together:
`simulation_engine_fast.R` interleaves the Monte Carlo engine itself with
~25 `summarise_*()`/`build_*()` presentation/report builders and a ~560-line
PDF renderer; `app.R` interleaves UI markup, reactive wiring, and inline
helper functions for all 12 tabs in one file. Neither is broken, but both
are hard to navigate and change safely — a two-line label fix earlier in
this project (the zipper-efficiency display bug) required reading through
the whole engine file to find the one function responsible. Splitting along
existing seams (no new abstractions, just relocated code) fixes that without
touching behavior.

## Non-goals

- No simulation logic changes. Every function moves with its body byte-for-byte.
- No new abstractions, interfaces, or generic frameworks introduced "for
  future flexibility" — files are split along boundaries that already exist
  in the code today.
- No dependency version changes.
- Not a rewrite of `app.R`'s reactive graph — module extraction preserves
  the existing `input$`/`output$`/`reactive()` wiring, just relocates it.

## Target module layout

### `R/simulation_engine_fast.R` (4,114 lines) splits into:

| New file | Contents | Rationale |
|---|---|---|
| `R/engine_core.R` | `normalise_text`, `triangle_sample`, `strict_as_numeric`, `build_param_cache` + `cache_lookup`/`sample_*_cached` family, `build_pad_assignment[_cached]`, `risk_resource_class`, `derive_risk_consequences`, `build_risk_grid`, `empty_risk_event_log`, `empty_sums_matrix`, `draw_risks_on_grid`, `synthetic_historical_wells`, `schedule_pre_frac`, `schedule_post_frac_milling`, `load_workflow_config`, `workflow_resource_phases`, `summarise_workflow`, `simulate_campaign_detailed`, `simulate_one_campaign` | The actual Monte Carlo engine: sampling, scheduling, risk-grid mechanics, and the top-level `simulate_campaign_detailed()` entry point. Nothing here should ever need `dplyr::summarise`-style presentation logic. |
| `R/summaries.R` | `summarise_delay_contributors`, `summarise_simulation`, `summarise_resource_utilization`, `summarise_wireline_constraint`, `summarise_bottlenecks`, `summarise_stage_level_risks`, `build_traffic_lights`, `build_readiness_score`, `build_cost_impact`, `build_total_campaign_cost`, `build_recommendation_confidence`, `build_resource_recommendations`, `build_executive_kpis`, `build_executive_summary`, `build_resource_timeline`, `build_investment_ranking`, `build_bottleneck_narrative`, `summarise_risk_consequences` | Pure post-processing of a completed simulation result into UI-ready tables. Depends on `engine_core.R`'s output shape only, never on its internals. |
| `R/report_pdf.R` | `build_management_report`, `build_management_report_pdf` | Isolates the `gridExtra`/PDF-device-heavy report renderer (~560 lines) from the engine so `engine_core.R`/`summaries.R` have no plotting-device side effects. |
| `R/optimiser_cascade.R` | `optimise_campaign_scenarios`, `analyse_constraint_cascade` | Multi-run orchestration (grid search, greedy cascade) that calls `simulate_campaign_detailed()` repeatedly — a distinct concern from either running one simulation or summarising its result. |

`R/optimiser_parallel.R` (already a separate file) is unaffected; it depends
on `engine_core.R`'s `simulate_campaign_detailed()` the same way it depends
on `simulation_engine_fast.R` today — only the `source()` line in `app.R`
and `check_regression.R` needs to change to source 4 files instead of 1, in
the same order (`engine_core.R` before the other three, since they call into
it).

### `app/app.R` (4,079 lines) splits into (later step, larger risk):

Deferred to a second PR after the engine split lands and is proven stable.
Proposed direction only, not committed to:

- One `R/modules/mod_<tab>.R` pair (`<tab>UI()` / `<tab>Server()`) per
  `nav_panel()` — Overview, Historical Learning, Decision Support,
  Sensitivity, Bayesian Update, What-If, Scenario Library, Risks, Resources,
  Wireline & Readiness, Optimiser, Workflow, Risk Editor, Audit & Data.
- Shared reactives that cross tab boundaries (`sim_results`, `focus_mode_r`,
  `rec_v2_r`, `robustness_rv`, etc.) stay in the top-level `server()` and are
  passed into module servers as reactive arguments — this is the standard
  Shiny-modules pattern and requires no change to reactive semantics, only
  to where the code physically lives.
- `app.R` itself shrinks to: package loads, `source()` calls, the top-level
  `ui <- page_sidebar(...)` assembling module UIs, and `server <- function(...)`
  wiring shared reactives + calling module servers.

## Sequencing and safety

1. **Step 1 (this PR): plan only.** No code moves. Reviewed and merged as
   the scaffold.
2. **Step 2 (separate PR): engine split.** Move functions verbatim into the
   4 new files per the table above. Update `source()` order in `app.R`,
   `check_regression.R`, `check_scheduling_modes.R`, and every `R/test_*.R`
   script that sources `simulation_engine_fast.R` directly. Run
   `check_regression.R` (must stay bit-identical) and all 15 `test_*.R`
   scripts after every file move, not just at the end — moving a function
   before a helper it calls is the most likely failure mode, and per-move
   verification catches it immediately instead of after the fact.
3. **Step 3 (separate PR, only after Step 2 is merged and stable): `app.R`
   modularization.** Higher risk — reactive-graph wiring is easy to get
   subtly wrong (e.g. a module losing access to a reactive it needs, or a
   `req()`/`observeEvent()` firing in the wrong scope). Needs its own Phase-0
   regression review before starting, per this project's established
   practice for anything touching `app.R`'s reactive graph.

## Regression guardrails already in place

- `R/check_regression.R` — proves `simulate_campaign_detailed()` and
  `optimise_campaign_scenarios()` stay bit-identical to the archived
  original engine. Must still source whatever files the split produces, in
  dependency order, and still pass after Step 2.
- `R/test_*.R` (15 scripts) — property checks across recommendations,
  Bayesian decisions, scheduling, risk multipliers, etc. None hardcode which
  file a function lives in (they `source()` by filename), so Step 2 only
  requires updating those `source()` lines, not the assertions themselves.
- `.github/workflows/` — already runs both of the above on every push/PR;
  no CI changes needed beyond what Step 2's `source()` updates require.

## Explicitly out of scope for this cleanup

- Renaming any public function (`simulate_campaign_detailed`,
  `recommend_action`, etc.) — external callers (test scripts, `app.R`,
  `check_regression.R`) reference these by name today and should keep doing so.
- Changing the `R/archive/` reference engine used by `check_regression.R` as
  the oracle.
- Any of the Phase 1/2 items from the production-readiness audit
  (dependency pinning, zipper-label fix, KS wording, CSV sanitization,
  day-rate constants) — those already shipped before this checkpoint.
