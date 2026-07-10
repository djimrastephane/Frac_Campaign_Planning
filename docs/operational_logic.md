# Operational Logic

*Deep reference, moved out of the [README](../README.md) to keep it orientation-focused. See also the [subsystem architecture diagrams](architecture.md).*

This document covers the modelled equipment relationships and operation sequence so the logic can be verified against, and adapted to, your own operations.

## Equipment Relationship Map

Key dependencies as modelled:

- **Wireline gates frac.** A stage cannot pump until wireline has perforated and set the plug. In zipper mode, if wireline workload per well exceeds frac workload per well, the frac fleet waits (`wireline_readiness_delay_days`) and the idle cost is reported.
- **CT cleanout runs in parallel with frac (conventional).** CT preps well N+1 during well N's frac execution. CT only gates the campaign if it becomes the pacing resource (i.e. CT workload per well > frac+wireline workload per well). See Conventional Frac Execution Logic below.
- **Cement evaluation runs offline when a spare wireline unit exists.** If `wireline_units >= 2`, cement evaluation is always run offline (spare unit available while primary unit perforates). With a single wireline unit, cement evaluation offline probability is set in the assumptions CSV (default 80%).
- **Frac trees gate zipper.** Zipper requires 2 trees minimum. With exactly 2, each inter-well transition incurs a swap delay; a 3rd tree reduces transition waiting (~5%), 4+ slightly more (~10%, diminishing).
- **Milling follows frac** and is scheduled discretely. Milling cannot start until a well is fully fraced AND a milling unit AND testing unit are both free. Wells are scheduled in frac-release order; later wells can begin milling while earlier wells are still in flowback.
- **CT / cleanout is separate from milling.** CT cleanout is pre-frac well intervention. Milling is post-frac plug drill-out on a dedicated milling spread. These are tracked as separate resources with separate utilization. Use "Allow CT to support milling" only if your CT unit genuinely does plug drill-outs.
- **Testing follows milling** per well. Each well's flowback + testing window starts when its milling completes AND a testing unit is free. The testing unit holds the resource during both milling (test confirmation) and flowback (pressure monitoring).

---

## Conventional Frac Execution Logic

```
CAMPAIGN PACING: Sequential well-by-well. One frac fleet, one wireline unit.

For each well (N = 1 to 30):
─────────────────────────────────────────────────────────────────────────
PRE-FRAC (CT / cleanout — runs in parallel with previous well's frac):
  CT cleanout / scraper run        ~0.5 d  (from assumptions CSV)
  Cement evaluation                ~1.0 d  (if running online; else 0)
  Note: CT preps well N during well N-1's frac execution.
        CT only delays campaign if ct_workload > (frac + wireline) per well.

FRAC STAGE LOOP (per stage, wireline then frac, sequential):
  Wireline: perforate stage N      time/stage (sidebar)
  Wireline: set isolation plug     isolation plug duration (CSV)
  [Optional: temperature log]      if this stage is a log stage
  Frac fleet: pump stage N         frac_time_per_stage (sidebar)
  Settling time                    settling hours (sidebar)
  → Repeat for all stages

POST-FRAC (milling + testing — run in parallel with ongoing frac on other wells):
  Milling unit: drill out plugs    plugs × milling_days_per_plug (historical)
  Testing unit: occupied during milling  (resource held, not additional time)
  Testing unit: flowback + well test     7–10 days (configurable, per well)
─────────────────────────────────────────────────────────────────────────

CAMPAIGN DURATION:
  Frac path  = Σ max(ct_per_well, frac_per_well + wireline_per_well) over all wells
  Post-frac  = discrete scheduler: wells flow into milling/testing queue
               as they are released; last well's flowback_finish = campaign end
  Campaign   = max(frac_path, post_frac_completion_day)

WHAT THIS MEANS IN PRACTICE:
  With typical values (frac 12h/stage, wireline 6h/stage, 6 stages/well):
    frac+wireline ≈ 4.5 d/well  >  ct_workload ≈ 1.6 d/well
    → CT completes within frac window; does NOT add to campaign time
    → Campaign paced by (frac + wireline) sequential path ≈ 4.5 d/well × 30 = 135 d
  Post-frac queue (milling + testing) runs concurrently; with 3+ testing units
  it clears before or alongside the frac path.
```

---

## Zipper Frac Execution Logic

```
CAMPAIGN PACING: Alternating wells on two simultaneous fracs.
Requires: ≥ 2 frac trees, ≥ 2 wells on pad (or adjacent pads).

For each pair of wells (A, B alternating):
─────────────────────────────────────────────────────────────────────────
PRE-FRAC (overlapped — both wells prepped before first stage):
  CT cleanout: Well A              ~0.5 d
  CT cleanout: Well B              ~0.5 d  (CT moves after Well A)
  Cement evaluation: well A + B (if online)
  Wireline: perforate Well A Stage 1

FRAC STAGE LOOP (alternating wells):
  ┌────────────────────────────────────────────────────────────┐
  │ Well A: FRAC Stage N              While: Wireline preps B  │
  │ Well B: FRAC Stage N              While: Wireline preps A  │
  │ → Frac fleet moves A → B → A, wireline stays 1 stage ahead│
  └────────────────────────────────────────────────────────────┘
  Frac fleet idle if wireline not ready → wireline_readiness_delay_days

  Tree swap: each A→B transition incurs swap_delay
    2 trees: full swap delay (sidebar: frac_tree_swap_delay_hours, default 4h)
    3 trees: 5% reduction in effective execution time
    4+ trees: 10% reduction (diminishing)

  Zipper execution factor applied to frac workload (default 0.75)
  → frac_execution = base_frac × 0.75 (25% faster than conventional)
─────────────────────────────────────────────────────────────────────────

CAMPAIGN DURATION:
  Per well:   frac_related = ct_fleet_days + max(frac_fleet_days, wireline_fleet_days)
              (wireline and frac run in parallel; whichever is slower governs)
  Frac path  = Σ frac_related over all wells
  Post-frac  = same discrete scheduler as conventional
  Campaign   = max(frac_path, post_frac_completion_day)

WHY ZIPPER IS FASTER:
  Conventional per well: frac + wireline in sequence  ≈ 4.5 d
  Zipper per well:       max(frac × 0.75, wireline)  ≈ max(3.4, 3.6) ≈ 3.6 d
  Campaign saving:       (4.5 - 3.6) × 30 = ~27 d (frac path alone)
  Additional saving:     post-frac queue shorter because wells release earlier
  Total P50 saving: typically 60–120 days for a 30-well campaign

WIRELINE CONSTRAINT:
  If wireline_days_per_well > frac_days_per_well × 0.75:
    Frac fleet waits on wireline → idle cost reported
  If wireline_days_per_well < frac_days_per_well × 0.75:
    Wireline waits between wells → no frac idle cost

FRAC TREE CONSTRAINT:
  2 trees: every A→B transition costs swap_delay_hours ÷ 24 per well
  3 trees: spare tree pre-positioned; swap delay reduced ~5%
  4+ trees: further reduction ~10% (diminishing returns)
```

---

## Risk Consequence Propagation

Risks do not just add delay days. Each technical risk cascades into induced resource workload:

Default consequence library (per occurred event, overridable per-risk via CSV):

| Risk event | Scope | Wireline runs | CT days | Milling plugs | Testing days | Pump days |
|---|---|---|---|---|---|---|
| Screenout | stage | 1 | 0.50 | - | - | 0.25 (+ extra stage) |
| Plug pressure test failure | stage | 1 | - | - | 0.15 (+ plug) | - |
| Premature plug set | stage | - | 0.25 | - | 0.30 | - |
| Perforation / gun misfire | stage | 1 | - | - | - | - |
| Isolation plug failure | stage | 1 | 0.50 | 1 | 0.25 | - |
| UPCT failure | stage | 1 | 0.25 | - | - | - |
| Cement in casing | stage | - | 1.00 | - | - | - |
| Cement above plug | stage | - | 0.50 | - | - | - |
| Wireline crew unavailable | campaign | - | - | - | - | - |
| CT unit unavailable | campaign | - | - | - | - | - |
| Weather / permit / access | campaign | - | - | - | - | - |

**Risk scope** controls how probability is applied:
- `stage`: probability per stage. Effective per-well probability = `1 - (1-p)^N_stages`. Use for events that can occur on any individual stage (screenout, gun misfire).
- `well`: independent probability per well. Use for well-level events (surface equipment failure).
- `campaign`: single Bernoulli draw for the whole campaign. Use for crew absences, weather, permits — events that affect the whole operation, not each well independently. This prevents the structural error of treating a crew walkout as 30 independent per-well events.

**Risk frequency multiplier** (Scenario sidebar, 0.25–3.0, default 1.0): a global scalar that scales how often risk events occur. It does not change consequence severity (days/resource impact once a risk fires) — only its probability of firing. The multiplier is applied to the *base* probability first, then scope conversion happens on top of the scaled value:
- `stage`: scale per-stage probability first, then compound across stages — `p_stage_adj = min(p × multiplier, 1)`, `adjusted = 1 - (1 - p_stage_adj)^n_stages`.
- `well` / `campaign`: scale directly — `adjusted = min(p × multiplier, 1)`.

Scaling before compounding matters: scaling the already-compounded per-well probability instead would over-amplify stage-scope risks (e.g. screenout), since compounding is non-linear. Use the multiplier to stress-test a single what-if scenario (e.g. 2x for a higher-risk pad, 0.5x for a mature/low-risk area) without editing the assumptions CSV. This is distinct from the Sensitivity Analysis Engine's ±50% OAT sweep, which perturbs each risk probability one at a time to rank sensitivity rather than scale all risks together for a single run.

---

## Campaign Duration Formula

```
─── PER WELL ──────────────────────────────────────────────────────────────

wireline_workload = stages × time/stage
                  + wireline rig up/down
                  + temperature log days
                  + wireline contingency %
                  + risk delays (wireline-class)
                  + induced re-runs from consequences

frac_workload     = (stages × frac_time/stage
                  + frac settling time
                  + isolation plug duration
                  + risk delays (frac-class + external)
                  + induced pumping from consequences
                  + frac tree swap delays)
                  × zipper_efficiency_factor

ct_workload       = cleanout duration
                  + cement eval duration (if running online; 0 if offline)
                  + risk delays (CT-class)
                  + induced CT interventions from consequences

milling_workload  = plugs × milling_days_per_plug
                  + extra plugs from risk consequences

flowback_testing  = uniform(flowback_min, flowback_max) days
                  + induced testing days from consequences

─── PASS 1: FRAC PATH ─────────────────────────────────────────────────────

Conventional:
  frac_related_per_well = max(ct_workload, frac_workload + wireline_workload)
  [CT runs in parallel with frac on adjacent well; only gates campaign if CT
   is the pacing resource]

Zipper:
  frac_related_per_well = ct_workload + max(frac_workload, wireline_workload)
  [CT precedes each well; frac and wireline overlap across alternating wells]

frac_path_days = Σ frac_related_per_well over all wells

─── PASS 2: CT SPARE CAPACITY FOR MILLING (optional) ──────────────────────

total_ct_capacity      = frac_path_days × ct_units
ct_available_capacity  = max(total_ct_capacity - total_ct_primary_workload, 0)
ct_milling_support     = min(milling_demand, ct_available × efficiency)
adjusted_milling       = milling_demand - ct_milling_support

─── POST-FRAC DISCRETE SCHEDULER ─────────────────────────────────────────

Wells released from frac in campaign order (earliest first).
For each released well, allocate first available (milling_unit, testing_unit) pair.
Testing unit is held during both milling and flowback phases.
post_frac_completion = max(flowback_finish across all 30 wells)

─── CAMPAIGN DURATION ─────────────────────────────────────────────────────

campaign_days = max(frac_path_days, post_frac_completion)
```

---

## Adapting the Model to Your Operations

| Your operation differs in... | Adjust... | Where |
|---|---|---|
| Stage cycle times | Frac time per stage, wireline time per stage, settling time | Sidebar > Operation timing |
| No cement evaluation or scraper run | Set duration rows to 0 in assumptions CSV | `master_risks_assumptions.csv` |
| Cement evaluation always offline | Set Cement eval offline probability to 1.0 | `master_risks_assumptions.csv` |
| CT and milling are the same unit | Enable "Allow CT to support milling", set CT milling efficiency (0.65 default) | Sidebar > Resources |
| Different milling rate | Supply actual MillingDaysPerPlug values | `historical_wells.csv` |
| Risk likelihoods and delays | Probability / Min / ML / Max per risk | `master_risks_assumptions.csv` |
| Risk scope (per-stage vs per-well vs campaign-wide) | Scope column in assumptions CSV | `master_risks_assumptions.csv` |
| Risk operational consequences | Add consequence override columns | `master_risks_assumptions.csv` |
| Flowback duration | Flowback + testing min/max days | Sidebar > Resources |
| Tree swap handling | Frac tree swap delay hours; frac trees available | Sidebar > Resources |
| Different operation sequence | See Workflow tab in the app | Workflow tab |
| New resource type | Code change required: add to resource vectors and workload formula | `R/engine_core.R` |

