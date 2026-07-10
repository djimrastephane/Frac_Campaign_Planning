# Architecture — Subsystem Diagrams

Detailed diagrams for the five subsystems a reviewer most needs to understand.
Every box names the actual function and file it describes (post engine-split
layout: `R/engine_core.R` / `R/summaries.R` / `R/report_pdf.R` /
`R/optimiser_cascade.R` — see [`architecture_cleanup_plan.md`](architecture_cleanup_plan.md)
for how that split was made and verified). The top-level system diagram lives
in the [README's Architecture section](../README.md#architecture).

Diagrams are [Mermaid](https://mermaid.js.org/) — GitHub renders them inline,
and they diff like code, so they can be reviewed and kept current in the same
PRs that change the functions they describe.

---

## 1. Simulation pipeline

One `simulate_campaign_detailed()` call = one operation mode. "Compare both"
runs two calls with the **same seed** (common random numbers), so the
Conventional-vs-Zipper delta is a paired comparison, not two independent
samples.

```mermaid
flowchart TD
    subgraph inputs ["Inputs"]
        HW["historical_wells.csv<br/>(or synthetic_historical_wells())"]
        AS["master_risks_assumptions.csv<br/>(+ Risk Editor grid edits)"]
        RL["risk_consequence_library.csv<br/>(optional)"]
    end

    subgraph validate ["Validation — R/load_inputs.R, R/validate_inputs.R"]
        V["row-level checks: numeric columns, triangle ordering,<br/>probability range, duplicate/blank risk names, scope values"]
    end

    subgraph precompute ["Once per call — R/engine_core.R"]
        PC["build_param_cache()<br/>assumption lookups resolved once"]
        RT["build_risk_table()  (R/risk_library_engine.R)<br/>scope-aware probability + consequence magnitudes"]
        RG["build_risk_grid()<br/>static wells x risks cross-join"]
    end

    subgraph iter ["Per iteration (n_iterations times) — R/engine_core.R"]
        PA["build_pad_assignment_cached()<br/>+ stage/plug count sampling"]
        DR["draw_risks_on_grid()<br/>Bernoulli draws; triangular delays;<br/>consequence workloads (see diagram 5)"]
        SCHED["pre-frac scheduling<br/>event: schedule_pre_frac()<br/>formula: two-pass workload calc"]
        POST["schedule_post_frac_milling()<br/>(see diagram 4)"]
        CP["campaign duration =<br/>critical path across resource streams"]
    end

    subgraph outputs ["Outputs (one row per iteration / well / event)"]
        SUM["summary"]
        WD["well_details<br/>(audit trail)"]
        REL["risk_event_log"]
        RU["resource_utilization"]
        AU["assumptions_used"]
    end

    subgraph downstream ["Downstream"]
        SMRY["R/summaries.R — UI-ready tables"]
        PDF["R/report_pdf.R — management report"]
        APP["app/app.R — dashboard + audit zip"]
    end

    HW --> V
    AS --> V
    RL --> V
    V --> PC --> RT --> RG --> PA
    PA --> DR --> SCHED --> POST --> CP
    CP --> SUM & WD & REL & RU & AU
    SUM & WD & REL & RU & AU --> SMRY --> APP
    SMRY --> PDF --> APP
```

The main "Run simulation" click executes this inside a `future::multisession`
worker (`app/app.R`) so the UI stays responsive; "Compare both" forks the two
mode calls across cores via `.par_lapply()` (`R/optimiser_parallel.R`),
bit-identical to sequential execution because every call seeds itself.

---

## 2. Historical learning pipeline

Runs automatically whenever the historical-wells data changes — no button.
Fitting is `MASS::fitdistr()` MLE; **AIC rank is the selection logic**, and
the KS p-value is an *indicative* check only (its parameters were fitted from
the same data being tested), which is why the UI wording never claims a
confirmed fit.

```mermaid
flowchart TD
    HW["historical_wells.csv upload<br/>(or 30 synthetic wells, flagged in UI)"]
    VAL["validate_historical_wells()<br/>R/validate_inputs.R"]

    subgraph outliers ["Outlier screen — R/learning_engine.R"]
        OW["summarise_outlier_wells()<br/>on frac_days_per_stage"]
        TIER{"tier"}
        WL["Watch-list: above P95<br/>shown, never auto-excluded"]
        EX["Extreme: above P99 or 2x P90<br/>eligible for exclusion"]
        EXCL["user checkbox: exclude extreme wells<br/>(refused if fewer than 5 wells would remain)"]
    end

    subgraph fit ["Distribution fitting — learn_from_historical(), R/learning_engine.R"]
        GATE{"at least 5<br/>valid values?"}
        FITS["MASS::fitdistr() x 4 families:<br/>Normal, Lognormal, Gamma, Weibull"]
        AIC["rank by AIC (primary selection)"]
        KS["indicative KS check to label fit quality<br/>Good / Moderate / Poor"]
        TRI["suggested triangular assumption:<br/>min = P5, mode = distribution mode, max = P95"]
    end

    USER["user pastes suggested min/mode/max into<br/>master_risks_assumptions.csv duration rows"]
    BOOT["filtered well pool feeds the simulation's<br/>bootstrap sampling (single source of truth<br/>with the learning fit)"]

    HW --> VAL --> OW --> TIER
    TIER --> WL
    TIER --> EX --> EXCL
    VAL --> GATE
    EXCL -.->|"removes wells from"| GATE
    GATE -->|yes| FITS --> AIC --> KS --> TRI --> USER
    GATE -->|"no — note shown, no fit"| USER
    EXCL -.->|"removes wells from"| BOOT
```

The Bayesian Update tab extends this pipeline: new completed wells update the
duration prior (Normal-conjugate) and observed risk counts update event
probabilities (Beta-Binomial), gated by `BAYES_DECISION_THRESHOLDS`
(`R/bayesian_updater.R`) before any planning number changes. "Apply" merges
the new wells into the same bootstrap pool.

---

## 3. Recommendation engine

The recommendation is *traceable*: the default answer is an instant analytic
estimate, but the "Verify by re-simulation" button re-runs the full Monte
Carlo with one extra unit of the binding resource at the **same seed** and
measures the actual paired improvement. Both paths end at the same
three-way verdict, driven entirely by `REC_DECISION_THRESHOLDS`
(`R/recommendations.R`) — the UI's "Decision Rules" disclosure reads those
constants live, so displayed rules cannot drift from the code.

```mermaid
flowchart TD
    SIM["completed simulation result"]
    EXPL["explain_bottlenecks()<br/>R/bottleneck_explain.R<br/>ranks resources by measured queue-delay<br/>contribution (not raw utilization alone)"]
    PRIM["primary binding constraint"]

    subgraph paths ["recommend_action() — R/recommendations.R"]
        AN["analytic path (default, instant):<br/>expected saving = cascade rank-1<br/>delay contribution"]
        VER["verified path (button):<br/>re-run simulate_campaign_detailed()<br/>with +1 unit, SAME seed"]
        PAIR["paired per-iteration reductions:<br/>P50 reduction + win rate"]
    end

    subgraph econ ["Economic gate"]
        EV["schedule value = dP50 x total spread rate<br/>added cost = unit day rate x new P50<br/>net value = value - cost"]
    end

    subgraph verdict ["Three-way verdict — REC_DECISION_THRESHOLDS"]
        G1{"net value > 0 AND<br/>dP50 > 0.5 d?"}
        G2{"win-rate confidence<br/>Moderate/High (>= 75%)?"}
        NJ["Not justified"]
        OPT["Optional"]
        REC["Recommended"]
    end

    ROB["robustness OAT sweep (±15%, on demand)<br/>R/robustness.R — does the verdict flip?"]
    BADGE["confidence badge =<br/>win rate x robustness stability"]

    SIM --> EXPL --> PRIM
    PRIM --> AN --> EV
    PRIM --> VER --> PAIR --> EV
    EV --> G1
    G1 -->|no| NJ
    G1 -->|yes| G2
    G2 -->|no| OPT
    G2 -->|yes| REC
    NJ & OPT & REC --> BADGE
    ROB --> BADGE
```

The Overview tab's bottleneck card, the Decision Support tab's recommendation
panel, and the saved scenario records all read the **same** `rec_v2_r()`
object in `app/app.R` — one source of truth, so two tabs can never name
different bottlenecks for the same run.

---

## 4. Resource scheduler

Two schedulers, both in `R/engine_core.R`. The engine is a workload
aggregator, not a calendar-resolution discrete-event simulator — these model
resource *contention*, not stage-by-stage timing (see README Limitations).

```mermaid
flowchart TD
    subgraph prefrac ["Pre-frac — schedule_pre_frac() (event mode, default)"]
        ORDER["wells in sequence order"]
        CT["CT cleanout: own availability timeline<br/>(naturally parallel with previous well's frac)"]
        WL["wireline: earliest-available unit across the<br/>WHOLE pool — a unit finishing well i-1 early<br/>can start well i+1 immediately"]
        FR["frac: earliest-free fleet;<br/>cannot finish before its own wireline"]
        ATTR["wait attribution per well:<br/>wireline-capacity wait vs CT-caused wait;<br/>CT-caused split into queueing (fixable by units)<br/>vs duration floor (not fixable by units)"]
    end

    subgraph postfrac ["Post-frac — schedule_post_frac_milling()"]
        REL["wells in frac-release order (FCFS)"]
        MILL["milling unit assignment<br/>(+ optional CT assist at 0.65 efficiency)"]
        TEST["testing unit: held through BOTH<br/>milling and flowback window"]
        DONE["campaign end = max across all streams"]
    end

    subgraph zipper ["Zipper-mode adjustments (applied to workloads, not queues)"]
        ZF["pump-time factor (slider, default 0.75)"]
        TS["frac-tree swap delay (2 trees);<br/>reduced ~5% at 3 trees, ~10% at 4+"]
        WT["within-pad transition halved"]
    end

    ORDER --> CT --> WL --> FR --> ATTR
    FR --> REL --> MILL --> TEST --> DONE
    zipper -.-> prefrac
```

The legacy "formula" mode (workload ÷ units, two-pass) is kept selectable in
the sidebar and is what `check_regression.R` uses to prove the fast engine
bit-identical to the archived original. The event scheduler's properties are
guarded by `R/test_schedule_pre_frac.R` (42 checks) and
`R/check_scheduling_modes.R` (physical lower bounds + monotonicity).

---

## 5. Risk propagation

Risks don't just add delay — technical risks cascade into *induced workload*
on specific resources, which then feeds the schedulers above. Scope is the
key calibration: treating a campaign-level event (e.g. crew unavailability)
as 30 independent per-well events is the classic error this design prevents.

```mermaid
flowchart TD
    subgraph sources ["Risk definitions"]
        RR["risk rows in master_risks_assumptions.csv<br/>(probability, min/ML/max delay, scope)"]
        LIB["risk_consequence_library.csv (optional):<br/>per-risk consequence magnitude overrides"]
        MULT["sidebar risk-frequency multiplier<br/>(scales occurrence, never severity)"]
    end

    subgraph prob ["Scope-aware probability — build_risk_table(), R/risk_library_engine.R"]
        SC{"scope"}
        PS["stage: per-well p = 1 - (1 - p*m)^stages<br/>multiplier applied per-stage BEFORE compounding"]
        PW["well: p*m independently per well"]
        PCAMP["campaign: single Bernoulli draw,<br/>delay applied once to a representative well"]
    end

    subgraph draw ["Per iteration — draw_risks_on_grid(), R/engine_core.R"]
        GRID["static wells x risks grid<br/>(built once, drawn per iteration)"]
        BERN["Bernoulli occurrence draws"]
        LAZY["delays sampled lazily —<br/>only for events that occurred"]
    end

    subgraph conseq ["Consequences per occurred event"]
        DIRECT["direct delay:<br/>triangular(min, ML, max) days"]
        IND["induced workload — CSV override or<br/>CONSEQUENCE_CONFIG default:<br/>extra wireline runs / CT days / milling plugs /<br/>testing days / extra stages / logistics days"]
    end

    ACC["accumulate_sums(): per-well totals by resource<br/>(frac / wireline / CT / milling / testing / external)"]
    STREAMS["resource workload streams -> schedulers (diagram 4)<br/>-> campaign duration"]
    LOG["risk_event_log: every occurred event with its<br/>delay + consequence columns (audit zip)"]

    RR --> SC
    MULT --> SC
    SC --> PS & PW & PCAMP --> GRID
    LIB --> IND
    GRID --> BERN --> LAZY --> DIRECT & IND --> ACC --> STREAMS
    DIRECT & IND --> LOG
```

The Risks tab's "Consequence propagation" chart splits each risk's total
impact into direct delay vs induced workload precisely because these are two
different mitigation conversations: the first is about the event itself, the
second about the resource that absorbs its aftermath.
