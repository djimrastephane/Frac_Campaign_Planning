# Input Files

*Column-by-column CSV reference, moved out of the [README](../README.md) to keep it orientation-focused.*

The application reads two required files and one optional file. Ready-to-use templates with embedded editing guides are in the [`data_templates/`](../data_templates/) folder.

---

## historical_wells.csv — required

One row per completed well from your previous campaigns. Minimum 5 wells. The two right-hand columns are the only ones the simulation engine reads; all others are metadata kept for audit output.

| Column | Role | Notes |
|---|---|---|
| `WellID` | Metadata | Any unique identifier (text) |
| `PadID` | Metadata | Pad name or number (text) |
| `StagesPlanned` | Metadata | Stages originally planned |
| `StagesCompleted` | Used by engine | Stages actually pumped to design |
| `PlugsInstalled` | Used by engine | Total isolation plugs set |
| `ContingencyPlugs` | Used by engine | Extra plugs set due to failures (0 if none) |
| `FracDays` | Metadata | Total pumping days for this well |
| `CementEvalDays` | Metadata | CT/wireline time for cement evaluation |
| `MillingDays` | Metadata | Total milling days for this well |
| **`FracDaysPerStage`** | **KEY — simulation input** | `= FracDays / StagesCompleted` — compute carefully |
| **`MillingDaysPerPlug`** | **KEY — simulation input** | `= MillingDays / PlugsInstalled` — include contingency plugs |

**Example rows:**

```
WellID,PadID,StagesPlanned,StagesCompleted,PlugsInstalled,ContingencyPlugs,FracDays,CementEvalDays,MillingDays,FracDaysPerStage,MillingDaysPerPlug
W-001, Pad_A,6,            6,              5,             0,               11.6,    1.0,           3.4,        1.93,            0.68
W-002, Pad_A,6,            6,              5,             0,               12.0,    0.8,           3.5,        2.00,            0.70
W-003, Pad_B,7,            7,              6,             0,               13.5,    0.9,           4.1,        1.93,            0.68
W-004, Pad_C,6,            6,              5,             0,               6.2,     0.5,           2.0,        1.03,            0.40   ← fast well
W-005, Pad_D,6,            6,              5,             0,               55.5,    2.1,           7.5,        9.25,            1.50   ← slow well/screenout
```

The engine bootstrap-resamples `FracDaysPerStage` and `MillingDaysPerPlug` to build duration distributions. Include outliers — they represent real uncertainty.

---

## master_risks_assumptions.csv — required

Contains two types of rows. **Read the header comments before editing.**

### Locked-name rows (Campaign Setup + Base Operation)
The engine looks these up by exact name. Do **not** rename them. Only change the numeric values.

| Category | Variable / Risk Event | Type | Editable values |
|---|---|---|---|
| Campaign Setup | Wells per pad | Random input | Min / Most Likely / Max |
| Campaign Setup | Stages per well | Random input | Min / Most Likely / Max |
| Campaign Setup | Temperature log stages | Random input | Min / Most Likely / Max |
| Campaign Setup | Cement eval offline | Random input | Probability (0–1) |
| Base Operation | Cement eval duration | Duration | Min / ML / Max Days |
| Base Operation | Scraper / cleanout run | Duration | Min / ML / Max Days |
| Base Operation | Frac days per stage | Historical | Reference only — engine resamples from CSV |
| Base Operation | Milling days per plug | Historical | Reference only — engine resamples from CSV |

### Free rows (Technical Risk / Resource Risk / External Risk)
Risk rows are processed generically — names are for display only. Rename, add, or remove freely.

| Column | What it controls |
|---|---|
| `Probability` | 0–1. Stage-scope: probability per stage. Campaign-scope: probability for the whole campaign. |
| `Min Days / Most Likely Days / Max Days` | Triangular distribution for the delay when the event occurs. |
| `Scope` | `stage`, `well`, or `campaign` — controls how probability is applied |
| `extra_wireline_runs` | Additional wireline trips triggered by this event (optional override) |
| `extra_ct_days` | Additional CT unit-days (optional override) |
| `extra_milling_plugs` | Additional plugs to mill (optional override) |
| `extra_testing_days` | Additional testing unit-days (optional override) |
| `extra_frac_days` | Additional pumping time (optional override) |

**Example rows (first few columns shown):**

```
Category,      Variable / Risk Event,    Type,        Probability,Min Days,Most Likely Days,Max Days,...,Scope
Campaign Setup,Wells per pad,            Random input,N/A,        3,       5,               6,       ...,
Base Operation,Cement eval duration,     Duration,    N/A,        0.5,     1.0,             2.0,     ...,
Technical Risk,Screen out,               Risk,        0.03,       0.25,    0.50,            2.0,     ...,stage
Resource Risk, Wireline crew unavailable,Risk,        0.08,       0.50,    1.0,             2.0,     ...,campaign
External Risk, Weather delay,            Risk,        0.05,       0.25,    1.0,             4.0,     ...,campaign
```

---

## workflow_config.csv — optional

Override the operational sequence without editing R code. If this file is absent, the engine uses the built-in plug-and-perf sequence automatically.

Each row is one activity. Row order within each phase controls execution order.

| Column | Editable? | Notes |
|---|---|---|
| `activity` | Free text | Short name, must be unique within the file |
| `phase` | Locked values | `pre_frac`, `frac_stage`, or `post_frac` |
| `resource` | Locked values | `Frac fleet`, `Wireline`, `CT / cleanout`, `Milling`, `Testing unit` |
| `duration_source` | Syntax locked | `param:<name>`, `formula:<expr>`, or `historical` |
| `conditional` | Locked values or blank | `!cement_eval_offline`, `temp_log`, `is_zipper`, `is_first_on_pad`, `!is_first_on_pad` |
| `path_type` | Locked values | `sequential` (extends schedule) or `parallel` (absorbed if spare capacity) |
| `notes` | Free text | Description shown in the Workflow tab |

**Example rows:**

```
activity,             phase,     resource,     duration_source,                         conditional,         path_type, notes
CT cleanout / scraper,pre_frac,  CT / cleanout,param:Scraper / cleanout run,            ,                    parallel,  ...
Cement evaluation,    pre_frac,  CT / cleanout,param:Cement eval duration,              !cement_eval_offline,parallel,  ...
Pump stage,           frac_stage,Frac fleet,   formula:frac_time_per_stage,             ,                    sequential,...
Mill out plugs,       post_frac, Milling,      formula:n_plugs * milling_days_per_plug, ,                    sequential,...
Flowback + well test, post_frac, Testing unit, formula:flowback_days,                   ,                    sequential,...
```

To add a new activity, copy any row of the same phase, paste at the end of that phase section, and edit. To disable an activity without removing it, set its duration to 0 in `master_risks_assumptions.csv`.

