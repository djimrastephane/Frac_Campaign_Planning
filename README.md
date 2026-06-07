# Frac Campaign Planning Simulator

A Shiny app for Monte Carlo simulation of multi-pad hydraulic fracturing campaigns.

The user does not need to edit R code. The app reads CSV input files, runs the simulation, and exports an audit package.

## Main features

- Conventional vs zipper frac comparison
- Resource acceleration inputs
  - Frac fleets
  - Wireline units
  - CT / cleanout units
  - Milling units
  - Frac trees available
- Risk multiplier for low, normal, or high-risk campaigns
- Well-level audit trail
- Risk event log
- Delay contributor summary
- Resource utilization summary
- Assumptions used table
- Downloadable audit package

## Required input files

Upload these files in the app:

1. `historical_wells.csv`
2. `master_risks_assumptions.csv`

Templates are available in `data_templates/`.

## Run locally

Open R, RStudio, or Positron from the project root and run:

```r
source("run_local.R")
```

## Output tables

The app generates:

- `simulation_summary.csv`
- `simulation_well_details.csv`
- `simulation_risk_event_log.csv`
- `resource_utilization.csv`
- `assumptions_used.csv`
- `delay_contributors.csv`
- `resource_utilization_summary.csv`

These files are bundled when the user clicks **Download audit package**.

## Notes

The model is an operational planning simulator. It is not a hydraulic fracture propagation model.

Probabilities and durations are controlled by the uploaded assumptions file. Historical durations come from the uploaded historical wells file.
