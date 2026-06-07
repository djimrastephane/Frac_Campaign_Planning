# Frac Campaign Planning Simulator

A Shiny application for planning and evaluating multi-pad hydraulic fracturing campaigns using Monte Carlo simulation, operational risk modelling, and resource-constrained scheduling.

The simulator is designed for completion engineers, project managers, and operations teams who need to estimate campaign duration, assess uncertainty, evaluate zipper frac strategies, and quantify the impact of additional resources before execution.

The user does not need to edit R code. The application reads CSV input files, runs simulations, generates audit trails, and exports planning reports.

---

# Project Overview

Frac campaigns are complex operations involving multiple wells, multiple pads, shared resources, operational uncertainty, and competing schedules.

Traditional planning approaches often rely on deterministic durations and engineering judgment, making it difficult to quantify schedule uncertainty or evaluate alternative execution strategies.

This project provides a data-driven framework for:

- Campaign duration forecasting
- Operational risk assessment
- Resource planning
- Zipper frac evaluation
- Campaign acceleration studies
- Decision support before execution

The simulator uses historical campaign data and configurable assumptions to generate thousands of possible campaign outcomes through Monte Carlo simulation.

---

# Business Problem

Planning multi-well hydraulic fracturing campaigns is inherently uncertain.

Campaign duration depends on:

- Operational performance
- Equipment reliability
- Crew availability
- Resource constraints
- Technical failures
- Regulatory delays
- Environmental factors

Small delays can accumulate across dozens of wells and significantly impact project schedules and costs.

The objective of this simulator is to provide a quantitative framework for evaluating:

- Campaign duration uncertainty
- Resource requirements
- Operational risks
- Schedule acceleration opportunities
- Conventional versus zipper frac execution strategies

before field execution begins.

---

# Main Features

## Campaign Planning

- Conventional frac simulation
- Zipper frac simulation
- Multi-pad campaign modelling
- Campaign sizes from 20 to 40 wells
- Randomized well allocation across pads
- Randomized stage counts per well

## Resource Acceleration

- Frac fleets
- Wireline units
- CT / cleanout units
- Milling units
- Frac tree availability

## Risk Assessment

- Technical risks
- Resource risks
- External risks
- User-adjustable risk multiplier

## Audit and Reporting

- Well-level audit trail
- Risk event log
- Delay contributor summary
- Resource utilization summary
- Assumptions used table
- Downloadable audit package

---

# Architecture

The simulator follows a modular workflow designed to separate data ingestion, risk modelling, resource allocation, and reporting.

```text
Historical Wells CSV
            │
            ▼
     Input Validation
            │
            ▼
 Historical Duration
     Extraction
            │
            ▼
     Monte Carlo
  Simulation Engine
            │
     ┌──────┼──────┐
     ▼      ▼      ▼
 Risk   Resource  Campaign
 Model    Model   Generator
     └──────┼──────┘
            ▼
     Schedule Forecast
            │
            ▼
      Result Engine
            │
     ┌──────┼──────┐
     ▼      ▼      ▼
 Dashboard Audit   CSV
          Package Exports
```

The architecture allows assumptions, risks, and operational parameters to be modified through CSV files without requiring changes to the underlying R code.

---

# Current Capabilities

## Campaign Planning

- Multi-pad completion campaign simulation
- Campaign sizes from 20 to 40 wells
- Randomized wells per pad allocation
- Randomized stage counts per well
- Conventional frac simulation
- Zipper frac simulation

## Operational Modelling

- SCMT operations
- Scraper / cleanout operations
- Plug and perf execution
- Temperature logging
- Isolation plug installation
- Post-frac milling

## Risk Modelling

- Plug pressure test failures
- Premature plug setting
- Perforation gun misfires
- UPCT failures
- Cement in casing
- Cement above plugs
- Surface equipment failures
- Crew availability constraints
- External operational delays

## Resource Modelling

- Frac fleets
- Wireline units
- CT / cleanout units
- Milling units
- Frac tree availability

## Outputs

- P10 campaign duration
- P50 campaign duration
- P90 campaign duration
- Campaign duration distributions
- Well-level audit trail
- Risk event logs
- Resource utilization summaries
- Downloadable audit package

---

# Business Questions Addressed

## Campaign Duration

- How long will a 20, 30, or 40 well campaign take?
- What is the expected P50 campaign duration?
- What is the probability of exceeding a target completion date?

## Zipper Frac Evaluation

- What schedule reduction can be achieved through zipper frac?
- Is zipper frac justified for a specific campaign size?
- How sensitive is campaign duration to zipper execution efficiency?

## Resource Planning

- How many frac fleets are required?
- How many wireline units are required?
- How many milling units are required?
- Which resources become campaign bottlenecks?

## Risk Assessment

- Which risks contribute most to schedule growth?
- How much contingency should be included in the plan?
- What is the impact of increasing campaign risk exposure?

## Campaign Acceleration

- What is the benefit of adding a second frac fleet?
- What is the benefit of adding additional wireline crews?
- Which investment provides the greatest schedule reduction?

---

# Screenshots

## Main Dashboard

![Main Dashboard](docs/images/dashboard.png)

## Conventional vs Zipper Comparison

![Conventional vs Zipper Comparison](docs/images/comparison.png)

## Campaign Duration Distribution

![Campaign Duration Distribution](docs/images/distribution.png)

## Audit Package Outputs

![Audit Package Outputs](docs/images/audit_package.png)

---

# Input Files

The simulator uses user-supplied CSV files.

## Required Files

### historical_wells.csv

Historical campaign performance data used to derive operational duration distributions.

Typical fields include:

- Well ID
- Pad ID
- Planned stages
- Completed stages
- Plugs installed
- Contingency plugs
- Job duration
- Frac duration
- SCMT duration
- Milling duration
- Proppant pumped

### master_risks_assumptions.csv

Configurable assumptions and risk library used by the simulator.

Typical fields include:

- Probability
- Minimum duration
- Most likely duration
- Maximum duration
- Risk category
- Operational impact

Templates are available in:

```text
data_templates/
```

---

# Outputs

The simulator generates:

- simulation_summary.csv
- simulation_well_details.csv
- simulation_risk_event_log.csv
- resource_utilization.csv
- assumptions_used.csv
- delay_contributors.csv
- resource_utilization_summary.csv

These files are bundled automatically when the user selects:

```text
Download Audit Package
```

---

# Run Locally

## Requirements

- R 4.3+
- Shiny
- tidyverse
- DT
- plotly

## Install Dependencies

```r
install.packages(c(
  "shiny",
  "tidyverse",
  "DT",
  "plotly",
  "readr",
  "dplyr",
  "purrr"
))
```

## Launch Application

Open R, RStudio, or Positron from the project root and run:

```r
source("run_local.R")
```

The application will launch locally in your default browser.

---

# Roadmap

## Version 1.0 – MVP Simulator (Completed)

- Monte Carlo simulation engine
- Historical duration extraction
- Risk assumptions framework
- Conventional frac simulation
- Zipper frac simulation
- Resource acceleration inputs
- Audit package generation

## Version 2.0 – Operational Realism (In Progress)

- Stage-level simulation
- Detailed plug failure logic
- Screen-out modelling
- Operational dependency tracking
- Enhanced delay attribution
- Improved resource utilization reporting

## Version 3.0 – Resource Scheduling Engine (Planned)

- Dynamic resource scheduling
- Critical path identification
- Campaign bottleneck detection
- Multi-fleet optimization
- Pad-to-pad resource movement modelling
- Resource utilization forecasting

## Version 4.0 – Campaign Planning Platform (Planned)

- Interactive Gantt charts
- Schedule visualization
- Campaign comparison dashboard
- Scenario management
- Executive planning reports
- Portfolio-level campaign planning

---

# Limitations

Current version limitations include:

- Operational planning model only
- Not a hydraulic fracture propagation simulator
- Does not model reservoir performance
- Does not model production forecasts
- Assumes campaign activities follow predefined workflows
- Resource scheduling is simplified in Version 1.0
- Results depend on the quality of historical data and assumptions

---

# License

This project is released under the MIT License.

See the LICENSE file for details.

---

# Author

**Stephane Soulanoudjingar**

Senior Completions & Well Intervention Engineer with 17+ years of industry experience and MSc Data Science candidate.

This project combines operational domain expertise with data science techniques to support data-driven campaign planning and execution decision-making.
