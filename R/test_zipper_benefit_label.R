# test_zipper_benefit_label.R -- run: Rscript test_zipper_benefit_label.R
#
# Regression test for the "zipper efficiency display consistency" fix:
# build_zipper_benefit_breakdown()'s component label must reflect whatever
# zipper_efficiency value it is actually called with, not a hardcoded 0.75.
ENGINE_FILES <- if (file.exists("engine_core.R")) {
  c("engine_core.R", "summaries.R", "report_pdf.R", "optimiser_cascade.R")
} else "archive/simulation_engine.R"
suppressPackageStartupMessages({ for (.ef in ENGINE_FILES) source(.ef); source("risk_library_engine.R") })

ASSUMPTIONS <- dplyr::bind_rows(
  tibble::tribble(
    ~variable,~category,~type,~probability,~min_days,~most_likely_days,~max_days,~simulation_impact,~scope,
    "Stages per well","Param","param",NA,8,10,14,NA,NA,
    "Temperature log stages","Param","param",NA,1,2,3,NA,NA,
    "Wells per pad","Param","param",NA,2,3,4,NA,NA,
    "Cement eval duration","Param","param",NA,0.5,1.0,2.0,NA,NA,
    "Scraper / cleanout run","Param","param",NA,0.3,0.5,1.0,NA,NA,
    "Temperature log duration","Param","param",NA,0.2,0.3,0.5,NA,NA,
    "Isolation plug duration","Param","param",NA,0.3,0.5,1.0,NA,NA,
    "Cement eval offline","Param","param",0.8,0,0,0,NA,NA),
  tibble::tribble(
    ~variable,~category,~type,~probability,~min_days,~most_likely_days,~max_days,~simulation_impact,~scope,
    "Milling difficulty","Milling","risk",0.10,0.5,1.0,3.0,"extra milling","well")
)
set.seed(1)
HISTORICAL <- tibble::tibble(
  well_id=paste0("HW_",1:30), pad_id=paste0("Pad_",((1:30-1)%/%3)+1),
  stages_completed=sample(8:14,30,TRUE), plugs_installed=sample(8:14,30,TRUE),
  contingency_plugs=sample(0:2,30,TRUE), frac_days=round(runif(30,8,18),1),
  cement_eval_days=round(runif(30,0.5,2),2), milling_days=round(runif(30,5,12),1),
  frac_days_per_stage=round(triangle_sample(0.5,0.8,2.5,30),3),
  milling_days_per_plug=round(triangle_sample(0.4,0.7,1.5,30),3))

mk <- function(mode, eff) {
  a <- list(historical_wells=HISTORICAL, assumptions=ASSUMPTIONS, n_wells=20, n_iterations=200,
            milling_units=2, testing_units=1, operation_mode=mode, frac_trees=2,
            zipper_efficiency=eff, seed=42)
  do.call(simulate_campaign_detailed, a)
}

ok <- TRUE
chk <- function(c,m){cat(sprintf("  [%s] %s\n", if(isTRUE(c))"PASS" else "FAIL", m)); ok<<-ok&&isTRUE(c)}

# Non-default efficiency (0.85, i.e. 15% faster) -- the historical bug always
# showed "×0.75" / "25% faster" here regardless of this input.
eff <- 0.85
zip <- mk("Zipper", eff); conv <- mk("Conventional", 1)
combined <- dplyr::bind_rows(zip$summary, conv$summary)
bd <- build_zipper_benefit_breakdown(combined, zipper_efficiency = eff)

chk(!is.null(bd), "breakdown produced for a run with both modes")
chk(grepl("×0.85", bd$component[1]), "component label reflects the actual efficiency (x0.85), not x0.75")
chk(!grepl("×0.75", bd$component[1]), "component label does not show the stale default x0.75")
chk(grepl("15% faster", bd$explanation[1]), "explanation text says 15% faster (1 - 0.85), matching the input")

# Sanity: default call (no zipper_efficiency arg) still behaves as before --
# confirms this is purely a labeling fix, not a behavior change.
bd_default <- build_zipper_benefit_breakdown(combined)
chk(grepl("×0.75", bd_default$component[1]), "default-arg call still shows x0.75 (unchanged default)")
chk(identical(bd$saving_days, bd_default$saving_days),
    "saving_days is computed purely from `summary`, not the label -- unaffected by this fix")

cat(sprintf("\n==== %s ====\n", if (ok) "ALL PROPERTY CHECKS PASS" else "FAILURES ABOVE"))
if (!ok) quit(status = 1)
