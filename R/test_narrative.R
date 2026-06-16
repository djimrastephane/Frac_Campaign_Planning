# test_narrative.R  -- run: Rscript test_narrative.R
ENGINE <- if (file.exists("simulation_engine_fast.R")) "simulation_engine_fast.R" else "simulation_engine.R"
suppressPackageStartupMessages({
  source(ENGINE); source("risk_uncertainty.R"); source("bottleneck_explain.R")
  source("recommendations.R"); source("narrative_engine.R")
})

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

mk <- function(mode, trees) {
  a <- list(historical_wells=HISTORICAL, assumptions=ASSUMPTIONS, n_wells=30, n_iterations=400,
            milling_units=2, testing_units=1, operation_mode=mode, frac_trees=trees, seed=42)
  list(args=a, res=do.call(simulate_campaign_detailed, a))
}
z <- mk("Zipper",2); c0 <- mk("Conventional",1)
combined <- list(summary=dplyr::bind_rows(z$res$summary, c0$res$summary),
                 resource_utilization=dplyr::bind_rows(z$res$resource_utilization, c0$res$resource_utilization))

n <- generate_narrative(combined, sim_args=z$args, target_days=170, budget=60e6)
cat("\n== Narrative ==\n"); print_narrative(n)

ok <- TRUE
chk <- function(c,m){cat(sprintf("  [%s] %s\n", if(isTRUE(c))"PASS" else "FAIL", m)); ok<<-ok&&isTRUE(c)}
chk(nchar(n$narrative) > 80, "narrative non-trivial")
chk(grepl("Zipper", n$narrative), "names recommended mode")
chk(grepl("Testing unit", n$narrative), "names primary constraint")
chk(grepl("faster than conventional", n$narrative), "includes mode comparison")
chk(grepl("target", n$narrative), "includes target probability")
chk(grepl("P10-P90", n$narrative), "includes uncertainty range")

cat(sprintf("\n==== %s ====\n", if (ok) "ALL PROPERTY CHECKS PASS" else "FAILURES ABOVE"))
if (!ok) quit(status = 1)
