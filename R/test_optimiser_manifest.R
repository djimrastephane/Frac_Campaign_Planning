# test_optimiser_manifest.R -- run: Rscript test_optimiser_manifest.R
#
# Phase 6.A of the optimiser auditability pass: reproducibility manifest.
suppressPackageStartupMessages({
  library(dplyr); library(tibble)
  source("constants.R"); source("optimiser_manifest.R")
})

ok <- TRUE
chk <- function(c, m) { cat(sprintf("  [%s] %s\n", if (isTRUE(c)) "PASS" else "FAIL", m)); ok <<- ok && isTRUE(c) }

# ---- Table hashing: content-only, column-order-independent -------------------
params_a <- tibble(variable = c("Stages per well", "Cement eval offline"),
                   min_days = c(8, 0), max_days = c(14, 0))
params_a_reordered <- params_a[, rev(names(params_a))]
params_b <- params_a; params_b$max_days[1] <- 15  # one changed value

chk(hash_optimiser_table(params_a) == hash_optimiser_table(params_a_reordered),
    "column order does not affect a table's hash")
chk(hash_optimiser_table(params_a) != hash_optimiser_table(params_b),
    "changing one value changes the table's hash")
chk(hash_optimiser_table(NULL) == hash_optimiser_table(tibble()),
    "NULL and empty tibble hash identically (both 'no table')")
chk(hash_optimiser_table(params_a) != hash_optimiser_table(NULL),
    "a real table never collides with the empty-table hash")

# ---- Full manifest: identical inputs -> identical INPUT hashes ---------------
mk_manifest <- function(seed = 123L, params = params_a, risks = tibble(x = 1), cons = tibble(y = 2),
                        hist = tibble(well_id = paste0("W", 1:5))) {
  build_optimiser_run_manifest(
    project_root = ".", seed = seed, operation_modes = "Conventional",
    scheduling_mode = "event", n_wells = 30L, screen_iterations = 150L,
    refine_iterations = 600L, top_n_refine = 5L, results = NULL,
    scenario_grid = tibble(frac_fleets = 1:2),
    resource_search_ranges = list(frac_fleets = c(1, 2)),
    day_rates = as.list(DEFAULT_DAY_RATES),
    active_parameters_df = params, active_risks_df = risks,
    risk_consequence_df = cons, historical_wells = hist, using_synthetic = TRUE
  )
}

m1 <- mk_manifest()
Sys.sleep(1.1)  # ensure a different wall-clock second, to prove input hashes are timestamp-independent
m2 <- mk_manifest()

input_hash_fields <- c("active_parameters_hash", "active_risks_hash", "risk_consequence_hash",
                       "historical_data_hash", "scenario_grid_hash")
chk(all(vapply(input_hash_fields, function(f) identical(m1$hashes[[f]], m2$hashes[[f]]), logical(1))),
    "identical inputs (run at different times) produce identical input hashes")
chk(m1$run_identity$run_id != m2$run_identity$run_id,
    "manifest_hash / run_id still differ between two runs, even with identical inputs (they encode run identity/time, not just inputs -- see optimiser_manifest.R)")

# ---- One changed parameter changes the hash -----------------------------------
m_param_changed <- mk_manifest(params = params_b)
chk(m1$hashes$active_parameters_hash != m_param_changed$hashes$active_parameters_hash,
    "changing one active-parameter value changes active_parameters_hash")
chk(m1$hashes$active_risks_hash == m_param_changed$hashes$active_risks_hash,
    "...and does NOT change active_risks_hash (independent hashes per table)")

# ---- One changed risk changes the hash -----------------------------------------
m_risk_changed <- mk_manifest(risks = tibble(x = 999))
chk(m1$hashes$active_risks_hash != m_risk_changed$hashes$active_risks_hash,
    "changing the active risk table changes active_risks_hash")

# ---- One changed consequence changes the hash -----------------------------------
m_cons_changed <- mk_manifest(cons = tibble(y = 999))
chk(m1$hashes$risk_consequence_hash != m_cons_changed$hashes$risk_consequence_hash,
    "changing the risk consequence table changes risk_consequence_hash")

# ---- Seed change is captured -----------------------------------------------------
m_seed_changed <- mk_manifest(seed = 456L)
chk(m1$simulation_controls$seed != m_seed_changed$simulation_controls$seed,
    "changing the seed is captured in simulation_controls$seed")
chk(m1$run_identity$run_id != m_seed_changed$run_identity$run_id,
    "changing the seed produces a different run_id")

# ---- Historical data change is captured ------------------------------------------
m_hist_changed <- mk_manifest(hist = tibble(well_id = paste0("W", 1:9)))
chk(m1$hashes$historical_data_hash != m_hist_changed$hashes$historical_data_hash,
    "changing historical data changes historical_data_hash")
chk(m1$historical_data$row_count != m_hist_changed$historical_data$row_count,
    "row_count reflects the actual historical data used")

# ---- Local paths excluded ---------------------------------------------------------
chk(is.na(redact_filename("/Users/someone/Downloads/historical_wells.csv")) == FALSE &&
    redact_filename("/Users/someone/Downloads/historical_wells.csv") == "historical_wells.csv",
    "redact_filename() strips the directory, keeps only the basename")
chk(!grepl("/", redact_filename("/a/b/c/d.csv"), fixed = TRUE),
    "redacted filename contains no path separator")
manifest_json <- as.character(jsonlite::toJSON(mk_manifest(), auto_unbox = TRUE, digits = NA))
chk(!grepl("/Users/|/home/|C:\\\\", manifest_json),
    "serialized manifest JSON contains no obvious local filesystem path")
chk(!grepl(Sys.getenv("HOME"), manifest_json, fixed = TRUE),
    "serialized manifest JSON does not contain the current user's home directory")

# ---- Excluded well IDs recorded, not silently dropped -----------------------------
m_excl <- build_optimiser_run_manifest(
  project_root = ".", seed = 1L, operation_modes = "Conventional", scheduling_mode = "event",
  n_wells = 30L, screen_iterations = 150L, refine_iterations = 600L, top_n_refine = 5L,
  results = NULL, scenario_grid = tibble(a = 1), resource_search_ranges = list(),
  day_rates = list(), active_parameters_df = tibble(), active_risks_df = tibble(),
  risk_consequence_df = tibble(), historical_wells = tibble(well_id = paste0("W", 1:20)),
  using_synthetic = FALSE, historical_filename = "/local/path/hist.csv",
  excluded_well_ids = c("W3", "W7")
)
chk(setequal(m_excl$historical_data$excluded_well_ids, c("W3", "W7")),
    "excluded well IDs are recorded in the manifest")
chk(m_excl$historical_data$excluded_count == 2,
    "excluded_count matches the number of excluded wells")
chk(m_excl$historical_data$filename == "hist.csv",
    "uploaded filename is redacted to its basename in the manifest")

# ---- manifest_to_flat_df() -----------------------------------------------------
flat <- manifest_to_flat_df(m1)
chk(is.data.frame(flat) && all(c("key", "value") %in% names(flat)),
    "manifest_to_flat_df() returns a key/value table")
chk(any(grepl("^simulation_controls\\.seed$", flat$key)),
    "flattened table includes simulation_controls.seed")
chk(!any(grepl("active_parameters$", flat$key) & flat$value == ""),
    "flattened table doesn't emit blank rows for the (deliberately excluded) full parameter table"
)

# ---- CSV text sanitisation is applied (reuses app.R's existing helper) -----------
app_path <- if (file.exists("../app/app.R")) "../app/app.R" else "app/app.R"
exprs <- parse(app_path)
env <- new.env()
for (e in as.list(exprs)) {
  if (is.call(e) && identical(e[[1]], as.name("<-")) &&
      is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function")) &&
      deparse(e[[2]]) %in% c("sanitize_csv_cell", "sanitize_csv_text_cols")) {
    eval(e, envir = env)
  }
}
chk(exists("sanitize_csv_text_cols", envir = env),
    "app.R's sanitize_csv_text_cols() is available for the manifest's CSV exports")
sanitize_csv_text_cols <- get("sanitize_csv_text_cols", envir = env)
risky <- tibble(variable = c("Normal risk", "=cmd|'/c calc'"), simulation_impact = c("extra stage", "note"))
sanitized <- sanitize_csv_text_cols(risky, c("variable", "simulation_impact"))
chk(startsWith(sanitized$variable[2], "'"),
    "a formula-injection-shaped active-risk cell gets the same apostrophe-prefix treatment as other exports")

cat(sprintf("\n==== %s ====\n", if (ok) "ALL PROPERTY CHECKS PASS" else "FAILURES ABOVE"))
if (!ok) quit(status = 1)
