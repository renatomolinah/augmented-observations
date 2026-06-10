# ==============================================================================
# Orchestrator for the denial cleaning pipeline: sources scripts 1.00 -> 1.03.
# Inputs:  data/raw_denial/ and VOI outputs in data/clean/
# Outputs: data/clean_denial/forecast_denial_data_with_lead_times.{csv,rds}
# Run:     Rscript --vanilla code/cleaning_denial/run_all.r
# ==============================================================================

suppressPackageStartupMessages(source("code/cleaning_denial/1.00.prepare_packages.r"))

scripts <- c(
  "1.01.clean_forecast_error_aug_obs.r",
  "1.03.clean_lead_time_data.r"
)

for (s in scripts) {
  cat(sprintf("\n=== %s ===\n", s))
  source(file.path("code/cleaning_denial", s))
}
