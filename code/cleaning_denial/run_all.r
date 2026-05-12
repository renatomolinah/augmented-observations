# ==============================================================================
# Orchestrator: Denial cleaning pipeline
# ------------------------------------------------------------------------------
# Depends on VOI cleaning outputs in data/clean/. Sources 1.00 -> 1.03 to
# produce data/clean_denial/forecast_denial_data_with_lead_times.csv|rds.
# ==============================================================================

suppressPackageStartupMessages(source("code/cleaning_denial/1.00.prepare_packages.r"))

scripts <- c(
  "1.01.clean_forecast_error_aug_obs.r",
  "1.02.merge_data.r",
  "1.03.clean_lead_time_data.r"
)

for (s in scripts) {
  cat(sprintf("\n=== %s ===\n", s))
  source(file.path("code/cleaning_denial", s))
}
