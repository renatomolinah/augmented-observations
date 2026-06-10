# ==============================================================================
# Orchestrator for the VOI cleaning pipeline: sources scripts 1.00 -> 1.07.
# Inputs:  data/raw/ (see individual scripts)
# Outputs: data/clean/final_dataset.rds and supporting intermediates
# Run:     Rscript --vanilla code/cleaning_voi/run_all.r
# ==============================================================================

suppressPackageStartupMessages(source("code/cleaning_voi/1.00.prepare_packages.r"))

scripts <- c(
  "1.01.clean_fema_foia.r",
  "1.02.clean_forecast_track.r",
  "1.03.clean_forecast_error.r",
  "1.05.clean_damage.r",
  "1.06.clean_demographics.r",
  "1.07.merge_data.r"
)

for (s in scripts) {
  cat(sprintf("\n=== %s ===\n", s))
  source(file.path("code/cleaning_voi", s))
}
