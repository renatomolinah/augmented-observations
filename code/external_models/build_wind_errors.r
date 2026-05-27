################################################################################
# Wind error aggregator
# ------------------------------------------------------------------------------
# Build `data/raw/wind_error/<storm>_errors.csv` from county-grid best-track
# observations and Monte Carlo county-grid prediction members.
################################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(purrr)
  library(readr)
  library(stringr)
  library(tibble)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop("Usage: Rscript build_wind_errors.r STORM_NAME MC_TARBALL [OBS_DIR] [OUTPUT_DIR] [OBS_PREFIX]")
}

storm_name <- args[[1]]
mc_tarball <- normalizePath(args[[2]], mustWork = TRUE)
obs_dir <- if (length(args) >= 3) args[[3]] else "data/raw/wind_error/all_forecasts"
output_dir <- if (length(args) >= 4) args[[4]] else "data/raw/wind_error"
obs_prefix <- if (length(args) >= 5) args[[5]] else storm_name

obs_files <- list.files(
  path = obs_dir,
  pattern = "bdeck_BEST",
  full.names = TRUE
) |>
  keep(~ startsWith(basename(.x), obs_prefix))

if (length(obs_files) == 0) {
  stop(sprintf("No best-track county-grid files found for '%s' under %s.", storm_name, obs_dir))
}

obs <- obs_files |>
  map_dfr(function(path) {
    data <- read_csv(path, show_col_types = FALSE)
    file_datetime <- str_extract(basename(path), "\\d{12}")
    data$pred_date <- as.Date(substr(file_datetime, 1, 8), format = "%Y%m%d")
    data
  }) |>
  as.data.table()
setkey(obs, pred_date)

temp_dir <- tempfile("ao_mc_wind_")
dir.create(temp_dir, recursive = TRUE)
on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

untar(mc_tarball, exdir = temp_dir)

pred_files <- list.files(
  path = temp_dir,
  pattern = "\\.csv$",
  full.names = TRUE,
  recursive = TRUE
)

if (length(pred_files) == 0) {
  stop(sprintf("No Monte Carlo county-grid CSVs were found inside %s.", mc_tarball))
}

preds <- pred_files |>
  map_dfr(function(path) {
    data <- read_csv(path, show_col_types = FALSE)
    file_datetime <- str_extract(basename(path), "\\d{12}")
    data$pred_date <- as.Date(substr(file_datetime, 1, 8), format = "%Y%m%d")
    data
  }) |>
  as.data.table()
setkey(preds, pred_date)

data <- merge(
  preds,
  obs,
  by = c("gridid", "pred_date", "glat", "glon"),
  suffixes = c("_pred", "_obs")
)

data[, `:=`(
  vmax_gust_dif = vmax_gust_pred - vmax_gust_obs,
  vmax_sust_dif = vmax_sust_pred - vmax_sust_obs,
  gust_dur_dif = gust_dur_pred - gust_dur_obs,
  sust_dur_dif = sust_dur_pred - sust_dur_obs
)]

summary <- data[, .(
  vmax_sust_obs = mean(vmax_sust_obs, na.rm = TRUE),
  vmax_gust_obs = mean(vmax_gust_obs, na.rm = TRUE),
  sust_dur_obs = mean(sust_dur_obs, na.rm = TRUE),
  gust_dur_obs = mean(gust_dur_obs, na.rm = TRUE),
  vmax_gust_dif_mean = mean(vmax_gust_dif, na.rm = TRUE),
  vmax_gust_dif_sd = sd(vmax_gust_dif, na.rm = TRUE),
  vmax_sust_dif_mean = mean(vmax_sust_dif, na.rm = TRUE),
  vmax_sust_dif_sd = sd(vmax_sust_dif, na.rm = TRUE),
  gust_dur_dif_mean = mean(gust_dur_dif, na.rm = TRUE),
  gust_dur_dif_sd = sd(gust_dur_dif, na.rm = TRUE),
  sust_dur_dif_mean = mean(sust_dur_dif, na.rm = TRUE),
  sust_dur_dif_sd = sd(sust_dur_dif, na.rm = TRUE),
  vmax_gust_pred_mean = mean(vmax_gust_pred, na.rm = TRUE),
  vmax_gust_pred_sd = sd(vmax_gust_pred, na.rm = TRUE),
  vmax_sust_pred_mean = mean(vmax_sust_pred, na.rm = TRUE),
  vmax_sust_pred_sd = sd(vmax_sust_pred, na.rm = TRUE),
  gust_dur_pred_mean = mean(gust_dur_pred, na.rm = TRUE),
  gust_dur_pred_sd = sd(gust_dur_pred, na.rm = TRUE),
  sust_dur_pred_mean = mean(sust_dur_pred, na.rm = TRUE),
  sust_dur_pred_sd = sd(sust_dur_pred, na.rm = TRUE)
), by = c("pred_date", "gridid", "glon", "glat")]

out <- as_tibble(summary) |>
  mutate(
    name = storm_name,
    lead_days = as.numeric(max(pred_date) - pred_date + 1),
    lead_hours = lead_days * 24,
    pred_date = as.Date(pred_date)
  ) |>
  select(name, pred_date, lead_days, lead_hours, everything())

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_file <- file.path(output_dir, paste0(storm_name, "_errors.csv"))
write.csv(out, output_file, row.names = FALSE)

message("Wrote ", output_file)
