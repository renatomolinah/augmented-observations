# ==============================================================================
# Mock wind and rain forecast errors for a Puerto Rico landfall, built from NHC
# files alone as a stand-in until the official Monte Carlo / P-RAIN runs arrive.
# Forecast = the deterministic OFCL track (no Monte Carlo, so the *_sd columns
# are NA); observed rain = P-RAIN on the best track, since ERA5 is not set up.
# Inputs:  data/raw/primitive_inputs/standard/<storm>/mock/{a,b}<id><year>.dat,
#          <yymmddhh><ID><yy>_ships.txt per cycle; code/external_models/config/storm_manifest.csv;
#          data/code/run_stormwindmodel/us_county_stats_puertorico.csv; data/raw/census/counties_tigris.rds;
#          code/external_models/assets/prain/{bin/phram_pr.exe, topo/US_topo_10_50N_50_130W.txt}
# Outputs: data/raw/wind_error/pr_mock/<storm>_errors.csv;
#          data/raw/precip_error/pr_mock/<storm>_precip_errors.csv
# Run:     Rscript --vanilla code/external_models/mock/build_pr_mock.r fiona
# ==============================================================================
suppressPackageStartupMessages({
  library(data.table); library(dplyr); library(sf); library(stormwindmodel)
})
sf_use_s2(FALSE)

storm <- commandArgs(trailingOnly = TRUE)[1]
man <- fread("code/external_models/config/storm_manifest.csv", colClasses = "character")[stormname == storm]
stopifnot(nrow(man) == 1)
atcf_id <- paste0(man$stormid, man$year)                                  # al072022
mock_dir <- file.path("data/raw/primitive_inputs/standard", storm, "mock")
cycles <- data.table(lead_days = 1:3, init = substr(c(man$forecast_24h, man$forecast_48h, man$forecast_72h), 1, 10))

# ATCF decks: one row per (time, tau), lat/lon in tenths with hemisphere letters
read_atcf <- function(path, tech) {
  x <- fread(cmd = paste("cut -d, -f1-20", shQuote(path)), header = FALSE, fill = TRUE, strip.white = TRUE)
  x <- x[V5 == tech, .(dtg = as.character(V3), tau = as.integer(V6), lat = V7, lon = V8, vmax = as.numeric(V9), rmw = as.numeric(V20))]
  x <- unique(x, by = c("dtg", "tau"))
  x[, lat := as.numeric(sub("[NS]$", "", lat)) / 10 * ifelse(grepl("S$", lat), -1, 1)]
  x[, lon := as.numeric(sub("[EW]$", "", lon)) / 10 * ifelse(grepl("W$", lon), -1, 1)]
  x[, time := as.POSIXct(dtg, format = "%Y%m%d%H", tz = "UTC") + 3600 * tau][]
}
adeck <- read_atcf(file.path(mock_dir, paste0("a", atcf_id, ".dat")), "OFCL")
bdeck <- read_atcf(file.path(mock_dir, paste0("b", atcf_id, ".dat")), "BEST")

# tracks on a 6-hourly 0-120 h grid from a start time
on_grid <- function(tr, start) {
  hrs <- seq(0, 120, 6); t <- start + 3600 * hrs
  data.table(hr = hrs, time = t,
             lat = approx(tr$time, tr$lat, t, rule = 2)$y, lon = approx(tr$time, tr$lon, t, rule = 2)$y,
             vmax = approx(tr$time, tr$vmax, t, rule = 2)$y)
}
start_of <- function(dtg) as.POSIXct(dtg, format = "%Y%m%d%H", tz = "UTC")
fc_track  <- function(init) on_grid(adeck[dtg == init], start_of(init))
obs_track <- on_grid(bdeck, start_of(cycles[lead_days == 3, init]))

# ---- wind: stormwindmodel on the Puerto Rico municipio grid -------------------
grid <- fread("data/code/run_stormwindmodel/us_county_stats_puertorico.csv") |> as.data.frame()
grid$glandsea <- TRUE
grid_winds <- function(tr) {
  w <- get_grid_winds(hurr_track = data.frame(date = format(tr$time, "%Y%m%d%H%M"), latitude = tr$lat,
                                              longitude = tr$lon, wind = tr$vmax), grid_df = grid)
  w <- as.data.table(w)[, .(gridid = as.integer(gridid), vmax_sust, vmax_gust, sust_dur, gust_dur)]
  merge(w, as.data.table(grid)[, .(gridid = as.integer(gridid), glat, glon)], by = "gridid")
}
w_obs <- grid_winds(obs_track)
wind <- rbindlist(lapply(seq_len(nrow(cycles)), function(i) {
  init <- cycles$init[i]
  w <- merge(grid_winds(fc_track(init)), w_obs, by = c("gridid", "glat", "glon"), suffixes = c("_pred", "_obs"))
  w[, .(name = storm, pred_date = format(start_of(init), "%Y-%m-%d"), lead_days = cycles$lead_days[i],
        lead_hours = 24L * cycles$lead_days[i], gridid, glon, glat,
        vmax_sust_obs, vmax_gust_obs, sust_dur_obs, gust_dur_obs,
        vmax_gust_dif_mean = vmax_gust_pred - vmax_gust_obs, vmax_gust_dif_sd = NA_real_,
        vmax_sust_dif_mean = vmax_sust_pred - vmax_sust_obs, vmax_sust_dif_sd = NA_real_,
        gust_dur_dif_mean = gust_dur_pred - gust_dur_obs, gust_dur_dif_sd = NA_real_,
        sust_dur_dif_mean = sust_dur_pred - sust_dur_obs, sust_dur_dif_sd = NA_real_,
        vmax_gust_pred_mean = vmax_gust_pred, vmax_gust_pred_sd = NA_real_,
        vmax_sust_pred_mean = vmax_sust_pred, vmax_sust_pred_sd = NA_real_,
        gust_dur_pred_mean = gust_dur_pred, gust_dur_pred_sd = NA_real_,
        sust_dur_pred_mean = sust_dur_pred, sust_dur_pred_sd = NA_real_)]
}))

# ---- rain: P-RAIN (Puerto Rico grid build) on forecast and best tracks --------
# SHIPS shear, interpolated to 6 h through u/v as create_storm_data.f90 does
ships_shear <- function(init) {
  f <- file.path(mock_dir, paste0(substr(init, 3, 10), toupper(man$stormid), substr(man$year, 3, 4), "_ships.txt"))
  l <- readLines(f)
  row <- function(key) as.numeric(strsplit(trimws(substring(l[startsWith(l, key)][1], 15)), " +")[[1]][1:13])
  h <- row("TIME (HR"); m <- row("SHEAR (KT"); d <- row("SHEAR DIR")
  u <- m * sin((450 - d) * pi / 180); v <- m * cos((450 - d) * pi / 180)
  hrs <- seq(0, 120, 6)
  ui <- approx(h, u, hrs)$y; vi <- approx(h, v, hrs)$y
  list(mag = approx(h, m, hrs)$y, dir = (atan2(vi, ui) * 180 / pi + 360) %% 360)
}
rmw_at <- function(t) approx(bdeck$time, bdeck$rmw, t, rule = 2)$y * 1.852      # nm -> km

pr_poly <- readRDS("data/raw/census/counties_tigris.rds") |> filter(substr(GEOID, 1, 2) == "72") |>
  st_transform(4326) |> select(GEOID)
run_prain <- function(tr, shear, rmw_km) {
  wd <- tempfile("prain_"); dir.create(wd)
  writeLines(sprintf("%3d%3s%3s%3s%3s%8.2f%8.2f%6.1f%6.1f%6.1f%6.1f%6.1f", as.integer(tr$hr), "00", "00", "00", "00",
                     tr$lat, -tr$lon, 0, tr$vmax, shear$mag, shear$dir, rmw_km), file.path(wd, "storm_data.txt"))
  file.symlink(normalizePath("code/external_models/assets/prain/topo/US_topo_10_50N_50_130W.txt"), file.path(wd, "US_topo.txt"))
  old <- setwd(wd); on.exit({ setwd(old); unlink(wd, recursive = TRUE) })
  system(normalizePath(file.path(old, "code/external_models/assets/prain/bin/phram_pr.exe")), ignore.stdout = TRUE)
  rain <- scan("R-CLIPER.txt", quiet = TRUE)
  stopifnot(length(rain) == 1201 * 1201)
  pts <- data.table(lon = rep(-75 + (0:1200) / 60, times = 1201), lat = rep(10 + (0:1200) / 60, each = 1201), mm = rain)
  pts <- pts[lon > -67.5 & lon < -65.1 & lat > 17.8 & lat < 18.6]
  j <- st_join(st_as_sf(pts, coords = c("lon", "lat"), crs = 4326), pr_poly, join = st_within)
  as.data.table(st_drop_geometry(j))[!is.na(GEOID), .(prec = mean(mm)), by = .(gridid = as.integer(GEOID))]
}
r_obs <- run_prain(obs_track, ships_shear(cycles[lead_days == 3, init]), rmw_at(obs_track$time))
rain <- rbindlist(lapply(seq_len(nrow(cycles)), function(i) {
  init <- cycles$init[i]; tr <- fc_track(init)
  r <- merge(run_prain(tr, ships_shear(init), rep(rmw_at(start_of(init)), nrow(tr))), r_obs,
             by = "gridid", suffixes = c("_pred", "_obs"))
  merge(r, grid[, c("gridid", "glon", "glat")], by = "gridid")[, .(
    pred_date = format(start_of(init), "%Y-%m-%d %H:%M:%S"), lead_days = cycles$lead_days[i],
    lead_hrs = 24L * cycles$lead_days[i], gridid, glon, glat,
    prec_dif_mean = prec_pred - prec_obs, prec_dif_sd = NA_real_,
    prec_pred_mean = prec_pred, prec_pred_sd = NA_real_, prec_obs)]
}))

dir.create("data/raw/wind_error/pr_mock", showWarnings = FALSE)
dir.create("data/raw/precip_error/pr_mock", showWarnings = FALSE)
fwrite(wind, file.path("data/raw/wind_error/pr_mock", paste0(storm, "_errors.csv")))
write.csv(rain, file.path("data/raw/precip_error/pr_mock", paste0(storm, "_precip_errors.csv")))
cat(sprintf("Wrote mock %s: %d wind rows, %d rain rows\n", storm, nrow(wind), nrow(rain)))
