# ==============================================================================
# Helper functions for the VOI cleaning scripts (track distances, centroids,
# and safe core selection for parallel processing).
# Run:     sourced by code/cleaning_voi/1.00.prepare_packages.r
# ==============================================================================

# County centroids in EPSG:4326 with numeric FIPS
load_county_centroids <- function() {
  counties <- get_urbn_map("counties", sf = TRUE) %>%
    st_transform(4326) %>%
    mutate(
      county_fips = as.numeric(county_fips)
    )

  centroids <- counties %>%
    st_centroid() %>%
    select(county_fips, geometry) %>%
    st_set_crs(4326)
  
  return(centroids)
}

# County-to-track distances (km) for one hurricane and lead time
calculate_distances <- function(hurricane, lead_time, county_centroids) {
  hurricane_path <- here("data/raw/track_raw", hurricane, "track")
  
  # Load observed track (24-hour BEST)
  obs_line_file <- list.files(
    path = file.path(hurricane_path, "24"),
    pattern = "trackline_bdeck_BEST.shp$",
    full.names = TRUE
  )
  
  # Load predicted track line
  preds_line_file <- list.files(
    path = file.path(hurricane_path, lead_time),
    pattern = "trackline_adeck_OFCL.shp$",
    full.names = TRUE
  )
  
  # Read track data
  obs_line <- st_read(obs_line_file[1], quiet = TRUE) %>%
    st_set_crs(4326)
  preds_line <- st_read(preds_line_file[1], quiet = TRUE) %>%
    st_set_crs(4326)
  
  # Calculate distances for each county (in meters)
  obs_distances <- st_distance(county_centroids, obs_line) %>%
    as.numeric()
  
  pred_distances <- st_distance(county_centroids, preds_line) %>%
    as.numeric()
  
  # Distances returned in km (st_distance is meters)
  results <- data.frame(
    county = county_centroids$county_fips,
    hurricane = hurricane,
    lead_time = as.numeric(lead_time),
    obs_distance = obs_distances / 1000,
    pred_distance = pred_distances / 1000
  )
  
  return(results)
}

# ==============================================================================
# Pick a memory-aware core count for parallel processing.
# Override via HF_N_CORES, MAX_CORES_FOR_MCLAPPLY, or SLURM_CPUS_PER_TASK;
# otherwise derive from detected RAM, capped at physical cores and hard_max_cores.
# ==============================================================================

get_safe_mclapply_cores <- function(target_gb_per_core = 2.5,
                                    hard_max_cores = 8L) {

  # Manual overrides, in precedence order
  if (Sys.getenv("HF_N_CORES") != "") {
    cores <- suppressWarnings(as.integer(Sys.getenv("HF_N_CORES")))
    if (!is.na(cores) && cores >= 1L) {
      message("HF_N_CORES env var → using ", cores, " cores")
      return(max(1L, cores))
    }
  }
  
  if (Sys.getenv("MAX_CORES_FOR_MCLAPPLY") != "") {
    cores <- as.integer(Sys.getenv("MAX_CORES_FOR_MCLAPPLY"))
    message("MAX_CORES_FOR_MCLAPPLY env var → using ", cores, " cores")
    return(max(1L, cores))
  }
  
  # SLURM 
  if (Sys.getenv("SLURM_CPUS_PER_TASK") != "") {
    cores <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK"))
    message("SLURM_CPUS_PER_TASK = ", cores, " → using all allocated cores")
    return(cores)
  }
  
  # Try to detect total memory 
  total_gb <- NA_real_
  
  # Linux
  if (file.exists("/proc/meminfo")) {
    total_gb <- as.numeric(read.table("/proc/meminfo", nrows = 1)[1, 2]) * 1024 / 1e9
  }
  
  # macOS
  if (is.na(total_gb) && Sys.info()["sysname"] == "Darwin") {
    total_gb <- suppressWarnings(
      as.numeric(system("sysctl -n hw.memsize", intern = TRUE)) / 1e9
    )
  }
  
  # Windows
  if (is.na(total_gb) && .Platform$OS.type == "windows") {
    total_gb <- suppressWarnings(
      tryCatch({
        mem <- system("wmic computersystem get TotalPhysicalMemory", intern = TRUE)
        as.numeric(sub(".*TotalPhysicalMemory\\s+:\\s+(\\d+).*", "\\1", mem[3])) / 1e9
      }, error = function(e) NA)
    )
  }
  
  # Conservative fallback
  if (is.na(total_gb) || total_gb < 4) {
    total_gb <- 32  # typical modern laptop
  }
  
  message("Detected ≈ ", round(total_gb, 1), " GB total RAM")
  
  # Leave headroom for OS + R itself
  available_gb <- max(6, total_gb - 6)
  cores <- floor(available_gb / target_gb_per_core)
  
  # Never exceed physical cores or hard limit
  max_possible <- max(1L, parallel::detectCores() - 1L)
  cores <- min(cores, max_possible, hard_max_cores)
  
  message(sprintf("Target ≤ %.1f GB per worker → using %d cores", target_gb_per_core, cores))
  
  return(max(1L, cores))
}