# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  data.table, janitor, magrittr, fixest, 
  broom, tidyverse, tidylog, here, data.table, progress
)
options("tidylog.display" = NULL)
`%notin%` = Negate(`%in%`)

# =============================
# Clean wind errors
# =============================

# NOTE: precip errors were direct from model output from Brian McNoldy

# =============================
# Updated `get_errors` Function
# =============================

get_errors <- function(name, file_type = c("NO", "ALL"), progress = T) {
  file_type <- match.arg(file_type)  # Ensure valid file_type
  
  if (progress) { message("Reading swath of observed path") }
  
  # Read in the swath for the hurricane tracks
  obs <- list.files(
    path = here("data/raw_denial", name, "wind"),
    pattern = "*bdeck_BEST*",
    full.names = TRUE
  ) %>%
    map_dfr(~ {
      data <- read_csv(.x, show_col_types = FALSE)
      file_date <- str_extract(basename(.x), "\\d{8}")
      data$pred_date <- as.Date(file_date, format = "%Y%m%d")
      return(data)
    }) %>%
    as.data.table()
  
  setkey(obs, pred_date)
  
  if (progress) { message("Done...") }
  if (progress) { message("Reading lead simulations") }
  
  # Path to the specific tar.gz file based on file_type
  file_pattern <- paste0("mc.*", file_type, "\\.tar\\.gz$")
  tar_file <- list.files(
    path = here("data/raw_denial", name, "wind"),
    pattern = file_pattern,
    full.names = TRUE
  )
  
  if (length(tar_file) == 0) {
    message("No file found for ", file_type, " in storm: ", name)
    return(NULL)
  }
  
  # Temporary directory for extraction
  temp_dir <- tempdir()
  
  # Extract the tar.gz file
  untar(tar_file, exdir = temp_dir)
  
  # Read the extracted files
  preds <- list.files(
    path = temp_dir,
    pattern = "\\.csv$",
    full.names = TRUE
  ) %>%
    map_dfr(~ {
      data <- read_csv(.x, show_col_types = FALSE)
      file_datetime <- str_extract(basename(.x), "\\d{12}")
      data$pred_date <- as.Date(substr(file_datetime, 1, 8), format = "%Y%m%d")
      return(data)
    }) %>%
    as.data.table()
  
  setkey(preds, pred_date)
  
  if (progress) message("Done...")
  
  # Merge and calculate errors
  if (progress) message("Calculating (3 steps)")
  if (progress) message("          Merging data (1/3)")
  
  data <- merge(
    preds, obs,
    by = c("gridid", "pred_date", "glat", "glon"),
    suffixes = c("_pred", "_obs")
  )
  
  if (progress) message("          Calculating differences (2/3)")
  
  data[, `:=`(
    vmax_gust_dif = vmax_gust_pred - vmax_gust_obs,
    vmax_sust_dif = vmax_sust_pred - vmax_sust_obs,
    gust_dur_dif = gust_dur_pred - gust_dur_obs,
    sust_dur_dif = sust_dur_pred - sust_dur_obs
  )]
  
  if (progress) message("          Summarizing by county and lead (3/3)")
  
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
  
  if (progress) message("Done")
  
  # Finalize output
  out <- as_tibble(summary) %>%
    mutate(
      name = as.character(name),
      lead_days = as.numeric(max(pred_date) - pred_date + 1),
      lead_hrs = lead_days * 24,
      pred_date = lubridate::ymd(pred_date)
    ) %>%
    select(name, pred_date, lead_days, lead_hrs, everything())
  
  unlink(temp_dir, recursive = TRUE)  # Cleanup
  
  return(out)
}

# =============================
# Updated `wrapper_function`
# =============================

wrapper_function <- function(storm_names, file_type = c("NO", "ALL")) {
  file_type <- match.arg(file_type)  # Ensure valid file_type
  
  options("tidylog.display" = list())
  
  # Initialize progress bar
  pb <- progress_bar$new(
    format = "  Processing [:bar] :current/:total (:percent) in :elapsed",
    total = length(storm_names),
    clear = FALSE,
    width = 60
  )
  
  # Process each storm name
  lapply(storm_names, function(storm_name) {
    pb$tick()  # Update the progress bar
    
    # Ensure the output directory exists
    output_dir <- file.path("data/raw_denial", paste0("wind_error_", file_type))
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    # File path for the output
    output_file <- file.path(output_dir, paste0(storm_name, "_wind_errors_", file_type, ".csv"))
    
    # Process and save the output
    result <- get_errors(storm_name, file_type = file_type, progress = F)
    if (!is.null(result)) {
      write.csv(result, file = output_file, row.names = FALSE)
    }
  })
  
  options("tidylog.display" = NULL)
}

# Define a vector of storm names
storm_names <- c("sally",
                 "michael",
                 "maria",
                 "laura",
                 "isaias",
                 "irma",
                 "harvey",
                 "hanna",
                 "florence",
                 "dorian"

) # add all directories here


# Get errors
wrapper_function(storm_names, file_type = "NO")
wrapper_function(storm_names, file_type = "ALL")
