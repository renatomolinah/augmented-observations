# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  data.table, janitor, magrittr, fixest, 
  broom, tidyverse, tidylog, here, data.table, progress
)
options("tidylog.display" = NULL)
`%notin%` = Negate(`%in%`)

parse_env_list <- function(var_name, default) {
  raw_value <- trimws(Sys.getenv(var_name, unset = ""))
  if (raw_value == "") {
    return(default)
  }

  values <- unlist(strsplit(raw_value, ",", fixed = TRUE))
  values <- trimws(values)
  values[values != ""]
}

# =============================
# Clean precip errors
# =============================

# Function to copy precipitation files for each storm and save separately
process_precipitation_files <- function(storm_names, file_type = c("NO", "ALL")) {
  # Initialize progress bar
  pb <- progress_bar$new(
    format = "  Processing [:bar] :current/:total (:percent) in :elapsed",
    total = length(storm_names),
    clear = FALSE,
    width = 60
  )
  
  # Define the base input directory
  base_dir <- here("data/raw_denial")  # Path where the raw data is located
  
  # Process each storm
  lapply(storm_names, function(storm_name) {
    pb$tick()  # Update progress bar
    
    # Define the input directory for the storm's precipitation data
    precip_input_dir <- file.path(base_dir, storm_name, "precip")  # path: /data/raw_denial/<storm_name>/precip
    
    # Skip if input directory doesn't exist
    if (!dir.exists(precip_input_dir)) {
      message("Skipping ", storm_name, ": Precipitation input directory not found.")
      return(NULL)
    }
    
    # Locate the precipitation error files for this storm
    precip_files <- list.files(
      path = precip_input_dir,
      pattern = paste0(storm_name, "_precip_errors_.*", file_type, "\\.csv$"),
      full.names = TRUE
    )
    
    if (length(precip_files) == 0) {
      message("Skipping ", storm_name, ": No precipitation files found.")
      return(NULL)
    }
    
    # Create the target directory for the storm
    target_dir <- file.path(base_dir, paste0("precip_error_", file_type))  # path: /data/raw_denial/precip_error_<file_type>
    
    # Create the target directory for the storm if it doesn't exist
    if (!dir.exists(target_dir)) {
      dir.create(target_dir, recursive = TRUE)
    }
    
    # Process each precipitation file for the storm
    lapply(precip_files, function(file) {
      precip_data <- read_csv(file, show_col_types = FALSE)
      
      # Add the storm name to the data
      precip_data <- precip_data %>%
        mutate(name = storm_name)
      
      # Ensure gridid is consistently a character type for compatibility
      precip_data$gridid <- as.character(precip_data$gridid)
      
      # Ensure lead_days and lead_hrs are numeric
      precip_data$lead_days <- as.numeric(precip_data$lead_days)
      precip_data$lead_hrs <- as.numeric(precip_data$lead_hrs)
      
      # Remove the time from pred_date and convert it to just a date (YYYY-MM-DD)
      precip_data$pred_date <- as.Date(precip_data$pred_date)
      
      # Standardize columns and save each file separately
      precip_data <- precip_data %>%
        select(name, pred_date, lead_days, lead_hrs, gridid, glon, glat,
               prec_obs, prec_dif_mean, prec_dif_sd, prec_pred_mean, prec_pred_sd)
      
      # Define output file path for each file
      output_file <- file.path(target_dir, paste0(storm_name, "_precip_errors_", file_type, ".csv"))
      
      # Save the file separately
      write.csv(precip_data, output_file, row.names = FALSE)
      message("Processed and saved: ", output_file)
    })
  })
}







# Define the list of storm names
storm_names <- parse_env_list(
  "AO_STORM_LIST",
  c(
    "sally",
    "michael",
    "maria",
    "laura",
    "isaias",
    "irma",
    "harvey",
    "hanna",
    "florence",
    "dorian"
  )
)

file_types <- parse_env_list("AO_DENIAL_FILE_TYPES", c("NO", "ALL"))

# Run the function to process and save precipitation data
invisible(lapply(file_types, function(file_type) {
  process_precipitation_files(storm_names, file_type = file_type)
}))
