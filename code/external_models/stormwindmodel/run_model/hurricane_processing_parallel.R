################################################################################
# Stormwindmodel parallel worker
# ------------------------------------------------------------------------------
# Helper function used by hurricane_processing.R to process one Monte Carlo
# stormwindmodel input file and write the corresponding county-grid output.
################################################################################

grid_winds_wrapper <- function(filepath, return = F){
  # Load the built-in county grid.
  data("county_points")            # Not sure if it is more efficient to load in-place or transfer as an argument
  if (stormname == "maria") {
    print("Using Puerto Rico county data")
    county_points_file <- if (exists("puertorico_county_points_file", inherits = TRUE)) {
      puertorico_county_points_file
    } else {
      here::here("code", "external_models", "stormwindmodel", "run_model", "us_county_stats_puertorico.csv")
    }

    county_points <- read.csv(county_points_file) %>%
      mutate(glandsea = TRUE)
  }
  
  # Read the input track.
  track <- read.csv(filepath,                                               # Point to the data within the RStudio Project
                    header = F,                                             # The raw data does not have colum names
                    col.names = c("date", "latitude", "longitude", "wind"))
  
  # Run the wind model.
  wind_grid <- get_grid_winds(hurr_track = track, grid_df = county_points)
  
  # Return the result or write it to disk.
  if(return){
    return(wind_grid)
  } else {
    filename <- basename(filepath)
    filename2 <- paste(tools::file_path_sans_ext(filename),"_countygrid.csv",sep="")


    if (stormname == "maria") {
      wind_grid$glandsea <- NULL  # Remove the landsea column before writing the CSV.
    }

    #write.csv(wind_grid, here("outputs",stormname,filename2),row.names = F)
    write.csv(wind_grid, file.path(output_dir, filename2), row.names = F)
  }
}
