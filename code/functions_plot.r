

load_df <- function() {
  # Load in data
  df = readRDS("data/clean_denial/final_dataset_denial.rds") |>
    as_tibble()
  return(df)
}

load_pdd <- function() {
  # Load in data
  df = readRDS("data/clean_denial/final_dataset_denial.rds") |>
    as_tibble()

  pdd_df = fread("data/raw/fema/DisasterDeclarationsSummaries.csv") |>
    clean_names() |>
    filter(incident_type == "Hurricane") |>
    filter(str_detect(declaration_title, paste(str_to_upper(unique(df$hurricane)), collapse = "|"))) |>
    mutate(
      hurricane = str_extract(declaration_title, paste(str_to_upper(unique(df$hurricane)), collapse = "|")),
      hurricane = str_to_lower(hurricane)
    ) |>
    filter(declaration_type == "DR") |> # disaster declaration
    select(hurricane, fips_state_code, fips_county_code)

  pdd_df = pdd_df[, ":="(
    county = 1000 * fips_state_code + fips_county_code
  )] |>
    distinct(county, hurricane)

  df = df |>
    left_join(pdd_df |> mutate(pdd_declared = TRUE)) |>
    replace_na(list(pdd_declared = FALSE)) |>
    filter(!(pdd_declared & damages == 0))

  return(df)
}

# =============================
# ggplot theme
# =============================
sysfonts::font_add_google("Lato")
main_theme <- 
  theme_minimal() +
  theme(
    legend.position = "none",
    title = element_text(size = 16),
    text = element_text(family = "Lato"),
    axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),
    panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(),
    axis.line = element_line(colour = "black"), axis.ticks = element_line()
  ) 

slides_theme <- 
  theme_minimal() +
  theme(
    legend.position = "none",
    title = element_text(size = 14),
    text = element_text(family = "Lato"),
    axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),
    panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(),
    axis.line = element_line(colour = "black"), axis.ticks = element_line()
  ) 
main_theme_map <- 
  theme_minimal() +
  theme(
    title = element_text(size = 16),
    text = element_text(family = "Lato")
  ) 

# ================================================
#     Forecast Wind Map with Storm Track
# ================================================

# Interpolate track
interp_track <- function(track){
  
  if(nrow(track) < 3){
    return(track)
  } else {
    interp_date <- seq(from = min(track$date),
                       to = max(track$date),
                       by = 900) # interpolate to every 15 minutes
    
    interp_lat <- stats::spline(x = track$date,
                                y = track$latitude,
                                xout = interp_date,
                                method = "natural")$y
    interp_lon <- stats::spline(x = track$date,
                                y = track$longitude,
                                xout = interp_date,
                                method = "natural")$y
    
    full_track <- data.frame(storm_id = track$storm_id[1],
                             date = interp_date,
                             latitude = interp_lat,
                             longitude = interp_lon)
  }
  return(full_track)
}
