add_adjust_box <- function(file, line_before = "\\begin{adjustbox}{width = .9\\textwidth}", line_after = "\\end{adjustbox}", before = "\\begin{threeparttable}", after = "\\end{threeparttable}") {
    # read in the file
    lines <- readLines(file)

    # find the line number of the line beginning with "\begin{threeparttable}"
    after_line <- grep(after, lines, fixed = TRUE)

    # find the line number of the line beginning with "\end{threeparttable}"
    before_line <- grep(before, lines, fixed = TRUE)

    # add the line after the line beginning with "\begin{threeparttable}" and before the line beginning with "\end{threeparttable}"
    lines <- c(lines[1:(before_line-1)], line_before, lines[(before_line):(after_line)], line_after, lines[(after_line+1):length(lines)])

    # write the file
    writeLines(lines, file)
}

load_df_normalized <- function() {
  # Load in data
  df = readRDS("data/clean/normalized_dataset.rds") |>
    as_tibble()
  return(df)
}

load_df_normalized_global <- function() {
  # Load in data
  df = readRDS("data/clean/normalized_dataset_global.rds") |>
    as_tibble()
  return(df)
}

load_df <- function() {
  # Load in data
  df = readRDS("data/clean/final_dataset.rds") |>
    as_tibble()
  return(df)
}

load_df_track <- function(analysis_cutoff = 400) {
  # Load the track errors dataset
  df_track_errors <- readRDS(here("data/clean/pooled_track_errors_all_cutoffs.rds")) %>%
    filter(cutoff == analysis_cutoff)
  return(df_track_errors)
}

load_pdd <- function() {
  # Load in data
  df = readRDS("data/clean/final_dataset.rds") |>
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

load_df_popw <- function() {
  # Load population-weighted dataset
  df_popw = readRDS("data/clean/final_dataset_popw.rds") |>
    as_tibble()
  return(df_popw)
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
