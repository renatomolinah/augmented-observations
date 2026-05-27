# ==============================================================================
cat("================================================================================\n")
cat("RUNNING: 1.06.clean_demographics.r\n")
cat("PURPOSE: Clean demographic data - creates county_demographics.csv\n")
cat("================================================================================\n")

#=============================
# 1. Clean BEA income and GDP
#=============================

# Locate the latest BEA vintage automatically. Filenames look like
# CAINC1__ALL_AREAS_1969_<YYYY>.csv and CAGDP1__ALL_AREAS_2001_<YYYY>.csv;
# picking the highest YYYY lets new vintages drop in without code edits.
pick_latest_bea = function(prefix) {
  files = list.files("data/raw/bea", pattern = paste0("^", prefix, "_\\d{4}_\\d{4}\\.csv$"),
                     full.names = TRUE)
  stopifnot(length(files) > 0)
  years = as.integer(str_extract(files, "\\d{4}(?=\\.csv$)"))
  list(file = files[which.max(years)], end_year = max(years))
}

inc_info = pick_latest_bea("CAINC1__ALL_AREAS")
gdp_info = pick_latest_bea("CAGDP1__ALL_AREAS")

income = fread(inc_info$file, fill = TRUE) |>
    filter(str_detect(Description, "capita"))
gdp1 = fread(gdp_info$file, fill = TRUE) |>
    filter(LineCode == 1)
crosswalk = readxl::read_xlsx("data/raw/bea/census-bea-crosswalk.xlsx", skip = 1) |>
    select(fips_census = 3, county_fips = 6) |>
    drop_na() |>
    mutate(county_fips = str_pad(county_fips, 5, "left", pad = "0"))

inc_year_cols = as.character(2005:inc_info$end_year)
gdp_year_cols = as.character(2005:gdp_info$end_year)

income = income |>
    select(county_fips = GeoFIPS, all_of(inc_year_cols)) |>
    pivot_longer(
        cols = -county_fips,
        names_to = "year",
        values_to = "income_pc"
    )

gdp = gdp1 |>
    select(county_fips = GeoFIPS, county_name = GeoName, all_of(gdp_year_cols)) |>
    pivot_longer(
        cols = all_of(gdp_year_cols),
        names_to = "year",
        values_to = "gdp"
    )

income_gdp = inner_join(income, gdp, by = c("county_fips", "year")) |>
    filter(substr(county_fips, 1, 2) != "00" & substr(county_fips, 3, 5) != "000") |>
    left_join(crosswalk, by = "county_fips") |>
    mutate(county_fips = ifelse(!is.na(fips_census), fips_census, county_fips)) |>
    select(-fips_census)

fwrite(income_gdp, "data/clean/income_gdp.csv")

#=============================
# 2. Clean Coastline Counties List
#=============================

# Read in coastline counties list
coastline_counties <- readxl::read_xlsx("data/raw/census/coastline-counties-list.xlsx", skip = 2) |>
  # Clean and standardize the data
  janitor::clean_names() |>
  # Ensure FIPS codes are properly formatted as 5-digit strings
  mutate(
    fips = as.character(state_county_fips),
    fips = str_pad(fips, 5, "left", pad = "0")
  ) |>
  # Select relevant columns
  select(
    county_fips = fips
  ) |>
  mutate(coastal = TRUE)

# Save the cleaned data
fwrite(coastline_counties, "data/clean/coastline_counties.csv")

#=============================
# 3. Clean ACS population
#=============================

# Unified Census processing controls:
# - HF_CENSUS_OVERRIDE: auto | process | skip
# Precedence: explicit skip, then key-based processing, otherwise skip.
census_key <- trimws(Sys.getenv("CENSUS_API_KEY", unset = ""))
has_census_key <- nzchar(census_key)

override_raw <- tolower(trimws(Sys.getenv("HF_CENSUS_OVERRIDE", unset = "")))
if (override_raw == "") {
  override_raw <- "auto"
}

if (!override_raw %in% c("auto", "process", "skip")) {
  warning("Invalid HF_CENSUS_OVERRIDE value. Using 'auto'. Valid values: auto, process, skip.")
  override_raw <- "auto"
}

census_effective_mode <- if (override_raw == "skip") {
  "skip"
} else if (has_census_key) {
  "process"
} else {
  "skip"
}

print(
  sprintf(
    "Census ACS mode: HF_CENSUS_OVERRIDE=%s, CENSUS_API_KEY=%s, effective_mode=%s.",
    override_raw,
    ifelse(has_census_key, "present", "missing"),
    census_effective_mode
  )
)

if (census_effective_mode == "process") {
  
  states = urbnmapr::states |> distinct(state_abbv) |> filter(state_abbv != "DC") |> pull()

  county_demos = map_dfr(states,
          function(state) {
            tidycensus::get_acs(geography = "county", 
                    variables = c(population = "B01003_001"),
                    state = state, 
                    year = 2009) |>
              select(-moe) |>
              pivot_wider(
                names_from = 'variable',
                values_from = 'estimate'
              ) |> 
              select(county = GEOID, population)
          }) 

  fwrite(county_demos, "data/raw/census/county_demographics.csv")
  
}

if (!file.exists("data/raw/census/county_demographics.csv")) {
  stop(
    "Missing data/raw/census/county_demographics.csv. Provide this file or rerun with CENSUS_API_KEY set and HF_CENSUS_OVERRIDE=process."
  )
}
fread("data/raw/census/county_demographics.csv") |>
    fwrite("data/clean/county_demographics.csv")
