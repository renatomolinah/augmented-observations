# ==============================================================================
# Combine per-hurricane SHELDUS damage files, standardize columns and FIPS,
# and attach county/state names.
# Inputs:  data/raw/sheldus/damages_*.csv
# Outputs: data/clean/data_sheldus.csv
# Run:     sourced by code/cleaning_voi/run_all.r
# ==============================================================================
cat("================================================================================\n")
cat("RUNNING: 1.05.clean_damage.r\n")
cat("PURPOSE: Clean SHELDUS damage data - creates sheldus_clean.csv\n")
cat("================================================================================\n")

# ==============================================================================
# 1. Load per-hurricane SHELDUS files
# ==============================================================================

# Each file is damages_YEAR_NAME.csv with hurricane-specific damage records
damage_files = Sys.glob("data/raw/sheldus/damages_*.csv")
stopifnot(length(damage_files) > 0)

damage_list = lapply(damage_files, function(f) {
  # Extract hurricane name from filename pattern damages_YEAR_NAME.csv
  base = tools::file_path_sans_ext(basename(f))
  parts = strsplit(base, "_")[[1]]
  hurr_name = parts[3]

  dt = fread(f)
  dt[, hurricane := hurr_name]
  return(dt)
})

data_sheldus = rbindlist(damage_list, fill = TRUE)

# Rename columns to match pipeline
setnames(data_sheldus, c(
  "StateName", "CountyName", "County_FIPS", "Year", "Month",
  "Fatalities", "FatalitiesPerCapita",
  "CropDmg", "CropDmg(ADJ 2019)", "CropDmgPerCapita",
  "PropertyDmg", "PropertyDmg(ADJ 2019)", "PropertyDmgPerCapita"
), c(
  "state", "county", "county_fips", "year", "month",
  "deaths", "deaths_pc",
  "crop_damage", "crop_damage_2019", "crop_damage_pc_2019",
  "property_damage", "property_damage_2019", "property_damage_pc_2019"
))

# Zero-pad FIPS to 5 chars (new files have bare numeric e.g. 28001)
data_sheldus[, county_fips := str_pad(as.character(county_fips), 5, "left", "0")]
data_sheldus[, state := str_to_lower(as.character(state))]
data_sheldus[, county := str_to_lower(as.character(county))]

# Drop duration columns and records
drop_cols = grep("_Duration|Duration_Days|Records", names(data_sheldus), value = TRUE)
data_sheldus[, (drop_cols) := NULL]

# Add injuries columns (not in new files; keeps across(crop_damage:deaths_pc) range in 1.07)
data_sheldus[, injuries := 0]
data_sheldus[, injuries_pc := 0]

# Convert to tibble for consistency with pipeline
data_sheldus = as_tibble(data_sheldus) |>
  filter(year >= 1980) |>
  arrange(county_fips, year) |>
  select(hurricane, state, county, county_fips, year, month,
         crop_damage, crop_damage_2019, crop_damage_pc_2019,
         property_damage, property_damage_2019, property_damage_pc_2019,
         injuries, injuries_pc, deaths, deaths_pc)

cat("Loaded", n_distinct(data_sheldus$hurricane), "hurricanes from",
    length(damage_files), "files\n")

# ==============================================================================
# 2. Load county data
# ==============================================================================

counties_sf = get_urbn_map("counties", sf = TRUE) |>
  st_transform(4326)

counties_sf = counties_sf |> 
  as_tibble() |>
  rename(std_state = state_name,
         std_county = county_name) |>
  mutate(std_state = str_to_lower(std_state),
         std_county = str_to_lower(std_county))

# ==============================================================================
# 3. Combine data
# ==============================================================================

# Attach standardized county/state names by FIPS; drop Puerto Rico (FIPS 72)
data_sheldus = left_join(
                     data_sheldus |> select(-state, -county),
                     counties_sf,
                     by = "county_fips"
                     ) |>
  select(-fips_class, -geometry) |>
  rename(state = std_state,
         county = std_county) |>
  filter(str_sub(county_fips, 1, 2) != "72")

cat("Total property damage (2019 $B):", sum(data_sheldus$property_damage_2019) / 1e9, "\n")
cat("Total deaths:", sum(data_sheldus$deaths), "\n")

# ==============================================================================
# 4. Save SHELDUS data
# ==============================================================================

fwrite(data_sheldus, "data/clean/data_sheldus.csv")