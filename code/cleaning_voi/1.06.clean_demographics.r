# ==============================================================================
# Build county-level controls: BEA per-capita income and GDP, coastline flags,
# and ACS population (re-downloaded only with a Census API key).
# Inputs:  data/raw/bea/{CAINC1,CAGDP1}*.csv, census-bea-crosswalk.xlsx;
#          data/raw/bea/pr/{PRGDP1-1,PRGDP1-2}_PR_2012_2022.csv, qcew_pr_municipio_wages.csv;
#          data/raw/census/{coastline-counties-list.xlsx, county_demographics.csv}
# Outputs: data/clean/{income_gdp.csv, coastline_counties.csv, county_demographics.csv}
# Run:     sourced by code/cleaning_voi/run_all.r
# ==============================================================================
cat("================================================================================\n")
cat("RUNNING: 1.06.clean_demographics.r\n")
cat("PURPOSE: Clean demographic data - creates county_demographics.csv\n")
cat("================================================================================\n")

# ==============================================================================
# 1. Clean BEA income and GDP
# ==============================================================================

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

# Puerto Rico GDP: BEA territory GDP (PRGDP.zip) split by QCEW wage share, chained 2017 $
if (include_pr) {
    pr_line1 = function(f) fread(f, fill = TRUE)[LineCode == 1] |>
        select(matches("^\\d{4}$")) |>
        pivot_longer(everything(), names_to = "year", values_to = "v") |>
        mutate(v = as.numeric(v))
    pr_nominal = pr_line1("data/raw/bea/pr/PRGDP1-1_PR_2012_2022.csv")
    pr_real12  = pr_line1("data/raw/bea/pr/PRGDP1-2_PR_2012_2022.csv")
    pr_gdp = inner_join(pr_nominal, pr_real12, by = "year", suffix = c("_nom", "_real")) |>
        mutate(price = v_nom / v_real,
               gdp_pr = 1e3 * v_nom / (price / price[year == "2017"])) |>
        select(year, gdp_pr)
    pr_income_gdp = fread("data/raw/bea/pr/qcew_pr_municipio_wages.csv",
                          colClasses = c(county_fips = "character", year = "character")) |>
        group_by(year) |>
        mutate(share = total_annual_wages / sum(total_annual_wages)) |>
        ungroup() |>
        inner_join(pr_gdp, by = "year") |>
        transmute(county_fips, year, income_pc = NA_real_, county_name = NA_character_,
                  gdp = share * gdp_pr)
    income_gdp = bind_rows(mutate(income_gdp, across(c(county_fips, year, income_pc, gdp), as.character)),
                           mutate(pr_income_gdp, across(c(income_pc, gdp), as.character)))
}

fwrite(income_gdp, file.path(clean_dir, "income_gdp.csv"))

# ==============================================================================
# 2. Clean coastline counties list
# ==============================================================================

coastline_counties <- readxl::read_xlsx("data/raw/census/coastline-counties-list.xlsx", skip = 2) |>
  janitor::clean_names() |>
  mutate(
    fips = as.character(state_county_fips),
    fips = str_pad(fips, 5, "left", pad = "0")
  ) |>
  select(
    county_fips = fips
  ) |>
  mutate(coastal = TRUE)

fwrite(coastline_counties, file.path(clean_dir, "coastline_counties.csv"))

# ==============================================================================
# 3. Clean ACS population
# ==============================================================================

# HF_CENSUS_OVERRIDE controls re-downloading ACS: auto | process | skip.
# Precedence: explicit skip, then process if a Census key is set, else skip.
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
# Puerto Rico municipios come from the same 2005-2009 ACS table, pulled separately
county_demos_out = fread("data/raw/census/county_demographics.csv")
if (include_pr) county_demos_out = rbind(county_demos_out, fread("data/raw/census/pr_demographics.csv"))
fwrite(county_demos_out, file.path(clean_dir, "county_demographics.csv"))
