# ==============================================================================
# Clean FEMA Public Assistance spending: map disaster numbers to hurricanes,
# deflate to 2019$, aggregate to county-storm, split ex-ante vs ex-post.
# Inputs:  data/raw/fema/PublicAssistanceFundedProjectsDetails_2024.csv;
#          code/external_models/config/storm_manifest.csv
# Outputs: data/clean/fema_spending.csv
# Run:     sourced by code/cleaning_voi/run_all.r
# ==============================================================================
cat("================================================================================\n")
cat("RUNNING: 1.01.clean_fema_foia.r\n")
cat("PURPOSE: Clean FEMA disaster spending data and create fema_spending.csv\n")
cat("================================================================================\n")

# ==============================================================================
# 1. Load raw data
# ==============================================================================

# https://www.fema.gov/openfema-data-page/public-assistance-funded-projects-details-v1
fema_df = fread("data/raw/fema/PublicAssistanceFundedProjectsDetails_2024.csv") |> 
  clean_names()
fema_df = remove_empty(fema_df, which = c("cols"))

# Create indicator for ex ante spending
fema_df = fema_df[, ex_ante_spending := damage_category_code == "B - Protective Measures"]

# Remove currency symbols from dollar variables
fema_df = fema_df[, 
                  ':=' (
                    project_amount = as.numeric(gsub('[$,]', '', project_amount)),
                    federal_share_obligated = as.numeric(gsub('[$,]', '', federal_share_obligated)),
                    total_obligated = as.numeric(gsub('[$,]', '', total_obligated))
                  )]

# Aggregate spending by county, storm, and ex ante indicator
fema_df = fema_df[, .(
  project_amount = sum(project_amount, na.rm = TRUE),
  federal_share_obligated = sum(federal_share_obligated, na.rm = TRUE),
  total_obligated = sum(total_obligated, na.rm = TRUE)
  ),
  by = .(disaster_number, declaration_date, state_number_code, county, county_code, ex_ante_spending)
  ]

# ==============================================================================
# 2. Disaster numbers by hurricane
# ==============================================================================
# Source: https://www.fema.gov/disaster/declarations

# 2024
milton_num = c(3622, 3623, 4834, 4844)
helene_num = c(3615, 3616, 3617, 3618, 3619, 3620, 4827, 4828, 4829, 4830, 4831, 4832, 4848, 4851)
francine_num = c(3614, 4817)
debby_num = c(3605, 3606, 3607, 3608, 4806, 4815, 4821, 4825, 4835)
beryl_num = c(4798)

# 2023
idalia_num = c(3596, 3597, 4734, 4738)

# 2022
ian_num = c(4677, 3586, 4673, 3585, 3584)
nicole_num = c(4680, 3588, 3587)

# 2021
ida_num = c(4627, 4629, 3627, 4626, 4618, 4614, 4615, 3572, 3573, 4611, 3569, 3568)
nicholas_num = c(3574) # no financial assistance approved according to FEMA website

# 2020
laura_num = c(4572, 3541, 4559, 3540, 3539, 3538, 3537)
hanna_num = c(3530)
isaias_num = c(4574, 4568, 4566, 4567, 4560, 3535, 3534, 3533, 4583, 4580)
delta_num = c(4570, 3548, 3547)
zeta_num = c(3549, 4579, 4573, 4576, 4577)
sally_num = c(4564, 4563, 3546, 3543, 3544, 3545)

# 2019
dorian_num = c(4468, 4465, 4464, 3423, 3422, 3421, 3420, 3419, 3418)
barry_num = c(4458)

# 2018
michael_num = c(4412, 4411, 4406, 4401, 4400, 3407, 4399, 3406, 3405)
florence_num = c(4393, 3403, 3400, 3401, 4394)

# 2017
harvey_num = c(4345, 3382, 4332)
irma_num = c(3385, 3383, 3384, 4335, 4338, 3389, 4336, 4337, 3387, 3388, 3386, 4341, 4346)
nate_num = c(4350, 4349, 3394, 3395, 3393, 3392)

# 2016
matthew_num = c(4291, 4286, 4285, 4283, 4284, 3380, 3378, 3379, 3377)
hermine_num = c(4280)

# 2014
arthur_num = c() # no disasters declared on website or spreadsheets

# 2012
sandy_num = c(4099, 4098, 4097, 4096, 4095, 4093, 4092, 4091, 4090, 4089, 4086, 4087, 4085, 
              3360, 3355, 3356, 3357, 3358, 3359, 3354, 3349, 3350, 3351, 3352, 3353)

isaac_num = c(4084, 4082, 4080, 4081, 3348, 3347) # no disasters declared on website or spreadsheets

# 2011
irene_num = c(4037, 4036, 4034, 4032, 4028, 4027, 4026, 4025, 
              4024, 4023, 4022, 4021, 4020, 4019, 4017, 3338, 
              3339, 3336, 3337, 3333, 3334, 3335, 3331, 3332, 
              3328, 3329, 3330, 3327, 3326)

# 2008
ike_num = c(1791, 1792, 3295, 3294, 3293)
gustav_num = c(1806, 1794, 1793, 1789, 1786, 3292, 3291, 3289, 3290)
dolly_num = c(1780)

# 2005
wilma_num = c(1609)
katrina_num = c(3262, 3263, 3258, 3255, 3256, 3257, 3251, 3244, 3252, 3245, 3253, 3246, 
                3248, 3249, 3242, 3250, 3243, 3240, 3241, 3238, 3231, 3239, 3232, 3233, 
                3235, 3236, 3237, 3230, 3225, 3226, 3229, 3220, 3221, 3222, 3217, 3218, 
                3219, 3215, 3216, 1605, 1603, 1604, 3213, 3214, 1602, 3212)
dennis_num = c(1593, 1594, 1595)
rita_num = c(1606, 1607, 3260, 3261, 3259)
cindy_num = c(1601)

# Collect every *_num vector defined above into one set of disaster numbers
all_objects <- ls()
num_objects <- all_objects[grep("_num$", all_objects)]
disaster_numbers <- NULL
for (obj_name in num_objects) {
  disaster_numbers <- c(disaster_numbers, get(obj_name))
}

# ==============================================================================
# 3. Process FEMA data
# ==============================================================================

# Keep only hurricanes in the analysis sample
fema_df = fema_df[disaster_number %in% disaster_numbers]

# Generate hurricane name variable
fema_df = fema_df[,
                  hurricane := case_when(
                    disaster_number %in% arthur_num ~ "arthur",
                    disaster_number %in% barry_num ~ "barry",
                    disaster_number %in% cindy_num ~ "cindy",
                    disaster_number %in% delta_num ~ "delta",
                    disaster_number %in% dennis_num ~ "dennis",
                    disaster_number %in% dolly_num ~ "dolly",
                    disaster_number %in% dorian_num ~ "dorian",
                    disaster_number %in% florence_num ~ "florence",
                    disaster_number %in% gustav_num ~ "gustav",
                    disaster_number %in% hanna_num ~ "hanna",
                    disaster_number %in% harvey_num ~ "harvey",
                    disaster_number %in% hermine_num ~ "hermine",
                    disaster_number %in% ian_num ~ "ian",
                    disaster_number %in% ida_num ~ "ida",
                    disaster_number %in% idalia_num ~ "idalia",
                    disaster_number %in% beryl_num ~ "beryl",
                    disaster_number %in% debby_num ~ "debby",
                    disaster_number %in% francine_num ~ "francine",
                    disaster_number %in% helene_num ~ "helene",
                    disaster_number %in% milton_num ~ "milton",
                    disaster_number %in% ike_num ~ "ike",
                    disaster_number %in% irene_num ~ "irene",
                    disaster_number %in% irma_num ~ "irma",
                    disaster_number %in% isaac_num ~ "isaac",
                    disaster_number %in% isaias_num ~ "isaias",
                    disaster_number %in% katrina_num ~ "katrina",
                    disaster_number %in% laura_num ~ "laura",
                    disaster_number %in% matthew_num ~ "matthew",
                    disaster_number %in% michael_num ~ "michael",
                    disaster_number %in% nate_num ~ "nate",
                    disaster_number %in% nicholas_num ~ "nicholas",
                    disaster_number %in% nicole_num ~ "nicole",
                    disaster_number %in% rita_num ~ "rita",
                    disaster_number %in% sally_num ~ "sally",
                    disaster_number %in% sandy_num ~ "sandy",
                    disaster_number %in% wilma_num ~ "wilma",
                    disaster_number %in% zeta_num ~ "zeta",
                    TRUE ~ NA_character_
                  )
]

# Catch the case where a *_num vector was added above but its case_when
# arm was forgotten: the pre-filter lets the rows in, case_when returns NA,
# and the storm would be silently dropped by the downstream filter.
stopifnot(!any(is.na(fema_df$hurricane)))

# Create FIPS and date variables
fema_df = fema_df[, ':=' (
  fips = 1000*state_number_code + county_code,
  declaration_date = date(declaration_date),
  year = year(declaration_date),
  month = month(declaration_date),
  day = day(declaration_date)
)]

# Deflate project_amount to 2019$ using BLS CPI-U annual averages
# (series CUUR0000SA0). Use the storm's landfall year (from storm_manifest)
# as the dollar vintage so each storm's spending is on a single basis;
# this matches the SHELDUS damages already in 2019$ from the source feed.
cpi_u = c(
  "2005" = 195.300, "2006" = 201.600, "2007" = 207.342, "2008" = 215.303,
  "2009" = 214.537, "2010" = 218.056, "2011" = 224.939, "2012" = 229.594,
  "2013" = 232.957, "2014" = 236.736, "2015" = 237.017, "2016" = 240.007,
  "2017" = 245.120, "2018" = 251.107, "2019" = 255.657, "2020" = 258.811,
  "2021" = 270.970, "2022" = 292.655, "2023" = 304.702, "2024" = 313.689
)

storm_year_map = fread("code/external_models/config/storm_manifest.csv")[
  , .(hurricane = str_to_lower(stormname), storm_year = as.integer(year))
]

fema_df = fema_df |>
  inner_join(storm_year_map, by = "hurricane") |>
  mutate(
    cpi_storm = cpi_u[as.character(storm_year)],
    project_amount = project_amount * (cpi_u["2019"] / cpi_storm)
  ) |>
  select(-storm_year, -cpi_storm) |>
  as.data.table()

stopifnot(!any(is.na(fema_df$project_amount)))

fema_df_county = fema_df[county_code != 0]
fema_df_county = fema_df_county[, c("fips", "hurricane", "project_amount", "ex_ante_spending")]

# Aggregate across disaster types (2019 dollars)
fema_df_county = fema_df_county[, .(
  project_amount = sum(project_amount, na.rm = TRUE)
),
by = .(fips, hurricane, ex_ante_spending)]

fema_df_county = fema_df_county |>
  pivot_wider(
    names_from = "ex_ante_spending",
    values_from = "project_amount",
    names_prefix = "spending_"
  )

setnames(
  fema_df_county, 
  new = c("ex_ante_spending", "ex_post_spending"), 
  old = c("spending_TRUE", "spending_FALSE")
)

fema_df_county[is.na(fema_df_county)] = 0
fema_df_county$pdd = TRUE

# ==============================================================================
# 4. Save FEMA data
# ==============================================================================

# Print per-storm spending so a missing or misassigned storm is visible.
cat("FEMA storms in fema_spending.csv (project_amount sum, millions $):\n")
print(
  fema_df_county |>
    group_by(hurricane) |>
    summarise(spending_m = round(sum(ex_post_spending + ex_ante_spending) / 1e6, 1)) |>
    arrange(hurricane),
  n = Inf
)

fwrite(fema_df_county, "data/clean/fema_spending.csv")
