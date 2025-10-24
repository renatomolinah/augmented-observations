# For this comparison, we need the data before averaging over lead times
df_weather = fread("data/clean_denial/forecast_denial_data_with_lead_times.csv")

# Load final panel with damages data
df_denial = readRDS("data/clean_denial/final_dataset_denial.rds")


# Assemble a table with wind speed errors for every hurricane, county, and lead hours
errors_no <- df_weather |>
    filter(augmented_obs == 0) |>
    select(hurricane, county, lead_hours, vmax_sust_dif_mean, prec_dif_mean) |>
    rename(wind_error_no = vmax_sust_dif_mean,
           prec_error_no = prec_dif_mean)

errors_yes <- df_weather |>
    filter(augmented_obs == 1) |>
    select(hurricane, county, lead_hours, vmax_sust_dif_mean, prec_dif_mean) |>
    rename(wind_error_yes = vmax_sust_dif_mean,
           prec_error_yes = prec_dif_mean)

df_errors <- merge(errors_no, errors_yes, by = c("hurricane", "county", "lead_hours"), all = TRUE)


# Merge damage information by county and hurricane
df_errors <- df_errors |>
    left_join(df_denial |> select(county, hurricane, damages), by = c("hurricane", "county"))


# Calculate absolute and relative error changes
df_errors <- df_errors |>
    mutate(wind_error_change_abs = wind_error_yes - wind_error_no,
           wind_error_change_rel = wind_error_change_abs / wind_error_no,
           prec_error_change_abs = prec_error_yes - prec_error_no,
           prec_error_change_rel = prec_error_change_abs / prec_error_no) 

# For relative error changes in precip where the no errors is 0, replace NaN with 0
df_errors$prec_error_change_rel[is.nan(df_errors$prec_error_change_rel)] <- 0

# Plot density distribution of absolute error changes for wind

# Calculate the common x-axis limits based on the 0.005 and 0.995 quantiles
x_min_abs <- quantile(df_errors$wind_error_change_abs, 0.005, na.rm = TRUE)
x_max_abs <- quantile(df_errors$wind_error_change_abs, 0.995, na.rm = TRUE)

# Create separate plots
df_errors %>% 
    filter(wind_error_change_abs >= x_min_abs & wind_error_change_abs <= x_max_abs) |>
    ggplot(aes(x = wind_error_change_abs, fill = factor(lead_hours))) +
    geom_histogram(bins = 50) +
    facet_wrap(~ lead_hours, ncol = 1) +
    labs(title = "Absolute error changes in wind speed",
         x = "Absolute error change",
         y = "Count") +
    theme_minimal() +
    scale_fill_viridis_d(name = "Lead hours") +
    coord_cartesian(xlim = c(x_min_abs, x_max_abs))
ggsave("output/augmented_obs/errors_distr/wind_abs_error_changes.png", width = 6, height = 8)

# Make a boxplot of these three lead times adding the mean as a red point
df_errors |>
    ggplot(aes(x = factor(lead_hours), y = wind_error_change_abs)) +
    geom_boxplot() +
    geom_point(aes(x = factor(lead_hours), y = mean(wind_error_change_abs, na.rm = TRUE)), color = "red") +
    labs(title = "Absolute error changes in wind speed",
         x = "Lead hours",
         y = "Absolute error change") +
    theme_minimal() +
    scale_x_discrete(name = "Lead hours")
ggsave("output/augmented_obs/errors_distr/wind_boxplot_abs_error_changes.png", width = 6, height = 8)

# Plot density distribution of relative error changes
x_min_rel <- quantile(df_errors$wind_error_change_rel, 0.025, na.rm = TRUE)
x_max_rel <- quantile(df_errors$wind_error_change_rel, 0.975, na.rm = TRUE)

df_errors %>% 
    filter(wind_error_change_rel >= x_min_rel & wind_error_change_rel <= x_max_rel) |>
    ggplot(aes(x = wind_error_change_rel*100, fill = factor(lead_hours))) +
    geom_histogram(alpha = 0.7, bins = 50) +
    facet_wrap(~ lead_hours, ncol = 1) +
    labs(title = "Relative error changes in wind speed",
         x = "Relative error change (%)",
         y = "Count") +
    theme_minimal() +
    scale_fill_viridis_d(name = "Lead hours") +
    coord_cartesian(xlim = c(x_min_rel*100, x_max_rel*100)) 
ggsave("output/augmented_obs/errors_distr/wind_rel_error_changes.png", width = 6, height = 8)

df_errors |>
    filter(wind_error_change_rel >= x_min_rel & wind_error_change_rel <= x_max_rel) |>
    ggplot(aes(x = factor(lead_hours), y = wind_error_change_rel*100)) +
    geom_boxplot() +
    geom_point(aes(x = factor(lead_hours), y = mean(wind_error_change_rel, na.rm = TRUE)), color = "red") +
    labs(title = "Boxplot of relative error changes in wind speed",
         x = "Lead hours",
         y = "Relative error change (%)") +
    theme_minimal() +
    scale_x_discrete(name = "Lead hours")
ggsave("output/augmented_obs/errors_distr/wind_boxplot_rel_error_changes.png", width = 6, height = 8)


# Filtered visualizations for observations with damages > 0

# Absolute error changes - histogram
df_errors %>% 
    filter(damages > 0 & wind_error_change_abs >= x_min_abs & wind_error_change_abs <= x_max_abs) |>
    ggplot(aes(x = wind_error_change_abs, fill = factor(lead_hours))) +
    geom_histogram(bins = 50) +
    facet_wrap(~ lead_hours, ncol = 1) +
    labs(title = "Absolute error changes in wind speed (damages > 0)",
         x = "Absolute error change",
         y = "Count") +
    theme_minimal() +
    scale_fill_viridis_d(name = "Lead hours") +
    coord_cartesian(xlim = c(x_min_abs, x_max_abs))
ggsave("output/augmented_obs/errors_distr/wind_abs_error_changes_damages_gt0.png", width = 6, height = 8)

# Absolute error changes - boxplot
df_errors |>
    filter(damages > 0) |>
    ggplot(aes(x = factor(lead_hours), y = wind_error_change_abs)) +
    geom_boxplot() +
    stat_summary(fun = "mean", geom = "point", color = "red", size = 2) +
    labs(title = "Absolute error changes in wind speed (damages > 0)",
         x = "Lead hours",
         y = "Absolute error change") +
    theme_minimal() +
    scale_x_discrete(name = "Lead hours")
ggsave("output/augmented_obs/errors_distr/wind_boxplot_abs_error_changes_damages_gt0.png", width = 6, height = 8)

# Relative error changes - histogram
df_errors %>% 
    filter(damages > 0 & wind_error_change_rel >= x_min_rel & wind_error_change_rel <= x_max_rel) |>
    ggplot(aes(x = wind_error_change_rel*100, fill = factor(lead_hours))) +
    geom_histogram(alpha = 0.7, bins = 50) +
    facet_wrap(~ lead_hours, ncol = 1) +
    labs(title = "Relative error changes in wind speed (damages > 0)",
         x = "Relative error change (%)",
         y = "Count") +
    theme_minimal() +
    scale_fill_viridis_d(name = "Lead hours") +
    coord_cartesian(xlim = c(x_min_rel*100, x_max_rel*100)) 
ggsave("output/augmented_obs/errors_distr/wind_rel_error_changes_damages_gt0.png", width = 6, height = 8)

# Relative error changes - boxplot
df_errors |>
    filter(damages > 0 & wind_error_change_rel >= x_min_rel & wind_error_change_rel <= x_max_rel) |>
    ggplot(aes(x = factor(lead_hours), y = wind_error_change_rel*100)) +
    geom_boxplot() +
    stat_summary(fun = "mean", geom = "point", color = "red", size = 2) +
    labs(title = "Boxplot of relative error changes in wind speed (damages > 0)",
         x = "Lead hours",
         y = "Relative error change (%)") +
    theme_minimal() +
    scale_x_discrete(name = "Lead hours")
ggsave("output/augmented_obs/errors_distr/wind_boxplot_rel_error_changes_damages_gt0.png", width = 6, height = 8)


# Plot density distribution of absolute error changes for precipitation

# Calculate the common x-axis limits based on the 0.005 and 0.995 quantiles
x_min_abs_prec <- quantile(df_errors$prec_error_change_abs, 0.005, na.rm = TRUE)
x_max_abs_prec <- quantile(df_errors$prec_error_change_abs, 0.995, na.rm = TRUE)

# Create separate plots
df_errors %>% 
    filter(prec_error_change_abs >= x_min_abs_prec & prec_error_change_abs <= x_max_abs_prec) |>
    ggplot(aes(x = prec_error_change_abs, fill = factor(lead_hours))) +
    geom_histogram(bins = 50) +
    facet_wrap(~ lead_hours, ncol = 1) +
    labs(title = "Absolute error changes in precipitation",
         x = "Absolute error change",
         y = "Count") +
    theme_minimal() +
    scale_fill_viridis_d(name = "Lead hours") +
    coord_cartesian(xlim = c(x_min_abs_prec, x_max_abs_prec))
ggsave("output/augmented_obs/errors_distr/prec_abs_error_changes.png", width = 6, height = 8)

# Make a boxplot of these three lead times adding the mean as a red point
df_errors |>
    ggplot(aes(x = factor(lead_hours), y = prec_error_change_abs)) +
    geom_boxplot() +
    geom_point(aes(x = factor(lead_hours), y = mean(prec_error_change_abs, na.rm = TRUE)), color = "red") +
    labs(title = "Absolute error changes in precipitation",
         x = "Lead hours",
         y = "Absolute error change") +
    theme_minimal() +
    scale_x_discrete(name = "Lead hours")
ggsave("output/augmented_obs/errors_distr/prec_boxplot_abs_error_changes.png", width = 6, height = 8)

# Relative error changes for precipitation look weird because many have 0s


# Filtered visualizations for observations with damages > 0

# Absolute error changes - histogram
df_errors %>% 
    filter(damages > 0 & prec_error_change_abs >= x_min_abs_prec & prec_error_change_abs <= x_max_abs_prec) |>
    ggplot(aes(x = prec_error_change_abs, fill = factor(lead_hours))) +
    geom_histogram(bins = 50) +
    facet_wrap(~ lead_hours, ncol = 1) +
    labs(title = "Absolute error changes in precipitation (damages > 0)",
         x = "Absolute error change",
         y = "Count") +
    theme_minimal() +
    scale_fill_viridis_d(name = "Lead hours") +
    coord_cartesian(xlim = c(x_min_abs_prec, x_max_abs_prec))
ggsave("output/augmented_obs/errors_distr/prec_abs_error_changes_damages_gt0.png", width = 6, height = 8)

# Absolute error changes - boxplot
df_errors |>
    filter(damages > 0) |>
    ggplot(aes(x = factor(lead_hours), y = prec_error_change_abs)) +
    geom_boxplot() +
    stat_summary(fun = "mean", geom = "point", color = "red", size = 2) +
    labs(title = "Absolute error changes in precipitation (damages > 0)",
         x = "Lead hours",
         y = "Absolute error change") +
    theme_minimal() +
    scale_x_discrete(name = "Lead hours")
ggsave("output/augmented_obs/errors_distr/prec_boxplot_abs_error_changes_damages_gt0.png", width = 6, height = 8)

