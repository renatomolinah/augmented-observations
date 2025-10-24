# Load panel used in the main paper
df_main = readRDS("data/clean/final_dataset.rds") # (Maria is not included in this dataset)

# Load panel with augmented obs
df_denial = readRDS("data/clean_denial/final_dataset_denial.rds")

# Plot density to compare the distribution of log population in the observations of both datasets
df_denial %>%
  ggplot(aes(x = log(population))) +
  geom_density(aes(fill = "Denial"), alpha = 0.5) +
  geom_density(data = df_main, aes(x = log(population), fill = "Main"), alpha = 0.5) +
  scale_fill_manual(values = c("Denial" = "red", "Main" = "blue")) +
  labs(title = "Density of population in the main and denial datasets",
       x = "Log population",
       y = "Density") +
  theme_minimal()

# These are quite similar

# Next, do the same for GDP
df_denial %>%
  ggplot(aes(x = log(gdp))) +
  geom_density(aes(fill = "Denial"), alpha = 0.5) +
  geom_density(data = df_main, aes(x = log(gdp), fill = "Main"), alpha = 0.5) +
  scale_fill_manual(values = c("Denial" = "red", "Main" = "blue")) +
  labs(title = "Density of GDP in the main and denial datasets",
       x = "Log GDP",
       y = "Density") +
  theme_minimal()

# Let's look at RHS variables next. Let's look at the distribution of windspeed
df_denial %>%
  count(forecast_cut) %>%
  mutate(dataset = "Denial") %>%
  bind_rows(
    df_main %>%
      count(forecast_cut) %>%
      mutate(dataset = "Main")
  ) %>%
  group_by(dataset) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(x = forecast_cut, y = percentage, fill = dataset)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distribution of forecast_cut in the main and denial datasets",
       x = "Forecast_cut",
       y = "Percentage of Total Observations") +
  theme_minimal()

# Higher wind is slighly overrepresented in the denial dataset

# What about precipitation?
df_denial %>%
  count(forecast_precip_cut) %>%
  mutate(dataset = "Denial") %>%
  bind_rows(
    df_main %>%
      count(forecast_precip_cut) %>%
      mutate(dataset = "Main")
  ) %>%
  group_by(dataset) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(x = forecast_precip_cut, y = percentage, fill = dataset)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distribution of forecast_precip_cut in the main and denial datasets",
       x = "Forecast_cut",
       y = "Percentage of Total Observations") +
  theme_minimal()

# Not much difference either

# Let's look at the RHS variables now. First, compare the densities of log ex_ante_spending
df_denial %>%
    filter(ex_ante_spending > 0) %>%
  ggplot(aes(x = log(ex_ante_spending))) +
  geom_density(aes(fill = "Denial"), alpha = 0.5) +
  geom_density(data = filter(df_main, ex_ante_spending > 0), aes(x = log(ex_ante_spending), fill = "Main"), alpha = 0.5) +
  scale_fill_manual(values = c("Denial" = "red", "Main" = "blue")) +
  labs(title = "Density of ex_ante_spending in the main and denial datasets",
       x = "Log(ex ante spending)",
       y = "Density") +
  theme_minimal()

# Looks like upper percentiles are quite different. What are the 90, 99, and 99.5 percentiles for each one here?
df_main %>%
  filter(ex_ante_spending > 0) %>%
  summarise(
    `90th percentile` = quantile(ex_ante_spending, 0.9)/1e6,
    `99th percentile` = quantile(ex_ante_spending, 0.99)/1e6,
    `99.5th percentile` = quantile(ex_ante_spending, 0.995)/1e6
  )
df_denial %>%
  filter(ex_ante_spending > 0) %>%
  summarise(
    `90th percentile` = quantile(ex_ante_spending, 0.9)/1e6,
    `99th percentile` = quantile(ex_ante_spending, 0.99)/1e6,
    `99.5th percentile` = quantile(ex_ante_spending, 0.995)/1e6
  )
# Main has a higher right tail

# Next, let's look at the density of ex_ante_spending_per_gdp
df_denial %>%
  filter(ex_ante_spending > 0) %>%
  ggplot(aes(x = log(ex_ante_spending_per_gdp))) +
  geom_density(aes(fill = "Denial"), alpha = 0.5) +
  geom_density(data = filter(df_main, ex_ante_spending > 0), aes(x = log(ex_ante_spending_per_gdp), fill = "Main"), alpha = 0.5) +
  scale_fill_manual(values = c("Denial" = "red", "Main" = "blue")) +
  labs(title = "Density of ex_ante_spending_per_gdp in the main and denial datasets",
       x = "Log ex ante spending per GDP",
       y = "Density") +
  theme_minimal()

# And per capita
df_denial %>%
  filter(ex_ante_spending_pc > 0) %>%
  ggplot(aes(x = log(ex_ante_spending_pc))) +
  geom_density(aes(fill = "Denial"), alpha = 0.5) +
  geom_density(data = filter(df_main, ex_ante_spending_pc > 0), aes(x = log(ex_ante_spending_pc), fill = "Main"), alpha = 0.5) +
  scale_fill_manual(values = c("Denial" = "red", "Main" = "blue")) +
  labs(title = "Density of ex_ante_spending_per_gdp in the main and denial datasets",
       x = "Log ex ante spending per capita",
       y = "Density") +
  theme_minimal()  

 # Not terribly different, either... 