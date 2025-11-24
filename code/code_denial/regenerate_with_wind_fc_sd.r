# Regenerate clean denial dataset with wind_fc_sd
# Run this script to update the final dataset to include wind_fc_sd, gust_fc_sd, and dur_fc_sd

print("=========================================")
print("Step 0: Loading packages")
print("=========================================")
source("code/code_denial/1.00.prepare_packages.r")

print("=========================================")
print("Step 1: Cleaning forecast errors")
print("=========================================")
source("code/code_denial/1.01.clean_forecast_error_aug_obs.r")

print("=========================================")
print("Step 2: Merging data")
print("=========================================")
source("code/code_denial/1.02.merge_data.r")

print("=========================================")
print("Done! final_dataset_denial.rds now includes:")
print("  - wind_fc_sd")
print("  - gust_fc_sd") 
print("  - dur_fc_sd")
print("=========================================")

