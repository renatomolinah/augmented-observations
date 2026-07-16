# Augmented Observations - Replication Pipeline
# ==============================================

SHELL := /bin/bash
R     := Rscript --vanilla

GREEN := \033[0;32m
YELLOW := \033[1;33m
NC := \033[0m

.PHONY: all raw_check raw_exact_check raw_manifest_refresh external_help external_assets_stage external_toolchain_check external_setup_free external_build_phram external_install_stormwindmodel external_run_case external_run_denial_case clean_voi clean_denial data precision cost valuation costbenefit clean help

## Default
all: raw_check data precision cost valuation costbenefit
	@echo -e "$(GREEN)Pipeline complete.$(NC)"

## Help
help:
	@echo -e "$(GREEN)Augmented Observations replication$(NC)"
	@echo
	@echo "Targets:"
	@echo "  all          analysis pipeline on top of pre-generated raw files"
	@echo "  raw_check    verify required pre-generated raw files are present under data/"
	@echo "  raw_exact_check verify raw files against the byte-for-byte baseline manifest"
	@echo "  raw_manifest_refresh rebuild the byte-for-byte raw baseline manifest from the current data/"
	@echo "  external_help explain the separate external-model generation layer"
	@echo "  external_assets_stage copy required non-source external assets from data/code/ into repo-local asset dirs"
	@echo "  external_toolchain_check verify GDL/IDL, other runtimes, staged assets, and compiled helpers for raw regeneration"
	@echo "  external_setup_free set up repo-owned free-toolchain pieces and print remaining platform-specific install steps"
	@echo "  external_build_phram compile phram.exe into code/external_models/assets/prain/bin/"
	@echo "  external_install_stormwindmodel install the staged stormwindmodel tarball into the active R library"
	@echo "  external_run_case generate one storm's standard raw products (set STORM=..., optional SKIP_ERA5_DOWNLOAD=1 SKIP_ERA5_SPLIT=1)"
	@echo "  external_run_denial_case generate one storm's denial raw products (set STORM=..., optional EXPERIMENT=NO|ALL)"
	@echo "  clean_voi    run code/cleaning_voi/* -> data/clean/final_dataset.rds"
	@echo "  clean_denial run code/cleaning_denial/* -> data/clean_denial/*"
	@echo "  data         run code/0.data.r -> output/{summary_dmg,data_denial}.tex + denial_tracks_map.png"
	@echo "  precision    run code/1.precision.r -> Stage 1 tables + stage1_numbers.txt"
	@echo "  cost         run code/2.cost.r -> Stage 2 tables + stage2_numbers.txt"
	@echo "  valuation    run code/3.valuation.r -> Stage 3 tables + stage3_numbers.txt"
	@echo "  costbenefit  run code/4.costbenefit.r -> platform_comparison.tex + stage4_costbenefit_numbers.txt"
	@echo "  clean        remove output/ (preserves data/raw/ and data/clean/*)"
	@echo
	@echo "Prereqs: R packages restored via renv (renv::restore()) and"
	@echo "         pre-generated raw files available under data/raw/ and data/raw_denial/."

raw_check:
	@bash code/external_models/check_raw_inputs.sh

raw_exact_check:
	@python3 code/external_models/verify_raw_manifest.py

raw_manifest_refresh:
	@python3 code/external_models/build_raw_manifest.py

external_help:
	@echo "The external-model layer is separate from the analysis Make pipeline."
	@echo "Use generated raw files under data/raw/ and data/raw_denial/ before running 'make all'."
	@echo "The legacy .pro scripts prefer the free 'gdl' runtime and fall back to 'idl'."
	@echo "Use 'make external_setup_free' to stage repo-owned assets and print the remaining free-software install steps."
	@echo "To generate upstream raw products, use 'make external_run_case STORM=<storm>' or"
	@echo "  'make external_run_denial_case STORM=<storm> EXPERIMENT=NO|ALL'."
	@echo "Use 'make raw_exact_check' after generation to compare those raw files byte-for-byte to the current baseline."
	@echo "See README.md and code/external_models/README.md for details."

external_assets_stage:
	@bash code/external_models/stage_external_assets.sh

external_toolchain_check:
	@bash code/external_models/check_external_toolchain.sh

external_setup_free:
	@bash code/external_models/setup_free_external_toolchain.sh

external_build_phram:
	@bash code/external_models/build_phram.sh

external_install_stormwindmodel:
	@bash code/external_models/install_stormwindmodel.sh

external_run_case:
	@test -n "$(STORM)" || (echo "Set STORM=<stormname>." >&2; exit 1)
	@args=(--storm "$(STORM)"); \
	if [ "$(SKIP_ERA5_DOWNLOAD)" = "1" ]; then args+=(--skip-era5-download); fi; \
	if [ "$(SKIP_ERA5_SPLIT)" = "1" ]; then args+=(--skip-era5-split); fi; \
	bash code/external_models/run_external_case.sh "$${args[@]}"

external_run_denial_case:
	@test -n "$(STORM)" || (echo "Set STORM=<stormname>." >&2; exit 1)
	@experiment="$(if $(EXPERIMENT),$(EXPERIMENT),NO)"; \
	args=(--storm "$(STORM)" --denial 1 --experiment "$$experiment"); \
	if [ "$(SKIP_ERA5_DOWNLOAD)" = "1" ]; then args+=(--skip-era5-download); fi; \
	if [ "$(SKIP_ERA5_SPLIT)" = "1" ]; then args+=(--skip-era5-split); fi; \
	bash code/external_models/run_external_case.sh "$${args[@]}"

## Step 1 - VOI panel
data/clean/final_dataset.rds: code/cleaning_voi/run_all.r \
                              $(wildcard code/cleaning_voi/*.r)
	@echo -e "$(YELLOW)[1/3] VOI cleaning...$(NC)"
	@mkdir -p data/clean
	@$(R) code/cleaning_voi/run_all.r
	@echo -e "$(GREEN)  -> data/clean/final_dataset.rds$(NC)"

clean_voi: data/clean/final_dataset.rds

## Step 2 - Denial panel (depends on VOI outputs)
data/clean_denial/forecast_denial_data_with_lead_times.csv: data/clean/final_dataset.rds \
                                                            code/cleaning_denial/run_all.r \
                                                            $(wildcard code/cleaning_denial/*.r)
	@echo -e "$(YELLOW)[2/3] Denial cleaning...$(NC)"
	@mkdir -p data/clean_denial
	@$(R) code/cleaning_denial/run_all.r
	@echo -e "$(GREEN)  -> data/clean_denial/forecast_denial_data_with_lead_times.csv$(NC)"

clean_denial: data/clean_denial/forecast_denial_data_with_lead_times.csv

## Step 3 - Data section (summary statistics, denial sample, track map)
output/summary_dmg.tex: data/clean/final_dataset.rds data/clean/track_actual.rds \
                        code/0.data.r code/functions_table.r
	@echo -e "$(YELLOW)[3/7] Data section...$(NC)"
	@mkdir -p output
	@$(R) code/0.data.r
	@echo -e "$(GREEN)  -> output/{summary_dmg,data_denial}.tex, denial_tracks_map.png$(NC)"

data: output/summary_dmg.tex

## Step 4 - Stage 1: precision gain (delta), wild cluster bootstrap
output/stage1_numbers.txt: data/clean_denial/forecast_denial_data_with_lead_times.csv \
                           code/1.precision.r code/functions_table.r
	@echo -e "$(YELLOW)[4/7] Stage 1: precision gain (delta)...$(NC)"
	@mkdir -p output
	@$(R) code/1.precision.r
	@echo -e "$(GREEN)  -> Stage 1 tables + output/stage1_numbers.txt$(NC)"

precision: output/stage1_numbers.txt

## Step 5 - Stage 2: marginal cost of forecast error (beta), 3-day-ahead lead
output/stage2_numbers.txt: data/clean/final_dataset.rds code/2.cost.r code/functions_table.r
	@echo -e "$(YELLOW)[5/7] Stage 2: marginal cost (beta)...$(NC)"
	@mkdir -p output
	@$(R) code/2.cost.r
	@echo -e "$(GREEN)  -> Stage 2 tables + output/stage2_numbers.txt$(NC)"

cost: output/stage2_numbers.txt

## Step 6 - Stage 3: valuation (reads the Stage 1 and Stage 2 numbers)
output/stage3_numbers.txt: output/stage1_numbers.txt output/stage2_numbers.txt \
                           data/clean_denial/forecast_denial_data_with_lead_times.csv \
                           data/clean/final_dataset.rds code/3.valuation.r code/functions_table.r
	@echo -e "$(YELLOW)[6/7] Stage 3: valuation...$(NC)"
	@mkdir -p output
	@$(R) code/3.valuation.r
	@echo -e "$(GREEN)  -> Stage 3 tables + output/{stage3_numbers,extended_valuation_numbers}.txt$(NC)"

valuation: output/stage3_numbers.txt

## Step 7 - Stage 4: cost-benefit (reads the Stage 3 numbers)
output/stage4_costbenefit_numbers.txt: output/stage3_numbers.txt code/4.costbenefit.r
	@echo -e "$(YELLOW)[7/7] Stage 4: cost-benefit...$(NC)"
	@mkdir -p output
	@$(R) code/4.costbenefit.r
	@echo -e "$(GREEN)  -> output/platform_comparison.tex + stage4_costbenefit_numbers.txt$(NC)"

costbenefit: output/stage4_costbenefit_numbers.txt

## Cleanup
clean:
	@echo -e "$(YELLOW)Removing output/...$(NC)"
	@rm -rf output
