# Augmented Observations - Replication Pipeline
# ==============================================

SHELL := /bin/bash
R     := Rscript --vanilla

GREEN := \033[0;32m
YELLOW := \033[1;33m
NC := \033[0m

.PHONY: all analysis_all raw_check raw_exact_check raw_exact_check_storm raw_manifest_refresh external_help external_assets_stage external_toolchain_check external_setup_free external_build_phram external_install_stormwindmodel external_run_case external_run_denial_case clean_voi clean_denial report appendix workshop clean help

## Default
all: analysis_all

analysis_all: raw_check report appendix workshop
	@echo -e "$(GREEN)Pipeline complete.$(NC)"

## Help
help:
	@echo -e "$(GREEN)Augmented Observations replication$(NC)"
	@echo
	@echo "Targets:"
	@echo "  all          analysis pipeline on top of pre-generated raw files"
	@echo "  analysis_all same as 'all'"
	@echo "  raw_check    verify required pre-generated raw files are present under data/"
	@echo "  raw_exact_check verify raw files against the byte-for-byte baseline manifest"
	@echo "  raw_exact_check_storm verify one storm's raw files against the byte-for-byte baseline manifest (set STORM=...)"
	@echo "  raw_manifest_refresh rebuild the byte-for-byte raw baseline manifest from the current data/"
	@echo "  external_help explain the separate external-model generation layer"
	@echo "  external_assets_stage copy required non-source external assets from data/code/ into repo-local asset dirs"
	@echo "  external_toolchain_check verify GDL/IDL, other runtimes, staged assets, and compiled helpers for raw regeneration"
	@echo "  external_setup_free set up repo-owned free-toolchain pieces and print remaining platform-specific install steps"
	@echo "  external_build_phram compile phram.exe into code/external_models/assets/prain/bin/"
	@echo "  external_install_stormwindmodel install the staged stormwindmodel tarball into the active R library"
	@echo "  external_run_case regenerate one storm's standard raw products (set STORM=..., optional SKIP_ERA5_DOWNLOAD=1 SKIP_ERA5_SPLIT=1)"
	@echo "  external_run_denial_case regenerate one storm's denial raw products (set STORM=..., optional EXPERIMENT=NO|ALL)"
	@echo "  clean_voi    run code/cleaning_voi/* -> data/clean/final_dataset.rds"
	@echo "  clean_denial run code/cleaning_denial/* -> data/clean_denial/*"
	@echo "  report       run code/NOAA_report.R -> output/{table_voi,summary_dmg,data_denial,noaa_report_controls_table}.tex + output/augmented_panel.png"
	@echo "  appendix     run code/appendix.R -> output/app{A,B}_*.tex (incl. appA_valuations) + output/appC_per_storm.tex"
	@echo "  workshop     run code/workshop_responses.R -> output/appD_*.tex"
	@echo "  clean        remove output/ (preserves data/raw/ and data/clean/*)"
	@echo
	@echo "Prereqs: R packages restored via renv (renv::restore()) and"
	@echo "         pre-generated raw files available under data/raw/ and data/raw_denial/."

raw_check:
	@bash code/external_models/check_raw_inputs.sh

raw_exact_check:
	@python3 code/external_models/verify_raw_manifest.py

raw_exact_check_storm:
	@test -n "$(STORM)" || (echo "Set STORM=<stormname>." >&2; exit 1)
	@python3 code/external_models/verify_raw_manifest.py --storm "$(STORM)"

raw_manifest_refresh:
	@python3 code/external_models/build_raw_manifest.py

external_help:
	@echo "The external-model layer is separate from the analysis Make pipeline."
	@echo "Use generated raw files under data/raw/ and data/raw_denial/ before running 'make all'."
	@echo "The legacy .pro scripts now prefer the free 'gdl' runtime and fall back to 'idl'."
	@echo "Use 'make external_setup_free' to stage repo-owned assets and print the remaining free-software install steps."
	@echo "To rerun upstream products, use 'make external_run_case STORM=<storm>' or"
	@echo "  'make external_run_denial_case STORM=<storm> EXPERIMENT=NO|ALL'."
	@echo "Use 'make raw_exact_check' after a rerun to compare those raw files byte-for-byte to the current baseline."
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
                              $(wildcard code/cleaning_voi/*.r) \
                              $(wildcard code/cleaning_voi/*.R)
	@echo -e "$(YELLOW)[1/3] VOI cleaning...$(NC)"
	@$(R) code/cleaning_voi/run_all.r
	@echo -e "$(GREEN)  -> data/clean/final_dataset.rds$(NC)"

clean_voi: data/clean/final_dataset.rds

## Step 2 - Denial panel (depends on VOI outputs)
data/clean_denial/forecast_denial_data_with_lead_times.csv: data/clean/final_dataset.rds \
                                                            code/cleaning_denial/run_all.r \
                                                            $(wildcard code/cleaning_denial/*.r) \
                                                            $(wildcard code/cleaning_denial/*.R)
	@echo -e "$(YELLOW)[2/3] Denial cleaning...$(NC)"
	@$(R) code/cleaning_denial/run_all.r
	@echo -e "$(GREEN)  -> data/clean_denial/forecast_denial_data_with_lead_times.csv$(NC)"

clean_denial: data/clean_denial/forecast_denial_data_with_lead_times.csv

## Step 3 - NOAA report (depends on both panels)
output/table_voi.tex: data/clean/final_dataset.rds \
                     data/clean_denial/forecast_denial_data_with_lead_times.csv \
                     code/NOAA_report.R \
                     code/functions_table.r
	@echo -e "$(YELLOW)[3/3] NOAA report...$(NC)"
	@mkdir -p output
	@$(R) code/NOAA_report.R
	@echo -e "$(GREEN)  -> output/{table_voi,summary_dmg,data_denial,noaa_report_controls_table}.tex$(NC)"
	@echo -e "$(GREEN)  -> output/augmented_panel.png$(NC)"

report: output/table_voi.tex

## Step 4 - Appendix (robustness + per-storm decomposition)
output/appA_jackknife.tex: data/clean/final_dataset.rds \
                          data/clean_denial/forecast_denial_data_with_lead_times.csv \
                          code/appendix.R \
                          code/functions_table.r
	@echo -e "$(YELLOW)[4/4] Appendix (A: β robustness, B: δ robustness, C: per-storm)...$(NC)"
	@mkdir -p output
	@$(R) code/appendix.R
	@echo -e "$(GREEN)  -> output/app{A,B}_*.tex$(NC)"
	@echo -e "$(GREEN)  -> output/appC_per_storm.tex$(NC)"

appendix: output/appA_jackknife.tex

## Step 5 - Workshop response tabulation (Appendix D)
output/appD_sample.tex: code/workshop_responses.R \
                       data/workshop/Part\ 1.csv \
                       data/workshop/Part\ 2.csv
	@echo -e "$(YELLOW)[5/5] Workshop responses (Appendix D)...$(NC)"
	@mkdir -p output
	@$(R) code/workshop_responses.R
	@echo -e "$(GREEN)  -> output/appD_{sample,likert_part1,likert_part2,quotes_part1,quotes_part2}.tex$(NC)"

workshop: output/appD_sample.tex

## Cleanup
clean:
	@echo -e "$(YELLOW)Removing output/...$(NC)"
	@rm -rf output
