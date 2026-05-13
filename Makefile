# Augmented Observations - Replication Pipeline
# ==============================================

SHELL := /bin/bash
R     := Rscript --vanilla

GREEN := \033[0;32m
YELLOW := \033[1;33m
NC := \033[0m

.PHONY: all clean_voi clean_denial report appendix workshop clean help

## Default
all: report appendix workshop
	@echo -e "$(GREEN)Pipeline complete.$(NC)"

## Help
help:
	@echo -e "$(GREEN)Augmented Observations replication$(NC)"
	@echo
	@echo "Targets:"
	@echo "  all          full pipeline: VOI cleaning -> denial cleaning -> NOAA_report.R -> appendix -> workshop"
	@echo "  clean_voi    run code/cleaning_voi/* -> data/clean/final_dataset.rds"
	@echo "  clean_denial run code/cleaning_denial/* -> data/clean_denial/*"
	@echo "  report       run code/NOAA_report.R -> output/{table_voi,summary_dmg,data_denial,noaa_report_controls_table}.tex + output/augmented_panel.png"
	@echo "  appendix     run code/appendix.R -> output/app{A,B}_*.tex (incl. appA_valuations) + output/appC_per_storm.tex"
	@echo "  workshop     run code/workshop_responses.R -> output/appD_*.tex"
	@echo "  clean        remove output/ (preserves data/raw/ and data/clean/*)"
	@echo
	@echo "Prereqs: R packages restored via renv (renv::restore()) and"
	@echo "         data/ symlinked or populated with raw/, clean/, raw_denial/, clean_denial/."

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
