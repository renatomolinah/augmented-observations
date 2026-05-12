# augmented-observations

Replication pipeline for the NOAA report on the socio-economic value of
augmented hurricane observations.

## Setup

1. **Data**: symlink `data/` to the Box `augmented-observations/data/` folder
   (or otherwise populate `data/raw/`, `data/raw_denial/`).
   ```
   ln -s ~/Library/CloudStorage/Box-Box/augmented-observations/data data
   ```
2. **R packages**: open the project (`augmented-observations.Rproj`) and run
   `renv::restore()` to install the locked package versions.

## Run

```
make help      # list targets
make all       # full pipeline: VOI cleaning -> denial cleaning -> NOAA report
make clean_voi      # just the VOI cleaning
make clean_denial   # just the denial cleaning (depends on clean_voi)
make report         # just NOAA_report.R (depends on both)
make clean          # remove output/
```

Outputs land in `output/`:
- `table_voi.tex`, `summary_dmg.tex`, `data_denial.tex`, `noaa_report_controls_table.tex`
- `augmented_panel.png`

## Layout

```
code/
  NOAA_report.R          main report: Stage 0 tables + Stage 1 (beta) + Stage 2 (delta) + Stage 3 valuation
  cleaning_voi/          HF VOI cleaning scripts (patched for 37-storm panel through 2024)
  cleaning_denial/       denial cleaning (1.01 -> 1.03)
  2.0*.r                 supplementary denial results
  code_VOI/              reference: original Molina2024 paper code (read-only)
  exploratory/           Quarto notebooks
augmented_observations/  Overleaf source for the NOAA report
data/                    symlink to Box (gitignored)
output/                  generated tables and figures (gitignored)
```
