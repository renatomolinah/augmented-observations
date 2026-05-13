# augmented-observations

Replication pipeline for the NOAA report on the socio-economic value of
augmented hurricane observations.

The manuscript itself lives on Overleaf; this repo holds the code that
produces the tables, figures, and in-text numbers the manuscript imports.

## Setup

1. **Data**: symlink `data/` to the Box `augmented-observations/data/` folder
   (or otherwise populate `data/raw/`, `data/raw_denial/`,
   `data/workshop/Part 1.csv`, `data/workshop/Part 2.csv`).
   ```
   ln -s ~/Library/CloudStorage/Box-Box/augmented-observations/data data
   ```
2. **R packages**: open the project (`augmented-observations.Rproj`) and run
   `renv::restore()` to install the locked package versions.

## Run

```
make help      # list targets
make all       # full pipeline: clean_voi -> clean_denial -> report -> appendix -> workshop
make clean_voi      # VOI panel cleaning only
make clean_denial   # denial panel cleaning only (depends on clean_voi)
make report         # NOAA_report.R: main tables + Stages 1-4
make appendix       # appendix.R: Appendices A, B, C tables
make workshop       # workshop_responses.R: Appendix D tables
make clean          # remove output/
```

Outputs land in `output/` (gitignored). The relevant `.tex` / `.png` files are
manually copied into the Overleaf project for the manuscript build.

| Stage | Files |
|---|---|
| Main report (`make report`) | `table_voi.tex`, `summary_dmg.tex`, `data_denial.tex`, `noaa_report_controls_table.tex`, `augmented_panel.png`, `stage{2,3,4}_*numbers.txt` |
| Appendices A/B/C (`make appendix`) | `appA_{outcomes,valuations,jackknife}.tex`, `appB_{leadtime,intensity,intensity_maria,jackknife,maria,jackknife_maria}.tex`, `appC_per_storm.tex` |
| Workshop (`make workshop`) | `appD_{sample,likert_part1,likert_part2,quotes_part1,quotes_part2}.tex` |

## Layout

```
code/
  NOAA_report.R          main report: Stage 0 tables + Stage 1 (beta) + Stage 2 (delta) + Stages 3-4 (valuation, cost-benefit)
  appendix.R             Appendices A (beta robustness), B (delta robustness), C (per-storm decomposition)
  workshop_responses.R   Appendix D tables from data/workshop/Part {1,2}.csv
  cleaning_voi/          VOI panel cleaning (37-storm continental-US panel, 2005-2024)
  cleaning_denial/       denial sample cleaning (1.01 -> 1.03)
  exploratory/           Quarto notebooks
data/                    symlink to Box (gitignored)
  raw/, clean/             VOI inputs and processed panel
  raw_denial/, clean_denial/  denial archive
  workshop/                Qualtrics CSV exports for Appendix D
output/                  generated tables and figures (gitignored)
Makefile                 dependency-tracking pipeline
renv.lock                pinned R package versions
```

The manuscript Overleaf project includes the contents of `output/` (manually
synced) plus its own `latex_extras/`, `references/`, `figures/`, and
`NOAA_report.tex`; nothing manuscript-side is tracked in this repo.
