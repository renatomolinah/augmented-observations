# augmented-observations

Replication pipeline for the NOAA report on the socio-economic value of
augmented hurricane observations.

## Funding and Citation

Please include the NOAA award number in publications, presentations, reports,
datasets, and code releases derived from this repository. This repository
should cite NOAA Award `NA23OAR4590366-T1-01`.

Suggested language: "This research was supported by NOAA Weather Program
Office (<https://ror.org/01q70bz83>) Award NA23OAR4590366-T1-01. Data and code
are available at <https://github.com/renatomolinah/augmented-observations>."

## Setup

1. **Data**: symlink `data/` to the project data folder
   (or otherwise populate `data/raw/`, `data/raw_denial/`,
   `data/workshop/Part 1.csv`, `data/workshop/Part 2.csv`).
   ```
   ln -s /path/to/data data
   ```
2. **R packages**: open the project (`augmented-observations.Rproj`) and run
   `renv::restore()` to install the locked package versions.

Restricted/private inputs used by the pipeline are documented in
`CODEBOOK_restricted_data.md`.

## Run

```
make help           # list targets
make raw_check      # verify required raw files are present
make raw_exact_check  # verify those raw files match the current baseline byte-for-byte
make external_setup_free  # set up repo-owned free-toolchain pieces and print remaining install steps
make all            # analysis pipeline only: clean_voi -> clean_denial -> report -> appendix -> workshop
make clean_voi      # VOI panel cleaning only
make clean_denial   # denial panel cleaning only (depends on clean_voi)
make report         # NOAA_report.R: main tables + Stages 1-4
make appendix       # appendix.R: Appendices A, B, C tables
make workshop       # workshop_responses.R: Appendix D tables
make clean          # remove output/
```

Outputs land in `output/` (gitignored).

`make all` is intentionally analysis-only. It assumes the upstream wind, rain,
and track raw products have already been generated and placed under
`data/raw/` and `data/raw_denial/`. Run `make raw_check` first if you want a
quick prerequisite check before launching the analysis pipeline.

If the upstream raw-generation layer is rerun and refreshed raw files are
available, run `make raw_exact_check` to compare them byte-for-byte against the
current baseline captured in
`code/external_models/config/raw_output_manifest.tsv`.

The upstream generation path is separate from the analysis pipeline. The
standard path is:

1. `make external_assets_stage`
2. `make external_setup_free`
3. `make external_toolchain_check`
4. `make external_run_case STORM=<storm>`
5. `make raw_exact_check`

For denial reruns, use `make external_run_denial_case STORM=<storm> EXPERIMENT=NO`
or `EXPERIMENT=ALL`, then re-run `make raw_exact_check` once the refreshed
files have replaced the current baseline set.

Primitive upstream inputs should be staged separately from generated raw
products under `data/raw/primitive_inputs/`. That folder is organized by workflow and
storm:

- `data/raw/primitive_inputs/standard/<storm>/nhc_files/`
- `data/raw/primitive_inputs/standard/<storm>/mcrall_inputs/`
- `data/raw/primitive_inputs/standard/<storm>/era5/`
- `data/raw/primitive_inputs/denial/<storm>/nhc_files/`
- `data/raw/primitive_inputs/denial/<storm>/mcrall_inputs/`
- `data/raw/primitive_inputs/denial/<storm>/era5/`

The external-generation wrappers read those storm folders directly when they
build wind, track, and rain raw products.

For example, `data/raw/primitive_inputs/standard/irma/` is the location for
Irma's preserved deck files, Monte Carlo aggregate inputs, and ERA5 downloads
before any wind, track, or rain outputs are generated into `data/raw/` or
`data/raw_denial/`.

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
  functions_table.r      table-rendering helpers (sourced by NOAA_report.R and appendix.R)
  cleaning_voi/          VOI panel cleaning (37-storm continental-US panel, 2005-2024)
  cleaning_denial/       denial sample cleaning (1.01 -> 1.03)
  exploratory/           Quarto notebooks (Ditchek replication, wind-error inference)
  external_models/       archived upstream wind/rain/track generator code
data/                    data folder (gitignored)
  raw/, clean/             VOI inputs and processed panel
  raw_denial/, clean_denial/  denial archive
  workshop/                Qualtrics CSV exports for Appendix D
output/                  generated tables and figures (gitignored)
Makefile                 dependency-tracking pipeline
renv.lock                pinned R package versions
```

## External Model Provenance

The main replication pipeline reads precomputed wind, rain, and track products
from `data/raw/` and `data/raw_denial/`; it does not rerun the upstream
stormwindmodel / P-RAIN / cone-generation workflows during `make all`.

The corresponding generator scripts live in `code/external_models/`. That tree
contains the external modeling steps that produce files such as:

- `data/raw/wind_error/*.csv`
- `data/raw/precip_error/*.csv`
- `data/raw/track_raw/*/track`
- `data/raw_denial/*/{wind,precip,track}/...`

Large compiled assets and machine-specific binaries are not tracked in git. See
[`code/external_models/README.md`](code/external_models/README.md) for
the rerun wrappers, primitive-input layout, and prerequisite checks.

The legacy `.pro` execution layer prefers the free GNU Data Language runtime
(`gdl`) when it is available and falls back to proprietary `idl` if needed.
