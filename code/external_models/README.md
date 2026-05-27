# External Model Code

This directory holds the upstream generator code that produces the wind, rain,
and track-cone raw inputs consumed later by the main replication pipeline.

The main `make all` workflow does not rerun these scripts. Instead, the
cleaning code reads their outputs from `data/raw/` and `data/raw_denial/`.

The intended replication boundary is:

- `make raw_check`: confirm the required raw wind, rain, and track products are
  present.
- `make raw_exact_check`: verify those raw products match the current baseline
  byte-for-byte using `config/raw_output_manifest.tsv`.
- `make all`: run the analysis pipeline on top of those raw files.

For an upstream rerun workflow, the expected sequence is:

1. Run `make external_assets_stage`.
2. Run `make external_setup_free`.
3. Run `make external_toolchain_check`.
4. Run `make external_run_case STORM=<storm>` for standard storms, or
   `make external_run_denial_case STORM=<storm> EXPERIMENT=NO|ALL` for denial
   outputs.
5. Run `make raw_exact_check` after replacing the refreshed raw baseline.
6. If that passes, run `make all`.

Primitive upstream inputs are staged separately from generated raw outputs
under `data/raw/primitive_inputs/`, organized by workflow and storm:

- `data/raw/primitive_inputs/standard/<storm>/nhc_files/`
- `data/raw/primitive_inputs/standard/<storm>/mcrall_inputs/`
- `data/raw/primitive_inputs/standard/<storm>/era5/`
- `data/raw/primitive_inputs/denial/<storm>/nhc_files/`
- `data/raw/primitive_inputs/denial/<storm>/mcrall_inputs/`
- `data/raw/primitive_inputs/denial/<storm>/era5/`

Those folders are meant for preserved deck files, Monte Carlo aggregate
inputs, and storm-level ERA5 downloads only. Generated wind, track, and rain
raw products should continue to land under `data/raw/` and `data/raw_denial/`.

The wrapper scripts read those storm folders directly when resolving primitive
inputs for the standard and denial workflows.

For the legacy `.pro` scripts, the wrappers prefer the free GNU Data Language
runtime (`gdl`) and fall back to proprietary `idl` only if `gdl` is not
available.

The main user-facing R, Python, shell, Fortran, and IDL entry points share
storm timing metadata through `config/storm_manifest.csv`. The legacy IDL files
still keep their original in-file tables as fallbacks for older archival storms
that are not represented in the shared manifest.

## Layout

- `stormwindmodel/`: IDL + R + cluster job scripts used to build wind-model
  inputs and county-level wind swaths.
- `prain/`: Fortran + IDL + Python + shell scripts used to run P-RAIN and
  aggregate ERA5 rainfall into county-level precipitation error files.
- `counties_in_cone/`: IDL utilities for cone / track geography used by the
  track-side raw inputs.

## What These Feed

- `stormwindmodel/` underlies files in `data/raw/wind_error/`,
  `data/raw/wind_error/all_forecasts/`, and the denial wind folders under
  `data/raw_denial/*/wind/`.
- `prain/` underlies files in `data/raw/precip_error/` and the denial
  precipitation folders under `data/raw_denial/*/precip/`.
- `counties_in_cone/` underlies the track products in `data/raw/track_raw/`
  and `data/raw_denial/*/track/`.

## Primitive-input staging status

The `primitive_inputs/` directories described above are partially staged:

- **Standard side (`primitive_inputs/standard/<storm>/`)** is fully populated
  for all 37 storms in `config/storm_manifest.csv`. `make external_run_case
  STORM=<name>` can rerun any standard storm end-to-end.
- **Denial side (`primitive_inputs/denial/<storm>/`)** currently only contains
  `irma/`. The other nine 2017–2020 Ditchek-archive storms (`harvey`,
  `florence`, `michael`, `hanna`, `isaias`, `laura`, `maria`, `sally`,
  `dorian`) need their primitives staged before `make
  external_run_denial_case STORM=<name>` will work for them.

This gap does not affect the analysis pipeline. The denial-side rerun path is
only relevant if someone wants to regenerate `data/raw_denial/wind_error_*/`
or `data/raw_denial/precip_error_*/` from scratch — the analysis pipeline
itself consumes those CSVs directly and is fully reproducible with what is
already on disk.

### To stage a denial storm

Create the three folders and populate each as follows:

- `primitive_inputs/denial/<storm>/nhc_files/` — NHC b-deck (BEST track),
  a-deck (forecast), and ATCF best-track files for the storm. Source:
  <https://ftp.nhc.noaa.gov/atcf/archive/>. Mirror the same file inventory
  that exists under `primitive_inputs/standard/irma/nhc_files/`.
- `primitive_inputs/denial/<storm>/mcrall_inputs/` — Monte Carlo ensemble
  inputs produced by the `stormwindmodel` R package (see
  `code/external_models/stormwindmodel/run_model/`) for the dropsonde-ALL and
  dropsonde-NO HWRF analyses described in Ditchek et al. (2023). These are
  not freely downloadable — they require either the original Ditchek archive
  files or a local rerun of `stormwindmodel` against the HWRF outputs that
  produced the denial wind/precip errors. Mirror the inventory under
  `primitive_inputs/standard/irma/mcrall_inputs/`.
- `primitive_inputs/denial/<storm>/era5/` — ERA5 reanalysis grids for the
  storm window (typically landfall ± 5 days). Source: Copernicus CDS via
  `cdsapi`; see `code/external_models/prain/4_get_era5_rainfall/` for the
  request templates and `split_era5_grib.sh` for the post-processing.

Once a denial storm's three folders are populated, validate with `make
external_toolchain_check`, then exercise the rerun with `make
external_run_denial_case STORM=<storm> EXPERIMENT=ALL` (and again with
`EXPERIMENT=NO`). Compare the regenerated `data/raw_denial/*.csv` against
the current baseline with `make raw_exact_check`.

## Wrappers

The rerun wrappers that drive the portable entry points are:

- `run_external_case.sh`: full standard wind + track + precipitation rerun for
  one storm.
- `run_wind_case.sh`: storm-level wind rerun, including generation of the final
  `data/raw/wind_error/<storm>_errors.csv` file.
- `run_track_case.sh`: storm-level cone and county-track rerun.
- `run_precip_case.sh`: storm-level P-RAIN + ERA5 rerun.
- `setup_free_external_toolchain.sh`: stages repo-owned assets, installs the
  staged R package, and prints the remaining free-software install steps for
  the current platform.
- [`run_prain_case.sh`](run_prain_case.sh): manifest-driven helper for launching
  one P-RAIN storm/lead combination without editing source files.
- [`config/raw_output_manifest.tsv`](config/raw_output_manifest.tsv): exact-match
  baseline for the current raw files.

## Omitted Heavyweight Assets

These files were intentionally left out of git because they are bulky compiled
artifacts, binary saves, or external dependency bundles rather than source:

- `data/code/P-RAIN/2_phram_model/US_topo_10_50N_50_130W.txt`
- `data/code/P-RAIN/4_get_ERA5_rainfall/eccodes-2.19.1-Source.tar.gz`
- `data/code/P-RAIN/6_calculate_county_stats/us_county_stats.sav`
- `data/code/P-RAIN/6_calculate_county_stats/us_county_stats_puertorico.sav`
- `data/code/run_stormwindmodel/stormwindmodel_0.1.4.tar.gz`

Use `make external_toolchain_check` before rerunning these workflows on a new
machine.
