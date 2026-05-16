# P-RAIN Workflow

This subdirectory collects the upstream code used to generate rainfall swaths
and county-level precipitation error files.

## Contents

- `1_create_phram_inputs/`: Fortran utilities that prepare ensemble inputs.
- `2_phram_model/`: Core P-RAIN source.
- `3_run_phram/`: Batch scripts for launching the PHRAM jobs.
- `4_get_era5_rainfall/`: ERA5 download and split helpers.
- `5_create_era5_rain_swath/`: IDL code for observed rainfall swaths.
- `6_calculate_county_stats/`: IDL code that aggregates ensemble and observed
  rainfall into county-level error outputs.

## Downstream Outputs

These scripts feed:

- `data/raw/precip_error/*.csv`
- `data/raw_denial/*/precip/*.csv`
- summary files later merged into the VOI and denial cleaning pipelines

## Notes

Some non-source dependencies remain outside git, including the large topo file,
IDL `.sav` binaries, and the archived `eccodes` bundle listed in the parent
README.

The repo-native defaults now use these intermediate directories:

- `data/raw/prain/DATA/<atcf_id>_<start_time>/` for `storm_data_xxxx.txt`
  inputs created by `1_create_phram_inputs/`
- `data/raw/prain/phram/<storm>/<lead>/` for member-level `prain_xxxx.txt.gz`
  outputs written by `3_run_phram/`
- `data/raw/prain/ships/` for cached SHIPS text files used during input
  creation

Use [`run_prain_case.sh`](../run_prain_case.sh) to launch a manifest-driven
P-RAIN case without editing `prain_job.sh` or `prain_wrapper.sh`.
