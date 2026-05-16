# Stormwindmodel Workflow

This subdirectory collects the upstream code used to produce county-level wind
swath inputs for the analysis.

## Contents

- `create_inputs/`: IDL scripts that turn A-deck, B-deck, or Monte Carlo track
  files into stormwindmodel input CSVs.
- `run_model/`: R and cluster job scripts that run the wind model and write
  county-grid output files.

## Downstream Outputs

These scripts feed:

- `data/raw/wind_error/*.csv`
- `data/raw/wind_error/all_forecasts/*.csv`
- `data/raw_denial/*/wind/*.csv`
- Monte Carlo tarballs stored under `data/raw_denial/*/wind/`

## Notes

Some scripts still assume the original filesystem and HPC job layout. The
canonical repo interface is the downstream raw-data products, not direct local
execution of these scripts.
