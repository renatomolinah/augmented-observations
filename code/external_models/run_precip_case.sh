#!/bin/bash
set -euo pipefail

################################################################################
# Precipitation raw-product runner
# ------------------------------------------------------------------------------
# Generate the repo's precipitation raw products for one storm, including
# P-RAIN members, ERA5 swaths, county-level precipitation errors, and the
# denial-side aggregate copies used by the analysis pipeline.
################################################################################

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo_root=$(CDPATH= cd -- "${script_dir}/../.." && pwd)
source "${script_dir}/manifest_utils.sh"
asset_root="${AO_EXTERNAL_ASSET_ROOT:-${script_dir}/assets}"

storm=""
denial=0
experiment="NO"
lead_filter=""
skip_era5_download=0
skip_era5_split=0

usage() {
  cat <<'EOF'
Usage: run_precip_case.sh --storm NAME [--lead 24|48|72] [--denial 0|1] [--experiment NO|ALL] [--skip-era5-download] [--skip-era5-split]
EOF
}

while [ "$#" -gt 0 ]; do
  case "$1" in
    --storm)
      storm="$2"
      shift 2
      ;;
    --lead)
      lead_filter="$2"
      shift 2
      ;;
    --denial)
      denial="$2"
      shift 2
      ;;
    --experiment)
      experiment="$2"
      shift 2
      ;;
    --skip-era5-download)
      skip_era5_download=1
      shift
      ;;
    --skip-era5-split)
      skip_era5_split=1
      shift
      ;;
    --help|-h)
      usage
      exit 0
      ;;
    *)
      echo "Unknown argument: $1" >&2
      usage >&2
      exit 1
      ;;
  esac
done

if [ -z "$storm" ]; then
  echo "--storm is required." >&2
  usage >&2
  exit 1
fi

ao_manifest_require

available_leads="$(ao_manifest_available_leads "$storm")"
if [ -z "$available_leads" ]; then
  echo "Storm '${storm}' does not have any forecast lead timestamps in $(ao_manifest_path)." >&2
  exit 1
fi

if [ -n "$lead_filter" ]; then
  case "$lead_filter" in
    24|48|72) ;;
    *)
      echo "--lead must be one of 24, 48, or 72." >&2
      exit 1
      ;;
  esac
  available_leads="$lead_filter"
fi

stormid="$(ao_manifest_field "$storm" 2)"
primitive_mc_input_root="${repo_root}/data/raw/primitive_inputs/standard/${storm}/mcrall_inputs"
primitive_mc_input_root_denial="${repo_root}/data/raw/primitive_inputs/denial/${storm}/mcrall_inputs"
mc_input_root_standard="${repo_root}/data/raw/stormwindmodel_inputs/${storm}"
mc_input_root_denial="${repo_root}/data/raw_denial/${storm}/wind/inputs"
temp_denial_root="${repo_root}/data/tmp/external_models/denial/${storm}/precip/${experiment}"

if [ -d "$primitive_mc_input_root" ]; then
  mc_input_root_standard="$primitive_mc_input_root"
fi

if [ -d "$primitive_mc_input_root_denial" ]; then
  mc_input_root_denial="$primitive_mc_input_root_denial"
fi

if [ "$denial" -eq 1 ]; then
  export AO_ATCF_PRIMITIVE_DIR="${repo_root}/data/raw/primitive_inputs/denial/${storm}/nhc_files"
else
  export AO_ATCF_PRIMITIVE_DIR="${repo_root}/data/raw/primitive_inputs/standard/${storm}/nhc_files"
fi

export AO_SKIP_ATCF_DOWNLOAD=1
bash "${script_dir}/fetch_atcf_file.sh" --storm "$storm" --file-type bdeck >/dev/null

for lead in $available_leads; do
  export AO_STORMNAME="$storm"

  if [ "$denial" -eq 1 ]; then
    forecast_timestamp="$(ao_manifest_forecast_timestamp "$storm" "$lead")"
    runtime_tag="$(printf '%s' "$forecast_timestamp" | cut -c1-10)"
    export AO_PRAIN_MC_INPUT_DIR="${AO_PRAIN_MC_INPUT_DIR:-$mc_input_root_denial}"
    export AO_PRAIN_DATA_OUTPUT_DIR="${AO_PRAIN_DATA_OUTPUT_DIR:-${temp_denial_root}/DATA/${stormid}_${runtime_tag}}"
    export AO_PRAIN_INPUT_DIR="${AO_PRAIN_INPUT_DIR:-${temp_denial_root}/DATA/${stormid}_${runtime_tag}}"
    export AO_PRAIN_OUTPUT_DIR="${AO_PRAIN_OUTPUT_DIR:-${temp_denial_root}/phram/${lead}}"
  else
    export AO_PRAIN_MC_INPUT_DIR="${AO_PRAIN_MC_INPUT_DIR:-$mc_input_root_standard}"
  fi

  bash "${script_dir}/build_prain_inputs.sh" --storm "$storm" --lead "$lead"
  bash "${script_dir}/run_prain_case.sh" --storm "$storm" --lead "$lead"

  unset AO_PRAIN_DATA_OUTPUT_DIR AO_PRAIN_INPUT_DIR AO_PRAIN_OUTPUT_DIR
done

export AO_STORMNAME="$storm"

if [ -z "${AO_WGRIB_EXE:-}" ] && [ -x "${asset_root}/bin/wgrib" ]; then
  export AO_WGRIB_EXE="${asset_root}/bin/wgrib"
fi

if [ "$skip_era5_download" -ne 1 ]; then
  python3 "${script_dir}/prain/4_get_era5_rainfall/GetERA5-hourlyprecip.py"
fi

if [ "$skip_era5_split" -ne 1 ]; then
  bash "${script_dir}/split_era5_grib.sh" --storm "$storm"
fi

bash "${script_dir}/run_idl_procedure.sh" \
  --pro-file "${script_dir}/prain/5_create_era5_rain_swath/tc_rain_swath_era5.pro" \
  --procedure tc_rain_swath_era5 \
  --workdir "$repo_root"

if [ "$denial" -eq 1 ]; then
  export AO_PHRAM_DENIAL=1
  export AO_DENIAL_EXPERIMENT="$experiment"
  export AO_PHRAM_INPUT_DIR="${temp_denial_root}/phram/"
  export AO_PHRAM_OUTPUT_DIR="${repo_root}/data/raw_denial/${storm}/precip/"
else
  export AO_PHRAM_DENIAL=0
fi

bash "${script_dir}/run_idl_procedure.sh" \
  --pro-file "${script_dir}/prain/6_calculate_county_stats/phram_county_stats.pro" \
  --procedure phram_county_stats \
  --workdir "$repo_root"

if [ "$denial" -eq 1 ]; then
  AO_STORM_LIST="$storm" AO_DENIAL_FILE_TYPES="$experiment" \
    Rscript --vanilla "${repo_root}/code/cleaning_denial/clean_rain_errors_denial.R"
fi
