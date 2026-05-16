#!/bin/bash
set -euo pipefail

################################################################################
# Wind raw-product runner
# ------------------------------------------------------------------------------
# Generate the repo's wind-model raw products for one storm, including the
# county-grid swaths and the final `*_errors.csv` deliverable.
################################################################################

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo_root=$(CDPATH= cd -- "${script_dir}/../.." && pwd)
source "${script_dir}/manifest_utils.sh"

storm=""
denial=0
experiment="NO"

usage() {
  cat <<'EOF'
Usage: run_wind_case.sh --storm NAME [--denial 0|1] [--experiment NO|ALL]
EOF
}

while [ "$#" -gt 0 ]; do
  case "$1" in
    --storm)
      storm="$2"
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

stormid="$(ao_manifest_field "$storm" 2)"
year="$(ao_manifest_field "$storm" 3)"
temp_root="${repo_root}/data/tmp/external_models/wind_mc"

if [ "$denial" -eq 1 ]; then
  export AO_ATCF_PRIMITIVE_DIR="${repo_root}/data/raw/primitive_inputs/denial/${storm}/nhc_files"
else
  export AO_ATCF_PRIMITIVE_DIR="${repo_root}/data/raw/primitive_inputs/standard/${storm}/nhc_files"
fi

export AO_SKIP_ATCF_DOWNLOAD=1
bash "${script_dir}/fetch_atcf_file.sh" --storm "$storm" --file-type bdeck >/dev/null
if [ "$denial" -eq 0 ]; then
  bash "${script_dir}/fetch_atcf_file.sh" --storm "$storm" --file-type adeck >/dev/null
fi

run_one_case() {
  local type="$1"
  local model="$2"
  local output_dir="$3"

  export AO_STORMNAME="$storm"
  export AO_STORMWINDMODEL_TYPE="$type"
  export AO_STORMWINDMODEL_MODEL="$model"
  export AO_STORMWINDMODEL_DENIAL="$denial"
  export AO_DENIAL_EXPERIMENT="$experiment"
  export AO_STORMWINDMODEL_OUTPUT_DIR="$output_dir"

  bash "${script_dir}/run_idl_procedure.sh" \
    --pro-file "${script_dir}/stormwindmodel/create_inputs/stormwindmodel_inputs.pro" \
    --procedure stormwindmodel_inputs \
    --workdir "$repo_root"

  Rscript --vanilla "${script_dir}/stormwindmodel/run_model/hurricane_processing.R"
}

if [ "$denial" -eq 1 ]; then
  final_dir="${repo_root}/data/raw_denial/${storm}/wind"
  mc_output_dir="${temp_root}/raw_denial/${storm}/${experiment}"
  mkdir -p "$final_dir" "$mc_output_dir"

  run_one_case "bdeck" "BEST" "$final_dir"
  run_one_case "adeck" "HWRF" "$final_dir"
  run_one_case "mc" "HWRF" "$mc_output_dir"

  tarball="${final_dir}/${storm}_mc_windswath_${experiment}.tar.gz"
  tar -czf "$tarball" -C "$mc_output_dir" .

  AO_STORM_LIST="$storm" AO_DENIAL_FILE_TYPES="$experiment" \
    Rscript --vanilla "${repo_root}/code/cleaning_denial/clean_wind_errors_denial.r"
else
  final_dir="${repo_root}/data/raw/wind_error/all_forecasts"
  mc_output_dir="${temp_root}/${storm}"
  temp_tar_dir="${repo_root}/data/tmp/external_models/wind_mc_tarballs"
  temp_tarball="${temp_tar_dir}/${storm}_mc_windswath.tar.gz"
  mkdir -p "$final_dir" "$mc_output_dir" "$temp_tar_dir"

  run_one_case "bdeck" "BEST" "$final_dir"
  run_one_case "adeck" "OFCL" "$final_dir"
  run_one_case "adeck" "OCD5" "$final_dir"
  run_one_case "mc" "OFCL" "$mc_output_dir"

  tar -czf "$temp_tarball" -C "$mc_output_dir" .

  Rscript --vanilla "${script_dir}/build_wind_errors.R" \
    "$storm" \
    "$temp_tarball" \
    "${repo_root}/data/raw/wind_error/all_forecasts" \
    "${repo_root}/data/raw/wind_error" \
    "${stormid}${year}"
fi
