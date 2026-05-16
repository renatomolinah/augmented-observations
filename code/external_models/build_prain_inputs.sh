#!/bin/bash
set -euo pipefail

################################################################################
# P-RAIN input builder
# ------------------------------------------------------------------------------
# Resolve one storm/lead case from the shared manifest, compile the Fortran
# helper if needed, and create the `storm_data_xxxx.txt` files consumed by
# P-RAIN.
################################################################################

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo_root=$(CDPATH= cd -- "${script_dir}/../.." && pwd)
source "${script_dir}/manifest_utils.sh"

storm=""
lead_hours="72"
compile_only=0
asset_root="${AO_EXTERNAL_ASSET_ROOT:-${script_dir}/assets}"

usage() {
  cat <<'EOF'
Usage: build_prain_inputs.sh --storm NAME [--lead 24|48|72] [--compile-only]
EOF
}

while [ "$#" -gt 0 ]; do
  case "$1" in
    --storm)
      storm="$2"
      shift 2
      ;;
    --lead)
      lead_hours="$2"
      shift 2
      ;;
    --compile-only)
      compile_only=1
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

if ! command -v gfortran >/dev/null 2>&1; then
  echo "gfortran is required to build the P-RAIN input helper." >&2
  exit 1
fi

stormid="$(ao_manifest_field "$storm" 2)"
forecast_timestamp="$(ao_manifest_forecast_timestamp "$storm" "$lead_hours")"
if [ -z "$forecast_timestamp" ]; then
  echo "Storm '${storm}' does not have a ${lead_hours}h forecast timestamp in $(ao_manifest_path)." >&2
  exit 1
fi

runtime_tag="$(printf '%s' "$forecast_timestamp" | cut -c1-10)"
primitive_mc_input_dir="${repo_root}/data/raw/primitive_inputs/standard/${storm}/mcrall_inputs"
legacy_mc_input_dir="${repo_root}/data/raw/stormwindmodel_inputs/${storm}"
default_mc_input_dir="${AO_STORMWINDMODEL_MC_DIR:-$legacy_mc_input_dir}"
if [ -z "${AO_STORMWINDMODEL_MC_DIR:-}" ] && [ -d "$primitive_mc_input_dir" ]; then
  default_mc_input_dir="$primitive_mc_input_dir"
fi
default_output_dir="${repo_root}/data/raw/prain/DATA/${stormid}_${runtime_tag}"
default_ships_dir="${repo_root}/data/raw/prain/ships"

exe_dir="${asset_root}/prain/bin"
exe_path="${exe_dir}/create_storm_data.exe"
src_dir="${script_dir}/prain/1_create_phram_inputs"
mkdir -p "$exe_dir"

gfortran -c "${src_dir}/utilities.f90" -J "$exe_dir" -o "${exe_dir}/utilities.o"
gfortran "${src_dir}/create_storm_data.f90" "${exe_dir}/utilities.o" -I "$exe_dir" -o "$exe_path"

if [ "$compile_only" -eq 1 ]; then
  echo "Built ${exe_path}"
  exit 0
fi

export AO_PRAIN_ATCF_ID="${AO_PRAIN_ATCF_ID:-$stormid}"
export AO_PRAIN_START_TIME="${AO_PRAIN_START_TIME:-$runtime_tag}"
export AO_PRAIN_MC_INPUT_DIR="${AO_PRAIN_MC_INPUT_DIR:-$default_mc_input_dir}"
export AO_PRAIN_DATA_OUTPUT_DIR="${AO_PRAIN_DATA_OUTPUT_DIR:-$default_output_dir}"
export AO_PRAIN_SHIPS_CACHE_DIR="${AO_PRAIN_SHIPS_CACHE_DIR:-$default_ships_dir}"
export AO_PRAIN_SKIP_SHIPS_DOWNLOAD=1

bash "${script_dir}/fetch_ships_file.sh" --storm "$storm" --lead "$lead_hours" >/dev/null

"$exe_path"
