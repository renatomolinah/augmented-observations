#!/bin/bash
set -euo pipefail

################################################################################
# Track-cone runner
# ------------------------------------------------------------------------------
# Generate the county-cone raw track products for one storm across the forecast
# combinations used by the replication repo.
################################################################################

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo_root=$(CDPATH= cd -- "${script_dir}/../.." && pwd)

storm=""
denial=0
experiment="NO"

usage() {
  cat <<'EOF'
Usage: run_track_case.sh --storm NAME [--denial 0|1] [--experiment NO|ALL]
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

if [ "$denial" -eq 1 ]; then
  export AO_ATCF_PRIMITIVE_DIR="${repo_root}/data/raw/raw_raw/denial/${storm}/nhc_files"
else
  export AO_ATCF_PRIMITIVE_DIR="${repo_root}/data/raw/raw_raw/standard/${storm}/nhc_files"
fi

export AO_SKIP_ATCF_DOWNLOAD=1
bash "${script_dir}/fetch_atcf_file.sh" --storm "$storm" --file-type bdeck >/dev/null
if [ "$denial" -eq 0 ]; then
  bash "${script_dir}/fetch_atcf_file.sh" --storm "$storm" --file-type adeck >/dev/null
fi

run_one_case() {
  local type="$1"
  local model="$2"

  export AO_STORMNAME="$storm"
  export AO_TRACK_TYPE="$type"
  export AO_TRACK_MODEL="$model"
  export AO_TRACK_DENIAL="$denial"
  export AO_DENIAL_EXPERIMENT="$experiment"

  bash "${script_dir}/run_idl_procedure.sh" \
    --pro-file "${script_dir}/counties_in_cone/points_in_cone.pro" \
    --procedure points_in_cone \
    --workdir "$repo_root"
}

if [ "$denial" -eq 1 ]; then
  run_one_case "bdeck" "BEST"
  run_one_case "adeck" "HWRF"
else
  run_one_case "bdeck" "BEST"
  run_one_case "adeck" "OFCL"
  run_one_case "adeck" "OCD5"
fi
