#!/bin/bash
set -euo pipefail

################################################################################
# External-model storm runner
# ------------------------------------------------------------------------------
# Regenerate the wind, track, and precipitation raw products for one storm.
################################################################################

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)

storm=""
denial=0
experiment="NO"
skip_era5_download=0
skip_era5_split=0

usage() {
  cat <<'EOF'
Usage: run_external_case.sh --storm NAME [--denial 0|1] [--experiment NO|ALL] [--skip-era5-download] [--skip-era5-split]
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

bash "${script_dir}/run_wind_case.sh" --storm "$storm" --denial "$denial" --experiment "$experiment"
bash "${script_dir}/run_track_case.sh" --storm "$storm" --denial "$denial" --experiment "$experiment"

precip_args=(--storm "$storm" --denial "$denial" --experiment "$experiment")
if [ "$skip_era5_download" -eq 1 ]; then
  precip_args+=(--skip-era5-download)
fi
if [ "$skip_era5_split" -eq 1 ]; then
  precip_args+=(--skip-era5-split)
fi

bash "${script_dir}/run_precip_case.sh" "${precip_args[@]}"
