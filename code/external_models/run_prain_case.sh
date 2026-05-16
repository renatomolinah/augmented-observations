#!/bin/bash
set -euo pipefail

################################################################################
# P-RAIN case launcher
# ------------------------------------------------------------------------------
# Resolve one storm/lead combination from the shared manifest, then launch the
# portable P-RAIN wrapper with repo-native defaults.
################################################################################

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
manifest_path="${script_dir}/config/storm_manifest.csv"

storm=""
lead_hours="72"
members="${AO_PRAIN_ENSNUM:-1000}"
method="${AO_PRAIN_METHOD:-sequ}"
runner="${AO_PRAIN_RUNNER:-local}"

usage() {
  cat <<'EOF'
Usage: run_prain_case.sh --storm NAME [--lead 24|48|72] [--members N] [--method sequ|rand] [--runner local|bsub]
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
    --members)
      members="$2"
      shift 2
      ;;
    --method)
      method="$2"
      shift 2
      ;;
    --runner)
      runner="$2"
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

case "$lead_hours" in
  24) forecast_column=5 ;;
  48) forecast_column=6 ;;
  72) forecast_column=7 ;;
  *)
    echo "--lead must be one of 24, 48, or 72." >&2
    exit 1
    ;;
esac

storm_record=$(
  awk -F, -v storm="$storm" '$1 == storm { print $0 }' "$manifest_path"
)

if [ -z "$storm_record" ]; then
  echo "Storm '${storm}' was not found in ${manifest_path}." >&2
  exit 1
fi

stormid=$(printf '%s\n' "$storm_record" | awk -F, '{print $2}')
forecast_timestamp=$(printf '%s\n' "$storm_record" | awk -F, -v column="$forecast_column" '{print $column}')

if [ -z "$forecast_timestamp" ]; then
  echo "Storm '${storm}' does not have a ${lead_hours}h forecast timestamp in the shared manifest." >&2
  exit 1
fi

runtime_tag=$(printf '%s' "$forecast_timestamp" | cut -c1-10)

export AO_STORMNAME="$storm"
export AO_PRAIN_LEAD_HOURS="$lead_hours"
export AO_PRAIN_STAG="${stormid}_${runtime_tag}"
export AO_PRAIN_ATCF_ID="$stormid"
export AO_PRAIN_START_TIME="$runtime_tag"
export AO_PRAIN_ENSNUM="$members"
export AO_PRAIN_METHOD="$method"
export AO_PRAIN_RUNNER="$runner"

echo "Launching P-RAIN for ${storm} (${lead_hours}h, ${AO_PRAIN_STAG})."
exec bash "${script_dir}/prain/3_run_phram/prain_wrapper.sh"
