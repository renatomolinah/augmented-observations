#!/bin/bash
set -euo pipefail

################################################################################
# Storm manifest helpers
# ------------------------------------------------------------------------------
# Shared shell helpers for resolving storm metadata from the repo manifest.
################################################################################

ao_manifest_script_dir=$(CDPATH= cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
AO_MANIFEST_PATH_DEFAULT="${ao_manifest_script_dir}/config/storm_manifest.csv"

ao_manifest_path() {
  printf '%s\n' "${AO_STORM_MANIFEST:-$AO_MANIFEST_PATH_DEFAULT}"
}

ao_manifest_require() {
  local manifest_path
  manifest_path="$(ao_manifest_path)"
  if [ ! -f "$manifest_path" ]; then
    echo "Storm manifest not found: ${manifest_path}" >&2
    return 1
  fi
}

ao_manifest_row() {
  local storm="$1"
  local manifest_path
  manifest_path="$(ao_manifest_path)"
  awk -F, -v storm="$storm" 'NR > 1 && $1 == storm { print; exit }' "$manifest_path"
}

ao_manifest_field() {
  local storm="$1"
  local field_index="$2"
  local row
  row="$(ao_manifest_row "$storm")"
  if [ -z "$row" ]; then
    echo "Storm '${storm}' was not found in $(ao_manifest_path)." >&2
    return 1
  fi
  printf '%s\n' "$row" | awk -F, -v column="$field_index" '{print $column}'
}

ao_manifest_lead_column() {
  case "$1" in
    24) printf '5\n' ;;
    48) printf '6\n' ;;
    72) printf '7\n' ;;
    *)
      echo "Unsupported lead '$1'; expected 24, 48, or 72." >&2
      return 1
      ;;
  esac
}

ao_manifest_forecast_timestamp() {
  local storm="$1"
  local lead="$2"
  local column
  column="$(ao_manifest_lead_column "$lead")"
  ao_manifest_field "$storm" "$column"
}

ao_manifest_available_leads() {
  local storm="$1"
  local lead timestamp leads=()

  for lead in 24 48 72; do
    timestamp="$(ao_manifest_forecast_timestamp "$storm" "$lead")"
    if [ -n "$timestamp" ]; then
      leads+=("$lead")
    fi
  done

  printf '%s\n' "${leads[*]}"
}
