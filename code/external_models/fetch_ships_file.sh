#!/bin/bash
set -euo pipefail

################################################################################
# SHIPS downloader
# ------------------------------------------------------------------------------
# Fetch the SHIPS text file needed by the P-RAIN input builder using curl,
# wget, or a Python fallback, then cache it under `data/raw/prain/ships/`.
################################################################################

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo_root=$(CDPATH= cd -- "${script_dir}/../.." && pwd)
source "${script_dir}/manifest_utils.sh"

storm=""
lead_hours="72"

usage() {
  cat <<'EOF'
Usage: fetch_ships_file.sh --storm NAME [--lead 24|48|72]
EOF
}

download_to_file() {
  local url="$1"
  local destination="$2"

  if command -v curl >/dev/null 2>&1; then
    curl -fsSL "$url" -o "$destination"
    return 0
  fi

  if command -v wget >/dev/null 2>&1; then
    wget -q -O "$destination" "$url"
    return 0
  fi

  python3 - "$url" "$destination" <<'PY'
import sys
import urllib.request

url, destination = sys.argv[1], sys.argv[2]
with urllib.request.urlopen(url) as response, open(destination, "wb") as handle:
    handle.write(response.read())
PY
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
forecast_timestamp="$(ao_manifest_forecast_timestamp "$storm" "$lead_hours")"
if [ -z "$forecast_timestamp" ]; then
  echo "Storm '${storm}' does not have a ${lead_hours}h forecast timestamp in $(ao_manifest_path)." >&2
  exit 1
fi

start_time="${AO_PRAIN_START_TIME:-$(printf '%s' "$forecast_timestamp" | cut -c1-10)}"
ships_base_url="${AO_PRAIN_SHIPS_BASE_URL:-ftp://ftp.nhc.noaa.gov/atcf/archive/MESSAGES}"
ships_cache_dir="${AO_PRAIN_SHIPS_CACHE_DIR:-${repo_root}/data/raw/prain/ships}"

mkdir -p "$ships_cache_dir"

atcf_upper="$(printf '%s' "$stormid" | tr '[:lower:]' '[:upper:]')"
ships_id="${atcf_upper}${start_time:2:2}"
ships_year="${start_time:0:4}"
ships_time="${start_time:2:8}"
ships_file="${ships_cache_dir}/${ships_time}${ships_id}_ships.txt"

if [ -f "$ships_file" ]; then
  printf '%s\n' "$ships_file"
  exit 0
fi

url_root="${ships_base_url%/}"
candidate_urls=(
  "${url_root}/${ships_year}/stext/${ships_time}${ships_id}_ships.txt"
  "${url_root}/${ships_year}/stext/${ships_year}/${ships_time}${ships_id}_ships.txt"
)

tmp_file="${ships_file}.tmp"
cleanup() {
  rm -f "$tmp_file"
}
trap cleanup EXIT

for url in "${candidate_urls[@]}"; do
  if download_to_file "$url" "$tmp_file"; then
    mv "$tmp_file" "$ships_file"
    printf '%s\n' "$ships_file"
    exit 0
  fi
done

echo "Failed to fetch SHIPS file for ${storm} (${lead_hours}h)." >&2
exit 1
