#!/bin/bash
set -euo pipefail

################################################################################
# ATCF deck downloader
# ------------------------------------------------------------------------------
# Fetch and cache one A-deck or B-deck file under `data/raw/atcf/` using curl,
# wget, or a Python fallback.
################################################################################

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo_root=$(CDPATH= cd -- "${script_dir}/../.." && pwd)
source "${script_dir}/manifest_utils.sh"

storm=""
file_type=""

usage() {
  cat <<'EOF'
Usage: fetch_atcf_file.sh --storm NAME --file-type adeck|bdeck
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
    --file-type)
      file_type="$2"
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

if [ -z "$storm" ] || [ -z "$file_type" ]; then
  usage >&2
  exit 1
fi

ao_manifest_require

stormid="$(ao_manifest_field "$storm" 2)"
year="$(ao_manifest_field "$storm" 3)"
current_year="$(date +%Y)"
atcf_dir="${AO_ATCF_DIR:-${repo_root}/data/raw/atcf}"
mkdir -p "$atcf_dir"

if [ "$file_type" = "adeck" ]; then
  file_name="a${stormid}${year}.dat"
  remote_name="${file_name}.gz"
  if [ "$year" = "$current_year" ]; then
    remote_path="aid_public/${remote_name}"
  else
    remote_path="archive/${year}/${remote_name}"
  fi
  final_path="${atcf_dir}/${file_name}"
  download_path="${final_path}.gz"
elif [ "$file_type" = "bdeck" ]; then
  file_name="b${stormid}${year}.dat"
  if [ "$year" = "$current_year" ]; then
    remote_name="${file_name}"
    remote_path="btk/${remote_name}"
    final_path="${atcf_dir}/${file_name}"
    download_path="${final_path}"
  else
    remote_name="${file_name}.gz"
    remote_path="archive/${year}/${remote_name}"
    final_path="${atcf_dir}/${file_name}"
    download_path="${final_path}.gz"
  fi
else
  echo "Unsupported file type: ${file_type}" >&2
  exit 1
fi

if [ -f "$final_path" ]; then
  printf '%s\n' "$final_path"
  exit 0
fi

url_root="https://ftp.nhc.noaa.gov/atcf"
url="${url_root}/${remote_path}"
tmp_path="${download_path}.tmp"
cleanup() {
  rm -f "$tmp_path"
}
trap cleanup EXIT

download_to_file "$url" "$tmp_path"
mv "$tmp_path" "$download_path"

if [[ "$download_path" == *.gz ]]; then
  gzip -df "$download_path"
fi

printf '%s\n' "$final_path"
