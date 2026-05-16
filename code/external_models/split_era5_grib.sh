#!/bin/bash
set -euo pipefail

################################################################################
# ERA5 GRIB splitter
# ------------------------------------------------------------------------------
# Split one storm-level ERA5 GRIB download into hourly `.grib1` files using the
# bundled `split.rule.hourly` rule file.
################################################################################

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo_root=$(CDPATH= cd -- "${script_dir}/../.." && pwd)
source "${script_dir}/manifest_utils.sh"

storm=""

usage() {
  cat <<'EOF'
Usage: split_era5_grib.sh --storm NAME
EOF
}

while [ "$#" -gt 0 ]; do
  case "$1" in
    --storm)
      storm="$2"
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

if ! command -v grib_filter >/dev/null 2>&1; then
  echo "grib_filter is required to split ERA5 downloads." >&2
  exit 1
fi

stormid="$(ao_manifest_field "$storm" 2)"
year="$(ao_manifest_field "$storm" 3)"
era5_dir="${AO_PRAIN_ERA5_DIR:-${repo_root}/data/raw/era5}"
grib_file="${era5_dir}/${stormid}${year}.grib"
rule_file="${script_dir}/prain/4_get_era5_rainfall/split.rule.hourly"

if [ ! -f "$grib_file" ]; then
  echo "ERA5 GRIB file not found: ${grib_file}" >&2
  exit 1
fi

(
  cd "$era5_dir"
  grib_filter "$rule_file" "$grib_file"
)
