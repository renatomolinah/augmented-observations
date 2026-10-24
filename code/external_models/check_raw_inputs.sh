#!/bin/bash
set -euo pipefail

################################################################################
# Raw input checker
# ------------------------------------------------------------------------------
# Verify that the pre-generated raw files required by the analysis pipeline are
# present before running `make all`.
################################################################################

missing=0

check_glob() {
  local label="$1"
  local pattern="$2"

  if compgen -G "$pattern" > /dev/null; then
    printf '[ok] %s\n' "$label"
  else
    printf '[missing] %s (%s)\n' "$label" "$pattern" >&2
    missing=1
  fi
}

check_path() {
  local label="$1"
  local path="$2"

  if [ -e "$path" ]; then
    printf '[ok] %s\n' "$label"
  else
    printf '[missing] %s (%s)\n' "$label" "$path" >&2
    missing=1
  fi
}

check_glob "VOI wind error files" "data/raw/wind_error/*_errors.csv"
check_glob "VOI precipitation error files" "data/raw/precip_error/*_precip_error*.csv"

# Per-storm coverage check: for every storm in the analysis manifest,
# verify that its wind_error CSV exists. The bare globs above pass even
# when only one storm has files. Continental-US storms only; the Puerto
# Rico storms are checked separately below.
manifest="code/external_models/config/storm_manifest.csv"
if [ -f "$manifest" ]; then
  while IFS=, read -r stormname _rest; do
    [ "$stormname" = "stormname" ] && continue
    [ "$stormname" = "maria" ] && continue
    [ "$stormname" = "fiona" ] && continue
    f="data/raw/wind_error/${stormname}_errors.csv"
    if [ ! -e "$f" ]; then
      printf '[missing] VOI wind error file for %s (%s)\n' "$stormname" "$f" >&2
      missing=1
    fi
  done < "$manifest"
fi

# Puerto Rico landfalls (Maria, Fiona) live in pr/ and enter only the AO_INCLUDE_PR=1 build
if [ "${AO_INCLUDE_PR:-0}" = "1" ]; then
  for storm in maria fiona; do
    check_path "Puerto Rico wind error file for ${storm}" "data/raw/wind_error/pr/${storm}_errors.csv"
    check_path "Puerto Rico precipitation error file for ${storm}" "data/raw/precip_error/pr/${storm}_precip_errors.csv"
  done
fi

if [ -d "data/raw/track_raw" ] || [ -f "data/raw/track_raw.zip" ]; then
  printf '[ok] VOI track raw source\n'
else
  printf '[missing] VOI track raw source (data/raw/track_raw or data/raw/track_raw.zip)\n' >&2
  missing=1
fi

check_glob "Denial wind error files (ALL)" "data/raw_denial/wind_error_ALL/*.csv"
check_glob "Denial wind error files (NO)" "data/raw_denial/wind_error_NO/*.csv"
check_glob "Denial precipitation error files (ALL)" "data/raw_denial/precip_error_ALL/*.csv"
check_glob "Denial precipitation error files (NO)" "data/raw_denial/precip_error_NO/*.csv"
check_path "Workshop Part 1" "data/workshop/Part 1.csv"
check_path "Workshop Part 2" "data/workshop/Part 2.csv"

if [ "$missing" -ne 0 ]; then
  cat >&2 <<'EOF'

Missing raw prerequisites. `make all` is analysis-only and expects pre-generated
raw wind, rain, and track products to already exist under data/.
See README.md and code/external_models/README.md for the external-model layer.
EOF
  exit 1
fi

printf '\nAll required raw analysis inputs are present.\n'
