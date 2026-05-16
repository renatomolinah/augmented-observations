#!/bin/bash
set -euo pipefail

################################################################################
# External toolchain checker
# ------------------------------------------------------------------------------
# Verify that required runtimes, staged assets, and compiled helpers exist for
# upstream raw regeneration.
################################################################################

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
asset_root="${AO_EXTERNAL_ASSET_ROOT:-${script_dir}/assets}"

missing=0

check_cmd() {
  local label="$1"
  local cmd_name="$2"
  if command -v "$cmd_name" >/dev/null 2>&1; then
    printf '[ok] %s\n' "$label"
  else
    printf '[missing] %s (%s)\n' "$label" "$cmd_name" >&2
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

check_idl_backend() {
  local requested_cmd="${AO_IDL_CMD:-}"

  if [ -n "$requested_cmd" ]; then
    if command -v "$requested_cmd" >/dev/null 2>&1; then
      printf '[ok] procedure runtime via AO_IDL_CMD (%s)\n' "$requested_cmd"
    else
      printf '[missing] procedure runtime via AO_IDL_CMD (%s)\n' "$requested_cmd" >&2
      missing=1
    fi
    return
  fi

  if command -v gdl >/dev/null 2>&1; then
    printf '[ok] free procedure runtime (gdl)\n'
    return
  fi

  if command -v idl >/dev/null 2>&1; then
    printf '[ok] proprietary procedure runtime fallback (idl)\n'
    return
  fi

  printf '[missing] procedure runtime (install gdl or idl)\n' >&2
  missing=1
}

check_wgrib_backend() {
  local requested_cmd="${AO_WGRIB_EXE:-}"

  if [ -n "$requested_cmd" ]; then
    if [ -x "$requested_cmd" ]; then
      printf '[ok] wgrib via AO_WGRIB_EXE (%s)\n' "$requested_cmd"
    else
      printf '[missing] wgrib via AO_WGRIB_EXE (%s)\n' "$requested_cmd" >&2
      missing=1
    fi
    return
  fi

  if command -v wgrib >/dev/null 2>&1; then
    printf '[ok] wgrib\n'
    return
  fi

  if [ -x "${asset_root}/bin/wgrib" ]; then
    printf '[ok] staged local wgrib (%s)\n' "${asset_root}/bin/wgrib"
    return
  fi

  printf '[missing] wgrib (wgrib)\n' >&2
  missing=1
}

check_cmd "R" R
check_cmd "Rscript" Rscript
check_cmd "Python" python3
check_idl_backend
check_wgrib_backend
check_cmd "grib_filter" grib_filter

if command -v gfortran >/dev/null 2>&1; then
  printf '[ok] gfortran\n'
else
  if [ -x "${asset_root}/prain/bin/phram.exe" ]; then
    printf '[ok] phram.exe already built; gfortran not required for rerun\n'
  else
    printf '[missing] gfortran (needed to build phram.exe)\n' >&2
    missing=1
  fi
fi

check_path "staged stormwindmodel tarball" "${asset_root}/stormwindmodel/stormwindmodel_0.1.4.tar.gz"
check_path "staged P-RAIN topo file" "${asset_root}/prain/topo/US_topo_10_50N_50_130W.txt"
check_path "staged county stats sav" "${asset_root}/prain/idl/us_county_stats.sav"
check_path "staged Puerto Rico county stats sav" "${asset_root}/prain/idl/us_county_stats_puertorico.sav"
check_path "compiled phram.exe" "${asset_root}/prain/bin/phram.exe"

if Rscript --vanilla -e "library(stormwindmodel)" >/dev/null 2>&1; then
  printf '[ok] stormwindmodel R package installed\n'
else
  printf '[missing] stormwindmodel R package (run code/external_models/install_stormwindmodel.sh)\n' >&2
  missing=1
fi

if python3 -c "import cdsapi" >/dev/null 2>&1; then
  printf '[ok] cdsapi Python package installed\n'
else
  printf '[missing] cdsapi Python package\n' >&2
  missing=1
fi

if [ "$missing" -ne 0 ]; then
  cat >&2 <<'EOF'

The external raw-generation toolchain is not complete yet. Stage the Box-backed
assets, install the stormwindmodel package, build phram.exe if needed, and make
sure GDL or IDL, plus wgrib / grib_filter / cdsapi, are available before
expecting a full rerun to work.
EOF
  exit 1
fi

printf '\nExternal raw-generation toolchain looks ready.\n'
