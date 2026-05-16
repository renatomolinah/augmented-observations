#!/bin/bash
set -euo pipefail

################################################################################
# External asset stager
# ------------------------------------------------------------------------------
# Copy required non-source external-model assets from the Box-backed data/code
# tree into canonical repo-local asset directories.
################################################################################

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo_root=$(CDPATH= cd -- "${script_dir}/../.." && pwd)
source_root="${AO_EXTERNAL_SOURCE_ROOT:-${repo_root}/data/code}"
asset_root="${AO_EXTERNAL_ASSET_ROOT:-${script_dir}/assets}"

copy_asset() {
  local source_path="$1"
  local dest_path="$2"

  if [ ! -f "$source_path" ]; then
    echo "Missing source asset: $source_path" >&2
    return 1
  fi

  mkdir -p "$(dirname "$dest_path")"
  cp "$source_path" "$dest_path"
  echo "Staged $(basename "$dest_path")"
}

copy_asset \
  "${source_root}/P-RAIN/6_calculate_county_stats/us_county_stats.sav" \
  "${asset_root}/prain/idl/us_county_stats.sav"
copy_asset \
  "${source_root}/P-RAIN/6_calculate_county_stats/us_county_stats_puertorico.sav" \
  "${asset_root}/prain/idl/us_county_stats_puertorico.sav"
copy_asset \
  "${source_root}/P-RAIN/2_phram_model/US_topo_10_50N_50_130W.txt" \
  "${asset_root}/prain/topo/US_topo_10_50N_50_130W.txt"
copy_asset \
  "${source_root}/run_stormwindmodel/stormwindmodel_0.1.4.tar.gz" \
  "${asset_root}/stormwindmodel/stormwindmodel_0.1.4.tar.gz"

echo
echo "External assets staged under ${asset_root}"
