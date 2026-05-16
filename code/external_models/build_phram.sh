#!/bin/bash
set -euo pipefail

################################################################################
# PHRAM builder
# ------------------------------------------------------------------------------
# Compile the upstream P-RAIN Fortran model into a canonical repo-local binary.
################################################################################

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
source_dir="${script_dir}/prain/2_phram_model"
asset_root="${AO_EXTERNAL_ASSET_ROOT:-${script_dir}/assets}"
output_dir="${asset_root}/prain/bin"

mkdir -p "$output_dir"

gfortran -O2 -o "${output_dir}/phram.exe" "${source_dir}/rainfall_topo.f"

echo "Built ${output_dir}/phram.exe"
