#!/bin/bash
set -euo pipefail

################################################################################
# Free-toolchain bootstrap helper
# ------------------------------------------------------------------------------
# Set up the repo-owned pieces of the external-model toolchain and print the
# remaining free-software install steps for the current platform.
################################################################################

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo_root=$(CDPATH= cd -- "${script_dir}/../.." && pwd)
install_python=0
build_phram=0

usage() {
  cat <<'EOF'
Usage: setup_free_external_toolchain.sh [--install-python] [--build-phram]

This helper stages repo assets, installs the packaged stormwindmodel R tarball,
and prints platform-specific next steps for the remaining free dependencies.
EOF
}

while [ "$#" -gt 0 ]; do
  case "$1" in
    --install-python)
      install_python=1
      shift
      ;;
    --build-phram)
      build_phram=1
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

echo "Staging required non-source assets..."
bash "${script_dir}/stage_external_assets.sh"

echo
if Rscript --vanilla -e "library(stormwindmodel)" >/dev/null 2>&1; then
  echo "stormwindmodel R package already installed; skipping reinstall."
else
  echo "Installing staged stormwindmodel R package..."
  if ! bash "${script_dir}/install_stormwindmodel.sh"; then
    echo "Could not install stormwindmodel automatically. Re-run later with a writable R library path." >&2
  fi
fi

echo
if [ "$install_python" -eq 1 ]; then
  echo "Installing Python package cdsapi..."
  python3 -m pip install cdsapi
else
  echo "Python package still needed:"
  echo "  python3 -m pip install cdsapi"
fi

echo
if [ "$build_phram" -eq 1 ]; then
  echo "Attempting to build phram.exe..."
  if bash "${script_dir}/build_phram.sh"; then
    echo "Built phram.exe successfully."
  else
    echo "Could not build phram.exe yet; install gfortran first." >&2
  fi
else
  echo "Optional binary build after gfortran is installed:"
  echo "  make external_build_phram"
fi

echo
echo "Remaining free-software installs depend on the machine:"
case "$(uname -s)" in
  Darwin)
    cat <<'EOF'
macOS:
  1. GDL:
     - Preferred free route from the GDL project: install the official DMG, or
       use MacPorts:
         sudo port install gnudatalanguage
  2. ecCodes / grib_filter:
     - Homebrew route:
         brew install eccodes
     - or Conda route:
         conda install -c conda-forge eccodes
  3. wgrib:
     - Install from NOAA CPC source/build instructions.
  4. gfortran:
     - Install the GNU compiler toolchain, for example via Homebrew:
         brew install gcc
EOF
    ;;
  Linux)
    cat <<'EOF'
Linux:
  1. GDL:
     - Ubuntu/Debian: sudo apt install gnudatalanguage
     - Fedora:        sudo yum install gdl
     - Arch:          sudo pacman -S gnudatalanguage
  2. ecCodes / grib_filter:
     - Conda route:
         conda install -c conda-forge eccodes
     - or install the distro's ecCodes tools package.
  3. wgrib:
     - Install from NOAA CPC source/build instructions.
  4. gfortran:
     - Install the distro's GNU Fortran compiler package.
EOF
    ;;
  *)
    cat <<'EOF'
Unknown platform:
  Install these free dependencies manually:
    - GDL (preferred) or IDL
    - ecCodes tools providing grib_filter
    - wgrib
    - gfortran
    - Python package cdsapi
EOF
    ;;
esac

echo
echo "When those are installed, run:"
echo "  make external_toolchain_check"
echo
echo "Then test one storm:"
echo "  make external_run_case STORM=irma"
echo "  make raw_exact_check"
