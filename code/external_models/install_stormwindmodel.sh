#!/bin/bash
set -euo pipefail

################################################################################
# Stormwindmodel installer
# ------------------------------------------------------------------------------
# Install the staged stormwindmodel source tarball into a writable R library.
################################################################################

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
asset_root="${AO_EXTERNAL_ASSET_ROOT:-${script_dir}/assets}"
package_tarball="${AO_STORMWINDMODEL_TARBALL:-${asset_root}/stormwindmodel/stormwindmodel_0.1.4.tar.gz}"

if [ ! -f "$package_tarball" ]; then
  echo "Missing stormwindmodel tarball: $package_tarball" >&2
  exit 1
fi

PACKAGE_TARBALL="$package_tarball" Rscript --vanilla - <<'EOF'
package_tarball <- Sys.getenv("PACKAGE_TARBALL")
target_lib <- .libPaths()[1]

if (!dir.exists(target_lib)) {
  dir.create(target_lib, recursive = TRUE, showWarnings = FALSE)
}

if (file.access(target_lib, 2) != 0) {
  user_lib <- Sys.getenv("R_LIBS_USER", unset = "")
  if (user_lib == "") {
    version_short <- paste(R.version$major, strsplit(R.version$minor, "\\.")[[1]][1], sep = ".")
    user_lib <- file.path(path.expand("~"), "Library", "R", version_short, "library")
  }
  dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
  if (file.access(user_lib, 2) != 0) {
    stop(sprintf("No writable R library path is available. Tried %s and %s.", target_lib, user_lib))
  }
  target_lib <- user_lib
}

install.packages(package_tarball, repos = NULL, type = "source", lib = target_lib)
message(sprintf("Installed stormwindmodel into %s", target_lib))
EOF
