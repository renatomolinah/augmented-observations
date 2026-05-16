#!/bin/bash
set -euo pipefail

################################################################################
# GDL / IDL procedure runner
# ------------------------------------------------------------------------------
# Compile one .pro file in batch mode and execute the requested top-level
# procedure from the repo root or another caller-selected working directory.
# Prefer the free `gdl` runtime when available, and fall back to `idl`.
################################################################################

repo_root=$(CDPATH= cd -- "$(dirname -- "$0")"/../.. && pwd)
workdir="$repo_root"
pro_file=""
procedure=""
idl_cmd=""

usage() {
  cat <<'EOF'
Usage: run_idl_procedure.sh --pro-file PATH --procedure NAME [--workdir PATH]
EOF
}

resolve_idl_cmd() {
  local requested_cmd="${AO_IDL_CMD:-}"

  if [ -n "$requested_cmd" ]; then
    if command -v "$requested_cmd" >/dev/null 2>&1; then
      printf '%s\n' "$requested_cmd"
      return 0
    fi

    echo "Configured AO_IDL_CMD was not found on PATH: ${requested_cmd}" >&2
    return 1
  fi

  if command -v gdl >/dev/null 2>&1; then
    printf 'gdl\n'
    return 0
  fi

  if command -v idl >/dev/null 2>&1; then
    printf 'idl\n'
    return 0
  fi

  echo "Neither GDL ('gdl') nor IDL ('idl') was found on PATH." >&2
  return 1
}

while [ "$#" -gt 0 ]; do
  case "$1" in
    --pro-file)
      pro_file="$2"
      shift 2
      ;;
    --procedure)
      procedure="$2"
      shift 2
      ;;
    --workdir)
      workdir="$2"
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

if [ -z "$pro_file" ] || [ -z "$procedure" ]; then
  usage >&2
  exit 1
fi

idl_cmd="$(resolve_idl_cmd)"

if [ ! -f "$pro_file" ]; then
  echo "IDL source file not found: $pro_file" >&2
  exit 1
fi

pro_file=$(cd "$(dirname "$pro_file")" && pwd)/$(basename "$pro_file")
pro_dir=$(dirname "$pro_file")
pro_name=$(basename "$pro_file")

batch_file=$(mktemp /private/tmp/ao_idl_XXXXXX.batch)
trap 'rm -f "$batch_file"' EXIT

cat >"$batch_file" <<EOF
!path = expand_path('+${pro_dir}:' + !path)
.cd, '${workdir}'
.compile ${pro_name}
$procedure
exit
EOF

(
  cd "$workdir"
  "$idl_cmd" < "$batch_file"
)
