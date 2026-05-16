#!/usr/bin/env python3
"""Verify current raw products against the exact-match manifest."""

from __future__ import annotations

import argparse
import csv
from pathlib import Path

from raw_manifest_lib import DEFAULT_MANIFEST_PATH, PROJECT_ROOT, iter_inventory, storm_filter


def load_manifest(path: Path) -> dict[str, dict[str, str]]:
    with path.open(newline="") as handle:
        reader = csv.DictReader(handle, delimiter="\t")
        return {row["relative_path"]: row for row in reader}


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--manifest",
        default=str(DEFAULT_MANIFEST_PATH),
        help="Path to the manifest TSV to verify against.",
    )
    parser.add_argument(
        "--storm",
        help="Restrict the verification to files for one storm.",
    )
    args = parser.parse_args()

    manifest_path = Path(args.manifest).resolve()
    if not manifest_path.exists():
        raise SystemExit(f"Manifest not found: {manifest_path}")

    expected = load_manifest(manifest_path)
    path_filter = storm_filter(args.storm) if args.storm else None
    if path_filter is not None:
        expected = {
            path: row
            for path, row in expected.items()
            if path_filter(PROJECT_ROOT / path)
        }
    actual = {row["relative_path"]: row for row in iter_inventory(path_filter=path_filter)}

    missing = sorted(set(expected) - set(actual))
    extra = sorted(set(actual) - set(expected))
    changed = sorted(
        path
        for path in set(expected) & set(actual)
        if expected[path]["size_bytes"] != actual[path]["size_bytes"]
        or expected[path]["sha256"] != actual[path]["sha256"]
    )

    if missing:
        print("Missing files:")
        for path in missing[:50]:
            print(f"  {path}")
        if len(missing) > 50:
            print(f"  ... and {len(missing) - 50} more")

    if extra:
        print("Unexpected files:")
        for path in extra[:50]:
            print(f"  {path}")
        if len(extra) > 50:
            print(f"  ... and {len(extra) - 50} more")

    if changed:
        print("Changed files:")
        for path in changed[:50]:
            print(f"  {path}")
        if len(changed) > 50:
            print(f"  ... and {len(changed) - 50} more")

    if missing or extra or changed:
        print(
            f"\nRaw manifest mismatch: {len(missing)} missing, {len(extra)} extra, {len(changed)} changed."
        )
        return 1

    if args.storm:
        print(f"Raw manifest matches exactly for storm '{args.storm}': {len(expected)} files verified.")
    else:
        print(f"Raw manifest matches exactly: {len(expected)} files verified.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
