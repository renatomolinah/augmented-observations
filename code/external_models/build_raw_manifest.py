#!/usr/bin/env python3
"""Build the exact-match manifest for raw products."""

from __future__ import annotations

import argparse
import csv

from raw_manifest_lib import DEFAULT_MANIFEST_PATH, iter_inventory, storm_filter


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--output",
        default=str(DEFAULT_MANIFEST_PATH),
        help="Path to the manifest TSV to write.",
    )
    parser.add_argument(
        "--storm",
        help="Restrict the manifest to files for one storm.",
    )
    args = parser.parse_args()

    path_filter = storm_filter(args.storm) if args.storm else None
    records = iter_inventory(path_filter=path_filter)
    output_path = DEFAULT_MANIFEST_PATH.__class__(args.output).resolve()
    output_path.parent.mkdir(parents=True, exist_ok=True)

    with output_path.open("w", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=["relative_path", "size_bytes", "sha256"], delimiter="\t")
        writer.writeheader()
        writer.writerows(records)

    print(f"Wrote {len(records)} records to {output_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
