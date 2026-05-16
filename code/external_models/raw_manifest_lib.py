#!/usr/bin/env python3
"""Helpers for byte-for-byte raw output manifests."""

from __future__ import annotations

import hashlib
import csv
from pathlib import Path

PROJECT_ROOT = Path(__file__).resolve().parents[2]
DEFAULT_MANIFEST_PATH = PROJECT_ROOT / "code" / "external_models" / "config" / "raw_output_manifest.tsv"

INCLUDE_ROOTS = [
    PROJECT_ROOT / "data" / "raw" / "wind_error",
    PROJECT_ROOT / "data" / "raw" / "precip_error",
    PROJECT_ROOT / "data" / "raw" / "track_raw",
    PROJECT_ROOT / "data" / "raw_denial",
]

EXCLUDED_FILE_NAMES = {".DS_Store"}
EXCLUDED_DIR_NAMES = {"__MACOSX"}


def should_include(path: Path) -> bool:
    if not path.is_file():
        return False
    if path.name in EXCLUDED_FILE_NAMES:
        return False
    if any(part in EXCLUDED_DIR_NAMES for part in path.parts):
        return False
    return True


def sha256_for_file(path: Path, chunk_size: int = 1024 * 1024) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        while True:
            chunk = handle.read(chunk_size)
            if not chunk:
                break
            digest.update(chunk)
    return digest.hexdigest()


def load_storm_manifest() -> dict[str, dict[str, str]]:
    manifest_path = PROJECT_ROOT / "code" / "external_models" / "config" / "storm_manifest.csv"
    with manifest_path.open(newline="") as handle:
        return {row["stormname"]: row for row in csv.DictReader(handle)}


def storm_filter(storm: str):
    manifest_rows = load_storm_manifest()
    if storm not in manifest_rows:
        raise KeyError(f"Storm '{storm}' was not found in the shared storm manifest.")

    storm_row = manifest_rows[storm]
    obs_prefix = f'{storm_row["stormid"]}{storm_row["year"]}_'

    def include(path: Path) -> bool:
        relative_path = path.relative_to(PROJECT_ROOT).as_posix()
        file_name = path.name
        return (
            f"/{storm}/" in relative_path
            or file_name.startswith(f"{storm}_")
            or file_name.startswith(obs_prefix)
        )

    return include


def iter_inventory(path_filter=None) -> list[dict[str, str]]:
    records: list[dict[str, str]] = []
    for root in INCLUDE_ROOTS:
        if not root.exists():
            continue
        for path in sorted(root.rglob("*")):
            if not should_include(path):
                continue
            if path_filter is not None and not path_filter(path):
                continue
            records.append(
                {
                    "relative_path": path.relative_to(PROJECT_ROOT).as_posix(),
                    "size_bytes": str(path.stat().st_size),
                    "sha256": sha256_for_file(path),
                }
            )
    return records
