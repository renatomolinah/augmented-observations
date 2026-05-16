################################################################################
# ERA5 hourly precipitation downloader
# ------------------------------------------------------------------------------
# Download hourly ERA5 precipitation fields for one storm over a predefined
# date range.
################################################################################

import cdsapi
import csv
import os
from pathlib import Path
from datetime import datetime,timedelta

#=============================
# 1. Configure the run
#=============================


storm = os.getenv("AO_STORMNAME", "milton").strip() or "milton"
script_path = Path(__file__).resolve()
external_models_dir = script_path.parents[2]
repo_root = script_path.parents[4]
data_dir = Path(os.getenv("AO_DATA_DIR", str(repo_root / "data")))
era5_dir = Path(os.getenv("AO_PRAIN_ERA5_DIR", str(data_dir / "raw" / "era5")))
manifest_path = external_models_dir / "config" / "storm_manifest.csv"

with manifest_path.open(newline="") as handle:
    manifest_rows = {row["stormname"]: row for row in csv.DictReader(handle)}

if storm not in manifest_rows:
    raise ValueError(f"Storm '{storm}' was not found in {manifest_path}.")

storm_row = manifest_rows[storm]
atcf_id = storm_row["stormid"]
year = storm_row["year"]
date_range = [storm_row["era5_start"], storm_row["era5_end"]]

if not all(date_range):
    raise ValueError(f"Storm '{storm}' is missing ERA5 date bounds in {manifest_path}.")




# --- Helper to generate date list ---
def make_date_list(start_date, end_date):
    start = datetime.strptime(start_date, "%Y-%m-%d")
    end   = datetime.strptime(end_date, "%Y-%m-%d")

    if end < start:
        raise ValueError("end_date must be >= start_date")

    return [
        (start + timedelta(days=i)).strftime("%Y-%m-%d")
        for i in range((end - start).days + 1)
    ]

# --- Create date list ---
dates = make_date_list(*date_range)

c = cdsapi.Client()
era5_dir.mkdir(parents=True, exist_ok=True)

dataset = 'reanalysis-era5-land'
request = {
  'variable': 'total_precipitation',
  'date': dates,
  'time': [
      '00:00', '01:00', '02:00',
      '03:00', '04:00', '05:00',
      '06:00', '07:00', '08:00',
      '09:00', '10:00', '11:00',
      '12:00', '13:00', '14:00',
      '15:00', '16:00', '17:00',
      '18:00', '19:00', '20:00',
      '21:00', '22:00', '23:00',
  ],
  'data_format': 'grib',
  'download_format': 'unarchived',
  'area': [50, -105, 20, -65]
  #'area': [30, -75, 10, -55],  #for Maria (Puerto Rico)
}
target = str(era5_dir / f"{atcf_id}{year}.grib")
c.retrieve(dataset,request,target)
