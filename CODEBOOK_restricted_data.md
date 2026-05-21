# Restricted/Private Data Codebook

This codebook documents restricted, private, or otherwise non-self-documenting
inputs used by the `augmented-observations` replication pipeline.

## 1) SHELDUS hurricane damage inputs

- Files: `data/raw/sheldus/damages_*.csv`
- Access modality: subscription/restricted source extracts, not downloaded by
  the pipeline itself
- Used in: `code/cleaning_voi/1.05.clean_damage.r`, then merged into the main
  panel in `code/cleaning_voi/1.07.merge_data.r`

The pipeline expects one hurricane-specific CSV per file, matched by the glob
pattern `damages_*.csv`. The hurricane name is inferred from the filename
itself using the pattern `damages_YEAR_NAME.csv`.

Source variables read by the cleaning script:

- `StateName`
- `CountyName`
- `County_FIPS`
- `Year`
- `Month`
- `Fatalities`
- `FatalitiesPerCapita`
- `CropDmg`
- `CropDmg(ADJ 2019)`
- `CropDmgPerCapita`
- `PropertyDmg`
- `PropertyDmg(ADJ 2019)`
- `PropertyDmgPerCapita`

Key transformations:

- The script renames the source fields to snake-case analysis names such as
  `state`, `county`, `county_fips`, `crop_damage_2019`, and
  `property_damage_2019`.
- `county_fips` is zero-padded to a 5-character county FIPS code.
- State and county names are standardized to lowercase.
- Duration-style columns and record counters are dropped when present.
- `injuries` and `injuries_pc` are added as zeros so downstream code can rely
  on a stable column set.
- Puerto Rico observations (`county_fips` beginning with `72`) are excluded.
- The cleaned output is written to `data/clean/data_sheldus.csv`.

## 2) Workshop Qualtrics exports

- Files:
  - `data/workshop/Part 1.csv`
  - `data/workshop/Part 2.csv`
- Access modality: private survey exports, not publicly fetched by the pipeline
- Used in: `code/workshop_responses.R`

The script expects raw Qualtrics CSV exports with:

- row 1 = machine-readable column names
- row 2 = question text
- row 3 = ImportId metadata
- response rows beginning on row 4

Columns read directly by the script include:

- Sample composition:
  - Part 1: `Q35`, `Q34`
  - Part 2: `Q23`, `Q24`
- Likert items:
  - Part 1: `S1-1` through `S2-4`
  - Part 2: `S3-1` through `CB3`
- Open-ended responses:
  - Part 1: `S1-4`, `S1-5`, `S1-6`, `S2-5`, `S2-6`
  - Part 2: `S3-4`, `S3-5`, `S3-6`, `CB4`, `CB5`, `CB6`, `CS1`, `CS2`

Key transformations:

- Value labels for respondent role and years of experience are decoded inside
  the script.
- Blank, missing, and placeholder responses are normalized before tabulation.
- The script writes Appendix D tables to `output/appD_*.tex`.

## 3) Replication support

- Restricted/private inputs should be preserved with the project data folder
  referenced by `data/`.
- The main workflow README is `README.md`.
- Primitive upstream wind/track/rain generation inputs are documented in the
  README and are distinct from the restricted/private inputs described here.
