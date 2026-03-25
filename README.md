# magic_analysis

Analyze Magic: The Gathering Arena draft history (from 17Lands) with R and
`{targets}`.

This repo is for people who:

- play MTG on Arena,
- use 17Lands,
- and are comfortable working in R.

For now, it is mostly for analyzing win/loss records.

## What this project does

The pipeline reads your 17Lands event history, cleans it, applies optional
manual corrections, and produces summary tables such as:

- `wins_losses`: wins, losses, and win rate by set (most recent sets first)
- `deck_color_counts`: deck-color counts by set (one row per set)

## Requirements

- R (recent version)
- R packages used by this project:
  - `targets`
  - `tarchetypes`
  - `dplyr`
  - `readr`
  - `rlang`
  - `stringr`
  - `tibble`
  - `tidyr`

Install packages if needed:

```r
install.packages(c(
  "targets", "tarchetypes", "dplyr", "readr", "rlang",
  "stringr", "tibble", "tidyr"
))
```

## Data input

### 1) Export event history from 17Lands

From the 17Lands **Event History** page:

1. Click **Copyable**.
2. Select all text (`Cmd+A` on macOS).
3. Copy and paste into `data/record.txt`.

### 2) Optional manual corrections

Add row-level fixes to `manual_corrections.csv`. This is to fix games where the win/loss record from 17lands was not correct.

Columns:

- `datetime` (UTC timestamp matching the record row)
- `set_code`
- `deck_color`
- `format`
- `wins`
- `losses`

If a correction does not match exactly one row in the record data, the pipeline
will warn and ignore that correction.

## Run the pipeline

From the project root:

```r
targets::tar_make()
```

Read outputs:

```r
targets::tar_read(wins_losses)
targets::tar_read(deck_color_counts)
```
