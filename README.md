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
  - `httr2`
  - `jsonlite`
  - `shiny`
  - `rlang`
  - `stringr`
  - `tibble`
  - `tidyr`

Install packages if needed:

```r
install.packages(c(
  "targets", "tarchetypes", "dplyr", "readr", "httr2", "jsonlite", "shiny", "rlang",
  "stringr", "tibble", "tidyr"
))
```

## Data input

### 1) Configure 17Lands credentials

Set credentials in your `.Renviron` file:

```r
17LANDS_USERNAME="your-email@example.com"
17LANDS_PASSWORD="your-password"
```

You can use `17LANDS_EMAIL` instead of `17LANDS_USERNAME`.

The pipeline logs in to 17Lands and downloads event history directly from
the website.

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

## Dashboard

Running `targets::tar_make()` now also renders a simple HTML dashboard with
charts for:

- set-level win rate (`wins_losses`)
- set-level deck color composition (`deck_color_counts`)

Open the generated page at `dashboard.html`.

## Shiny dashboard

You can run an interactive dashboard with a minimum-games filter for the
set-level win-rate chart.

1. Build data with `targets::tar_make()`.
2. Launch the app:

```r
shiny::runApp("app.R")
```

The app reads `working/wins_losses.csv` and
`working/deck_color_counts.csv`, so run `tar_make()` first.
