source("R/packages.R")
source("R/functions.R")

list(
  # File input ----
  # - Set SEVENTEENLANDS_USERNAME (or SEVENTEENLANDS_EMAIL) and SEVENTEENLANDS_PASSWORD in
  #   your environment before running tar_make().
  tar_target(record_raw, fetch_17lands_record_raw()),

  # Record parsing ----
  tar_target(data_lines, extract_data_lines(record_raw)),
  tar_target(record_df, parse_record_lines(data_lines)),
  tar_target(record_clean, clean_record(record_df)),

  # Manual corrections ----
  # - Make any manual fixes to your record in this CSV file. If the time, set
  #   code etc. don't match the data from 17lands, the correction will be
  #   ignored.
  tarchetypes::tar_file_read(
    manual_corrections,
    "manual_corrections.csv",
    read_manual_corrections(!!.x)
  ),
  tar_target(
    record_corrected,
    apply_manual_corrections(record_clean, manual_corrections)
  ),

  # Draft summaries ----
  tar_target(wins_losses, summarize_wins_losses(record_corrected)),
  tar_target(deck_color_counts, count_deck_colors(record_corrected)),
  tar_target(
    lands_set_averages,
    fetch_17lands_set_averages(wins_losses$set_code)
  ),

  # Dashboard inputs ----
  tar_target(
    wins_losses_csv,
    {
      readr::write_csv(wins_losses, "working/wins_losses.csv", na = "")
      "working/wins_losses.csv"
    },
    format = "file"
  ),
  tar_target(
    deck_color_counts_csv,
    {
      readr::write_csv(
        deck_color_counts,
        "working/deck_color_counts.csv",
        na = ""
      )
      "working/deck_color_counts.csv"
    },
    format = "file"
  ),
  tar_target(
    lands_set_averages_csv,
    {
      readr::write_csv(
        lands_set_averages,
        "working/lands_set_averages.csv",
        na = ""
      )
      "working/lands_set_averages.csv"
    },
    format = "file"
  ),

  # Dashboard ----
  tarchetypes::tar_quarto(
    dashboard,
    "dashboard.qmd",
    output_file = "dashboard.html",
    quiet = FALSE
  )
)
