library(targets)

source("R/functions.R")

tar_option_set(
  packages = c("dplyr", "readr", "rlang", "stringr", "tibble", "tidyr")
)

list(
  # File input
  tarchetypes::tar_file_read(
    record_raw,
    "data/record.txt",
    read_record_raw(!!.x)
  ),

  # Record parsing
  tar_target(data_lines, extract_data_lines(record_raw)),
  tar_target(record_df, parse_record_lines(data_lines)),
  tar_target(record_clean, clean_record(record_df)),

  # Manual corrections
  tarchetypes::tar_file_read(
    manual_corrections,
    "manual_corrections.csv",
    read_manual_corrections(!!.x)
  ),
  tar_target(
    record_corrected,
    apply_manual_corrections(record_clean, manual_corrections)
  ),

  # Draft summaries
  tar_target(wins_losses, summarize_wins_losses(record_corrected)),
  tar_target(deck_color_counts, count_deck_colors(record_corrected))
)
