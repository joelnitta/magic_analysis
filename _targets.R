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
  tar_target(
    target_set,
    resolve_target_set(record_clean, target_set = NULL)
  ),

  # Draft summaries
  tar_target(wins_losses, summarize_wins_losses(record_clean, target_set)),
  tar_target(deck_color_counts, count_deck_colors(record_clean, target_set)),
  tar_target(black_usage, summarize_black_usage(record_clean, target_set))
)
