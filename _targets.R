source("R/packages.R")
source("R/functions.R")

list(
  # File input
  # - Click on "Copyable" in the 17lands "Event History" page, then select ALL
  #   of the text on webpage with ctrl (or command) + a, and copy it to
  #   data/record.txt
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

  # Draft summaries
  tar_target(wins_losses, summarize_wins_losses(record_corrected)),
  tar_target(deck_color_counts, count_deck_colors(record_corrected))
)
