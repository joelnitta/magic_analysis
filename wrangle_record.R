# Load required libraries
library(tidyverse)

# Read the file
record_raw <- read_lines("data/record.txt")

# Find lines that start with a date pattern (yyyy-mm-dd)
# These are the main data rows
data_lines <- record_raw[str_detect(record_raw, "^\\d{4}-\\d{2}-\\d{2}")]

# Parse each data row directly by tabs.
# This is more robust to missing trailing fields in the copied export.
record_df <- data_lines |>
  str_split(pattern = "\\t", n = 9, simplify = TRUE) |>
  as_tibble(.name_repair = "minimal") |>
  set_names(
    c(
      "datetime",
      "set",
      "trophy",
      "colors",
      "wl",
      "format",
      "start_rank",
      "end_rank",
      "shareable_links"
    )
  )

# Wrangle the data
record_clean <- record_df %>%
  # Separate datetime into date and time
  separate(datetime, into = c("date", "time"), sep = " ", extra = "merge") %>%
  # Separate W/L into wins and losses
  separate(wl, into = c("wins", "losses"), sep = " - ", convert = TRUE) %>%
  # Select and rename columns as needed
  select(
    date,
    time,
    set_code = set,
    deck_color = colors,
    wins,
    losses,
    format,
    start_rank,
    end_rank
  ) %>%
  # Convert date/time to proper types and trim copied whitespace
  mutate(
    across(where(is.character), str_trim),
    wins = as.integer(wins),
    losses = as.integer(losses),
    date = as.Date(date),
    datetime = as.POSIXct(str_c(date, " ", time), tz = "UTC")
  )

# Set this manually (e.g., "TLA") or leave NULL to use most recent set.
target_set <- NULL

if (is.null(target_set)) {
  target_set <- record_clean |>
    arrange(desc(datetime)) |>
    slice_head(n = 1) |>
    pull(set_code)
}

record_clean |>
  filter(
    str_detect(format, "Draft"),
    set_code == target_set
  ) |>
  summarize(
    wins = sum(wins),
    losses = sum(losses)
  )

record_clean |>
  filter(
    str_detect(format, "Draft"),
    set_code == target_set
  ) |>
  count(deck_color)

# Count decks with black as main color (B) or splash (b)
record_clean |>
  filter(
    str_detect(format, "Draft"),
    set_code == target_set
  ) |>
  filter(str_detect(deck_color, "[Bb]")) |>
  summarize(
    black_main = sum(str_detect(deck_color, "B")),
    black_splash = sum(str_detect(deck_color, "b"))
  )
