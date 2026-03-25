# Load required libraries
library(tidyverse)

# Read the file
record_raw <- read_lines("record.txt")

# Find lines that start with a date pattern (yyyy-mm-dd)
# These are the main data rows
data_lines <- record_raw[str_detect(record_raw, "^\\d{4}-\\d{2}-\\d{2}")]

# Parse the tab-separated data
record_df <- read_tsv(
  I(paste(data_lines, collapse = "\n")),
  col_names = c(
    "datetime",
    "set",
    "trophy",
    "colors",
    "wl",
    "format",
    "start_rank",
    "end_rank",
    "shareable_links"
  ),
  col_types = cols(.default = "c")
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
  # Convert date to Date type
  mutate(date = as.Date(date))

record_clean |>
  filter(
    str_detect(format, "Draft"),
    set_code == "TLA"
  ) |>
  summarize(
    wins = sum(wins),
    losses = sum(losses)
  )

record_clean |>
  filter(
    str_detect(format, "Draft"),
    set_code == "TLA"
  ) |>
  count(deck_color)

# Count decks with black as main color (B) or splash (b)
record_clean |>
  filter(
    str_detect(format, "Draft"),
    set_code == "TLA"
  ) |>
  filter(str_detect(deck_color, "[Bb]")) |>
  summarize(
    black_main = sum(str_detect(deck_color, "B")),
    black_splash = sum(str_detect(deck_color, "b"))
  )
