# File input

#' Read raw 17Lands event history text
#'
#' @param path Path to the exported event history text file.
#'
#' @return A character vector of lines from the file.
read_record_raw <- function(path) {
  readr::read_lines(path)
}

# Record parsing

#' Keep lines that contain tab-separated event rows
#'
#' @param record_raw Character vector of raw lines.
#'
#' @return Character vector containing only data lines.
extract_data_lines <- function(record_raw) {
  stringr::str_subset(record_raw, "^\\d{4}-\\d{2}-\\d{2}")
}

#' Parse tab-separated event rows into a table
#'
#' @param data_lines Character vector of tab-separated event rows.
#'
#' @return A tibble with raw parsed fields.
parse_record_lines <- function(data_lines) {
  data_lines |>
    stringr::str_split(pattern = "\\t", n = 9, simplify = TRUE) |>
    tibble::as_tibble(.name_repair = "minimal") |>
    rlang::set_names(
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
}

#' Clean parsed record data for analysis
#'
#' @param record_df Tibble returned by `parse_record_lines()`.
#'
#' @return A cleaned tibble with parsed date/time and win/loss columns.
clean_record <- function(record_df) {
  record_df |>
    tidyr::separate(
      datetime,
      into = c("date", "time"),
      sep = " ",
      extra = "merge"
    ) |>
    tidyr::separate(wl, into = c("wins", "losses"), sep = " - ") |>
    dplyr::select(
      date,
      time,
      set_code = set,
      deck_color = colors,
      wins,
      losses,
      format,
      start_rank,
      end_rank
    ) |>
    dplyr::mutate(
      dplyr::across(where(is.character), stringr::str_trim),
      wins = as.integer(wins),
      losses = as.integer(losses),
      date = as.Date(date),
      datetime = as.POSIXct(
        stringr::str_c(date, " ", time),
        tz = "UTC"
      )
    )
}

#' Choose set code for filtering
#'
#' @param record_clean Cleaned record tibble.
#' @param target_set Optional set code to use. If `NULL`, use most recent set.
#'
#' @return A single set code string.
resolve_target_set <- function(record_clean, target_set = NULL) {
  if (!is.null(target_set)) {
    return(target_set)
  }

  record_clean |>
    dplyr::arrange(dplyr::desc(datetime)) |>
    dplyr::slice_head(n = 1) |>
    dplyr::pull(set_code)
}

# Draft summaries

#' Summarize wins and losses for draft events in a set
#'
#' @param record_clean Cleaned record tibble.
#' @param target_set Set code to include.
#'
#' @return A one-row tibble with total wins and losses.
summarize_wins_losses <- function(record_clean, target_set) {
  record_clean |>
    dplyr::filter(
      stringr::str_detect(format, "Draft"),
      set_code == target_set
    ) |>
    dplyr::summarize(
      wins = sum(wins, na.rm = TRUE),
      losses = sum(losses, na.rm = TRUE)
    )
}

#' Count deck color combinations for draft events in a set
#'
#' @param record_clean Cleaned record tibble.
#' @param target_set Set code to include.
#'
#' @return A tibble of deck color counts.
count_deck_colors <- function(record_clean, target_set) {
  record_clean |>
    dplyr::filter(
      stringr::str_detect(format, "Draft"),
      set_code == target_set
    ) |>
    dplyr::count(deck_color)
}

#' Summarize black main-color and splash-color usage
#'
#' @param record_clean Cleaned record tibble.
#' @param target_set Set code to include.
#'
#' @return A one-row tibble with `black_main` and `black_splash` counts.
summarize_black_usage <- function(record_clean, target_set) {
  record_clean |>
    dplyr::filter(
      stringr::str_detect(format, "Draft"),
      set_code == target_set
    ) |>
    dplyr::filter(stringr::str_detect(deck_color, "[Bb]")) |>
    dplyr::summarize(
      black_main = sum(stringr::str_detect(deck_color, "B"), na.rm = TRUE),
      black_splash = sum(stringr::str_detect(deck_color, "b"), na.rm = TRUE)
    )
}
