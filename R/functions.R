# File input ----

#' Read raw 17Lands event history text
#'
#' @param path Path to the exported event history text file.
#'
#' @return A character vector of lines from the file.
read_record_raw <- function(path) {
  readr::read_lines(path)
}

# Record parsing ----

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

# Manual corrections ----

#' Validate manual corrections data
#'
#' @param manual_corrections Tibble of manual corrections.
#'
#' @return A cleaned corrections tibble.
validate_manual_corrections <- function(manual_corrections) {
  required_cols <- c(
    "datetime",
    "set_code",
    "deck_color",
    "format",
    "wins",
    "losses"
  )
  missing_cols <- setdiff(required_cols, names(manual_corrections))

  if (length(missing_cols) > 0) {
    stop(
      paste0(
        "manual_corrections.csv is missing required columns: ",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invalid_rows <- manual_corrections |>
    dplyr::filter(is.na(datetime) | is.na(wins) | is.na(losses))

  if (nrow(invalid_rows) > 0) {
    invalid_preview <- paste(
      capture.output(print(invalid_rows, n = min(10, nrow(invalid_rows)))),
      collapse = "\n"
    )
    warning(
      paste0(
        "manual_corrections.csv contains invalid values. ",
        "Rows with parse failures will be ignored.\n",
        "Invalid rows preview:\n",
        invalid_preview
      ),
      call. = FALSE
    )
  }

  valid_corrections <- manual_corrections |>
    dplyr::filter(!is.na(datetime), !is.na(wins), !is.na(losses))

  duplicate_keys <- valid_corrections |>
    dplyr::count(datetime, set_code, deck_color, format, name = "n") |>
    dplyr::filter(n > 1)

  if (nrow(duplicate_keys) > 0) {
    duplicate_preview <- paste(
      capture.output(print(duplicate_keys, n = min(10, nrow(duplicate_keys)))),
      collapse = "\n"
    )
    warning(
      paste0(
        "manual_corrections.csv has duplicate correction keys. ",
        "Keeping the first row per key and ignoring the rest.\n",
        "Duplicate key preview:\n",
        duplicate_preview
      ),
      call. = FALSE
    )
  }

  valid_corrections |>
    dplyr::distinct(datetime, set_code, deck_color, format, .keep_all = TRUE)
}

#' Read manual corrections from a CSV file
#'
#' @param path Path to a CSV of manual corrections.
#'
#' @return A tibble of corrected values keyed by identifying fields.
read_manual_corrections <- function(path) {
  manual_corrections <- readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::mutate(
      datetime = as.POSIXct(datetime, tz = "UTC"),
      wins = as.integer(wins),
      losses = as.integer(losses)
    )

  validate_manual_corrections(manual_corrections)

  manual_corrections
}

#' Apply manual corrections to known bad record rows
#'
#' @param record_clean Cleaned record tibble.
#' @param manual_corrections Tibble returned by `read_manual_corrections()`.
#'
#' @return A cleaned tibble with manual corrections applied.
apply_manual_corrections <- function(record_clean, manual_corrections) {
  correction_match_counts <- manual_corrections |>
    dplyr::left_join(
      record_clean |>
        dplyr::select(datetime, set_code, deck_color, format),
      by = c("datetime", "set_code", "deck_color", "format")
    ) |>
    dplyr::count(
      datetime,
      set_code,
      deck_color,
      format,
      wins,
      losses,
      name = "n_matches"
    )

  unmatched_corrections <- correction_match_counts |>
    dplyr::filter(n_matches == 0)

  if (nrow(unmatched_corrections) > 0) {
    unmatched_preview <- paste(
      capture.output(
        print(unmatched_corrections, n = min(10, nrow(unmatched_corrections)))
      ),
      collapse = "\n"
    )
    warning(
      paste0(
        "Some manual corrections matched no rows in record data and were ",
        "ignored.\n",
        "Unmatched corrections preview:\n",
        unmatched_preview
      ),
      call. = FALSE
    )
  }

  ambiguous_corrections <- correction_match_counts |>
    dplyr::filter(n_matches > 1)

  if (nrow(ambiguous_corrections) > 0) {
    ambiguous_preview <- paste(
      capture.output(
        print(ambiguous_corrections, n = min(10, nrow(ambiguous_corrections)))
      ),
      collapse = "\n"
    )
    warning(
      paste0(
        "Some manual corrections matched multiple rows in record data and ",
        "were ignored.\n",
        "Ambiguous corrections preview:\n",
        ambiguous_preview
      ),
      call. = FALSE
    )
  }

  applicable_corrections <- correction_match_counts |>
    dplyr::filter(n_matches == 1) |>
    dplyr::select(-n_matches)

  record_clean |>
    dplyr::left_join(
      applicable_corrections |>
        dplyr::rename(corrected_wins = wins, corrected_losses = losses),
      by = c("datetime", "set_code", "deck_color", "format")
    ) |>
    dplyr::mutate(
      wins = dplyr::coalesce(corrected_wins, wins),
      losses = dplyr::coalesce(corrected_losses, losses)
    ) |>
    dplyr::select(-corrected_wins, -corrected_losses)
}

# Draft summaries ----

#' Summarize wins and losses for draft events by set
#'
#' @param record_clean Cleaned record tibble.
#'
#' @return A tibble with one row per set.
summarize_wins_losses <- function(record_clean) {
  record_clean |>
    dplyr::filter(stringr::str_detect(format, "Draft")) |>
    dplyr::group_by(set_code) |>
    dplyr::summarize(
      last_played = max(datetime, na.rm = TRUE),
      wins = sum(wins, na.rm = TRUE),
      losses = sum(losses, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      win_rate = dplyr::if_else(
        wins + losses > 0,
        100 * wins / (wins + losses),
        NA_real_
      )
    ) |>
    dplyr::arrange(dplyr::desc(last_played)) |>
    dplyr::select(-last_played)
}

#' Count deck color combinations for draft events by set
#'
#' @param record_clean Cleaned record tibble.
#'
#' @return A tibble with one row per set and one column per deck color.
count_deck_colors <- function(record_clean) {
  record_clean |>
    dplyr::filter(stringr::str_detect(format, "Draft")) |>
    dplyr::mutate(
      deck_color = dplyr::if_else(deck_color == "", "unknown", deck_color)
    ) |>
    dplyr::count(set_code, deck_color) |>
    tidyr::pivot_wider(
      names_from = deck_color,
      values_from = n,
      values_fill = 0,
      names_prefix = "deck_color_",
      names_sort = TRUE
    ) |>
    dplyr::arrange(set_code)
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
