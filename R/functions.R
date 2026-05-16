# File input ----

#' Read raw 17Lands event history text
#'
#' @param path Path to the exported event history text file.
#'
#' @return A character vector of lines from the file.
read_record_raw <- function(path) {
  readr::read_lines(path)
}

#' Read 17Lands login credentials from environment variables
#'
#' Required variables are `SEVENTEENLANDS_USERNAME` and `SEVENTEENLANDS_PASSWORD`.
#' `SEVENTEENLANDS_EMAIL` is also accepted as an alias for the username.
#'
#' @return A named list with `email` and `password`.
read_17lands_credentials <- function() {
  username <- Sys.getenv("SEVENTEENLANDS_USERNAME", unset = "")
  password <- Sys.getenv("SEVENTEENLANDS_PASSWORD", unset = "")
  email_alias <- Sys.getenv("SEVENTEENLANDS_EMAIL", unset = "")

  email <- if (username != "") username else email_alias

  if (email == "" || password == "") {
    stop(
      paste0(
        "Missing 17Lands credentials. Set SEVENTEENLANDS_USERNAME (or ",
        "SEVENTEENLANDS_EMAIL) and SEVENTEENLANDS_PASSWORD in your environment."
      ),
      call. = FALSE
    )
  }

  list(email = email, password = password)
}

#' Create an authenticated 17Lands request object
#'
#' This performs login and returns a request object with persisted cookies.
#'
#' @param credentials A named list from `read_17lands_credentials()`.
#'
#' @return An httr2 request object with an authenticated cookie jar.
login_17lands <- function(credentials) {
  cookie_path <- tempfile("17lands-cookies-")

  login_req <- httr2::request("https://www.17lands.com/login") |>
    httr2::req_cookie_preserve(path = cookie_path) |>
    httr2::req_method("POST") |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_headers(
      `content-type` = "application/json",
      accept = "application/json, text/plain, */*"
    ) |>
    httr2::req_body_json(
      list(
        email = credentials$email,
        password = credentials$password,
        remember_me = FALSE
      ),
      auto_unbox = TRUE
    )

  login_resp <- httr2::req_perform(login_req)
  login_text <- httr2::resp_body_string(login_resp)
  login_status <- httr2::resp_status(login_resp)

  if (login_status >= 400 || grepl("Invalid login", login_text, fixed = TRUE)) {
    stop(
      "17Lands login failed. Check SEVENTEENLANDS_USERNAME and SEVENTEENLANDS_PASSWORD.",
      call. = FALSE
    )
  }

  httr2::request("https://www.17lands.com") |>
    httr2::req_cookie_preserve(path = cookie_path) |>
    httr2::req_headers(accept = "application/json, text/plain, */*")
}

#' Return a scalar character value from a list-like draft record
#'
#' @param x A value from a parsed JSON object.
#' @param default Default value when `x` is empty or missing.
#'
#' @return A scalar character value.
scalar_value <- function(x, default = "") {
  if (is.null(x) || length(x) == 0) {
    return(default)
  }

  value <- x[[1]]
  if (is.na(value)) {
    return(default)
  }

  as.character(value)
}

#' Convert one draft record from API JSON into a copyable history line
#'
#' @param draft One draft entry from the 17Lands events payload.
#'
#' @return One tab-delimited line matching the copyable event-history format.
draft_to_record_line <- function(draft) {
  datetime <- scalar_value(draft$datetime)
  if (datetime == "") {
    datetime <- scalar_value(draft$last_event_server_time)
  }
  if (datetime == "") {
    datetime <- scalar_value(draft$first_event_server_time)
  }
  if (datetime == "") {
    datetime <- scalar_value(draft$first_pick_time)
  }

  set_code <- scalar_value(draft$set)
  if (set_code == "") {
    set_code <- scalar_value(draft$expansion)
  }

  colors <- scalar_value(draft$colors)
  if (colors == "") {
    colors <- scalar_value(draft$deck_color)
  }

  wins <- suppressWarnings(as.integer(scalar_value(draft$wins, default = "0")))
  losses <- suppressWarnings(
    as.integer(scalar_value(draft$losses, default = "0"))
  )
  if (is.na(wins)) {
    wins <- 0L
  }
  if (is.na(losses)) {
    losses <- 0L
  }

  event_wins <- suppressWarnings(
    as.integer(scalar_value(draft$event_wins, default = "0"))
  )
  if (is.na(event_wins)) {
    event_wins <- 0L
  }

  trophy_flag <- scalar_value(draft$trophy)
  trophy <- if (trophy_flag == "TRUE" || event_wins >= 1L) "x" else ""

  event_type <- scalar_value(draft$format)
  if (event_type == "") {
    event_type <- scalar_value(draft$event_type)
  }

  start_rank <- scalar_value(draft$start_rank)
  end_rank <- scalar_value(draft$end_rank)
  shareable_links <- scalar_value(draft$shareable_links)

  stringr::str_c(
    c(
      datetime,
      set_code,
      trophy,
      colors,
      stringr::str_c(wins, " - ", losses),
      event_type,
      start_rank,
      end_rank,
      shareable_links
    ),
    collapse = "\t"
  )
}

#' Extract event-history lines from a parsed 17Lands payload
#'
#' @param payload Parsed JSON payload from a 17Lands endpoint.
#'
#' @return Character vector of copyable history lines.
extract_record_lines_from_payload <- function(payload) {
  copyable_text <- scalar_value(payload$copyable)
  if (copyable_text == "") {
    copyable_text <- scalar_value(payload$copyable_text)
  }
  if (copyable_text != "") {
    return(stringr::str_split(copyable_text, "\n", simplify = FALSE)[[1]])
  }

  drafts <- payload$drafts
  if (is.null(drafts) && !is.null(payload$data)) {
    drafts <- payload$data$drafts
  }

  if (is.null(drafts) || length(drafts) == 0) {
    return(character())
  }

  vapply(drafts, draft_to_record_line, character(1))
}

#' Fetch raw 17Lands event-history rows directly from the website
#'
#' The function authenticates with username/password from environment variables,
#' then requests the events payload and returns lines compatible with
#' `extract_data_lines()`.
#'
#' @return A character vector of tab-delimited record lines.
fetch_17lands_record_raw <- function() {
  credentials <- read_17lands_credentials()
  auth_req <- login_17lands(credentials)

  candidate_urls <- c(
    "https://www.17lands.com/user/data/events",
    "https://www.17lands.com/user/data/history/events",
    "https://www.17lands.com/user/data"
  )

  statuses <- character()

  for (url in candidate_urls) {
    events_req <- auth_req |>
      httr2::req_url(url) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_headers(`x-cancel-previous` = "true")

    events_resp <- httr2::req_perform(events_req)
    events_status <- httr2::resp_status(events_resp)
    events_text <- httr2::resp_body_string(events_resp)

    statuses <- c(statuses, paste0(url, " -> ", events_status))

    if (events_status %in% c(401L, 403L)) {
      stop(
        "17Lands authentication was rejected while fetching event data.",
        call. = FALSE
      )
    }

    if (events_status >= 400) {
      next
    }

    payload <- tryCatch(
      jsonlite::fromJSON(events_text, simplifyVector = FALSE),
      error = function(e) NULL
    )

    if (is.null(payload)) {
      next
    }

    record_lines <- extract_record_lines_from_payload(payload)
    if (length(record_lines) > 0) {
      return(record_lines)
    }
  }

  stop(
    paste0(
      "Could not find 17Lands event rows from known endpoints. Tried: ",
      paste(statuses, collapse = "; "),
      "."
    ),
    call. = FALSE
  )
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

#' Fetch one set-level 17Lands all-decks average win rate
#'
#' @param set_code Set code (for example, "SOS").
#' @param event_type 17Lands event type (for example, "PremierDraft").
#' @param fallback_event_types Fallback event types if primary has no games.
#' @param start_date Start date in YYYY-MM-DD.
#' @param end_date End date in YYYY-MM-DD.
#'
#' @return A tibble with one row and set-level benchmark columns.
fetch_17lands_set_average <- function(
  set_code,
  event_type = "PremierDraft",
  fallback_event_types = c("QuickDraft", "PickTwoDraft"),
  start_date = "2000-06-01",
  end_date = as.character(Sys.Date())
) {
  event_types <- unique(c(event_type, fallback_event_types))

  for (event_type_i in event_types) {
    referer <- paste0(
      "https://www.17lands.com/deck_color_data?expansion=",
      utils::URLencode(set_code, reserved = TRUE),
      "&format=",
      utils::URLencode(event_type_i, reserved = TRUE),
      "&start=",
      utils::URLencode(start_date, reserved = TRUE)
    )

    req <- httr2::request("https://www.17lands.com/color_ratings/data") |>
      httr2::req_url_query(
        expansion = set_code,
        event_type = event_type_i,
        start_date = start_date,
        end_date = end_date,
        combine_splash = "true"
      ) |>
      httr2::req_headers(
        accept = "application/json, text/plain, */*",
        `x-requested-with` = "XMLHttpRequest",
        referer = referer
      ) |>
      httr2::req_error(is_error = function(resp) FALSE)

    resp <- httr2::req_perform(req)
    status <- httr2::resp_status(resp)

    if (status >= 400) {
      next
    }

    ratings <- tryCatch(
      jsonlite::fromJSON(httr2::resp_body_string(resp)),
      error = function(e) NULL
    )

    if (is.null(ratings) || !is.data.frame(ratings) || nrow(ratings) == 0) {
      next
    }

    all_decks <- ratings |>
      dplyr::filter(color_name == "All Decks")

    if (nrow(all_decks) == 0) {
      all_decks <- ratings |>
        dplyr::filter(!is_summary)

      wins <- sum(all_decks$wins, na.rm = TRUE)
      games <- sum(all_decks$games, na.rm = TRUE)
    } else {
      wins <- all_decks$wins[[1]]
      games <- all_decks$games[[1]]
    }

    if (games > 0) {
      losses <- games - wins
      win_rate <- 100 * wins / games

      return(
        tibble::tibble(
          set_code = set_code,
          lands_wins = as.numeric(wins),
          lands_losses = as.numeric(losses),
          lands_win_rate = as.numeric(win_rate)
        )
      )
    }
  }

  tibble::tibble(
    set_code = set_code,
    lands_wins = 0,
    lands_losses = 0,
    lands_win_rate = NA_real_
  )
}

#' Fetch set-level 17Lands all-decks averages for many sets
#'
#' @param set_codes Character vector of set codes.
#' @param event_type 17Lands event type (for example, "PremierDraft").
#' @param fallback_event_types Fallback event types if primary has no games.
#' @param start_date Start date in YYYY-MM-DD.
#' @param end_date End date in YYYY-MM-DD.
#'
#' @return A tibble with one row per set.
fetch_17lands_set_averages <- function(
  set_codes,
  event_type = "PremierDraft",
  fallback_event_types = c("QuickDraft", "PickTwoDraft"),
  start_date = "2000-06-01",
  end_date = as.character(Sys.Date())
) {
  unique_codes <- unique(as.character(set_codes))

  dplyr::bind_rows(
    lapply(
      unique_codes,
      function(code) {
        fetch_17lands_set_average(
          set_code = code,
          event_type = event_type,
          fallback_event_types = fallback_event_types,
          start_date = start_date,
          end_date = end_date
        )
      }
    )
  )
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
