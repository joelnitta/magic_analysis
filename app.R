library(dplyr)
library(ggplot2)
library(readr)
library(scales)
library(shiny)
library(stringr)
library(tidyr)

wins_file <- "working/wins_losses.csv"
decks_file <- "working/deck_color_counts.csv"

read_dashboard_data <- function() {
  if (!file.exists(wins_file) || !file.exists(decks_file)) {
    stop(
      paste0(
        "Missing dashboard input files in working/. Run targets::tar_make() ",
        "first to generate wins_losses and deck_color_counts CSV files."
      ),
      call. = FALSE
    )
  }

  wins_losses <- readr::read_csv(wins_file, show_col_types = FALSE) |>
    mutate(
      games = wins + losses,
      win_rate = dplyr::coalesce(win_rate, 0)
    )

  set_order <- wins_losses$set_code

  deck_color_counts <- readr::read_csv(decks_file, show_col_types = FALSE) |>
    mutate(set_code = factor(set_code, levels = set_order))

  deck_long <- deck_color_counts |>
    pivot_longer(
      cols = starts_with("deck_color_"),
      names_to = "deck_color",
      values_to = "n"
    ) |>
    mutate(
      deck_color = str_remove(deck_color, "^deck_color_"),
      set_code = factor(set_code, levels = set_order)
    )

  list(wins_losses = wins_losses, deck_long = deck_long)
}

data_bundle <- read_dashboard_data()

deck_controls_data <- data_bundle$deck_long |>
  mutate(
    main_color = str_remove_all(deck_color, "[a-z]"),
    is_main_color = str_detect(deck_color, "^[A-Z]+$")
  )

main_color_choices <- deck_controls_data |>
  distinct(main_color) |>
  filter(main_color != "") |>
  pull(main_color) |>
  sort()

deck_top_n_default <- min(
  12,
  max(deck_controls_data |> distinct(deck_color) |> nrow(), 1)
)

deck_top_n_max <- max(deck_controls_data |> distinct(deck_color) |> nrow(), 1)

ui <- fluidPage(
  titlePanel("MTGA Draft Dashboard"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "min_games",
        "Minimum games played per set",
        min = 0,
        max = max(data_bundle$wins_losses$games, na.rm = TRUE),
        value = min(10, max(data_bundle$wins_losses$games, na.rm = TRUE)),
        step = 1
      ),
      helpText(
        "Use this filter to hide sets with very little data in the win-rate ",
        "chart."
      ),
      radioButtons(
        "deck_detail_mode",
        "Deck color detail",
        choices = c(
          "Main colors only (default)" = "main_only",
          "Main colors + variants within selected main colors" = "subset",
          "All color combinations" = "all"
        ),
        selected = "main_only"
      ),
      selectizeInput(
        "main_color_subset",
        "Main color subset",
        choices = main_color_choices,
        selected = main_color_choices,
        multiple = TRUE
      ),
      sliderInput(
        "deck_top_n",
        "Maximum deck color combinations shown",
        min = 1,
        max = deck_top_n_max,
        value = deck_top_n_default,
        step = 1
      )
    ),
    mainPanel(
      h3("Win Rate by Set"),
      plotOutput("win_rate_plot", height = "420px"),
      h3("Deck Color Composition by Set"),
      plotOutput("deck_plot", height = "420px")
    )
  )
)

server <- function(input, output, session) {
  filtered_wins <- reactive({
    data_bundle$wins_losses |>
      filter(games >= input$min_games) |>
      mutate(set_code = factor(set_code, levels = set_code))
  })

  filtered_decks <- reactive({
    keep_sets <- filtered_wins()$set_code

    deck <- data_bundle$deck_long |>
      filter(as.character(set_code) %in% as.character(keep_sets)) |>
      mutate(
        main_color = str_remove_all(deck_color, "[a-z]"),
        main_color = dplyr::if_else(main_color == "", "Other", main_color),
        is_main_color = str_detect(deck_color, "^[A-Z]+$")
      )

    if (input$deck_detail_mode == "main_only") {
      deck <- deck |>
        mutate(deck_color = main_color) |>
        group_by(set_code, deck_color) |>
        summarise(n = sum(n), .groups = "drop")
    } else if (input$deck_detail_mode == "subset") {
      deck <- deck |>
        filter(main_color %in% input$main_color_subset)
    }

    ranked_colors <- deck |>
      group_by(deck_color) |>
      summarise(total_n = sum(n), .groups = "drop") |>
      arrange(desc(total_n), deck_color) |>
      slice_head(n = input$deck_top_n) |>
      pull(deck_color)

    deck |>
      filter(deck_color %in% ranked_colors) |>
      mutate(deck_color = factor(deck_color, levels = ranked_colors))
  })

  output$win_rate_plot <- renderPlot({
    wins <- filtered_wins()

    if (nrow(wins) == 0) {
      plot.new()
      text(0.5, 0.5, "No sets match the selected minimum games filter.")
      return(invisible(NULL))
    }

    ggplot(wins, aes(x = set_code, y = win_rate, fill = win_rate)) +
      geom_col(width = 0.7) +
      geom_text(
        aes(label = paste0(wins, "-", losses)),
        vjust = -0.4,
        size = 3
      ) +
      scale_fill_gradient(low = "#c7e9c0", high = "#238b45") +
      scale_y_continuous(
        labels = label_percent(scale = 1),
        limits = c(0, max(wins$win_rate, na.rm = TRUE) + 8)
      ) +
      labs(
        x = "Set",
        y = "Win Rate",
        fill = "Win Rate",
        subtitle = "Labels show wins-losses"
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "none")
  })

  output$deck_plot <- renderPlot({
    deck <- filtered_decks()

    if (nrow(deck) == 0) {
      plot.new()
      text(0.5, 0.5, "No deck data for selected sets.")
      return(invisible(NULL))
    }

    ggplot(deck, aes(x = set_code, y = n, fill = deck_color)) +
      geom_col(width = 0.7) +
      labs(x = "Set", y = "Number of Decks", fill = "Deck Color") +
      theme_minimal(base_size = 12)
  })
}

app <- shinyApp(ui = ui, server = server)
