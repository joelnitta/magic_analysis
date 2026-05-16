library(dplyr)
library(ggplot2)
library(readr)
library(scales)
library(shiny)
library(stringr)
library(tidyr)

wins_file <- "working/wins_losses.csv"
decks_file <- "working/deck_color_counts.csv"
lands_file <- "working/lands_set_averages.csv"

dashboard_inputs <- c(wins_file, decks_file, lands_file)

last_updated <- if (all(file.exists(dashboard_inputs))) {
  format(
    max(file.info(dashboard_inputs)$mtime, na.rm = TRUE),
    "%Y-%m-%d %H:%M %Z"
  )
} else {
  "unavailable"
}

read_dashboard_data <- function() {
  if (
    !file.exists(wins_file) ||
      !file.exists(decks_file) ||
      !file.exists(lands_file)
  ) {
    stop(
      paste0(
        "Missing dashboard input files in working/. Run targets::tar_make() ",
        "first to generate wins_losses, deck_color_counts, and ",
        "lands_set_averages CSV files."
      ),
      call. = FALSE
    )
  }

  wins_losses <- readr::read_csv(wins_file, show_col_types = FALSE) |>
    mutate(
      games = wins + losses,
      win_rate = dplyr::coalesce(win_rate, 0)
    )

  lands_set_averages <- readr::read_csv(lands_file, show_col_types = FALSE)

  wins_losses <- wins_losses |>
    left_join(lands_set_averages, by = "set_code")

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
          "All color combinations" = "all"
        ),
        selected = "main_only"
      ),
      helpText(
        "Color notation: uppercase letters are main colors and lowercase ",
        "letters are splash colors (for example, WUr has main colors W/U and ",
        "a red splash)."
      ),
      helpText(
        "Main colors only: variants are grouped into their uppercase base ",
        "colors (deck totals stay the same; labels are simplified)."
      ),
      helpText(
        "All color combinations: each exact color string is shown as its own ",
        "category."
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
      plotOutput("deck_plot", height = "420px"),
      tags$hr(),
      tags$p(
        style = "font-size: 0.9em; color: #666;",
        paste("Last updated:", last_updated),
        tags$span(" | "),
        tags$a(
          href = "https://github.com/joelnitta/magic_analysis",
          target = "_blank",
          rel = "noopener noreferrer",
          icon("github"),
          " Source code"
        )
      )
    )
  )
)

server <- function(input, output, session) {
  mtg_colors <- c(
    W = "#F8F6D8",
    U = "#0E68AB",
    B = "#837C79",
    R = "#D3202A",
    G = "#00733E",
    Other = "#8A8A8A"
  )

  filtered_wins <- reactive({
    data_bundle$wins_losses |>
      filter(games >= input$min_games) |>
      mutate(set_code = factor(set_code, levels = set_code))
  })

  filtered_decks <- reactive({
    keep_sets <- as.character(filtered_wins()$set_code)

    deck <- data_bundle$deck_long |>
      filter(as.character(set_code) %in% keep_sets) |>
      mutate(
        set_code = factor(as.character(set_code), levels = keep_sets),
        main_color = str_remove_all(deck_color, "[a-z]"),
        main_color = dplyr::if_else(main_color == "", "Other", main_color),
        is_main_color = str_detect(deck_color, "^[A-Z]+$")
      )

    if (input$deck_detail_mode == "main_only") {
      deck <- deck |>
        mutate(deck_color = main_color) |>
        group_by(set_code, deck_color) |>
        summarise(n = sum(n), .groups = "drop")
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

    bar_width <- 0.7

    wins_plot_df <- wins |>
      mutate(
        set_code = factor(
          as.character(set_code),
          levels = as.character(set_code)
        ),
        set_x = as.numeric(set_code)
      )

    lands_total_games <- sum(
      wins_plot_df$lands_wins + wins_plot_df$lands_losses,
      na.rm = TRUE
    )
    overall_17lands_rate <- if (lands_total_games > 0) {
      100 * sum(wins_plot_df$lands_wins, na.rm = TRUE) / lands_total_games
    } else {
      mean(wins_plot_df$lands_win_rate, na.rm = TRUE)
    }

    wins_plot_df <- wins_plot_df |>
      mutate(win_rate_delta = win_rate - overall_17lands_rate)

    scico_diverging <- scico::scico(3, palette = "roma")
    delta_limit <- max(abs(wins_plot_df$win_rate_delta), na.rm = TRUE)
    legend_breaks <- if (delta_limit > 0) {
      c(-delta_limit, 0, delta_limit)
    } else {
      0
    }

    ggplot(
      wins_plot_df,
      aes(x = set_x, y = win_rate, fill = win_rate_delta)
    ) +
      geom_hline(yintercept = 50, linetype = "dashed", color = "#6B6B6B") +
      geom_col(width = bar_width) +
      # Add win-loss and win rate label at bottom of bar
      geom_text(
        aes(
          y = 0,
          label = paste0(wins, "-", losses, "\n", sprintf("%.1f%%", win_rate))
        ),
        vjust = -0.1,
        size = 3
      ) +
      scale_fill_gradient2(
        low = scico_diverging[3],
        mid = scico_diverging[2],
        high = scico_diverging[1],
        midpoint = 0,
        limits = c(-delta_limit, delta_limit),
        breaks = legend_breaks,
        labels = function(x) sprintf("%+.1f%%", x)
      ) +
      scale_y_continuous(
        labels = label_percent(scale = 1, accuracy = 0.1),
        limits = c(0, max(wins$win_rate, na.rm = TRUE) + 8),
        breaks = function(x) sort(unique(c(pretty(x, n = 6), 50)))
      ) +
      scale_x_continuous(
        breaks = seq_along(levels(wins_plot_df$set_code)),
        labels = levels(wins_plot_df$set_code),
        expand = expansion(mult = c(0.02, 0.02))
      ) +
      labs(
        x = "Set",
        y = "Win Rate",
        fill = "Win Rate vs. 17lands",
        subtitle = paste0(
          "Fill is centered on overall 17Lands average (",
          sprintf("%.1f%%", overall_17lands_rate),
          "). Labels: wins-losses and win rate (bottom)."
        )
      ) +
      # Add shorter, dashed horizontal line for 17Lands win rate
      geom_segment(
        data = wins_plot_df[!is.na(wins_plot_df$lands_win_rate), ],
        aes(
          x = set_x - 0.25 * bar_width,
          xend = set_x + 0.25 * bar_width,
          y = lands_win_rate,
          yend = lands_win_rate
        ),
        color = "#1A1A1A",
        linewidth = 1.1,
        linetype = "33",
        inherit.aes = FALSE
      ) +
      geom_point(
        aes(y = lands_win_rate),
        inherit.aes = TRUE,
        shape = 21,
        fill = "#1A1A1A",
        color = "white",
        stroke = 0.35,
        size = 2.8,
        na.rm = TRUE
      ) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank()
      )
  })

  output$deck_plot <- renderPlot({
    deck <- filtered_decks()

    if (nrow(deck) == 0) {
      plot.new()
      text(0.5, 0.5, "No deck data for selected sets.")
      return(invisible(NULL))
    }

    bar_width <- 0.7

    deck_plot_df <- deck |>
      mutate(
        set_code = factor(set_code, levels = levels(set_code)),
        set_x = as.numeric(set_code),
        color_letters = lapply(
          deck_color,
          function(x) {
            mtg_letters <- stringr::str_extract_all(x, "[WUBRGwubrg]")[[1]] |>
              toupper() |>
              unique()

            if (length(mtg_letters) == 0) {
              return("Other")
            }

            mtg_letters
          }
        ),
        n_parts = pmax(lengths(color_letters), 1)
      ) |>
      group_by(set_code) |>
      arrange(deck_color, .by_group = TRUE) |>
      mutate(
        ymax = cumsum(n),
        ymin = ymax - n,
        xmin = set_x - (bar_width / 2),
        xmax = set_x + (bar_width / 2)
      )

    stripe_df <- deck_plot_df |>
      mutate(seg_id = row_number()) |>
      select(
        seg_id,
        set_code,
        xmin,
        xmax,
        ymin,
        ymax,
        color_letters,
        n_parts
      ) |>
      tidyr::unnest_longer(
        color_letters,
        values_to = "slice_color",
        indices_to = "slice_index"
      ) |>
      mutate(
        slice_color = if_else(
          slice_color %in% names(mtg_colors),
          slice_color,
          "Other"
        ),
        slice_xmin = xmin + ((slice_index - 1) / n_parts) * (xmax - xmin),
        slice_xmax = xmin + (slice_index / n_parts) * (xmax - xmin)
      )

    p <- ggplot() +
      geom_rect(
        data = stripe_df,
        aes(
          xmin = slice_xmin,
          xmax = slice_xmax,
          ymin = ymin,
          ymax = ymax,
          fill = slice_color
        ),
        color = "white",
        linewidth = 0.2
      )

    p <- p +
      geom_rect(
        data = deck_plot_df,
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
        fill = NA,
        color = "#2F2F2F",
        linewidth = 0.45
      )

    p +
      scale_fill_manual(values = mtg_colors, drop = FALSE) +
      scale_x_continuous(
        breaks = seq_along(levels(deck_plot_df$set_code)),
        labels = levels(deck_plot_df$set_code),
        expand = expansion(mult = c(0.02, 0.02))
      ) +
      labs(
        x = "Set",
        y = "Number of Decks",
        fill = "Color",
        subtitle = "Each segment is split into color slivers, including splashes"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "bottom",
        legend.box = "horizontal",
        panel.grid.minor = element_blank()
      ) +
      guides(fill = guide_legend(nrow = 2, byrow = TRUE))
  })
}

app <- shinyApp(ui = ui, server = server)
