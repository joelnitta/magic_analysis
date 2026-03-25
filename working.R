library(mtgr)
library(tidyverse)

tla <- mr_get_17lands_data(set = "tdm", data_type = "game", event_type = "premier")

tla_top_wr <- read_tsv("tla_top_users_2color_wr.tsv")

tla_top_wr |> pull(n_games) |> sum()


tla |>
  # filter(user_game_win_rate_bucket > 0.64, user_n_games_bucket > 10) |>
  count(main_colors) |>
  as_tibble() |>
  # filter(splash_colors == "") |>
  mutate(n_color = nchar(main_colors)) |>
  filter(n_color == 2) |>
  arrange(desc(n))



dim(tla)

tla |>
  count(user_n_games_bucket)

total_win_rate <- sum(tla$won) / nrow(tla)

tla |> count(user_game_win_rate_bucket)

tla |> colnames() |> head(n = 20)
tla |> colnames() |> tail(n = 20)


tdm <- mr_get_17lands_data(set = "tdm", data_type = "game", event_type = "premier")

head(colnames(tdm), n = 20)

# Look up trophy decks with a selected card

tdm |> 
  select(`deck_Ambling Stormshell`, won, draft_id) |>
  filter(`deck_Ambling Stormshell` == 1) |>
  group_by(draft_id) |>
  add_count(won) |>
  ungroup() |>
  filter(n > 6) |>
  count(draft_id)
