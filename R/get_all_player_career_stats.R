get_all_player_career_season_stats <-
  function(only_active = F,
           per_mode = "Totals",
           return_message = T) {
    players <-
      get_nba_players_ids()

    mode <-
      per_mode %>%
      str_to_title() %>%
      str_replace('\\ ', '') %>%
      match.arg(c('Totals', 'PerGame', "Per36"))

    if (return_message == T) {
      rm <-
        T
    } else {
      rm <-
        F
    }

    if (only_active == T) {
      player_ids <-
        players %>%
        dplyr::filter(is.active_player == T) %>%
        .$id.player
    } else {
      player_ids <-
        players$id.player
    }

    all_players_seasons <-
      player_ids %>%
      purrr::map(function(x)
        get_player_career_stat(per_mode = mode, id.player = x, return_message = rm)) %>%
      compact %>%
      bind_rows
  }
