dictionary_player_tables <-
  data_frame(slugTable = c("playerdashboardbyclutch", "leaguedashplayerbiostats",
                           "PlayerClutchStats", "PlayerCompareStats",
                           "PlayerDefenseStats", "playerfantasyprofile",
                           "PlayerLastNGamesStats", "shotchartdetail",
                           "PlayerGameLog", "playergamelogs", "PlayerNextNGames",
                           "leagueplayerondetails", "playerdashptreb",
                           "PlayerYearOverYearStats", "playervsplayer",
                           "playerdashboardbyteamperformance", "playerdashptshots",
                           "PlayerPassesStats", "playerdashboardbyopponent",
                           "PlayerYearOverYearStats"))



# PlayerCareerStats -------------------------------------------------------
# http://nbasense.com/nba-api/Stats/Stats/Player/PlayerCareerStats


# playerpasss -------------------------------------------------------------
# http://nbasense.com/nba-api/Stats/Stats/Player/PlayerPassesStats#request-example

passes <-
  function(game_id = 21601112,
           period_start = 0,
           period_end = 12,
           return_message = T,
           ...) {
    game_slug <-
      pad_id(game_id)
    json_url <-
      make_url(
        datatype = "PlayerPassesStats",
        GameID = game_slug,
        StartPeriod = period_start,
        EndPeriod = period_end
      )

    if (return_message) {
      glue::glue("Getting play by play for game {game_id}") %>% message()
    }
    json <-
      json_url  %>%
      curl_json_to_vector()

    data <-
      json$resultSets$rowSet[[1]] %>%
      as_data_frame()

    json_names <-
      json$resultSets$headers[[1]]
    actual_names <-
      json_names %>% resolve_nba_names()

    data <-
      data %>%
      purrr::set_names(actual_names) %>%
      munge_nba_data()

    data
  }
