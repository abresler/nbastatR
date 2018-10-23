get_pbp <-
  function(game_id = 21601112,
           period_start = 0,
           period_end = 12,
           return_message = T,
           ...) {
    game_slug <-
      pad_id(game_id)
    json_url <-
      make_url(
        datatype = "playbyplay",
        GameID = game_slug,
        StartPeriod = period_start,
        EndPeriod = period_end
      )

    if (return_message) {
      glue::glue("Getting play by play for game {game_id}") %>% cat(fill = T)
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

get_fanduel <-
  function(game_id = 21700003,
           return_message = TRUE) {
    game_slug <-
      pad_id(game_id)
    json_url <-
      glue::glue(
      "http://stats.nba.com/stats/infographicfanduelplayer/?gameId={game_slug}"
      ) %>%
      as.character()

    if (return_message) {
      glue::glue("Getting fanduel summary for game {game_id}") %>% cat(fill = T)
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
      munge_nba_data() %>%
      suppressMessages()
    data

  }

get_win_prob <-
  function(game_id = 21601112,
           period_start = 0,
           period_end = 12,
           return_message = T,
           ...) {
    game_slug <-
      pad_id(game_id)
    json_url <-
      glue::glue(
        "http://stats.nba.com/stats/winprobabilitypbp?SeasonType=&LeagueID=&Season=&IsOnlyCurrentSeason=&PlayerID=&TeamID=&GameID={game_slug}&ContextMeasure=&PlayerPosition=&DateFrom=&DateTo=&GameSegment=&LastNGames=&Location=&Month=&OpponentTeamID=&Outcome=&SeasonSegment=&VSConference=&VSDivision=&RookieYear=&Period=&StartPeriod=0&EndPeriod=12&StartRange=0&EndRange=12&RangeType=1&Runtype=each%20second"
      ) %>%
      as.character()

    if (return_message) {
      glue::glue("Getting win probability and play-by-play for game {game_id}") %>% cat(fill = T)
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

    df_metadata <-
      json$resultSets$rowSet[[2]] %>%
      as_data_frame()

    names_md <-
      json$resultSets$headers[[2]]

    names_md <- names_md %>% resolve_nba_names()

    df_metadata <-
      df_metadata %>%
      purrr::set_names(names_md) %>%
      mutate(dateGame = dateGame %>% lubridate::mdy()) %>%
      mutate_at(c("idGame", "idTeamHome", "idTeamAway", "ptsTotalTeamHome", "ptsTotalTeamAway"),
                funs(. %>% as.integer())
      ) %>%
      select(-matches("pts"))

    names_md <- names(df_metadata)


    data <-
      data %>%
      purrr::set_names(actual_names) %>%
      munge_nba_data() %>%
      dplyr::rename(slugLocationPossession = locationGame) %>%
      left_join(df_metadata) %>%
      select(one_of(names_md), everything()) %>%
      suppressMessages()

    data
  }


#' NBA games win probabilities
#'
#' Gets nba in-game win probabilities
#' for specified game ids
#'
#' @param game_ids vector of game ids
#' @param filter_non_plays if \code{TRUE} filters out non plays
#' @param nest_data if \code{TRUE} nests data
#' @param return_message if \code{T} returns message
#' @return a \code{data_frame}
#' @export
#' @import dplyr curl stringr lubridate readr magrittr tidyr httr purrr jsonlite
#' @importFrom glue glue
#' @family game
#' @family season
#' @return a `data_frame`
#' @export
#'
#' @examples
#' get_games_pbp_win_probablity(game_ids = c(21700002, 21700005), filter_non_plays = T,
#' nest_data = FALSE,
#' return_message = TRUE)

get_games_pbp_win_probablity <-
  function(game_ids = c(21700002, 21700003),
           nest_data = FALSE,
           filter_non_plays = FALSE,
           return_message = TRUE) {
    if (game_ids %>% purrr::is_null()) {
      stop("Please enter game ids")
    }
    get_win_prob_safe <-
      purrr::possibly(get_win_prob, data_frame())

    all_data <-
      game_ids %>%

      future_map_dfr(function(game_id){
        get_win_prob_safe(game_id = game_id, return_message = return_message)
      })

    if (!'df_nba_team_dict' %>% exists()) {
      df_nba_team_dict <- get_nba_teams()

      assign('df_nba_team_dict', df_nba_team_dict, envir = .GlobalEnv)
    }

    all_data <-
      all_data %>%
      left_join(
        df_nba_team_dict %>% select(idTeamHome = idTeam, nameTeamHome = nameTeam)
      ) %>%
      left_join(
        df_nba_team_dict %>% select(idTeamAway = idTeam, nameTeamAway = nameTeam)
      ) %>%
      select(idGame, dateGame, matches("nameTeam"), everything()) %>%
      suppressMessages()

    if (filter_non_plays) {
      all_data <-
        all_data %>%
        filter(isPlayVisible == T)
    }

    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-c(dateGame, idGame, nameTeamHome, idTeamHome, slugTeamHome, nameTeamAway, idTeamAway, slugTamAway), .key = 'dataWinProbability')
    }
    all_data
  }


#' NBA games play-by play
#'
#' Returns play-by-play data
#' for specified game ids
#'
#' @param game_ids vector of game ids
#' @param nest_data if \code{TRUE} nests data
#' @param return_message if \code{T} returns message
#'
#' @return a \code{data_frame}
#' @export
#' @import dplyr curl stringr lubridate readr magrittr tidyr httr purrr jsonlite
#' @importFrom glue glue
#' @examples
#' get_games_play_by_play(game_ids = c(21700002, 21700003), nest_data = F, return_message = T)
get_games_play_by_play <-
  function(game_ids = NULL,
           nest_data = FALSE,
           return_message = TRUE) {
    if (game_ids %>% purrr::is_null()) {
      stop("Please enter game ids")
    }
    get_pbp_safe <-
      purrr::possibly(get_pbp, data_frame())

    all_data <-
      game_ids %>%
      future_map_dfr(function(game_id){
        get_pbp_safe(game_id = game_id, return_message = return_message)
      })

    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-c(idGame), .key = dataPlayByPlay)
    }
      all_data
  }


#' Games fanduel summary
#'
#' Returns fanduel summary for specified
#' game ids
#'
#' @param game_ids vector of game ids
#' @param nest_data if \code{TRUE} nests data
#' @param return_message if \code{T} returns message
#'
#' @return a \code{data_frame}
#' @export
#' @family season
#' @family game
#' @import dplyr curl stringr lubridate readr magrittr tidyr httr purrr jsonlite
#' @importFrom glue glue
#'
#' @examples
#' get_games_fanduel_summary(game_ids = c(21700002, 21700003), nest_data = F, return_message = T)

get_games_fanduel_summary <-
  function(game_ids = c(21700002, 21700003),
           nest_data = FALSE,
           return_message = TRUE) {
    if (game_ids %>% purrr::is_null()) {
      stop("Please enter game ids")
    }
    get_fanduel_safe <-
      purrr::possibly(get_fanduel, data_frame())

    all_data <-
      game_ids %>%
      future_map_dfr(function(game_id){
        get_fanduel_safe(game_id = game_id, return_message = return_message)
      })

    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-c(idGame), .key = dataFanDuel)
    }
    all_data
  }
