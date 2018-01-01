
# full_logs ---------------------------------------------------------------
get_season_gamelog <-
  function(season = 2018,
           result_type  = "player",
           season_type = "Regular Season",
           date_from = NULL,
           date_to =  Sys.Date() - 1,
           return_message = TRUE,
           ...) {
    if (season < 1947) {
      stop("NBA data starts for the 1946-47 season")
    }
    slug_type <- result_type %>% str_to_lower()

    if (!slug_type %in% c("team", "player")) {
      stop("Result type can only be player or team")
    }

    table_slug <-
      case_when(slug_type == "team" ~ "T",
                TRUE ~ "P")
    season_slug <-
      generate_season_slug(season = season)

    if (return_message) {
      glue::glue("Acquiring basic {result_type} game logs for the {season_slug} {season_type}") %>% message()
    }

    URL <- gen_url("leaguegamelog")
    params <- list(
      Season = season_slug,
      LeagueID = '00',
      PlayerOrTeam = table_slug,
      Direction = 'DESC',
      SeasonType = season_type,
      Sorter = 'FGM',
      DateFrom = date_from,
      DateTo = date_to,
      # Defaulting to yesterday as it gets wonky with games still in play
      Counter = 0
    )
    params <- utils::modifyList(params, list(...))

    nba_h <- get_nba_headers()

    resp <-
      httr::GET(url = URL, query = params, nba_h) %>%
      httr::content("text", encoding = "UTF-8")
    json <-
      resp %>% jsonlite::fromJSON(simplifyVector = T)

    data <-
      json %>%
      nba_json_to_df() %>%
      mutate(slugSeason = season_slug,
             typeSeason = season_type,
             typeResult = result_type) %>%
      mutate(dateGame = dateGame %>% lubridate::ymd()) %>%
      select(typeResult, typeSeason, slugSeason, everything()) %>%
      arrange(dateGame)

    data <-
      data %>%
      mutate(slugTeamWinner = case_when(outcomeGame == "W" ~ slugTeam,
                                        TRUE ~ slugOpponent),
             slugTeamLoser = case_when(outcomeGame == "L" ~ slugTeam,
                                       TRUE ~ slugOpponent)
      )

    data <-
      data %>%
      clean_data_table_name()

    data <-
      data %>%
      group_by(slugSeason, idTeam) %>%
      mutate(numberGameTeamSeason = 1:n()) %>%
      ungroup() %>%
      dplyr::select(typeSeason:teamName, numberGameTeamSeason, everything())

    if (result_type == "player") {
      data <-
        data %>%
        group_by(slugSeason, idPlayer) %>%
        mutate(numberGamePlayerSeason = 1:n()) %>%
        ungroup() %>%
        dplyr::select(typeSeason:teamName, numberGameTeamSeason, numberGamePlayerSeason, everything())
    }



    data <-
      data %>%
      mutate(yearSeason = season,
             typeResult = result_type) %>%
      mutate(urlTeamSeasonLogo = generate_team_season_logo(season = yearSeason, slug_team = slugTeam)) %>%
      dplyr::select(typeResult, typeSeason, yearSeason, everything()) %>%
      nest(-c(typeResult, slugSeason, yearSeason), .key = 'dataTables')

    closeAllConnections()
    data
  }


#' NBA game logs
#' NBA game logs for specified parameers
#'
#' @param seasons vector of seasons where season is year ending
#' @param result_types vector of result types \itemize{
#' \item player
#' \item team
#' }
#' @param season_types vector of Season types \itemize{
#' \item Regular Season
#' \item Playoffs
#' \item Pre Season
#' \item All Star
#' }
#' @param nest_data if \code{TRUE} nests data
#' @param assign_to_environment assigns individual table to environment
#' @param ...
#'
#' @return
#' @export
#' @import dplyr jsonlite purrr stringr lubridate magrittr tidyr tibble httr
#' @importFrom  glue glue
#' @examples
#' get_game_logs(seasons = 2017:2018, result_types = c("team", "player"))
get_game_logs <-
  function(seasons = 2017:2018,
           result_types  = "player",
           season_types = "Regular Season",
           nest_data = F,
           assign_to_environment = TRUE,
           return_message = TRUE,
           ...) {
    result_length <- result_types %>% length()
    if (result_length == 2) {
      result_types <-  c("player", "team")
    }

    input_df <-
      expand.grid(
        season = seasons,
        result = result_types,
        season_type = season_types,
        stringsAsFactors = F
      ) %>%
      as_data_frame() %>%
      arrange(season)

    get_season_gamelog_safe <-
      purrr::possibly(get_season_gamelog, data_frame())

    all_data <-
      1:nrow(input_df) %>%
      purrr::map_df(function(x) {
        df_row <-
          input_df %>% slice(x)
        data_row <-
          df_row %$%
          get_season_gamelog_safe(season = season,
                                  result_type = result,
                                  season_type = season_type,
                                  return_message = return_message)
        data_row
      })

    if (result_length == 1) {
      all_data <-
        all_data %>%
        select(-typeResult) %>%
        unnest()

      return(all_data)
    }

    if (assign_to_environment) {
      results <-
        all_data$typeResult %>% unique()

      results %>%
        walk(function(result){
          df_table <-
            all_data %>%
            filter(typeResult == result) %>%
            select(-typeResult) %>%
            unnest()

          col_order <- c("typeSeason","yearSeason" ,"slugSeason", "idSeason","idTeam", "slugMatchup","idGame", "outcomeGame", "locationGame","teamName","slugTeam",  "slugOpponent", "slugTeamWinner", "slugTeamLoser" ,  "dateGame","numberGameTeamSeason","idPlayer", "namePlayer","numberGamePlayerSeason",

                         "minutes", "fgm",
                         "fga", "pctFG", "fg3m", "fg3a", "pctFG3", "ftm", "fta", "pctFT",
                         "oreb", "dreb", "reb", "ast", "stl", "blk", "tov", "pf", "pts",
                         "plusminus", "hasVideo", "fg2m", "fg2a", "pctFG2")

          df_table <-
            df_table %>%
            dplyr::select(one_of(col_order), everything()) %>%
            suppressWarnings()



          if (nest_data) {
            df_table <-
              df_table %>%
              nest(-c(typeSeason, slugSeason, yearSeason), .key = "dataGameLogs")
          }

          if (df_table %>% tibble::has_name("idPlayer")) {
            if (!'df_nba_player_dict' %>% exists()) {
              df_nba_player_dict <-
                get_nba_players()

              assign(x = 'df_nba_player_dict', df_nba_player_dict, envir = .GlobalEnv)
            }

            df_table <-
              df_table %>%
              left_join(df_nba_player_dict %>% select(idPlayer, matches("url"))) %>%
              suppressMessages()

          }

          table_name <- glue::glue("dataGameLogs{str_to_title(result)}") %>% as.character()

          assign(x = table_name, df_table, envir = .GlobalEnv)

        })
    }
    all_data
  }


# schedule ----------------------------------------------------------------

get_season_schedule <-
  function(season = 2018,
           season_type = "Regular Season",
           return_message = TRUE) {

    data <-
      get_season_gamelog(season = season,
                         result_type = "team",
                         season_type = season_type,
                         return_message = return_message) %>%
      unnest()
    data %>%
      select(one_of(
        c(
          "typeSeason",
          "slugSeason",
          "idSeason",
          "idGame" ,
          "dateGame",
          "slugMatchup",
          "slugTeamWinner",
          "slugTeamLoser"
        )
      )) %>%
      group_by(idGame) %>%
      mutate(idRow = 1:n()) %>%
      filter(idRow == min(idRow)) %>%
      ungroup() %>%
      select(-idRow) %>%
      suppressWarnings()

  }

#' Get NBA Seasons Schedule
#'
#' @param seasons vector of seasons where season is year ending
#' @param season_types
#' @param return_message
#'
#' @return
#' @export
#' @import dplyr jsonlite purrr stringr lubridate magrittr tidyr tibble httr
#' @importFrom  glue glue
#' @examples
#' get_seasons_schedule(seasons = c(2012, 2018))
get_seasons_schedule <-
  function(seasons = 2018,
           season_types = "Regular Season",
           nest_data = FALSE,
           return_message = TRUE) {
    input_df <-
      expand.grid(
        season = seasons,
        season_type = season_types,
        stringsAsFactors = F
      ) %>%
      as_data_frame() %>%
      arrange(season)

    get_season_schedule_safe <-
      purrr::possibly(get_season_schedule, data_frame())

    all_data <-
      1:nrow(input_df) %>%
      purrr::map_df(function(x) {
        df_row <-
          input_df %>% slice(x)
        data_row <-
          df_row %$%
          get_season_schedule_safe(season = season,
                                   season_type = season_type,
                                   return_message = return_message)
        data_row
      }) %>%
      arrange(dateGame, idGame) %>%
      group_by(dateGame) %>%
      mutate(numberGameDay = 1:n()) %>%
      ungroup()
    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-c(typeSeason, slugSeason), .key = "dataSchedule")
    }
    all_data
  }



# seasons_players ---------------------------------------------------------
#' Get NBA Player Dictionary
#'
#' @param return_message
#' @param ...
#'
#' @return
#' @export
#' @import dplyr jsonlite purrr stringr lubridate magrittr tidyr tibble httr
#' @importFrom  glue glue
#' @examples
get_seasons_players <-
  function(return_message = TRUE,
           ...) {
    table_id <- 1
    URL <- gen_url("commonallplayers")
    params <- list(
      LeagueID = "00",
      SeasonType = "",
      Season = "2017-18",
      IsOnlyCurrentSeason = "0",
      PlayerID = "",
      TeamID = "",
      GameID = "",
      ContextMeasure = "",
      PlayerPosition = "",
      DateFrom = "",
      DateTo = "",
      GameSegment = "",
      LastNGames = "",
      Location = "",
      Month = "",
      OpponentTeamID = "",
      Outcome = "",
      SeasonSegment = "",
      VSConference = "",
      VSDivision = "",
      RookieYear = "",
      Period = "",
      StartPeriod = "",
      EndPeriod = ""
    )
    params <- utils::modifyList(params, list(...))

    nba_h <- get_nba_headers()

    resp <-
      httr::GET(url = URL, query = params, nba_h) %>%
      httr::content("text", encoding = "UTF-8")
    json <-
      resp %>% jsonlite::fromJSON(simplifyVector = T)

    data <-
      json %>%
      nba_json_to_df(table_id = table_id)

    closeAllConnections()
    data
  }



#' Get Seasons teams
#'
#' @param return_message
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' get_seasons_teams()
get_seasons_teams <-
  function(return_message = TRUE,
           ...) {
    table_id <- 1
    URL <- gen_url("commonteamyears")
    params <- list(
      LeagueID = "00",
      SeasonType = "",
      Season = "2017-18",
      IsOnlyCurrentSeason = "0",
      PlayerID = "",
      TeamID = "",
      GameID = "",
      ContextMeasure = "",
      PlayerPosition = "",
      DateFrom = "",
      DateTo = "",
      GameSegment = "",
      LastNGames = "",
      Location = "",
      Month = "",
      OpponentTeamID = "",
      Outcome = "",
      SeasonSegment = "",
      VSConference = "",
      VSDivision = "",
      RookieYear = "",
      Period = "",
      StartPeriod = "",
      EndPeriod = ""
    )
    params <- utils::modifyList(params, list(...))

    nba_h <- get_nba_headers()

    resp <-
      httr::GET(url = URL, query = params, nba_h) %>%
      httr::content("text", encoding = "UTF-8")
    json <-
      resp %>% jsonlite::fromJSON(simplifyVector = T)

    data <-
      json %>%
      nba_json_to_df(table_id = table_id) %>%
      mutate(isActive = ifelse(slugTeam %>% is.na(), F, T)) %>%
      dplyr::select(-one_of("idLeague")) %>%
      select(isActive, slugTeam, idTeam, everything()) %>%
      arrange(slugTeam)

    closeAllConnections()
    data
  }


get_team_season_roster <-
  function(season = 2018, team_id = 1610612751) {
    if (season < 1947) {
      stop("NBA data starts for the 1946-47 season")
    }
    season_slug <-
      generate_season_slug(season = season)

    table_id <- 1
    URL <- gen_url("commonteamroster")
    params <- list(
      LeagueID = "00",
      SeasonType = "",
      Season = season_slug,
      IsOnlyCurrentSeason = "0",
      PlayerID = "",
      TeamID = team_id,
      GameID = "",
      ContextMeasure = "",
      PlayerPosition = "",
      DateFrom = "",
      DateTo = "",
      GameSegment = "",
      LastNGames = "",
      Location = "",
      Month = "",
      OpponentTeamID = "",
      Outcome = "",
      SeasonSegment = "",
      VSConference = "",
      VSDivision = "",
      RookieYear = "",
      Period = "",
      StartPeriod = "",
      EndPeriod = ""
    )
    # params <- utils::modifyList(params, list(...))

    nba_h <- get_nba_headers()

    resp <-
      httr::GET(url = URL, query = params, nba_h) %>%
      httr::content("text", encoding = "UTF-8")
    json <-
      resp %>% jsonlite::fromJSON(simplifyVector = T)

    data <-
      json %>%
      nba_json_to_df(table_id = table_id) %>%
      mutate(slugSeason = season_slug)

    data <-
      data %>%
      mutate(countYearsExperience = ifelse(is.na(countYearsExperience), 0, countYearsExperience),
             dateBirth = dateBirth %>% lubridate::mdy()) %>%
      tidyr::separate(heightInches, sep = "\\-", into = c("feet", "inches")) %>%
      mutate_at(c("feet", "inches"),
                funs(. %>% as.numeric())) %>%
      mutate(heightInches = (12 * feet) + inches) %>%
      select(-one_of(c("feet", "inches"))) %>%
      select(slugSeason, idTeam:weightLBS, heightInches, everything())

    data
  }


get_season_roster <-
  function(season = 2012, return_message = TRUE) {
    get_team_season_roster_safe <-
      purrr::possibly(get_team_season_roster, data_frame())

    df_teams <-
      get_seasons_teams() %>%
      filter(isActive) %>%
      select(idTeam, slugTeam)

    df_rosters <-
      1:nrow(df_teams) %>%
      map_df(function(x) {
        if (return_message) {
          glue::glue("Acquiring {df_teams$slugTeam[x]}'s team roster for the {season} season") %>% message()
        }
        get_team_season_roster_safe(season = season, team_id = df_teams$idTeam[x])
      })

    df_rosters <-
      df_rosters %>%
      left_join(df_teams) %>%
      mutate(yearSeason = season) %>%
      dplyr::select(yearSeason,
                    slugSeason,
                    slugTeam,
                    idPlayer,
                    namePlayer,
                    everything()) %>%
      mutate(urlTeamSeasonLogo = generate_team_season_logo(season = yearSeason, slug_team = slugTeam)) %>%
      suppressMessages()
    df_rosters
  }


#' Get seasons rosters
#'
#' Returns rosters for each team of a specified season
#'
#' @param seasons vector of seasons
#' @param return_message if \code{TRUE} returns a message
#' @param nest_data if \code{TRUE} nests data
#'
#' @return
#' @export
#' @import dplyr jsonlite purrr stringr lubridate magrittr tidyr tibble httr
#' @importFrom glue glue
#' @examples
#' get_seasons_rosters(2018)

get_seasons_rosters <-
  function(seasons = 2000:2018, return_message = TRUE, nest_data = F) {
    get_season_roster_safe <-
      purrr::possibly(get_season_roster, data_frame())

    all_data <-
      seasons %>%
      map_df(function(season){
        get_season_roster_safe(season = season, return_message = return_message)
      })

    if (!'df_nba_player_dict' %>% exists()) {
      df_nba_player_dict <-
        get_nba_players()

      assign(x = 'df_nba_player_dict', df_nba_player_dict, envir = .GlobalEnv)
    }

    all_data <-
      all_data %>%
      left_join(df_nba_player_dict %>% select(idPlayer, matches("url"))) %>%
      suppressMessages() %>%
      m

    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-c(slugSeason), .key = "dataRosters")
    }
    all_data
  }
