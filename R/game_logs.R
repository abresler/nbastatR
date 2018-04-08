

# full_logs ---------------------------------------------------------------
get_season_gamelog <-
  function(season = 2018,
           result_type  = "player",
           season_type = "Regular Season",
           date_from = NULL,
           date_to =  Sys.Date() + 1,
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
      mutate(
        slugTeamWinner = case_when(outcomeGame == "W" ~ slugTeam,
                                   TRUE ~ slugOpponent),
        slugTeamLoser = case_when(outcomeGame == "L" ~ slugTeam,
                                  TRUE ~ slugOpponent)
      )

    data <-
      data %>%
      clean_data_table_name() %>%
      mutate(yearSeason = season,
             typeResult = result_type) %>%
      mutate(urlTeamSeasonLogo = generate_team_season_logo(season = yearSeason, slug_team = slugTeam))


    df_teams_games <-
      data %>%
      distinct(yearSeason, dateGame, idTeam, slugTeam) %>%
      group_by(yearSeason, slugTeam) %>%
      mutate(
        numberGameTeamSeason = 1:n(),
        countDaysRestTeam = ifelse(numberGameTeamSeason > 1,
                                    (dateGame - lag(dateGame) - 1),
                                    120),
        countDaysNextGameTeam =
          ifelse(numberGameTeamSeason < 82,
                  ((
                    lead(dateGame) - dateGame
                  ) - 1),
                  120)
      ) %>%
      mutate(
        countDaysNextGameTeam = countDaysNextGameTeam %>% as.numeric(),
        countDaysRestTeam = countDaysRestTeam %>% as.numeric(),
        isB2B = ifelse(countDaysNextGameTeam == 0 |
                          countDaysRestTeam == 0, TRUE, FALSE)
      ) %>%
      mutate(
        isB2BFirst = ifelse(dplyr::lead(countDaysNextGameTeam) == 0, TRUE, FALSE),
        isB2BSecond = ifelse(dplyr::lag(countDaysNextGameTeam) == 0, TRUE, FALSE)
      ) %>%
      ungroup() %>%
      mutate_if(is.logical,
                funs(ifelse(. %>% is.na(), FALSE, .)))

    data <-
      data %>%
      left_join(df_teams_games) %>%
      dplyr::select(one_of(
        c(
          "typeResult",
          "yearSeason",
          "slugSeason",
          "typeSeason",
          "dateGame",
          "idGame",
          "numberGameTeamSeason",
          "nameTeam",
          "idTeam",
          "isB2B",
          "isB2BFirst",
          "isB2BSecond",
          "locationGame",
          "slugMatchup",
          "slugTeam",
          "countDaysRestTeam",
          "countDaysNextGameTeam",
          "slugOpponent",
          "slugTeamWinner",
          "slugTeamLoser",
          "outcomeGame"
        )
      ), everything()) %>%
      suppressMessages()


    if (result_type == "player") {
      if (!'df_nba_player_dict' %>% exists()) {
        df_nba_player_dict <-
          get_nba_players()

        assign(x = 'df_nba_player_dict', df_nba_player_dict, envir = .GlobalEnv)
      }

      data <-
        data %>%
        left_join(df_nba_player_dict %>% select(idPlayer, matches("url"))) %>%
        suppressMessages()


      df_players_games <-
        data %>%
        distinct(yearSeason, dateGame, idPlayer, namePlayer) %>%
        group_by(yearSeason, idPlayer, namePlayer) %>%
        mutate(
          numberGamePlayerSeason = 1:n(),
          countDaysRestPlayer = ifelse(numberGamePlayerSeason > 1,
                                        (dateGame - lag(dateGame) - 1),
                                        120),
          countDaysNextGamePlayer =
            ifelse(countDaysRestPlayer < 82,
                    ((
                      lead(dateGame) - dateGame
                    ) - 1),
                    120)
        ) %>%
        mutate(
          countDaysNextGamePlayer = countDaysNextGamePlayer %>% as.numeric(),
          countDaysRestPlayer = countDaysRestPlayer %>% as.numeric()
        ) %>%
        ungroup() %>%
        mutate_if(is.logical,
                  funs(ifelse(. %>% is.na(), FALSE, .)))

      data <-
        data %>%
        left_join(df_players_games) %>%
        suppressMessages()

      data <-
        data %>%
        dplyr::select(
          typeResult:namePlayer,
          numberGamePlayerSeason,
          countDaysRestPlayer,
          countDaysNextGamePlayer,
          everything()
        )

    }



    data <-
      data %>%
      dplyr::select(typeResult, typeSeason, yearSeason, everything()) %>%
      nest(-c(typeResult, slugSeason, yearSeason), .key = 'dataTables')

    data
  }


#' NBA game logs
#' NBA game logs for specified parameters
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
#' @return a `data_frame`
#' @family game
#' @export
#' @import dplyr jsonlite purrr stringr lubridate magrittr tidyr tibble httr
#' @importFrom  glue glue
#' @examples
#' get_game_logs(seasons = 2017:2018, result_types = c("team", "player"))
get_game_logs <-
  function(seasons = NULL,
           result_types  = NULL,
           season_types = "Regular Season",
           nest_data = F,
           assign_to_environment = TRUE,
           return_message = TRUE,
           ...) {
    if (seasons %>% purrr::is_null()) {
      stop("Please enter season(s)")
    }

    if (result_types %>% purrr::is_null()) {
      stop("Please enter result type {player and/or team}")
    }

    if (result_types %>% str_to_lower()  %in% c("player", "team") %>% sum(na.rm = T) == 0) {
      stop("Result type can only be player and/or team")
    }

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
          get_season_gamelog_safe(
            season = season,
            result_type = result,
            season_type = season_type,
            return_message = return_message,
            ...
          )
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
        walk(function(result) {
          df_table <-
            all_data %>%
            filter(typeResult == result) %>%
            select(-typeResult) %>%
            unnest()

          col_order <-
            c(
              "typeSeason",
              "yearSeason" ,
              "slugSeason",
              "idSeason",
              "idTeam",
              "slugMatchup",
              "idGame",
              "outcomeGame",
              "locationGame",
              "teamName",
              "slugTeam",
              "slugOpponent",
              "slugTeamWinner",
              "slugTeamLoser" ,
              "dateGame",
              "numberGameTeamSeason",
              "idPlayer",
              "namePlayer",
              "numberGamePlayerSeason",

              "minutes",
              "fgm",
              "fga",
              "pctFG",
              "fg3m",
              "fg3a",
              "pctFG3",
              "ftm",
              "fta",
              "pctFT",
              "oreb",
              "dreb",
              "reb",
              "ast",
              "stl",
              "blk",
              "tov",
              "pf",
              "pts",
              "plusminus",
              "hasVideo",
              "fg2m",
              "fg2a",
              "pctFG2"
            )

          df_table <-
            df_table %>%
            dplyr::select(one_of(col_order), everything()) %>%
            suppressWarnings()



          if (nest_data) {
            df_table <-
              df_table %>%
              nest(-c(typeSeason, slugSeason, yearSeason), .key = "dataGameLogs")
          }



          table_name <-
            glue::glue("dataGameLogs{str_to_title(result)}") %>% as.character()

          assign(x = table_name, df_table, envir = .GlobalEnv)

        })
    }
    all_data
  }


# schedule ----------------------------------------------------------------

get_season_schedule <-
  function(season = 2018,
           season_type = "Regular Season",
           parse_boxscores = F,
           box_score_tables = c(
             "Traditional",
             "Advanced",
             "Scoring",
             "Misc",
             "Usage",
             "Four Factors",
             "hustle",
             "tracking"
           ),
           return_message = TRUE) {
    data <-
      get_season_gamelog(
        season = season,
        result_type = "team",
        season_type = season_type,
        return_message = return_message
      ) %>%
      unnest()
    data <-
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
      suppressWarnings() %>%
      arrange(idGame)

    if (parse_boxscores) {
      game_ids <-
        data$idGame %>%
        unique() %>%
        sort()


      df_box_scores <-
        get_games_box_scores(
          game_ids = game_ids,
          box_score_types = box_score_tables,
          result_types = c("player", "team"),
          join_data = T,
          assign_to_environment = F,
          return_message = return_message
        )
      data <-
        data %>%
        left_join(
          df_box_scores %>%
            filter(typeResult == "player") %>%
            select(dataBoxScore) %>%
            unnest() %>%
            nest(-c(idGame, slugTeam), .key = 'dataBoxScorePlayer')
        ) %>%
        left_join(
          df_box_scores %>%
            filter(typeResult == "team") %>%
            select(dataBoxScore) %>%
            unnest() %>%
            nest(-c(idGame, slugTeam), .key = 'dataBoxScoreTeam')
        ) %>%
        mutate(isTeamWinner = ifelse(slugTeamWinner == slugTeam, TRUE, FALSE)) %>%
        suppressMessages() %>%
        dplyr::select(typeSeason, slugSeason, dateGame, idGame, numberGameDay,
                      slugTeam, isTeamWinner, everything())
    }

    data

  }

#' NBA seasons schedules
#'
#' @param seasons vector of seasons where season is year ending
#' @param season_types type of season
#' @param return_message if `TRUE`
#' @param parse_boxscores if `TRUE` parses box scores
#' @param box_score_tables vector of box score table names
#' @param nest_data if `TRUE` nests the data
#'
#' @return a \code{data_frame()}
#' @family schedule
#' @export
#' @import dplyr jsonlite purrr stringr lubridate magrittr tidyr tibble httr
#' @importFrom  glue glue
#' @examples
#' get_seasons_schedule(seasons = c(2012, 2018))
get_seasons_schedule <-
  function(seasons = 2018,
           season_types = "Regular Season",
           parse_boxscores = F,
           box_score_tables = c(
             "Traditional"
           ),
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
          get_season_schedule_safe(
            season = season,
            season_type = season_type,
            parse_boxscores = parse_boxscores,
            box_score_tables = box_score_tables,
            return_message = return_message
          )
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
      select(isActive, slugTeam, idTeam, everything()) %>%
      arrange(slugTeam) %>%
      suppressWarnings()
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
      mutate(
        countYearsExperience = ifelse(is.na(countYearsExperience), 0, countYearsExperience),
        dateBirth = dateBirth %>% lubridate::mdy()
      ) %>%
      tidyr::separate(heightInches,
                      sep = "\\-",
                      into = c("feet", "inches")) %>%
      mutate_at(c("feet", "inches"),
                funs(. %>% as.numeric())) %>%
      mutate(heightInches = (12 * feet) + inches) %>%
      select(-one_of(c("feet", "inches"))) %>%
      select(slugSeason, idTeam:weightLBS, heightInches, everything())

    data
  }


get_season_roster <-
  function(season = 2012,
           return_message = TRUE) {
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
        team_id <-  df_teams$idTeam[x]
        get_team_season_roster_safe(season = season, team_id = team_id)
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


#' NBA teams seasons rosters
#'
#' Rosters for each team of specified seasons
#'
#' @param seasons vector of seasons
#' @param return_message if \code{TRUE} returns a message
#' @param nest_data if \code{TRUE} nests data
#'
#' @return a `date_frame`
#' @family teams
#' @family rosters
#' @export
#' @import dplyr jsonlite purrr stringr lubridate magrittr tidyr tibble httr
#' @importFrom glue glue
#' @examples
#' library(nbastatR)
#' library(dplyr)
#' df_rosters <- get_seasons_rosters(2015:2018)
#'
#' ### Mean Age by Season and Team
#' df_rosters %>%
#' group_by(slugSeason, slugTeam) %>%
#' summarise(ageMean = mean(agePlayer)) %>%
#' arrange(ageMean) %>%
#' ungroup()

get_seasons_rosters <-
  function(seasons = NULL,
           return_message = TRUE,
           nest_data = F) {
    if (seasons %>% purrr::is_null()) {
      stop("Enter seasons")
    }
    get_season_roster_safe <-
      purrr::possibly(get_season_roster, data_frame())

    all_data <-
      seasons %>%
      map_df(function(season) {
        get_season_roster_safe(season = season, return_message = return_message)
      })

    assign_nba_players()

    all_data <-
      all_data %>%
      left_join(df_nba_player_dict %>% select(idPlayer, matches("url"))) %>%
      suppressMessages()

    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-c(slugSeason), .key = "dataRosters")
    }
    all_data
  }
