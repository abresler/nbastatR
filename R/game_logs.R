


# full_logs ---------------------------------------------------------------
.get_season_gamelog <-
  function(season = 1984,
           league = "NBA",
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
      glue::glue(
        "Acquiring {league} basic {result_type} game logs for the {season_slug} {season_type}"
      ) %>% cat(fill = T)
    }

    call_slug <-
      case_when(league %>% str_to_upper() == "WNBA" ~ "wnbaseasonstats",
                TRUE ~ "leaguegamelog")

    league_slug <-
      case_when(
        league %>% str_to_upper() == "WNBA" ~ "10",
        league %>% str_to_upper() == "GLEAGUE" ~ "20",
        TRUE ~ "00"
      )


    season_name_slug <- URLencode(season_type)

    if (league %>% str_to_upper() == "WNBA") {
      url <-
        glue::glue(
          "https://stats.nba.com/stats/wnbaseasonstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=10&Location=&MeasureType=Base&Month=0&OpponentTeamID=&Outcome=&PORound=&PaceAdjust=&PerMode=PerGame&Period=&PlayerExperience=&PlayerPosition=&PlusMinus=&Rank=&Season={season}&SeasonSegment=&SeasonType={URLencode(season_type)}&ShotClockRange=&StarterBench=&StatCategory=PTS&TeamID=0&VsConference=&VsDivision=&Weight="
        ) %>%
        URLencode() %>%
        as.character()

    } else {
      url <-
        glue::glue(
          "https://stats.nba.com/stats/leaguegamelog?Counter=1000&Season={season_slug}&Direction=DESC&LeagueID={league_slug}&PlayerOrTeam={table_slug}&SeasonType={season_name_slug}&Sorter=DATE"
        ) %>% as.character()

    }
    resp <-
      url %>%
      curl() %>%
      readr::read_lines()

    json <-
      resp %>% jsonlite::fromJSON(simplifyVector = T)


      data <-
        json %>%
        nba_json_to_df() %>%
        mutate(slugSeason = season_slug,
               typeSeason = season_type,
               typeResult = result_type)

    if (data %>% tibble::has_name("dateGame")) {
      data <-
        data %>%
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
        ) %>%
        mutate(isWin = slugTeamWinner == slugTeam) %>%
        select(typeSeason:idTeam, isWin, everything())
    }

      data <-
        data %>%
        clean_data_table_name() %>%
        mutate(yearSeason = season,
               typeResult = result_type) %>%
        mutate(urlTeamSeasonLogo = generate_team_season_logo(season = yearSeason, slug_team = slugTeam))

    if (data %>% tibble::has_name("dateGame")) {
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
          isB2BFirst = ifelse(lead(countDaysNextGameTeam) == 0, TRUE, FALSE),
          isB2BSecond = ifelse(lag(countDaysNextGameTeam) == 0, TRUE, FALSE)
        ) %>%
        ungroup() %>%
        mutate_if(is.logical,
                  funs(ifelse(. %>% is.na(), FALSE, .)))

      data <-
        data %>%
        left_join(df_teams_games) %>%
        select(one_of(
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
    }


    if (result_type == "player" && league %>% str_to_upper() == "NBA") {
      if (!'df_nba_player_dict' %>% exists()) {
        df_nba_player_dict <-
          nba_players()

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
        select(
          typeResult:namePlayer,
          numberGamePlayerSeason,
          countDaysRestPlayer,
          countDaysNextGamePlayer,
          everything()
        )

    }



    data <-
      data %>%
      mutate(slugLeague = league) %>%
      select(typeResult, typeSeason, yearSeason, everything()) %>%
      nest(-c(slugLeague, typeResult, slugSeason, yearSeason), .key = dataTables)



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
#' @param league league name defaults to WNBA
#' @param ...
#' @param return_message
#'
#' @return a `tibble`
#' @family game
#' @export
#' @import dplyr jsonlite purrr stringr lubridate magrittr tidyr tibble httr
#' @importFrom  glue glue
#' @examples
#' game_logs(seasons = 2019, result_types = c("team", "player"))
game_logs <-
  function(seasons = 2019,
           league = "NBA",
           result_types  = "player",
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
      as_tibble() %>%
      arrange(season)

    .get_season_gamelog_safe <-
      purrr::possibly(.get_season_gamelog, tibble())

    all_data <-
      1:nrow(input_df) %>%
      map_df(function(x) {
        df_row <-
          input_df %>% slice(x)

        data_row <-
          df_row %$%
          .get_season_gamelog(
            season = season,
            result_type = result,
            season_type = season_type,
            return_message = return_message,
            league = league
          )
        data_row
      })

    if (result_length == 1 && all_data %>% tibble::has_name("typeResult")) {
      all_data <-
        all_data %>%
        select(-typeResult) %>%
        unnest_legacy()

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
            unnest_legacy()

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
            select(one_of(col_order), everything()) %>%
            suppressWarnings()



          if (nest_data) {
            df_table <-
              df_table %>%
              nest(-c(typeSeason, slugSeason, yearSeason), .key = dataGameLogs)
          }



          table_name <-
            glue::glue("dataGameLogs{str_to_title(result)}") %>% as.character()

          assign(x = table_name, df_table, envir = .GlobalEnv)

        })
    }
    all_data
  }


# schedule ----------------------------------------------------------------

.get_season_schedule <-
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
    .get_season_gamelog_safe <- possibly(.get_season_gamelog, tibble())
    data <-
      .get_season_gamelog(
        season = season,
        result_type = "team",
        season_type = season_type,
        return_message = return_message
      ) %>%
      unnest_legacy()
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
            unnest_legacy() %>%
            nest(-c(idGame, slugTeam), .key = dataBoxScorePlayer)
        ) %>%
        left_join(
          df_box_scores %>%
            filter(typeResult == "team") %>%
            select(dataBoxScore) %>%
            unnest_legacy() %>%
            nest(-c(idGame, slugTeam), .key = dataBoxScoreTeam)
        ) %>%
        mutate(isTeamWinner = ifelse(slugTeamWinner == slugTeam, TRUE, FALSE)) %>%
        suppressMessages() %>%
        select(typeSeason, slugSeason, dateGame, idGame, numberGameDay,
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
#' @return a \code{tibble()}
#' @family schedule
#' @export
#' @import dplyr jsonlite purrr stringr lubridate magrittr tidyr tibble httr
#' @importFrom  glue glue
#' @examples
#' seasons_schedule(seasons = c(2012, 2018))
seasons_schedule <-
  function(seasons = 2019,
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
      as_tibble() %>%
      arrange(season)

    .get_season_schedule_safe <-
      purrr::possibly(.get_season_schedule, tibble())

    all_data <-
      1:nrow(input_df) %>%
      future_map_dfr(function(x) {
        df_row <-
          input_df %>% slice(x)
        data_row <-
          df_row %$%
          .get_season_schedule_safe(
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
        nest(-c(typeSeason, slugSeason), .key = dataSchedule)
    }
    all_data
  }
