# http://nbasense.com/nba-api/Stats/Stats/Charts/InfographicFanDuelPlayer#request-example

assign_tables_modes <-
  function(all_data, stat_type = "Player", add_mode_names = TRUE) {
    tables <-
      all_data$nameTable %>% unique()

    dict_tables <- dictionary_table_names()

    tables %>%
      walk(function(table){
        table %>% message()
        df_table_names <-
          dict_tables %>%
          filter(nameTable == table)

        df_tables <-
          all_data %>%
          filter(nameTable == table) %>%
          select(-nameTable)

        modes <-
          df_tables$modeSearch %>% unique()

        modes %>%
          walk(function(mode){
            df_table <-
              df_tables %>% filter(modeSearch == mode) %>%
              tidyr::unnest()

            slug_season <-
              df_table_names$slugTable

            period <-
              df_table_names$periodTable

            if (slug_season %>% length() > 0) {
            df_table <-
              df_table %>%
              mutate(slugSeasonType = slug_season) %>%
              select(slugSeasonType, everything())
            }

            has_measure <-  df_table %>% tibble::has_name("typeMeasure")

            if (has_measure) {
              measures <-
                df_table$typeMeasure %>% unique()

              table_map <-
                measures %>%
                map(function(measure){
                  df <-
                    df_table %>% filter(typeMeasure == measure) %>%
                    dplyr::select(which(colMeans(is.na(.)) < 1))

                  if (measure == "Advanced") {
                    df <-
                      df %>% dplyr::rename(minutesTotal = minutes)
                    if (df %>% tibble::has_name("fgm")) {
                      df <-
                        df %>%
                        dplyr::rename(fgmTotal = fgm)
                    }

                    if (df %>% tibble::has_name("fga")) {
                      df <-
                        df %>%
                        dplyr::rename(fgaTotal = fga)
                    }
                  }


                  if (measure == "Opponent") {
                    df <- df %>% dplyr::rename(minutesOpp = minutes,
                                               plusminusOpp = plusminus)
                  }


                  remove_rank <- names(df) %>% str_detect("Rank") %>% sum(na.rm = T) > 0 & (!df %>% tibble::has_name("isRank") & !table %>% str_detect("LeagueDash"))

                  if (remove_rank){
                    df <- df %>% select(-matches("Rank"))
                  }
                  df <-
                    df %>% select(-one_of(c("modeSearch", "typeMeasure")))

                  if (add_mode_names) {

                    df_table_names <-
                      df %>% select_if(is.numeric) %>% names()
                    ignore <-
                      c("age", "gp", "gs", "id[A-Z]", "number[A-Z]", "pct[A-Z]", "ortg", "netrtg", "drtg", "ratio[A-Z]",
                        "countLastNGames", "wins", "losses", "minutesTotal",
                        mode) %>% str_c(collapse = "|")

                    names(df)[names(df) %in% df_table_names[!df_table_names %>% str_detect(ignore)]] <-
                      df_table_names[!df_table_names %>% str_detect(ignore)] %>%
                      str_c(., mode)

                  }

                  df %>%
                    dplyr::select(one_of(c("slugSeason", "slugTeam", "idTeam",
                                           "isRookie", "numberPlayerSeason", "namePlayer", "idPlayer")), everything()) %>%
                    suppressWarnings()
                })

              data <-
                table_map %>%
                purrr::reduce(left_join) %>%
                suppressWarnings() %>%
                suppressMessages()

              if (slug_season %>% length() == 0) {
                slug_season <- ""
              }

              if (period %>% length() == 0) {
                period <- ""
              }
              table_slug <- table %>% str_replace_all("Dashboard", "")
              table_name <-
                glue::glue("data{table_slug}{mode}") %>%
                as.character()

              assign(table_name, eval(data), envir = .GlobalEnv)
            } else {

            if (period %>% str_to_lower() %>% str_detect("career")) {
              period <- "Career"
            }

            table_name <-
              glue::glue("data{str_to_title(stat_type)}{table}")

            if (add_mode_names) {
              df_table <-
                df_table %>%
                select(-modeSearch)

              df_table_names <-
                df_table %>% select_if(is.numeric) %>% names()
              ignore <-
                c("age", "gp", "gs", "id[A-Z]", "number[A-Z]", "pct[A-Z]", "ortg", "netrtg", "drtg", "ratio[A-Z]",
                  "countLastNGames", "wins", "losses",
                  mode) %>% str_c(collapse = "|")

              names(df_table)[names(df_table) %in% df_table_names[!df_table_names %>% str_detect(ignore)]] <-
                df_table_names[!df_table_names %>% str_detect(ignore)] %>%
                str_c(., mode)

            }

            df_table <-
              df_table %>%
              dplyr::select(one_of(c("slugSeason", "slugTeam", "idTeam",
                                     "isRookie", "numberPlayerSeason", "namePlayer", "idPlayer")), everything()) %>%
              suppressWarnings()

            assign(table_name, eval(df_table), envir = .GlobalEnv)
            }

          })
      })
  }

dictionary_table_names <-
  function() {
    data_frame(nameTable = c("CareerTotalsAllStarSeason", "CareerTotalsCollegeSeason", "CareerTotalsPostSeason",
                             "CareerTotalsRegularSeason", "SeasonRankingsPostSeason", "SeasonRankingsRegularSeason",
                             "SeasonTotalsAllStarSeason", "SeasonTotalsCollegeSeason", "SeasonTotalsPostSeason",
                             "SeasonTotalsRegularSeason",
                             "OverallTeamDashboard", "LocationTeamDashboard", "WinsLossesTeamDashboard",
                             "MonthTeamDashboard", "PrePostAllStarTeamDashboard", "DaysRestTeamDashboard"
                             ),
               periodTable = c("Career", "Career", "Career",
                               "Career", "BySeason", "BySeason",
                               "BySeason", "BySeason", "BySeason",
                               "BySeason",
                               "", "", "",
                               "", "", ""
                               ),
               typeTable = c("AllStar", "College", "Playoffs",
                             "Season", "Playoffs", "Season",
                             "AllStar", "College", "PlayOffs",
                             "Season",
                             "Overall", "ByLocation", "ByWinLoss",
                             "ByMonth", "ByPrePostASG", "ByRest"
               ),
               slugTable  = c("ASG", "COL", "PO",
                              "RS", "PO", "RS",
                              "ASG", "COL", "PO",
                              "RS",
                              "Overall", "ByLocation", "ByWinLoss",
                              "ByMonth", "ByPrePostASG", "ByRest"
               ))
  }

get_nba_headers <- function() {
  nba_hdrs <- httr::add_headers(
    Connection = 'keep-alive',
    'Cache-Control' = 'max-age=0',
    Accept = 'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8',
    'User-Agent' = 'Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/48.0.2564.82 Safari/537.36',
    'Upgrade-Insecure-Requests' = '1',
    'Accept-Language' = 'en-US,en;q=0.8',
    'Accept-Encoding' = 'gzip, deflate, sdch'
  )

  nba_hdrs

}

#' Get NBA Player Ids
#'
#' @param players vector of player names
#' @param player_ids vector of player ids
#'
#' @return
#' @export
#' @import dplyr stringr jsonlite readr purrr tibble tidyr curl
#' @examples
#' get_nba_players_ids(players = c("Mitch Richmond", "Kyle Kuzma"))
get_nba_players_ids <-
  function(players = NULL, player_ids = NULL) {

    if (player_ids %>% purrr::is_null() && players %>% purrr::is_null()) {
      stop("Please enter players of player ids")
    }

    ids <- c()

    if (!player_ids %>% purrr::is_null()) {
      ids <-
        ids %>%
        append(player_ids)
    }

    if (!'df_nba_player_dict' %>% exists()) {
      df_nba_player_dict <-
        get_nba_players()

      assign(x = 'df_nba_player_dict', df_nba_player_dict, envir = .GlobalEnv)
    }

    if (!players %>% purrr::is_null() ) {
    player_search <-
      players %>% str_c(collapse = "|")

    search_ids <-
      df_nba_player_dict %>%
      filter(namePlayer %>% str_detect(player_search)) %>%
      pull(idPlayer) %>%
      unique()

    ids <-
      ids %>%
      append(search_ids)

    }
    ids %>% unique() %>% sort()
  }

#' Get NBA Team IDs
#'
#' @param teams vector of team names
#' @param team_ids vector of team ids
#'
#' @return
#' @export
#'
#' @examples
#' get_nba_teams_ids(teams = c("Brooklyn Nets", "Denver Nuggets"))
get_nba_teams_ids <-
  function(teams = NULL, team_ids = NULL) {

    if (team_ids %>% purrr::is_null() && teams %>% purrr::is_null()) {
      stop("Please enter teams or team_ids ids")
    }

    ids <- c()

    if (!team_ids %>% purrr::is_null()) {
      ids <-
        ids %>%
        append(team_ids)
    }

    if (!'df_nba_team_dict' %>% exists()) {
      df_nba_team_dict <- get_nba_teams()

      assign('df_nba_team_dict', df_nba_team_dict, envir = .GlobalEnv)
    }

    if (!teams %>% purrr::is_null() ) {
      teams_search <-
        teams %>% str_c(collapse = "|")

      search_ids <-
        df_nba_team_dict %>%
        filter(!teamName %>% is.na()) %>%
        filter(nameTeam %>% str_detect(teams_search)) %>%
        pull(idTeam) %>%
        unique()

      ids <-
        ids %>%
        append(search_ids)

    }
    ids %>% unique() %>% sort()
  }


gen_url <- function(endpoint) {
  path <- paste('stats/', endpoint, sep = '')
  url <- httr::modify_url("http://stats.nba.com/stats/", path = path)
  url
}



clean_data_table_name <-
  function(data) {
    if (!data %>% hasName("typeResult")){
      return(data)
    }
    type <- data %>% pull(typeResult) %>% unique()

    if (type == "player") {
      data <-
        data %>%
        dplyr::select(-typeResult)
      return(data)
    }

    data <-
      data %>%
      dplyr::select(-typeResult)

    num_names <-
      data %>% select_if(is.numeric) %>% select(-matches("id")) %>% names()

    num_new <-
      glue::glue("{num_names}Team") %>% as.character()

    names(data)[names(data) %in% num_names] <-
      num_new

    data

  }

get_missing_names <-
  function(json_names) {
    df_names <- dictionary_nba_names()

    json_names[!json_names %in% df_names$nameNBA] %>% emacs()
  }
generate_season_slug <-
  function(season = 2018) {
    season_start <- season - 1
    season_end_slug <- season %>% substr(3,4)

    glue::glue("{season_start}-{season_end_slug}") %>%
      as.character()

  }

pad_id <-
  function(id = 21601112) {
    zeros <- 10 - nchar(id)
    start <- rep("0", zeros) %>% str_c(collapse = "")
    glue::glue("{start}{id}") %>% as.character()
  }

#' Dictioanry of NBA Headers and nbastatR usd name
#'
#' @return
#' @export
#' @import dplyr
#' @examples
dictionary_nba_names <-
  function() {
    data_frame(nameNBA =
                 c("PERSON_ID", "DISPLAY_LAST_COMMA_FIRST", "DISPLAY_FIRST_LAST",
                   "ROSTERSTATUS", "FROM_YEAR", "TO_YEAR", "PLAYERCODE", "TEAM_ID",
                   "TEAM_CITY", "TEAM_NAME", "TEAM_ABBREVIATION", "TEAM_CODE", "GAMES_PLAYED_FLAG",
                   "SEASON_ID", "PLAYER_ID", "PLAYER_NAME", "GAME_ID", "GAME_DATE",
                   "MATCHUP", "WL", "MIN", "FGM", "FGA", "FG_PCT", "FG3M", "FG3A",
                   "FG3_PCT", "FTM", "FTA", "FT_PCT", "OREB", "DREB", "REB", "AST",
                   "STL", "BLK", "TOV", "PF", "PTS", "PLUS_MINUS", "VIDEO_AVAILABLE",
                   "START_POSITION", "COMMENT", "USG_PCT", "PCT_FGM", "PCT_FGA",
                   "PCT_FG3M", "PCT_FG3A", "PCT_FTM", "PCT_FTA", "PCT_OREB", "PCT_DREB",
                   "PCT_REB", "PCT_AST", "PCT_TOV", "PCT_STL", "PCT_BLK", "PCT_BLKA",
                   "PCT_PF", "PCT_PFD", "PCT_PTS", "USG_PCT", "PCT_FGM", "PCT_FGA",
                   "PCT_FG3M", "PCT_FG3A", "PCT_FTM", "PCT_FTA", "PCT_OREB", "PCT_DREB",
                   "PCT_REB", "PCT_AST", "PCT_TOV", "PCT_STL", "PCT_BLK", "PCT_BLKA",
                   "PCT_PF", "PCT_PFD", "PCT_PTS","EVENTNUM", "EVENTMSGTYPE", "EVENTMSGACTIONTYPE", "PERIOD",
                   "WCTIMESTRING", "PCTIMESTRING", "HOMEDESCRIPTION", "NEUTRALDESCRIPTION",
                   "VISITORDESCRIPTION", "SCORE", "SCOREMARGIN", "PERSON1TYPE",
                   "PLAYER1_ID", "PLAYER1_NAME", "PLAYER1_TEAM_ID", "PLAYER1_TEAM_CITY",
                   "PLAYER1_TEAM_NICKNAME", "PLAYER1_TEAM_ABBREVIATION", "PERSON2TYPE",
                   "PLAYER2_ID", "PLAYER2_NAME", "PLAYER2_TEAM_ID", "PLAYER2_TEAM_CITY",
                   "PLAYER2_TEAM_NICKNAME", "PLAYER2_TEAM_ABBREVIATION", "PERSON3TYPE",
                   "PLAYER3_ID", "PLAYER3_NAME", "PLAYER3_TEAM_ID", "PLAYER3_TEAM_CITY",
                   "PLAYER3_TEAM_NICKNAME", "PLAYER3_TEAM_ABBREVIATION", "VIDEO_AVAILABLE_FLAG",
                   "HOME_TEAM_ID", "HOME_TEAM_ABR", "HOME_TEAM_PTS", "VISITOR_TEAM_ID",
                   "VISITOR_TEAM_ABR", "VISITOR_TEAM_PTS",
                   "JERSEY_NUM", "PLAYER_POSITION", "LOCATION", "FAN_DUEL_PTS",
                   "NBA_FANTASY_PTS", "BLKA", "PFD",
                   "GAME_DATE_EST", "GAME_SEQUENCE", "GAME_STATUS_ID", "GAME_STATUS_TEXT",
                   "GAMECODE", "SEASON", "LIVE_PERIOD", "LIVE_PC_TIME", "NATL_TV_BROADCASTER_ABBREVIATION",
                   "LIVE_PERIOD_TIME_BCAST", "WH_STATUS",
                   "LEAGUE_ID", "PTS_PAINT", "PTS_2ND_CHANCE", "PTS_FB", "LARGEST_LEAD",
                   "LEAD_CHANGES", "TIMES_TIED", "TEAM_TURNOVERS", "TOTAL_TURNOVERS",
                   "TEAM_REBOUNDS", "PTS_OFF_TO", "OFFICIAL_ID", "FIRST_NAME", "LAST_NAME",
                   "ATTENDANCE", "GAME_TIME",
                   "TEAM_CITY_NAME", "TEAM_NICKNAME", "TEAM_WINS_LOSSES", "PTS_QTR1",
                   "PTS_QTR2", "PTS_QTR3", "PTS_QTR4", "PTS_OT1", "PTS_OT2", "PTS_OT3",
                   "PTS_OT4", "PTS_OT5", "PTS_OT6", "PTS_OT7", "PTS_OT8", "PTS_OT9",
                   "PTS_OT10",
                   "LAST_GAME_ID", "LAST_GAME_DATE_EST", "LAST_GAME_HOME_TEAM_ID",
                   "LAST_GAME_HOME_TEAM_CITY", "LAST_GAME_HOME_TEAM_NAME", "LAST_GAME_HOME_TEAM_ABBREVIATION",
                   "LAST_GAME_HOME_TEAM_POINTS", "LAST_GAME_VISITOR_TEAM_ID", "LAST_GAME_VISITOR_TEAM_CITY",
                   "LAST_GAME_VISITOR_TEAM_NAME", "LAST_GAME_VISITOR_TEAM_CITY1",
                   "LAST_GAME_VISITOR_TEAM_POINTS",
                   "HOME_TEAM_WINS", "HOME_TEAM_LOSSES", "SERIES_LEADER",
                   "PT_AVAILABLE", "PT_XYZ_AVAILABLE", "HUSTLE_STATUS", "HISTORICAL_STATUS",
                   "TO", "SPD", "DIST", "ORBC", "DRBC", "RBC", "TCHS", "SAST", "FTAST",
                   "PASS", "CFGM", "CFGA", "CFG_PCT", "UFGM", "UFGA", "UFG_PCT",
                   "DFGM", "DFGA", "DFG_PCT",
                   "PCT_FGA_2PT", "PCT_FGA_3PT", "PCT_PTS_2PT", "PCT_PTS_2PT_MR",
                   "PCT_PTS_3PT", "PCT_PTS_FB", "PCT_PTS_FT", "PCT_PTS_OFF_TOV",
                   "PCT_PTS_PAINT", "PCT_AST_2PM", "PCT_UAST_2PM", "PCT_AST_3PM",
                   "PCT_UAST_3PM", "PCT_AST_FGM", "PCT_UAST_FGM",
                   "PTS_OFF_TOV", "OPP_PTS_OFF_TOV", "OPP_PTS_2ND_CHANCE", "OPP_PTS_FB",
                   "OPP_PTS_PAINT",
                   "MINUTES", "CONTESTED_SHOTS", "CONTESTED_SHOTS_2PT", "CONTESTED_SHOTS_3PT",
                   "DEFLECTIONS", "LOOSE_BALLS_RECOVERED", "CHARGES_DRAWN", "SCREEN_ASSISTS",
                   "EFG_PCT", "FTA_RATE", "TM_TOV_PCT", "OREB_PCT", "OPP_EFG_PCT",
                   "OPP_FTA_RATE", "OPP_TOV_PCT", "OPP_OREB_PCT",
                   "OFF_RATING", "DEF_RATING", "NET_RATING", "AST_PCT", "AST_TOV",
                   "AST_RATIO", "DREB_PCT", "REB_PCT", "TS_PCT", "PACE", "PIE",
                   "GRID_TYPE", "GAME_EVENT_ID", "MINUTES_REMAINING", "SECONDS_REMAINING",
                   "EVENT_TYPE", "ACTION_TYPE", "SHOT_TYPE", "SHOT_ZONE_BASIC",
                   "SHOT_ZONE_AREA", "SHOT_ZONE_RANGE", "SHOT_DISTANCE", "LOC_X",
                   "LOC_Y", "SHOT_ATTEMPTED_FLAG", "SHOT_MADE_FLAG", "HTM", "VTM",
                   "TeamID", "LeagueID", "PLAYER", "NUM", "POSITION", "HEIGHT",
                   "WEIGHT", "BIRTH_DATE", "AGE", "EXP", "SCHOOL",
                   "MIN_YEAR", "MAX_YEAR", "ABBREVIATION", "PLAYER_AGE", "GP", "GS",
                   "Team_ID", "ORGANIZATION_ID", "SCHOOL_NAME",
                   "RANK_MIN", "RANK_FGM", "RANK_FGA", "RANK_FG_PCT", "RANK_FG3M",
                   "RANK_FG3A", "RANK_FG3_PCT", "RANK_FTM", "RANK_FTA", "RANK_FT_PCT",
                   "RANK_OREB", "RANK_DREB", "RANK_REB", "RANK_AST", "RANK_STL",
                   "RANK_BLK", "RANK_TOV", "RANK_PTS", "RANK_EFF",
                   "RANK_PG_MIN", "RANK_PG_FGM", "RANK_PG_FGA", "RANK_PG_FG3M",
                   "RANK_PG_FG3A", "RANK_PG_FTM", "RANK_PG_FTA", "RANK_PG_OREB",
                   "RANK_PG_DREB", "RANK_PG_REB", "RANK_PG_AST", "RANK_PG_STL",
                   "RANK_PG_BLK", "RANK_PG_TOV", "RANK_PG_PTS", "RANK_PG_EFF",
                   "RANK_PMIN_MIN", "RANK_PMIN_FGM", "RANK_PMIN_FG3M", "RANK_PMIN_FG3A",
                   "RANK_PMIN_FTM", "RANK_PMIN_FTA", "RANK_PMIN_OREB", "RANK_PMIN_DREB",
                   "RANK_PMIN_REB", "RANK_PMIN_AST", "RANK_PMIN_STL", "RANK_PMIN_BLK",
                   "RANK_PMIN_TOV", "RANK_PMIN_PTS", "RANK_PMIN_EFF",
                   "PLAYER_NAME", "SEASON", "ROUND_NUMBER", "ROUND_PICK", "OVERALL_PICK",
                   "ORGANIZATION", "ORGANIZATION_TYPE", "GROUP_SET", "GROUP_VALUE", "SEASON_YEAR", "W", "L", "W_PCT",
                   "GP_RANK", "W_RANK", "L_RANK", "W_PCT_RANK", "MIN_RANK", "FGM_RANK",
                   "FGA_RANK", "FG_PCT_RANK", "FG3M_RANK", "FG3A_RANK", "FG3_PCT_RANK",
                   "FTM_RANK", "FTA_RANK", "FT_PCT_RANK", "OREB_RANK", "DREB_RANK",
                   "REB_RANK", "AST_RANK", "TOV_RANK", "STL_RANK", "BLK_RANK", "BLKA_RANK",
                   "PF_RANK", "PFD_RANK", "PTS_RANK", "PLUS_MINUS_RANK", "CFID",
                   "CFPARAMS", "MeasureType", "PerMode", "PlusMinus", "PaceAdjust", "Rank",
                   "LeagueID", "Season", "SeasonType", "PORound", "TeamID", "Month",
                   "OpponentTeamID", "Period", "LastNGames", "TEAM_GAME_LOCATION", "GAME_RESULT", "SEASON_MONTH_NAME", "SEASON_SEGMENT", "TEAM_DAYS_REST_RANGE", "PlayerID",
                   "Outcome", "Location", "SeasonSegment", "DateFrom", "DateTo",
                   "VsConference", "VsDivision", "GameSegment", "ShotClockRange",
                   "AST_TO", "OFF_RATING_RANK", "DEF_RATING_RANK", "NET_RATING_RANK",
                   "AST_PCT_RANK", "AST_TO_RANK", "AST_RATIO_RANK", "OREB_PCT_RANK",
                   "DREB_PCT_RANK", "REB_PCT_RANK", "TM_TOV_PCT_RANK", "EFG_PCT_RANK",
                   "TS_PCT_RANK", "PACE_RANK", "PIE_RANK",
                   "PTS_OFF_TOV_RANK", "PTS_2ND_CHANCE_RANK", "PTS_FB_RANK", "PTS_PAINT_RANK",
                   "OPP_PTS_OFF_TOV_RANK", "OPP_PTS_2ND_CHANCE_RANK", "OPP_PTS_FB_RANK",
                   "OPP_PTS_PAINT_RANK", "PCT_FGA_2PT_RANK", "PCT_FGA_3PT_RANK", "PCT_PTS_2PT_RANK",
                   "PCT_PTS_2PT_MR_RANK", "PCT_PTS_3PT_RANK", "PCT_PTS_FB_RANK",
                   "PCT_PTS_FT_RANK", "PCT_PTS_OFF_TOV_RANK", "PCT_PTS_PAINT_RANK",
                   "PCT_AST_2PM_RANK", "PCT_UAST_2PM_RANK", "PCT_AST_3PM_RANK",
                   "PCT_UAST_3PM_RANK", "PCT_AST_FGM_RANK", "PCT_UAST_FGM_RANK",
                   "FTA_RATE_RANK", "OPP_EFG_PCT_RANK", "OPP_FTA_RATE_RANK", "OPP_TOV_PCT_RANK",
                   "OPP_OREB_PCT_RANK",
                   "OPP_FGM", "OPP_FGA", "OPP_FG_PCT", "OPP_FG3M", "OPP_FG3A",
                   "OPP_FG3_PCT", "OPP_FTM", "OPP_FTA", "OPP_FT_PCT", "OPP_OREB",
                   "OPP_DREB", "OPP_REB", "OPP_AST", "OPP_TOV", "OPP_STL", "OPP_BLK",
                   "OPP_BLKA", "OPP_PF", "OPP_PFD", "OPP_PTS", "OPP_FGM_RANK", "OPP_FGA_RANK",
                   "OPP_FG_PCT_RANK", "OPP_FG3M_RANK", "OPP_FG3A_RANK", "OPP_FG3_PCT_RANK",
                   "OPP_FTM_RANK", "OPP_FTA_RANK", "OPP_FT_PCT_RANK", "OPP_OREB_RANK",
                   "OPP_DREB_RANK", "OPP_REB_RANK", "OPP_AST_RANK", "OPP_TOV_RANK",
                   "OPP_STL_RANK", "OPP_BLK_RANK", "OPP_BLKA_RANK", "OPP_PF_RANK",
                   "OPP_PFD_RANK", "OPP_PTS_RANK",
                   'FGM_PG', 'FGA_PG',  'FGA_PG_RANK',  'FGM_PG_RANK', 'USG_PCT_RANK',
                   "DD2", "TD3", "NBA_FANTASY_PTS_RANK", "DD2_RANK",  "TD3_RANK",
                   "PCT_FGM_RANK", "PCT_FGA_RANK", "PCT_FG3M_RANK", "PCT_FG3A_RANK",
                   "PCT_FTM_RANK", "PCT_FTA_RANK", "PCT_OREB_RANK", "PCT_DREB_RANK",
                   "PCT_REB_RANK", "PCT_AST_RANK", "PCT_TOV_RANK", "PCT_STL_RANK",
                   "PCT_BLK_RANK", "PCT_BLKA_RANK", "PCT_PF_RANK", "PCT_PFD_RANK",
                   "PCT_PTS_RANK", "DISPLAY_FI_LAST", "BIRTHDATE", "COUNTRY", "LAST_AFFILIATION",
                   "SEASON_EXP", "JERSEY", "DLEAGUE_FLAG", "DRAFT_YEAR", "DRAFT_ROUND",
                   "DRAFT_NUMBER", "TimeFrame", "ALL_STAR_APPEARANCES",
                   "EFF", "TPP", "ORPG", "DRPG", "TRPG", "APG", "TPG",
                   "SPG", "BPG", "PFPG", "PPG", "OPPG", "FGP", "FTP",
                   "HEIGHT_WO_SHOES", "HEIGHT_WO_SHOES_FT_IN", "HEIGHT_W_SHOES",
                   "HEIGHT_W_SHOES_FT_IN", "WINGSPAN", "WINGSPAN_FT_IN", "STANDING_REACH",
                   "STANDING_REACH_FT_IN", "BODY_FAT_PCT", "HAND_LENGTH", "HAND_WIDTH",
                   "STANDING_VERTICAL_LEAP", "MAX_VERTICAL_LEAP", "LANE_AGILITY_TIME",
                   "MODIFIED_LANE_AGILITY_TIME", "THREE_QUARTER_SPRINT", "BENCH_PRESS",
                   "SPOT_FIFTEEN_CORNER_LEFT", "SPOT_FIFTEEN_BREAK_LEFT", "SPOT_FIFTEEN_TOP_KEY",
                   "SPOT_FIFTEEN_BREAK_RIGHT", "SPOT_FIFTEEN_CORNER_RIGHT", "SPOT_COLLEGE_CORNER_LEFT",
                   "SPOT_COLLEGE_BREAK_LEFT", "SPOT_COLLEGE_TOP_KEY", "SPOT_COLLEGE_BREAK_RIGHT",
                   "SPOT_COLLEGE_CORNER_RIGHT", "SPOT_NBA_CORNER_LEFT", "SPOT_NBA_BREAK_LEFT",
                   "SPOT_NBA_TOP_KEY", "SPOT_NBA_BREAK_RIGHT", "SPOT_NBA_CORNER_RIGHT",
                   "OFF_DRIB_FIFTEEN_BREAK_LEFT", "OFF_DRIB_FIFTEEN_TOP_KEY", "OFF_DRIB_FIFTEEN_BREAK_RIGHT",
                   "OFF_DRIB_COLLEGE_BREAK_LEFT", "OFF_DRIB_COLLEGE_TOP_KEY", "OFF_DRIB_COLLEGE_BREAK_RIGHT",
                   "ON_MOVE_FIFTEEN", "ON_MOVE_COLLEGE", "ContextFilter", "ContextMeasure",
                   "TEAM", "RANK", "Scope", "StatCategory", "STL_TOV",
                   "SEASON_TYPE", "ACTIVE_WITH_TEAM", "F_RANK_GP", "F_RANK_MINUTES",
                   "F_RANK_FGM", "F_RANK_FGA", "F_RANK_FG_PCT", "F_RANK_FG3M", "F_RANK_FG3A",
                   "F_RANK_FG3_PCT", "F_RANK_FTM", "F_RANK_FTA", "F_RANK_FT_PCT",
                   "F_RANK_OREB", "F_RANK_DREB", "F_RANK_REB", "F_RANK_AST", "F_RANK_PF",
                   "F_RANK_STL", "F_RANK_TOV", "F_RANK_BLK", "F_RANK_PTS",
                   "EVENT_NUM", "HOME_PCT", "VISITOR_PCT", "HOME_PTS", "VISITOR_PTS",
                   "HOME_SCORE_MARGIN", "HOME_POSS_IND", "HOME_G", "DESCRIPTION",
                   "ISVISIBLE",
                   "Transaction_Type", "TRANSACTION_DATE", "TRANSACTION_DESCRIPTION",
                   "Additional_Sort", "GroupSort",
                   "ListItemCaption", "ListItemDescription", "ListItemPubDate",
                   "lastUpdate", "UpdateId", "RotoId", "FirstName", "LastName",
                   "Position", "Team", "TeamCode", "Date", "Priority", "Headline",
                   "Injured", "Injured_Status", "Injury_Location", "Injury_Type",
                   "Injury_Detail", "Injury_Side",
                   "YEAR", "WINS", "LOSSES", "WIN_PCT", "CONF_RANK", "DIV_RANK",
                   "PO_WINS", "PO_LOSSES", "CONF_COUNT", "DIV_COUNT", "NBA_FINALS_APPEARANCE",
                   "PlayerIDSID", "PlayerFirstName", "PlayerLastName", "PlayerNumber",
                   "P", "TeamIDSID", "TeamName", "TeamNameAbbreviation", "TeamShortName",
                   "Poss", "Time", "Points", "PPP", "WorsePPP", "BetterPPP", "PossG",
                   "FGAG", "FGMG", "FGmG", "FGm", "FG", "aFG", "FT", "SF", "PlusOne",
                   "Score", "name", "season", "seasonType",
                   "SeasonID", "TeamCity", "Conference", "ConferenceRecord", "PlayoffRank",
                   "ClinchIndicator", "Division", "DivisionRecord", "DivisionRank",
                   "WinPCT", "LeagueRank", "Record", "HOME", "ROAD", "L10", "Last10Home",
                   "Last10Road", "OT", "ThreePTSOrLess", "TenPTSOrMore", "LongHomeStreak",
                   "strLongHomeStreak", "LongRoadStreak", "strLongRoadStreak", "LongWinStreak",
                   "LongLossStreak", "CurrentHomeStreak", "strCurrentHomeStreak",
                   "CurrentRoadStreak", "strCurrentRoadStreak", "CurrentStreak",
                   "strCurrentStreak", "ConferenceGamesBack", "DivisionGamesBack",
                   "ClinchedConferenceTitle", "ClinchedDivisionTitle", "ClinchedPlayoffBirth",
                   "EliminatedConference", "EliminatedDivision", "AheadAtHalf",
                   "BehindAtHalf", "TiedAtHalf", "AheadAtThird", "BehindAtThird",
                   "TiedAtThird", "Score100PTS", "OppScore100PTS", "OppOver500",
                   "LeadInFGPCT", "LeadInReb", "FewerTurnovers", "PointsPG", "OppPointsPG",
                   "DiffPointsPG", "vsEast", "vsAtlantic", "vsCentral", "vsWest",
                   "vsPacific", "vsMidwest", "Jan", "Feb", "Mar", "Apr", "May",
                   "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
                   "vsNorthwest", "vsSoutheast", "vsSouthwest",
                   "CONFERENCE", "HIGH_SEED_RANK", "HIGH_SEED_TEAM", "HIGH_SEED_TEAM_ID",
                   "LOW_SEED_RANK", "LOW_SEED_TEAM", "LOW_SEED_TEAM_ID", "HIGH_SEED_SERIES_W",
                   "HIGH_SEED_SERIES_L", "HIGH_SEED_SERIES_REMAINING_G", "HIGH_SEED_SERIES_REMAINING_HOME_G",
                   "HIGH_SEED_SERIES_REMAINING_AWAY_G",
                   "REMAINING_G", "REMAINING_HOME_G", "REMAINING_AWAY_G",
                   "PCT", "DIV", "CONF", "AWAY", "GB", "GR_OVER_500", "GR_OVER_500_HOME",
                   "GR_OVER_500_AWAY", "GR_UNDER_500", "GR_UNDER_500_HOME", "GR_UNDER_500_AWAY",
                   "RANKING_CRITERIA", "CLINCHED_PLAYOFFS", "CLINCHED_CONFERENCE",
                   "CLINCHED_DIVISION", "ELIMINATED_PLAYOFFS", "SOSA_REMAINING",
                   "NICKNAME", "YEARFOUNDED", "CITY", "ARENA", "ARENACAPACITY",
                   "OWNER", "GENERALMANAGER", "HEADCOACH", "DLEAGUEAFFILIATION",
                   "YEARACTIVETILL", "ACCOUNTTYPE", "WEBSITE_LINK", "YEARAWARDED",
                   "OPPOSITETEAM", "PLAYERID", "SEASONSWITHTEAM",
                   "HOME_TV_BROADCASTER_ABBREVIATION", "AWAY_TV_BROADCASTER_ABBREVIATION",
                   "ARENA_NAME",
                   "STANDINGSDATE", "G", "HOME_RECORD", "ROAD_RECORD",
                   "PTS_PLAYER_ID", "PTS_PLAYER_NAME", "REB_PLAYER_ID", "REB_PLAYER_NAME",
                   "AST_PLAYER_ID", "AST_PLAYER_NAME",
                   "teamId", "win", "loss", "winPct", "winPctV2", "lossPct", "lossPctV2",
                   "gamesBehind", "divGamesBehind", "clinchedPlayoffsCode", "clinchedPlayoffsCodeV2",
                   "confRank", "confWin", "confLoss", "divWin", "divLoss", "homeWin",
                   "homeLoss", "awayWin", "awayLoss", "lastTenWin", "lastTenLoss",
                   "streak", "divRank", "isWinStreak", "tieBreakerPts"

                 ),
               nameActual =
                 c("idPlayer", "namePlayerLastFirst", "namePlayer",
                   "isActive", "yearSeasonFirst", "yearSeasonLast", "slugPlayer", "idTeam",
                   "cityTeam", "teamName", "slugTeam", "codeTeam", "hasGamesPlayedFlag",
                   "slugSeason", "idPlayer", "namePlayer", "idGame", "dateGame",
                   "slugMatchup", "outcomeGame", "minutes", "fgm", "fga", "pctFG", "fg3m", "fg3a",
                   "pctFG3", "ftm", "fta", "pctFT", "oreb", "dreb", "treb", "ast",
                   "stl", "blk", "tov", "pf", "pts", "plusminus", "hasVideo",
                   "groupStartPosition", "descriptionComment", "pctUSG", "pctFGMOfTeam", "pctFGAOfTeam",
                   "pctFG3MOfTeam","pctFG3AOfTeam", "pctFTMOfTeam", "pctFTAOfTeam", "pctOREBOfTeam",
                   "pctDREBOfTeam", "pctREBOfTeam", "pctASTOfTeam", "pctTOVOfTeam",
                   "pctSTLOfTeam", "pctBLKOfTeam", "pctBLKAOfTeam", "pctPFOfTeam",
                   "pctPFDOfTeam", "pctPTSOfTeam", "pctUSG", "pctFGMOfTeam", "pctFGAOfTeam",
                   "pctFG3MOfTeam", "pctFG3AOfTeam", "pctFTMOfTeam", "pctFTAOfTeam",
                   "pctOREBOfTeam", "pctDREBOfTeam", "pctTREBOfTeam", "pctASTOfTeam",
                   "pctTOVOfTeam", "pctSTLOfTeam", "pctBLKOfTeam", "pctBLKAOfTeam",
                   "pctPFOfTeam", "pctPFDOfTeam", "pctPTSOfTeam",
                   "numberEvent", "numberEventMessageType", "numberEventActionType", "numberPeriod",
                   "timeStringWC", "timeQuarter", "descriptionPlayHome", "descriptionPlayNeutral",
                   "descriptionPlayVisitor", "slugScore", "marginScore", "idPersonType1",
                   "idPlayerNBA1", "namePlayer1", "idTeamPlayer1", "cityTeamPlayer1",
                   "teamNamePlayer1", "slugTeamPlayer1", "idPersonType2",
                   "idPlayerNBA2", "namePlayer2", "idTeamPlayer2", "cityTeamPlayer2",
                   "teamNamePlayer2", "slugTeamPlayer2", "idPersonType3",
                   "idPlayerNBA3", "namePlayer3", "idTeamPlayer3", "cityTeamPlayer3",
                   "teamNamePlayer3", "slugTeamPlayer3", "hasVideo",
                   "idTeamHome", "slugTeamHome", "ptsTotalTeamHome", "idTeamAway",
                   "slugTeamAway", "ptsTotalTeamAway",
                   "numberJersey", "groupPosition", "locationGame", "fptsFDActual",
                   "fptsNBAActual", "blka", "pfd",
                   "dateGame", "numberGameDate", "idGameStatus", "descriptionGameStatus",
                   "slugGame", "yearSeasonStart", "numberPeriodTime", "timeGameLive", "slugTVBroadcaster",
                   "slugQuarterLive", "hasStatusWH",
                   "idLeague", "ptsPaint", "ptsSecondChance", "ptsFastBreak", "leadLargest",
                   "countLeadChanges", "countTies", "tovTeam", "tobTotal",
                   "trbTeam", "ptsOffTOV", "idOffical", "nameFirst", "nameLast",
                   "countAttendance", "timeGame",
                   "cityTeam", "teamName", "recordOverall", "ptsQ1",
                   "ptsQ2", "ptsQ3", "ptsQ4", "ptsOT1", "ptsOT2", "ptsOT3",
                   "ptsOT4", "ptsOT5", "ptsOT6", "ptsOT7", "ptsOT8", "ptsOT9",
                   "ptsOT10",
                   "idGameLast", "dateGameLast", "idTeamHomeLast",
                   "cityTeamHomeLast", "nameTeamHomeLast", "slugTeamHomeLast",
                   "ptsTeamHomeLast", "idTeamAwayLast", "cityTeamAwayLast",
                   "nameTeamAwayLast", "slugTeamAwayLast",
                   "ptsTeamAwayLast",
                   "countWinsSeriesSeasonHome", "countLossesSeriesSeasonHome", "descriptionSeriesLeader",
                   "hasPT", "hasPTXYZ", "hasHustleStats", "hasHistoricalStatus", "tov",
                   "mphMean", "distMiles", "orebChances", "drebChances", "trebChances", "touches", "astSecondary", "ftAST",
                   "passes", "fgmContested", "fgaContested", "pctFGContested", "fgmUncontested", "fgaUncontested", "pctFGUncontested",
                   "fgmRimDefended", "fgaRimDefended", "pctFGRimDefended",
                   "pctFGAasFG2", "pctFGAasFG3", "pctPTSasFG2", "pctPTSasFG2asMR",
                   "pctsPTSasFG3", "pctPTSasFB", "pctPTSasFT", "pctPTSasOffTOV",
                   "pctPTSasPaint", "pctFG2MasAssisted", "pctFG2MasUnassisted", "pctFG3MasAssisted",
                   "pctFG3MasUnassisted", "pctFGMasAssisted", "pctFGMasUnassisted",
                   "ptsOffTOV", "ptsOffTOVOpp", "ptsSecondChanceOpp", "ptsFastBreakOpp",
                   "ptsPaintOpp",
                   "minutes", "fgContested", "fg2Contested", "fg3Contested",
                   "deflections", "looseBallsRecovered", "chargesDrawn", "screenAssist",
                   "pctEFG", "rateFTA", "pctTOVTeam", "pctOREB", "pctEFGOpp",
                   "rateFTAOpp", "pctTOVOpp", "pctOREBOpp",
                   "ortg", "drtg", "netrtg", "pctAST", "ratioASTtoTOV",
                   "ratioAST", "pctDREB", "pctTREB", "pctTS", "pace", "ratioPIE",
                   "typeGrid", "idEvent", "minutesRemaining", "secondsRemaining",
                   "typeEvent", "typeAction", "typeShot", "zoneBasic",
                   "zoneArea", "zoneRange", "distanceShot", "locationX",
                   "locationY", "isShotAttempted", "isShotMade", "slugTeamHome", "slugTeamAway",
                   "idTeam", "idLeague", "namePlayer", "numberJersey", "groupPosition", "heightInches",
                   "weightLBS", "dateBirth", "agePlayer", "countYearsExperience", "nameSchool",
                   "yearFirst", "yearLast", "slugTeam", "agePlayer", "gp", "gs",
                   "idTeam", "idOrganization", "nameSchool",
                   "minutesRank", "fgmRank", "fgaRank", "pctFGRank", "fg3mRank",
                   "fg3aRank", "pctFG3Rank", "ftmRank", "ftaRank", "pctFTRank",
                   "orebRank", "drebRank", "trebRank", "astRank", "stlRank",
                   "blkRank", "tovRank", "ptsRank", "effRank",
                   "minutesRankPerGame", "fgmRankPerGame", "fgaRankPerGame", "fg3mRankPerGame",
                   "fg3aRankPerGame", "ftmRankPerGame", "ftaRankPerGame", "orebRankPerGame",
                   "drebRankPerGame", "trebRankPerGame", "astRankPerGame", "stlRankPerGame",
                   "blkRankPerGame", "tovRankPerGame", "ptsRankPerGame", "effRankPerGame",
                   "minutesRankPer36", "fgmRankPer36", "fg3mRankPer36", "fg3aRankPer36",
                   "ftmRankPer36", "ftaRankPer36", "orebRankPer36", "drebRankPer36",
                   "trebRankPer36", "astRankPer36", "stlRankPer36", "blkRankPer36",
                   "tovRankPer36", "ptsRankPer36", "effRankPer36",
                   "namePlayer", "yearSeason", "numberRound", "numberRoundPick", "numberPickOverall",
                   "nameOrganizationFrom", "typeOrganizationFrom",
                   "nameGroup", "nameGroupValue", "slugSeason", "wins", "losses", "pctWins",
                   "gpRank", "winsRank", "lossesRank", "pctWinsRank", "minutesRank", "fgmRank",
                   "fgaRank", "pctFGRank", "fg3mRank", "fg3aRank", "pctFG3Rank",
                   "rankFTM", "rankFTA", "pctFTRank", "orebRank", "drebRank",
                   "trebRank","astRank", "tovRank", "stlRank", "blkRank", "blkaRank",
                   "pfRank", "pfdRank", "ptsRank", "plusminusRank", "idCIF",
                   "slugCIF",
                   "typeMeasure", "modeSearch", "isPlusMinus", "isPaceAdjust", "isRank",
                   "idLeague", "slugSeason", "typeSeason", "idPlayoffRound", "idTeam", "idMonth",
                   "idTeamOpponent", "idPeriod", "countLastNGames",
                   "locationGame", "resultGame", "nameSeasonMonth", "segmentSeason", "rangeDaysRest",
                   "idPlayer",
                   "outcomeGame", "locationGame", "segmentSeason", "rangeDateFrom", "rangeDateTo",
                   "vsConference", "vsDivision", "segmentGame", "rangeShotClock",
                   "ratioASTtoTO", "ortgRank", "drtgRank", "netrtgRank",
                   "pctASTRank", "ratioASTtoTORank", "ratioASTRank", "pctOREBRank",
                   "pctDREBRank", "pctTREBRank", "pctTOVTmRank", "pctEFGRank",
                   "pctTSRank", "paceRank", "pieRank",
                   "ptsOffTOVRank", "ptsSecondChanceRank", "ptsFastBreakRank", "ptsPaintRank",
                   "potsOffTOVOppRank", "ptsSecondChanceOppRank", "ptsFastBreakOppRank",
                   "ptsPaintOppRank",
                   "pctFGAasFG2Rank", "pctFGAasFG3Rank", "pctPTSasFG2Rank",
                   "pctPTSasFG2asMRRank", "pctsPTSasFG3Rank", "pctPTSasFBRank",
                   "pctPTSasFTRank", "pctPTSasOffTOVRank", "pctPTSasPaintRank",
                   "pctFG2MasAssistedRank", "pctFG2MasUnassistedRank", "pctFG3MasAssistedRank",
                   "pctFG3MasUnassistedRank", "pctFGMasAssistedRank", "pctFGMasUnassistedRank",
                   "rateFTARank", "pctEFGOppRank", "rateFTAOppRank", "pctTOVOppRank",
                   "pctOREBOppRank",
                   "fgmOpp", "fgaOpp", "pctFGOpp", "fg3mOpp", "fg3aOpp",
                   "pctFG3Opp", "ftmOpp", "ftaOpp", "pctFTOpp", "orebOpp",
                   "drebOpp", "trebOpp", "astOpp", "tovOpp", "stlOpp", "blkOpp",
                   "blkaOpp", "pfOpp", "pfdOpp", "ptsOpp", "fgmOppRank", "fgaOppRank",
                   "pctFGOppRank", "fg3mOppRank", "fg3aOppRank", "pctFG3OppRank",
                   "ftmOppRank", "ftaOppRank", "pctFTOppRank", "orebOppRank",
                   "drebOppRank", "trebOppRank", "astOppRank", "tovOppRank",
                   "stlOppRank", "blkOppRank", "blkaOppRank", "pfOppRank",
                   "pfdOppRank", "ptsOppRank",
                   'fgmPerGame', 'fgaPerGame',  'fgaPerGameRank',  'fgmPerGameRank', 'pctUSGRank',
                   "dd2", "td3", "fptsRank", "dd2Rank",  "td3Rank",

                   "pctFGMOfTeamRank", "pctFGAOfTeamRank", "pctFG3MOfTeamRank", "pctFG3AOfTeamRank",
                   "pctFTMOfTeamRank", "pctFTAOfTeamRank", "pctOREBOfTeamRank", "pctDREBOfTeamRank",
                   "pctTREBOfTeamRank", "pctASTOfTeamRank", "pctTOVOfTeamRank", "pctSTLOfTeamRank",
                   "pctBLKOfTeamRank", "pctBLKAOfTeamRank", "pctPFOfTeamRank", "pctPFDOfTeamRank",
                   "pctPTSOfTeamRank",
                   "namePlayerAbbr", "datetimeBirth", "countryPlayer", "nameOrganizationFrom",
                   "countSeasonsPlayed", "numberJersey", "hasDLeagueFlag", "yearDraft", "numberRound",
                   "numberOverallPick", "slugSeason", "countAllStarGames",
                   "eff", "pctFG3", "orebPerGame", "drebPerGame", "trebPerGame", "astPerGame", "tovPerGame",
                   "stlPerGame", "blkPerGame", "pfPerGame", "ptsPerGame", "ptsOppPerGame",
                   "pctFGPerGame", "pctFTPerGame",
                   "heightWOShoesInches", "heightWOShoes", "heightWShoesInches",
                   "heightWShoes", "wingspanInches", "wingspan", "reachStandingInches",
                   "reachStandingO", "pctBodyFat", "lengthHandInches", "widthHandInches",
                   "verticalLeapStandingInches", "verticalLeapMaxInches", "timeLaneAgility",
                   "timeModifiedLaneAgility", "timeThreeQuarterCourtSprint", "repsBenchPress135",
                   "setSpot15CornerLeft", "setSpot15BreakLeft", "setSpot15TopKey",
                   "setSpot15BreakRight", "setSpot15CornerRight", "setSpot15CornerLeftCollege",
                   "setSpot15BreakLeftCollege", "setSpot15TopKeyCollege", "setSpot15BreakRightCollege",
                   "setSpot15CornerRightCollege", "setSpot15CornerLeftNBA", "setSpot15BreakLeftNBA",
                   "setSpot15TopKeyNBA", "setSpotBreakRightNBA", "setSpotCornerRightNBA",
                   "setOffDrib15BreakLeft", "setSpotOffDrib15TopKey", "setOffDrib15BreakRight",
                   "setOffDribBreakLeftCollege", "setOffDribTopKeyCollege", "setOffDribBreakRightCollege",
                   "setOnMove15", "setOnMoveCollege",
                   "filterContext", "measureContext", "slugTeam", "numberRank", "slugScope", "categoryStat",
                   "ratioSTLtoTOV",
                   "typeSeason", "isActiveWithTeam", "gpRankF", "minutsRankF",
                   "fgmRankF", "fgaRankF", "pctFGRankF", "fg3mRankF", "fg3aRankF",
                   "pctFG3RankF", "ftmRankF", "ftaRankF", "pctFTRankF",
                   "orebRankF", "drebRankF", "trebRankF", "astRankF", "pfRankF",
                   "stlRankF", "tovRankF", "blkRankF", "fptsRankF",
                   "numberEvent", "pctWinProbHome", "pctWinProbAway", "ptsTotalHome", "ptsTotalVisitor",
                   "ptsMarginHome", "isHomePossesion", "numberHomeG", "descriptionPlay",
                   "isPlayVisible",
                   "typeTransaction", "dateTransaction", "descriptionTransaction",
                   "idTeamFrom", "sortGroup",
                   "captionItem", "descriptionItem", "datetimePublished",
                   "datetimeUpdatedLast", "idUpdate", "idRotoWorld", "nameFirst", "nameLast",
                   "groupPosition", "slugTeam", "codeTeam", "dateISO", "numberPriority", "articleHeadline",
                   "slugInjured", "statusInjury", "locationInjury", "typeInjury",
                   "detailInjury", "sideInjury",
                   "slugSeason", "wins", "losses", "pctWins", "rankConference", "rankDivision",
                   "countWinsPlayoffs", "countLossesPlayoffs", "countConferenceTitles", "countDivisionTitles", "descriptionNBAFinalsAppearance",
                   "idPlayer", "nameFirst", "nameLast", "numberJersey",
                   "groupPosition", "idTeam", "nameTeam", "slugTeam", "nameTeamShort",
                   "poss", "pctFrequency", "pts", "ppp", "pppWorseTeams", "pppBetterTeams", "possPerGame",
                   "fgaPerGame", "fgmPerGame", "fgMissPerGame", "fgmTotal", "pctFG", "pctEFG", "pctFTDrawn", "pctShootingFoulDrawn", "pctAnd1Drawn",
                   "pctScore", "typeSet", "yearSeason", "typeSeason",
                   "idSeason", "cityTeam", "nameConference", "recordConference", "rankPlayoffs",
                   "slugPlayoffClinch", "nameDivison", "RecordDivision", "rankDivision",
                   "pctWinTeam", "rankTeam", "recordOverall", "recordHome", "recordAway", "recordLast10", "recordLast10Home",
                   "recordLast10Away", "recordOT", "recordThreePTSOrLess", "recordTenPTSOrMore", "streakLongHome",
                   "slugStreakLongHomeStreak", "streakLongAway", "slugStreakLongAway", "streakWinLong",
                   "streakLossLong", "streakHomeCurrent", "slugStreakHomeCurrent",
                   "streakAwayCurrent", "slugStreakAwayCurrent", "streakCurrent",
                   "slugStreakCurrent", "gamesBackConference", "gamesBackDivision",
                   "hasClinchedConferenceTitle", "hasClinchedDivisionTitle", "hasClinchedPlayoffBirth",
                   "isEliminatedConference", "isEliminatedDivision", "recordAheadAtHalf",
                   "recordBehindAtHalf", "recordTiedAtHalf", "recordAheadAtThird", "recordBehindAtThird",
                   "recordTiedAtThird", "recordScore100PTS", "recordOppScore100PTS", "recordOppOver500",
                   "recordLeadInFGPCT", "recordLeadInReb", "recordFewerTurnovers", "ptsPerGameTeam", "ptsPerGameOpp",
                   "ptsPerGameDiff", "recordVsEast", "recordVsAtlantic", "recordVsCentral", "recordVsWest",
                   "recordVsPacific", "recordVsMidwest", "recordJan", "recordFeb", "recordMar", "recordApr", "reocrdMay",
                   "recordJun", "recordJul", "recordAug", "recordSep", "recordOct", "recordNov", "recordDec",
                   "recordVsNorthwest", "recordVsSoutheast", "recordVsSouthwest",
                   "nameConference", "rankSeedHigh", "teamNameShortHigh", "idTeamHigh",
                   "rankSeedLow", "teamNameShortLow", "idTeamLow", "winsPlayoffSeedLigh",
                   "lossesPlayoffsSeedHIgh", "gamesRemainingSeedHome", "gamesRemainingAtHomeSeedHome",
                   "gamesRemainingAwaySeedHome",
                   "gamesRemaining", "gamesRemainingHome", "gamesRemainingAway",
                   "pctWinTeam", "recordDivision", "recordConference", "recordAway", "gamesBack", "gamesRemainingOver500", "gamesRemainingOver500Home",
                   "gamesRemainingOver500Away", "gamesRemainingUnder500", "gamesRemainingUnder500Home", "gamesRemainingUnder500Away",
                   "rankCriteria", "hasClinchedPlayoffs", "hasClinchedConference",
                   "hasClinchedDivision", "isEliminatedPlayoffs", "strengthScheduleRemaining",
                   "teamName", "yearFounded", "cityTeam", "nameArena", "capacityArena",
                   "nameOwner", "nameGeneralManager", "nameHeadCoach", "nameDLeagueAffiliate",
                   "yearActiveUntil", "nameAccount", "urlAccount", "yearSeason",
                   "nameTeamOpponent", "idPlayer", "slugSeasonsWithTeam",
                   "slugNetworkTVHome", "slugTVNetworkAway",
                   "nameArena",
                   "dateStandings", "gamesTotal", "recordHome", "recordAway",
                   "idPlayerPTSLeader", "namePlayerPTSLeader", "idPlayerTREBLeader", "namePlayerTREBLeader",
                   "idPlayerASTLeader", "namePlayerASTLeader",
                   "idTeam", "wins", "losses", "pctWinRemove", "pctWins", "pctLossRemove", "pctLosses",
                   "gamesBehindPlayoffs", "gamesBehindDivision", "codePlayoffRemove", "codePlayoffCling",
                   "rankConference", "winsConference", "lossesConference", "winsDivison", "lossesDivision", "winsHome",
                   "lossesHome", "winsAway", "lossesAway", "winsLast10", "lossesLast10",
                   "streakCurrent", "rankDivision", "isWinStreak", "ptsTieBreaker"

                 )
    )
  }

resolve_nba_names <- function(json_names) {
  df_nba_names <-
    dictionary_nba_names()

  json_names %>%
    map_chr(function(name){
      no_name <-
        df_nba_names %>%
        filter(nameNBA == name) %>%
        nrow() == 0

      if (no_name) {
        glue::glue("Missing {name} in dictionary") %>% message()
        return(name)
      }
      df_nba_names %>%
        filter(nameNBA == name) %>%
        pull(nameActual) %>%
        unique() %>%
        .[[1]]
    })

}

char_words <-
  function(words = c("name[A-Z]", "date[A-Z]", "slug[A-Z]", "outcome[A-Z]", "team[A-Z]", 'height[A-Z]', 'result[A-Z]', "segment[A-Z]", "range[A-Z]", "vs[A-Z]", "mode[A-Z]", "category[A-Z]", "record[A-Z]", "^url[A-Z]",
                     "description", "city", "time[A-Z]", "nickname[A-Z]", "group[A-Z]", "location[A-Z]", "zone[A-Z]", "type[A-Z]")){
    words %>% stringr::str_c(collapse = "|")
  }

### ned to think about htis
munge_play_description <- function(data) {

}

munge_nba_data <- function(data) {
  if (data %>% tibble::has_name("datetimeBirth")) {
    data <-
      data %>%
      mutate(datetimeBirth = datetimeBirth %>%  readr::parse_datetime() %>% as.Date())
  }

  if (data %>% tibble::has_name("timeGame")) {
    data <-
      data %>%
      tidyr::separate(timeGame, into = c("hours", "minutes"), sep = "\\:") %>%
      mutate_at(c("hours", "minutes"),
                funs(. %>% as.numeric())) %>%
      mutate(lengthGameMinutes = (hours * 60) + minutes) %>%
      dplyr::select(-one_of(c("hours", "minutes")))
  }

  if (data %>% tibble::has_name("minutes")) {
    if (data$minutes %>% str_count("\\:") %>% sum(na.rm = T) > 0) {
      data <- data %>%
        tidyr::separate(minutes, into = c("min", "seconds"), sep = "\\:") %>%
        mutate_at(c("min", "seconds"),
                  funs(. %>% as.numeric())) %>%
        mutate(seconds = seconds / 60,
               minExact = min + seconds) %>%
        dplyr::select(-c(min, seconds)) %>%
        dplyr::select(one_of(c("idGame", "descriptionComment", "minExact")), everything()) %>%
        suppressWarnings()
    }
  }

  char_names <- data %>% dplyr::select(matches(char_words())) %>% names()

  num_names <-
    data %>% dplyr::select(-one_of(char_names)) %>% names()

  data <-
    data %>%
    mutate_at(num_names,
              funs(. %>% as.numeric())) %>%
    suppressWarnings()

  if (data %>% tibble::has_name("fga") && data %>%  tibble::has_name("fg3a")) {
    data <-
      data %>%
      mutate(fg2m = fgm - fg3m,
             fg2a = fga - fg3a,
             pctFG2 = fg2m / fg2a)
  }

  if (data %>% tibble::has_name("slugMatchup")){
    data <-
      data %>%
      mutate(locationGame = case_when(slugMatchup %>% str_detect("@") ~
                                        "A",
                                      T ~ "H")) %>%
      tidyr::separate(
        slugMatchup,
        into = c("remove", "slugOpponent"),
        sep = c("vs.|@"),
        remove = F
      ) %>%
      dplyr::select(-remove) %>%
      mutate_if(is.character,
                funs(. %>% str_trim())) %>%
      dplyr::select(slugSeason:outcomeGame, locationGame, everything())
  }

  if (data %>% tibble::has_name("groupStartPosition")){
    data <-
      data %>%
      mutate(isStarter = !is.na(groupStartPosition)) %>%
      dplyr::select(matches("id|name|slug|city|is"), everything())
  }

  if (data %>% tibble::has_name("dateGame")) {
    if (data$dateGame %>% str_detect("T") %>% sum(na.rm = T) > 0) {
      data <-
        data %>%
        mutate(dateGame = dateGame %>% substr(1,10) %>% lubridate::ymd())
    }
  }

  if (data %>% tibble::has_name("dateGameLast")) {
    if (data$dateGameLast %>% str_detect("T") %>% sum(na.rm = T) > 0) {
      data <-
        data %>%
        mutate(dateGameLast = dateGameLast %>% substr(1,10) %>% lubridate::ymd())
    } else {
      data <-
        data %>%
        mutate(dateGameLast = dateGameLast %>% lubridate::mdy())
    }
  }
  if (data %>% tibble::has_name("slugScore")) {
    data <-
      data %>%
      tidyr::separate(slugScore, into = c("scoreHome", "scoreAway"), sep = "\\ - ", remove = F) %>%
      mutate_at(c("scoreHome", "scoreAway"),
                funs(. %>% as.numeric())) %>%
      mutate(slugTeamLeading = case_when(marginScore == 0 ~ "Tie",
                                         marginScore < 0 ~ "Away",
                                         TRUE ~ "Home"))
  }

  if (data %>% tibble::has_name("timeQuarter")) {
    data <-
      data %>%
      tidyr::separate(
        "timeQuarter",
        into = c("minuteRemainingQuarter", "secondsRemainingQuarter"),
        sep = "\\:",
        remove = F
      ) %>%
      mutate_at(c("minuteRemainingQuarter", "secondsRemainingQuarter"),
                funs(. %>% as.numeric())) %>%

      mutate(
        minuteGame = ((numberPeriod - 1) * 12) + (12 - minuteRemainingQuarter) + (((
          60 - secondsRemainingQuarter
        ) / 60) - 1),
        timeRemaining = 48 - ((numberPeriod - 1) * 12) - (12 - minuteRemainingQuarter) -
          ((60 - secondsRemainingQuarter) / 60 - 1)
      ) %>%
      dplyr::select(idGame:numberPeriod, minuteGame, timeRemaining, everything())
  }

  if (data %>% tibble::has_name("slugRecordTeam")){
    data <-
      data %>%
      tidyr::separate(slugRecordTeam,
                      sep = "\\-",
                      into = c("winsTeam", "lossesTeam"),
                      remove = F) %>%
      mutate_at(c("winsTeam", "lossesTeam"),
                funs(. %>% as.numeric())) %>%
      mutate(countGamesTeam = winsTeam + lossesTeam,
             pctWinTeam = winsTeam / (countGamesTeam)) %>%
      dplyr::select(idGame, slugRecordTeam,countGamesTeam, pctWinTeam, everything())
  }
  data <-
    data %>%
    mutate_if(is.character,
              funs(str_trim)) %>%
    mutate_if(is.character,
              funs(ifelse(. == "", NA, .)))

  logicial_names <-
    data %>% dplyr::select(matches("^has[A-Z]|^is[A-Z]")) %>% names()

  if (logicial_names %>% length() > 0) {
    data <-
      data %>%
      mutate_at(logicial_names,
                funs(. %>% as.numeric() %>% as.logical()))
  }

  id_names <-
    data %>% dplyr::select(matches("idTeam", "idPlayer")) %>% names()

  if (id_names %>% length() > 0) {
    data <-
      data %>%
      mutate_at(id_names,
                funs(. %>% as.numeric()))
  }


  data <-
    data %>%
    dplyr::select(-matches("CIF"))

  data <-
    data %>%
    dplyr::select(which(colMeans(is.na(.)) < 1))

  data
}

nba_json_to_df <-
  function(json, table_id = 1) {
    json_names <-
      json$resultSets$headers[[table_id]]

    actual_names <-
      json_names %>% resolve_nba_names()

    data <-
      json$resultSets$rowSet[table_id] %>%
      data.frame(stringsAsFactors = F) %>%
      dplyr::as_data_frame() %>%
      purrr::set_names(actual_names)


    if (data %>% nrow() == 0) {
      return(invisible())
    }

    data %>%
      munge_nba_data()
  }
