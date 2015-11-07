packages <- #need all of these installed including some from github
  c('dplyr',
    'magrittr',
    'jsonlite',
    'tidyr',
    'purrr',
    'stringr',
    'lubridate',
    'tidyr')
options(warn = -1)
lapply(packages, library, character.only = T)
get_headers <- function() {
  headers_df <-
    data_frame(
      name.nba = c(
        "PLAYER_ID",
        "SEASON_ID",
        "LEAGUE_ID",
        "TEAM_ID",
        "TEAM_ABBREVIATION",
        "PLAYER_AGE",
        "GP",
        "GS",
        "MIN",
        "FGM",
        "FGA",
        "FG_PCT",
        "FG3M",
        "FG3A",
        "FG3_PCT",
        "FTM",
        "FTA",
        "FT_PCT",
        "OREB",
        "DREB",
        "REB",
        "AST",
        "STL",
        "BLK",
        "TOV",
        "PF",
        "PTS",
        "ORGANIZATION_ID",
        "SCHOOL_NAME",
        "RANK_MIN",
        "RANK_FGM",
        "RANK_FGA",
        "RANK_FG_PCT",
        "RANK_FG3M",
        "RANK_FG3A",
        "RANK_FG3_PCT",
        "RANK_FTM",
        "RANK_FTA",
        "RANK_FT_PCT",
        "RANK_OREB",
        "RANK_DREB",
        "RANK_REB",
        "RANK_AST",
        "RANK_STL",
        "RANK_BLK",
        "RANK_TOV",
        "RANK_PTS",
        "RANK_EFF",
        "PLUS_MINUS",
        "WL",
        "MATCHUP",
        "VIDEO_AVAILABLE",
        "GAME_DATE",
        "Game_ID",
        "PERSON_ID",
        "FIRST_NAME",
        "LAST_NAME",
        "DISPLAY_FIRST_LAST",
        "DISPLAY_LAST_COMMA_FIRST",
        "DISPLAY_FI_LAST",
        "BIRTHDATE",
        "SCHOOL",
        "COUNTRY",
        "LAST_AFFILIATION",
        "HEIGHT",
        "WEIGHT",
        "SEASON_EXP",
        "JERSEY",
        "POSITION",
        "ROSTERSTATUS",
        "TEAM_NAME",
        "TEAM_CODE",
        "TEAM_CITY",
        "PLAYERCODE",
        "FROM_YEAR",
        "TO_YEAR",
        "DLEAGUE_FLAG",
        "GAMES_PLAYED_FLAG",
        "PLAYER_NAME",
        "TimeFrame",
        "PIE",
        "AGE",
        "W",
        "L",
        "W_PCT",
        "BLKA",
        "PFD",
        "DD2",
        "TD3",
        "CFID",
        "CFPARAMS",
        "OFF_RATING",
        "DEF_RATING",
        "NET_RATING",
        "AST_PCT",
        "AST_TO",
        "AST_RATIO",
        "OREB_PCT",
        "DREB_PCT",
        "REB_PCT",
        "TM_TOV_PCT",
        "EFG_PCT",
        "TS_PCT",
        "USG_PCT",
        "PACE",
        "FGM_PG",
        "FGA_PG",
        "PTS_OFF_TOV",
        "PTS_2ND_CHANCE",
        "PTS_FB",
        "PTS_PAINT",
        "OPP_PTS_OFF_TOV",
        "OPP_PTS_2ND_CHANCE",
        "OPP_PTS_FB",
        "OPP_PTS_PAINT",
        "PCT_FGA_2PT",
        "PCT_FGA_3PT",
        "PCT_PTS_2PT",
        "PCT_PTS_2PT_MR",
        "PCT_PTS_3PT",
        "PCT_PTS_FB",
        "PCT_PTS_FT",
        "PCT_PTS_OFF_TOV",
        "PCT_PTS_PAINT",
        "PCT_AST_2PM",
        "PCT_UAST_2PM",
        "PCT_AST_3PM",
        "PCT_UAST_3PM",
        "PCT_AST_FGM",
        "PCT_UAST_FGM",
        "PCT_FGM",
        "PCT_FGA",
        "PCT_FG3M",
        "PCT_FG3A",
        "PCT_FTM",
        "PCT_FTA",
        "PCT_OREB",
        "PCT_DREB",
        "PCT_REB",
        "PCT_AST",
        "PCT_TOV",
        "PCT_STL",
        "PCT_BLK",
        "PCT_BLKA",
        "PCT_PF",
        "PCT_PFD",
        "PCT_PTS",
        "GAME_ID",
        "START_POSITION",
        "COMMENT",
        "TO",
        "STARTERS_BENCH", "AST_TOV", "FTA_RATE", "OPP_EFG_PCT", "OPP_FTA_RATE", "OPP_TOV_PCT", "OPP_OREB_PCT",
        "EVENTNUM", "EVENTMSGTYPE", "EVENTMSGACTIONTYPE", "PERIOD",
        "WCTIMESTRING", "PCTIMESTRING", "HOMEDESCRIPTION", "NEUTRALDESCRIPTION",
        "VISITORDESCRIPTION", "SCORE", "SCOREMARGIN", "PERSON1TYPE",
        "PLAYER1_ID", "PLAYER1_NAME", "PLAYER1_TEAM_ID", "PLAYER1_TEAM_CITY",
        "PLAYER1_TEAM_NICKNAME", "PLAYER1_TEAM_ABBREVIATION", "PERSON2TYPE",
        "PLAYER2_ID", "PLAYER2_NAME", "PLAYER2_TEAM_ID", "PLAYER2_TEAM_CITY",
        "PLAYER2_TEAM_NICKNAME", "PLAYER2_TEAM_ABBREVIATION", "PERSON3TYPE",
        "PLAYER3_ID", "PLAYER3_NAME", "PLAYER3_TEAM_ID", "PLAYER3_TEAM_CITY",
        "PLAYER3_TEAM_NICKNAME", "PLAYER3_TEAM_ABBREVIATION"
      ),
      name.actual = c(
        "id.player",
        "code.season",
        "id.league",
        "id.team",
        "slug.team",
        "age.player",
        "gp",
        "gs",
        "min",
        "fgm",
        "fga",
        "pct.fg",
        "fg3m",
        "fg3a",
        "pct.fg3",
        "ftm",
        "fta",
        "pct.ft",
        "oreb",
        "dreb",
        "reb",
        "ast",
        "stl",
        "blk",
        "tov",
        "fouls",
        "pts",
        "id.organization",
        "name.school",
        "rank.min",
        "rank.fgm",
        "rank.fga",
        "rank.pct.fg",
        "rank.fg3m",
        "rank.fg3a",
        "rank.pct.fg3",
        "rank.ftm",
        "rank.fta",
        "rank.pct.ft",
        "rank.oreb",
        "rank.dreb",
        "rank_reb",
        "rank.ast",
        "rank.stl",
        "rank.blk",
        "rank.tov",
        "rank.pts",
        "rank.eff",
        "plus.minus",
        "wl",
        "matchup",
        "is.video_available",
        "date.game",
        "id.game",
        "id.player",
        "name.first",
        "name.last",
        "name.player",
        "name.last.display",
        "name.middle.display",
        "date.birth",
        "school",
        "country",
        "college.non_nba_team",
        "height",
        "weight.lbs",
        "years.experience",
        "jersey",
        "position",
        "status.roster",
        "team",
        "code.team",
        "city.team",
        "slug.player",
        "year.from",
        "year.to",
        "has.d_league_data",
        "gp.flag",
        "name.player",
        "id.season",
        "pie",
        "age",
        "wins",
        "losses",
        "pct.wins",
        "fga.blocked",
        "fouls.drawn",
        "double_doubles",
        "triple_doubles",
        "cfid",
        "cfparms",
        "ortg",
        "drtg",
        "netrtg",
        "pct.ast",
        "ratio.ast.to",
        "ratio.ast",
        "pct.oreb",
        "pct.dreb",
        "pct.reb",
        "ratio.to",
        "pct.efg",
        "pct.ts",
        "pct.usg",
        "pace",
        "fgm.per_game",
        "fga.per_game",
        "pts.off_to",
        "pts.2nd_chance",
        "pts.fastbreak",
        "pts.paint",
        "pts.off_to.opponent",
        "pts.2nd_chance.opponent",
        "pts.fastbreak.opponent",
        "pts.paint.opponent",
        "pct.fga2a",
        "pct.fga3a",
        "pct.pts.fg2m",
        "pct.pts.mid_range_2",
        "pct.pts.fg3m",
        "pct.pts.fast_break",
        "pct.pts.ft",
        "pct.pts.off_tos",
        "pct.paints.paint",
        "pct.fg2m.assisted",
        "pct.fg2m.unassisted",
        "pct.fg3m.assisted",
        "pct.fg3m.unassisted",
        "pct.fgm.assisted",
        "pct.fgm.unassisted",
        "pct.fgm",
        "pct.fga",
        "pct.fg3m",
        "pct.fg3a",
        "pct.ftm",
        "pct.fta",
        "pct.oreb",
        "pct.dreb",
        "pct.reb",
        "pct.ast",
        "pct.tov",
        "pct.stl",
        "pct.blk",
        "pct.blocked",
        "pct.fouls",
        "pct.fouls.drawn",
        "pct.pts",
        "id.game",
        "id.position.start",
        "comment",
        "tov",
        "starter_bench", "ratio.ast.to",
        "rate.fta", "pct.efg.opponent", "rate.fta.opponent", "rate.tov.opponent", "pct.oreb.opponent"
      ),
      id.row = 1:length(name.actual)
    )
  return(headers_df)
}

get_header_names <- function(headers){
  actual_names <-
    1:length(headers) %>%
    purrr::map(
      function(x)
        data_frame(
          name.actual =
            headers_df %>%
            mutate(name.nba = name.nba) %>%
            dplyr::filter(name.nba == headers[x]) %>%
            .$name.actual
        )
    ) %>%
    bind_rows()
    actual_headers <-
      actual_names
    return(actual_headers)
}

get_game_id_box_score_data <-
  function(game_id,
           box_score_table = "Play by Play",
           time_period = "All",
           include_team = T,
           include_bench_starter = F,
           ...) {
    tables <-
      c(
        "Summary",
        "Traditional",
        "Advanced",
        "Misc",
        "Scoring",
        "Usage",
        "Four Factors",
        "Play by Play",
        "Player Tracking",
        "Game Charts",
        "Win Probability"
      )

    slugs <-
      c(
        "boxscoresummaryv2",
        "boxscoretraditionalv2",
        "boxscoreadvancedv2",
        "boxscoremiscv2",
        "boxscorescoringv2",
        "boxscoreusagev2",
        "boxscorefourfactorsv2",
        "playbyplayv2",
        "boxscoreplayertrackv2",
        "infographicfanduelplayer",
        "winprobabilitypbp"
      )

    df_tables <-
      data_frame(table = tables,
                 url.slug = slugs)

    if (!(box_score_table %>% str_to_lower)  %in% (tables %>% str_to_lower)) {
      "Sorry table type can only be " %>%
        paste0(tables %>% paste0(collapse = ', ')) %>%
        stop(call. = F)
    }
    t <-
      box_score_table
    base <-
      'http://stats.nba.com/stats/'

    url_stem <-
      df_tables %>%
      mutate(table = table %>% str_to_lower()) %>%
      dplyr::filter(table == t %>% str_to_lower()) %>%
      .$url.slug

    if (!game_id %>% substr(start = 1, 2) == "00") {
      game_id <-
        "00" %>%
        paste0(game_id %>% as.character())
    }

    periods <-
      c(
        "All",
        "1st Quarter",
        "2nd Quarter",
        "3rd Quarter",
        "4th Quarter",
        '1st Half',
        '2nd-3rd Quarters',
        '2nd Half'
      )

    if (!(time_period %>% str_to_lower) %in% (periods %>% str_to_lower)) {
      "Sorry period can only be " %>%
        paste0(periods %>% paste0(collapse = ', ')) %>%
        stop(call. = F)
    }

    if (time_period == "All") {
      StartPeriod_stem <-
        1
      EndPeriod_stem <-
        10
      EndRange_stem <-
        34800
      StartRange_stem <-
        "0000"
      RangeType_stem <-
        2
    }

    if (time_period == "1st Half") {
      StartPeriod_stem <-
        1
      EndPeriod_stem <-
        10
      EndRange_stem <-
        14400
      StartRange_stem <-
        "0000"
      RangeType_stem <-
        2
    }

    if (time_period == "2nd-3rd Quarters") {
      StartPeriod_stem <-
        1
      EndPeriod_stem <-
        10
      EndRange_stem <-
        21600
      StartRange_stem <-
        7200
      RangeType_stem <-
        2
    }

    if (time_period == "2nd Half") {
      StartPeriod_stem <-
        1
      EndPeriod_stem <-
        10
      EndRange_stem <-
        28800
      StartRange_stem <-
        14400
      RangeType_stem <-
        2
    }

    if (time_period == "1st Quarter") {
      EndPeriod_stem <-
        10
      EndRange_stem <-
        7200

      RangeType_stem <-
        2

      StartPeriod_stem <-
        1

      StartRange_stem <-
        "0000"
    }

    if (time_period == "2nd Quarter") {
      EndPeriod_stem <-
        10
      EndRange_stem <-
        14400

      RangeType_stem <-
        2

      StartPeriod_stem <-
        1

      StartRange_stem <-
        7200
    }

    if (time_period == "3rd Quarter") {
      EndPeriod_stem <-
        10
      EndRange_stem <-
        21600

      RangeType_stem <-
        2

      StartPeriod_stem <-
        1

      StartRange_stem <-
        14400
    }

    if (time_period == "4th Quarter") {
      EndPeriod_stem <-
        10
      EndRange_stem <-
        28800

      RangeType_stem <-
        2

      StartPeriod_stem <-
        1

      StartRange_stem <-
        21600
    }

    url_json <-
      base %>%
      paste0(
        url_stem,
        '?',
        'EndPeriod=',
        EndPeriod_stem,
        '&EndRange=',
        EndRange_stem,
        '&GameID=',
        game_id,
        '&RangeType=',
        RangeType_stem,
        '&StartPeriod=',
        StartPeriod_stem,
        '&StartRange=',
        StartPeriod_stem
      )

    json_data <-
      url_json %>%
      fromJSON(simplifyDataFrame = T, flatten = T)

    tables_names <-
      json_data$resultSets$name

    headers_df <-
      get_headers()

    if (t == "Traditional") {
      if ('PlayerStats' %in% tables_names) {
        if (json_data$resultSets$rowSet[1] %>%
            data.frame %>%
            tbl_df %>% nrow > 0) {
          headers <-
            json_data$resultSets$headers[1] %>%
            unlist

          player_data <-
            json_data$resultSets$rowSet[1] %>%
            data.frame %>%
            tbl_df

          actual_names <-
            headers %>%
            get_header_names %>%
            .$name.actual

          names(player_data) <-
            actual_names

          player_data %<>%
            separate(min, c("minute", "second"), sep = '\\:') %>%
            mutate(
              minute = minute %>% as.numeric,
              second = second %>% as.numeric,
              min = minute + (second / 60)
            ) %>%
            dplyr::select(-c(minute, second)) %>%
            dplyr::select(id.game:comment, min, everything())

          player_data %<>%
            mutate_each_(
              funs(as.numeric),
              vars = player_data %>% dplyr::select(id.team, id.player, min:plus.minus) %>% names
            ) %>%
            mutate(name.table = 'Player Stats',
                   table.boxscore = t)
        } else {
          player_data <-
            data_frame(
              name.table = 'Player Stats',
              table.boxscore = t,
              id.game = game_id
            )
        }
      }

      if ('TeamStats' %in% tables_names & include_team == T) {
        if (json_data$resultSets$rowSet[2] %>%
            data.frame %>%
            tbl_df %>% nrow > 0) {
          headers <-
            json_data$resultSets$headers[2] %>%
            unlist

          team_data <-
            json_data$resultSets$rowSet[2] %>%
            data.frame %>%
            tbl_df

          actual_names <-
            headers %>%
            get_header_names %>%
            .$name.actual

          names(team_data) <-
            actual_names

          team_data %<>%
            mutate(team = city.team %>% paste(team)) %>%
            separate(min, c("minute", "second"), sep = '\\:') %>%
            mutate(
              minute = minute %>% as.numeric,
              second = second %>% as.numeric,
              min = minute + (second / 60)
            ) %>%
            dplyr::select(-c(minute, second, city.team)) %>%
            dplyr::select(id.game:slug.team, min, everything())

          team_data %<>%
            mutate_each_(funs(as.numeric),
                         vars = team_data %>% dplyr::select(id.team, fgm:plus.minus) %>% names) %>%
            mutate(name.table = 'Team Stats',
                   table.boxscore = t)
        } else {
          team_data <-
            mutate(
              id.game = game_id,
              name.table = 'Team Stats',
              table.boxscore = t
            )
        }
      }

      if ('TeamStarterBenchStats' %in% tables_names &
          include_bench_starter == T) {
        if (json_data$resultSets$rowSet[3] %>%
            data.frame %>%
            tbl_df %>% nrow > 0) {
          headers <-
            json_data$resultSets$headers[3] %>%
            unlist

          sb_data <-
            json_data$resultSets$rowSet[3] %>%
            data.frame %>%
            tbl_df


          actual_names <-
            headers %>%
            get_header_names %>%
            .$name.actual

          names(sb_data) <-
            actual_names

          sb_data %<>%
            mutate(team = city.team %>% paste(team)) %>%
            separate(min, c("minute", "second"), sep = '\\:') %>%
            mutate(
              minute = minute %>% as.numeric,
              second = second %>% as.numeric,
              min = minute + (second / 60)
            ) %>%
            dplyr::select(-c(minute, second, city.team)) %>%
            dplyr::select(id.game:starter_bench, min, everything())

          sb_data %<>%
            mutate_each_(funs(as.numeric),
                         vars = sb_data %>% dplyr::select(id.team, fgm:plus.minus) %>% names) %>%
            mutate(name.table = 'Starter Bench Stats',
                   table.boxscore = t)
        } else {
          sb_data <-
            mutate(
              id.game = game_id,
              name.table = 'Starter Bench Stats',
              table.boxscore = t
            )
        }
      }

      if (include_team == T & include_bench_starter == T) {
        data <-
          list(player_data, team_data, sb_data)

        names(data) <-
          c('player', 'team', 'sb_data')
      }
      if (include_team == T &
          include_team == T & include_bench_starter == F) {
        team <-
          team_data %>%
          dplyr::select(id.team, min:pts)

        names(team)[2:length(names(team))] %<>%
          paste0('.team')

        data <-
          player_data %>%
          left_join(team)

      } else {
        data <-
          player_data
      }

    }

    if (t == "Advanced") {
      if ('PlayerStats' %in% tables_names) {
        if (json_data$resultSets$rowSet[1] %>%
            data.frame %>%
            tbl_df %>% nrow > 0) {
          headers <-
            json_data$resultSets$headers[1] %>%
            unlist

          player_data <-
            json_data$resultSets$rowSet[1] %>%
            data.frame %>%
            tbl_df

          actual_names <-
            headers %>%
            get_header_names %>%
            .$name.actual

          names(player_data) <-
            actual_names

          player_data %<>%
            separate(min, c("minute", "second"), sep = '\\:') %>%
            mutate(
              minute = minute %>% as.numeric,
              second = second %>% as.numeric,
              min = minute + (second / 60)
            ) %>%
            dplyr::select(-c(minute, second, city.team)) %>%
            dplyr::select(id.game:comment, min, everything())

          player_data %<>%
            mutate_each_(
              funs(as.numeric),
              vars = player_data %>% dplyr::select(id.team, id.player, min:pie) %>% names
            ) %>%
            mutate(name.table = 'Player Stats',
                   table.boxscore = t)
        } else {
          player_data <-
            data_frame(
              name.table = 'Player Stats',
              table.boxscore = t,
              id.game = game_id
            )
        }
      }

      if ('TeamStats' %in% tables_names & include_team == T) {
        if (json_data$resultSets$rowSet[2] %>%
            data.frame %>%
            tbl_df %>% nrow > 0) {
          headers <-
            json_data$resultSets$headers[2] %>%
            unlist

          team_data <-
            json_data$resultSets$rowSet[2] %>%
            data.frame %>%
            tbl_df

          actual_names <-
            headers %>%
            get_header_names %>%
            .$name.actual

          names(team_data) <-
            actual_names

          team_data %<>%
            mutate(team = city.team %>% paste(team)) %>%
            separate(min, c("minute", "second"), sep = '\\:') %>%
            mutate(
              minute = minute %>% as.numeric,
              second = second %>% as.numeric,
              min = minute + (second / 60)
            ) %>%
            dplyr::select(-c(minute, second, city.team)) %>%
            dplyr::select(id.game:slug.team, min, everything())

          team_data %<>%
            mutate_each_(funs(as.numeric),
                         vars = team_data %>% dplyr::select(id.team, ortg:pie) %>% names) %>%
            mutate(name.table = 'Team Stats',
                   table.boxscore = t)
        } else {
          team_data <-
            mutate(
              id.game = game_id,
              name.table = 'Team Stats',
              table.boxscore = t
            )
        }
      }

      if (include_team == T) {
        team <-
          team_data %>%
          dplyr::select(id.team, min:pie)

        names(team)[2:length(names(team))] %<>%
          paste0('.team')

        data <-
          player_data %>%
          left_join(team)

      } else {
        data <-
          player_data
      }
    }

    if (t == "Misc") {
      if ('PlayerStats' %in% tables_names) {
        if (json_data$resultSets$rowSet[1] %>%
            data.frame %>%
            tbl_df %>% nrow > 0) {
          headers <-
            json_data$resultSets$headers[1] %>%
            unlist

          player_data <-
            json_data$resultSets$rowSet[1] %>%
            data.frame %>%
            tbl_df

          actual_names <-
            headers %>%
            get_header_names %>%
            .$name.actual

          names(player_data) <-
            actual_names

          player_data %<>%
            separate(min, c("minute", "second"), sep = '\\:') %>%
            mutate(
              minute = minute %>% as.numeric,
              second = second %>% as.numeric,
              min = minute + (second / 60)
            ) %>%
            dplyr::select(-c(minute, second, city.team)) %>%
            dplyr::select(id.game:comment, min, everything())

          player_data %<>%
            mutate_each_(
              funs(as.numeric),
              vars = player_data %>% dplyr::select(id.team, id.player, min:fouls.drawn) %>% names
            ) %>%
            mutate(name.table = 'Player Stats',
                   table.boxscore = t)
        } else {
          player_data <-
            data_frame(
              name.table = 'Player Stats',
              table.boxscore = t,
              id.game = game_id
            )
        }
      }

      if ('TeamStats' %in% tables_names & include_team == T) {
        if (json_data$resultSets$rowSet[2] %>%
            data.frame %>%
            tbl_df %>% nrow > 0) {
          headers <-
            json_data$resultSets$headers[2] %>%
            unlist

          team_data <-
            json_data$resultSets$rowSet[2] %>%
            data.frame %>%
            tbl_df

          actual_names <-
            headers %>%
            get_header_names %>%
            .$name.actual

          names(team_data) <-
            actual_names


          team_data %<>%
            mutate(team = city.team %>% paste(team)) %>%
            separate(min, c("minute", "second"), sep = '\\:') %>%
            mutate(
              minute = minute %>% as.numeric,
              second = second %>% as.numeric,
              min = minute + (second / 60)
            ) %>%
            dplyr::select(-c(minute, second, city.team)) %>%
            dplyr::select(id.game:slug.team, min, everything())

          team_data %<>%
            mutate_each_(funs(as.numeric),
                         vars = team_data %>% dplyr::select(id.team, min:fouls.drawn) %>% names) %>%
            mutate(name.table = 'Team Stats',
                   table.boxscore = t)
        } else {
          team_data <-
            mutate(
              id.game = game_id,
              name.table = 'Team Stats',
              table.boxscore = t
            )
        }
      }

      if (include_team == T) {
        team <-
          team_data %>%
          dplyr::select(id.team, min:fouls.drawn)

        names(team)[2:length(names(team))] %<>%
          paste0('.team')

        data <-
          player_data %>%
          left_join(team)

      } else {
        data <-
          player_data
      }
    }

    if (t == "Scoring") {
      if ('sqlPlayersScoring' %in% tables_names) {
        if (json_data$resultSets$rowSet[1] %>%
            data.frame %>%
            tbl_df %>% nrow > 0) {
          headers <-
            json_data$resultSets$headers[1] %>%
            unlist

          player_data <-
            json_data$resultSets$rowSet[1] %>%
            data.frame %>%
            tbl_df

          actual_names <-
            headers %>%
            get_header_names %>%
            .$name.actual

          names(player_data) <-
            actual_names

          player_data %<>%
            separate(min, c("minute", "second"), sep = '\\:') %>%
            mutate(
              minute = minute %>% as.numeric,
              second = second %>% as.numeric,
              min = minute + (second / 60)
            ) %>%
            dplyr::select(-c(minute, second, city.team)) %>%
            dplyr::select(id.game:comment, min, everything())

          player_data %<>%
            mutate_each_(
              funs(as.numeric),
              vars = player_data %>% dplyr::select(id.team, id.player, min:pct.fgm.unassisted) %>% names
            ) %>%
            mutate(name.table = 'Player Stats',
                   table.boxscore = t)
        } else {
          player_data <-
            data_frame(
              name.table = 'Player Stats',
              table.boxscore = t,
              id.game = game_id
            )
        }
      }

      if ('sqlTeamsScoring' %in% tables_names & include_team == T) {
        if (json_data$resultSets$rowSet[2] %>%
            data.frame %>%
            tbl_df %>% nrow > 0) {
          headers <-
            json_data$resultSets$headers[2] %>%
            unlist

          team_data <-
            json_data$resultSets$rowSet[2] %>%
            data.frame %>%
            tbl_df

          actual_names <-
            headers %>%
            get_header_names %>%
            .$name.actual

          names(team_data) <-
            actual_names

          team_data %<>%
            mutate(team = city.team %>% paste(team)) %>%
            separate(min, c("minute", "second"), sep = '\\:') %>%
            mutate(
              minute = minute %>% as.numeric,
              second = second %>% as.numeric,
              min = minute + (second / 60)
            ) %>%
            dplyr::select(-c(minute, second, city.team)) %>%
            dplyr::select(id.game:slug.team, min, everything())

          team_data %<>%
            mutate_each_(funs(as.numeric),
                         vars = team_data %>% dplyr::select(id.team, min:pct.fgm.unassisted) %>% names) %>%
            mutate(name.table = 'Team Stats',
                   table.boxscore = t)
        } else {
          team_data <-
            mutate(
              id.game = game_id,
              name.table = 'Team Stats',
              table.boxscore = t
            )
        }
      }

      if (include_team == T) {
        team <-
          team_data %>%
          dplyr::select(id.team, min:pct.fgm.unassisted)

        names(team)[2:length(names(team))] %<>%
          paste0('.team')

        data <-
          player_data %>%
          left_join(team)

      } else {
        data <-
          player_data
      }
    }

    if (t == "Usage") {
      if ('sqlPlayersUsage' %in% tables_names) {
        if (json_data$resultSets$rowSet[1] %>%
            data.frame %>%
            tbl_df %>% nrow > 0) {
          headers <-
            json_data$resultSets$headers[1] %>%
            unlist

          player_data <-
            json_data$resultSets$rowSet[1] %>%
            data.frame %>%
            tbl_df

          actual_names <-
            headers %>%
            get_header_names %>%
            .$name.actual

          names(player_data) <-
            actual_names

          player_data %<>%
            separate(min, c("minute", "second"), sep = '\\:') %>%
            mutate(
              minute = minute %>% as.numeric,
              second = second %>% as.numeric,
              min = minute + (second / 60)
            ) %>%
            dplyr::select(-c(minute, second, city.team)) %>%
            dplyr::select(id.game:comment, min, everything())

          player_data %<>%
            mutate_each_(
              funs(as.numeric),
              vars = player_data %>% dplyr::select(id.team, id.player, min:pct.pts) %>% names
            ) %>%
            mutate(name.table = 'Player Stats',
                   table.boxscore = t)
        } else {
          player_data <-
            data_frame(
              name.table = 'Player Stats',
              table.boxscore = t,
              id.game = game_id
            )
        }
      }

        data <-
          player_data
    }

    if (t == "Four Factors") {
      if ('sqlPlayersFourFactors' %in% tables_names) {
        if (json_data$resultSets$rowSet[1] %>%
            data.frame %>%
            tbl_df %>% nrow > 0) {
          headers <-
            json_data$resultSets$headers[1] %>%
            unlist

          player_data <-
            json_data$resultSets$rowSet[1] %>%
            data.frame %>%
            tbl_df

          actual_names <-
            headers %>%
            get_header_names %>%
            .$name.actual

          names(player_data) <-
            actual_names

          player_data %<>%
            separate(min, c("minute", "second"), sep = '\\:') %>%
            mutate(
              minute = minute %>% as.numeric,
              second = second %>% as.numeric,
              min = minute + (second / 60)
            ) %>%
            dplyr::select(-c(minute, second, city.team)) %>%
            dplyr::select(id.game:comment, min, everything())

          player_data %<>%
            mutate_each_(
              funs(as.numeric),
              vars = player_data %>% dplyr::select(id.team, id.player, min:pct.oreb.opponent) %>% names
            ) %>%
            mutate(name.table = 'Player Stats',
                   table.boxscore = t)
        } else {
          player_data <-
            data_frame(
              name.table = 'Player Stats',
              table.boxscore = t,
              id.game = game_id
            )
        }
      }

      if ('sqlTeamsFourFactors' %in% tables_names & include_team == T) {
        if (json_data$resultSets$rowSet[2] %>%
            data.frame %>%
            tbl_df %>% nrow > 0) {
          headers <-
            json_data$resultSets$headers[2] %>%
            unlist

          team_data <-
            json_data$resultSets$rowSet[2] %>%
            data.frame %>%
            tbl_df

          actual_names <-
            headers %>%
            get_header_names %>%
            .$name.actual

          names(team_data) <-
            actual_names

          team_data %<>%
            mutate(team = city.team %>% paste(team)) %>%
            separate(min, c("minute", "second"), sep = '\\:') %>%
            mutate(
              minute = minute %>% as.numeric,
              second = second %>% as.numeric,
              min = minute + (second / 60)
            ) %>%
            dplyr::select(-c(minute, second, city.team)) %>%
            dplyr::select(id.game:slug.team, min, everything())

          team_data %<>%
            mutate_each_(funs(as.numeric),
                         vars = team_data %>% dplyr::select(id.team, min:pct.oreb.opponent) %>% names) %>%
            mutate(name.table = 'Team Stats',
                   table.boxscore = t)
        } else {
          team_data <-
            mutate(
              id.game = game_id,
              name.table = 'Team Stats',
              table.boxscore = t
            )
        }
      }

      if (include_team == T) {
        team <-
          team_data %>%
          dplyr::select(id.team, min:pct.oreb.opponent)

        names(team)[2:length(names(team))] %<>%
          paste0('.team')

        data <-
          player_data %>%
          left_join(team)

      } else {
        data <-
          player_data
      }
    }

    if (t == "Four Factors") {
      if ('sqlPlayersFourFactors' %in% tables_names) {
        if (json_data$resultSets$rowSet[1] %>%
            data.frame %>%
            tbl_df %>% nrow > 0) {
          headers <-
            json_data$resultSets$headers[1] %>%
            unlist

          player_data <-
            json_data$resultSets$rowSet[1] %>%
            data.frame %>%
            tbl_df

          actual_names <-
            headers %>%
            get_header_names %>%
            .$name.actual

          names(player_data) <-
            actual_names

          player_data %<>%
            separate(min, c("minute", "second"), sep = '\\:') %>%
            mutate(
              minute = minute %>% as.numeric,
              second = second %>% as.numeric,
              min = minute + (second / 60)
            ) %>%
            dplyr::select(-c(minute, second, city.team)) %>%
            dplyr::select(id.game:comment, min, everything())

          player_data %<>%
            mutate_each_(
              funs(as.numeric),
              vars = player_data %>% dplyr::select(id.team, id.player, min:pct.oreb.opponent) %>% names
            ) %>%
            mutate(name.table = 'Player Stats',
                   table.boxscore = t)
        } else {
          player_data <-
            data_frame(
              name.table = 'Player Stats',
              table.boxscore = t,
              id.game = game_id
            )
        }
      }

      if ('sqlTeamsFourFactors' %in% tables_names & include_team == T) {
        if (json_data$resultSets$rowSet[2] %>%
            data.frame %>%
            tbl_df %>% nrow > 0) {
          headers <-
            json_data$resultSets$headers[2] %>%
            unlist

          team_data <-
            json_data$resultSets$rowSet[2] %>%
            data.frame %>%
            tbl_df

          actual_names <-
            headers %>%
            get_header_names %>%
            .$name.actual

          names(team_data) <-
            actual_names

          team_data %<>%
            mutate(team = city.team %>% paste(team)) %>%
            separate(min, c("minute", "second"), sep = '\\:') %>%
            mutate(
              minute = minute %>% as.numeric,
              second = second %>% as.numeric,
              min = minute + (second / 60)
            ) %>%
            dplyr::select(-c(minute, second, city.team)) %>%
            dplyr::select(id.game:slug.team, min, everything())

          team_data %<>%
            mutate_each_(funs(as.numeric),
                         vars = team_data %>% dplyr::select(id.team, min:pct.oreb.opponent) %>% names) %>%
            mutate(name.table = 'Team Stats',
                   table.boxscore = t)
        } else {
          team_data <-
            mutate(
              id.game = game_id,
              name.table = 'Team Stats',
              table.boxscore = t
            )
        }
      }

      if (include_team == T) {
        team <-
          team_data %>%
          dplyr::select(id.team, min:pct.oreb.opponent)

        names(team)[2:length(names(team))] %<>%
          paste0('.team')

        data <-
          player_data %>%
          left_join(team)

      } else {
        data <-
          player_data
      }
    }

    if (t == "Play by Play") {
      if ('PlayByPlay' %in% tables_names) {
        if (json_data$resultSets$rowSet[1] %>%
            data.frame %>%
            tbl_df %>% nrow > 0) {
          headers <-
            json_data$resultSets$headers[1] %>%
            unlist
          headers[!headers %in% headers_df$name.nba]
          player_data <-
            json_data$resultSets$rowSet[1] %>%
            data.frame %>%
            tbl_df

          actual_names <-
            headers %>%
            get_header_names %>%
            .$name.actual

          names(player_data) <-
            actual_names

          player_data %<>%
            separate(min, c("minute", "second"), sep = '\\:') %>%
            mutate(
              minute = minute %>% as.numeric,
              second = second %>% as.numeric,
              min = minute + (second / 60)
            ) %>%
            dplyr::select(-c(minute, second, city.team)) %>%
            dplyr::select(id.game:comment, min, everything())

          player_data %<>%
            mutate_each_(
              funs(as.numeric),
              vars = player_data %>% dplyr::select(id.team, id.player, min:pct.pts) %>% names
            ) %>%
            mutate(name.table = 'Player Stats',
                   table.boxscore = t)
        } else {
          player_data <-
            data_frame(
              name.table = 'Player Stats',
              table.boxscore = t,
              id.game = game_id
            )
        }
      }

      data <-
        player_data
    }

      return(data)
  }
