function_packages <-
  c(
    'dplyr',
    'magrittr',
    'jsonlite',
    'tidyr',
    'stringr',
    'lubridate',
    'purrr',
    'stringr',
    'tidyr'
  )
options(warn = -1)

install_needed_packages <-
  function(required_packages = function_packages) {
    needed_packages <-
      required_packages[!(required_packages %in% installed.packages()[, "Package"])]

    if (length(needed_packages) > 0) {
      if (!require("pacman"))
        install.packages("pacman")
      pacman::p_load(needed_packages)
    }
  }

load_needed_packages <-
  function(required_packages = function_packages) {
    loaded_packages <-
      gsub('package:', '', search())

    package_to_load <-
      required_packages[!required_packages %in% loaded_packages]
    if (length(package_to_load) > 0) {
      lapply(package_to_load, library, character.only = T)
    }
  }
get_headers <- function() {
  install_needed_packages(function_packages)
  load_needed_packages(function_packages)
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
        "STARTERS_BENCH",
        "AST_TOV",
        "FTA_RATE",
        "OPP_EFG_PCT",
        "OPP_FTA_RATE",
        "OPP_TOV_PCT",
        "OPP_OREB_PCT",
        "EVENTNUM",
        "EVENTMSGTYPE",
        "EVENTMSGACTIONTYPE",
        "PERIOD",
        "WCTIMESTRING",
        "PCTIMESTRING",
        "HOMEDESCRIPTION",
        "NEUTRALDESCRIPTION",
        "VISITORDESCRIPTION",
        "SCORE",
        "SCOREMARGIN",
        "PERSON1TYPE",
        "PLAYER1_ID",
        "PLAYER1_NAME",
        "PLAYER1_TEAM_ID",
        "PLAYER1_TEAM_CITY",
        "PLAYER1_TEAM_NICKNAME",
        "PLAYER1_TEAM_ABBREVIATION",
        "PERSON2TYPE",
        "PLAYER2_ID",
        "PLAYER2_NAME",
        "PLAYER2_TEAM_ID",
        "PLAYER2_TEAM_CITY",
        "PLAYER2_TEAM_NICKNAME",
        "PLAYER2_TEAM_ABBREVIATION",
        "PERSON3TYPE",
        "PLAYER3_ID",
        "PLAYER3_NAME",
        "PLAYER3_TEAM_ID",
        "PLAYER3_TEAM_CITY",
        "PLAYER3_TEAM_NICKNAME",
        "PLAYER3_TEAM_ABBREVIATION",
        "SPD",
        "DIST",
        "ORBC",
        "DRBC",
        "RBC",
        "TCHS",
        "SAST",
        "FTAST",
        "PASS",
        "CFGM",
        "CFGA",
        "CFG_PCT",
        "UFGM",
        "UFGA",
        "UFG_PCT",
        "DFGM",
        "DFGA",
        "DFG_PCT",
        "TEAM_NICKNAME",
        'FAN_DUEL_PTS',
        "JERSEY_NUM",
        "PLAYER_POSITION",
        "LOCATION",
        "EVENT_NUM",
        "HOME_PCT",
        "VISITOR_PCT",
        "HOME_PTS",
        "VISITOR_PTS",
        "HOME_SCORE_MARGIN",
        "SECONDS_REMAINING",
        "HOME_POSS_IND",
        "HOME_G",
        "DESCRIPTION",
        "ISVISIBLE",
        "HOME_TEAM_ID",
        "HOME_TEAM_ABR",
        "HOME_TEAM_PTS",
        "VISITOR_TEAM_ID",
        "VISITOR_TEAM_ABR",
        "VISITOR_TEAM_PTS",
        "GAME_DATE_EST",
        "GAME_SEQUENCE",
        "GAME_STATUS_ID",
        "GAME_STATUS_TEXT",
        "GAMECODE",
        "SEASON",
        "LIVE_PERIOD",
        "LIVE_PC_TIME",
        "NATL_TV_BROADCASTER_ABBREVIATION",
        "LIVE_PERIOD_TIME_BCAST",
        "WH_STATUS",
        "LARGEST_LEAD",
        "LEAD_CHANGES",
        "TIMES_TIED",
        "ids.officials",
        "officials",
        "jerseys.official",
        "id.team",
        "players.inactive",
        "id.players.inactive",
        "ATTENDANCE",
        "GAME_TIME",
        "TEAM_CITY_NAME",
        "TEAM_WINS_LOSSES",
        "PTS_QTR1",
        "PTS_QTR2",
        "PTS_QTR3",
        "PTS_QTR4",
        "PTS_OT1",
        "PTS_OT2",
        "PTS_OT3",
        "PTS_OT4",
        "PTS_OT5",
        "PTS_OT6",
        "PTS_OT7",
        "PTS_OT8",
        "PTS_OT9",
        "PTS_OT10",
        "LAST_GAME_ID",
        "LAST_GAME_DATE_EST",
        "LAST_GAME_HOME_TEAM_ID",
        "LAST_GAME_HOME_TEAM_CITY",
        "LAST_GAME_HOME_TEAM_NAME",
        "LAST_GAME_HOME_TEAM_ABBREVIATION",
        "LAST_GAME_HOME_TEAM_POINTS",
        "LAST_GAME_VISITOR_TEAM_ID",
        "LAST_GAME_VISITOR_TEAM_CITY",
        "LAST_GAME_VISITOR_TEAM_NAME",
        "LAST_GAME_VISITOR_TEAM_CITY1",
        "LAST_GAME_VISITOR_TEAM_POINTS",
        "HOME_TEAM_WINS",
        "HOME_TEAM_LOSSES",
        "SERIES_LEADER",
        "VIDEO_AVAILABLE_FLAG",
        "PT_AVAILABLE",
        "PT_XYZ_AVAILABLE",
        'OFFICIAL_ID'

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
        "starter_bench",
        "ratio.ast.to",
        "rate.fta",
        "pct.efg.opponent",
        "rate.fta.opponent",
        "rate.tov.opponent",
        "pct.oreb.opponent",
        "id.event",
        "id.type.type",
        "id.action.type",
        "period",
        "time",
        "minute.quarter",
        "play.home.description",
        "play.neutral.description",
        "play.visitor.description",
        "score",
        "score.margin",
        "type.player.1",
        "id.player.1",
        "name.player.1",
        "id.team.player.1",
        "city.team.player.1",
        "name.team.player.1",
        "slug.team.player.1",
        "type.player.2",
        "id.player.2",
        "name.player.2",
        "id.team.player.2",
        "city.team.player.2",
        "name.team.player.2",
        "slug.team.player.2",
        "type.player.3",
        "id.player.3",
        "name.player.3",
        "id.team.player.3",
        "city.team.player.3",
        "name.team.player.3",
        "slug.team.player.3",
        "avg.mph",
        "distance.miles",
        "oreb.chances",
        "dreb.chances",
        "reb.chances",
        "touches",
        "ast.secondary",
        "ast.to.fta",
        "passes",
        "fgm.contested",
        "fga.contested",
        "pct.fg.contested",
        "fgm.uncontested",
        "fga.uncontested",
        "pct.fg.uncontested",
        "fgm.opponent.rim_defense",
        "fga.opponent.rim_defense",
        "pct.fgm.opponent.rim_defense",
        "name.team",
        'points.fanduel',
        "jersey",
        "position",
        "location",
        "id.event",
        "pct.win_prob.home",
        "pct.win_prob.away",
        "points.home",
        "points.away",
        "score.margin.home",
        "quarter.seconds_remaining",
        "is.home.possesion",
        "home.g",
        "play",
        "is.visible",
        "id.team.home",
        "slug.team.home",
        "points.team.home",
        "id.team.away",
        "slug.team.away",
        "points.team.away",
        "date.game",
        "sequence.game",
        "id.stats.game",
        "text.status.game",
        "slug.game",
        "year.season_start",
        "period.live",
        "time.live.pc",
        "network.tv",
        "period.live.broadcast",
        "status.wh",
        "lead.largest",
        "lead.changes",
        "times.tied",
        "ids.officials",
        "officials",
        "jerseys.official",
        "id.team",
        "players.inactive",
        "id.players.inactive",
        "attendance",
        "time.game",
        "city.team",
        "record.team",
        "pts.q1",
        "pts.q2",
        "pts.q3",
        "pts.q4",
        "pts.ot.1",
        "pts.ot.2",
        "pts.ot.3",
        "pts.ot.4",
        "pts.ot.5",
        "pts.ot.6",
        "pts.ot.7",
        "pts.ot.8",
        "pts.ot.9",
        "pts.ot.10",
        "id.game.last",
        "date.game.last",
        "id.team.home.last",
        "city.team.home.last",
        "name.team.home.last",
        "slug.team.home.last",
        "pts.team.home.last",
        "id.team.away.last",
        "city.team.away.last",
        "name.team.away.last",
        "slug.team.away.last",
        "pts.team.away.last",
        "wins.team.home",
        "losses.team.home",
        "team.series_leader",
        "is.video_available",
        "is.pt.available",
        "is.pt.xyz.available",
        'id.official'
      ),
      id.row = 1:length(name.actual)
    )
  return(headers_df)
}
get_header_names <- function(headers) {
  headers_df <-
    get_headers()
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
get_box_score_table_df <- function() {
  install_needed_packages(function_packages)
  load_needed_packages(function_packages)
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
  return(df_tables)
}
get_box_score_url <-
  function(game_id = 21500837,
           box_score_table = "Player Tracking",
           time_period = 'All') {
    install_needed_packages(function_packages)
    load_needed_packages(function_packages)
    box_score_df <-
      get_box_score_table_df()
    tables <-
      box_score_df$table
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
      box_score_df %>%
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

    if (box_score_table == "Win Probability") {
      url_json <-
        paste0(
          'http://stats.nba.com/stats/winprobabilitypbp?GameID=',
          game_id,
          '&RunType=each+second'
        )
    }
    return(url_json)
  }

get_game_id_box_score_data <-
  function(game_id = 21500837,
           box_score_table = "Scoring",
           time_period = "All",
           ...) {
    install_needed_packages(function_packages)
    load_needed_packages(function_packages)
    if (!'game_id' %>% exists) {
      stop("You must enter a gameID")
    }
    t <-
      box_score_table
    p <-
      time_period
    g <-
      game_id
    url_json <-
      get_box_score_url(
        game_id  = g,
        box_score_table = t,
        time_period = p
      )

    json_data <-
      url_json %>%
      fromJSON(simplifyDataFrame = T, flatten = T)
    return(json_data)
  }

get_game_team_win_probability_df <-
  function(game_id = 21500837,
           time_period = "All",
           include_game_data = F,
           return_message = T) {
    t <-
      'Win Probability'
    p <-
      time_period
    g <-
      game_id
    json_data <-
      get_game_id_box_score_data(
        game_id = g,
        box_score_table = t,
        time_period = p
      )

    tables_names <-
      json_data$resultSets$name

    headers_df <-
      get_headers()

    game_summary_df <-
      json_data$resultSets$rowSet[[2]] %>%
      data.frame %>%
      tbl_df

    names(game_summary_df) <-
      json_data$resultSets$headers[2] %>%
      unlist %>%
      get_header_names %>%
      .$name.actual

    game_summary_df %<>%
      mutate(date.game = date.game %>% mdy %>% as.Date())

    game_summary_df %<>%
      mutate_each_(funs(as.numeric),
                   game_summary_df %>% dplyr::select(c(contains("id"), contains("points."))) %>% names)

    headers <-
      json_data$resultSets$headers[1] %>%
      unlist

    data <-
      json_data$resultSets$rowSet[[1]] %>%
      data.frame %>%
      tbl_df

    actual_names <-
      headers %>%
      get_header_names %>%
      .$name.actual

    names(data) <-
      actual_names

    data %<>%
      mutate_each_(funs(as.numeric), vars =
                     data %>% dplyr::select(-c(play, home.g, location, minute.quarter)) %>% names()) %>%
      dplyr::rename(id.team.location = location) %>%
      mutate(
        is.home.possesion = is.home.possesion %>% as.logical,
        id.team.location = ifelse(id.team.location == "h", "H", "A")
      ) %>%
      mutate(
        game.time = ((period - 1) * 720)  + (720 - quarter.seconds_remaining),
        game.minute = game.time / 60
      ) %>%
      mutate(
        date.game = game_summary_df$date.game,
        slug.team.home = game_summary_df$slug.team.home,
        slug.team.away = game_summary_df$slug.team.away,
        game = slug.team.away %>% paste0('@', slug.team.home)
      ) %>%
      fill(id.team.location, .direction = 'up') %>%
      mutate(slug.team = ifelse(id.team.location == "H", slug.team.home, slug.team.away)) %>%
      dplyr::select(id.game,
                    date.game:game,
                    slug.team,
                    id.event,
                    game.time,
                    game.minute,
                    everything())



    if (return_message == T) {
      "You got win probability " %>%
        paste0(
          t,
          " data for game ",
          game_id,
          ' ',
          data$slug.team.home %>% unique,
          ' vs. ',
          data$slug.team.away %>% unique
        ) %>%
        message
    }
    return(data)
  }

get_game_box_score_summary_metadata_df <-
  function(game_id = 21500837,
           time_period = "All",
           return_message = T) {
    t <-
      'Summary'
    p <-
      time_period
    g <-
      game_id

    json_data <-
      get_game_id_box_score_data(
        game_id = g,
        box_score_table = t,
        time_period = p
      )

    game_date <-
      get_game_team_win_probability_df(
        game_id = g,
        return_message = F,
        time_period = p
      ) %>%
      .$date.game %>%
      unique

    all_headers <-
      json_data$resultSets$headers

    all_table_names <-
      json_data$resultSets$name

    all_data <-
      json_data$resultSets$rowSet

    summary_df <-
      all_data[1] %>%
      data.frame %>%
      tbl_df

    names(summary_df) <-
      all_headers[1] %>%
      unlist %>%
      get_header_names() %>%
      .$name.actual

    summary_df %<>%
      mutate(date.game = game_date) %>%
      mutate_each_(funs(as.numeric),
                   summary_df %>% dplyr::select(c(
                     contains("id."), sequence.game, year.season_start
                   )) %>% names)

    game_metadata_df <-
      all_data[5] %>%
      data.frame %>%
      tbl_df

    names(game_metadata_df) <-
      all_headers[5] %>%
      unlist %>%
      get_header_names() %>%
      .$name.actual

    game_metadata_df %<>%
      dplyr::mutate(id.game = g,
                    date.game = game_date) %>%
      dplyr::select(date.game, id.game, everything()) %>%
      mutate(attendance = attendance %>% as.numeric) %>%
      separate(time.game, into = c('min', 'sec'), sep = '\\:') %>%
      mutate(
        min = min %>% as.numeric * 60,
        sec = sec %>% as.numeric,
        time.game = (min + sec) %>% digits(2)
      ) %>%
      dplyr::select(-c(min, sec))


    season_series_df <-
      all_data[8] %>%
      data.frame %>%
      tbl_df

    names(season_series_df) <-
      all_headers[8] %>%
      unlist %>%
      get_header_names() %>%
      .$name.actual

    season_series_df %<>%
      mutate(date.game = game_date,
             id.game = g) %>%
      mutate_each_(
        funs(as.numeric(.)),
        season_series_df %>% dplyr::select(-team.series_leader) %>% names
      )

    last_meeting_df <-
      all_data[7] %>%
      data.frame %>%
      tbl_df

    names(last_meeting_df) <-
      all_headers[7] %>%
      unlist %>%
      get_header_names() %>%
      .$name.actual

    last_meeting_df %<>%
      unite(team.home.last,
            city.team.home.last,
            name.team.home.last,
            sep = ' ') %>%
      unite(team.away.last,
            city.team.away.last,
            name.team.away.last,
            sep = ' ') %>%
      mutate(
        date.game = game_date,
        id.game = g,
        date.game.last = date.game.last %>% substr(1, 10) %>%
          ymd %>% as.Date()
      ) %>%
      mutate_each(funs(as.numeric), c(contains("pts"), contains("id")))

    all_summary_data <-
      game_metadata_df %>%
      left_join(summary_df) %>%
      left_join(last_meeting_df) %>%
      suppressMessages() %>%
      dplyr::select(-status.wh)
    return(all_summary_data)
  }

get_game_box_score_summary_other_team_df <-
  function(game_id = 21500837,
           time_period = "All",
           return_message = T) {
    t <-
      'Summary'
    p <-
      time_period
    g <-
      game_id

    json_data <-
      get_game_id_box_score_data(
        game_id = g,
        box_score_table = t,
        time_period = p
      )

    game_date <-
      get_game_team_win_probability_df(
        game_id = g,
        return_message = F,
        time_period = p
      ) %>%
      .$date.game %>%
      unique

    all_headers <-
      json_data$resultSets$headers

    all_table_names <-
      json_data$resultSets$name

    all_data <-
      json_data$resultSets$rowSet

    other_df <-
      all_data[2] %>%
      data.frame %>%
      tbl_df

    names(other_df) <-
      all_headers[2] %>%
      unlist %>%
      get_header_names() %>%
      .$name.actual

    other_df %<>%
      dplyr::mutate(id.game = g,
                    date.game = game_date) %>%
      dplyr::select(-c(id.league))

    other_df %<>%
      mutate_each_(funs(as.numeric),
                   other_df %>% dplyr::select(-c(city.team, slug.team, date.game)) %>% names) %>%
      dplyr::select(date.game, id.game, everything())
    return(other_df)
  }

get_game_ref_df <-
  function(game_id = 21500837,
           time_period = "All",
           return_message = T) {
    t <-
      'Summary'
    p <-
      time_period
    g <-
      game_id

    json_data <-
      get_game_id_box_score_data(
        game_id = g,
        box_score_table = t,
        time_period = p
      )

    game_date <-
      get_game_team_win_probability_df(
        game_id = g,
        return_message = F,
        time_period = p
      ) %>%
      .$date.game %>%
      unique

    all_headers <-
      json_data$resultSets$headers

    all_table_names <-
      json_data$resultSets$name

    all_data <-
      json_data$resultSets$rowSet

    ref_df <-
      all_data[3] %>%
      data.frame %>%
      tbl_df

    names(ref_df) <-
      all_headers[3] %>%
      unlist %>%
      get_header_names() %>%
      .$name.actual

    ref_df %<>%
      unite(name.offical, name.first, name.last, sep = ' ') %>%
      mutate_each_(funs(as.numeric(.)), c('id.official', 'jersey')) %>%
      dplyr::mutate(id.game = g,
                    date.game = game_date) %>%
      dplyr::select(date.game, id.game, everything())

    return(ref_df)
  }

get_game_inactive_player_df <-
  function(game_id = 21500837,
           time_period = "All",
           return_message = T) {
    t <-
      'Summary'
    p <-
      time_period
    g <-
      game_id

    json_data <-
      get_game_id_box_score_data(
        game_id = g,
        box_score_table = t,
        time_period = p
      )

    game_date <-
      get_game_team_win_probability_df(
        game_id = g,
        return_message = F,
        time_period = p
      ) %>%
      .$date.game %>%
      unique

    all_headers <-
      json_data$resultSets$headers

    all_table_names <-
      json_data$resultSets$name

    all_data <-
      json_data$resultSets$rowSet

    inactive_df <-
      all_data[4] %>%
      data.frame %>%
      tbl_df

    names(inactive_df) <-
      all_headers[4] %>%
      unlist %>%
      get_header_names() %>%
      .$name.actual

    inactive_df %<>%
      unite(name.player, name.first, name.last, sep = ' ') %>%
      unite(team, city.team, team, sep = ' ') %>%
      mutate_each_(funs(as.numeric(.)),
                   c('id.player', 'id.team', 'jersey')) %>%
      dplyr::mutate(id.game = g,
                    date.game = game_date) %>%
      dplyr::select(date.game,
                    id.game,
                    id.team,
                    team,
                    slug.team,
                    id.player,
                    name.player,
                    everything())

    return(inactive_df)
  }

get_game_team_scoring_by_period_df <-
  function(game_id = 21500837,
           time_period = "All",
           return_message = T) {
    t <-
      'Summary'
    p <-
      time_period
    g <-
      game_id

    json_data <-
      get_game_id_box_score_data(
        game_id = g,
        box_score_table = t,
        time_period = p
      )

    game_date <-
      get_game_team_win_probability_df(
        game_id = g,
        return_message = F,
        time_period = p
      ) %>%
      .$date.game %>%
      unique

    all_headers <-
      json_data$resultSets$headers

    all_table_names <-
      json_data$resultSets$name

    all_data <-
      json_data$resultSets$rowSet

    line_score_df <-
      all_data[6] %>%
      data.frame %>%
      tbl_df

    names(line_score_df) <-
      all_headers[6] %>%
      unlist %>%
      get_header_names() %>%
      .$name.actual

    line_score_df %<>%
      mutate(date.game = game_date,
             id.game = g) %>%
      unite(team, city.team, name.team, sep = ' ') %>%
      separate(
        record.team,
        into = c('wins.team', 'losses.team'),
        sep = '\\-',
        remove = F
      )

    line_score_df %<>%
      mutate_each_(funs(as.numeric(.)),
                   line_score_df %>% dplyr::select(-c(team, date.game, slug.team, record.team)) %>% names)

    line_score_df %<>%
      gather(item, value, -c(date.game:losses.team)) %>%
      dplyr::filter(value > 0) %>%
      spread(item, value) %>%
      dplyr::rename(pts.team = pts) %>%
      arrange(desc(pts.team))
    return(line_score_df)
  }

get_game_player_box_score_percentage_df <-
  function(game_id = 21500837,
           time_period = "All",
           return_message = T) {
    t <-
      'Scoring'
    p <-
      time_period
    g <-
      game_id

    game_date <-
      get_game_team_win_probability_df(
        game_id = g,
        return_message = F,
        time_period = p
      ) %>%
      .$date.game %>%
      unique

    json_data <-
      get_game_id_box_score_data(
        game_id = g,
        box_score_table = t,
        time_period = p
      )

    tables_names <-
      json_data$resultSets$name

    headers_df <-
      get_headers()

    headers <-
      json_data$resultSets$headers[[1]]

    data <-
      json_data$resultSets$rowSet[[1]] %>%
      data.frame %>%
      tbl_df

    actual_names <-
      headers %>%
      get_header_names %>%
      .$name.actual

    names(data) <-
      actual_names

    data %<>%
      separate(min, into = c('min', 'sec'), sep = ':') %>%
      mutate(
        min = min %>% as.numeric,
        sec = sec %>% as.numeric / 60,
        min = min + sec
      ) %>%
      dplyr::select(-sec) %>%
      mutate_each_(funs(as.numeric),
                   vars = data %>% dplyr::select(
                     -c(
                       city.team,
                       slug.team,
                       name.player,
                       comment,
                       id.position.start
                     )
                   ) %>% names)

    data %<>%
      mutate(date.game = game_date) %>%
      dplyr::select(date.game, everything())
    if (return_message == T) {
      "You got player " %>%
        paste0(
          t,
          " data for game ",
          game_id,
          ' ',
          data$slug.team %>% unique %>% .[1],
          ' vs. ',
          data$slug.team %>% unique %>% .[2]
        ) %>%
        message
    }
    return(data)
  }

get_game_team_box_score_percentage_df <-
  function(game_id = 21500837,
           time_period = "All",
           return_message = T) {
    t <-
      'Scoring'
    p <-
      time_period
    g <-
      game_id

    game_date <-
      get_game_team_win_probability_df(
        game_id = g,
        return_message = F,
        time_period = p
      ) %>%
      .$date.game %>%
      unique

    json_data <-
      get_game_id_box_score_data(
        game_id = g,
        box_score_table = t,
        time_period = p
      )

    tables_names <-
      json_data$resultSets$name

    headers_df <-
      get_headers()

    headers <-
      json_data$resultSets$headers[[2]]

    data <-
      json_data$resultSets$rowSet[[2]] %>%
      data.frame %>%
      tbl_df

    actual_names <-
      headers %>%
      get_header_names %>%
      .$name.actual

    names(data) <-
      actual_names

    data %<>%
      separate(min, into = c('min', 'sec'), sep = ':') %>%
      mutate(
        min = min %>% as.numeric,
        sec = sec %>% as.numeric / 60,
        min = min + sec
      ) %>%
      unite(team, city.team, team, sep = ' ') %>%
      dplyr::select(-sec)

    data %<>%
      mutate_each_(funs(as.numeric),
                   vars = data %>% dplyr::select(-c(team,
                                                    slug.team)) %>% names)
    data %<>%
      mutate(date.game = game_date) %>%
      dplyr::select(date.game, everything())
    if (return_message == T) {
      "You got team " %>%
        paste0(
          t,
          " data for game ",
          game_id,
          ' ',
          data$slug.team %>% unique %>% .[1],
          ' vs. ',
          data$slug.team %>% unique %>% .[2]
        ) %>%
        message
    }
    return(data)
  }

get_game_traditional_box_score_data <-
  function(game_id = 21500836,
           time_period = "All") {
    t <-
      'Traditional'
    p <-
      time_period
    g <-
      game_id
    json_data <-
      get_game_id_box_score_data(
        game_id = g,
        box_score_table = t,
        time_period = p
      )

    game_date <-
      get_game_team_win_probability_df(
        game_id = g,
        return_message = F,
        time_period = p
      ) %>%
      .$date.game %>%
      unique

    tables_names <-
      json_data$resultSets$name

    headers_df <-
      get_headers()

    all_headers <-
      json_data$resultSets$headers
    all_table_names <-
      json_data$resultSets$name

    all_data <-
      json_data$resultSets$rowSet

    all_table_data <-
      data_frame()

    for (x in 1:(all_data %>% length)) {
      df <-
        all_data[x] %>%
        data.frame %>%
        tbl_df

      names(df) <-
        all_headers[x] %>%
        unlist %>%
        get_header_names() %>%
        .$name.actual

      if (x == 3) {
        df %<>%
          unite(team, city.team, team, sep = ' ') %>%
          mutate_each(funs(as.numeric), contains("id.")) %>%
          separate(min, into = c('min', 'sec'), sep = '\\:') %>%
          mutate(
            min = min %>% as.numeric,
            sec = sec %>% as.numeric,
            min = min + (sec / 60)
          ) %>%
          dplyr::select(-sec) %>%
          mutate(id.table = x,
                 name.table = all_table_names[x]) %>%
          gather(
            item,
            value,
            -c(
              id.table,
              name.table,
              id.game,
              id.team,
              team,
              slug.team,
              starter_bench
            )
          ) %>%
          mutate(value = value %>% as.numeric %>% digits(3))
      }
      if (x == 2) {
        df %<>%
          mutate_each(funs(as.numeric), contains("id.")) %>%
          separate(min, into = c('min', 'sec'), sep = '\\:') %>%
          mutate(
            min = min %>% as.numeric,
            sec = sec %>% as.numeric,
            min = min + (sec / 60)
          ) %>%
          mutate(id.table = x,
                 name.table = all_table_names[x]) %>%
          unite(team, city.team, team, sep = ' ') %>%
          gather(item,
                 value,
                 -c(id.table, name.table, id.game, id.team, team, slug.team)) %>%
          mutate(value = value %>% as.numeric %>% digits(3))
      }
      if (x == 1) {
        df %<>%
          mutate_each(funs(as.numeric), contains("id.")) %>%
          separate(min, into = c('min', 'sec'), sep = '\\:') %>%
          mutate(
            min = min %>% as.numeric,
            sec = sec %>% as.numeric,
            min = min + (sec / 60)
          ) %>%
          mutate(id.table = x,
                 name.table = all_table_names[x]) %>%
          dplyr::select(-comment) %>%
          gather(
            item,
            value,
            -c(
              id.table,
              name.table,
              id.game,
              id.team,
              city.team,
              slug.team,
              name.player,
              id.player,
              id.position.start
            )
          ) %>%
          mutate(value = value %>% as.numeric %>% digits(3))
      }
      all_table_data %<>%
        bind_rows(df)
    }

    all_table_data %<>%
      mutate(date.game = game_date)

    table_order <-
      c(
        "id.table",
        "name.table",
        'date.game',
        "team",
        "id.game",
        "id.team",
        "slug.team",
        "city.team",
        "id.player",
        "name.player",
        "id.position.start",
        "starter_bench",
        "item",
        "value"
      )

    team_name_df <-
      all_table_data %>%
      dplyr::filter(!is.na(team)) %>%
      dplyr::select(team, slug.team) %>% distinct()

    all_table_data %<>%
      dplyr::select(-team) %>%
      left_join(team_name_df)

    colOrder <-
      table_order %>%
      match(names(all_table_data))

    all_table_data %<>%
      dplyr::select(colOrder)

    return(all_table_data)
  }

get_game_player_box_score_traditional_df <-
  function(game_id = 21500837,
           time_period = "All",
           return_message = T) {
    g <-
      game_id
    t <-
      time_period

    all_data <-
      get_game_traditional_box_score_data(game_id = g, time_period = t)

    data <-
      all_data %>%
      dplyr::filter(id.table == 1) %>%
      dplyr::select(-c(id.table, name.table)) %>%
      distinct() %>%
      dplyr::select(-c(id.position.start, starter_bench)) %>%
      spread(item, value) %>%
      dplyr::select(-c(sec, city.team)) %>%
      dplyr::select(
        date.game:name.player,
        min,
        pts,
        ast,
        blk,
        stl,
        tov,
        blk,
        reb,
        oreb,
        dreb,
        fouls,
        everything()
      )

    if (return_message == T) {
      "You got player box score for " %>%
        paste0(
          data$slug.team %>% unique %>% .[1],
          ' vs ',
          data$slug.team %>% unique %>% .[2],
          ' on ',
          data$date.game %>% unique
        ) %>%
        message()
    }
    return(data)
  }

get_game_team_box_score_traditional_df <-
  function(game_id = 21500837,
           time_period = "All",
           return_message = T) {
    g <-
      game_id

    t <-
      time_period

    all_data <-
      get_game_traditional_box_score_data(game_id = g, time_period = t)

    data <-
      all_data %>%
      dplyr::filter(id.table == 2) %>%
      dplyr::select(-c(id.table, name.table)) %>%
      distinct() %>%
      dplyr::select(-c(
        id.position.start,
        starter_bench,
        name.player,
        city.team,
        id.player
      )) %>%
      spread(item, value) %>%
      dplyr::select(-sec) %>%
      arrange(desc(pts)) %>%
      mutate(is.win = c(T, F)) %>%
      dplyr::select(
        date.game,
        id.game,
        id.team,
        slug.team,
        team,
        is.win,
        min,
        pts,
        ast,
        blk,
        stl,
        tov,
        blk,
        reb,
        oreb,
        dreb,
        fouls,
        everything()
      )

    if (return_message == T) {
      "You got team box score for " %>%
        paste0(
          data$slug.team %>% unique %>% .[1],
          ' vs ',
          data$slug.team %>% unique %>% .[2],
          ' on ',
          data$date.game %>% unique
        ) %>%
        message()
    }
    return(data)
  }

get_game_player_box_score_advanced_stat_df <-
  function(game_id = 21500837,
           time_period = "All",
           include_game_data = F,
           return_message = T) {
    t <-
      'Advanced'

    p <-
      time_period

    g <-
      game_id

    game_date <-
      get_game_team_win_probability_df(
        game_id = g,
        return_message = F,
        time_period = p
      ) %>%
      .$date.game %>%
      unique

    json_data <-
      get_game_id_box_score_data(
        game_id = g,
        box_score_table = t,
        time_period = p
      )

    tables_names <-
      json_data$resultSets$name

    headers_df <-
      get_headers()

    headers <-
      json_data$resultSets$headers[[1]]

    data <-
      json_data$resultSets$rowSet[[1]] %>%
      data.frame %>%
      tbl_df

    actual_names <-
      headers %>%
      get_header_names %>%
      .$name.actual

    names(data) <-
      actual_names

    data %<>%
      separate(min, into = c('min', 'sec'), sep = ':') %>%
      mutate(
        min = min %>% as.numeric,
        sec = sec %>% as.numeric / 60,
        min = min + sec
      ) %>%
      dplyr::select(-c(sec, comment))

    data %<>%
      mutate_each_(funs(as.numeric),
                   vars = data %>% dplyr::select(-c(
                     city.team,
                     name.player,
                     slug.team,
                     id.position.start
                   )) %>% names)
    data %<>%
      mutate(date.game = game_date) %>%
      dplyr::select(date.game, everything())
    if (return_message == T) {
      "You got player " %>%
        paste0(
          t,
          " data for game ",
          game_id,
          ' ',
          data$slug.team %>% unique %>% .[1],
          ' vs. ',
          data$slug.team %>% unique %>% .[2]
        ) %>%
        message
    }
    return(data)
  }

get_game_team_box_score_advanced_stat_df <-
  function(game_id = 21500837,
           time_period = "All",
           return_message = T) {
    t <-
      'Advanced'

    p <-
      time_period

    g <-
      game_id

    game_date <-
      get_game_team_win_probability_df(
        game_id = g,
        return_message = F,
        time_period = p
      ) %>%
      .$date.game %>%
      unique

    json_data <-
      get_game_id_box_score_data(
        game_id = g,
        box_score_table = t,
        time_period = p
      )

    tables_names <-
      json_data$resultSets$name

    headers_df <-
      get_headers()

    headers <-
      json_data$resultSets$headers[[2]]

    data <-
      json_data$resultSets$rowSet[[2]] %>%
      data.frame %>%
      tbl_df

    actual_names <-
      headers %>%
      get_header_names %>%
      .$name.actual

    names(data) <-
      actual_names

    data %<>%
      separate(min, into = c('min', 'sec'), sep = ':') %>%
      mutate(
        min = min %>% as.numeric,
        sec = sec %>% as.numeric / 60,
        min = min + sec
      ) %>%
      unite(team, city.team, team, sep = ' ') %>%
      dplyr::select(-sec)

    data %<>%
      mutate_each_(funs(as.numeric),
                   vars = data %>% dplyr::select(-c(team,
                                                    slug.team)) %>% names)
    data %<>%
      mutate(date.game = game_date) %>%
      dplyr::select(date.game, everything())
    if (return_message == T) {
      "You got team " %>%
        paste0(
          t,
          " data for game ",
          game_id,
          ' ',
          data$slug.team %>% unique %>% .[1],
          ' vs. ',
          data$slug.team %>% unique %>% .[2]
        ) %>%
        message
    }
    return(data)
  }

get_game_player_box_score_misc_df <-
  function(game_id = 21500837,
           time_period = "All",
           return_message = T) {
    t <-
      'Misc'

    p <-
      time_period

    g <-
      game_id

    game_date <-
      get_game_team_win_probability_df(
        game_id = g,
        return_message = F,
        time_period = p
      ) %>%
      .$date.game %>%
      unique

    json_data <-
      get_game_id_box_score_data(
        game_id = g,
        box_score_table = t,
        time_period = p
      )

    tables_names <-
      json_data$resultSets$name

    headers_df <-
      get_headers()

    headers <-
      json_data$resultSets$headers[[1]]

    data <-
      json_data$resultSets$rowSet[[1]] %>%
      data.frame %>%
      tbl_df

    actual_names <-
      headers %>%
      get_header_names %>%
      .$name.actual

    names(data) <-
      actual_names

    data %<>%
      separate(min, into = c('min', 'sec'), sep = ':') %>%
      mutate(
        min = min %>% as.numeric,
        sec = sec %>% as.numeric / 60,
        min = min + sec
      ) %>%
      dplyr::select(-c(sec, comment))

    data %<>%
      mutate_each_(funs(as.numeric),
                   vars = data %>% dplyr::select(-c(
                     city.team,
                     name.player,
                     slug.team,
                     id.position.start
                   )) %>% names)
    data %<>%
      mutate(date.game = game_date) %>%
      dplyr::select(date.game, everything())

    if (return_message == T) {
      "You got player " %>%
        paste0(
          t,
          " data for game ",
          game_id,
          ' ',
          data$slug.team %>% unique %>% .[1],
          ' vs. ',
          data$slug.team %>% unique %>% .[2]
        ) %>%
        message
    }
    return(data)
  }

get_game_team_box_score_misc_df <-
  function(game_id = 21500837,
           time_period = "All",
           include_game_data = F,
           return_message = T) {
    t <-
      'Misc'

    p <-
      time_period

    g <-
      game_id

    game_date <-
      get_game_team_win_probability_df(
        game_id = g,
        return_message = F,
        time_period = p
      ) %>%
      .$date.game %>%
      unique

    json_data <-
      get_game_id_box_score_data(
        game_id = g,
        box_score_table = t,
        time_period = p
      )

    tables_names <-
      json_data$resultSets$name

    headers_df <-
      get_headers()

    headers <-
      json_data$resultSets$headers[[2]]

    data <-
      json_data$resultSets$rowSet[[2]] %>%
      data.frame %>%
      tbl_df

    actual_names <-
      headers %>%
      get_header_names %>%
      .$name.actual

    names(data) <-
      actual_names

    data %<>%
      separate(min, into = c('min', 'sec'), sep = ':') %>%
      mutate(
        min = min %>% as.numeric,
        sec = sec %>% as.numeric / 60,
        min = min + sec
      ) %>%
      unite(team, city.team, team, sep = ' ') %>%
      dplyr::select(-sec)

    data %<>%
      mutate_each_(funs(as.numeric),
                   vars = data %>% dplyr::select(-c(team,
                                                    slug.team)) %>% names)
    data %<>%
      mutate(date.game = game_date) %>%
      dplyr::select(date.game, everything())
    if (return_message == T) {
      "You got team " %>%
        paste0(
          t,
          " data for game ",
          game_id,
          ' ',
          data$slug.team %>% unique %>% .[1],
          ' vs. ',
          data$slug.team %>% unique %>% .[2]
        ) %>%
        message
    }
    return(data)
  }

get_game_player_box_score_usage_df <-
  function(game_id = 21500837,
           time_period = "All",
           return_message = T) {
    t <-
      'Usage'

    p <-
      time_period

    g <-
      game_id

    game_date <-
      get_game_team_win_probability_df(
        game_id = g,
        return_message = F,
        time_period = p
      ) %>%
      .$date.game %>%
      unique

    json_data <-
      get_game_id_box_score_data(
        game_id = g,
        box_score_table = t,
        time_period = p
      )

    tables_names <-
      json_data$resultSets$name

    headers_df <-
      get_headers()

    headers <-
      json_data$resultSets$headers[[1]]

    data <-
      json_data$resultSets$rowSet[[1]] %>%
      data.frame %>%
      tbl_df

    actual_names <-
      headers %>%
      get_header_names %>%
      .$name.actual

    names(data) <-
      actual_names

    data %<>%
      separate(min, into = c('min', 'sec'), sep = ':') %>%
      mutate(
        min = min %>% as.numeric,
        sec = sec %>% as.numeric / 60,
        min = min + sec
      ) %>%
      dplyr::select(-c(sec, comment))

    data %<>%
      mutate_each_(funs(as.numeric),
                   vars = data %>% dplyr::select(-c(
                     city.team,
                     name.player,
                     slug.team,
                     id.position.start
                   )) %>% names)
    data %<>%
      mutate(date.game = game_date) %>%
      dplyr::select(date.game, everything())

    names(data)[11:length(names(data))] %<>%
      paste0(., '.team.on_court')

    if (return_message == T) {
      "You got player " %>%
        paste0(
          t,
          " data for game ",
          game_id,
          ' ',
          data$slug.team %>% unique %>% .[1],
          ' vs. ',
          data$slug.team %>% unique %>% .[2]
        ) %>%
        message
    }
    return(data)
  }

get_game_player_box_score_four_factor_df <-
  function(game_id = 21500837,
           time_period = "All",
           return_message = T) {
    t <-
      'Four Factors'

    p <-
      time_period

    g <-
      game_id

    game_date <-
      get_game_team_win_probability_df(
        game_id = g,
        return_message = F,
        time_period = p
      ) %>%
      .$date.game %>%
      unique

    json_data <-
      get_game_id_box_score_data(
        game_id = g,
        box_score_table = t,
        time_period = p
      )

    tables_names <-
      json_data$resultSets$name

    headers_df <-
      get_headers()

    headers <-
      json_data$resultSets$headers[[1]]

    data <-
      json_data$resultSets$rowSet[[1]] %>%
      data.frame %>%
      tbl_df

    actual_names <-
      headers %>%
      get_header_names %>%
      .$name.actual

    names(data) <-
      actual_names

    data %<>%
      separate(min, into = c('min', 'sec'), sep = ':') %>%
      mutate(
        min = min %>% as.numeric,
        sec = sec %>% as.numeric / 60,
        min = min + sec
      ) %>%
      dplyr::select(-c(sec, comment))

    data %<>%
      mutate_each_(funs(as.numeric),
                   vars = data %>% dplyr::select(-c(
                     city.team,
                     name.player,
                     slug.team,
                     id.position.start
                   )) %>% names)
    data %<>%
      mutate(date.game = game_date) %>%
      dplyr::select(date.game, everything())

    if (return_message == T) {
      "You got player " %>%
        paste0(
          t,
          " data for game ",
          game_id,
          ' ',
          data$slug.team %>% unique %>% .[1],
          ' vs. ',
          data$slug.team %>% unique %>% .[2]
        ) %>%
        message
    }
    return(data)
  }

get_game_team_box_score_four_factor_df <-
  function(game_id = 21500837,
           time_period = "All",
           include_game_data = F,
           return_message = T) {
    t <-
      'Four Factors'

    p <-
      time_period

    g <-
      game_id

    game_date <-
      get_game_team_win_probability_df(
        game_id = g,
        return_message = F,
        time_period = p
      ) %>%
      .$date.game %>%
      unique

    json_data <-
      get_game_id_box_score_data(
        game_id = g,
        box_score_table = t,
        time_period = p
      )

    tables_names <-
      json_data$resultSets$name

    headers_df <-
      get_headers()

    headers <-
      json_data$resultSets$headers[[2]]

    data <-
      json_data$resultSets$rowSet[[2]] %>%
      data.frame %>%
      tbl_df

    actual_names <-
      headers %>%
      get_header_names %>%
      .$name.actual

    names(data) <-
      actual_names

    data %<>%
      separate(min, into = c('min', 'sec'), sep = ':') %>%
      mutate(
        min = min %>% as.numeric,
        sec = sec %>% as.numeric / 60,
        min = min + sec
      ) %>%
      unite(team, city.team, team, sep = ' ') %>%
      dplyr::select(-sec)

    data %<>%
      mutate_each_(funs(as.numeric),
                   vars = data %>% dplyr::select(-c(team,
                                                    slug.team)) %>% names)
    data %<>%
      mutate(date.game = game_date) %>%
      dplyr::select(date.game, everything())
    if (return_message == T) {
      "You got team " %>%
        paste0(
          t,
          " data for game ",
          game_id,
          ' ',
          data$slug.team %>% unique %>% .[1],
          ' vs. ',
          data$slug.team %>% unique %>% .[2]
        ) %>%
        message
    }
    return(data)
  }

get_game_player_box_score_player_tracking_df <-
  function(game_id = 21500837,
           time_period = "All",
           return_message = T) {
    t <-
      'Player Tracking'
    p <-
      time_period

    g <-
      game_id

    game_date <-
      get_game_team_win_probability_df(
        game_id = g,
        return_message = F,
        time_period = p
      ) %>%
      .$date.game %>%
      unique

    json_data <-
      get_game_id_box_score_data(
        game_id = g,
        box_score_table = t,
        time_period = p
      )

    tables_names <-
      json_data$resultSets$name

    headers_df <-
      get_headers()

    headers <-
      json_data$resultSets$headers[[1]]

    data <-
      json_data$resultSets$rowSet[[1]] %>%
      data.frame %>%
      tbl_df

    actual_names <-
      headers %>%
      get_header_names %>%
      .$name.actual

    names(data) <-
      actual_names

    data %<>%
      separate(min, into = c('min', 'sec'), sep = ':') %>%
      mutate(
        min = min %>% as.numeric,
        sec = sec %>% as.numeric / 60,
        min = min + sec
      ) %>%
      dplyr::select(-c(sec))

    data %<>%
      mutate_each_(funs(as.numeric),
                   vars = data %>% dplyr::select(-c(
                     city.team,
                     name.player,
                     slug.team,
                     id.position.start
                   )) %>% names)
    data %<>%
      mutate(date.game = game_date) %>%
      dplyr::select(date.game, everything())

    if (return_message == T) {
      "You got player " %>%
        paste0(
          t,
          " data for game ",
          game_id,
          ' ',
          data$slug.team %>% unique %>% .[1],
          ' vs. ',
          data$slug.team %>% unique %>% .[2]
        ) %>%
        message
    }
    return(data)
  }

get_game_team_box_score_player_tracking_df <-
  function(game_id = 21500837,
           time_period = "All",
           include_game_data = F,
           return_message = T) {
    t <-
      'Player Tracking'

    p <-
      time_period

    g <-
      game_id

    game_date <-
      get_game_team_win_probability_df(
        game_id = g,
        return_message = F,
        time_period = p
      ) %>%
      .$date.game %>%
      unique

    json_data <-
      get_game_id_box_score_data(
        game_id = g,
        box_score_table = t,
        time_period = p
      )

    tables_names <-
      json_data$resultSets$name

    headers_df <-
      get_headers()

    headers <-
      json_data$resultSets$headers[[2]]

    data <-
      json_data$resultSets$rowSet[[2]] %>%
      data.frame %>%
      tbl_df

    actual_names <-
      headers %>%
      get_header_names %>%
      .$name.actual

    names(data) <-
      actual_names

    data %<>%
      separate(min, into = c('min', 'sec'), sep = ':') %>%
      mutate(
        min = min %>% as.numeric,
        sec = sec %>% as.numeric / 60,
        min = min + sec
      ) %>%
      unite(team, city.team, name.team, sep = ' ') %>%
      dplyr::select(-sec)

    data %<>%
      mutate_each_(funs(as.numeric),
                   vars = data %>% dplyr::select(-c(team,
                                                    slug.team)) %>% names)
    data %<>%
      mutate(date.game = game_date) %>%
      dplyr::select(date.game, everything())
    if (return_message == T) {
      "You got team " %>%
        paste0(
          t,
          " data for game ",
          game_id,
          ' ',
          data$slug.team %>% unique %>% .[1],
          ' vs. ',
          data$slug.team %>% unique %>% .[2]
        ) %>%
        message
    }
    return(data)
  }

get_game_play_by_play_df <-
  function(game_id = 21500839,
           time_period = "All",
           include_game_data = F,
           return_message = T) {
    t <-
      'Play by Play'
    p <-
      time_period
    g <-
      game_id

    game_date <-
      get_game_team_win_probability_df(
        game_id = g,
        return_message = F,
        time_period = p
      ) %>%
      .$date.game %>%
      unique

    json_data <-
      get_game_id_box_score_data(
        game_id = g,
        box_score_table = t,
        time_period = p
      )

    tables_names <-
      json_data$resultSets$name

    headers_df <-
      get_headers()

    pbp_df <-
      json_data$resultSets$rowSet[[1]] %>%
      data.frame %>%
      tbl_df

    names(pbp_df) <-
      json_data$resultSets$headers[1] %>%
      unlist %>%
      get_header_names %>%
      .$name.actual

    pbp_df %<>%
      mutate(date.game = game_date)

    pbp_df %<>%
      mutate_each_(funs(as.numeric), pbp_df %>%
                     dplyr::select(c(
                       period,
                       contains("type."),
                       contains("id"),
                       contains("score.")
                     )) %>% names) %>%
      dplyr::select(date.game, everything())

    pbp_df %<>%
      separate(minute.quarter,
               sep = ':',
               into = c('min', 'sec')) %>%
      mutate_each_(funs(as.numeric), c('min', 'sec')) %>%
      mutate(
        game.min.sec = 720 - ((min * 60) + sec),
        period.min.sec = ((period - 1) * 720),
        game.min = (game.min.sec + period.min.sec) / 60 %>% digits(2)
      ) %>%
      dplyr::select(-c(game.min.sec, period.min.sec)) %>%
      dplyr::select(date.game:time, game.min, everything())

    pbp_df %<>%
      separate(score,
               into = c('pts.away', 'pts.home'),
               remove = F) %>%
      mutate_each(funs(as.numeric(.)), contains("pts."))


    if (return_message == T) {
      "You got win probability " %>%
        paste0(
          t,
          " data for game ",
          game_id,
          ' ',
          pbp_df %>% dplyr::filter(!is.na(slug.team.player.1)) %>% .$slug.team.player.1 %>% unique %>% .[1],
          ' vs. ',
          pbp_df %>% dplyr::filter(!is.na(slug.team.player.1)) %>% .$slug.team.player.1 %>% unique %>% .[2]
        ) %>%
        message
    }
    return(pbp_df)
  }

get_game_box_score_game_chart_df <-
  function(game_id = 21500839,
           time_period = "All",
           include_game_data = F,
           return_message = T) {
    t <-
      'Game Charts'
    p <-
      time_period
    g <-
      game_id

    game_date <-
      get_game_team_win_probability_df(
        game_id = g,
        return_message = F,
        time_period = p
      ) %>%
      .$date.game %>%
      unique

    json_data <-
      get_game_id_box_score_data(
        game_id = g,
        box_score_table = t,
        time_period = p
      )

    tables_names <-
      json_data$resultSets$name

    headers_df <-
      get_headers()

    data <-
      json_data$resultSets$rowSet[[1]] %>%
      data.frame %>%
      tbl_df

    names(data) <-
      json_data$resultSets$headers[1] %>%
      unlist %>%
      get_header_names %>%
      .$name.actual

    data %<>%
      mutate(date.game = game_date)

    data %<>%
      mutate_each_(funs(as.numeric),
                   data %>% dplyr::select(
                     -c(date.game, slug.team, team, name.player, position, location)
                   ) %>% names) %>%
      dplyr::select(date.game,
                    slug.team,
                    team,
                    name.player,
                    position,
                    location,
                    everything())


    data %<>%
      left_join(
        data %>%
          group_by(slug.team) %>%
          summarise(pts.team = sum(pts)) %>%
          arrange(desc(pts.team)) %>%
          mutate(is.win = c(T, F))
      ) %>%
      mutate(is.home_game = ifelse(location == "Home", T, F)) %>%
      dplyr::select(date.game:location, is.win, is.home_game, everything()) %>%
      dplyr::select(-c(pts.team, location))

    if (return_message == T) {
      "You got player " %>%
        paste0(
          t,
          " data for game ",
          game_id,
          ' ',
          data$slug.team %>% unique %>% .[1],
          ' vs. ',
          data$slug.team %>% unique %>% .[2]
        ) %>%
        message
    }

    return(data)
  }

get_game_player_box_score_traditional_df_safe <-
  failwith(NULL, get_game_player_box_score_traditional_df)

get_game_player_box_score_advanced_stat_df_safe <-
  failwith(NULL, get_game_player_box_score_advanced_stat_df)

get_game_player_box_score_player_tracking_df_safe <-
  failwith(NULL, get_game_player_box_score_player_tracking_df)

get_game_player_box_score_misc_df_safe <-
  failwith(NULL, get_game_player_box_score_misc_df)

get_game_player_box_score_percentage_df_safe <-
  failwith(NULL, get_game_player_box_score_percentage_df)
get_game_player_box_score_usage_df_safe <-
  failwith(NULL, get_game_player_box_score_usage_df)

get_game_player_boxscore_table_df <-
  function(game_id = 21500837,
           time_period = "All",
           return_wide = T,
           return_message = T) {
    p <-
      time_period
    g <-
      game_id

    traditional_df <-
      get_game_player_box_score_traditional_df_safe(
        game_id = g,
        time_period = p,
        return_message = F
      ) %>%
      suppressMessages()

    advanced_df <-
      get_game_player_box_score_advanced_stat_df_safe(
        game_id = g,
        time_period = p,
        return_message = F
      )

    all_data <-
      traditional_df %>%
      left_join(advanced_df %>% dplyr::select(-min)) %>%
      suppressMessages()

    tracking_df <-
      get_game_player_box_score_player_tracking_df_safe(
        game_id = g,
        time_period = p,
        return_message = F
      ) %>%
      suppressMessages()

    all_data %<>%
      left_join(tracking_df %>%
                  dplyr::select(-c(comment, min))) %>%
      suppressMessages()

    misc_df <-
      get_game_player_box_score_misc_df_safe(
        game_id = g,
        time_period = p,
        return_message = F
      )

    all_data %<>%
      left_join(misc_df %>% dplyr::select(-min)) %>%
      suppressMessages()

    usage_df <-
      get_game_player_box_score_usage_df_safe(
        game_id = g,
        time_period = p,
        return_message = F
      )

    all_data %<>%
      left_join(usage_df %>%
                  dplyr::select(-c(min))) %>%
      suppressMessages()

    score_pct_df <-
      get_game_player_box_score_percentage_df_safe(
        game_id = g,
        time_period = p,
        return_message = F
      )

    all_data %<>%
      left_join(score_pct_df %>%
                  dplyr::select(-c(min))) %>%
      suppressMessages()

    all_data %<>%
      mutate(is.starter = ifelse(!id.position.start == '', T, F)) %>%
      dplyr::select(-c(city.team, comment, id.position.start)) %>%
      dplyr::select(date.game:name.player, is.starter, everything()) %>%
      arrange(slug.team, desc(is.starter))
    if (return_wide == F) {
      all_data %<>%
        gather(metric, value, -c(date.game:is.starter))
    }
    if (return_message == T) {
      "You got all player boxscore data for " %>%
        paste0(
          all_data$slug.team %>% unique %>% .[1],
          " vs. ",
          all_data$slug.team %>% unique %>% .[2],
          ' on ',
          all_data$date.game %>% unique
        ) %>%
        message()
    }
    return(all_data)
  }
get_game_team_box_score_traditional_df_safe <-
  failwith(NULL, get_game_team_box_score_traditional_df)

get_game_team_box_score_advanced_stat_df_safe <-
  failwith(NULL, get_game_team_box_score_advanced_stat_df)

get_game_team_box_score_player_tracking_df_safe <-
  failwith(NULL, get_game_team_box_score_player_tracking_df)

get_game_team_box_score_misc_df_safe <-
  failwith(NULL, get_game_team_box_score_misc_df)

get_game_team_box_score_percentage_df_safe <-
  failwith(NULL, get_game_team_box_score_percentage_df)

get_game_team_boxscore_table_df <-
  function(game_id = 21500837,
           time_period = "All",
           return_wide = T,
           return_message = T) {
    p <-
      time_period
    g <-
      game_id

    traditional_df <-
      get_game_team_box_score_traditional_df_safe(
        game_id = g,
        time_period = p,
        return_message = F
      ) %>%
      suppressMessages()

    advanced_df <-
      get_game_team_box_score_advanced_stat_df_safe(
        game_id = g,
        time_period = p,
        return_message = F
      )

    all_data <-
      traditional_df %>%
      left_join(advanced_df %>% dplyr::select(-min)) %>%
      suppressMessages()

    tracking_df <-
      get_game_team_box_score_player_tracking_df_safe(
        game_id = g,
        time_period = p,
        return_message = F
      ) %>%
      suppressMessages()

    all_data %<>%
      left_join(tracking_df %>%
                  dplyr::select(-c(min))) %>%
      suppressMessages()

    misc_df <-
      get_game_team_box_score_misc_df_safe(
        game_id = g,
        time_period = p,
        return_message = F
      )

    all_data %<>%
      left_join(misc_df %>% dplyr::select(-min)) %>%
      suppressMessages()

    score_pct_df <-
      get_game_team_box_score_percentage_df_safe(
        game_id = g,
        time_period = p,
        return_message = F
      )

    all_data %<>%
      left_join(score_pct_df %>%
                  dplyr::select(-c(min))) %>%
      suppressMessages()

    if (return_wide == F) {
      all_data %<>%
        gather(metric, value, -c(date.game:is.win))
    }

    if (return_message == T) {
      "You got all team boxscore data for " %>%
        paste0(
          all_data$slug.team %>% unique %>% .[1],
          " vs. ",
          all_data$slug.team %>% unique %>% .[2],
          ' on ',
          all_data$date.game %>% unique
        ) %>%
        message()
    }

    return(all_data)
  }

get_game_player_boxscore_table_df_safe <-
  failwith(NULL, get_game_player_boxscore_table_df)

get_games_player_boxscore_table_df <-
  function(game_ids,
           period = 'All',
           wide = T,
           message = T) {
    if (!'game_ids' %>% exists) {
      stop("Please enter game ids")
    }
    all_player_stats <-
      game_ids %>%
      unique %>%
      map(function(x) {
        get_game_player_boxscore_table_df_safe(
          game_id = x,
          time_period = period,
          return_wide = wide,
          return_message = message
        )
      }) %>%
      compact %>%
      bind_rows

    if (message == T) {
      "You got " %>%
        paste0(all_player_stats$id.game %>% unique %>% length,
               ' player game box score tables') %>%
        message()
    }
    return(all_player_stats)
  }


get_game_team_boxscore_table_df_safe <-
  failwith(NULL, get_game_team_boxscore_table_df)

get_games_team_boxscore_table_dfs <-
  function(game_ids,
           period = 'All',
           wide = T,
           message = T) {
    if (!'game_ids' %>% exists) {
      stop("Please enter game ids")
    }
    all_team_tables <-
      game_ids %>%
      unique %>%
      map(function(x) {
        get_game_team_boxscore_table_df_safe(
          game_id = x,
          time_period = period,
          return_wide = wide,
          return_message = message
        )
      }) %>%
      compact %>%
      bind_rows

    if (message == T) {
      "You got " %>%
        paste0(all_team_tables$id.game %>% unique %>% length,
               ' team game bosx score tables') %>%
        message()
    }
    return(all_team_tables)
  }
