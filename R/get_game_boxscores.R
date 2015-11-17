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
        "PLAYER3_TEAM_NICKNAME", "PLAYER3_TEAM_ABBREVIATION",
        "SPD", "DIST", "ORBC", "DRBC", "RBC", "TCHS", "SAST", "FTAST",
        "PASS", "CFGM", "CFGA", "CFG_PCT", "UFGM", "UFGA", "UFG_PCT",
        "DFGM", "DFGA", "DFG_PCT","TEAM_NICKNAME",'FAN_DUEL_PTS',
        "JERSEY_NUM", "PLAYER_POSITION", "LOCATION",
        "EVENT_NUM", "HOME_PCT", "VISITOR_PCT", "HOME_PTS", "VISITOR_PTS",
        "HOME_SCORE_MARGIN", "SECONDS_REMAINING", "HOME_POSS_IND", "HOME_G",
        "DESCRIPTION", "ISVISIBLE",
        "HOME_TEAM_ID", "HOME_TEAM_ABR", "HOME_TEAM_PTS", "VISITOR_TEAM_ID",
        "VISITOR_TEAM_ABR", "VISITOR_TEAM_PTS",
          "GAME_DATE_EST", "GAME_SEQUENCE", "GAME_STATUS_ID", "GAME_STATUS_TEXT",
          "GAMECODE", "SEASON", "LIVE_PERIOD", "LIVE_PC_TIME", "NATL_TV_BROADCASTER_ABBREVIATION",
          "LIVE_PERIOD_TIME_BCAST", "WH_STATUS", "LARGEST_LEAD", "LEAD_CHANGES",
          "TIMES_TIED", "ids.officials", "officials", "jerseys.official",
          "id.team", "players.inactive", "id.players.inactive", "ATTENDANCE",
          "GAME_TIME", "TEAM_CITY_NAME", "TEAM_WINS_LOSSES", "PTS_QTR1",
          "PTS_QTR2", "PTS_QTR3", "PTS_QTR4", "PTS_OT1", "PTS_OT2", "PTS_OT3",
          "PTS_OT4", "PTS_OT5", "PTS_OT6", "PTS_OT7", "PTS_OT8", "PTS_OT9",
          "PTS_OT10", "LAST_GAME_ID", "LAST_GAME_DATE_EST", "LAST_GAME_HOME_TEAM_ID",
          "LAST_GAME_HOME_TEAM_CITY", "LAST_GAME_HOME_TEAM_NAME", "LAST_GAME_HOME_TEAM_ABBREVIATION",
          "LAST_GAME_HOME_TEAM_POINTS", "LAST_GAME_VISITOR_TEAM_ID", "LAST_GAME_VISITOR_TEAM_CITY",
          "LAST_GAME_VISITOR_TEAM_NAME", "LAST_GAME_VISITOR_TEAM_CITY1",
          "LAST_GAME_VISITOR_TEAM_POINTS", "HOME_TEAM_WINS", "HOME_TEAM_LOSSES",
          "SERIES_LEADER", "VIDEO_AVAILABLE_FLAG", "PT_AVAILABLE", "PT_XYZ_AVAILABLE"

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
        "rate.fta", "pct.efg.opponent", "rate.fta.opponent", "rate.tov.opponent", "pct.oreb.opponent",
        "id.event", "id.type.type", "id.action.type", "period",
        "time", "minute.quarter", "play.home.description", "play.neutral.description",
        "play.visitor.description", "score", "score.margin", "type.player.1",
        "id.player.1", "name.player.1", "id.team.player.1", "city.team.player.1",
        "name.team.player.1", "slug.team.player.1", "type.player.2",
        "id.player.2", "name.player.2", "id.team.player.2", "city.team.player.2",
        "name.team.player.2", "slug.team.player.2","type.player.3",
        "id.player.3", "name.player.3", "id.team.player.3", "city.team.player.3",
        "name.team.player.3", "slug.team.player.3",
        "avg.mph", "distance.miles", "oreb.chances", "dreb.chances", "reb.chances",
        "touches", "ast.secondary", "ast.to.fta",
        "passes", "fgm.contested", "fga.contested", "pct.fg.contested", "fgm.uncontested", "fga.uncontested", "pct.fg.uncontested",
        "fgm.opponent.rim_defense", "fga.opponent.rim_defense", "pct.fgm.opponent.rim_defense",
        "name.team",'points.fanduel',"jersey", "position", "location",
        "id.event", "pct.win_prob.home", "pct.win_prob.away", "points.home", "points.away",
        "score.margin.home", "quarter.seconds_remaining", "is.home.possesion", "home.g",
        "play", "is.visible",
        "id.team.home", "slug.team.home", "points.team.home", "id.team.away",
        "slug.team.away", "points.team.away",
        "date.game", "sequence.game", "id.stats.game", "text.status.game",
        "slug.game", "year.season_start", "period.live", "time.live.pc", "network.tv",
        "period.live.broadcast", "status.wh", "lead.largest", "lead.changes",
        "times.tied", "ids.officials", "officials", "jerseys.official",
        "id.team", "players.inactive", "id.players.inactive", "attendance",
        "time.game", "city.team", "record.team", "points.q1",
        "points.q2", "points.q3", "points.q4", "points.ot.1", "points.ot.2", "points.ot.3",
        "points.ot.4", "points.ot.5", "points.ot.6", "points.ot.7", "points.ot.8", "points.ot.9",
        "points.ot.10", "id.game.last", "date.game.last", "id.team.home.last",
        "city.team.home.last", "name.team.home.last", "slug.team.home.last",
        "points.team.home.last",
        "id.team.away.last",
        "city.team.away.last", "name.team.away.last", "slug.team.away.last",
        "points.team.away.last","wins.team.home", "losses.team.home",
        "team.series_leader", "is.video_available", "is.pt.available", "is.pt.xyz.available"
      ),
      id.row = 1:length(name.actual)
    )
  return(headers_df)
}
get_header_names <- function(headers){
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
get_boxscore_url <-
  function(game_id,
           box_score_table,
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

    if (box_score_table == "Win Probability"){
      url_json <-
        paste0('http://stats.nba.com/stats/winprobabilitypbp?GameID=',
               game_id, '&RunType=each+second')
    }
    return(url_json)
  }
get_game_id_box_score_data <-
  function(game_id,
           box_score_table = "Scoring",
           time_period = "All",
           include_team = F,
           include_bench_starter = F,
           return_wide = T,
           include_game_data = F,
           return_message = T,
           ...) {
    t <-
      box_score_table
    p <-
      time_period
    g <-
      game_id
    url_json <-
      get_boxscore_url(game_id  = g, box_score_table = t, time_period = p)

    json_data <-
      url_json %>%
      fromJSON(simplifyDataFrame = T, flatten = T)

    tables_names <-
      json_data$resultSets$name

    headers_df <-
      get_headers()

    if (t == "Summary") {
      all_summary_data <-
        data_frame()
      for (i in 1:length(tables_names)){
        if (tables_names[i] == "Officials"){
          js_data <-
            json_data$resultSets$rowSet[i] %>%
            data.frame() %>%
            tbl_df
          headers <-
            json_data$resultSets$headers[i] %>% unlist

          names(js_data) <-
            headers

          js_data %<>%
            mutate(official = FIRST_NAME %>% paste(LAST_NAME)) %>%
            dplyr::select(official, jersey = JERSEY_NUM, id = OFFICIAL_ID)

          js_data <-
            data_frame(ids.officials = js_data$id %>% paste(collapse = ', '),
                     officials = js_data$official %>% paste(collapse = ', '),
                     jerseys.official = js_data$jersey %>% paste(collapse = ', ')
                     )

        }

        if(tables_names[i] == "InactivePlayers"){
          js_data <-
            json_data$resultSets$rowSet[i] %>%
            data.frame() %>%
            tbl_df

          headers <-
            json_data$resultSets$headers[i] %>% unlist

          names(js_data) <-
            headers

          js_data %<>%
            mutate(name.player = FIRST_NAME %>% paste(LAST_NAME),
                   team = TEAM_CITY %>% paste(TEAM_NAME)) %>%
            dplyr::select(id.player = PLAYER_ID, name.player, jersey = JERSEY_NUM,
                          id.team = TEAM_ID, team
                          )

          js_data %<>%
            group_by(id.team) %>%
            summarise(players.inactive = name.player %>% paste(collapse = ', '),
                      id.players.inactive = id.player %>% paste(collapse = ', ')) %>%
            mutate(id.game = game_id) %>%
            ungroup()
        }

        if(tables_names[i] == 'GameInfo'){
          headers <-
            json_data$resultSets$headers[i] %>% unlist

          js_data <-
            json_data$resultSets$rowSet[i] %>%
            data.frame() %>%
            tbl_df

          actual_names <-
            headers %>%
            get_header_names %>%
            .$name.actual

          names(js_data) <-
            actual_names

          js_data %<>%
            dplyr::select(-date.game)
        }

        if(!tables_names[i] %in% c('Officials','InactivePlayers','GameInfo')){
          headers <-
          json_data$resultSets$headers[i] %>% unlist

        js_data <-
          json_data$resultSets$rowSet[i] %>%
          data.frame() %>%
          tbl_df

        actual_names <-
          headers %>%
          get_header_names %>%
          .$name.actual

        names(js_data) <-
          actual_names
        }
        if(!"GAME_ID" %in% names(js_data)){
          js_data %<>%
            mutate(id.game = game_id)
        }
        if (i == 1) {
          all_summary_data %<>%
          bind_rows(js_data)
        } else {
          all_summary_data %<>%
            left_join(js_data)
        }

      }
      all_summary_data %<>%
        dplyr::rename(is.final = text.status.game,
                      points.team = pts) %>%
        mutate(team = city.team %>% paste(name.team),
               is.home = ifelse(id.team == id.team.home, T,F),
               date.game = date.game %>% substr(1, 10) %>% lubridate::ymd %>% as.Date(),
               is.final = ifelse(is.final == "Final", T, F)) %>%
        dplyr::select(is.home, team, date.game:points.team) %>%
        dplyr::select(-c(period.live:id.league, id.stats.game, city.team, name.team, id.team.home, id.team.away,slug.game, sequence.game))

      if (return_wide == T){
        summary_wide <-
          all_summary_data %>%
          gather(item, value, -c(is.home, date.game, id.game, is.final, year.season_start,times.tied:jerseys.official, attendance, time.game)) %>%
          mutate(item = item %>% as.character()) %>%
          mutate(item = ifelse(is.home == T, item %>% paste0('.home'), item %>% paste0('.away'))) %>%
          dplyr::select(-is.home) %>%
          spread(item, value) %>%
          dplyr::select(date.game, id.game, team.home, record.team.home, slug.team.home, team.away, record.team.away, slug.team.away, everything()) %>%
          separate(record.team.home, into = c('wins.team.home', 'losses.team.home'), sep = '\\-') %>%
          separate(record.team.away, into = c('wins.team.away', 'losses.team.away'), sep = '\\-')

        data <-
          summary_wide %>%
          mutate_each_(funs(as.numeric(.)),vars =
                         summary_wide %>% dplyr::select(wins.team.home, losses.team.home ,wins.team.away,losses.team.away,
                                                        year.season_start:times.tied,attendance,
                                                        lead.largest.away, lead.largest.home,
                                                        points.ot.1.away:pts.paint.home,id.team.home, id.team.away
                         ) %>% names
          ) %>%
          mutate(id.season = year.season_start %>% paste0("-", (year.season_start + 1) %>% substr(3,4))) %>%
          dplyr::select(id.season,id.game, date.game,everything())

      } else {
        data <-
          all_summary_data %>%
          mutate_each_(funs(as.numeric(.)),vars =
                         all_summary_data %>% dplyr::select(
                           id.team, year.season_start,pts.paint:times.tied,
                           attendance, points.q1:points.ot.10,points.team
                         ) %>% names
          ) %>%
          separate(record.team, into = c('team.wins', 'team.losses'), sep = '\\-') %>%
          mutate(team.wins = team.wins %>% as.numeric,
                 team.losses = team.losses %>% as.numeric,
                 id.season = year.season_start %>% paste0("-", (year.season_start + 1) %>% substr(3,4))) %>%
          dplyr::select(id.season, id.game, date.game, is.home,team,points.team,everything())

      }
      data %<>%
        mutate(name.table = "Summary",
               table.boxscore = t)
    }

    if (t == "Traditional"&json_data$resultSets$rowSet[1] %>%
        data.frame %>%
        tbl_df %>% nrow > 0) {
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
              id.game = game_id,
              id.team = NA
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
            data_frame(
              id.game = game_id,
              name.table = 'Team Stats',
              table.boxscore = t,
              id.team = NA
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
            data_frame(
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

    if (t == "Advanced"&json_data$resultSets$rowSet[1] %>%
        data.frame %>%
        tbl_df %>% nrow > 0) {
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
            data_frame(
              id.game = game_id,
              name.table = 'Team Stats',
              table.boxscore = t
            )
        }
      }

      if (include_team == T & player_data %>% nrow() > 1) {
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

    if (t == "Misc"&json_data$resultSets$rowSet[1] %>%
        data.frame %>%
        tbl_df %>% nrow > 0) {
      if(json_data$resultSets$rowSet[1] %>%
         data.frame %>%
         tbl_df %>% nrow == 0){
        data <-
          data_frame(
            id.game = game_id,
            name.table = 'Misc',
            table.boxscore = t
          )
      }
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
          dplyr::select(-c(id.game:slug.team, plus.minus, name.table, table.boxscore))

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

    if (t == "Scoring"&json_data$resultSets$rowSet[1] %>%
        data.frame %>%
        tbl_df %>% nrow > 0) {
      if(json_data$resultSets$rowSet[1] %>%
         data.frame %>%
         tbl_df %>% nrow == 0){
        data <-
          data_frame(
            id.game = game_id,
            name.table = 'Scoring',
            table.boxscore = t
          )
      }
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

    if (t == "Usage"&json_data$resultSets$rowSet[1] %>%
        data.frame %>%
        tbl_df %>% nrow > 0) {
      if(json_data$resultSets$rowSet[1] %>%
         data.frame %>%
         tbl_df %>% nrow == 0){
        data <-
          data_frame(
            id.game = game_id,
            name.table = 'Usage',
            table.boxscore = t
          )
      }
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

    if (t == "Four Factors"&json_data$resultSets$rowSet[1] %>%
        data.frame %>%
        tbl_df %>% nrow > 0) {
      if(json_data$resultSets$rowSet[1] %>%
         data.frame %>%
         tbl_df %>% nrow == 0){
        data <-
          data_frame(
            id.game = game_id,
            name.table = 'Player Stats',
            table.boxscore = t
          )
      }
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

    if (t == "Play by Play"&json_data$resultSets$rowSet[1] %>%
        data.frame %>%
        tbl_df %>% nrow > 0) {
      if(json_data$resultSets$rowSet[1] %>%
         data.frame %>%
         tbl_df %>% nrow == 0){
        data <-
          data_frame(
            id.game = game_id,
            name.table = 'Play by Play',
            table.boxscore = t
          )
      }
      if ('PlayByPlay' %in% tables_names) {
        if (json_data$resultSets$rowSet[1] %>%
            data.frame %>%
            tbl_df %>% nrow > 0) {
          headers <-
            json_data$resultSets$headers[1] %>%
            unlist

          play_by_play <-
            json_data$resultSets$rowSet[1] %>%
            data.frame %>%
            tbl_df

          names(play_by_play) <-
            headers

          actual_names <-
            headers %>%
            get_header_names %>%
            .$name.actual

          names(play_by_play) <-
            actual_names

          play_by_play %<>%
            separate(score, into = c('points.away', 'points.home'), sep = '\\-', remove = F) %>%
            mutate(score.margin = ifelse(score.margin == "TIE", 0, score.margin))

          play_by_play %<>%
            mutate_each_(
              funs(as.numeric),
              vars = play_by_play %>%
                dplyr::select(id.event:period, points.away:id.player.1,id.team.player.1,type.player.2:id.player.2,
                              id.team.player.2,type.player.3:id.player.3, id.team.player.3
                              ) %>% names
            ) %>%
            mutate(name.table = 'Play by Play',
                   table.boxscore = t) %>%
            dplyr::select(-play.neutral.description) %>%
            fill(points.away) %>%
            fill(points.home) %>%
            fill(score.margin) %>%
            fill(score) %>%
            dplyr::filter(!id.event == 0)
          } else {
          play_by_play <-
            data_frame(
              name.table = 'Play by Play',
              table.boxscore = t,
              id.game = game_id
            )
        }
      }

      data <-
        play_by_play
    }

    if (t == "Player Tracking"&json_data$resultSets$rowSet[1] %>%
        data.frame %>%
        tbl_df %>% nrow > 0) {
      if(json_data$resultSets$rowSet[1] %>%
         data.frame %>%
         tbl_df %>% nrow == 0){
        data <-
          data_frame(
            id.game = game_id,
            name.table = 'Player Tracking',
            table.boxscore = t
          )
      }
      if ('PlayerTrack' %in% tables_names) {
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
              vars = player_data %>% dplyr::select(id.team, id.player, min:pct.fgm.opponent.rim_defense) %>% names
            ) %>%
            mutate(name.table = 'Player Tracking',
                   table.boxscore = t)
        } else {
          player_data <-
            data_frame(
              name.table = 'Player Tracking',
              table.boxscore = t,
              id.game = game_id
            )
        }
      }
      if ('PlayerTrackTeam' %in% tables_names & include_team == T) {
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
            mutate(team = city.team %>% paste(name.team)) %>%
            separate(min, c("minute", "second"), sep = '\\:') %>%
            mutate(
              minute = minute %>% as.numeric,
              second = second %>% as.numeric,
              min = minute + (second / 60)
            ) %>%
            dplyr::select(-c(minute, second, city.team)) %>%
            dplyr::select(id.game:slug.team, team, min, everything())

          team_data %<>%
            mutate_each_(funs(as.numeric),
                         vars = team_data %>% dplyr::select(id.team, min:pct.fgm.opponent.rim_defense) %>% names) %>%
            mutate(name.table = 'Team Player Tracking',
                   table.boxscore = t)
        } else {
          team_data <-
            mutate(
              id.game = game_id,
              name.table = 'Team Player Tracking',
              table.boxscore = t
            )
        }
      }
      if (include_team == T) {
        team <-
          team_data %>%
          dplyr::select(id.team, min:pct.fgm.opponent.rim_defense)

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

    if (t == "Game Charts"&json_data$resultSets$rowSet[1] %>%
        data.frame %>%
        tbl_df %>% nrow > 0) {
      if(json_data$resultSets$rowSet[1] %>%
         data.frame %>%
         tbl_df %>% nrow == 0){
        data <-
          data_frame(
            id.game = game_id,
            name.table = 'Game Charts',
            table.boxscore = t
          )
      }
      if ('FanDuelPlayer' %in% tables_names) {
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
            mutate_each_(
              funs(as.numeric),
              vars = player_data %>% dplyr::select(id.player,id.team, jersey, points.fanduel:plus.minus) %>% names
            ) %>%
            mutate(name.table = 'Fantasy',
                   table.boxscore = t)
        } else {
          player_data <-
            data_frame(
              name.table = 'Fantasy',
              table.boxscore = t,
              id.game = game_id
            )
        }
      }

      data <-
        player_data
    }

    if (t == "Win Probability"&json_data$resultSets$rowSet[1] %>%
        data.frame %>%
        tbl_df %>% nrow > 0) {
      if(json_data$resultSets$rowSet[1] %>%
         data.frame %>%
         tbl_df %>% nrow == 0){
        data <-
          data_frame(
            id.game = game_id,
            name.table = 'Win Probability',
            table.boxscore = t
          )
      }
      if ('WinProbPBP' %in% tables_names) {
        if (json_data$resultSets$rowSet[1] %>%
            data.frame %>%
            tbl_df %>% nrow > 0) {
          headers <-
            json_data$resultSets$headers[1] %>%
            unlist

          headers.2 <-
            json_data$resultSets$headers[2] %>%
            unlist

          wbp_data <-
            json_data$resultSets$rowSet[1] %>%
            data.frame %>%
            tbl_df

          game_data <-
            json_data$resultSets$rowSet[2] %>%
            data.frame %>%
            tbl_df

          actual_names <-
            headers %>%
            get_header_names %>%
            .$name.actual

          actual_names.2 <-
            headers.2 %>%
            get_header_names %>%
            .$name.actual


          names(wbp_data) <-
            actual_names

          names(game_data) <-
            actual_names.2

          wbp_data %<>%
            mutate_each_(
              funs(as.numeric),
              vars = wbp_data %>% dplyr::select(id.event:home.g) %>% names
            ) %>%
            mutate(name.table = 'Win Probability',
                   table.boxscore = t)
          wbp_data %<>%
            dplyr::select(-location) %>%
            dplyr::filter(!id.event == 0) %>%
            mutate(is.home.possesion = ifelse(is.home.possesion == "1", T, F)) %>%
            fill(id.event) %>%
            fill(is.home.possesion)
          if (include_game_data == T){
            wbp_data %<>%
              left_join(game_data %>%
                          mutate(date.game = date.game %>% lubridate::mdy() %>% as.Date) %>%
                          mutate_each_(funs(as.numeric(.)),
                                       game_data %>% dplyr::select(id.team.home, points.team.home, id.team.away, points.team.away) %>% names
                          ))
          }



        } else {
          wbp_data <-
            data_frame(
              name.table = 'Win Probability',
              table.boxscore = t,
              id.game = game_id
            )
        }
      }

      data <-
        wbp_data
    }

    if ('id.position.start' %in% names(data)){
      data %<>%
        mutate(is.starter = ifelse(!id.position.start == '', T, F))
    }

    if (return_message == T) {
      "You got " %>%
        paste0(t %>% str_to_lower, ' for game ', game_id) %>%
        message()
    }
    if(data %>% ncol() > 3){
      return(data)
    }
  }
get_game_id_box_score_data_safe <-
  failwith(NULL, get_game_id_box_score_data)

get_games_ids_box_score_tables <-
  function(game_id, include_team = F,
         tables =
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
           ),
         time_period = "All") {
  g <-
    game_id
  it <-
    include_team
  p <-
    time_period
  all_data <-
    data_frame()

  for (t in 1:length(tables)) {
    table <-
      get_game_id_box_score_data_safe(
        game_id = g,
        box_score_table = tables[t],
        return_wide = T,
        time_period = p,
        include_team = it,
        include_bench_starter = F,
        include_game_data = F
      )

      if('name.table' %in% names(table)){
        table %<>%
      dplyr::select(-c(name.table, table.boxscore))
      }

    if('table.boxscore' %in% names(table)){
      table %<>%
        dplyr::select(-c(table.boxscore))
    }

    if (t == 1){
      all_data %<>%
        bind_rows(table)
    } else {
      table_names <-
        names(table)[!names(table) %in% names(all_data)]

      table_names <-
        c('id.game', 'id.player',table_names)

      all_data %<>%
        left_join(table[,table_names])

    }
  }

  return(all_data)

}

