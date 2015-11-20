get_fd_name_df <- function(){
  fd_nba_name_df <-
    data_frame(
      name.fanduel = c("C.J. Watson", "J.J. Redick", "Louis Williams", "C.J. Miles",
                         "Chuck Hayes", "J.J. Hickson", "Bradley Beal", "P.J. Tucker",
                         "Ish Smith", "Luc Richard Mbah a Moute", "Moe Harkless", "C.J. McCollum",
                         "Nene Hilario", "K.J. McDaniels", "P.J. Hairston", "J.J. Barea",
                         "Glenn Robinson", "T.J. Warren", "R.J. Hunter", "Joseph Young",
                         "Larry Nance", "T.J. McConnell", "Raulzinho Neto"),
      name.nba = c("CJ Watson", "JJ Redick", "Lou Williams", "CJ Miles",
                   "Chuck Hayes", "JJ Hickson", "Bradley Beal", "PJ Tucker",
                   "Ish Smith", "Luc Mbah a Moute", "Maurice Harkless", "CJ McCollum",
                   "Nene Hilario", "KJ McDaniels", "PJ Hairston", "Jose Juan Barea",
                   "Glenn Robinson", "TJ Warren", "RJ Hunter", "Joe Young",
                   "Larry Nance Jr.", "TJ McConnell", "Raul Neto"),
      is.different_name = T
    )
  return(fd_nba_name_df)
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
        "Player_ID"

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
        "points.q1",
        "points.q2",
        "points.q3",
        "points.q4",
        "points.ot.1",
        "points.ot.2",
        "points.ot.3",
        "points.ot.4",
        "points.ot.5",
        "points.ot.6",
        "points.ot.7",
        "points.ot.8",
        "points.ot.9",
        "points.ot.10",
        "id.game.last",
        "date.game.last",
        "id.team.home.last",
        "city.team.home.last",
        "name.team.home.last",
        "slug.team.home.last",
        "points.team.home.last",
        "id.team.away.last",
        "city.team.away.last",
        "name.team.away.last",
        "slug.team.away.last",
        "points.team.away.last",
        "wins.team.home",
        "losses.team.home",
        "team.series_leader",
        "is.video_available",
        "is.pt.available",
        "is.pt.xyz.available",
        "id.player"
      ),
      id.row = 1:length(name.actual)
    )
  return(headers_df)
}
get_nba_players_ids <- function(active_only = F, resolve_to_fanduel = F) {
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
  players.url <-
    "http://stats.nba.com/stats/commonallplayers?IsOnlyCurrentSeason=0&LeagueID=00&Season=2015-16"

  json_data <-
    players.url %>%
    jsonlite::fromJSON(simplifyDataFrame = T)

  data <-
    json_data$resultSets$rowSet %>%
    data.frame %>%
    tbl_df

  headers <-
    json_data$resultSets$headers %>%
    unlist %>%
    str_to_lower()

  headers_df <-
    get_headers()

  actual_names <-
    1:length(headers) %>%
    purrr::map(
      function(x)
        data_frame(
          name.actual =
            headers_df %>%
            mutate(name.nba = name.nba %>% str_to_lower) %>%
            dplyr::filter(name.nba == headers[x]) %>%
            .$name.actual
        )
    ) %>%
    bind_rows()

  names(data) <-
    actual_names$name.actual

  names_df <-
    data$name.last.display %>%
    str_split_fixed(pattern = '\\,', 2) %>%
    data.frame() %>%
    tbl_df

  names(names_df) <-
    c('name.last', 'name.first')

  names_df %<>%
    mutate(player = name.first %>% str_trim %>% paste(name.last %>% str_trim)) %>%
    dplyr::select(player)
  data$name.player <-
    names_df$player
  data %<>%
    mutate(
      id.player = id.player %>% as.numeric,
      is.active_player = ifelse(id.team == 0, FALSE, TRUE),
      id.team = id.team %>% as.numeric
    ) %>%
    dplyr::select(-c(status.roster, name.last.display)) %>%
    mutate_each(funs(extract_numeric), starts_with("year.")) %>%
    mutate(
      id.team = ifelse(id.team == 0, NA, id.team),
      name.player = name.player %>% str_trim,
      city.team = ifelse(city.team == '', NA, city.team),
      code.team = ifelse(code.team == '', NA, code.team),
      slug.team = ifelse(slug.team == '', NA, slug.team),
      team = ifelse(city.team %>% is.na, NA, paste(city.team, team)),
      seasons.played = year.to - year.from,
      url.player = id.player %>% paste0('http://stats.nba.com/player/#!/', .),
      image.player = id.player %>% paste0('http://stats.nba.com/media/players/132x132/',.,'.png')
    ) %>%
    dplyr::select(
      name.player,
      id.player,
      team,
      id.team,
      is.active_player,
      seasons.played,
      year.from,
      year.to,
      everything()
    )

  if (active_only == T) {
    data %<>%
      dplyr::filter(is.active_player == T)
  }

  if (resolve_to_fanduel == T ){
    fd_names <-
      get_fd_name_df()

    data %<>%
      left_join(fd_names %>%
                  dplyr::rename(name.player = name.nba))
    data %<>%
      mutate(
        is.different_name = ifelse(is.different_name %>% is.na, F, T),
        name.player = ifelse(is.different_name == T, name.fanduel, name.player)) %>%
      dplyr::select(-c(is.different_name, name.fanduel)) %>%
      arrange(name.player)
  }

  return(data)
}

height_in_inches <-
  function(height) {
    height_ft_in <-
      height %>%
      str_split("-") %>%
      unlist %>%
      as.numeric()
    height_in <-
      height_ft_in[1] * 12 + height_ft_in[2]
    return(height_in)
  }

weight_in_lbs <- function(x){

}

get_player_profile <- function(player,
                               id.player = NULL,
                               include_headline_stat = T,
                               return_message = T) {
  packages <- #need all of these installed including some from github
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
  players <-
    get_nba_players_ids()

  if (id.player %>% is.null()) {
    id.player <-
      players %>%
      dplyr::filter(name.player == player) %>%
      .$id.player

  } else {
    id <-
      id.player
    player <-
      players %>%
      dplyr::filter(id.player == id) %>%
      .$name.player
  }

  active_player <-
    players %>%
    dplyr::filter(id.player == id) %>%
    .$is.active

  ## Build URL
  url_json <-
    'http://stats.nba.com/stats/commonplayerinfo?LeagueID=00&PlayerID=' %>%
    paste0(id.player)

  json_data <-
    url_json %>%
    fromJSON(simplifyDataFrame = T, flatten = T)

  headers_df <-
    get_headers()

  headers <-
    json_data$resultSets$headers[1] %>%
    unlist %>%
    str_to_lower()

  data <-
    json_data$resultSets$rowSet[1] %>%
    data.frame %>%
    tbl_df

  actual_names <-
    1:length(headers) %>%
    purrr::map(
      function(x)
        data_frame(
          name.actual =
            headers_df %>%
            mutate(name.nba = name.nba %>% str_to_lower) %>%
            dplyr::filter(name.nba == headers[x]) %>%
            .$name.actual
        )
    ) %>%
    bind_rows()

  names(data) <-
    actual_names$name.actual

  data %<>%
    separate(date.birth, into = c('date.birth', 'ignore'), sep = 'T') %>%
    dplyr::select(-c(name.first, name.last, name.last.display, name.middle.display, gp.flag, ignore, status.roster)) %>%
    mutate(
      is.rookie = ifelse(years.experience == "R", T, F),
      years.experience = years.experience %>% str_replace("R", 0) %>% as.numeric(),
      id.team = id.team %>% as.numeric,
      jersey = jersey %>% as.numeric,
      height.inches = height %>% lapply(height_in_inches) %>% unlist,
      weight.lbs = weight.lbs %>% as.numeric,
      date.birth = date.birth %>% ymd %>% as.Date(),
      id.player = id.player %>% as.numeric,
      is.active_player = active_player,
      team = city.team %>% paste0(team),
      bmi = (weight.lbs / height.inches ^ 2) * 703,
      has.d_league_data = has.d_league_data %>% str_detect("Y")
    ) %>%
    dplyr::select(name.player, id.player, is.rookie, is.active_player, team, position, jersey, height, height.inches, weight.lbs, bmi, years.experience, year.from, year.to, everything())

  if (include_headline_stat == T) {
    headers <-
      json_data$resultSets$headers[2] %>%
      unlist %>%
      str_to_lower()

    stat <-
      json_data$resultSets$rowSet[2] %>%
      data.frame %>%
      tbl_df

    actual_names <-
      1:length(headers) %>%
      purrr::map(
        function(x)
          data_frame(
            name.actual =
              headers_df %>%
              mutate(name.nba = name.nba %>% str_to_lower) %>%
              dplyr::filter(name.nba == headers[x]) %>%
              .$name.actual
          )
      ) %>%
      bind_rows()

    names(stat) <-
      actual_names$name.actual

    stat %<>%
      mutate_each_(funs(extract_numeric), vars =
                     stat %>%
                     dplyr::select(id.player, pts:pie) %>% names) %>%
      rename(id.season.recent = id.season)

    names(stat)[4:length(names(stat))] %<>%
      paste0('.per_game.recent')

    data <-
      stat %>%
      left_join(data)
  }


  if (return_message == T) {
    "Congrats, you got " %>%
      paste0(player, "'s profile data") %>%
      message()
  }
  return(data)
}

#' Title
#'
#' @param id.player
#' @param player
#' @param season_type
#' @param year.season_start
#'
#' @return
#' @export
#'
#' @examples get_player_season_gamelog(player = "John Stockton", year.season_start = 1994, include_player_metadata = T)
#get_player_season_gamelog(player = "Kyle Korver", season_type = "Pre Season", year.season_start = 2015)
get_player_season_gamelog <- function(player,
                                      id.player = NULL,
                                      season_type = "Regular Season",
                                      year.season_start,
                                      include_date_detail = T,
                                      include_player_metadata = F,
                                      return_message = T) {
  packages <- #need all of these installed including some from github
    c(
      'dplyr',
      'magrittr',
      'jsonlite',
      'tidyr',
      'purrr',
      'stringr',
      'lubridate',
      'tidyr'
    )
  options(warn = -1)
  lapply(packages, library, character.only = T)

  seasons_types <-
    c("Regular Season", "Playoffs", "Pre Season", "All Star")
  if (!season_type %in% seasons_types) {
    "Sorry season type must be either " %>%
      paste0(seasons_types %>% paste0(collapse = ', ')) %>%
      stop(call. = F)
  }
  players <-
    get_nba_players_ids()

  season <-
    year.season_start %>%
    extract_numeric %>%
    paste0("-", (year.season_start + 1) %>% substr(start = 3, stop = 4))

  st <-
    season_type %>%
    str_replace(pattern = '\\ ', '\\+')

  if (id.player %>% is.null) {
    id.player <-
      players %>%
      dplyr::filter(name.player == player) %>%
      .$id.player

    id <-
      id.player

    start.season <-
      players %>%
      dplyr::filter(id.player == id) %>%
      .$year.from

    end.season <-
      players %>%
      dplyr::filter(id.player == id) %>%
      .$year.to

  } else {
    id <-
      id.player
    player <-
      players %>%
      dplyr::filter(id.player == id) %>%
      .$name.player %>%
      str_trim()

    start.season <-
      players %>%
      dplyr::filter(id.player == id) %>%
      .$year.from

    end.season <-
      players %>%
      dplyr::filter(id.player == id) %>%
      .$year.to
  }

  if (year.season_start < start.season) {
    "Sorry " %>%
      paste0(player, "'s first season was ", start.season) %>%
      stop(call. = F)
  }

  if (year.season_start > end.season) {
    "Sorry " %>%
      paste0(player, "'s last season was ", end.season) %>%
      stop(call. = F)
  }

  ## Build URL
  url_json <-
    'http://stats.nba.com/stats/playergamelog?LeagueID=00&PlayerID=' %>%
    paste0(id.player, '&Season=', season, '&SeasonType=', st)

  json_data <-
    url_json %>%
    fromJSON(simplifyDataFrame = T, flatten = T)

  headers_df <-
    get_headers()

  headers <-
    json_data$resultSets$headers %>%
    unlist %>%
    str_to_lower()

  if (json_data$resultSets$rowSet %>%
      data.frame %>%
      tbl_df %>% nrow > 0) {
    data <-
      json_data$resultSets$rowSet %>%
      data.frame %>%
      tbl_df

    headers <-
      json_data$resultSets$headers %>% unlist

    actual_names <-
      headers %>%
      get_header_names

    names(data) <-
      actual_names$name.actual

    data %<>%
      mutate_each_(
        funs(extract_numeric),
        vars =
          data %>%
          dplyr::select(id.player, min:plus.minus) %>% names
      ) %>%
      separate(
        matchup,
        into = c("slug.team", "slug.opponent"),
        sep = "@|vs.",
        remove = F
      ) %>%
      mutate(
        id.season = season,
        is.win = wl %>% str_detect("W"),
        date.game = date.game %>% lubridate::mdy() %>% as.Date,
        is.home_game = matchup %>% str_detect("vs. "),
        is.video_available = is.video_available %>% str_detect("1"),
        slug.team = slug.team %>% str_trim,
        slug.opponent = slug.opponent %>% str_trim,
        name.player = player,
        type.season = season_type,
        year.season_start
      ) %>%
      arrange(date.game) %>%
      mutate(days.rest = date.game - dplyr::lag(date.game)) %>%
      dplyr::select(-c(wl, code.season)) %>%
      dplyr::select(
        id.season,
        type.season,
        id.player,
        name.player,
        is.win,
        date.game,
        is.home_game,
        days.rest,
        everything()
      )



    if (include_date_detail == T) {
      data %<>%
        mutate(
          day.game = date.game %>% strftime('%A'),
          month.game = date.game %>% lubridate::month
        ) %>%
        dplyr::select(id.season:date.game,
                      day.game,
                      month.game,
                      everything())
    }

    if (include_player_metadata == T) {
      profile <-
        get_player_profile(
          id.player = data$id.player %>% unique,
          return_message = F,
          include_headline_stat = F
        )

      fd_names <-
        get_fd_name_df()
      if (profile$name.player %in% fd_names$name.nba == T) {
        np <- profile$name.player
        profile$name.player <-
          fd_names %>%
          dplyr::filter(name.nba == np) %>%
          .$name.fanduel
      }

      data <-
        profile %>%
        dplyr::select(name.player,
                      id.player,
                      position,
                      height.inches,
                      weight.lbs) %>%
        left_join(data) %>%
        dplyr::select(id.season, everything())
    }

    if (return_message == T) {
      "Congrats, you got " %>%
        paste0(season, " ", season_type, " game logs for ", player) %>%
        message()
    }
    return(data)
  } else {
    "Sorry " %>%
      paste0(player, " has no data for ", season, " " , season_type) %>%
      message
  }
}
