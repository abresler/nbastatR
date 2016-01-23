function_packages <-
  c('dplyr',
    'magrittr',
    'jsonlite',
    'tidyr',
    'stringr',
    'lubridate',
    'stringr',
    'tidyr')

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
    lapply(package_to_load, library, character.only = T)
  }
install_needed_packages(function_packages)
load_needed_packages(function_packages)

#' get_nba_franchise_data
#'
#' @param return_franchises
#' @param return_message
#'
#' @return
#' @export
#'
#' @examples
get_nba_franchise_data <-
  function(return_franchises = c('all', 'active', 'current'),
           return_message = T) {
    team_history_url <-
      'http://stats.nba.com/stats/franchisehistory?LeagueID=00'

    team_data <-
      team_history_url %>%
      fromJSON(simplifyDataFrame = T, flatten = T)

    names_active <-
      team_data$resultSets$headers[1] %>%
      unlist %>%
      str_to_lower

    names_defunct <-
      team_data$resultSets$headers[2] %>%
      unlist %>%
      str_to_lower

    active_data <-
      team_data$resultSets$rowSet[1] %>%
      data.frame %>%
      tbl_df()

    names(active_data) <-
      names_active

    active_data %<>%
      mutate(is.active = T)

    defunct_data <-
      team_data$resultSets$rowSet[2] %>%
      data.frame %>%
      tbl_df()

    names(defunct_data) <-
      names_defunct

    defunct_data %<>%
      mutate(is.active = F)

    data <-
      active_data %>%
      bind_rows(defunct_data)

    num_cols <-
      data %>%
      select(-c(contains("team")), -is.active) %>%
      names

    data %<>%
      mutate_each_(funs(as.numeric), vars = num_cols)

    names(data) <-
      c(
        "id.league",
        "id.team",
        "city.team",
        "name.team",
        "year.start.team",
        "year.end.team",
        "team.seasons",
        "team.games",
        "team.wins",
        "team.losses",
        "pct.wins",
        "team.po_appearances",
        "team.div_titles",
        "team.conf_titles",
        "team.league_titles",
        "is.active"
      )

    data %<>%
      mutate(team = city.team %>% paste(name.team),
             id.team = id.team %>% as.numeric()) %>%
      dplyr::select(-id.league) %>%
      dplyr::select(id.team, team, everything())

    if (return_franchises == 'current') {
      data %<>%
        mutate(id.row = 1:nrow(.)) %>%
        group_by(id.team) %>%
        dplyr::filter(id.row == min(id.row), is.active == T) %>%
        select(-id.row)
    }

    if (return_franchises == 'active') {
      data %<>%
        dplyr::filter(is.active == T)
    }
    if (return_message == T) {
      "You got NBA franchise data" %>%
        message
    }
    return(data)
  }

get_teams_ids <-
  function() {
    data <-
      data_frame(
        team = c(
          "Atlanta Hawks",
          "Boston Celtics",
          "Brooklyn Nets",
          "Charlotte Hornets",
          "Chicago Bulls",
          "Cleveland Cavaliers",
          "Dallas Mavericks",
          "Denver Nuggets",
          "Detroit Pistons",
          "Golden State Warriors",
          "Houston Rockets",
          "Indiana Pacers",
          "Los Angeles Clippers",
          "Los Angeles Lakers",
          "Memphis Grizzlies",
          "Miami Heat",
          "Milwaukee Bucks",
          "Minnesota Timberwolves",
          "New Orleans Pelicans",
          "New York Knicks",
          "Oklahoma City Thunder",
          "Orlando Magic",
          "Philadelphia 76ers",
          "Phoenix Suns",
          "Portland Trail Blazers",
          "Sacramento Kings",
          "San Antonio Spurs",
          "Toronto Raptors",
          "Utah Jazz",
          "Washington Wizards"
        ),
        slug.team = c(
          "ATL",
          "BOS",
          "BKN",
          "CHA",
          "CHI",
          "CLE",
          "DAL",
          "DEN",
          "DET",
          "GSW",
          "HOU",
          "IND",
          "LAC",
          "LAL",
          "MEM",
          "MIA",
          "MIL",
          "MIN",
          "NOP",
          "NYK",
          "OKC",
          "ORL",
          "PHI",
          "PHO",
          "POR",
          "SAC",
          "SAS",
          "TOR",
          "UTA",
          "WAS"
        )
      )
    return(data)
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
        "FTA_RATE",
        "OPP_EFG_PCT",
        "OPP_FTA_RATE",
        "OPP_TOV_PCT",
        "OPP_OREB_PCT",
        "OPP_FGM",
        "OPP_FGA",
        "OPP_FG_PCT",
        "OPP_FG3M",
        "OPP_FG3A",
        "OPP_FG3_PCT",
        "OPP_FTM",
        "OPP_FTA",
        "OPP_FT_PCT",
        "OPP_OREB",
        "OPP_DREB",
        "OPP_REB",
        "OPP_AST",
        "OPP_TOV",
        "OPP_STL",
        "OPP_BLK",
        "OPP_BLKA",
        "OPP_PF",
        "OPP_PFD",
        "OPP_PTS"
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
        "rate.fta",
        "pct.efg.opp",
        "rate.fta.opp",
        "pct.tov.opp",
        "pct.oreb.opp",
        "fgm.opp",
        "fga.opp",
        "pct.fg.opp",
        "fg3m.opp",
        "fg3a.opp",
        "pct.fg3.opp",
        "ftm.opp",
        "fta.opp",
        "pct.ft.opp",
        "oreb.opp",
        "dreb.opp",
        "rep.opp",
        "ast.opp",
        "tov.opp",
        "stl.opp",
        "blk.opp",
        "fga.blocked.opp",
        "fouls.opp",
        "fouls.drawn.opp",
        "pts.opp"
      ),
      id.row = 1:length(name.actual)
    )
  return(headers_df)
}

clean_to_stem <- function(x) {
  x %<>%
    str_replace('\\ ', '\\+') %>%
    str_replace('\\/', '\\2F') %>%
    str_replace("\\'", '%27')

  return(x)

}

#' get_nba_team_stat_table
#'
#' @param year.season_start
#' @param season_type
#' @param measure_type
#' @param per_mode
#' @param is.pace_adjusted
#' @param period
#' @param player_experience
#' @param player_position
#' @param is.rank
#' @param is.plus_minus
#' @param game_segment
#' @param conference
#' @param division
#' @param division_against
#' @param conference_against
#' @param ahead_behind
#' @param date_from
#' @param date_to
#' @param game_scope
#' @param last_n_games
#' @param location
#' @param month
#' @param season_segment
#' @param opponent
#' @param team
#' @param outcome
#' @param playoff_round
#' @param shot_clock_range
#' @param starter_bench
#' @param return_metadata
#' @param include_measure_name
#' @param return_message
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_nba_team_stat_table <-
  function(year.season_start = 2015,
           season_type = c('Regular Season', 'Pre Season', 'Playoffs', 'All Star'),
           measure_type = c("Base",
                            "Advanced",
                            "Misc",
                            "Scoring",
                            "Four Factors",
                            "Opponent"),
           per_mode =  c(
             "Totals",
             "PerGame",
             "MinutesPer",
             "Per48",
             "Per40",
             "Per36",
             "PerMinute",
             "PerPossession",
             "PerPlay",
             "Per100Possessions",
             "Per100Plays"
           ),
           is.pace_adjusted = F,
           period = 0,
           player_experience =  c(NA, "Rookie", "Sophomore", "Veteran"),
           player_position = c(NA, "F", "C", "G"),
           is.rank = F,
           is.plus_minus = c(F, T),
           game_segment = c(NA, "First Half", "Second Half", "Overtime"),
           conference = c(NA, "East", "West"),
           division = c(NA,
                        "Atlantic",
                        "Central",
                        "Northwest",
                        "Pacific",
                        "Southeast",
                        "Southwest"),
           division_against = c(NA,
                                "Atlantic",
                                "Central",
                                "Northwest",
                                "Pacific",
                                "Southeast",
                                "Southwest"),
           conference_against = c(NA, "East", "West"),
           ahead_behind = c(NA, 'Ahead', 'Behind'),
           date_from = NA,
           date_to = NA,
           game_scope =  c(NA, "Yesterday", "Last 10"),
           last_n_games = 0,
           location = c(NA, "Home", "Road"),
           month = 0,
           season_segment = c(NA, "Post All-Star", "Pre All-Star"),
           opponent = NA,
           team = NA,
           outcome = c(NA, "W", "L"),
           playoff_round = 0,
           shot_clock_range = c(
             NA,
             "24-22",
             "22-18 Very Early",
             "18-15 Early",
             "15-7 Average",
             "7-4 Late",
             "4-0 Very Late",
             "ShotClock Off"
           ),
           starter_bench = c(NA, "Starters", "Bench"),
           return_metadata = F,
           include_measure_name = c(T, F),
           return_message = c(T, F),
           ...) {
    if (year.season_start < 1996) {
      stop("Sorry data only goes back to the 1996-97 Season")
    }
    base <-
      'http://stats.nba.com/stats/leaguedashteamstats?'
    t <-
      team
    id.season <-
      year.season_start %>%
      paste0("-", (year.season_start + 1) %>% substr(3, 4))

    if (!ahead_behind[1] %>% is.na) {
      ahead_behind_stem <-
        ahead_behind[1] %>% clean_to_stem
    } else {
      ahead_head_stem <-
        ''
    }

    if (!conference_against[1] %>% is.na) {
      if (!conference_against[1] %in% c("East", "West")) {
        stop("Sorry conference against can only be East or West")
      }
      conference_against_stem <-
        conference_against[1] %>% clean_to_stem
    } else {
      conference_against_stem <-
        ''
    }

    if (!conference[1] %>% is.na) {
      if (!conference %in% c("East", "West")) {
        stop("Sorry conference can only be East or West")
      }
      conference_stem <-
        conference %>% clean_to_stem
    } else {
      conference_stem <-
        ''
    }

    if (!date_from %>% is.na) {
      date_from_stem <-
        date_from %>% clean_to_stem
    } else {
      date_from_stem <-
        ''
    }

    if (!date_to %>% is.na) {
      date_to_stem <-
        date_to %>% clean_to_stem
    } else {
      date_to_stem <-
        ''
    }

    if (!division_against[1] %>% is.na) {
      if (!division_against %in% c("Atlantic",
                                   "Central",
                                   "Northwest",
                                   "Pacific",
                                   "Southeast",
                                   "Southwest")) {
        stop(
          "Sorry division against can only be Atlantic, Central\nNorthwest, Pacific, Southeast, orSouthwest"
        )
      }
      division_against_stem <-
        division_against[1] %>% clean_to_stem
    } else {
      division_against_stem <-
        ''
    }

    if (!division[1] %>% is.na) {
      if (!division %in% c("Atlantic",
                           "Central",
                           "Northwest",
                           "Pacific",
                           "Southeast",
                           "Southwest")) {
        stop(
          "Sorry division can only be Atlantic, Central\nNorthwest, Pacific, Southeast, orSouthwest"
        )
      }
      division_stem <-
        division[1] %>% clean_to_stem
    } else {
      division_stem <-
        ''
    }

    if (!game_scope[1] %>% is.na) {
      GameScope = c("Yesterday", "Last 10")

      if (!game_scope[1] %in% GameScope) {
        "Sorry game scope can only be " %>%
          paste0(GameScope %>% paste0(collapse = ', ')) %>%
          stop(call. = F)
      }
      game_scope_stem <-
        game_scope[1] %>% clean_to_stem
    } else {
      game_scope_stem <-
        ''
    }

    if (!game_segment[1] %>% is.na) {
      GameSegment = c("First Half", "Second Half", "Overtime")

      if (!game_segment[1] %in% GameSegment) {
        "Sorry game segment can only be " %>%
          paste0(GameSegment %>% paste0(collapse = ', ')) %>%
          stop(call. = F)
      }
      game_segment_stem <-
        game_segment[1] %>% clean_to_stem
    } else {
      game_segment_stem <-
        ''
    }

    if (!last_n_games %>% is.na) {
      if (!last_n_games >= 0) {
        stop("Last N games must be over 0")
      }
      last_n_games_stem <-
        last_n_games
    } else {
      last_n_games_stem <-
        0
    }

    if (!location[1] %>% is.na) {
      if (!location[1] %in% c("Home", "Road")) {
        stop("Sorry location can only be Home or Road")
      }
      location_stem <-
        location[1] %>% clean_to_stem
    } else {
      location_stem <-
        ''
    }

    if (!measure_type[1] %>% is.na) {
      MeasureType = c("Base",
                      "Advanced",
                      "Misc",
                      "Scoring",
                      "Four Factors",
                      "Opponent")

      if (!measure_type[1] %in% MeasureType) {
        "Sorry measure type can only be " %>%
          paste0(measure_type %>% paste0(collapse = ', ')) %>%
          stop(call. = F)
      }
      measure_type_stem <-
        measure_type[1] %>% clean_to_stem
    } else {
      measure_type_stem <-
        'Base'
    }

    if (opponent %>% length > 0 | team %>% length > 0) {
      teams_ids <-
        get_nba_franchise_data(return_franchises = 'current', return_message = F) %>%
        dplyr::select(id.team, team, year.start.team, year.end.team) %>%
        ungroup %>%
        tbl_df

      nba_teams <-
        c(
          "Atlanta Hawks",
          "St. Louis Hawks",
          "Milwaukee Hawks",
          "Tri-Cities Blackhawks",
          "Boston Celtics",
          "Brooklyn Nets",
          "New Jersey Nets",
          "New York Nets",
          "Charlotte Hornets",
          "Charlotte Bobcats",
          "Chicago Bulls",
          "Cleveland Cavaliers",
          "Dallas Mavericks",
          "Denver Nuggets",
          "Detroit Pistons",
          "Ft. Wayne Zollner Pistons",
          "Golden State Warriors",
          "San Francisco Warriors",
          "Philadelphia Warriors",
          "Houston Rockets",
          "San Diego Rockets",
          "Indiana Pacers",
          "Los Angeles Clippers",
          "San Diego Clippers",
          "Buffalo Braves",
          "Los Angeles Lakers",
          "Minneapolis Lakers",
          "Memphis Grizzlies",
          "Vancouver Grizzlies",
          "Miami Heat",
          "Milwaukee Bucks",
          "Minnesota Timberwolves",
          "New Orleans Pelicans",
          "New Orleans Hornets",
          "New Orleans/Oklahoma City Hornets",
          "New York Knicks",
          "Oklahoma City Thunder",
          "Seattle SuperSonics",
          "Orlando Magic",
          "Philadelphia 76ers",
          "Syracuse Nationals",
          "Phoenix Suns",
          "Portland Trail Blazers",
          "Sacramento Kings",
          "Kansas City Kings",
          "Kansas City-Omaha Kings",
          "Cincinnati Royals",
          "Rochester Royals",
          "San Antonio Spurs",
          "Toronto Raptors",
          "Utah Jazz",
          "New Orleans Jazz",
          "Washington Wizards",
          "Washington Bullets",
          "Capital Bullets",
          "Baltimore Bullets",
          "Chicago Zephyrs",
          "Chicago Packers",
          "Anderson Packers",
          "Chicago Stags",
          "Cleveland Rebels",
          "Detroit Falcons",
          "Indianapolis Jets",
          "Indianapolis Olympians",
          "Pittsburgh Ironmen",
          "Providence Steamrollers",
          "Sheboygan Redskins",
          "St. Louis Bombers",
          "Toronto Huskies",
          "Washington Capitols",
          "Waterloo Hawks"
        )
    }

    if (!opponent %>% is.na) {
      if (!opponent %in% nba_teams) {
        "Opponent must be either " %>%
          paste0(nba_teams %>% paste0(collapse = ', ')) %>%
          stop()
      }

      opponent_stem <-
        teams_ids %>%
        mutate(team = team %>% str_to_lower()) %>%
        dplyr::filter(team == opponent %>% str_to_lower()) %>%
        .$team_id
    } else {
      opponent_stem <-
        0
    }

    if (!team %>% is.na) {
      if (!team %in% nba_teams) {
        "team must be either " %>%
          paste0(nba_teams %>% paste0(collapse = ', ')) %>%
          stop()
      }
      tn <-
        team
      team_stem <-
        teams_ids %>%
        mutate(team = team %>% str_to_lower()) %>%
        dplyr::filter(team == tn %>% str_to_lower()) %>%
        .$team_id
    } else {
      team_stem <-
        0
    }

    if (!outcome[1] %>% is.na) {
      Outcome = c("W", "L")

      if (!outcome[1] %in% Outcome) {
        "Sorry outcome can only be " %>%
          paste0(Outcome %>% paste0(collapse = ', ')) %>%
          stop(call. = F)
      }
      outcome_stem <-
        outcome[1] %>% clean_to_stem
    } else {
      outcome_stem <-
        ''
    }

    if (playoff_round > 4) {
      stop("Playoff round can only be zero to 4")
    } else {
      playoff_round_stem <-
        playoff_round
    }

    if (is.pace_adjusted == T) {
      pace_stem <-
        "Y"
    } else {
      pace_stem <-
        "N"
    }

    if (!per_mode[1] %>% is.na) {
      PerMode = c(
        "Totals",
        "PerGame",
        "MinutesPer",
        "Per48",
        "Per40",
        "Per36",
        "PerMinute",
        "PerPossession",
        "PerPlay",
        "Per100Possessions",
        "Per100Plays"
      )
      per_mode[1] %<>%
        str_replace('\\ ', '')
      if (!per_mode[1] %>% clean_to_stem %in% PerMode) {
        "Sorry per mode can only be " %>%
          paste0(per_mode[1] %>% paste0(collapse = ', ')) %>%
          stop(call. = F)
      }
      per_mode_type_stem <-
        per_mode[1] %>% clean_to_stem
    } else {
      per_mode_stem <-
        'Totals'
    }
    if (month > 12) {
      stop("Month can only be zero to 12")
    } else {
      month_stem <-
        month
    }

    if (period > 14) {
      stop("Period can only be zero to 14")
    } else {
      period_stem <-
        period
    }

    if (!player_experience[1] %>% is.na) {
      PlayerExperience = c("Rookie", "Sophomore", "Veteran")

      if (!player_experience %in% PlayerExperience) {
        "Sorry player experience can only be " %>%
          paste0(player_experience %>% paste0(collapse = ', ')) %>%
          stop(call. = F)
      }
      player_experience_stem <-
        player_experience[1] %>% clean_to_stem
    } else {
      player_experience_stem <-
        ''
    }

    if (!player_position[1] %>% is.na) {
      PlayerPosition = c("F", "C", "G")

      if (!player_position %in% PlayerPosition) {
        "Sorry player position can only be " %>%
          paste0(player_position %>% paste0(collapse = ', ')) %>%
          stop(call. = F)
      }
      player_position_stem <-
        player_position[1] %>% clean_to_stem
    } else {
      player_position_stem <-
        ''
    }

    if (is.plus_minus[1] == T) {
      plus_minus_stem <-
        "Y"
    } else {
      plus_minus_stem <-
        "N"
    }

    if (is.rank[1] == T) {
      rank_stem <-
        "Y"
    } else {
      rank_stem <-
        "N"
    }

    if (!season_segment[1] %>% is.na) {
      SeasonSegment = c("Post All-Star", "Pre All-Star")

      if (!season_segment %in% SeasonSegment) {
        "Sorry season segment can only be " %>%
          paste0(SeasonSegment %>% paste0(collapse = ', ')) %>%
          stop(call. = F)
      }
      season_segment_stem <-
        season_segment[1] %>% clean_to_stem
    } else {
      season_segment_stem <-
        ''
    }

    if (!season_type[1] %>% is.na) {
      SeasonType = c('Regular Season', 'Pre Season', 'Playoffs', 'All Star')

      if (!season_type %in% SeasonType) {
        "Sorry season season type can only be " %>%
          paste0(SeasonType %>% paste0(collapse = ', ')) %>%
          stop(call. = F)
      }
      season_type_stem <-
        season_type[1] %>% clean_to_stem
    } else {
      season_type_stem <-
        'Regular+Season'
    }

    if (!shot_clock_range[1] %>% is.na) {
      ShotClockRange = c(
        "24-22",
        "22-18 Very Early",
        "18-15 Early",
        "15-7 Average",
        "7-4 Late",
        "4-0 Very Late",
        "ShotClock Off"
      )

      if (!shot_clock_range[1] %in% SeasonSegment) {
        "Sorry shot clock range can only be " %>%
          paste0(ShotClockRange %>% paste0(collapse = ', ')) %>%
          stop(call. = F)
      }
      shot_clock_range_stem <-
        shot_clock_range[1] %>% clean_to_stem
    } else {
      shot_clock_range_stem <-
        ''
    }

    if (!starter_bench[1] %>% is.na) {
      StarterBench = c("Starters", "Bench")

      if (!starter_bench[1] %in% SeasonSegment) {
        "Sorry starter/bench can only be " %>%
          paste0(StarterBench %>% paste0(collapse = ', ')) %>%
          stop(call. = F)
      }
      starter_bench_stem <-
        starter_bench[1] %>% clean_to_stem
    } else {
      starter_bench_range_stem <-
        ''
    }


    url_json <-
      base %>%
      paste0(
        '&Conference=',
        conference_stem,
        '&DateFrom=',
        date_from_stem,
        '&DateTo=',
        date_to_stem,
        '&Division=',
        division_stem,
        '&GameScope=',
        game_scope_stem,
        '&GameSegment=',
        game_segment_stem,
        '&LastNGames=',
        last_n_games_stem,
        '&LeagueID=00',
        '&Location=',
        location_stem,
        '&MeasureType=',
        measure_type_stem,
        '&Month=',
        month_stem,
        '&OpponentTeamID=',
        opponent_stem,
        '&Outcome=',
        outcome_stem,
        '&PORound=',
        playoff_round_stem,
        '&PaceAdjust=',
        pace_stem,
        '&PerMode=',
        per_mode_type_stem,
        '&Period=',
        period_stem,
        '&PlayerExperience=',
        player_experience_stem,
        '&PlayerPosition=',
        player_position_stem,
        '&PlusMinus=',
        plus_minus_stem,
        '&Rank=',
        rank_stem,
        '&Season=',
        id.season,
        '&SeasonSegment=',
        season_segment_stem,
        '&SeasonType=',
        season_type_stem,
        '&ShotClockRange=',
        shot_clock_range_stem,
        '&StarterBench=',
        starter_bench_range_stem,
        '&TeamID=',
        team_stem,
        '&VsConference=',
        conference_against_stem,
        '&VsDivision=',
        division_against_stem,
        sep = ''
      )

    json_data <-
      url_json %>%
      fromJSON(simplifyDataFrame = T, flatten = T)

    headers <-
      json_data$resultSets$headers %>% unlist

    data <-
      json_data$resultSets$rowSet %>% data.frame %>%
      tbl_df

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

    names(data) <-
      actual_names$name.actual

    data <-
      data[, !data %>% names() %in% c('cfid', 'cfparms')]

    num_cols <-
      names(data)[!data %>% names() %in% c('name.player', 'slug.team', 'team')]

    data %<>%
      mutate_each_(funs(extract_numeric(.)), vars = num_cols) %>%
      mutate(
        id.season,
        measure_type = measure_type[1],
        season_type = season_type[1],
        per_mode = per_mode[1],
        is.pace_adjusted = is.pace_adjusted[1],
        is.rank = is.rank[1],
        date.data = Sys.Date()
      ) %>%
      dplyr::select(id.season:per_mode, everything())

    if (include_measure_name[1] == F) {
      data %<>%
        dplyr::select(-measure_type)
    }

    if (return_metadata[1] == T) {
      metadata_df <-
        data_frame(
          id.season = id.season,
          conference = conference[1],
          conference_against = conference_against[1],
          date_from,
          date_to,
          division = division[1],
          division_against = division_against[1] ,
          game_scope = game_scope[1],
          game_segment = game_segment[1],
          is.pace_adjusted = is.pace_adjusted[1],
          is.plus_minus = is.plus_minus[1],
          is.rank = is.rank[1],
          last_n_games = last_n_games[1],
          location = location[1],
          measure_type = measure_type[1],
          month = month[1],
          team.selected = t,
          opponent = opponent[1],
          outcome = outcome[1],
          per_mode = per_mode[1],
          period = period[1],
          player_experience = player_experience[1],
          player_position = player_position[1],
          playoff_round = playoff_round[1],
          season_type = season_type[1],
          season_segment = season_segment[1],
          shot_clock_range = shot_clock_range[1],
          starter_bench = shot_clock_range[1]
        )

      data <-
        list(data, metadata_df)

      names(data) <-
        c('data', 'metadata')

    } else {
      if (!t %>% is.na) {
        data %<>%
          mutate(team.selected = t) %>%
          dplyr::select(id.season, team.selected, everything())
      }
      if (!opponent %>% is.na) {
        data %<>%
          mutate(opponent) %>%
          dplyr::select(id.season, opponent, everything())
      }

      if (!conference[1] %>% is.na) {
        data %<>%
          mutate(conference) %>%
          dplyr::select(id.season, conference, everything())
      }

      if (!conference_against[1] %>% is.na) {
        data %<>%
          mutate(conference_against) %>%
          dplyr::select(id.season, conference_against, everything())
      }

      if (!date_to %>% is.na) {
        data %<>%
          mutate(date_to) %>%
          dplyr::select(id.season, date_to, everything())
      }

      if (!date_from %>% is.na) {
        data %<>%
          mutate(date_from) %>%
          dplyr::select(id.season, date_to, everything())
      }

      if (!division[1] %>% is.na) {
        data %<>%
          mutate(division) %>%
          dplyr::select(id.season, division, everything())
      }

      if (!division_against[1] %>% is.na) {
        data %<>%
          mutate(division_against) %>%
          dplyr::select(id.season, division_against, everything())
      }

      if (!game_scope[1] %>% is.na) {
        data %<>%
          mutate(game_scope)
      }

      if (!game_segment[1] %>% is.na) {
        data %<>%
          mutate(game_segment) %>%
          dplyr::select(id.season, game_segment, everything())
      }

      if (last_n_games > 0) {
        data %<>%
          mutate(last_n_games) %>%
          dplyr::select(id.season, last_n_games, everything())
      }

      if (!location[1] %>% is.na) {
        data %<>%
          mutate(location) %>%
          dplyr::select(id.season, location, everything())
      }

      if (month > 0) {
        data %<>%
          mutate(month) %>%
          dplyr::select(id.season, month, everything())
      }

      if (!outcome[1] %>% is.na) {
        data %<>%
          mutate(outcome) %>%
          dplyr::select(id.season, outcome, everything())
      }

      if (period > 0) {
        data %<>%
          mutate(period) %>%
          dplyr::select(id.season, period, everything())
      }

      if (!player_experience[1] %>% is.na) {
        data %<>%
          mutate(player_experience) %>%
          dplyr::select(id.season, player_experience, everything())
      }

      if (!player_position[1] %>% is.na) {
        data %<>%
          mutate(last_n_games) %>%
          dplyr::select(id.season, player_position, everything())
      }

      if (playoff_round > 0) {
        data %<>%
          mutate(playoff_round) %>%
          dplyr::select(id.season, playoff_round, everything())
      }
      if (!shot_clock_range[1] %>% is.na) {
        data %<>%
          mutate(shot_clock_range) %>%
          dplyr::select(id.season, shot_clock_range, everything())
      }

      if (!starter_bench[1] %>% is.na) {
        data %<>%
          mutate(starter_bench) %>%
          dplyr::select(id.season, starter_bench, everything())
      }

      if (return_message == T) {
        "You got " %>%
          paste0(measure_type,
                 " team stat data for the ",
                 id.season,
                 " season") %>%
          message()
      }
      return(data)
    }
  }

get_all_teams_traditional_stat_tables <-
  function(year.season_start = 2015,
           season_type = "Regular Season",
           measures =
             c("Base",
               "Advanced",
               "Scoring",
               "Misc",
               "Four Factors",
               "Opponent"),
           per_mode = "PerGame",
           merge_ids = T) {
    ys <-
      year.season_start

    st <-
      season_type

    pm <-
      per_mode

    all_data <-
      get_nba_team_stat_table(
        season_type = st,
        measure_type = measures[1],
        per_mode = pm,
        include_measure_name = F,
        year.season_start = ys
      )

    exclude <-
      c(
        "age",
        "blk",
        "date.data",
        "fga",
        "fga.blocked",
        "fgm",
        "fouls",
        "fouls.drawn",
        "gp",
        "id.season",
        "is.pace_adjusted",
        "is.rank",
        "losses",
        "min",
        "name.player",
        'team',
        "per_mode",
        "season_type",
        "wins",
        "pct.wins",
        "pct.fg"
      )

    for (m in measures[2:length(measures)]) {
      df <-
        get_nba_traditional_team_season_stat_table(
          season_type = st,
          measure_type = m,
          per_mode = pm,
          include_measure_name = F,
          year.season_start = ys
        )

      df <-
        df[, !names(df) %in% exclude]
      names(df)[names(df) %in% names(all_data)]

      if (m == "Four Factors") {
        df %<>%
          dplyr::select(-c(pct.efg, pct.oreb, ratio.to))
      }

      if (m == "Opponent") {
        df %<>%
          dplyr::select(-c(plus.minus))
      }

      all_data %<>%
        left_join(df)
    }

    if (merge_ids == T) {
      teams_ids <-
        get_teams_ids()

      all_data %<>%
        dplyr::left_join(teams_ids) %>%
        dplyr::select(id.season:id.team, slug.team, everything())

    }

    return(all_data)

  }

get_nba_team_stats_season_per_mode <-
  function(year.season_start = 2015,
           season_type = "Regular Season",
           measure = "Advanced",
           per_modes =  c(
             "Totals",
             "PerGame",
             "MinutesPer",
             "Per48",
             "Per40",
             "Per36",
             "PerMinute",
             "PerPossession",
             "PerPlay",
             "Per100Possessions",
             "Per100Plays"
           ),
           merge_ids = T,....) {
    ys <-
      year.season_start

    st <-
      season_type

    pm <-
      per_mode

    all_data <-
      per_modes %>%
      map(
        function(x)
          get_nba_team_stat_table(
            year.season_start = ys,
            season_type = st,
            per_mode = x,
            measure_type = measures
          )
      ) %>%
      compact() %>%
      bind_rows()
    return(all_data)

  }

get_nba_team_stats_season_by_segment <-
  function(year.season_start = 2015,
           season_type = "Regular Season",
           measure = "Base",
           game_segments = c(NA, "First Half", "Second Half", "Overtime"),
           per_mode = "Per100Possessions",
           merge_ids = T,....) {
    ys <-
      year.season_start

    st <-
      season_type

    pm <-
      per_mode

    all_data <-
      game_segments %>%
      map(
        function(x)
          get_nba_team_stat_table(
            year.season_start = ys,
            season_type = st,
            per_mode = per_mode,
            game_segment = x,
            measure_type = measure
          )
      ) %>%
      compact() %>%
      bind_rows()

    all_data %<>%
      mutate(game_segment = ifelse(game_segment %>% is.na, 'All', game_segment)) %>%
      dplyr::rename(segment.game = game_segment) %>%
      dplyr::select(id.season, team, segment.game, everything())

    return(all_data)

  }

get_column_types <- function() {
  column_rank_df <-
    data_frame(
      name.column = c(
        "id.team",
        "gp",
        "wins",
        "losses",
        "pct.wins",
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
        "tov",
        "stl",
        "blk",
        "fga.blocked",
        "fouls",
        "fouls.drawn",
        "pts",
        "plus.minus",
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
        "pace",
        "pie",
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
        "pts.off_to",
        "pts.2nd_chance",
        "pts.fastbreak",
        "pts.paint",
        "pts.off_to.opponent",
        "pts.2nd_chance.opponent",
        "pts.fastbreak.opponent",
        "pts.paint.opponent",
        "rate.fta",
        "pct.efg.opp",
        "rate.fta.opp",
        "pct.tov.opp",
        "pct.oreb.opp",
        "fgm.opp",
        "fga.opp",
        "pct.fg.opp",
        "fg3m.opp",
        "fg3a.opp",
        "pct.fg3.opp",
        "ftm.opp",
        "fta.opp",
        "pct.ft.opp",
        "oreb.opp",
        "dreb.opp",
        "rep.opp",
        "ast.opp",
        "tov.opp",
        "stl.opp",
        "blk.opp",
        "fga.blocked.opp",
        "fouls.opp",
        "fouls.drawn.opp",
        "pts.opp"
      )
    ) %>%
    mutate(is.desc.rank = ifelse(name.column %like% '.opp', F, T))
  return(column_rank_df)
}

get_team_ranks <- function(data, merge_data = T) {
  column_rank_df <-
    get_column_types()

  desc_columns <-
    column_rank_df %>%
    dplyr::filter(is.desc.rank == T) %>%
    dplyr::filter(!name.column == 'id.team') %>%
    .$name.column

  asc_columns <-
    column_rank_df %>%
    dplyr::filter(is.desc.rank == F) %>%
    dplyr::filter(!name.column == 'id.team') %>%
    .$name.column

  ranks_df <-
    data %>%
    mutate_each_(funs(dense_rank(desc(.))), vars = desc_columns) %>%
    mutate_each_(funs(dense_rank(.)), vars = asc_columns)

  names(ranks_df)[7:length(names(ranks_df))] %<>%
    paste0('.rank')

  if (merge_data == T) {
    data %<>%
      left_join(ranks_df) %>%
      mutate(date.data = Sys.Date())
  } else {
    data <-
      ranks_df %>%
      mutate(date.data = Sys.Date())
  }
  return(data)
}
