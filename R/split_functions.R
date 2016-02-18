source('utilities.R')
function_packages <-
  c("dplyr", "magrittr", "tidyr", "purrr", "stringr",
    'jsonlite')


# teams -------------------------------------------------------------------


get_team_id_df <-
  function() {
    team_id_df <-
      data_frame(
        team = c(
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
          "Baltimore Bullets",
          "Chicago Stags",
          "Cleveland Rebels",
          "Denver Nuggets",
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
        ),
        id.team = c(
          1610612737,
          1610612737,
          1610612737,
          1610612737,
          1610612738,
          1610612751,
          1610612751,
          1610612751,
          1610612766,
          1610612766,
          1610612741,
          1610612739,
          1610612742,
          1610612743,
          1610612765,
          1610612765,
          1610612744,
          1610612744,
          1610612744,
          1610612745,
          1610612745,
          1610612754,
          1610612746,
          1610612746,
          1610612746,
          1610612747,
          1610612747,
          1610612763,
          1610612763,
          1610612748,
          1610612749,
          1610612750,
          1610612740,
          1610612740,
          1610612740,
          1610612752,
          1610612760,
          1610612760,
          1610612753,
          1610612755,
          1610612755,
          1610612756,
          1610612757,
          1610612758,
          1610612758,
          1610612758,
          1610612758,
          1610612758,
          1610612759,
          1610612761,
          1610612762,
          1610612762,
          1610612764,
          1610612764,
          1610612764,
          1610612764,
          1610612764,
          1610612764,
          1610610023,
          1610610024,
          1610610025,
          1610610026,
          1610610027,
          1610610028,
          1610610029,
          1610610030,
          1610610031,
          1610610032,
          1610610033,
          1610610034,
          1610610035,
          1610610036,
          1610610037
        )

      )
    return(team_id_df)
  }

get_team_season_stat_splits <-
  function(team = "Brooklyn Nets",
           year.season_start = 2015,
           season_type = c('Regular Season', 'Pre Season', 'Playoffs', 'All Star'),
           measure_type = c("Base",
                            "Advanced",
                            "Misc",
                            "Scoring",
                            "Four Factors",
                            "Opponent"),
           per_mode =  c(
             "PerGame",
             "Totals",
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
           is.plus_minus = c(F, T),
           is.pace_adjusted = F,
           period = 0,
           is.rank = F,
           game_segment = c(NA, "First Half", "Second Half", "Overtime"),
           division_against = c(NA,
                                "Atlantic",
                                "Central",
                                "Northwest",
                                "Pacific",
                                "Southeast",
                                "Southwest"),
           conference_against = c(NA, "East", "West"),
           date_from = NA,
           date_to = NA,
           last_n_games = 0,
           location = c(NA, "Home", "Road"),
           month = 0,
           season_segment = c(NA, "Post All-Star", "Pre All-Star"),
           opponent = NA,
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
           return_message = T,
           ...) {
    install_needed_packages(function_packages)
    load_needed_packages(function_packages)
    team_id_df <-
      get_team_id_df()
    t <-
      team

    if (!t %>% str_to_lower() %in% (team_id_df$team %>% str_to_lower())) {
      stop(t %>% paste0(" is not a valid team"))
    }

    team_stem <-
      team_id_df %>%
      dplyr::filter(team %>% str_to_lower() == t %>% str_to_lower()) %>%
      .$id.team %>% .[1]

    base <-
      'http://stats.nba.com/stats/teamdashboardbygeneralsplits?'
    if (year.season_start < 1996) {
      stop("Sorry data only goes back to the 1996-97 Season")
    }

    id.season <-
      year.season_start %>%
      paste0("-", (year.season_start + 1) %>% substr(3, 4))

    if (!opponent %>% is.na) {
      if (!opponent %>% str_to_lower() %in% team_id_df$team %>% str_to_lower()) {
        "Opponent must be either " %>%
          paste0(team_id_df$team %>% paste0(collapse = ', ')) %>%
          stop()
      }

      opponent_stem <-
        team_id_df %>%
        mutate(team = team %>% str_to_lower()) %>%
        dplyr::filter(team == opponent %>% str_to_lower()) %>%
        .$team_id
    } else {
      opponent_stem <-
        0
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


    if (!date_from %>% is.na) {
      date_from_stem <-
        date_from %>% clean_to_stem()
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

      if (!season_segment[1] %in% SeasonSegment) {
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

      if (!season_type[1] %in% SeasonType) {
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
    url_json <-
      base %>%
      paste0(
        '&DateFrom=',
        date_from_stem,
        '&DateTo=',
        date_to_stem,
        '&GameSegment=',
        game_segment_stem,
        '&LastNGames=',
        last_n_games_stem,
        '&LeagueID=00&Location=',
        location_stem,
        '&MeasureType=',
        measure_type_stem,
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

    data <-
      1:(length(json_data$resultSets$rowSet)) %>%
      map(function(x)
        json_data$resultSets$rowSet[x] %>%
          compact %>%
          data.frame %>%
          tbl_df %>%
          .[, -3]) %>%
      bind_rows()

    headers <-
      json_data$resultSets$headers[1] %>%
      flatten_chr() %>%
      .[c(1:2, 4:length(.))]

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

    data %<>%
      dplyr::select(-c(cfid, cfparms))

    data %<>%
      mutate_each_(funs(as.numeric),
                   vars = data %>% dplyr::select(-contains("group")) %>% names)

    data %<>%
      mutate(
        id.season,
        team,
        season.type = season_type[1],
        measure.type = measure_type[1],
        per.mode = per_mode[1]
      ) %>%
      dplyr::select(id.season:per.mode, everything())

    if (!opponent %>% is.na) {
      data %<>%
        mutate(opponent) %>%
        dplyr::select(id.season:per.mode, opponent, everything())
    }


    if (!conference_against[1] %>% is.na) {
      data %<>%
        mutate(conference.against = conference_against[1]) %>%
        dplyr::select(id.season:per.mode, conference.against, everything())
    }

    if (!date_to %>% is.na) {
      data %<>%
        mutate(date.to = date_to) %>%
        dplyr::select(id.season:per.mode, date.to, everything())
    }

    if (!date_from %>% is.na) {
      data %<>%
        mutate(date.from = date_from) %>%
        dplyr::select(id.season:per.mode,  date.from, everything())
    }

    if (!division_against[1] %>% is.na) {
      data %<>%
        mutate(division.against = division_against[1]) %>%
        dplyr::select(id.season, division_against, everything())
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
        dplyr::select(id.season:per.mode,  outcome, everything())
    }

    if (period > 0) {
      data %<>%
        mutate(period) %>%
        dplyr::select(id.season:per.mode,  period, everything())
    }
    if (playoff_round > 0) {
      data %<>%
        mutate(playoff.round = playoff_round) %>%
        dplyr::select(id.season:per.mode,  playoff.roung, everything())
    }
    if (!shot_clock_range[1] %>% is.na) {
      data %<>%
        mutate(range.shotclock = shot_clock_range) %>%
        dplyr::select(id.season:per.mode, range.shotclock, everything())
    }


    if (return_message == T) {
      "You got " %>%
        paste0(
          team,
          " ",
          measure_type[1] %>% str_to_lower(),
          " team split stat data for the ",
          id.season,
          " season"
        ) %>%
        message()
    }
    return(data)
  }

get_teams_season_stat_splits <-
  function(teams = c(
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
  year.season_start = 2015,
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
  is.plus_minus = c(F, T),
  is.pace_adjusted = F,
  period = 0,
  is.rank = F,
  game_segment = c(NA, "First Half", "Second Half", "Overtime"),
  division_against = c(NA,
                       "Atlantic",
                       "Central",
                       "Northwest",
                       "Pacific",
                       "Southeast",
                       "Southwest"),
  conference_against = c(NA, "East", "West"),
  date_from = NA,
  date_to = NA,
  last_n_games = 0,
  location = c(NA, "Home", "Road"),
  month = 0,
  season_segment = c(NA, "Post All-Star", "Pre All-Star"),
  opponent = NA,
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
  message = T) {
    all_data <-
      teams %>%
      purrr::map(
        function(x)
          get_team_season_stat_splits(
            team = x,
            year.season_start = year.season_start,
            season_type = season_type,
            measure_type = measure_type,
            per_mode = per_mode,
            is.plus_minus = is.plus_minus,
            is.pace_adjusted = is.pace_adjusted,
            period = period,
            is.rank = is.rank,
            game_segment = game_segment[1],
            division_against = division_against,
            conference_against = conference_against,
            date_from = date_from,
            date_to = date_to,
            last_n_games = last_n_games,
            location = location,
            month = month,
            season_segment = season_segment,
            opponent = opponent,
            outcome = outcome,
            shot_clock_range = shot_clock_range,
            playoff_round = playoff_round,
            return_message = message
          )
      ) %>%
      compact %>%
      bind_rows

    return(all_data)

  }

# players -----------------------------------------------------------------


get_nba_players_ids <- function(active_only = c(F, T)) {
  if (Sys.Date() %>% lubridate::month >= 10) {
    year.season_start <-
      Sys.Date() %>%
      year
  } else {
    year.season_start <-
      Sys.Date() %>%
      year - 1
  }

  id.season <-
    year.season_start %>%
    paste0("-", (year.season_start + 1) %>% substr(start = 3, stop = 4))
  players.url <-
    'http://stats.nba.com/stats/commonallplayers?IsOnlyCurrentSeason=0&LeagueID=00&Season=' %>%
    paste0(id.season)

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
      city.team = ifelse(city.team == '', NA, city.team),
      code.team = ifelse(code.team == '', NA, code.team),
      slug.team = ifelse(slug.team == '', NA, slug.team),
      team = ifelse(city.team %>% is.na, NA, paste(city.team, team)),
      seasons.played = year.to - year.from,
      url.player = id.player %>% paste0('http://stats.nba.com/player/#!/', .),
      image.player = id.player %>% paste0('http://stats.nba.com/media/players/132x132/', ., '.png')
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

  return(data)
}

get_player_season_stat_split <-
  function(player = "Brook Lopez",
           year.season_start = 2015,
           season_type = c('Regular Season', 'Pre Season', 'Playoffs', 'All Star'),
           measure_type = c("Base",
                            "Advanced",
                            "Misc",
                            "Scoring",
                            "Four Factors",
                            "Opponent"),
           per_mode =  c(
             "PerGame",
             "Totals",
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
           is.plus_minus = c(F, T),
           is.pace_adjusted = F,
           period = 0,
           is.rank = F,
           game_segment = c(NA, "First Half", "Second Half", "Overtime"),
           division_against = c(NA,
                                "Atlantic",
                                "Central",
                                "Northwest",
                                "Pacific",
                                "Southeast",
                                "Southwest"),
           conference_against = c(NA, "East", "West"),
           date_from = NA,
           date_to = NA,
           last_n_games = 0,
           location = c(NA, "Home", "Road"),
           month = 0,
           season_segment = c(NA, "Post All-Star", "Pre All-Star"),
           opponent = NA,
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
           return_message = T) {
    package_df <-
      data.frame(installed.packages())

    if ('curlconverter' %in% package_df$Package == F) {
      devtools::install_github('hrbrmstr/curlconverter')
    }
    packages <-
      #need all of these installed including some from github
      c(
        'dplyr',
        'magrittr',
        'jsonlite',
        'httr',
        'curlconverter',
        'tidyr',
        'stringr',
        'lubridate'
      )
    options(warn = -1)
    install_needed_packages(packages)
    load_needed_packages(packages)

    player_id_df <-
      get_nba_players_ids()

    if (!(player %>% str_to_lower() %in% (player_id_df$name.player %>% str_to_lower()))) {
      stop("Sorry " %>%
             paste0(player, ' is an invalid player'))
    }

    player_df <-
      player_id_df %>%
      dplyr::filter(name.player %>% str_to_lower == player %>% str_to_lower())

    if (player_df %>% nrow > 1) {
      player_df %<>%
        dplyr::filter(is.active_player == T)
    }
    player_stem <-
      player_df$id.player

    base <-
      'http://stats.nba.com/stats/playerdashboardbygeneralsplits?'

    if (year.season_start < 1996) {
      stop("Sorry data only goes back to the 1996-97 Season")
    }

    id.season <-
      year.season_start %>%
      paste0("-", (year.season_start + 1) %>% substr(3, 4))

    if (!opponent %>% is.na) {
      team_id_df <-
        get_team_id_df()
      if (!opponent %>% str_to_lower() %in% team_id_df$team %>% str_to_lower()) {
        "Opponent must be either " %>%
          paste0(team_id_df$team %>% paste0(collapse = ', ')) %>%
          stop()
      }

      opponent_stem <-
        team_id_df %>%
        mutate(team = team %>% str_to_lower()) %>%
        dplyr::filter(team == opponent %>% str_to_lower()) %>%
        .$team_id
    } else {
      opponent_stem <-
        0
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


    if (!date_from %>% is.na) {
      date_from_stem <-
        date_from %>% clean_to_stem()
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

      if (!season_segment[1] %in% SeasonSegment) {
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

      if (!season_type[1] %in% SeasonType) {
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
    url_json <-
      base %>%
      paste0(
        '&DateFrom=',
        date_from_stem,
        '&DateTo=',
        date_to_stem,
        '&GameSegment=',
        game_segment_stem,
        '&LastNGames=',
        last_n_games_stem,
        '&LeagueID=00&Location=',
        location_stem,
        '&MeasureType=',
        measure_type_stem,
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
        '&playerId=',
        player_stem,
        '&VsConference=',
        conference_against_stem,
        '&VsDivision=',
        division_against_stem,
        sep = ''
      )

    header_code <-
      " -H 'Pragma: no-cache' -H 'DNT: 1' -H 'Accept-Encoding: gzip, deflate, sdch' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/49.0.2623.39 Safari/537.36' -H 'Accept: application/json, text/plain, */*' -H 'Referer: http://stats.nba.com/scores/' -H 'Cookie: s_cc=true; s_fid=18F437D0DE57C7DA-0D8E18725712DB1E; s_sq=%5B%5BB%5D%5D' -H 'Connection: keep-alive' -H 'Cache-Control: no-cache' --compressed"
    request_url <-
      "curl '" %>%
      paste0(url_json, "'", header_code)

    nba_url <-
      request_url %>%
      straighten() %>%
      make_req(quiet = TRUE) %>%
      suppressMessages()

    json_data <-
      nba_url[[1]]() %>%
      content(as = "parsed") %>%
      toJSON() %>%
      fromJSON(simplifyDataFrame = T, flatten = T)

    data <-
      1:(length(json_data$resultSets$rowSet)) %>%
      map(function(x)
        json_data$resultSets$rowSet[x] %>%
          compact %>%
          data.frame %>%
          tbl_df) %>%
      bind_rows()

    headers <-
      json_data$resultSets$headers[1] %>%
      flatten_chr() %>%
      unique()

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

    data %<>%
      dplyr::select(-c(cfid, cfparms))

    data %<>%
      mutate_each_(funs(. %>% as.numeric %>% abs),
                   vars = data %>% dplyr::select(-contains("group")) %>% names)

    data %<>%
      mutate(
        id.season,
        name.player = player_df$name.player[1],
        season.type = season_type[1],
        measure.type = measure_type[1],
        per.mode = per_mode[1]
      ) %>%
      dplyr::select(id.season:per.mode, everything())

    if (!opponent %>% is.na) {
      data %<>%
        mutate(opponent) %>%
        dplyr::select(id.season:per.mode, opponent, everything())
    }


    if (!conference_against[1] %>% is.na) {
      data %<>%
        mutate(conference.against = conference_against[1]) %>%
        dplyr::select(id.season:per.mode, conference.against, everything())
    }

    if (!date_to %>% is.na) {
      data %<>%
        mutate(date.to = date_to) %>%
        dplyr::select(id.season:per.mode, date.to, everything())
    }

    if (!date_from %>% is.na) {
      data %<>%
        mutate(date.from = date_from) %>%
        dplyr::select(id.season:per.mode,  date.from, everything())
    }

    if (!division_against[1] %>% is.na) {
      data %<>%
        mutate(division.against = division_against[1]) %>%
        dplyr::select(id.season, division_against, everything())
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
        dplyr::select(id.season:per.mode,  outcome, everything())
    }

    if (period > 0) {
      data %<>%
        mutate(period) %>%
        dplyr::select(id.season:per.mode,  period, everything())
    }
    if (playoff_round > 0) {
      data %<>%
        mutate(playoff.round = playoff_round) %>%
        dplyr::select(id.season:per.mode,  playoff.roung, everything())
    }
    if (!shot_clock_range[1] %>% is.na) {
      data %<>%
        mutate(range.shotclock = shot_clock_range) %>%
        dplyr::select(id.season:per.mode, range.shotclock, everything())
    }


    if (return_message == T) {
      "You got " %>%
        paste0(
          player,
          " ",
          measure_type[1] %>% str_to_lower(),
          " player's split stat data for the ",
          id.season,
          " season"
        ) %>%
        message()
    }
    return(data)

  }

get_players_season_stat_splits <-
  function(players = c(
    "Brook Lopez",
    "Joe Johnson",
    "Kyle Lowry"
  ),
  year.season_start = 2015,
  season_type = c('Regular Season', 'Pre Season', 'Playoffs', 'All Star'),
  measure_type = c("Base",
                   "Advanced",
                   "Misc",
                   "Scoring",
                   "Four Factors",
                   "Opponent"),
  per_mode =  c(
    "PerGame",
    "Totals",
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
  is.plus_minus = c(F, T),
  is.pace_adjusted = F,
  period = 0,
  is.rank = F,
  game_segment = c(NA, "First Half", "Second Half", "Overtime"),
  division_against = c(NA,
                       "Atlantic",
                       "Central",
                       "Northwest",
                       "Pacific",
                       "Southeast",
                       "Southwest"),
  conference_against = c(NA, "East", "West"),
  date_from = NA,
  date_to = NA,
  last_n_games = 0,
  location = c(NA, "Home", "Road"),
  month = 0,
  season_segment = c(NA, "Post All-Star", "Pre All-Star"),
  opponent = NA,
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
  message = T) {
    all_data <-
      players %>%
      purrr::map(
        function(x)
          get_player_season_stat_split(
            player = x,
            year.season_start = year.season_start,
            season_type = season_type,
            measure_type = measure_type,
            per_mode = per_mode,
            is.plus_minus = is.plus_minus,
            is.pace_adjusted = is.pace_adjusted,
            period = period,
            is.rank = is.rank,
            game_segment = game_segment[1],
            division_against = division_against,
            conference_against = conference_against,
            date_from = date_from,
            date_to = date_to,
            last_n_games = last_n_games,
            location = location,
            month = month,
            season_segment = season_segment,
            opponent = opponent,
            outcome = outcome,
            shot_clock_range = shot_clock_range,
            playoff_round = playoff_round,
            return_message = message
          )
      ) %>%
      compact %>%
      bind_rows

    return(all_data)

  }
