function_packages <-
  c(
    'dplyr',
    'magrittr',
    'data.table',
    'jsonlite',
    'tidyr',
    'stringr',
    'lubridate',
    'purrr',
    'stringr',
    'tidyr'
  )

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


get_nba_day_score_json_data <- function(game_date) {
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
  if (!game_date %>% class == "character") {
    stop("Make sure you enter date as a character!!")
  }

  if (game_date %>%  lubridate::guess_formats("mdy") %>% length == 0) {
    stop("Enter valid date in month, day, year format")
  }

  parsed_date <-
    game_date %>%
    mdy %>%
    as.Date

  if (parsed_date %>%  month %in% c(7:9)) {
    stop("Sorry the NBA doesn't play have games in the summer")
  }

  if (parsed_date %>% year < 1948) {
    stop("Sorry data starts in the 1948 season")
  }

  date_stem <-
    parsed_date %>%
    as.character %>%
    str_split('\\-') %>%
    unlist %>%
    .[c(2, 3, 1)] %>%
    paste0(collapse = '%2F')

  base <-
    'http://stats.nba.com/stats/scoreboardV2?DayOffset=0&LeagueID=00&gameDate='

  url_games <-
    base %>%
    paste0(date_stem)

  header_code <-
    " -H 'Pragma: no-cache' -H 'DNT: 1' -H 'Accept-Encoding: gzip, deflate, sdch' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/49.0.2623.39 Safari/537.36' -H 'Accept: application/json, text/plain, */*' -H 'Referer: http://stats.nba.com/scores/' -H 'Cookie: s_cc=true; s_fid=18F437D0DE57C7DA-0D8E18725712DB1E; s_sq=%5B%5BB%5D%5D' -H 'Connection: keep-alive' -H 'Cache-Control: no-cache' --compressed"

  request_url <-
    "curl '" %>%
    paste0(url_games, "'", header_code)

  show_req_headers <-
    request_url %>%
    make_req(quiet = TRUE)

  res <-
    show_req_headers()

  json_data <-
    content(res, as = "parsed") %>%
    toJSON() %>%
    fromJSON(simplifyDataFrame = T)
  return(json_data)

}

get_nba_day_score_json_data_safe <-
  failwith(NULL, get_nba_day_score_json_data)

get_day_nba_games <- function(date, return_message = T) {
  if (!'date' %>% exists()) {
    stop("Please enter a valid date")
  }
  json_data <-
    get_nba_day_score_json_data_safe(game_date = date)
  install_needed_packages(function_packages)
  load_needed_packages(function_packages)
  if (json_data %>% length == 0) {
    stop("No games for " %>% paste0(date))
  }
  if(json_data$resultSets$rowSet[[1]] %>%
     purrr::flatten() %>% length  > 0) {
  game_set <-
    json_data$resultSets$rowSet[[1]] %>%
    purrr::flatten()

  games <-
    json_data$resultSets$rowSet[[1]] %>%
    purrr::flatten() %>%
    length() %>%  {
      . / 14
    }

  vars_to_check <-
    1:(json_data$resultSets$rowSet[[1]] %>%
         purrr::flatten() %>%
         length())

  for (var in vars_to_check) {
    if (game_set[var] %>%
        unlist %>% length == 0) {
      game_set[var] <-
        NA
    }
  }
  items <-
    c(
      "game_date_est",
      "sequence.game",
      "id.game",
      "id.game_status",
      "text.games_status",
      "gamecode",
      "id.team.home",
      "id.team.away",
      "year.season_start",
      "period.live",
      "live_pc_time",
      "id.tv_broadcast",
      "period.live_broadcast",
      "status.wh"
    ) %>%
    rep(games)

  raw_data <-
    game_set %>%
    flatten_chr() %>%
    data.frame(item = items, value = .) %>%
    tbl_df

  game_data <-
    raw_data %>%
    mutate(game_date_est = ifelse(item == 'game_date_est', value, NA)) %>%
    tidyr::fill(game_date_est) %>%
    dplyr::filter(!item == 'game_date_est') %>%
    mutate(sequence.game = ifelse(item == 'sequence.game', value, NA)) %>%
    tidyr::fill(sequence.game) %>%
    dplyr::filter(!item == 'sequence.game') %>%
    spread(item, value)

  year_season_end <-
    game_data$year.season_start %>% unique %>% as.numeric() + 1

  id.season <-
    game_data$year.season_start %>% unique %>%
    paste0('-', year_season_end %>% substr(3, 4))

  game_data %<>%
    separate(gamecode, sep = '\\/', c('date.game', 'teamslugs')) %>%
    mutate(
      slug.team.away = teamslugs %>% substr(start = 1, stop = 3),
      slug.team.home = teamslugs %>% substr(4, 6),
      date.game = date.game %>% as.Date('%Y%m%d'),
      is.final = text.games_status %>% str_detect("Final"),
      id.season
    ) %>%
    dplyr::select(
      -c(
        game_date_est,
        live_pc_time,
        period.live_broadcast,
        teamslugs,
        text.games_status
      )
    ) %>%
    dplyr::select(
      id.season,
      date.game,
      sequence.game,
      id.game,
      is.final,
      slug.team.home,
      slug.team.away,
      everything()
    )

  names_numeric_cols <-
    game_data %>%
    dplyr::select(
      -c(
        id.season,
        date.game,
        is.final,
        id.game,
        slug.team.home,
        slug.team.away,
        id.tv_broadcast
      )
    ) %>%
    names

  game_data %<>%
    mutate_each_(funs(as.numeric), vars = names_numeric_cols) %>%
    dplyr::select(-year.season_start) %>%
    arrange(sequence.game)

  if (return_message == T) {
    "You got all " %>%
      paste0(game_data %>% nrow,
             ' NBA games for ',
             game_data$date.game %>% unique) %>%
      message()
  }

  return(game_data)
  } else {
    "Sorry no games for " %>%
      paste0(date) %>%
      message()
  }
}

get_day_nba_games_safe <-
  failwith(NULL, get_day_nba_games)

get_days_nba_games <-
  function(dates, message = T) {
    if(!'dates' %>% exists()) {
      stop("Please enter game dates")
    }
    all_games <-
      dates %>%
      purrr::map(function(x)
        get_day_nba_games_safe(date = x, return_message = F)
        ) %>%
      compact %>%
      bind_rows
    if (message == T) {
       "You got " %>%
        paste0(all_games$id.game %>% unique %>% length, " games from ",
               dates %>% min(), " to ", dates %>% max) %>%
      message
    }
    return(all_games)
  }

get_day_nba_game_scores <-  function(date, return_message = T, merge_data = T) {
  if (!'date' %>% exists()) {
    stop("Please enter a valid date")
  }
  json_data <-
    get_nba_day_score_json_data_safe(game_date = date)
  install_needed_packages(function_packages)
  load_needed_packages(function_packages)
  if (json_data %>% length == 0) {
    stop("No games for " %>% paste0(date))
  }

  days.scores <-
    json_data$resultSets$rowSet[[2]] %>%
    data.frame() %>%
    tbl_df

  names(days.scores) <-
    c(
      "date.game",
      "sequence.game",
      "id.game",
      "id.team",
      "slug.team",
      "city.team",
      "team.record",
      "pts.qtr1",
      "pts.qtr2",
      "pts.qtr3",
      "pts.qtr4",
      "pts.ot1",
      "pts.ot2",
      "pts.ot3",
      "pts.ot4",
      "pts.ot5",
      "pts.ot6",
      "pts.ot7",
      "pts.ot8",
      "pts.ot9",
      "pts.ot10",
      "pts",
      "pct.fg",
      "pct.ft",
      "pct.fg3",
      "ast",
      "reb",
      "tov"
    )

  days.scores %<>%
    mutate(date.game = date.game %>% as.Date('%Y-%m-%d'))

  if (days.scores$date.game %>% unique %>% month < 7) {
    year.season.start <-
      days.scores$date.game %>% unique %>% year %>% as.numeric() - 1
  } else {
    year.season.start <-
      days.scores$date.game %>% unique %>% year %>% as.numeric()
  }

  id.season <-
    year.season.start %>%
    paste0("-", substr(year.season.start + 1, start = 3, 4))

  days.scores %<>%
    mutate(id.season) %>%
    separate(team.record, sep = '\\-', c('wins', 'losses')) %>%
    dplyr::select(id.season, date.game, everything())

  names_numeric_cols <-
    days.scores %>%
    dplyr::select(-c(id.season, date.game, slug.team, city.team, id.game)) %>%
    names

  days.scores %<>%
    mutate_each_(funs(as.numeric), vars = names_numeric_cols)

  days.scores %<>%
    left_join(
      days.scores %>%
        group_by(id.game) %>%
        dplyr::filter(pts == max(pts)) %>%
        dplyr::select(id.game, slug.winner = slug.team)
    ) %>%
    mutate(is.win = ifelse(slug.winner == slug.team, T, F)) %>%
    dplyr::select(id.season:id.game, is.win, everything())

  if (merge_team_data == T) {
    game_date <-
      date
    days_games_df <-
      get_day_nba_games_safe(date = game_date, return_message = F)

    days.scores <-
      days_games_df %>%
      dplyr::select(id.game,
                    slug.team.home,
                    slug.team.away
      ) %>%
      left_join(days.scores) %>%
      suppressMessages() %>%
      dplyr::select(
        id.season,
        date.game,
        id.game,
        everything()
      ) %>%
      mutate(is.home_team = ifelse(slug.team == slug.team.home, T, F),
             slug.opponent = ifelse(slug.team == slug.team.home, slug.team.away, slug.team.home)
      ) %>%
      dplyr::select(-c(slug.team.home, slug.team.away)) %>%
      dplyr::select(id.season:sequence.game, id.game, slug.team, slug.opponent, is.home_team, is.win, everything())
  }

  if (return_message == T) {
    "You got all " %>%
      paste0(
        days.scores$sequence.game %>% length,
        ' NBA game scores for ',
        days.scores$date.game %>% unique
      ) %>%
      message()
  }

  return(days.scores)

}

get_day_nba_game_scores_safe <-
  failwith(NULL, get_day_nba_game_scores)

get_days_nba_scores <-
  function(dates, merge = T,
           message = T) {
    if(!'dates' %>% exists()) {
      stop("Please enter game dates")
    }
    all_scores <-
      dates %>%
      purrr::map(function(x)
        get_day_nba_game_scores(date = x, return_message = F, merge_data =  merge)
      ) %>%
      compact %>%
      bind_rows
    if (message == T) {
      "You got " %>%
        paste0(all_scores$id.game %>% unique %>% length, " game scores from ",
               dates %>% min(), " to ", dates %>% max) %>%
        message
    }
    return(all_scores)
  }

get_day_nba_teams_last_meeting <- function(date,
                                           merge_team_data = T,
                                           return_message = T) {
  if (!'date' %>% exists()) {
    stop("Please enter a valid date")
  }

  json_data <-
    get_nba_day_score_json_data_safe(game_date = date)
  install_needed_packages(function_packages)
  load_needed_packages(function_packages)
  if (json_data %>% length == 0) {
    stop("No games for " %>% paste0(date))
  }

  last_meeting_data <-
    json_data$resultSets$rowSet[[4]] %>%
    data.frame() %>%
    tbl_df

  names(last_meeting_data) <- c(
    "id.game",
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
    "pts.team.away.last"
  )

  last_meeting_data %<>%
    mutate(date.game.last = date.game.last %>% as.Date('%Y-%m-%d'))

  if (date %>% mdy %>% month < 7) {
    year.season.start <-
      date %>% mdy %>% year %>% as.numeric() - 1
  } else {
    year.season.start <-
      date %>% mdy %>% year %>% as.numeric()
  }
  id.season <-
    year.season.start %>%
    paste0("-", substr(year.season.start + 1, start = 3, 4))

  last_meeting_data %<>%
    mutate(id.season)

  names_numeric_cols <-
    last_meeting_data %>%
    dplyr::select(c(
      id.team.home.last,
      id.team.away.last,
      pts.team.home.last,
      pts.team.away.last
    )) %>%
    names

  last_meeting_data %<>%
    mutate_each_(funs(as.numeric), vars = names_numeric_cols) %>%
    mutate(date.game = date %>% mdy %>% as.Date()) %>%
    dplyr::select(
      id.season,
      date.game,
      date.game.last,
      slug.team.home.last,
      slug.team.away.last,
      pts.team.home.last,
      pts.team.away.last,
      everything()
    )

  if (merge_team_data == T) {
    game_date <-
      date

    days_games_df <-
      get_day_nba_games(date = game_date, return_message = F)

    last_meeting_data <-
      days_games_df %>%
      dplyr::select(id.game, slug.team.home, slug.team.away) %>%
      left_join(last_meeting_data) %>%
      suppressMessages() %>%
      dplyr::select(id.season,
                    date.game,
                    slug.team.home,
                    slug.team.away,
                    everything())
  }

  if (return_message == T) {
    "You got all " %>%
      paste0(
        last_meeting_data$id.game %>% length,
        ' last game histories ',
        last_meeting_data$date.game %>% unique
      ) %>%
      message()
  }

  return(last_meeting_data)

}

get_day_nba_teams_last_meeting_safe <-
  failwith(NULL, get_day_nba_teams_last_meeting)

get_days_nba_teams_last_meeting <-
  function(dates,
           merge = T,
           message = T) {
    if(!'dates' %>% exists()) {
      stop("Please enter game dates")
    }
    all_last_meeetings <-
      dates %>%
      purrr::map(function(x)
        get_day_nba_teams_last_meeting_safe(date = x, merge = T,
                                            return_message = F)
      ) %>%
      compact %>%
      bind_rows
    if (message == T) {
      "You got " %>%
        paste0(all_last_meeetings$id.game %>% unique %>% length, " games of last meeting data from ",
               dates %>% min(), " to ", dates %>% max) %>%
        message
    }
    if(all_last_meeetings %>% nrow() > 0){
    return(all_last_meeetings)
    }
  }

get_day_nba_league_standings <- function(date,
                                       return_message = T) {
  if (!'date' %>% exists()) {
    stop("Please enter a valid date")
  }

  json_data <-
    get_nba_day_score_json_data_safe(game_date = date)
  install_needed_packages(function_packages)
  load_needed_packages(function_packages)
  if (json_data %>% length == 0) {
    stop("No games for " %>% paste0(date))
  }

  standings_df <-
    json_data$resultSets$rowSet[[5]] %>%
    data.frame() %>%
    tbl_df %>%
    bind_rows(json_data$resultSets$rowSet[[6]] %>%
                data.frame() %>%
                tbl_df)

  names(standings_df) <-
    c(
      "id.team",
      "id.league",
      "id.season",
      "date.game",
      "conference",
      "city.team",
      "games",
      "wins",
      "losses",
      "pct.w",
      "home.record",
      "road.record"
    )
  if (date %>% mdy %>% month < 7) {
    year.season.start <-
      date %>% mdy %>% year %>% as.numeric() - 1
  } else {
    year.season.start <-
      date %>% mdy %>% year %>% as.numeric()
  }
  id.seasons <-
    year.season.start %>%
    paste0("-", substr(year.season.start + 1, start = 3, 4))

  standings_df %<>%
    dplyr::select(-c(id.league, id.season)) %>%
    mutate(id.season = id.seasons) %>%
    separate(home.record,
             sep = '\\-',
             into = c('wins.home', 'losses.home')) %>%
    separate(road.record,
             sep = '\\-',
             into = c('wins.away', 'losses.away'))

  names_numeric_cols <-
    standings_df %>%
    dplyr::select(-c(conference, city.team, date.game, id.season)) %>%
    names

  standings_df %<>%
    mutate_each_(funs(as.numeric), vars = names_numeric_cols) %>%
    mutate(id.season,
           date.game = date.game %>% as.Date('%m/%d/%Y')) %>%
    dplyr::select(id.season, date.game, everything())


  standings_df %<>%
    group_by(conference) %>%
    mutate(rank.team.conference = dplyr::dense_rank(desc(wins))) %>%
    dplyr::select(
      id.season:date.game,
      conference,
      city.team,
      id.team,
      rank.team.conference,
      city.team,
      everything()
    ) %>%
    ungroup


  if (return_message == T) {
    "You NBA standings data for " %>%
      paste0(date) %>%
      message()
  }

  return(standings_df)

}

get_day_nba_league_standings_safe <-
  failwith(NULL, get_day_nba_league_standings)

get_days_nba_league_standings <-
  function(dates,
           message = T) {
    if(!'dates' %>% exists()) {
      stop("Please enter game dates")
    }
    all_standings <-
      dates %>%
      purrr::map(function(x)
        get_day_nba_league_standings_safe(date = x,
                                            return_message = F)
      ) %>%
      compact %>%
      bind_rows

    if (message == T) {
      "You got team standings data from " %>%
        paste0(dates %>% min(), " to ", dates %>% max) %>%
        message
    }
    if(all_standings %>% nrow() > 0){
      return(all_standings)
    }
  }

get_day_nba_game_player_tracking_availability <-
  function(date,
           merge_team_data = T,
           return_message = T) {
    if (!'date' %>% exists()) {
      stop("Please enter a valid date")
    }

    json_data <-
      get_nba_day_score_json_data_safe(game_date = date)
    install_needed_packages(function_packages)
    load_needed_packages(function_packages)
    if (json_data %>% length == 0) {
      stop("No games for " %>% paste0(date))
    }

    player_tracking_data <-
      json_data$resultSets$rowSet[[7]] %>%
      data.frame() %>%
      tbl_df

    names(player_tracking_data) <-
      c('id.game', 'is.tracking.available')

    player_tracking_data %<>%
      mutate(date.game = date,
             is.tracking.available = ifelse(is.tracking.available == "1", T, F)
             )

    if (date %>% mdy %>% month < 7) {
      year.season.start <-
        date %>% mdy %>% year %>% as.numeric() - 1
    } else {
      year.season.start <-
        date %>% mdy %>% year %>% as.numeric()
    }
    id.seasons <-
      year.season.start %>%
      paste0("-", substr(year.season.start + 1, start = 3, 4))

    player_tracking_data %<>%
      mutate(date.game = date.game %>% as.Date('%m/%d/%Y'),
             id.season = id.seasons) %>%
      dplyr::select(id.season, date.game, id.game, is.tracking.available)

    if (merge_team_data == T) {
      game_date <-
        date
      days_games_df <-
        get_day_nba_games(date = game_date, return_message = F)

      player_tracking_data <-
        days_games_df %>%
        dplyr::select(id.game,
                      slug.team.home,
                      slug.team.away
                      ) %>%
        left_join(player_tracking_data) %>%
        suppressMessages() %>%
        dplyr::select(
          id.season,
          date.game,
          id.game,
          slug.team.home,
          slug.team.away,
          everything()
        )
    }

    if (return_message == T) {
      "You got player tracking availability for " %>%
        paste0(
          date
        ) %>%
        message()
    }

    return(player_tracking_data)

  }

get_day_nba_game_player_tracking_availability_safe <-
  failwith(NULL, get_day_nba_game_player_tracking_availability)

get_days_nba_game_player_tracking_availability <-
  function(dates,
           merge = T,
           message = T) {
    if(!'dates' %>% exists()) {
      stop("Please enter game dates")
    }
    all_tracking <-
      dates %>%
      purrr::map(function(x)
        get_day_nba_game_player_tracking_availability_safe(date = x,
                                          merge_team_data = merge,
                                          return_message = F)
      ) %>%
      compact %>%
      bind_rows

    if (message == T) {
      "You got tracking data from " %>%
        paste0(dates %>% min(), " to ", dates %>% max) %>%
        message
    }
    if(all_tracking %>% nrow() > 0){
      return(all_tracking)
    }
  }

get_day_nba_game_team_leaders <-
  function(date,
           merge_team_data = T,
           return_message = T)  {
    if (!'date' %>% exists()) {
      stop("Please enter a valid date")
    }

    json_data <-
      get_nba_day_score_json_data_safe(game_date = date)
    install_needed_packages(function_packages)
    load_needed_packages(function_packages)
    if (json_data %>% length == 0) {
      stop("No games for " %>% paste0(date))
    }

    team_leader_data <-
      json_data$resultSets$rowSet[[8]] %>%
      data.frame() %>%
      tbl_df

    names(team_leader_data) <-
      c(
        "id.game",
        "id.team",
        "city.team",
        "name.team",
        "slug.team",
        "id.player.pts.leader",
        "name.player.pts.leader",
        "pts",
        "id.player.reb.leader",
        "name.player.reb.leader",
        "reb",
        "id.player.ast.leader",
        "name.player.ast.leader",
        "ast"
      )

    team_leader_data %<>%
      mutate(date.game = date %>% as.Date('%m/%d/%Y'))

    if (date %>% mdy %>% month < 7) {
      year.season.start <-
        date %>% mdy %>% year %>% as.numeric() - 1
    } else {
      year.season.start <-
        date %>% mdy %>% year %>% as.numeric()
    }

    id.seasons <-
      year.season.start %>%
      paste0("-", substr(year.season.start + 1, start = 3, 4))

    names_numeric_cols <-
      team_leader_data %>%
      dplyr::select(c(id.team, pts, reb, ast, contains("id.player"))) %>% names


    team_leader_data %<>%
      mutate(id.season = id.seasons) %>%
      mutate_each_(funs(as.numeric), vars = names_numeric_cols) %>%
      mutate(id.season) %>%
      dplyr::select(id.season, date.game, everything())


    team_leader_data %<>%
      mutate(date.game = date.game %>% as.Date('%m/%d/%Y'),
             id.season) %>%
      unite(team, city.team, name.team, sep = ' ') %>%
      dplyr::select(id.season, date.game, id.game, everything())

    if (merge_team_data == T) {
      game_date <-
        date
      days_games_df <-
        get_day_nba_games(date = game_date, return_message = F)

      team_leader_data <-
        days_games_df %>%
        dplyr::select(id.game,
                      slug.team.home,
                      slug.team.away
        ) %>%
        left_join(team_leader_data) %>%
        suppressMessages() %>%
        dplyr::select(
          id.season,
          date.game,
          id.game,
          everything()
        ) %>%
        mutate(is.home_team = ifelse(slug.team == slug.team.home, T, F),
               slug.opponent = ifelse(slug.team == slug.team.home, slug.team.away, slug.team.home)
               ) %>%
        dplyr::select(-c(slug.team.home, slug.team.away)) %>%
        dplyr::select(id.season:id.team, team, slug.team, slug.opponent, is.home_team, everything())
    }

    if (return_message == T) {
      "You got team leaders for " %>%
        paste0(
          date
        ) %>%
        message()
    }

    return(team_leader_data)

  }

get_day_nba_game_team_leaders_safe <-
  failwith(NULL, get_day_nba_game_team_leaders)

get_days_nba_game_team_leaders <-
  function(dates,
           merge = T,
           message = T)  {
    if(!'dates' %>% exists()) {
      stop("Please enter game dates")
    }
    all_leaders <-
      dates %>%
      purrr::map(function(x)
        get_day_nba_game_team_leaders_safe(date = x,
                                                           merge_team_data = merge,
                                                           return_message = F)
      ) %>%
      compact %>%
      bind_rows

    if (message == T) {
      "You got team leader data from " %>%
        paste0(dates %>% min(), " to ", dates %>% max) %>%
        message
    }
    if(all_leaders %>% nrow() > 0){
      return(all_leaders)
    }
  }


get_day_nba_matchups <-
  function(is.today = F,
           date = NA) {
    get_dates_teams <- function(data) {
      data %<>%
        dplyr::select(date.game, id.game, slug.team.home, slug.team.away) %>%
        gather(location, slug.team, -c(id.game, date.game)) %>%
        mutate(
          location = location %>% as.character(),
          is.home_game = ifelse(location == 'slug.team.home', T, F)
        ) %>%
        dplyr::select(-location) %>%
        dplyr::select(date.game, everything())
      return(data)
    }
    get_date_char <- function(date) {
      dates <-
        date %>%
        str_split('\\-') %>% unlist
      date.char <-
        paste(dates[2], dates[3], dates[1], sep = '-')
      return(date.char)
    }
    if (is.today == T) {
      date <-
        Sys.Date() %>%
        get_date_char(date = .)
    }

    tomorrow <-
      (date %>% mdy %>% as.Date() + 1) %>%
      get_date_char

    yesterday <-
      (date %>% mdy %>% as.Date() - 1) %>% as.character() %>%
      get_date_char

    if(get_day_nba_games_safe(date = date) %>% length > 0){
      playing_today_df <-
        get_day_nba_games_safe(date = date)

      todays_teams <-
        playing_today_df %>%
        get_dates_teams() %>%
        arrange(id.game)

      playing_tomorrow_df <-
        get_day_nba_games_safe(date = tomorrow)

      playing_tomorrow_df %<>%
        get_dates_teams() %>%
        arrange(id.game)

      played_yesterday_df <-
        get_day_nba_games_safe(date = yesterday) %>%
        get_dates_teams

      todays_teams %<>%
        mutate(
          played.yesterday = todays_teams$slug.team %in% playing_tomorrow_df$slug.team,
          playing.tomorrow = todays_teams$slug.team %in% played_yesterday_df$slug.team,
          is.back_to_back = ifelse(played.yesterday == T |
                                     playing.tomorrow == T, T, F)
        )

      home_teams <-
        todays_teams %>%
        dplyr::filter(is.home_game == T) %>%
        dplyr::rename(
          slug.team.home = slug.team,
          has.team.home.played.yesterday = played.yesterday,
          is.team.home.playing.tomorrow = playing.tomorrow,
          is.team.home.b2b = is.back_to_back
        ) %>%
        dplyr::select(-is.home_game)

      away_teams <-
        todays_teams %>%
        dplyr::filter(is.home_game == F) %>%
        dplyr::rename(
          slug.team.away = slug.team,
          has.team.away.played.yesterday = played.yesterday,
          is.team.away.playing.tomorrow = playing.tomorrow,
          is.team.away.b2b = is.back_to_back
        ) %>%
        dplyr::select(-is.home_game)

      matchups <-
        home_teams %>%
        left_join(away_teams)

      teams <-
        todays_teams

      game_id <-
        todays_teams$id.game
      all_games <-
        data_frame()

      for (id in game_id) {
        team.home <-
          home_teams %>%
          dplyr::filter(id.game == id) %>%
          .$slug.team.home

        team.away <-
          away_teams %>%
          dplyr::filter(id.game == id) %>%
          .$slug.team.away


        g <-
          data_frame(id.game = id,
                     game = team.away %>% paste0('@', team.home))

        all_games %<>%
          bind_rows(g)
      }

      all_games %<>%
        distinct()

      matchup_df <-
        todays_teams %>%
        left_join(all_games) %>%
        left_join(matchups) %>%
        mutate(
          both.b2b = ifelse(
            is.team.away.b2b == T &
              is.team.home.b2b == T,
            T,
            F
          )
        ) %>%
        mutate(slug.opponent = ifelse(slug.team == slug.team.home, slug.team.away, slug.team.home)) %>%
        dplyr::select(date.game,
                      id.game,
                      game,
                      slug.team,
                      slug.opponent,
                      both.b2b,
                      everything()) %>%
        arrange(id.game) %>%
        distinct()

      return(matchup_df)
    }
  }

get_day_nba_matchups_safe <-
  failwith(NULL, get_day_nba_matchups)

get_days_nba_matchups <-
  function(dates,
           message = T)  {
    if(!'dates' %>% exists()) {
      stop("Please enter game dates")
    }
    all_matchups <-
      dates %>%
      purrr::map(function(x)
        get_day_nba_matchups_safe(date = x)
      ) %>%
      compact %>%
      bind_rows

    if (message == T) {
      "You got team leader data from " %>%
        paste0(dates %>% min(), " to ", dates %>% max) %>%
        message
    }
    if(all_leaders %>% nrow() > 0){
      return(all_leaders)
    }
  }
