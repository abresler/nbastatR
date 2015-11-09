#' Get any days nba box score and even more data!
#'
#' @param date enter a date
#' @param return_message
#'
#' @return
#' @export
#'
#' @examples data <- get_nba_days_scores(date = "11-09-1983")

get_nba_days_scores <- function(date, return_message = T) {
  packages <- #need all of these installed including some from github
    c('dplyr',
      'magrittr',
      'jsonlite',
      'tidyr',
      'stringr',
      'lubridate')
  options(warn = -1)
  lapply(packages, library, character.only = T)
  if(!date %>% class == "character"){
    stop("Make sure you enter date as a character!!")
  }

  if (date %>%  lubridate::guess_formats("mdy") %>% length == 0) {
    stop("Enter valid date in month, day, year format")
  }

  parsed_date <-
    date %>%
    mdy %>%
    as.Date

  if( parsed_date %>%  month %in% c(7:9)){
    stop("Sorry the NBA doesn't play have games in the summer")
  }

  if (parsed_date %>% year < 1948){
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

  json_data <-
    url_games %>%
    fromJSON(simplifyDataFrame = T, flatten = T)

  table_names <-
    json_data$resultSets$name

  # Game Header
  if ('GameHeader' %in% table_names) {
    if (json_data$resultSets$rowSet[1] %>% data.frame %>% nrow > 0) {
      game_data <-
        json_data$resultSets$rowSet[1] %>% data.frame %>%
        tbl_df

      names(game_data) <-
        c(
          "game_date_est",
          "sequence.game",
          "id.game",
          "id.game_status",
          "text.games_status",
          "gamecode",
          "id.home_team",
          "id.away_team",
          "year.season_start",
          "period.live",
          "live_pc_time",
          "id.tv_broadcast",
          "period.live_broadcast",
          "status.wh"
        )

      year_season_end <-
        game_data$year.season_start %>% unique %>% as.numeric() + 1

      id.season <-
        game_data$year.season_start %>% unique %>%
        paste0('-', year_season_end %>% substr(3, 4))

      game_data %<>%
        separate(gamecode, sep = '\\/', c('date', 'teamslugs')) %>%
        mutate(
          slug.away_team = teamslugs %>% substr(start = 1, stop = 3),
          slug.home_team = teamslugs %>% substr(4, 6),
          date = date %>% as.Date('%Y%m%d'),
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
          date,
          sequence.game,
          id.game,
          is.final,
          slug.home_team,
          slug.away_team,
          everything()
        )

      names_numeric_cols <-
        game_data %>%
        dplyr::select(
          -c(
            id.season,
            date,
            is.final,
            id.game,
            slug.home_team,
            slug.away_team,
            id.tv_broadcast
          )
        ) %>%
        names

      game_data %<>%
        mutate_each_(funs(as.numeric), vars = names_numeric_cols)
    } else {
      stop("No game data for " %>%
             paste0(date))
    }
  }

  if (game_data %>% nrow > 0) {
    id.season <-
      game_data$id.season %>% unique()
  } else {
    id.season <-
      NA
  }

  if ('LineScore' %in% table_names) {
    if (json_data$resultSets$rowSet[2] %>% data.frame %>% nrow > 0) {
      line_score_data <-
        json_data$resultSets$rowSet[2] %>% data.frame %>%
        tbl_df

      names(line_score_data) <-
        c(
          "game_date_est",
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

      line_score_data %<>%
        separate(team.record, sep = '\\-', c('wins', 'losses')) %>%
        mutate(date = parsed_date, id.season) %>%
        dplyr::select(-game_date_est) %>%
        dplyr::select(id.season, date, everything())

      names_numeric_cols <-
        line_score_data %>%
        dplyr::select(-c(id.season, date, slug.team, city.team, id.game)) %>%
        names

      line_score_data %<>%
        mutate_each_(funs(as.numeric), vars = names_numeric_cols)

      line_score_data %<>%
        left_join(
        line_score_data %>%
        group_by(id.game) %>%
        dplyr::filter(pts == max(pts)) %>%
        dplyr::select(id.game, slug.winner = slug.team)
        ) %>%
        mutate(is.winning_team = ifelse(slug.winner == slug.team, T, F)) %>%
        dplyr::select(id.season:id.game, is.winning_team, everything())
    } else {
    line_score_data <-
      data_frame()
    }
  }

  if ('SeriesStandings' %in% table_names) {
    if (json_data$resultSets$rowSet[3] %>% data.frame %>% nrow > 0) {
      series_standing_data <-
        json_data$resultSets$rowSet[3] %>% data.frame %>% tbl_df

      names(series_standing_data) <-
        c(
          "id.game",
          "id.home_team",
          "id.away_team",
          "game_date_est",
          "wins.home_team",
          "losses.home_team",
          "city.series_leader"
        )

      series_standing_data %<>%
        mutate(date = parsed_date, id.season) %>%
        dplyr::select(-game_date_est) %>%
        dplyr::select(id.season, date, everything())

      names_numeric_cols <-
        series_standing_data %>%
        dplyr::select(-c(id.season, date, city.series_leader, id.game)) %>%
        names

      series_standing_data %<>%
        mutate_each_(funs(as.numeric), vars = names_numeric_cols)
    }  else {
      series_standing_data <-
        data_frame()
    }
  }

  if ('LastMeeting' %in% table_names) {
    if (json_data$resultSets$rowSet[4] %>% data.frame %>% nrow > 0) {
      last_meeting_data <-
        json_data$resultSets$rowSet[4] %>% data.frame %>% tbl_df

      names(last_meeting_data) <-
        c(
          "id.game",
          "id.game.last",
          "last_game_date_est",
          "id.home_team.last",
          "city.home_team.last",
          "name.home_team.last",
          "slug.home_team.last",
          "pts.home_team.last",
          "id.away_team.last",
          "city.away_team.last",
          "name.away_team.last",
          "slug.away_team.last",
          "pts.away_team.last"
        )

      last_meeting_data %<>%
        separate(last_game_date_est,
                 sep = 'T',
                 into = c('date.last', 'ignore')) %>%
        dplyr::select(-ignore) %>%
        mutate(date = parsed_date,
               id.season,
               date.last = date.last %>% ymd %>% as.Date)

      names_numeric_cols <-
        last_meeting_data %>%
        dplyr::select(
          c(
            id.home_team.last,
            id.away_team.last,
            pts.home_team.last,
            pts.away_team.last
          )
        ) %>%
        names

      last_meeting_data %<>%
        mutate_each_(funs(as.numeric), vars = names_numeric_cols) %>%
        dplyr::select(
          id.season,
          date,
          date.last,
          slug.home_team.last,
          slug.away_team.last,
          pts.home_team.last,
          pts.away_team.last,
          everything()
        )

    } else {
    last_meeting_data <-
      data_frame()
    }
  }

  if ('EastConfStandingsByDay' %in% table_names) {
    if (json_data$resultSets$rowSet[5] %>% data.frame %>% nrow > 0) {
      east_standings_by_day_data <-
        json_data$resultSets$rowSet[5] %>% data.frame %>% tbl_df
      names(east_standings_by_day_data) <-
        c(
          "id.team",
          "id.league",
          "id.season",
          "date.standings",
          "name.conference",
          "city.team",
          "games",
          "wins",
          "losses",
          "pct.w",
          "home_record",
          "road_record"
        )

      east_standings_by_day_data %<>%
        dplyr::select(-c(id.league, id.season)) %>%
        separate(home_record,
                 sep = '\\-',
                 into = c('wins.home', 'losses.home')) %>%
        separate(road_record,
                 sep = '\\-',
                 into = c('wins.away', 'losses.away'))

      names_numeric_cols <-
        east_standings_by_day_data %>% dplyr::select(-c(name.conference, city.team, date.standings)) %>% names

      east_standings_by_day_data %<>%
        mutate_each_(funs(as.numeric), vars = names_numeric_cols) %>%
        mutate(
          id.season,
          date = parsed_date,
          date.standings = date.standings %>% as.Date('%m/%d/%Y')
        ) %>%
        dplyr::select(id.season, date, everything())
    } else {
    east_standings_by_day_data <-
      data_frame()
    }
  }

  if ('WestConfStandingsByDay' %in% table_names) {
    if (json_data$resultSets$rowSet[6] %>% data.frame %>% nrow > 0) {
      west_standings_by_day_data <-
        json_data$resultSets$rowSet[6] %>% data.frame %>% tbl_df
      names(west_standings_by_day_data) <-
        c(
          "id.team",
          "id.league",
          "id.season",
          "date.standings",
          "name.conference",
          "city.team",
          "games",
          "wins",
          "losses",
          "pct.w",
          "home_record",
          "road_record"
        )

      west_standings_by_day_data %<>%
        dplyr::select(-c(id.league, id.season)) %>%
        separate(home_record,
                 sep = '\\-',
                 into = c('wins.home', 'losses.home')) %>%
        separate(road_record,
                 sep = '\\-',
                 into = c('wins.away', 'losses.away'))

      names_numeric_cols <-
        west_standings_by_day_data %>% dplyr::select(-c(name.conference, city.team, date.standings)) %>% names

      west_standings_by_day_data %<>%
        mutate_each_(funs(as.numeric), vars = names_numeric_cols) %>%
        mutate(
          id.season,
          date = parsed_date,
          date.standings = date.standings %>% as.Date('%m/%d/%Y')
        ) %>%
        dplyr::select(id.season, date, everything())

    }
    else {
    west_standings_by_day_data <-
      data_frame()
    }
  }

  if (west_standings_by_day_data %>% nrow > 0 &
      east_standings_by_day_data %>% nrow > 0) {
    standings_data <-
      east_standings_by_day_data %>%
      bind_rows(west_standings_by_day_data)
  }

  if ('Available' %in% table_names) {
    if (json_data$resultSets$rowSet[7] %>% data.frame %>% nrow > 0) {
      player_tracking_data_available <-
        json_data$resultSets$rowSet[7] %>% data.frame
      names(player_tracking_data_available) <-
        c('id.game', 'pt_available')

      player_tracking_data_available %<>%
        mutate(
          id.season,
          date = parsed_date,
          is.player_tracking_available = ifelse(pt_available == "1", T, F)
        ) %>%
        dplyr::select(-pt_available)
    } else {
    player_tracking_data_available <-
      data_frame()
    }
  }

  if ('TeamLeaders' %in% table_names) {
    if (json_data$resultSets$rowSet[8] %>% data.frame %>% nrow > 0) {
      team_leader_data <-
        json_data$resultSets$rowSet[8] %>% data.frame %>% tbl_df
      names(team_leader_data) <-
        c(
          "id.game",
          "id.team",
          "city.team",
          "name.team",
          "slug.team",
          "id.player.pts_leader",
          "name.player.pts_leader",
          "pts",
          "id.player.reb_leader",
          "name.player.reb_leader",
          "reb",
          "id.player.ast_leader",
          "name.player.ast_leader",
          "ast"
        )

      names_numeric_cols <-
        team_leader_data %>% dplyr::select(id.team, pts, reb, ast) %>% names

      team_leader_data %<>%
        mutate_each_(funs(as.numeric), vars = names_numeric_cols) %>%
        mutate(id.season, date = parsed_date) %>%
        dplyr::select(id.season, date, everything())
    }
    else {
    team_leader_data <-
      data_frame()
    }
  }

  data <-
    list(
      game_data,
      line_score_data,
      series_standing_data,
      last_meeting_data,
      standings_data,
      player_tracking_data_available,
      team_leader_data
    )

  names(data) <-
    c(
      'games',
      'game_lines',
      'season_series_standings',
      'last_meeting',
      'standings',
      'player_tracking',
      'team_leaders'
    )
  if (return_message == T) {
    if(team_leader_data %>% nrow > 0){
      fun_data <-
        team_leader_data %>%
        sample_n(1) %>%
        dplyr::select(name.player.pts_leader, pts, name.team)

      fun_fact <-
        '.\nDid you know that ' %>%
        paste0(fun_data$name.player.pts_leader,' of the ', fun_data$name.team, '\nscored ',
               fun_data$pts, ' points that night?')
    } else {
      fun_fact <-
        ''
    }

    "You got data for all " %>%
      paste0(game_data %>% nrow, ' NBA games on ', date, fun_fact) %>%
      message()
  }
  return(data)
}
get_team_ids_to_fanduel <-
  function(){
    team_ids <-
      data_frame(team = c("Atlanta Hawks", "Brooklyn Nets", "Boston Celtics", "Charlotte Hornets",
                          "Chicago Bulls", "Cleveland Cavaliers", "Dallas Mavericks", "Denver Nuggets",
                          "Detroit Pistons", "Golden State Warriors", "Houston Rockets",
                          "Indiana Pacers", "Los Angeles Clippers", "Los Angeles Lakers",
                          "Memphis Grizzlies", "Miami Heat", "Milwaukee Bucks", "Minnesota Timberwolves",
                          "New Orleans Pelicans", "New York Knicks", "Oklahoma City Thunder",
                          "Orlando Magic", "Philadelphia 76ers", "Phoenix Suns", "Portland Trail Blazers",
                          "Sacramento Kings", "San Antonio Spurs", "Toronto Raptors", "Utah Jazz",
                          "Washington Wizards"),
                 slug.team = c("ATL", "BKN", "BOS", "CHA", "CHI", "CLE", "DAL", "DEN", "DET",
                               "GSW", "HOU", "IND", "LAC", "LAL", "MEM", "MIA", "MIL", "MIN",
                               "NOP", "NYK", "OKC", "ORL", "PHI", "PHX", "POR", "SAC", "SAS",
                               "TOR", "UTA", "WAS"),
                 slug.team.fanduel = c("ATL", "BKN", "BOS", "CHA", "CHI", "CLE", "DAL","DEN",
                                       "DET", "GS", "HOU", "IND", "LAC","LAL", "MEM","MIA", "MIL", "MIN", "NO", "NY", "OKC",
                                       "ORL", "PHI", "PHO", "POR",  "SAC","SA", "TOR", "UTA", "WAS"),
                 slug.team.roto = c("ATL", "BKN", "BOS", "CHA", "CHI", "CLE", "DAL", "DEN", "DET",
                                    "GSW", "HOU", "IND", "LAC", "LAL", "MEM", "MIA", "MIL", "MIN",
                                    "NOP", "NYK", "OKC", "ORL", "PHI", "PHO", "POR", "SAC", "SAS",
                                    "TOR", "UTA", "WAS")
      )
    return(team_ids)
  }
get_days_games <- function(is.today = T, date = NA) {
  packages <- #need all of these installed including some from github
    c('dplyr',
      'magrittr',
      'jsonlite',
      'tidyr',
      'stringr',
      'lubridate')
  options(warn = -1)
  lapply(packages, library, character.only = T)
  get_dates_teams <- function(data){
    data %<>%
      dplyr::select(date, id.game, slug.home_team, slug.away_team) %>%
      gather(location, slug.team, -id.game, -date) %>%
      mutate(
        location = location %>% as.character(),
        is.home_game = ifelse(location == 'slug.home_team', T, F)
      ) %>%
      dplyr::select(-location) %>%
      dplyr::select(date, everything())
    return(data)
  }
  get_date_char <- function(date){
    dates <-
      date %>%
      str_split('\\-') %>% unlist
    date.char <-
      paste(dates[2], dates[3], dates[1], sep = '-')
    return(date.char)
  }
  if(is.today == T){
    date <-
      Sys.Date() %>%
      get_date_char(date = .)
  }

  tomorrow <-
    (date %>% mdy %>% as.Date() + 1) %>%
    get_date_char

  yesterday <-
    (date %>% mdy %>% as.Date() -1) %>% as.character() %>%
    get_date_char

  games.today <-
    get_nba_days_scores(date = date) %>%
    .$games

  todays_teams <-
    games.today %>%
    get_dates_teams()


  playing_tomorrow <-
    get_nba_days_scores(date = tomorrow) %>%
    .$games %>%
    get_dates_teams

  played_yesterday <-
    get_nba_days_scores(date = yesterday) %>%
    .$games %>%
    get_dates_teams

  todays_teams %<>%
    mutate(played.yesterday = todays_teams$slug.team %in% played_yesterday$slug.team,
           playing.tomorrow = todays_teams$slug.team %in% playing_tomorrow$slug.team,
           is.back_to_back = ifelse(played.yesterday == T|playing.tomorrow == T, T, F)
    )

  home_teams <-
    todays_teams %>%
    dplyr::filter(is.home_game == T) %>%
    dplyr::rename(slug.home_team = slug.team, is.home_team.played_yesterday = played.yesterday,
                  is.home_team.playing_tomorrow = playing.tomorrow,
                  is.home_team.back_to_back = is.back_to_back) %>%
    dplyr::select(-is.home_game)

  away_teams <-
    todays_teams %>%
    dplyr::filter(is.home_game == F) %>%
    dplyr::rename(slug.away_team = slug.team, is.away_team.played_yesterday = played.yesterday,
                  is.away_team.playing_tomorrow = playing.tomorrow,
                  is.away_team.back_to_back = is.back_to_back) %>%
    dplyr::select(-is.home_game)

  matchups <-
    home_teams %>%
    left_join(away_teams)

  teams <-
    todays_teams

  game_id <-
    games.today$id.game %>% unique
  all_games <-
    data_frame()

  for (id in game_id) {
    t <-
      teams %>%
      dplyr::filter(id.game == id) %>%
      .$slug.team

    g <-
      data_frame(id.game = id, game = t[2] %>% paste0('@', t[1]))

    all_games %<>%
      bind_rows(g)
  }

  t <-
    games.today %>%
    left_join(all_games) %>%
    left_join(matchups) %>%
    mutate(both.back_to_back = ifelse(is.away_team.back_to_back == T & is.home_team.back_to_back == T, T, F)) %>%
    dplyr::select(id.season, date,id.game, game,both.back_to_back, everything())

  both_b2b <-
    t %>%
    dplyr::select(id.game, both.back_to_back)


  data <-
    list(teams %>% left_join(both_b2b), t)
  names(data) <-
    c('teams_today', 'games_today')
  return(data)
}
