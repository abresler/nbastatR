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
        "PERSON_ID", "FIRST_NAME", "LAST_NAME", "DISPLAY_FIRST_LAST",
        "DISPLAY_LAST_COMMA_FIRST", "DISPLAY_FI_LAST", "BIRTHDATE", "SCHOOL",
        "COUNTRY", "LAST_AFFILIATION", "HEIGHT", "WEIGHT", "SEASON_EXP",
        "JERSEY", "POSITION", "ROSTERSTATUS", "TEAM_NAME", "TEAM_CODE",
        "TEAM_CITY", "PLAYERCODE", "FROM_YEAR", "TO_YEAR", "DLEAGUE_FLAG",
        "GAMES_PLAYED_FLAG", "PLAYER_NAME", "TimeFrame", "PIE"
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
        "pf",
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
        "id.player", "name.first", "name.last", "name.player",
        "name.last.display", "name.middle.display", "date.birth", "school",
        "country", "college.non_nba_team", "height", "weight.lbs", "years.experience",
        "jersey", "position", "status.roster", "name.team", "code.team",
        "city.team", "slug.player", "year.from", "year.to", "has.d_league_data",
        "gp.flag", "name.player", "id.season", "pie"
      ),
      id.row = 1:length(name.actual)
    )
  return(headers_df)
}

get_nba_players_ids <- function(active_only = F){
  packages <- #need all of these installed including some from github
    c('dplyr',
      'magrittr',
      'jsonlite',
      'tidyr',
      'stringr',
      'data.table',
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
    str_split_fixed(pattern = '\\,',2) %>%
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
    mutate(id.team = ifelse(id.team == 0, NA, id.team),
           city.team = ifelse(city.team == '', NA, city.team),
           name.team = ifelse(name.team == '', NA, name.team),
           code.team = ifelse(code.team == '', NA, code.team),
           slug.team = ifelse(slug.team == '', NA, slug.team),
           team = ifelse(city.team %>% is.na, NA, paste(city.team, name.team)),
           seasons.played = year.to - year.from,
           url.player = id.player %>% paste0('http://stats.nba.com/player/#!/',.)
    ) %>%
    dplyr::select(name.player, id.player, team, id.team, is.active_player, seasons.played,
                  year.from, year.to,
                  everything())

  if(active_only == T){
    data %<>%
      dplyr::filter(is.active_player == T)
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
#' @examples get_player_season_gamelog(player = "John Stockton", year.season_start = 1994,include_player_metadata = T)
#get_player_season_gamelog(id.player = 201945, season_type = "Pre Season", year.season_start = 2015)
get_player_season_gamelog <- function(player,
                                      id.player = NULL,
                                      season_type = "Regular Season",
                                      year.season_start,
                                      include_date_detail = T,
                                      include_player_metadata = T,
                                      return_message = T) {
  seasons_types <-
    c("Regular Season", "Playoffs", "Pre Season", "All Star")
  if (!season_type %in% seasons_types) {
    "Sorry season type must be either " %>%
      paste0(seasons_types %>% paste0(collapse = ', ')) %>%
      stop(call. = F)
  }

  season <-
    year.season_start %>%
    extract_numeric %>%
    paste0("-", (year.season_start + 1) %>% substr(start = 3, stop = 4))

  st <-
    season_type %>%
    str_replace(pattern = '\\ ', '\\+')

  if (id.player %>% is.null()) {
    id.player <-
      players %>%
      dplyr::filter(name.player == player) %>%
      .$id.player

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
      .$name.player

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

  if(json_data$resultSets$rowSet %>%
    data.frame %>%
    tbl_df %>% nrow > 0) {
    data <-
    json_data$resultSets$rowSet %>%
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
    mutate_each_(funs(extract_numeric),
                 vars =
                   data %>%
                   dplyr::select(id.player, min:plus.minus) %>% names) %>%
    separate(
      matchup,
      into = c("slug.team", "slug.opponent"),
      sep = "@|vs.",
      remove = F
    ) %>%
    mutate(
      id.season = season,
      is.win = wl %>% str_detect("W"),
      date.game = date.game %>% mdy() %>% as.Date,
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

  if (include_date_detail == T){
    data %<>%
      mutate(day.game = date.game %>% strftime('%A'),
             month.game = date.game %>% month
             ) %>%
      dplyr::select(id.season:date.game, day.game, month.game, everything())
  }

  if (include_player_metadata == T) {
    profile <-
      get_player_profile(id.player = data$id.player %>% unique, return_message = F, include_headline_stat = F)

    data <-
      profile %>%
      dplyr::select(name.player, id.player, position, height.inches, weight.lbs) %>%
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
