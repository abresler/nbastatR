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

get_fd_name_df <- function(){
  fd_nba_name_df <-
    data_frame(
      name.fanduel = c(
        "Louis Amundson",
        "Ishmael Smith",
        "C.J. Wilcox",
        "Glenn Robinson III",
        "Joseph Young",
        "Luc Richard Mbah a Moute",
        "T.J. Warren",
        "Nene Hilario",
        "P.J. Tucker",
        "J.J. Redick",
        "C.J. Miles",
        "C.J. McCollum",
        "Brad Beal",
        "Roy Devyn Marble",
        "K.J. McDaniels",
        "C.J. Watson",
        "J.J. Hickson",
        "Jose Juan Barea"
      ),
      name.nba =  c(
        "Lou Amundson",
        "Ish Smith",
        "CJ Wilcox",
        "Glenn Robinson",
        "Joe Young",
        "Luc Mbah a Moute",
        "TJ Warren",
        "Nene",
        "PJ Tucker",
        "JJ Redick",
        "CJ Miles",
        "CJ McCollum",
        "Bradley Beal",
        "Devyn Marble",
        "KJ McDaniels",
        "CJ Watson",
        "JJ Hickson",
        "Jose Juan Barea"
      ),
      is.different_name = T
    )
  return(fd_nba_name_df)
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
        "PERSON_ID", "FIRST_NAME", "LAST_NAME", "DISPLAY_FIRST_LAST",
        "DISPLAY_LAST_COMMA_FIRST", "DISPLAY_FI_LAST", "BIRTHDATE", "SCHOOL",
        "COUNTRY", "LAST_AFFILIATION", "HEIGHT", "WEIGHT", "SEASON_EXP",
        "JERSEY", "POSITION", "ROSTERSTATUS", "TEAM_NAME", "TEAM_CODE",
        "TEAM_CITY", "PLAYERCODE", "FROM_YEAR", "TO_YEAR", "DLEAGUE_FLAG",
        "GAMES_PLAYED_FLAG", "PLAYER_NAME", "TimeFrame", "PIE",
        "AGE", "W", "L", "W_PCT", "BLKA", "PFD", "DD2", "TD3", "CFID",
        "CFPARAMS", "OFF_RATING", "DEF_RATING", "NET_RATING", "AST_PCT", "AST_TO",
        "AST_RATIO", "OREB_PCT", "DREB_PCT", "REB_PCT", "TM_TOV_PCT",
        "EFG_PCT", "TS_PCT", "USG_PCT", "PACE", "FGM_PG", "FGA_PG",
        "PTS_OFF_TOV", "PTS_2ND_CHANCE", "PTS_FB", "PTS_PAINT", "OPP_PTS_OFF_TOV",
        "OPP_PTS_2ND_CHANCE", "OPP_PTS_FB", "OPP_PTS_PAINT",
        "PCT_FGA_2PT", "PCT_FGA_3PT", "PCT_PTS_2PT", "PCT_PTS_2PT_MR",
        "PCT_PTS_3PT", "PCT_PTS_FB", "PCT_PTS_FT", "PCT_PTS_OFF_TOV",
        "PCT_PTS_PAINT", "PCT_AST_2PM", "PCT_UAST_2PM", "PCT_AST_3PM",
        "PCT_UAST_3PM", "PCT_AST_FGM", "PCT_UAST_FGM",
        "PCT_FGM", "PCT_FGA", "PCT_FG3M", "PCT_FG3A", "PCT_FTM", "PCT_FTA",
        "PCT_OREB", "PCT_DREB", "PCT_REB", "PCT_AST", "PCT_TOV", "PCT_STL",
        "PCT_BLK", "PCT_BLKA", "PCT_PF", "PCT_PFD", "PCT_PTS",
        "FTA_RATE", "OPP_EFG_PCT", "OPP_FTA_RATE", "OPP_TOV_PCT", "OPP_OREB_PCT",
        "OPP_FGM", "OPP_FGA", "OPP_FG_PCT", "OPP_FG3M", "OPP_FG3A",
        "OPP_FG3_PCT", "OPP_FTM", "OPP_FTA", "OPP_FT_PCT", "OPP_OREB",
        "OPP_DREB", "OPP_REB", "OPP_AST", "OPP_TOV", "OPP_STL", "OPP_BLK",
        "OPP_BLKA", "OPP_PF", "OPP_PFD", "OPP_PTS"
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
        "id.player", "name.first", "name.last", "name.player",
        "name.last.display", "name.middle.display", "date.birth", "school",
        "country", "college.non_nba_team", "height", "weight.lbs", "years.experience",
        "jersey", "position", "status.roster", "team", "code.team",
        "city.team", "slug.player", "year.from", "year.to", "has.d_league_data",
        "gp.flag", "name.player", "id.season", "pie",
        "age", "wins", "losses", "pct.wins", "fga.blocked", "fouls.drawn", "double_doubles", "triple_doubles", "cfid",
        "cfparms", "ortg", "drtg", "netrtg", "pct.ast", "ratio.ast.to",
        "ratio.ast", "pct.oreb", "pct.dreb", "pct.reb", "ratio.to",
        "pct.efg", "pct.ts", "pct.usg", "pace", "fgm.per_game", "fga.per_game",
        "pts.off_to", "pts.2nd_chance", "pts.fastbreak", "pts.paint", "pts.off_to.opponent",
        "pts.2nd_chance.opponent", "pts.fastbreak.opponent", "pts.paint.opponent",
        "pct.fga2a", "pct.fga3a", "pct.pts.fg2m", "pct.pts.mid_range_2",
        "pct.pts.fg3m", "pct.pts.fast_break", "pct.pts.ft", "pct.pts.off_tos",
        "pct.paints.paint", "pct.fg2m.assisted", "pct.fg2m.unassisted", "pct.fg3m.assisted",
        "pct.fg3m.unassisted", "pct.fgm.assisted", "pct.fgm.unassisted",
        "pct.fgm", "pct.fga", "pct.fg3m", "pct.fg3a", "pct.ftm", "pct.fta",
        "pct.oreb", "pct.dreb", "pct.reb", "pct.ast", "pct.tov", "pct.stl",
        "pct.blk", "pct.blocked", "pct.fouls", "pct.fouls.drawn", "pct.pts",
        "rate.fta", "pct.efg.opp", "rate.fta.opp", "pct.tov.opp", "pct.oreb.opp",
        "fgm.opp", "fga.opp", "pct.fg.opp", "fg3m.opp", "fg3a.opp",
        "pct.fg3.opp", "ftm.opp", "fta.opp", "pct.ft.opp", "oreb.opp",
        "dreb.opp", "rep.opp", "ast.opp", "tov.opp", "stl.opp", "blk.opp",
        "fga.blocked.opp", "fouls.opp", "fouls.drawn.opp", "pts.opp"
      ),
      id.row = 1:length(name.actual)
    )
  return(headers_df)
}

get_nba_players_ids <- function(active_only = F, resolve_to_fanduel = T) {

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

players <-
  get_nba_players_ids()

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
