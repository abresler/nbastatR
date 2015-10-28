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
get_bref_season_player_stats <- function(year_end_season, use_traded_player_team_total = F,
           stat_type = c("Advanced", "Totals", "Per Minute", "Per Game"),
           league = 'NBA') {
  options(warn = -1)
  packages <-
    c(
      'rvest',
      'stringr',
      'lubridate' ,
      'stringi',
      'readr',
      'urltools',
      'data.table',
      'curl',
      'tidyr',
      'magrittr',
      'formattable',
      'httr',
      'dplyr',
      'readxl'
    )

  lapply(packages, library, character.only = T)
    bref_team_base <-
      'http://www.basketball-reference.com/leagues/'
    bref_base <-
      'http://www.basketball-reference.com'

    if (!stat_type %in% c("Advanced", "Totals", "Per Minute", "Per Game")) {
      stop.message <-
        "Not a valid table try " %>%
        paste0(c("Advanced", "Totals", "Per Minute", "Per Game") %>%
                 sample(size = 1))
      stop(stop.message)
    }

    stat_type <-
      stat_type %>%
      tolower %>%
      gsub("\\ ", '_', .)

    url <-
      bref_team_base %>%
      paste0(league,
             '_',
             year_end_season,
             '_',
             stat_type,
             '.html')
    css_page <-
      paste0('#', stat_type)

    css_player <-
      "td:nth-child(2) a"

    page <-
      url %>%
      read_html()

    table <-
      page %>%
      html_nodes(css_page) %>%
      html_table %>%
      data.frame %>%
      tbl_df

    names(table) %<>%
      tolower()

    table %<>%
      dplyr::filter(!g == 'G')

    numeric_cols <-
      table[, !names(table) %in% c('player', 'pos', 'tm')] %>% names

    table %<>%
      mutate_each_(funs(as.numeric), vars = numeric_cols) %>%
      dplyr::select(-rk) %>%
      mutate(year.season = year_end_season) %>%
      dplyr::select(year.season, everything())

    url.bref.player <-
      page %>%
      html_nodes(css_player) %>%
      html_attr('href') %>%
      paste0('http://www.basketball-reference.com',.)

    stem.player <-
      page %>%
      html_nodes(css_player) %>%
      html_attr('href') %>%
      str_replace_all('/players/|.html', '') # eliminates the unnecassary words to help us get at the clean ID

    player <-
      page %>%
      html_nodes(css_player) %>%
      html_text

    players_urls <- # This will create a data frame with the information we want
      data_frame(player, url.bref.player, stem.player) %>%
      separate(stem.player, c('letter.first', 'id.bref.player'), #  Separates the remaining 2 parts by its delimiter the / we only want the second column which contains the id
               sep = '/') %>%
      dplyr::select(-letter.first) %>%  # removes the unneeded column
      distinct

    table %<>%
      left_join(players_urls)

    traded_players <-
      table %>%
      dplyr::filter(tm == 'TOT') %>%
      .$player

    table %<>%
      mutate(is.traded_player = ifelse(player %in% traded_players, T, F))

    if (use_traded_player_team_total == T) {
      traded_table <-
        table %>%
        dplyr::filter(player %in% traded_players, tm == "TOT")

      non_traded <-
        table %>%
        dplyr::filter(!player %in% traded_players)

      data <-
        non_traded %>%
        bind_rows(traded_table) %>%
        arrange(id.bref.player)

    } else {
      traded_table <-
        table %>%
        dplyr::filter(player %in% traded_players, !tm == "TOT")

      non_traded <-
        table %>%
        dplyr::filter(!player %in% traded_players)

      data <-
        non_traded %>%
        bind_rows(traded_table) %>%
        arrange(id.bref.player)
    }
    "Congrats you got BREF data for the " %>%
    paste0(year_end_season, ' season') %>%
      message
    return(data)
  }
