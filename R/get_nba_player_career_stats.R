
#' Title
#'
#' @param player , name of a player
#' @param id.player , id of a player
#' @param use_totals , use the totals for the seasoni if traded or each team
#' @param per_mode either Totals, Per 36, or Per Game
#' @param return_message
#'
#' @return
#' @export
#'
#' @examples
get_headers <- function(){
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
        "RANK_EFF"
      ),
      name.actual = c(
        "id.player",
        "id.season",
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
        "rank.eff"
      ),
      id.row = 1:length(name.actual)
    )
  return(headers_df)
}
players <-
  nbastatR::get_nba_players_ids()
get_nba_career_stat_table <- function(i, json_data, tables_names, use_totals) {
  headers_df <-
    get_headers()
  if (json_data$resultSets$rowSet[i] %>% data.frame %>% tbl_df %>% nrow > 1) {
    data <-
      json_data$resultSets$rowSet[i] %>% data.frame %>% tbl_df

    headers <-
      json_data$resultSets$headers[i] %>% unlist

    actual_names <-
      1:length(headers) %>%
      purrr::map(
        function(x)
          data_frame(
            name.actual =
              headers_df %>%
              dplyr::filter(name.nba == headers[x]) %>%
              .$name.actual
          )
      ) %>%
      bind_rows()

    names(data) <-
      actual_names$name.actual

    data %<>%
      dplyr::select(-id.league)

    data %<>%
      mutate_each_(funs(extract_numeric),
                   vars =
                     names(data)[!data %>% names %in% c('id.season', 'slug.team', 'name.school')])


    if (tables_names[i] %>% str_detect("College")) {
      data %<>%
        mutate(is.college = T)
    } else {
      data %<>%
        mutate(is.college = F)
    }

    if ('name.school' %in% names(data)) {
      data %<>%
        rename(slug.team = name.school)
    }

    if (!'slug.team' %in% names(data)) {
      data %<>%
        mutate(slug.team = NA)
    }

    if (tables_names[i] %>% str_detect("PostSeason")) {
      data %<>%
        mutate(is.playoffs = T)
    } else {
      data %<>%
        mutate(is.playoffs = F)
    }

    if (tables_names[i] %>% str_detect("AllStar")) {
      data %<>%
        mutate(is.allstar = T)
    } else {
      data %<>%
        mutate(is.allstar = F)
    }

    if (tables_names[i] %>% str_detect("RegularSeason")) {
      data %<>%
        mutate(is.regular_season = T)
    } else {
      data %<>%
        mutate(is.regular_season = F)
    }
    if ('id.season' %in% names(data)) {
      data %<>%
        separate(
          id.season,
          into = c('year.season_start', 'remove'),
          sep = '\\-',
          remove = F
        ) %>%
        dplyr::select(-remove) %>%
        mutate(year.season_start = year.season_start %>% as.numeric())
    }

    if (use_totals == T) {
      if (data %>% dplyr::filter(slug.team == "TOT") %>% nrow > 0){
        tot.season <-
          data %>%
          dplyr::filter(slug.team == "TOT") %>%
          .$year.season_start

        non_trade_data <-
          data %>%
          dplyr::filter(year.season_start == tot.season) %>%
          dplyr::filter(slug.team == "TOT")

        data <-
          data %>%
          dplyr::filter(!year.season_start == tot.season) %>%
          bind_rows(non_trade_data) %>%
          arrange(desc(year.season_start))
      } else {
        data %<>%
          dplyr::filter(!slug.team == 'TOT')
      }
    }

    data %<>%
      mutate(name.player = player,
             name.table = tables_names[i]) %>%
      dplyr::select(
        id.season,
        name.player,
        is.college,
        is.regular_season,
        is.playoffs,
        is.allstar,
        slug.team,
        everything()
      )
  } else {
    data <-
      data_frame(name.player = player,
                 name.table = tables_names[i])
    if (tables_names[i] %>% str_detect("College")) {
      data %<>%
        mutate(is.college = T)
    } else {
      data %<>%
        mutate(is.college = F)
    }

    if (tables_names[i] %>% str_detect("PostSeason")) {
      data %<>%
        mutate(is.playoffs = T)
    } else {
      data %<>%
        mutate(is.playoffs = F)
    }

    if (tables_names[i] %>% str_detect("AllStar")) {
      data %<>%
        mutate(is.allstar = T)
    } else {
      data %<>%
        mutate(is.allstar = F)
    }

    if (tables_names[i] %>% str_detect("RegularSeason")) {
      data %<>%
        mutate(is.regular_season = T)
    } else {
      data %<>%
        mutate(is.regular_season = F)
    }
  }
  return(data)
}
get_player_career_stat <- function(player,
                                   id.player = NULL,
                                   use_totals = T,
                                   per_mode = "Totals",
                                   include_college = T,
                                   return_message = T) {
  packages <- #need all of these installed including some from github
    c('dplyr',
      'magrittr',
      'jsonlite',
      'tidyr',
      'purrr',
      'stringr',
      'data.table',
      'tidyr')
  options(warn = -1)


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

  mode <-
    per_mode %>%
    str_to_title() %>%
    str_replace('\\ ', '') %>%
    match.arg(c('Totals', 'PerGame', "Per36"))

  url <-
    'http://stats.nba.com/stats/playercareerstats?LeagueID=00&http://stats.nba.com/stats/playercareerstats?LeagueID=00&PerMode=' %>%
    paste0(mode, '&PlayerID=', id.player)

  json_data <-
    url %>%
    fromJSON(simplifyDataFrame = T, flatten = T)

  tables_names <-
    json_data$resultSets$name
  ut <-
    use_totals

  all_data <-
    1:length(tables_names) %>%
    purrr::map(
      function(x)
        get_nba_career_stat_table(
          i = x,
          json_data = json_data,
          tables_names = tables_names,
          use_totals = ut
        )
    ) %>%
    compact() %>%
    bind_rows

  if ('slug.team' %in% names(all_data)){
  if (all_data %>% dplyr::filter(name.table == 'SeasonTotalsCollegeSeason') %>% nrow > 0) {
    college <-
      all_data %>%
      dplyr::filter(name.table == 'SeasonTotalsCollegeSeason') %>%
      dplyr::select(-c(name.table,
                starts_with("rank"),
                starts_with("is.")))
    if ('id.organization' %in% names(college)) {
      college %<>%
        dplyr::select(-id.organization)
    }
  }

  if (all_data %>% dplyr::filter(name.table == 'SeasonTotalsPostSeason') %>% nrow > 0) {
    ps <-
      all_data %>%
      dplyr::filter(name.table == 'SeasonTotalsPostSeason') %>%
      dplyr::select(-c(name.table,
                starts_with("rank"),
                starts_with("is.")))

    if ('id.organization' %in% names(ps)) {
      ps %<>%
        dplyr::select(-id.organization)
    }
    names(ps)[8:length(names(ps))] %<>%
      paste0('.playoffs')
  }

  if (all_data %>% dplyr::filter(name.table == 'SeasonTotalsAllStarSeason') %>% nrow > 0) {
    as <-
      all_data %>%
      dplyr::filter(name.table == 'SeasonTotalsAllStarSeason') %>%
      dplyr::select(-c(name.table,
                starts_with("rank"),
                starts_with("is.")))
    if ('id.organization' %in% names(as)) {
      as %<>%
        dplyr::select(-id.organization)
    }
    names(as)[8:length(names(as))] %<>%
      paste0('.allstar')
  }

  if (all_data %>% dplyr::filter(name.table == 'SeasonTotalsRegularSeason') %>% nrow > 0) {
    rs <-
      all_data %>%
      dplyr::filter(name.table == 'SeasonTotalsRegularSeason') %>%
      dplyr::select(-c(name.table,
                starts_with("rank"),
                starts_with("is.")))
    if ('id.organization' %in% names(rs)) {
      rs %<>%
        dplyr::select(-id.organization)
    }
  }

  if (all_data %>% dplyr::filter(name.table == 'SeasonRankingsRegularSeason') %>% nrow > 0) {
    rs.rank <-
      all_data %>%
      dplyr::filter(name.table == 'SeasonRankingsRegularSeason') %>%
      dplyr::select(-c(name.table,
                gp:pts,
                age.player,
                starts_with("is.")))
    if ('id.organization' %in% names(rs.rank)) {
      rs.rank %<>%
        dplyr::select(-id.organization)
    }
  }

  if (all_data %>% dplyr::filter(name.table == 'SeasonRankingsPostSeason') %>% nrow > 0) {
    ps.rank <-
      all_data %>%
      dplyr::filter(name.table == 'SeasonRankingsPostSeason') %>%
      dplyr::select(-c(name.table,
                gp:pts,
                age.player,
                starts_with("is.")))

    if ('id.organization' %in% names(ps.rank)) {
      ps.rank %<>%
        dplyr::select(-id.organization)
    }
    if(names(ps.rank) %>% length > 7){
    names(ps.rank)[7:length(ps.rank %>% names)] %<>%
      paste0('.playoffs')
    }
  }

  data <-
    rs %>%
    bind_rows(college) %>%
    left_join(rs.rank) %>%
    left_join(ps) %>%
    left_join(ps.rank) %>%
    left_join(as) %>%
    dplyr::filter(!is.na(year.season_start)) %>%
    arrange(desc(year.season_start)) %>%
    dplyr::select(-contains(".allstar")) %>%
    mutate(mode = per_mode) %>%
    dplyr::select(mode, everything())

  data %<>%
    mutate(is.college = ifelse(slug.team %>% nchar > 3, T, F)) %>%
    dplyr::select(mode,is.college, everything())

  if(include_college == F){
    data %<>%
      dplyr::filter(!is.college == T)
  }

  if (return_message == T) {
    "You pulled NBA stats for " %>%
      paste0(player) %>%
      message()
  }

  return(data)
  } else{
    "No data for " %>%
      paste0(player) %>%
      message()
  }
}
