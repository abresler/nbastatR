#' Title
#'
#' @param active_only
#'
#' @return
#' @export
#'
#' @examples
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
