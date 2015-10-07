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
  
  players.data <-
    players.url %>%
    jsonlite::fromJSON(simplifyDataFrame = T)
  
  players <-
    players.data$resultSets$rowSet %>%
    data.frame %>%
    tbl_df
  
  names(players) <-
    players.data$resultSets$headers %>%
    unlist %>%
    tolower
  
  players %<>%
    separate(
      display_last_comma_first,
      sep = '\\,',
      into = c('name.last', 'name.first')
    ) %>%
    rename(id.player = person_id,
           id.team = team_id,
           year.from = from_year,
           year.to = to_year,
           city.team = team_city,
           name.team = team_name,
           code.player = playercode,
           code.team = team_code,
           stem.team = team_abbreviation
           ) %>%
    mutate(
      name.first = name.first %>% gsub("[^A-Z a-z]", '', .),
      name.player = ifelse(
        name.first %>% is.na,
        name.last,
        paste(name.first %>% str_trim, name.last %>% str_trim) %>% str_to_title()
      ),
      id.player = id.player %>% as.numeric,
      is.active_player = ifelse(id.team == 0, FALSE, TRUE),
      id.team = id.team %>% as.numeric
    ) %>%
    select(-c(rosterstatus)) %>% 
    mutate_each(funs(extract_numeric), starts_with("year.")) %>% 
    mutate(id.team = ifelse(id.team == 0, NA, id.team),
           city.team = ifelse(city.team == '', NA, city.team),
           name.team = ifelse(name.team == '', NA, name.team),
           code.team = ifelse(code.team == '', NA, code.team),
           stem.team = ifelse(stem.team == '', NA, stem.team),
           team = ifelse(city.team %>% is.na, NA, paste(city.team, name.team)),
           seasons.played = year.to - year.from,
           url.player = id.player %>% paste0('http://stats.nba.com/player/#!/')
           ) %>% 
    select(name.player, id.player, team, id.team, is.active_player, seasons.played, 
           year.from, year.to, 
           everything())
  
  if(active_only == T){
    players %<>% 
      dplyr::filter(is.active_player == T)
  }
  
  return(players)
}