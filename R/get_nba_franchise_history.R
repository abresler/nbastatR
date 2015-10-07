#' Title
#'
#' @param return_franchises 
#' @param return_message 
#'
#' @return
#' @export
#'
#' @examples
get_nba_franchise_data <- function(return_franchises = c('all', 'active', 'current'),
                                   return_message = T){
  packages <- #need all of these installed including some from github
    c(
      'dplyr',
      'magrittr',
      'jsonlite',
      'tidyr',
      'stringr',
      'tidyr'
    )
  options(warn = -1)
  lapply(packages, library, character.only = T)
  team_history_url <-
    'http://stats.nba.com/stats/franchisehistory?LeagueID=00'
  
  team_data <- 
    team_history_url %>% 
    fromJSON(simplifyDataFrame = T, flatten = )
  
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
  
  if(return_franchises == 'current') {
    data %<>% 
    mutate(id.row = 1:nrow(.)) %>% 
    group_by(team_id) %>% 
    dplyr::filter(id.row == min(id.row), is.active == T) %>% 
      select(-id.row)
  }
  
  if (return_franchises == 'active') {
    data %<>% 
      dplyr::filter( is.active == T)
  }
  if(return_message == T){
    "You got NBA franchise data" %>% 
      message
  }
  return(data)
}
  