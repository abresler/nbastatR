
#2000-01 to 2015-16
get_year_draft_combine <- function(year.season_start = 2015, return_message = T){

  id.season <-
    year.season_start %>%
    paste0("-", (year.season_start + 1) %>% substr(3,4))

  base <-
    'http://stats.nba.com/stats/draftcombinestats?LeagueID=00&SeasonYear='
  url <-
    base %>%
    paste0(id.season)

  json_data <-
    url %>%
    fromJSON(simplifyDataFrame = T)

  data <-
    json_data$resultSets$rowSet %>%
    data.frame %>%
    tbl_df

  headers <-
    json_data$resultSets$headers %>% unlist %>%
    str_to_lower()
  ## add in header DF
  names(data) <-
    headers
  data %<>%
    mutate(id.season) %>%
    dplyr::select(id.season, everything())
  if(return_message == T){
    "You got draft combine data for the " %>%
      paste0(year.season_start," draft combine")
  }
  return(data)
}

get_all_draft_combines <- function(){
 years <-
   2000:2015

 all_draft_combines <-
   years %>%
   purrr::map(
     function(x)
       get_year_draft_combine(
         year.season_start = x,
         return_message = T
       )
   ) %>%
   compact %>%
   bind_rows

}
