get_year_draft_data <- function(draft_year = 2015){
  base <-
    'http://stats.nba.com/stats/drafthistory?College=&LeagueID=00&OverallPick=&RoundNum=&RoundPick=&Season='

  url <-
    base %>%
    paste0(draft_year)

  json_data <-
    url %>%
    fromJSON()

  draft_data <-
    json_data$resultSets$rowSet %>%
    data.frame %>%
    tbl_df

  headers <-
    json_data$resultSets$headers %>% unlist %>%
    str_to_lower()

  names(draft_data) <-
    headers

  return(draft_data)

}

get_all_draft_data <- function(){
  years <-
    1947:2015

  all_drafts <-
    years %>%
    purrr::map(
      function(x)
        get_year_draft_data(
          draft_year = x
        )
    ) %>%
    compact %>%
    bind_rows

  all_drafts %<>%
    arrange(desc(season))

}
