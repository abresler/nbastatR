
.parse_draftpick_url <-
  function(url = "https://data.nba.net/data/5s/prod/draft/2017/draft_pick.json") {
    data <- url %>% jsonlite::fromJSON(simplifyDataFrame = T) %>% as_data_frame()
    data
  }
