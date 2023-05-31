#'

.new_schedule <- function(url = "https://cdn.nba.com/static/json/staticData/scheduleLeagueV2_1.json") {
  json <-
    jsonlite::fromJSON(url, simplifyVector = T, simplifyDataFrame = T, flatten = T)
  tbl_broadcaster <- json$leagueSchedule$broadcasterList |> as_tibble() |> janitor::clean_names()
  tbl_weeks <- json$leagueSchedule$weeks |> unnest() |> as_tibble()
  season <- json$leagueSchedule$seasonYear
  id <- json$leagueSchedule$leagueId
  tbl_dates <- json$leagueSchedule$gameDates
  tbl_games <- tbl_dates |> select(games) |> unnest() |> janitor::clean_names()
  tbl_games_dates <-
    tbl_dates |> select(gameDate) |> unnest() |> janitor::clean_names()
}
