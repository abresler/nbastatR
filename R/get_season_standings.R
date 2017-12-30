#' Get NBA Current Standings
#'
#' @return
#' @export
#' @import dplyr curl stringr readr purrr jsonlite
#' @examples
#' get_nba_current_standings()
get_nba_current_standings <-
  function() {
    json <-
      "https://data.nba.net/prod/v1/current/standings_all_no_sort_keys.json" %>%
      curl_json_to_vector()
    json_data <- json$league$standard$teams
    select_cols <-
names(json_data)[!json_data %>% names() %>% str_detect("sortKey")]

    data <-
      json_data[select_cols] %>%
      dplyr::as_data_frame() %>%
      mutate_all(as.numeric) %>%
      remove_na_columns() %>%
      dplyr::rename(
        idTeam = teamId,
        wins = win,
        losss = loss,
        pctWins = winPct
      ) %>%
      left_join(get_nba_teams() %>% select(idTeam, nameTeam)) %>%
      mutate(dateStandings = Sys.Date()) %>%
      select(dateStandings, nameTeam, everything()) %>%
      suppressMessages()
    data
  }

get_seasons_playoff_picture <- function(year_season_start = 2015, is_pre_playoffs = T) {
  if (year_season_start <= 1970) {
    stop("Sorry data starts for the 1970-71 season")
  }

  id.season <-
    year_season_start %>%
    paste0("-",
      (year_season_start + 1) %>% substr(start = 3, stop = 4)
    )

  if (is_pre_playoffs) {
    json_url <-
      'http://stats.nba.com/stats/playoffpicture?LeagueID=00&SeasonID=2' %>%
      paste0(year_season_start)
  } else {
    json_url <-
      'http://stats.nba.com/stats/playoffpicture?LeagueID=00&SeasonID=1' %>%
      paste0(year_season_start)
  }

  json_data <-
    json_url %>%
    fromJSON()

  standings_data <-
    json_data$resultSets$rowSet[3:4] %>%
    map(function(x) unlist(x) %>% data.frame) %>%
    compact() %>%
    bind_rows

  standings_headers <-
    json_data$resultSets$headers[3:4] %>%
    map(function(x) unlist(x) %>% str_to_lower) %>%
    compact() %>%
    unlist %>%
    unique

  headers_df <-
    get_headers() %>%
    mutate(name.nba = name.nba %>% str_to_lower()) %>%
    distinct()

  actual_names <-
    1:length(standings_headers) %>%
    purrr::map(
      function(x)
        data_frame(
          name.actual =
            headers_df %>%
            mutate(name.nba = name.nba) %>%
            dplyr::filter(name.nba == standings_headers[x]) %>%
            .$name.actual
        )
    ) %>%
    bind_rows()

  names(standings_data) <-
    actual_names$name.actual


}


get_season_standing <-
  function(season = 2012, season_type = "Regular Season") {

  }
