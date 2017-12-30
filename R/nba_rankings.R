
#' Get NBA Team Rankings for most current period
#'
#' @return
#' @export
#' @import curl dplyr stringr readr purrr jsonlite
#' @importFrom lubridate ymd_hms
#' @examples
get_nba_team_current_rankings <-
  function() {
    json <-
      "https://data.nba.net/prod/v1/2017/team_stats_rankings.json" %>%
      curl_json_to_vector()

    datetimePublished <- json$`_internal`$pubDateTime %>% lubridate::ymd_hms()
    json_data <- json$league$standard
    dict_names <- dictionary_nba_names()
    year_season_start <- json_data$seasonYear
    teams <-
      json_data$regularSeason$teams[1] %>% pull(1) %>% as.numeric()
    col_nos <- 2:ncol(json_data$regularSeason$teams)
    data <- col_nos %>%  map(function(x) {
      var_data <- json_data$regularSeason$teams[x]
      var_name <- var_data %>% names() %>% str_to_upper()
      name_actual <-
        resolve_nba_names(json_names = var_name)
      types <- c("Avg", "Rank")
      var_names <-
        glue::glue("{name_actual}{types}") %>% as.character()

      var_df <-
        var_data %>%
        flatten_df() %>%
        mutate_all(readr::parse_number) %>%
        purrr::set_names(var_names) %>%
        mutate(idTeam = teams) %>%
        suppressWarnings()
      var_df
    })

     data <-
       data %>%
       purrr::reduce(left_join) %>%
       mutate(yearSeasonStart = year_season_start,
              datetimePublished) %>%
       left_join(get_nba_teams() %>% select(idTeam, nameTeam)) %>%
       suppressMessages() %>%
       mutate(typeSeason = "Regular Season") %>%
       select(yearSeasonStart, typeSeason, datetimePublished, idTeam, nameTeam, everything()) %>%
       filter(!minutesAvg %>% is.na())


     data
  }
