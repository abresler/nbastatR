

.get_teams_season_rankings <-
  function(season = 2021,
           return_message = T) {
    year <- season - 1
    url <- glue("https://data.nba.net/prod/v1/{year}/team_stats_rankings.json") %>%
      as.character()
    json <-
      fromJSON(url)

    # .curl_chinazi()

    season_slug <- generate_season_slug(season = season)

    if (return_message) {
      glue("Getting {season_slug} team rankings") %>% cat(fill = T)
    }



    datetimePublished <-
      json$`_internal`$pubDateTime %>% lubridate::ymd_hms()
    json_data <- json$league$standard
    dict_names <- dictionary_nba_names()
    year_season_start <- json_data$seasonYear
    teams <-
      json_data$regularSeason$teams[1] %>% pull(1) %>% as.numeric()
    col_nos <- 2:ncol(json_data$regularSeason$teams)
    data <-
      col_nos %>%
      future_map(function(x) {
      var_data <- json_data$regularSeason$teams[x]
      var_name <- var_data %>% names() %>% str_to_upper()
      name_actual <-
        resolve_nba_names(json_names = var_name)

      if (name_actual == "NAME") {
        return(tibble())
      }
      types <- c("Avg", "Rank")
      var_names <-
        glue("{name_actual}{types}") %>% as.character()

      var_df <-
        var_data %>%
        flatten_df() %>%
        mutate_all(funs(. %>% as.character() %>% parse_number())) %>%
        set_names(var_names) %>%
        mutate(idTeam = teams) %>%
        suppressWarnings()
      var_df
    })

    data <-
      data %>%
      reduce(left_join) %>%
      mutate(slugSeason = season_slug,
             yearSeason = year_season_start + 1,
             datetimePublished) %>%
      left_join(nba_teams() %>% select(idTeam, nameTeam)) %>%
      suppressMessages() %>%
      mutate(typeSeason = "Regular Season") %>%
      select(
        slugSeason,
        yearSeason,
        typeSeason,
        datetimePublished,
        idTeam,
        nameTeam,
        everything()
      ) %>%
      filter(!minutesAvg %>% is.na())

    data

  }

#' NBA teams rankings
#'
#' Team rankings for specified season
#'
#' @param seasons vector of numeric seasons
#' @param nest_data  if `TRUE` nests data by season
#' @param return_message `TRUE` returns a message
#'
#' @return a `tibble`
#' @export
#' @import curl dplyr stringr readr purrr jsonlite
#' @importFrom lubridate ymd_hms
#' @family rankings
#' @family teams
#'
#' @examples
#' teams_rankings(seasons = 2019)
teams_rankings <-
  function(seasons = NULL,
           nest_data = F,
           return_message = T) {
    .get_teams_season_rankings_safe <-
      possibly(.get_teams_season_rankings, tibble())
    if (seasons %>% is_null()) {
      stop("Enter seasons")
    }
    all_data <-
      seasons %>%
      future_map_dfr(function(season){
        .get_teams_season_rankings_safe(season = season, return_message = return_message)
      })

    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-c(slugSeason), .key = "dataRankings")
    }
    all_data
  }
