
get_nba_season_players <-
  function(year_season_start = 2012,
           return_message = T) {
    if (!'year_season_start' %>% exists()) {
      stop("Please enter a start to the season")
    }

    if (year_season_start < 1945) {
      stop("Sorry data starts in 1945")
    }

    slugSeason <-
      year_season_start %>%
      {
        year.season_end  <- . + 1
        paste0(year_season_start, "-", (year.season_end %>% substr(3, 4)))
      }

    all_players <-
      get_nba_players()

    nba_year.from <-
      year_season_start

    nba_year.to <-
      year_season_start

    seasons_players <-
      all_players %>%
      dplyr::filter(nba_year.to <= yearSeasonLast) %>%
      dplyr::filter(nba_year.from >= yearSeasonFirst)

    seasons_players <-
      seasons_players %>%
      mutate(slugSeason) %>%
      dplyr::select(slugSeason,
                    everything())

    seasons_players

    if (return_message) {
      "You got all NBA players for the " %>%
        paste0(slugSeason, " Season") %>%
        message()
    }

    seasons_players
  }


#' Get Season's Players
#'
#' @param years_start vector of years start
#' @param nest_data  if \code{TRUE} returns a nested data_frame
#' @param return_message  if \code{TRUE} return message
#'
#' @return
#' @export
#' @import dplyr purrr readr jsonlite tidyr curl
#' @importFrom glue glue
#' @examples
#' get_nba_seasons_players(2010:2017, nest_data = T, return_message = T)

get_nba_seasons_players <-
  function(years_start = 1960:2017,
           nest_data = F,
           return_message = T) {
    get_nba_season_players_safe <-
      purrr::possibly(get_nba_season_players, data_frame())

    all_data <-
      years_start %>%
      map_df(function(year_season_start){
        get_nba_season_players_safe(year_season_start = year_season_start,
                               return_message = return_message)
      })

    all_data <-
      all_data %>%
      dplyr::select(-one_of(c("idTeam","cityTeam","teamName","slugTeam","codeTeam" )))
    if (nest_data) {
      all_data  <-
        all_data %>%
        tidyr::nest(-c(slugSeason), .key = 'dataSeasonPlayers') %>%
        mutate(countPlayers = dataSeasonPlayers %>% map_dbl(nrow) %>% as.integer())
    }
    all_data
  }
