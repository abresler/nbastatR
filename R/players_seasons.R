
.get_nba_season_players <-
  function(season = NULL,
           return_message = T) {
    if (season %>% is_null()) {
      stop("Please enter as season")
    }

    if (season < 1945) {
      stop("Sorry data starts in 1945")
    }

    year_season_start <-
      season - 1

    slugSeason <-
      year_season_start %>%
      {
        year.season_end  <- . + 1
        paste0(year_season_start, "-", (year.season_end %>% substr(3, 4)))
      }

    all_players <-
      nba_players()

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
      mutate(slugSeason,
             yearSeason = season) %>%
      dplyr::select(slugSeason, slugSeason,
                    everything())

    seasons_players

    if (return_message) {
      "You got all NBA players for the " %>%
        paste0(slugSeason, " Season") %>%
        cat(fill = T)
    }

    seasons_players
  }


#' Seasons players
#'
#' @param seasons numeric vector of seasons
#' @param nest_data  if \code{TRUE} returns a nested tibble
#' @param return_message  if \code{TRUE} return message
#'
#' @return a `tibble`
#' @export
#' @import dplyr purrr readr jsonlite tidyr curl
#' @importFrom glue glue
#' @examples
#' seasons_players(2010:2017, nest_data = T, return_message = T)

seasons_players <-
  function(seasons = 1960:2018,
           nest_data = F,
           return_message = T) {
    .get_nba_season_players_safe <-
      possibly(.get_nba_season_players, tibble())

    all_data <-
      seasons %>%
      future_map_dfr(function(year_season_start){
        .get_nba_season_players_safe(season = year_season_start,
                               return_message = return_message)
      })

    all_data <-
      all_data %>%
      dplyr::select(-one_of(c("idTeam","cityTeam","teamName","slugTeam","codeTeam" )))
    if (nest_data) {
      all_data  <-
        all_data %>%
        nest(-c(slugSeason), .key = dataSeasonPlayers) %>%
        mutate(countPlayers = dataSeasonPlayers %>% map_dbl(nrow) %>% as.integer())
    }
    all_data
  }
