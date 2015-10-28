
#' Title
#'
#' @param year.season_start
#' @param season_type
#' @param return_message
#'
#' @return
#' @export
#'
#' @examples
#all_data <- get_batch_player_gamelogs(year.season_start = 2015, season_type = "Pre Season",
 #                         return_message = T)
get_batch_player_gamelogs <-
  function(year.season_start,
           season_type,
           return_message = T) {

    players <-
      get_nba_players_ids()

    type.season <-
      season_type
    rm <-
      return_message
    yst <-
      year.season_start

    season <-
      year.season_start %>%
      extract_numeric %>%
      paste0("-", (year.season_start + 1) %>% substr(start = 3, stop = 4))

    players_then <-
      players %>%
      dplyr::filter(
        year.season_start >= year.from & year.season_start <= year.to
                    )

    player_ids <-
      players_then$id.player

    all_data <-
      player_ids %>%
      purrr::map(function(x)
        get_player_season_gamelog(season_type = type.season, year.season_start = yst,
                                  id.player = x, return_message = rm)) %>%
      compact %>%
      bind_rows

    if (return_message == T){
      "You got all game logs for the " %>%
        paste0(season, " ", season_type) %>%
        message
    }
    return(all_data)
  }
