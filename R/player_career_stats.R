

.get_nba_player_career_stats <-
  function(player_id = 1628386,
           mode = "Totals",
           return_message = TRUE) {
    if (player_id %>% purrr::is_null()) {
      stop("Please enter an NBA player ID")
    }

    if (!'df_nba_player_dict' %>% exists()) {
      df_nba_player_dict <-
        nba_players()

      assign(x = 'df_nba_player_dict', df_nba_player_dict, envir = .GlobalEnv)
    }

    player <-
      df_nba_player_dict %>%
      filter(idPlayer == player_id) %>%
      pull(namePlayer)

    if (return_message) {
      glue::glue("Acquiring {player} career {mode} statistic tables") %>% cat(fill = T)
    }
    mode_options <-
      c('Totals', 'PerGame', "Per36") %>% str_to_upper()
    mode_slug <- str_to_upper(mode)

    if (!mode_slug %in% mode_options) {
      stop("Mode can only be Totals, PerGame or Per36")
    }

    url <-
      glue::glue(
        "https://stats.nba.com/stats/playercareerstats?LeagueID=00&PerMode={mode}&PlayerID={player_id}"
      ) %>%
      as.character()

    json <-
      curl::curl(url) %>%
      fromJSON(simplifyVector = T)

    table_length <-
      json$resultSets$rowSet %>% length()

    all_data <-
      1:table_length %>%
      future_map_dfr(function(x) {
        table_name <-
          json$resultSets$name[x]


        json_names <-
          json$resultSets$headers[[x]]

        data <-
          json$resultSets$rowSet[[x]] %>%
          data.frame(stringsAsFactors = F) %>%
          as_tibble()
        if (data %>% nrow() == 0) {
          return(invisible())
        }

        actual_names <-
          json_names %>%
          resolve_nba_names()

        data <-
          data %>%
          purrr::set_names(actual_names) %>%
          munge_nba_data() %>%
          mutate(
            modeSearch = mode,
            nameTable = table_name,
            idPlayer = player_id,
            namePlayer = player
          ) %>%
          select(nameTable, modeSearch, namePlayer, everything())

        if (data %>% tibble::has_name("slugSeason")) {
          df_players_seasons <-
            data %>%
            distinct(slugSeason) %>%
            mutate(numberPlayerSeason = 1:n() -
                     1) %>%
            mutate(isRookie = ifelse(numberPlayerSeason == 0, T, F))

          data <-
            data %>%
            left_join(df_players_seasons) %>%
            suppressMessages() %>%
            dplyr::select(nameTable:slugSeason,
                          numberPlayerSeason,
                          isRookie,
                          everything())
        }


        data <-
          data %>%
          dplyr::select(-one_of("idLeague")) %>%
          nest(-c(nameTable, modeSearch, idPlayer, namePlayer), .key = dataTable) %>%
          mutate(urlNBAAPI = url) %>%
          suppressWarnings()

        data
      })
    all_data
  }

#' Player career stats
#'
#' NBA player career statistics for
#' specified players and inputs
#'
#'
#' @param players \code{NULL} or \code{vector} of NBA players
#' @param player_ids \code{vector} of NBA Player IDs
#' @param modes \code{vector} of items that can include \itemize{
#' \item Totals
#' \item PerGame
#' \item Per36
#' }
#' @param assign_to_environment if \code{TRUE} assign to each table environment
#' with a name starting with data
#' @param add_mode_names if \code{TRUE} adds the type of mode to the table data
#' @param return_message if \code{TRUE} returns a message
#'
#' @return a \code{tibble}
#' @export
#' @importFrom glue glue
#' @family player
#' @family summary stats
#' @import jsonlite dplyr purrr tibble stringr tidyr curl
#' @examples
#' players_careers(players = c("Joe Harris", "Myles Turner", "Spencer Dinwiddie"),
#' modes = c("Totals", "PerGame"))

players_careers <-
  function(players = NULL,
           player_ids = NULL,
           modes = c("PerGame", "Totals"),
           assign_to_environment = TRUE,
           add_mode_names = TRUE,
           return_message = TRUE) {
    if (modes %>% purrr::is_null()) {
      stop("Please enter a valid mode")
    }
    ids <- c()

    if (!player_ids %>% purrr::is_null()) {
      ids <-
        ids %>%
        append(player_ids)
    }

    if (!players %>% purrr::is_null()) {
      players_search <-
        nba_player_ids(players = players)

      ids <-
        ids %>%
        append(players_search)
    }

    if (ids %>% length() == 0) {
      stop("Please enter valid players and/or player ids")
    }

    df_input <-
      expand.grid(
        player_id = ids %>% unique() %>% sort(),
        mode = modes,
        stringsAsFactors = F
      ) %>%
      dplyr::as_tibble()


    get_nba_player_career_stats_safe <-
      purrr::possibly(.get_nba_player_career_stats, tibble())

    all_data <-
      1:nrow(df_input) %>%
      future_map_dfr(function(x) {
        df_row <-
          df_input %>% slice(x)

        get_nba_player_career_stats_safe(
          player_id = df_row$player_id,
          mode = df_row$mode,
          return_message = return_message
        )
      })

    if (assign_to_environment) {
      assign_tables_modes(
        all_data = all_data,
        stat_type = "Player",
        add_mode_names = add_mode_names
      )
    }

    all_data
  }
