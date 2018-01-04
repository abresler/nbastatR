
# http://stats.nba.com/stats/scoreboardv2/?leagueId=00&gameDate=12%2F30%2F1990&dayOffset=0

get_day_nba_scores <-
  function(game_date = Sys.Date() - 1,
           day_offset= 0,
           include_standings = F,
           return_message = TRUE) {
    game_date <-
      game_date %>%
      readr::parse_date()

    if (return_message) {
      glue::glue("Getting NBA game details for {game_date}") %>%
        message()
    }

    date_slug <-
      parse_to_date_url(game_date = game_date)

    url <-
      glue::glue(
        "http://stats.nba.com/stats/scoreboardv2/?leagueId=00&gameDate={date_slug}&dayOffset={day_offset}"
      ) %>%
      as.character()
    json <-
      curl_json_to_vector(url = url)

    tables_data <- json$resultSets
    table_names <- json$resultSets
    tables <- json$resultSets$rowSet %>% length()

    data <-
      1:tables %>%
      map_df(function(x) {
        json_names <-
          tables_data$headers[[x]]
        table_name <- tables_data$name[[x]]

        if (table_name %in% c("TicketLinks", "Available")) {
          return(invisible())
        }
        if (!include_standings) {
          if (table_name %>% str_detect("Standings")) {
            return(invisible())
          }
        }
        data <-
          tables_data$rowSet[[x]] %>% as_data_frame()

        if (data %>% nrow() == 0) {
          return(invisible())
        }

        actual_names <- json_names %>% resolve_nba_names()
        data <-
          data %>% purrr::set_names(actual_names)

        data <-
          data %>%
          munge_nba_data() %>%
          mutate(nameTable = table_name,
                 dateGame = game_date) %>%
          remove_na_columns()

        if (data %>% tibble::has_name("cityTeam")) {
          data <-
            data %>%
            unite(nameTeam,
                  cityTeam,
                  teamName,
                  sep = " ",
                  remove = F)
        }

        if (data %>% tibble::has_name("dateStandings")) {
          data <-
            data %>%
            mutate(dateStandings = dateStandings %>% lubridate::mdy())
        }

        data %>%
          nest(-c(nameTable, dateGame), .key = 'dataTable')
      })
    data
  }


#' Get NBA Dates' NBA Scores
#'
#' Returns nba score data for a given date
#'
#' @param game_dates vector of dates
#' @param day_offset day offset
#' @param include_standings if \code{TRUE} includes standings as of the date
#' @param assign_to_environment if \code{TRUE} assigns each table to environment with name containing dataScore
#' @param return_message if \code{TRUE} returns a message
#'
#' @return a \code{data_frame}
#' @export
#' @import dplyr stringr curl jsonlite lubridate purrr tidyr rlang readr tibble
#' @importFrom glue glue
#' @examples
#' get_days_nba_scores(game_dates = "2017-12-31", include_standings = F, return_message = T)
get_days_nba_scores <-
  function(game_dates = NULL,
           day_offset= 0,
           include_standings = F,
           assign_to_environment = T,
           return_message = TRUE) {
    if (game_dates %>% purrr::is_null()) {
      stop("Please enter game dates")
    }
    input_df <-
      expand.grid(game_date = game_dates,
                  day_offset = day_offset,
                  stringsAsFactors = F) %>%
      as_data_frame()

    get_day_nba_scores_safe <-
      purrr::possibly(get_day_nba_scores, data_frame())

    all_data <-
      1:nrow(input_df) %>%
      map_df(function(x) {
        df_row <- input_df %>% slice(x)
        df_row %$%
          get_day_nba_scores_safe(
            game_date = game_date,
            return_message = return_message,
            include_standings = include_standings,
            day_offset = day_offset
          )
      }) %>%
      suppressWarnings()

    tables <- all_data$nameTable %>% unique()

    all_data <-
      tables %>%
      map_df(function(table){
        df_row <-
          all_data %>%
          filter(nameTable == table) %>%
          unnest() %>%
          distinct()
        df_row %>%
          nest(-c(nameTable), .key = 'dataTable')
      })

    if (assign_to_environment) {
      tables %>%
        walk(function(table){
          table_name <-
            glue::glue("dataScore{table}")
          df_row <-
            all_data %>%
            filter(nameTable == table) %>%
            unnest() %>%
            select(-nameTable) %>%
            distinct()

          assign(table_name, df_row,envir = .GlobalEnv)
        })
    }
    all_data
  }
