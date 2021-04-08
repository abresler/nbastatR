
# https://stats.nba.com/stats/scoreboardv2/?leagueId=00&gameDate=12%2F30%2F1990&dayOffset=0


.get_day_nba_scores <-
  function(game_date = Sys.Date() - 2,
           league = "NBA",
           day_offset= 0,
           include_standings = F,
           return_message = TRUE) {
    game_date <-
      game_date %>%
      as.character() %>%
      parse_date()


    league_slug <-
      case_when(
        league %>% str_to_upper() == "WNBA" ~ "10",
        league %>% str_to_upper() == "GLEAGUE" ~ "20",
        TRUE ~ "00"
      )

    if (return_message) {
      glue("Getting {league} game details for {game_date}") %>%
        cat(fill = T)
    }

    date_slug <-
      parse_to_date_url(game_date = game_date)

    url <-
      glue(
        "https://stats.nba.com/stats/scoreboardv2/?leagueId={league_slug}&gameDate={date_slug}&dayOffset={day_offset}"
      ) %>%
      as.character()

    json <-
      url %>%
      .curl_chinazi()

    tables_data <- json$resultSets
    table_names <- json$resultSets
    tables <- json$resultSets$rowSet %>% length()

    data <-
      1:tables %>%
      future_map_dfr(function(x) {
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
          tables_data$rowSet[[x]] %>%
          data.frame(stringsAsFactors = F) %>%
          as_tibble()

        if (data %>% nrow() == 0) {
          return(invisible())
        }

        actual_names <- json_names %>% resolve_nba_names()
        data <-
          data %>% set_names(actual_names)

        data <-
          data %>%
          munge_nba_data() %>%
          mutate(nameTable = table_name,
                 dateGame = game_date) %>%
          remove_na_columns()

        if (data %>% has_name("cityTeam")) {
          data <-
            data %>%
            unite(nameTeam,
                  cityTeam,
                  teamName,
                  sep = " ",
                  remove = F)
        }

        if (data %>% has_name("dateStandings")) {
          data <-
            data %>%
            mutate(dateStandings = dateStandings %>% lubridate::mdy())
        }

        data %>%
          nest(-c(nameTable, dateGame), .key = dataTable)
      })
    data
  }


#' Get NBA Dates' NBA Scores
#'
#' Returns nba score data for a given date
#'
#' @param game_dates vector of dates
#' @param league League \itemize{
#' \item WNBA
#' \item NBA
#' }
#' @param day_offset day offset
#' @param include_standings if \code{TRUE} includes standings as of the date
#' @param assign_to_environment if \code{TRUE} assigns each table to environment with name containing dataScore
#' @param return_message if \code{TRUE} returns a message
#'
#' @return a \code{tibble}
#' @export
#' @import dplyr stringr curl jsonlite lubridate purrr tidyr rlang readr tibble
#' @importFrom glue glue
#' @examples
#' days_scores(game_dates = "2017-12-31", include_standings = F, return_message = T)
days_scores <-
  function(game_dates = NULL,
           league = "NBA",
           day_offset= 0,
           include_standings = F,
           assign_to_environment = TRUE,
           return_message = TRUE) {
    if (game_dates %>% is_null()) {
      stop("Please enter game dates")
    }
    input_df <-
      expand.grid(game_date = game_dates,
                  day_offset = day_offset,
                  stringsAsFactors = F) %>%
      as_tibble()

    .get_day_nba_scores_safe <-
      possibly(.get_day_nba_scores, tibble())

    all_data <-
      1:nrow(input_df) %>%
      future_map_dfr(function(x) {
        df_row <- input_df %>% slice(x)
        df_row %$%
          .get_day_nba_scores_safe(
            game_date = game_date,
            league = league,
            return_message = return_message,
            include_standings = include_standings,
            day_offset = day_offset
          )
      }) %>%
      suppressWarnings()

    tables <- all_data$nameTable %>% unique()

    all_data <-
      tables %>%
      future_map_dfr(function(table) {
        df_row <-
          all_data %>%
          filter(nameTable == table) %>%
          unnest() %>%
          distinct()
        df_row %>%
          nest(-c(nameTable), .key = dataTable)
      })

    if (assign_to_environment) {
      tables %>%
        walk(function(table) {
          table_name <-
            glue("dataScore{table}{league}")
          df_row <-
            all_data %>%
            filter(nameTable == table) %>%
            unnest() %>%
            select(-nameTable) %>%
            distinct()

          assign(table_name, df_row, envir = .GlobalEnv)
        })
    }
    all_data
  }
