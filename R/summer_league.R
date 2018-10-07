#' Summer League Players
#'
#' @param year summer league year
#' @param assign_to_environment if \code{true assigns to environment}
#'
#' @return a nested \code{data_frame}
#' @export
#'
#' @examples
#' sl_players()
sl_players <-
  function(year = 2018,
           assign_to_environment = T) {

    json_data <-
      glue::glue("https://data.nba.net/prod/v1/{year}/players.json") %>%
      fromJSON(simplifyDataFrame = T)
    json_data <- json_data$league
    leagues <-
      names(json_data)

    data <-
      leagues %>%
      map_df(function(league){
        data <-
          json_data[[league]] %>%
          as_data_frame()

        json_names <-
          names(data)

        actual_names <-
          json_names %>% resolve_nba_names()

        data <-
          data %>% purrr::set_names(actual_names)

        height_cols <- data %>% select(matches("height")) %>% names()

        df <-
          data %>%
          unite(namePlayer, nameFirst, nameLast, sep = " ") %>%
          select(-matches("data")) %>%
          munge_nba_data()

        if (df %>% tibble::has_name("dateBirth")) {
          df <- df %>% mutate_at('dateBirth',
                                 funs(lubridate::ymd))
        }
        if (height_cols %>% length() > 0) {
          height_change <- height_cols[height_cols %in% names(df)]

          df <-
            df %>%
            mutate_at(height_change,
                      funs(. %>% as.numeric()))  %>%
            mutate(heightPlayer = (12 * heightFeet) + heightInches) %>%
            suppressWarnings()
        }

        if (data %>% tibble::has_name("dataTeams")) {
          df_teams <-
            data %>%
            select(idPlayer, dataTeams) %>%
            unnest()

          if (df_teams %>% nrow() > 0) {
          df_teams <-
            df_teams %>%
            purrr::set_names(names(df_teams) %>%
            resolve_nba_names()) %>%
            munge_nba_data()

          df_teams <- df_teams %>% nest(-idPlayer, .key = dataTeams)

          df <-
            df %>%
            left_join(df_teams) %>%
            suppressMessages()
          }
        }

        if (data %>% tibble::has_name("dataDraft")) {
          df_draft <-
            data$dataDraft %>% as_data_frame() %>%
            mutate(idPlayer = df$idPlayer) %>%
            select(idPlayer, everything()) %>%
            purrr::set_names(c("idPlayer", "idTeamDrafted", "numberPick", "numberRound", "yearDraft")) %>%
            mutate_all(as.numeric)

          df <-
            df %>%
            left_join(df_draft) %>%
            suppressMessages()
        }

        df <-
          df %>%
          mutate(yearData = year) %>%
          select(yearData, everything())


        if (assign_to_environment) {
          league_slug <- case_when(league == "standard" ~ "",
                                   TRUE ~ league %>% str_to_title())

          table_name <- glue::glue("dataPlayers{league_slug}")

          assign(x = table_name, df, envir = .GlobalEnv)

        }

        data_frame(slugLeague = league, dataTeams = list(df))
      })

  }


#' Summer League Teams
#'
#' @param assign_to_environment if \code{TRUE} assings to environment
#'
#' @return a nested \code{data_frame}
#' @export
#'
#' @examples
#' sl_teams()
sl_teams <-
  function(assign_to_environment = T) {
    json_data <-
      "https://data.nba.net/prod/v1/2018/teams.json" %>%
      fromJSON(simplifyDataFrame = T)
    json_data <- json_data$league

    leagues <- names(json_data)

    data <-
      leagues %>%
      map_df(function(league){
        data <-
          json_data[[league]] %>%
          as_data_frame()

        json_names <-
          names(data)

        actual_names <-
          json_names %>% resolve_nba_names()

        data <-
          data %>% purrr::set_names(actual_names)

        data <-
          data %>%
          munge_nba_data() %>%
          mutate_if(is.character,
                    funs(ifelse(. == "", NA_character_, .)))

        if (assign_to_environment) {
          league_slug <- case_when(league == "standard" ~ "",
                    TRUE ~ league %>% str_to_title())

          table_name <- glue::glue("dataTeams{league_slug}")

          assign(x = table_name, data, envir = .GlobalEnv)

        }

        data_frame(slugLeague = league, dataTeams = list(data))
      })

    data
  }

.parse_sl_schedule_url <-
  function(url = "https://data.nba.com/data/10s/v2015/json/mobile_teams/vegas/2018/league/15_full_schedule.json") {
    data <- url %>% jsonlite::fromJSON(simplifyDataFrame = T)
  }
