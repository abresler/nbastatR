# http://nbasense.com/nba-api/Stats/Stats/Standings/LeagueStandings#request-example
# http://stats.nba.com/stats/leaguestandingsv3/?leagueId=00&season=1989-90&seasonType=Regular+Season

assign_nba_teams <-
  function() {
    if (!'df_dict_nba_teams' %>% exists()) {
      df_dict_nba_teams <- get_nba_teams()
      assign(x = 'df_dict_nba_teams', df_dict_nba_teams, envir = .GlobalEnv)
    }
  }

parse_record <-
  function(data, record_column = "recordTiedAtHalf") {
    df_set <-
      data %>%
      select(one_of(record_column)) %>%
      distinct()

    all_data <-
      df_set %>% pull() %>%
      map_df(function(x) {
        if (x %>% is.na()) {
          return(data_frame(UQ(record_column) := x))
        }
        names_set <-
          c(
            record_column,
            glue::glue("{record_column}Wins"),
            glue::glue("{record_column}Losses"),
            glue::glue("{record_column}WinPct")
          )
        values <-
          x %>% str_split("\\-") %>% flatten_chr() %>% as.numeric()

        data_frame(
          X1 = x,
          X2 = values[1],
          X3 = values[2],
          X4 = (X2 / (X2 + X3))
        ) %>%
          purrr::set_names(c(names_set))
      })

    data %>%
      left_join(all_data)

  }

#' Parse Record columns
#'
#' @param data a \code{data_frame}
#' @param record_names vecor of record_names
#'
#' @return
#' @export
#' @import glue dplyr purrr rlang stringr
#' @examples
parse_records <- function(data, record_names) {
  data <-
    record_names %>%
    map(function(record) {
      parse_record(data = data, record_column = record)
    }) %>%
    suppressMessages()

  data <-
    data %>%
    purrr::reduce(left_join) %>%
    suppressMessages()

  data
}

# current -----------------------------------------------------------------

#' Current NBA season standings
#'
#' Acquires current season's standings
#'
#' @param return_message if \code{TRUE} returns a message
#'
#' @return
#' @export
#'
#' @examples
#' get_current_standings()
get_current_standings <-
  function(return_message = TRUE) {
    json <-
      "https://data.nba.net/prod/v1/current/standings_all_no_sort_keys.json" %>%
      curl_json_to_vector()

    if (return_message) {
      "Acquring current NBA season standings" %>% message()
    }

    if (!'df_dict_nba_teams' %>% exists()) {
      df_dict_nba_teams <- get_nba_teams()
      assign(x = 'df_dict_nba_teams', df_dict_nba_teams, envir = .GlobalEnv)
    }

    data <-
      json$league$standard$teams %>%
      dplyr::as_data_frame()

    actual_names <- names(data) %>% resolve_nba_names()

    data <-
      data %>%
      purrr::set_names(actual_names) %>%
      dplyr::select(-matches("Remove")) %>%
      munge_nba_data() %>%
      mutate_at(c("pctLosses", "pctWins"),
                funs(. / 100)) %>%
      left_join(df_dict_nba_teams %>% select(idTeam, slugTeam, nameTeam, matches("url"))) %>%
      mutate(dateData = Sys.Date(),
             rankTeam = 1:n()) %>%
      select(dateData, rankTeam, idTeam, nameTeam, everything()) %>%
      suppressMessages()

    data

  }


# Playoff Picture ---------------------------------------------------------


get_season_playoff_picture <-
  function(season = 2015,
           return_message = T,
           include_numeric_records = F) {
    if (season < 1947) {
      stop("NBA data starts in the 1946-47 seasons")
    }
    season_slug <- season %>% generate_season_slug()
    season_id <- season - 1

    if (return_message) {
      glue::glue("Getting {season_slug} NBA playoff picture") %>% message()
    }
    url <-
      glue::glue(
        "http://stats.nba.com/stats/playoffpicture/?leagueId=00&seasonId=2{season_id}"
      ) %>% as.character() %>% URLencode()

    json <-
      curl_json_to_vector(url = url)
    tables_data <- json$resultSets
    tables <- json$resultSets$rowSet %>% length()

    data <-
      1:tables %>%
      map_df(function(x) {
        json_names <-
          tables_data$headers[[x]]
        table_name <- tables_data$name[[x]]
        data <-
          tables_data$rowSet[[x]] %>% as_data_frame()

        if (data %>% nrow() == 0) {
          return(invisible())
        }

        actual_names <- json_names %>% resolve_nba_names()
        data <-
          data %>% purrr::set_names(actual_names)
        if (data %>% tibble::has_name("slugTeam")) {
          data <-
            data %>%
            dplyr::rename(nameTeamAbbr = slugTeam)
        }
        data <-
          data %>%
          munge_nba_data()

        if (!data %>% tibble::has_name("nameConference")) {
          data <-
            data %>%
            mutate(nameConference = table_name %>% substr(1, 4))
        }

        if (include_numeric_records) {
          record_names <-
            data %>%
            dplyr::select(matches("record")) %>%
            names()

          if (record_names %>% length() > 0) {
           data <- parse_records(data = data, record_names = record_names)
          }
        }


        data_frame(
          numberTable = x,
          nameTable = table_name,
          dataTable = list(data)
        )
      })

    if (data %>% nrow() == 0) {
      return(invisible())
    }

    data <-
      data %>%
      select(-numberTable) %>%
      mutate(slugSeason = season_slug) %>%
      dplyr::select(slugSeason, everything())
    data
  }

#' Get seasons playoff picture
#'
#' Returns
#'
#' @param seasons vector of seasons
#' @param include_include_numeric_records if \code{TRUE} parses records
#' @param assign_to_environment if \code{TRUE} returns assigns a data frame for each table in environment
#' @param nest_data if \code{TRUE} returns nested data fram
#' @param return_message if \code{TRUE} returns a message
#'
#' @return
#' @export
#' @import dplyr stringr curl jsonlite lubridate purrr tidyr rlang readr tibble
#' @importFrom glue glue
#' @examples
#' get_seasons_playoff_picture(seasons = 2015:2018, assign_to_environment = T, include_numeric_records = T)
get_seasons_playoff_picture <-
  function(seasons = NULL,
           assign_to_environment = TRUE,
           include_numeric_records = F,
           nest_data = FALSE,
           return_message = TRUE) {

    if (seasons %>% purrr::is_null()) {
      stop("Please enter seasons")
    }
    input_df <-
      expand.grid(season = seasons,
                  stringsAsFactors = F) %>%
      dplyr::as_data_frame()
    get_season_playoff_picture_safe <-
      purrr::possibly(get_season_playoff_picture, data_frame())

    all_data <-
      1:nrow(input_df) %>%
      map_df(function(x) {
        input_df %>%
          slice(x) %$%
          get_season_playoff_picture_safe(season = season,
                                          include_numeric_records = include_numeric_records,
                                          return_message = return_message)
      })


    if (assign_to_environment) {
      table_slugs <-
        c("PlayoffPicture", "ConfStandings", "ConfRemainingGames")
      table_slugs %>%
        walk(function(slug) {
          table_name <-
            glue::glue("data{slug %>% str_replace_all('Conf', '')}") %>% as.character()
          df_table <-
            all_data %>%
            filter(nameTable %>% str_detect(slug)) %>%
            unnest() %>%
            remove_na_columns() %>%
            dplyr::select(matches("slugSeason|^id|^name"), everything()) %>%
            select(-nameTable)

          if (nest_data) {
            df_table <-
              df_table %>%
              nest(-slugSeason)
          }

          assign(table_name, df_table, envir = .GlobalEnv)
        })
    }
    all_data
  }

# standings ---------------------------------------------------------------


get_season_standings <-
  function(season = 2015,
           season_type = "Regular Season",
           return_message = T) {
    if (season < 1947) {
      stop("NBA data starts in the 1946-47 seasons")
    }
    season_slug <- season %>% generate_season_slug()
    if (return_message) {
      glue::glue("Getting {season_slug} {season_type} NBA standings data") %>% message()
    }
    url <-
      glue::glue(
        "http://stats.nba.com/stats/leaguestandingsv3/?leagueId=00&season={season_slug}&seasonType={season_type}"
      ) %>% as.character() %>% URLencode()

    json <-
      curl_json_to_vector(url = url)
    data <-
      json %>% nba_json_to_df() %>%
      mutate(yearSeason = season,
             slugSeason = season_slug,
             typeSeason = season_type) %>%
      dplyr::rename(teamName = nameTeam) %>%
      tidyr::unite(nameTeam,
                   cityTeam,
                   teamName,
                   sep = " ",
                   remove = F) %>%
      select(typeSeason, yearSeason, slugSeason, everything()) %>%
      dplyr::select(-one_of("idLeague")) %>%
      remove_na_columns()

    data
  }

#' Get seasons standing data
#'
#' NBA season standing data
#'
#' @param seasons vector of seasons
#' @param season_types type of season options include \itemize{
#' \item Regular Season
#' \item Pre Season
#' }
#' @param resolve_records \code{TRUE} resolve records into wins, losses and win percentage
#' @param return_message if \code{TRUE} returns a message
#' @param nest_data if \code{TRUE} returns a nesed data frame
#'
#' @return
#' @export
#' @import dplyr stringr curl jsonlite lubridate purrr tidyr rlang readr tibble
#' @importFrom glue glue
#' @examples
#' get_seasons_standings(seasons = 2015:2018, season_types = "Regular Season", resolve_records = T, nest_data = F, return_message = T)
get_seasons_standings <-
  function(seasons = 1950:2018,
           season_types = c("Regular Season"),
           resolve_records = TRUE,
           nest_data = F,
           return_message = TRUE) {
    input_df <-
      expand.grid(
        season = seasons,
        season_type = season_types,
        stringsAsFactors = F
      ) %>%
      dplyr::as_data_frame()

    get_season_standings_safe <-
      purrr::possibly(get_season_standings, data_frame())

    all_data <-
      1:nrow(input_df) %>%
      map_df(function(x) {
        input_df %>%
          slice(x) %$%
          get_season_standings_safe(
            season = season,
            season_type = season_type,
            return_message = return_message
          )
      })

    if (resolve_records) {
      record_names <-
        all_data %>% select(matches("record[A-Z]")) %>% names()

      all_data <- parse_records(data = all_data, record_names = record_names)
    }

    all_data <-
      all_data %>%
      left_join(get_nba_teams() %>% select(nameTeam, slugTeam)) %>%
      select(slugSeason,
             yearSeason,
             typeSeason,
             nameTeam,
             slugTeam,
             everything()) %>%
      suppressMessages()

    all_data <-
      all_data %>% mutate(urlLogoTeamSeason = generate_team_season_logo(season = yearSeason, slug_team = slugTeam))

    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-c(yearSeason, slugSeason, typeSeason))
    }

    all_data
  }
