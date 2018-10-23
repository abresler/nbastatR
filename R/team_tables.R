# http://nbasense.com/nba-api/Stats/Stats/Teams/TeamsYearByYearStats#request-example

get_team_details <- function(team_id = 1610612745, return_message = TRUE) {
  url <-
    glue::glue("http://stats.nba.com/stats/teamdetails/?teamId={team_id}")
  if (!'df_dict_nba_teams_history' %>% exists()) {
    df_dict_nba_teams_history <- get_nba_franchise_history()
    assign(x = 'df_dict_nba_teams_history', df_dict_nba_teams_history, envir = .GlobalEnv)
  }

  team_name <-
    df_dict_nba_teams_history %>%
    filter(idTeam == team_id) %>%
    filter(yearEnd == max(yearEnd)) %>%
    pull(nameTeam) %>%
    unique() %>%
    .[[1]]

  if (return_message) {
    glue::glue("Acquiring team details for the {team_name}") %>% cat(fill = T)
  }

  json <-
    curl_json_to_vector(url = url)
  tables_data <- json$resultSets
  tables <- json$resultSets$rowSet %>% length()

  data <-
    1:tables %>%
    future_map_dfr(function(x){
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

      data <-
        data %>%
        munge_nba_data() %>%
        mutate(idTeam = team_id,
               nameTeam = team_name) %>%
        mutate(nameTable = table_name) %>%
        remove_na_columns()

      data %>%
        nest(-c(nameTable, nameTeam, idTeam), .key = dataTable)
    })
  data
}

#' NBA teams details
#'
#' Gets information about NBA franchises
#'
#' @param teams vector of team names
#' @param team_ids vector of team ids
#' @param all_teams if \code{TRUE} returns all team names
#' @param assign_to_environment if \code{TRUE} assigns each table to a data frame in
#' your environment starting with data
#' @param return_message if \code{TRUE} returns a message
#'
#' @return a `data_frame`
#' @export
#' @import dplyr stringr curl jsonlite lubridate purrr tidyr rlang readr tibble
#' @importFrom glue glue
#' @examples
#' get_teams_details(all_teams = TRUE, assign_to_environment = TRUE)
#'

get_teams_details <-
  function(teams = NULL,
           team_ids = NULL,
           all_teams = F,
           assign_to_environment = TRUE,
           return_message = T) {
    no_ids <- as.numeric( (team_ids %>% purrr::is_null() & !all_teams))
    no_teams <-  as.numeric((teams %>% purrr::is_null() & !all_teams))
    if (no_ids + no_teams == 2) {
      stop("Please enter a team or make all_teams = T")
    }
    if (!'df_dict_nba_teams_history' %>% exists()) {
      df_dict_nba_teams_history <- get_nba_franchise_history()
      assign(x = 'df_dict_nba_teams_history', df_dict_nba_teams_history, envir = .GlobalEnv)
    }

    ids <- c()

    df_teams_filter <- df_dict_nba_teams_history %>% filter(isActive)

    if (!teams %>% purrr::is_null()) {
      team_slugs <- teams %>% str_to_upper() %>% str_c(collapse = "|")

      search_ids <-
        df_teams_filter %>%
        mutate(nameUpper = nameTeam %>% str_to_upper()) %>%
        filter(nameUpper %>% str_detect(team_slugs)) %>%
        pull(idTeam) %>%
        unique()

      ids <-
        ids %>% append(search_ids)

    }

    if (all_teams) {
      all_ids <-
        df_teams_filter %>%
        filter(isActive) %>%
        distinct(nameTeam, idTeam) %>%
        pull(idTeam) %>%
        unique()

      ids <-
        ids %>%
        append(all_ids)
    }

    if (!team_ids %>% purrr::is_null()) {
      ids <-
        ids %>%
        append(team_ids)
    }

    ids <-
      ids %>%
      unique() %>%
      sort()
    get_team_details_safe <-
      purrr::possibly(get_team_details, data_frame())

    all_data <-
      ids %>%
      future_map_dfr(function(id) {
        get_team_details(team_id = id, return_message = return_message)
      })

    tables <- all_data$nameTable %>% unique()

    all_data <-
      tables %>%
      future_map_dfr(function(table){
        df_row <-
          all_data %>%
          filter(nameTable == table) %>%
          unnest() %>%
          distinct()

        if (df_row %>% tibble::has_name("capacityArena")) {
          df_row <-
            df_row %>%
            mutate(capacityArena = capacityArena %>% as.numeric())
        }

        if (table %in% c("TeamHof", "TeamRetired")) {
         df_row <-
           df_row %>%
            dplyr::rename(yearNBASeasonFinal =  slugSeason) %>%
            mutate(yearNBASeasonFinal = yearNBASeasonFinal %>% as.numeric())

        }

        if (table == "TeamAwardsChampionships") {
          df_row <-
            df_row %>%
            mutate(isNBAChampion = TRUE) %>%
            arrange(yearSeason)
        }

        if (table == "TeamAwardsDiv") {
          df_row <-
            df_row %>%
            mutate(isDivisionChampion = TRUE) %>%
            arrange(yearSeason)
        }

        if (table == "TeamAwardsConf") {
          df_row <-
            df_row %>%
            mutate(isConferenceChampion = TRUE) %>%
            arrange(yearSeason)
        }


        df_row %>%
          nest(-c(nameTable), .key = dataTable)
      })

    if (assign_to_environment) {
      tables %>%
        walk(function(table){
          table_name <-
            glue::glue('data{table}') %>% as.character()

          df_table <-
            all_data %>%
            filter(nameTable == table) %>%
            select(-nameTable) %>%
            unnest() %>%
            distinct()
          assign(table_name, df_table, envir = .GlobalEnv)
        })

    }

    all_data

  }

get_team_year_by_year_stats <-
  function(team_id = 1610612751,
         season_type = "Regular Season",
         mode = "Totals",
         return_message = T) {
  query_slug <- "teamyearbyyearstats"
  season_slug <- season_type %>% clean_to_stem()
  json_url <-
    glue::glue(
      "http://stats.nba.com/stats/{query_slug}/?teamId={team_id}&leagueId=00&seasonType={season_slug}&perMode={mode}"
    ) %>% as.character() %>% URLencode()
  json <-
    json_url %>%
    curl_json_to_vector()

  data <-
    json$resultSets$rowSet[[1]] %>%
    dplyr::as_data_frame()

  actual_names <-
    json$resultSets$headers[[1]] %>%
    resolve_nba_names()

  data <-
    data %>%
    purrr::set_names(actual_names) %>%
    tidyr::unite(nameTeam, cityTeam, teamName, sep = " ", remove = F)

  num_cols <- names(data)[!names(data) %>% str_detect(char_words())]

  teams <- data$nameTeam %>% unique() %>% str_c(collapse = ", ")

  if (return_message) {
    glue::glue("Acquired {teams} history") %>% cat(fill = T)
  }

  data <-
    data %>%
    mutate_at(num_cols,
              funs(. %>% as.numeric())) %>%
    mutate(modeSearch = mode) %>%
    dplyr::select(modeSearch, everything())

  data

}

#' NBA teams yearly performance
#'
#' @param teams vector of team names
#' @param team_ids vector of team ids
#' @param all_active_teams if \code{TRUE} returns all active teams
#' @param season_types type of season options include \itemize{
#' \item Pre Season
#' \item Regular Season
#' \item Playoffs
#' \item All Star
#' }
#' @param modes mode of search options include \itemize{
#' \item PerGame
#' \item Totals
#' }
#' @param return_message if \code{TRUE} returns a message
#' @param nest_data if \code{TRUE} returns a nested data_frame
#'
#' @return a \code{data_frame}
#' @export
#'
#' @examples
#' get_teams_year_by_year_stats(all_active_teams = T,
#' modes = c("Totals"),
#' return_message = TRUE,
#'  nest_data =F)
get_teams_year_by_year_stats <-
  function(teams = NULL,
           team_ids = NULL,
           all_active_teams = T,
           season_types = c("Regular Season"),
           modes = c("Totals"),
           return_message = TRUE,
           nest_data = F) {
    assign_nba_teams()
    team_ids <-
      get_nba_teams_ids(teams = teams,
                        team_ids = team_ids,
                        all_active_teams = all_active_teams)

    get_team_year_by_year_stats_safe <-
      purrr::possibly(get_team_year_by_year_stats, data_frame())
    df_input <-
      expand.grid(
        team_id = team_ids,
        season_type =  season_types,
        mode = modes,
        stringsAsFactors = F
      ) %>%
      as_data_frame()

    all_data <-
      1:nrow(df_input) %>%
      future_map_dfr(function(x) {
        df_row <- df_input %>% slice(x)

        df_row %$%
          get_team_year_by_year_stats_safe(
            team_id = team_id,
            season_type = season_type,
            mode = mode,
            return_message = return_message
          )
      })

    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-c(slugSeason), .key = dataTeamSeasonPerformance)
    }


    all_data <-
      all_data %>%
      mutate(
        descriptionNBAFinalsAppearance = case_when(
          descriptionNBAFinalsAppearance == "LEAGUE CHAMPION" ~ "NBA Champion",
          descriptionNBAFinalsAppearance == "FINALS APPEARANCE " ~ "Runner Up",
          descriptionNBAFinalsAppearance == "N/A" ~ NA_character_
        ),
        isConferenceChampion = !descriptionNBAFinalsAppearance %>% is.na(),
        isNBAChampion = descriptionNBAFinalsAppearance == "NBA Champion"
      )

    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-idTeam, .key = dataTeamYearlyStats)
    }

    all_data
  }
