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
    glue::glue("Acquiring team details for the {team_name}") %>% message()
  }

  json <-
    curl_json_to_vector(url = url)
  tables_data <- json$resultSets
  tables <- json$resultSets$rowSet %>% length()

  data <-
    1:tables %>%
    map_df(function(x){
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
        nest(-c(nameTable, nameTeam, idTeam), .key = 'dataTable')
    })
  data
}

#' Get NBA Teams details
#'
#' Gets information about NBA franchises
#'
#' @param teams vector of team names
#' @param team_ids vector of team ids
#' @param all_teams if \code{TRUE} retuns all team names
#' @param assign_to_environment if \code{TRUE} assigns each table to a data frame in
#' your environment starting with data
#' @param return_message if \code{TRUE} returns a message
#'
#' @return
#' @export
#' @import dplyr stringr curl jsonlite lubridate purrr tidyr rlang readr tibble
#' @importFrom glue glue
#' @examples
#' get_teams_details(all_teams = TRUE, assign_to_environment = TRUE)
get_teams_details <-
  function(teams = NULL, team_ids = NULL,
           all_teams = T,
           assign_to_environment = TRUE,
           return_message = T) {

    if (teams %>% purrr::is_null() & !all_teams) {
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

    ids <-
      ids %>%
      unique() %>%
      sort()
    get_team_details_safe <-
      purrr::possibly(get_team_details, data_frame())

    all_data <-
      ids %>%
      map_df(function(id) {
        get_team_details_safe(team_id = id, return_message = return_message)
      })

    tables <- all_data$nameTable %>% unique()

    all_data <-
      tables %>%
      map_df(function(table){
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
          nest(-c(nameTable), .key = 'dataTable')
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
         mode = "Totals") {
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

  data <-
    data %>%
    mutate_at(num_cols,
              funs(. %>% as.numeric()))

  data

}
