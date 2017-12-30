
get_season_metric_league_leaders <-
  function(season = 2018,
           metric = "pts",
           season_type = "Regular Season",
         mode =  "Per48",
         return_message = TRUE) {
    slug_season <-
      season %>% generate_season_slug()

    modes <- c(
      "Totals",
      "PerGame",
      "Per48"

    )

    if (!mode %>% str_to_lower() %in% str_to_lower(modes)) {
      mode_slugs <- modes %>% str_c(collapse='\n')
      stop(glue::glue("Modes can only be {mode_slugs}"))
    }
    scope_slug <- "S"
    json_url <-
      glue::glue("http://stats.nba.com/stats/leagueLeaders?LeagueID=00&PerMode={mode}&Scope={scope_slug}&Season={slug_season}&SeasonType={season_type}&StatCategory={metric}") %>%
      as.character() %>%
      URLencode()

    if (return_message) {
      glue::glue("Acquiring {metric} {mode} league leaders in the {slug_season} season") %>% message()
    }

    json <-
      json_url %>%
      curl() %>%
      fromJSON(simplifyDataFrame = T)

    actual_names <- json$resultSet$headers %>% resolve_nba_names()
    df_params <- json$parameters %>% flatten_df()
    actual_params <- names(df_params) %>% resolve_nba_names()
    df_params <-
      df_params %>%
      purrr::set_names(actual_params) %>%
      mutate(numberTable = 1)

    data <-
      json$resultSet$rowSet %>% dplyr::as_data_frame() %>%
      purrr::set_names(actual_names) %>%
      munge_nba_data() %>%
      mutate(numberTable = 1)

    data %>%
      left_join(df_params) %>%
      dplyr::select(one_of(actual_params), everything()) %>%
      select(-one_of("numberTable", "idLeague")) %>%
      suppressMessages()


  }

#' Get League Leaders by Season by Type
#'
#' @param seasons vector of seasons
#' @param modes mode of search \itemize{
#' \item Totals
#' \item Per Gamee
#' \item Per48
#' }
#'
#' @param season_types type of season \itemize{
#' \item Regular Season
#' \item Playoffs
#' \item Pre Season
#' }
#' @param metric name of metric to sort on \itemize{
#' \item pts
#' \item min
#' \item ast
#' \item treb
#' \item oreb
#' \item dreb
#' \item stl
#' \item tov
#' \item fg3
#' \item fga
#' \item fgm
#' \item fta
#' \item ftm
#' \item pf
#' \item eff
#'
#' }
#' @param return_message if \code{TRUE} returns a message
#' @param nest_data if \code{TRUE} returns a nested data fram
#'
#' @return a \code{data_frame}
#' @export
#' @import curl lubridate dplyr jsonlite stringr purrr tidyr readr
#' @importFrom glue glue
#' @examples
#' get_seasons_metrics_league_leaders(seasons = 2000:2005, metric = "pts", season_types = "Regular Season", modes = "PerGame", return_message = T)

get_seasons_metrics_league_leaders <-
  function(seasons = 2017:2018,
           metric = "pts",
           season_types = "Regular Season",
           modes = c("PerGame", "Totals"),
           return_message = TRUE,
           nest_data = FALSE) {
   input_df <-
     expand.grid(season = seasons,
               metric = metric,
               season_type = season_types,
               mode = modes,
               stringsAsFactors = F) %>%
      dplyr::as_data_frame()
   get_season_metric_league_leaders_safe <-
     purrr::possibly(get_season_metric_league_leaders, data_frame())

   all_data <-
     1:nrow(input_df) %>%
     map_df(function(x){
       df_row <- input_df %>% slice(x)
       df_row %$%
         get_season_metric_league_leaders_safe(season = season,
                                          metric = metric,
                                          season_type = season_type,
                                          mode = mode,
                                          return_message = return_message)
     })

   if (nest_data) {
     all_data <-
       all_data %>%
       nest(-c(modeSearch, categoryStat, slugSeason, typeSeason, slugScope), .key = 'dataLeaders')
   }
   all_data
  }



# franchise ---------------------------------------------------------------

get_franchise_leaders <-
  function(team_id = 1610612751,
           mode = "PerGame",
           season_type = "Regular Season",
           return_message = TRUE) {
    json_url <-
      glue::glue(
      "https://stats.nba.com/stats/franchiseleaderswrank?LeagueID=00&PerMode={mode}&SeasonType={season_type}&TeamID={team_id}"
    ) %>%
      URLencode() %>%
      as.character()


    if (return_message) {
      glue::glue("Acquiring {team_id} {mode} franchise leaders") %>% message()
    }

    json <-
      json_url %>%
      curl() %>%
      fromJSON(simplifyDataFrame = T)
    json_names <-   json$resultSet$headers[[1]]

    actual_names <-
    json_names %>% resolve_nba_names()

    df_params <-
      json$parameters %>% flatten_df()
    actual_params <- names(df_params) %>% resolve_nba_names()
    df_params <-
      df_params %>%
      purrr::set_names(actual_params) %>%
      mutate(numberTable = 1)

    data <-
      json$resultSet$rowSet[[1]] %>% dplyr::as_data_frame() %>%
      purrr::set_names(actual_names) %>%
      munge_nba_data() %>%
      mutate(isActiveWithTeam = isActiveWithTeam %>% as.logical()) %>%
      mutate(numberTable = 1) %>%
      select(-one_of("idLeague")) %>%
      dplyr::rename(nameTeam = slugTeam)

    data %>%
      left_join(df_params %>% select(-one_of("idLeague", "idTeam"))) %>%
      dplyr::select(one_of(actual_params), everything()) %>%
      select(-one_of("numberTable", "idLeague")) %>%
      suppressMessages() %>%
      suppressWarnings()

}


#' Get NBA teams' franchise leaders
#'
#' @param teams vector of team names
#' @param all_teams if \code{TRUE} returns all teams
#' @param remove_inactive_teams if \code{TRUE} removes inactive teams
#' @param modes mode of search \itemize{
#' \item Totals
#' \item Per Gamee
#' }
#'
#' @param season_types type of season \itemize{
#' \item Regular Season
#' \item Playoffs
#' \item Pre Season
#' }'
#' @return a \code{data_frame}
#' @export
#' @import curl lubridate dplyr jsonlite stringr purrr tidyr readr
#' @importFrom glue glue

#' @examples
#' get_teams_franchise_leaders(teams = "Brooklyn Nets", modes = c("Totals", "PerGame"))

get_teams_franchise_leaders <-
  function(teams = NULL,
           all_teams = FALSE,
           remove_inactive_teams = F,
           modes = c("PerGame", "Totals"),
           season_types = c("Regular Season"),
           return_message = TRUE,
           nest_data = FALSE) {

    if (teams %>% purrr::is_null() & !all_teams) {
      stop("Please enter a team or make all_teams = T")
    }
    if (!'df_dict_nba_teams' %>% exists()) {
      df_dict_nba_teams <- get_nba_franchise_history()
      assign(x = 'df_dict_nba_teams', df_dict_nba_teams, envir = .GlobalEnv)
    }

    ids <- c()

    if (remove_inactive_teams) {
      df_teams_filter <- df_dict_nba_teams %>% filter(isActive)
    } else {
      df_teams_filter <- df_dict_nba_teams
    }

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

    input_df <-
      expand.grid(team_id = ids,
                  modes = modes,
                  season_type = season_types,
                  stringsAsFactors = F) %>%
      dplyr::as_data_frame()
    get_franchise_leaders_safe <-
      purrr::possibly(get_franchise_leaders, data_frame())

    all_data <-
      1:nrow(input_df) %>%
      map_df(function(x){
        df_row <- input_df %>% slice(x)
        df_row %$%
          get_franchise_leaders_safe(team_id = team_id,
                                     mode = mode,
                                     season_type = season_type,
                                     return_message = return_message)
      })

    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-c(modeSearch, nameTeam, typeSeason, idTeam), .key = 'dataFranchiseLeaders')
    }
    all_data
  }
