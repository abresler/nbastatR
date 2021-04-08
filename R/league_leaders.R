
.get_season_metric_league_leaders <-
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
      stop(glue("Modes can only be {mode_slugs}"))
    }
    scope_slug <- "S"
    json_url <-
      glue("https://stats.nba.com/stats/leagueLeaders?LeagueID=00&PerMode={mode}&Scope={scope_slug}&Season={slug_season}&SeasonType={season_type}&StatCategory={metric}") %>%
      as.character() %>%
      URLencode()

    if (return_message) {
      glue("Acquiring {metric} {mode} league leaders in the {slug_season} season") %>% cat(fill = T)
    }

    json <-
      json_url %>%
      .curl_chinazi()

    actual_names <- json$resultSet$headers %>% resolve_nba_names()
    df_params <- json$parameters %>% flatten_df()
    actual_params <- names(df_params) %>% resolve_nba_names()
    df_params <-
      df_params %>%
      set_names(actual_params) %>%
      mutate(numberTable = 1)

    data <-
      json$resultSet$rowSet %>%
      data.frame(stringsAsFactors = F) %>%
      dplyr::as_tibble() %>%
      set_names(actual_names) %>%
      munge_nba_data() %>%
      mutate(numberTable = 1)

    data %>%
      left_join(df_params) %>%
      dplyr::select(one_of(actual_params), everything()) %>%
      select(-one_of("numberTable", "idLeague")) %>%
      suppressMessages()


  }

#' League leaders by season
#'
#' Gets league leader data by
#' specified input for specific seasons
#'
#' @param seasons vector of seasons
#' @param modes mode of search \itemize{
#' \item Totals
#' \item Per Game
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
#' @param nest_data if \code{TRUE} returns a nested data frame
#'
#' @return a \code{tibble}
#' @export
#' @import curl lubridate dplyr jsonlite stringr purrr tidyr readr
#' @importFrom glue glue
#' @family players
#' @family leaders
#' @examples
#' metrics_leaders(seasons = 2000:2005,
#' metric = "pts",
#' season_types = "Regular Season",
#'  modes = "PerGame",
#'  return_message = T)

metrics_leaders <-
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
      dplyr::as_tibble()
   .get_season_metric_league_leaders_safe <-
     possibly(.get_season_metric_league_leaders, tibble())

   all_data <-
     1:nrow(input_df) %>%
     future_map_dfr(function(x){
       df_row <- input_df %>% slice(x)
       df_row %$%
         .get_season_metric_league_leaders_safe(season = season,
                                          metric = metric,
                                          season_type = season_type,
                                          mode = mode,
                                          return_message = return_message)
     })

   if (nest_data) {
     all_data <-
       all_data %>%
       nest(-c(modeSearch, categoryStat, slugSeason, typeSeason, slugScope), .key = dataLeaders)
   }
   all_data
  }



# franchise ---------------------------------------------------------------

.get_franchise_leaders <-
  function(team_id = 1610612751,
           mode = "PerGame",
           season_type = "Regular Season",
           return_message = TRUE) {
    if (!'df_dict_nba_teams_history' %>% exists()) {
      df_dict_nba_teams_history <- nba_franchise_history()
      assign(x = 'df_dict_nba_teams_history', df_dict_nba_teams_history, envir = .GlobalEnv)
    }
    json_url <-
      glue(
      "https://stats.nba.com/stats/franchiseleaderswrank?LeagueID=00&PerMode={mode}&SeasonType={season_type}&TeamID={team_id}"
    ) %>%
      URLencode() %>%
      as.character()

    team_name <-
      df_dict_nba_teams_history %>%
      filter(idTeam == team_id) %>%
      pull(nameTeam) %>%
      unique() %>%
      str_c(collapse = ", ")


    if (return_message) {
      glue("Acquiring {team_name} {mode} franchise leaders") %>% cat(fill = T)
    }

    json <-
      json_url %>%
      .curl_chinazi()
    json_names <-   json$resultSet$headers[[1]]

    actual_names <-
    json_names %>% resolve_nba_names()

    df_params <-
      json$parameters %>% flatten_df()
    actual_params <- names(df_params) %>% resolve_nba_names()
    df_params <-
      df_params %>%
      set_names(actual_params) %>%
      mutate(numberTable = 1)

    data <-
      json$resultSet$rowSet[[1]] %>% dplyr::as_tibble() %>%
      set_names(actual_names) %>%
      munge_nba_data() %>%
      mutate(isActiveWithTeam = isActiveWithTeam %>% as.logical()) %>%
      mutate(numberTable = 1) %>%
      dplyr::rename(nameTeam = slugTeam)

    data %>%
      left_join(df_params %>% select(-one_of("idLeague", "idTeam"))) %>%
      dplyr::select(one_of(actual_params), everything()) %>%
      select(-one_of("numberTable", "idLeague")) %>%
      suppressMessages() %>%
      suppressWarnings()

}


#' Franchise leaders
#'
#' Gets franchise leader information for
#' by specified input for specific teams
#'
#' @param teams vector of team names
#' @param all_teams if \code{TRUE} returns all teams
#' @param remove_inactive_teams if \code{TRUE} removes inactive teams
#' @param modes mode of search \itemize{
#' \item Totals
#' \item Per Game
#' }
#'
#' @param season_types type of season \itemize{
#' \item Regular Season
#' \item Playoffs
#' \item Pre Season
#' }'
#' @return a \code{tibble}
#' @family teams
#' @family leaders
#' @export
#' @import curl lubridate dplyr jsonlite stringr purrr tidyr readr
#' @importFrom glue glue

#' @examples
#' franchise_leaders(teams = "Brooklyn Nets", modes = c("Totals"))

franchise_leaders <-
  function(teams = NULL,
           all_teams = FALSE,
           remove_inactive_teams = F,
           modes = "Totals",
           season_types = c("Regular Season"),
           return_message = TRUE,
           nest_data = FALSE) {

    if (teams %>% is_null() & !all_teams) {
      stop("Please enter a team or make all_teams = T")
    }
    if (!'df_dict_nba_teams_history' %>% exists()) {
      df_dict_nba_teams_history <- nba_franchise_history()
      assign(x = 'df_dict_nba_teams_history', df_dict_nba_teams_history, envir = .GlobalEnv)
    }

    ids <- c()

    if (remove_inactive_teams) {
      df_teams_filter <-
        df_dict_nba_teams_history %>%
        filter(isActive)
    } else {
      df_teams_filter <-
        df_dict_nba_teams_history
    }

    if (!teams %>% is_null()) {
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
      expand.grid(
        team_id = ids,
        mode = modes,
        season_type = season_types,
        stringsAsFactors = F
      ) %>%
      dplyr::as_tibble()
    .get_franchise_leaders_safe <-
      possibly(.get_franchise_leaders, tibble())

    all_data <-
      1:nrow(input_df) %>%
      future_map_dfr(function(x){
        df_row <- input_df %>% slice(x)
        df_row %$%
          .get_franchise_leaders_safe(team_id = team_id,
                                     mode = mode,
                                     season_type = season_type,
                                     return_message = return_message)
      }) %>%
      distinct()

    if (!'df_nba_player_dict' %>% exists()) {
      df_nba_player_dict <-
        nba_players()

      assign(x = 'df_nba_player_dict', df_nba_player_dict, envir = .GlobalEnv)
    }

    all_data <-
      all_data %>%
      left_join(
        df_nba_player_dict %>% select(idPlayer, urlPlayerHeadshot, urlPlayerThumbnail)
      ) %>%  suppressMessages()

    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-c(modeSearch, nameTeam, typeSeason, idTeam), .key = dataFranchiseLeaders)
    }
    all_data
  }
