# seasons_players ---------------------------------------------------------
.get_seasons_teams <-
  function(season = 2018,
           return_message = TRUE,
           ...) {
    season_slug <-
      generate_season_slug(season = season)

    table_id <- 1
    URL <- gen_url("commonteamyears")
    params <- list(
      LeagueID = "00",
      SeasonType = "",
      Season = season_slug,
      IsOnlyCurrentSeason = "0",
      PlayerID = "",
      TeamID = "",
      GameID = "",
      ContextMeasure = "",
      PlayerPosition = "",
      DateFrom = "",
      DateTo = "",
      GameSegment = "",
      LastNGames = "",
      Location = "",
      Month = "",
      OpponentTeamID = "",
      Outcome = "",
      SeasonSegment = "",
      VSConference = "",
      VSDivision = "",
      RookieYear = "",
      Period = "",
      StartPeriod = "",
      EndPeriod = ""
    )
    params <- utils::modifyList(params, list(...))

    slug_param <-
      .generate_param_slug(params = params)

    url <-
      glue::glue("{URL}?{slug_param}") %>% as.character()

    resp <-
      url %>%
      curl() %>%
      readr::read_lines()

    json <-
      resp %>% jsonlite::fromJSON(simplifyVector = T)

    data <-
      json %>%
      nba_json_to_df(table_id = table_id) %>%
      mutate(isActive = ifelse(slugTeam %>% is.na(), F, T)) %>%
      select(isActive, slugTeam, idTeam, everything()) %>%
      arrange(slugTeam) %>%
      suppressWarnings()
    data
  }


.get_team_id_season_roster <-
  function(season = 2018, team_id = 1610612751) {
    if (season < 1947) {
      stop("NBA data starts for the 1946-47 season")
    }
    season_slug <-
      generate_season_slug(season = season)

    table_id <- 1
    URL <- gen_url("commonteamroster")
    params <- list(
      LeagueID = "00",
      SeasonType = "",
      Season = season_slug,
      IsOnlyCurrentSeason = "0",
      PlayerID = "",
      TeamID = team_id,
      GameID = "",
      ContextMeasure = "",
      PlayerPosition = "",
      DateFrom = "",
      DateTo = "",
      GameSegment = "",
      LastNGames = "",
      Location = "",
      Month = "",
      OpponentTeamID = "",
      Outcome = "",
      SeasonSegment = "",
      VSConference = "",
      VSDivision = "",
      RookieYear = "",
      Period = "",
      StartPeriod = "",
      EndPeriod = ""
    )
    # params <- utils::modifyList(params, list(...))

    slug_param <-
      .generate_param_slug(params = params)

    url <-
      glue::glue("{URL}?{slug_param}") %>% as.character()

    resp <-
      url %>%
      curl() %>%
      readr::read_lines()

    json <-
      resp %>% jsonlite::fromJSON(simplifyVector = T)

    if (json$resultSets$rowSet[[1]] %>% length() == 0) {
      return(invisible())
    }

    data <-
      json %>%
      nba_json_to_df(table_id = table_id) %>%
      mutate(slugSeason = season_slug)

    if (data %>% tibble::has_name("dateBirth")) {
      data <-
        data %>%
        mutate(dateBirth = dateBirth %>% lubridate::mdy())
    }

    if (data %>% tibble::has_name("countYearsExperience")) {
    data <-
      data %>%
      mutate(
        countYearsExperience = ifelse(is.na(countYearsExperience), 0, countYearsExperience),
      )
    }

    if (data %>% tibble::has_name("heightInches")) {
      data <-
        data %>%
        tidyr::separate(heightInches,
                        sep = "\\-",
                        into = c("feet", "inches")) %>%
        mutate_at(c("feet", "inches"),
                  funs(. %>% as.numeric())) %>%
        mutate(heightInches = (12 * feet) + inches) %>%
        select(-one_of(c("feet", "inches"))) %>%
        select(slugSeason, idTeam:weightLBS, heightInches, everything())
    }

    data
  }


.get_season_roster <-
  function(season = 2012,
           return_message = TRUE) {

    df_teams <-
      .get_seasons_teams() %>%
      filter(isActive) %>%
      select(idTeam, slugTeam)

    team_ids <- df_teams$idTeam

    df_rosters <-
     team_ids  %>%
      future_map_dfr(function(team_id) {
        if (return_message) {
          team_slug <- df_teams %>% filter(idTeam == team_id) %>% pull(slugTeam)
          glue::glue("Acquiring {team_slug}'s team roster for the {season} season") %>% cat(fill = T)
        }
        .get_team_id_season_roster(season = season, team_id = team_id)
      })

    df_rosters <-
      df_rosters %>%
      left_join(df_teams) %>%
      mutate(yearSeason = season) %>%
      dplyr::select(yearSeason,
                    slugSeason,
                    slugTeam,
                    idPlayer,
                    namePlayer,
                    everything()) %>%
      mutate(urlTeamSeasonLogo = generate_team_season_logo(season = yearSeason, slug_team = slugTeam)) %>%
      suppressMessages()
    df_rosters
  }


#' NBA teams seasons rosters
#'
#' Rosters for each team of specified seasons
#'
#' @param seasons vector of seasons
#' @param return_message if \code{TRUE} returns a message
#' @param nest_data if \code{TRUE} nests data
#'
#' @return a `date_frame`
#' @family teams
#' @family rosters
#' @export
#' @import dplyr jsonlite purrr stringr lubridate magrittr tidyr tibble httr
#' @importFrom glue glue
#' @examples
#' library(nbastatR)
#' library(dplyr)
#' df_rosters <- seasons_rosters(2015:2018)
#'
#' ### Mean Age by Season and Team
#' df_rosters %>%
#' group_by(slugSeason, slugTeam) %>%
#' summarise(ageMean = mean(agePlayer)) %>%
#' arrange(ageMean) %>%
#' ungroup()
seasons_rosters <-
  function(seasons = NULL,
           return_message = TRUE,
           nest_data = F) {
    if (seasons %>% purrr::is_null()) {
      stop("Enter seasons")
    }
    .get_season_roster_safe <-
      purrr::possibly(.get_season_roster, data_frame())
    all_data <-
      seasons %>%
      future_map_dfr(function(season) {
        .get_season_roster_safe(season = season, return_message = return_message)
      })

    df_dict_nba_players <- nba_players()

    all_data <-
      all_data %>%
      left_join(df_dict_nba_players %>% select(idPlayer, dplyr::matches("url"))) %>%
      suppressMessages()

    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-c(slugSeason), .key = dataRosters)
    }
    all_data
  }
