#' Get NBA franchise history
#'
#' Acquires history for all NBA franchises
#'
#' @param return_message if \code{TRUE} returns message
#' @param only_active if \code{TRUE} only active teams
#'
#' @return a \code{tibble()}
#' @export
#' @import dplyr stringr curl purrr jsonlite
#' @examples
#' nba_franchise_history()
nba_franchise_history <-
  function(return_message = T,
           only_active = F) {
    team_history_url <-
      'https://stats.nba.com/stats/franchisehistory?LeagueID=00'

    team_data <-
      team_history_url %>%
      .curl_chinazi()

    names_active <-
      team_data$resultSets$headers[1] %>%
      unlist() %>%
      str_to_lower()

    names_defunct <-
      team_data$resultSets$headers[2] %>%
      unlist() %>%
      str_to_lower()

    active_data <-
      team_data$resultSets$rowSet[1] %>%
      data.frame(stringsAsFactors = F) %>%
      as_tibble()


    names(active_data) <-
      names_active

    active_data <-
      active_data %>%
      mutate(isActive = T)

    defunct_data <-
      team_data$resultSets$rowSet[2] %>%
      data.frame(stringsAsFactors = F) %>%
      as_tibble()

    names(defunct_data) <-
      names_defunct

    defunct_data <-
      defunct_data %>%
      mutate(isActive = F)

    data <-
      active_data %>%
      bind_rows(defunct_data)

    num_cols <-
      data %>%
      dplyr::select(-isActive) %>%
      dplyr::select(-dplyr::matches("team")) %>%
      names()

    data <-
      data %>%
      mutate_at(.vars = num_cols,
                .funs = as.numeric)

    names(data) <-
      c(
        "idLeague",
        "idTeam",
        "cityTeam",
        "teamName",
        "yearStart",
        "yearEnd",
        "countTeamSeasons",
        "countTeamGames",
        "countTeamWins",
        "countTeamLosses",
        "pctWins",
        "countPlayoffQualified",
        "countDivisionTitles",
        "countConferenceTitles",
        "countLeagueTitles",
        "isActive"
      )

    data <-
      data %>%
      mutate(idTeam = idTeam %>% as.numeric()) %>%
      unite(nameTeam, cityTeam, teamName, sep = " ", remove = F) %>%
      dplyr::select(idLeague, idTeam, nameTeam, everything()) %>%
      mutate(nameTeam = nameTeam %>% str_replace_all("LA Clippers", "Los Angeles Clippers"))

    if (only_active) {
      current_year <- data$yearEnd %>% max(na.rm = T)
      data <-
        data %>%
        filter(isActive) %>%
        filter(yearEnd == current_year) %>%
        group_by(idTeam) %>%
        filter(yearStart == min(yearStart)) %>%
        mutate(idRow = 1:n()) %>%
        filter(idRow == min(idRow)) %>%
        ungroup()
    }


    if (return_message) {
      "You got NBA franchise data" %>%
        cat(fill = T)
    }
   data
  }


#' Team roster
#'
#' Gets teams roster from a specified seasons
#'
#' @param team team name
#' @param season season vector
#' @param return_message
#'
#' @return a `tibble`
#' @family rosters
#' @export
#' @import dplyr purrr stringr readr tidyr jsonlite curl lubridate
#' @importFrom glue glue
#' @examples
#' team_season_roster(team = "Denver Nuggets", season = 1991)

team_season_roster <-
  function(team = "Denver Nuggets",
           season = 2015,
           return_message = T) {
    if (!'team' %>% exists()) {
      stop("Please enter a team")
    }

    season_start <-
      season - 1

    slugSeason <-
      season_start %>%
      paste(season %>% substr(start = 3, stop = 4),
            sep = "-")
    if (!'df_dict_team_history' %>% exists()) {
      df_dict_team_history <-
        nba_franchise_history(only_active = T)
      assign('df_dict_team_history', df_dict_team_history, envir = .GlobalEnv)
    }

    team_id <-
      df_dict_team_history %>%
      filter(nameTeam %>% str_detect(team)) %>%
      pull(idTeam) %>%
      unique()

    json_url <-
      glue('https://stats.nba.com/stats/commonteamroster?LeagueID=00&Season={slugSeason}&TeamID={team_id}') %>% as.character()


    json_data <-
      json_url %>%
      .curl_chinazi()

    names_roster <-
      json_data$resultSets$headers[1] %>%
      unlist() %>%
      str_to_lower()

    data_roster <-
      json_data$resultSets$rowSet[1] %>%
      data.frame(stringsAsFactors = F) %>%
      tbl_df()
    actual_names <- c(
      "idTeam",
      "yearSeason",
      "idLeague",
      "namePlayer",
      "nickname",
      "playerSlug",
      "numberJerseySeason",
      "groupPosition",
      "heightInches",
      "weightLBS",
      "dateBirth",
      "agePlayerSeason",
      "countSeasons",
      "nameSchool",
      "idPlayer",
      "howAcquired"
    )[seq_along(names(data_roster))]

     data_roster <-
      data_roster %>%
      set_names(
        actual_names
      ) %>%
      mutate(slugSeason,
             nameTeam = team) %>%
      dplyr::select(slugSeason, yearSeason, nameTeam, everything())


    data_roster <-
      data_roster %>%
      mutate_at(
        c(
          "idTeam",
          "yearSeason",
          "idLeague",
          "weightLBS",
          "agePlayerSeason",
          "countSeasons",
          "idPlayer"
        ),
        funs(. %>% as.character() %>% parse_number())
      ) %>%
      mutate(
        dateBirth = lubridate::mdy(dateBirth),
        heightInches = heightInches %>% map_dbl(height_in_inches),
        countSeasons = ifelse(countSeasons %>% is.na(), 0, countSeasons),
        isRookie = ifelse(countSeasons == 0, TRUE, FALSE)
      ) %>%
      dplyr::select(-one_of(c("idLeague"))) %>%
      suppressMessages() %>%
      suppressWarnings()

      if (return_message) {
        glue("You got the {team}'s roster for the {slugSeason}") %>% cat(fill = T)
      }
    data_roster
  }


.season_teams_rosters <-
  function(season = 1994,
           return_message = T) {
    if (!'df_dict_team_history' %>% exists()) {
      df_dict_team_history <-
        nba_franchise_history(only_active = T)
      assign('df_dict_team_history', df_dict_team_history, envir = .GlobalEnv)
    }
    team_season_roster_safe <-
      possibly(team_season_roster, tibble())

    all_data <-
      df_dict_team_history$nameTeam %>%
      future_map_dfr(function(team) {
        team_season_roster_safe(team = team,
                                        season = season,
                                        return_message = return_message)
      })

    all_data <-
      all_data %>%
      mutate(nameTeam = nameTeam %>% str_replace_all("LA Clippers", "Los Angeles Clippers"))

    all_data

  }

#' Teams seasons rosters
#'
#' Gets NBA teams rosters for specified seasons
#'
#' @param seasons vector of seasons
#' @param nest_data if \code{TRUE} returns nested data frame
#' @param return_message if \code{TRUE} returns a message
#'
#' @return a \code{tibble}
#' @family rosters
#' @export
#' @import dplyr purrr stringr readr tidyr jsonlite curl lubridate
#' @importFrom glue glue
#' @examples
#' teams_rosters(seasons = 2010:2018, nest_data = F, return_message = T)
teams_rosters <-
  function(seasons = 1990:2018,
           nest_data = F,
           return_message = T) {
    all_data <-
      seasons %>%
      future_map_dfr(function(season) {
        .season_teams_rosters(season = season)
      })

    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-c(slugSeason, nameTeam, idTeam), .key = 'dataRoster') %>%
        mutate(countPlayers = dataRoster %>% map_dbl(nrow))
    }

    all_data
  }

.team_coaches <-
  function(team = "Denver Nuggets",
           season = 2015,
           return_message = T) {
    if (!'team' %>% exists()) {
      stop("Please enter a team")
    }

    season_start <-
      season - 1

    slugSeason <-
      season_start %>%
      paste(season %>% substr(start = 3, stop = 4),
            sep = "-")
    if (!'df_dict_team_history' %>% exists()) {
      df_dict_team_history <-
        nba_franchise_history(only_active = T)
      assign('df_dict_team_history', df_dict_team_history, envir = .GlobalEnv)
    }

    team_id <-
      df_dict_team_history %>%
      filter(nameTeam %>% str_detect(team)) %>%
      pull(idTeam) %>%
      unique()

    json_url <-
      glue('https://stats.nba.com/stats/commonteamroster?LeagueID=00&Season={slugSeason}&TeamID={team_id}') %>% as.character()


    json_data <-
      json_url %>%
      .curl_chinazi()

    names_roster <-
      json_data$resultSets$headers[2] %>%
      unlist() %>%
      str_to_lower()

    data_roster <-
      json_data$resultSets$rowSet[2] %>%
      data.frame(stringsAsFactors = F) %>%
      tbl_df()

    if (data_roster %>% nrow() == 0) {
      return(invisible())
    }

    actual_names <- c(
      c("idTeam", "yearSeason", "idCoach", "nameFirst", "nameLast",
        "nameCoach", "slugCoach", "numberCoachType", "typeCoach", "schoolCoach",
        "remove")

    )[seq_along(names(data_roster))]

    data_roster <-
      data_roster %>%
      set_names(
        actual_names
      ) %>%
      mutate(slugSeason,
             nameTeam = team) %>%
      dplyr::select(slugSeason, yearSeason, nameTeam, everything()) %>%
      select(-one_of(c("nameFirst", "nameLast", "remove"))) %>%
      suppressWarnings()


    data_roster <-
      data_roster %>%
      mutate_at(
        c(
          "idTeam",
          "yearSeason"
        ),
        funs(. %>% as.character() %>%  parse_number())
      ) %>%
      separate(schoolCoach, into = c("typeSchoolCoach", "nameSchoolCoach"), sep = "\\ - ") %>%
      suppressMessages() %>%
      suppressWarnings()

    if (return_message) {
      glue("You got the {team}'s coaches for the {slugSeason} season") %>% cat(fill = T)
    }
    data_roster
  }

.season_teams_coaches <-
  function(season = 2010,
           return_message = T) {
    if (!'df_dict_team_history' %>% exists()) {
      df_dict_team_history <-
        nba_franchise_history(only_active = T)
      assign('df_dict_team_history', df_dict_team_history, envir = .GlobalEnv)
    }
    .team_coaches_safe <-
      possibly(.team_coaches, tibble())

    all_data <-
      df_dict_team_history$nameTeam %>%
      future_map_dfr(function(team) {
        .team_coaches_safe(team = team,
                                        season = season,
                                        return_message = return_message)
      })

    all_data <-
      all_data %>%
      mutate(nameTeam = nameTeam %>% str_replace_all("LA Clippers", "Los Angeles Clippers"))

    all_data

  }


#' Seasons coaching staffs
#'
#' Gets coaching staffs for specified seasons
#'
#' @param seasons vector of seasons
#' @param nest_data if \code{TRUE} nests data
#' @param return_message if \code{TRUE} returns a message
#'
#' @return a `tibble`
#' @export
#' @import dplyr purrr stringr readr tidyr jsonlite curl lubridate
#' @importFrom glue glue
#' @examples
#' teams_coaches(2018:2019)

teams_coaches <-
  function(seasons = 1990:2018,
           nest_data = F,
           return_message = T) {
    all_data <-
      seasons %>%
      future_map_dfr(function(season) {
        .season_teams_coaches(season = season)
      })

    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-c(slugSeason, nameTeam, idTeam), .key = 'dataCoaches') %>%
        mutate(countCoaches = dataRoster %>% map_dbl(nrow))
    }

    all_data
  }
