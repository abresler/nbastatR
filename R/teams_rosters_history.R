function_packages <-
  c('dplyr',
    'magrittr',
    'jsonlite',
    'tidyr',
    'stringr',
    'lubridate',
    'stringr',
    'tidyr')

install_needed_packages <-
  function(required_packages = function_packages) {
    needed_packages <-
      required_packages[!(required_packages %in% installed.packages()[, "Package"])]

    if (length(needed_packages) > 0) {
      if (!require("pacman"))
        install.packages("pacman")
      pacman::p_load(needed_packages)
    }
  }

load_needed_packages <-
  function(required_packages = function_packages) {
    loaded_packages <-
      gsub('package:', '', search())

    package_to_load <-
      required_packages[!required_packages %in% loaded_packages]
    lapply(package_to_load, library, character.only = T)
  }
install_needed_packages(function_packages)
load_needed_packages(function_packages)

height_in_inches <-
  function(height) {
    height_ft_in <-
      height %>%
      str_split("-") %>%
      unlist %>%
      as.numeric()
    height_in <-
      height_ft_in[1] * 12 + height_ft_in[2]
    return(height_in)
  }

get_nba_franchise_data <-
  function(return_franchises = c('all', 'active', 'current'),
           return_message = T) {
    team_history_url <-
      'http://stats.nba.com/stats/franchisehistory?LeagueID=00'

    team_data <-
      team_history_url %>%
      fromJSON(simplifyDataFrame = T, flatten = T)

    names_active <-
      team_data$resultSets$headers[1] %>%
      unlist %>%
      str_to_lower

    names_defunct <-
      team_data$resultSets$headers[2] %>%
      unlist %>%
      str_to_lower

    active_data <-
      team_data$resultSets$rowSet[1] %>%
      data.frame %>%
      tbl_df()

    names(active_data) <-
      names_active

    active_data %<>%
      mutate(is.active = T)

    defunct_data <-
      team_data$resultSets$rowSet[2] %>%
      data.frame %>%
      tbl_df()

    names(defunct_data) <-
      names_defunct

    defunct_data %<>%
      mutate(is.active = F)

    data <-
      active_data %>%
      bind_rows(defunct_data)

    num_cols <-
      data %>%
      dplyr::select(-c(contains("team")), -is.active) %>%
      names

    data %<>%
      mutate_each_(funs(as.numeric), vars = num_cols)

    names(data) <-
      c(
        "id.league",
        "id.team",
        "city.team",
        "name.team",
        "year.start.team",
        "year.end.team",
        "team.seasons",
        "team.games",
        "team.wins",
        "team.losses",
        "pct.wins",
        "team.po_appearances",
        "team.div_titles",
        "team.conf_titles",
        "team.league_titles",
        "is.active"
      )

    data %<>%
      mutate(team = city.team %>% paste(name.team),
             id.team = id.team %>% as.numeric()) %>%
      dplyr::select(-id.league) %>%
      dplyr::select(id.team, team, everything())

    if (return_franchises == 'current') {
      data %<>%
        mutate(id.row = 1:nrow(.)) %>%
        group_by(id.team) %>%
        dplyr::filter(id.row == min(id.row), is.active == T) %>%
        dplyr::select(-id.row)
    }

    if (return_franchises == 'active') {
      data %<>%
        dplyr::filter(is.active == T)
    }
    if (return_message == T) {
      "You got NBA franchise data" %>%
        message
    }
    return(data)
  }


get_nba_team_season_roster <- function(team = "Denver Nuggets",
                                       year_season_end = 1992,
                                       return_message = T) {
  if (!'team' %>% exists) {
    stop("Please enter a team")
  }

  year_season_start <-
    year_season_end - 1

  id.season <-
    year_season_start %>%
    paste(year_season_end %>% substr(start = 3, stop = 4),
          sep = "-")

  teams <-
    get_nba_franchise_data(return_franchises = 'all', return_message = F)

  teams_ids <-
    teams %>%
    dplyr::select(id.team, city.team, name.team, team, year.start.team)

  t <-
    team

  team_id <-
    teams_ids %>%
    dplyr::filter(team == t) %>%
    .$id.team %>%
    unique()

  if (team_id %>% length > 1) {
    team_id <-
      team_id[1]
  }

  if (year_season_end - 1 <
      teams %>%
      dplyr::filter(id.team == team_id) %>%
      dplyr::filter(year.start.team == min(year.start.team)) %>%
      .$year.start.team %>% unique()) {
    "Sorry " %>%
      paste0(
        year_season_end,
        ' is not a valid season for the ',
        teams_ids %>%
          dplyr::filter(id.team == team_id) %>%
          .$team
      ) %>%
      message()

  }

  roster_url <-
    'http://stats.nba.com/stats/commonteamroster?LeagueID=00&Season=' %>%
    paste0(id.season, '&TeamID=', team_id)

  json_data <-
    roster_url %>%
    fromJSON(simplifyDataFrame = T, flatten = T)

  names_roster <-
    json_data$resultSets$headers[1] %>%
    unlist %>%
    str_to_lower

  data_roster <-
    json_data$resultSets$rowSet[1] %>%
    data.frame(stringsAsFactors = F) %>%
    tbl_df

  names(data_roster) <-
    names_roster

  data_roster %<>%
    rename(
      id.team = teamid,
      id.player = player_id,
      name.player = player,
      number = num,
      date.birth = birth_date,
      years.experience = exp,
      weight.lbs = weight,
      number.jersey = num
    ) %>%
    dplyr::select(-c(leagueid, season)) %>%
    mutate(
      is.rookie = ifelse(years.experience == "R", T, F),
      years.experience = years.experience %>% str_replace("R", 0) %>% as.numeric(),
      id.team = id.team %>% as.numeric,
      number.jersey = number.jersey %>% as.numeric,
      height.inches = height %>% lapply(height_in_inches) %>% unlist,
      weight.lbs = weight.lbs %>% as.numeric,
      date.birth = date.birth %>% as.Date('%b %d, %Y'),
      id.player = id.player %>% as.numeric,
      id.season,
      season.year_end = year_season_end
    ) %>%
    dplyr::select(id.season,
                  season.year_end,
                  id.player,
                  name.player,
                  everything()) %>%
    separate(
      position,
      sep = '\\-',
      into = c('id.position', 'id.position.secondary')
    ) %>%
    left_join(
      teams_ids %>%
        group_by(id.team) %>%
        dplyr::filter(team == t) %>%
        dplyr::filter(year.start.team == min(year.start.team)) %>%
        ungroup
    ) %>%
    suppressMessages() %>%
    dplyr::select(-c(city.team, name.team, season.year_end, year.start.team)) %>%
    dplyr::select(
      id.season,
      id.team,
      team,
      id.position,
      id.position.secondary,
      is.rookie,
      name.player,
      weight.lbs,
      height.inches,
      everything()
    ) %>%
    suppressWarnings()

  if (return_message == T) {
    "You got the NBA roster for the " %>%
      paste0(t, " for the ", id.season, ' season.') %>%
      message()
  }
  return(data_roster)
}

get_nba_team_season_roster_safe <-
  failwith(NULL, get_nba_team_season_roster)

get_all_teams_season_rosters <-
  function(year_season_end = 1990,
           message = T) {
    teams <-
      get_nba_franchise_data(return_franchises = 'all')

    if (year_season_end > teams$year.start.team %>% max) {
      max_year <-
        teams$year.end.team %>% max
      teams_names <-
        teams %>%
        mutate(is.active = ifelse(year.end.team == max_year, T, F)) %>%
        dplyr::filter(is.active == T) %>%
        group_by(id.team) %>%
        dplyr::filter(year.end.team == max(year.end.team)) %>%
        dplyr::select(team) %>%
        distinct() %>%
        .$team
    } else {
      teams_ids <-
        teams %>%
        left_join(teams %>%
                    count(id.team) %>%
                    dplyr::rename(count.teams = n))

      one_teams <-
        teams_ids %>% dplyr::filter(count.teams == 1)

      teams_names <-
        teams_ids %>%
        dplyr::filter(count.teams > 1) %>%
        dplyr::filter(year_season_end >= year.start.team,
                      year_season_end < year.end.team) %>%
        group_by(id.team) %>%
        dplyr::filter(year.end.team == min(year.end.team)) %>%
        bind_rows(one_teams) %>%
        dplyr::filter(is.active == T) %>%
        dplyr::filter(year_season_end >= year.start.team) %>%
        arrange(team) %>%
        .$team %>%
        unique

    }
    yse <-
      year_season_end

    all_rosters <-
      teams_names %>%
      map(
        function(x)
          get_nba_team_season_roster_safe(
            team = x,
            year_season_end = yse,
            return_message = message
          )
      ) %>%
      compact %>%
      bind_rows()

    return(all_rosters)
  }

get_all_teams_season_rosters_safe <-
  failwith(NULL, get_all_teams_season_rosters)

get_all_teams_seasons_rosters <-
  function(seasons = 2005:2006,
           return_message = T) {
    all_years <-
      seasons %>%
      map(
        function(x)
          get_all_teams_season_rosters_safe(year_season_end = x, message = return_message)
      ) %>%
      compact %>%
      bind_rows()
    return(all_years)
  }


get_nba_team_season_coaches <- function(team,
                                        year_season_end = 2016,
                                        return_message = T)  {
  if (!'team' %>% exists) {
    stop("Please enter a team")
  }

  year_season_start <-
    year_season_end - 1

  id.season <-
    year_season_start %>%
    paste(year_season_end %>% substr(start = 3, stop = 4),
          sep = "-")

  teams <-
    get_nba_franchise_data(return_franchises = 'all', return_message = F)

  teams_ids <-
    teams %>%
    dplyr::select(id.team, city.team, name.team, team, year.start.team)

  t <-
    team

  team_id <-
    teams_ids %>%
    dplyr::filter(team == t) %>%
    .$id.team %>%
    unique()

  if (team_id %>% length > 1) {
    team_id <-
      team_id[1]
  }

  if (year_season_end - 1 <
      teams %>%
      dplyr::filter(id.team == team_id) %>%
      dplyr::filter(year.start.team == min(year.start.team)) %>%
      .$year.start.team %>% unique()) {
    "Sorry " %>%
      paste0(
        year_season_end,
        ' is not a valid season for the ',
        teams_ids %>%
          dplyr::filter(id.team == team_id) %>%
          .$team
      ) %>%
      message()

  }

  roster_url <-
    'http://stats.nba.com/stats/commonteamroster?LeagueID=00&Season=' %>%
    paste0(id.season, '&TeamID=', team_id)

  json_data <-
    roster_url %>%
    fromJSON(simplifyDataFrame = T, flatten = T)

  names_coaches <-
    json_data$resultSets$headers[2] %>%
    unlist %>%
    str_to_lower()

  data_coaches <-
    json_data$resultSets$rowSet[2] %>%
    data.frame(stringsAsFactors = F) %>%
    tbl_df

  if (data_coaches %>% nrow == 0)  {
    "Sorry " %>%
      paste0(team, " has no data for ", id.season) %>%
      stop()
  }

  names(data_coaches) <-
    names_coaches

  data_coaches %<>%
    dplyr::select(-c(season, first_name, last_name, sort_sequence, coach_code)) %>%
    dplyr::rename(
      id.team = team_id,
      id.coach = coach_id,
      name.coach = coach_name,
      type.coach = coach_type,
      id.coach_type = is_assistant
    ) %>%
    mutate(
      is.head_coach = ifelse(id.coach_type == "1", T, F),
      id.team = id.team %>% as.numeric,
      id.coach_type = id.coach_type %>% as.numeric,
      id.season,
      season.year_end = year_season_end,
      is.coach = T
    ) %>%
    dplyr::select(id.season, season.year_end, everything()) %>%
    separate(school,
             into = c('type.school', 'school'),
             sep = '\\-') %>%
    mutate(school = school %>% str_trim,
           type.school = type.school %>% str_trim) %>%
    left_join(
      teams_ids %>%
        group_by(id.team) %>%
        dplyr::filter(team == t) %>%
        dplyr::filter(year.start.team == min(year.start.team)) %>%
        ungroup
    ) %>% suppressMessages() %>%
    dplyr::select(-c(season.year_end, name.team, city.team, year.start.team)) %>%
    dplyr::select(id.season, team,
                  everything())


  if (return_message == T) {
    "You got the " %>%
      paste0(id.season,
             ' coaching data for the ',
             data_coaches$team %>% unique) %>%
      message()
  }
  return(data_coaches)
}

get_nba_team_season_coach_safe <-
  failwith(NULL, get_nba_team_season_coaches)


get_all_teams_season_coaches <-
  function(year_season_end = 2005,
           message = T) {
    teams <-
      get_nba_franchise_data(return_franchises = 'all')

    if (year_season_end > teams$year.start.team %>% max) {
      max_year <-
        teams$year.end.team %>% max
      teams_names <-
        teams %>%
        mutate(is.active = ifelse(year.end.team == max_year, T, F)) %>%
        dplyr::filter(is.active == T) %>%
        group_by(id.team) %>%
        dplyr::filter(year.end.team == max(year.end.team)) %>%
        dplyr::select(team) %>%
        distinct() %>%
        .$team
    } else {
      teams_ids <-
        teams %>%
        left_join(teams %>%
                    count(id.team) %>%
                    dplyr::rename(count.teams = n))

      one_teams <-
        teams_ids %>% dplyr::filter(count.teams == 1)

      teams_names <-
        teams_ids %>%
        dplyr::filter(count.teams > 1) %>%
        dplyr::filter(year_season_end >= year.start.team,
                      year_season_end < year.end.team) %>%
        group_by(id.team) %>%
        dplyr::filter(year.end.team == min(year.end.team)) %>%
        bind_rows(one_teams) %>%
        dplyr::filter(is.active == T) %>%
        dplyr::filter(year_season_end >= year.start.team) %>%
        arrange(team) %>%
        .$team %>%
        unique

    }
    yse <-
      year_season_end

    all_coaches <-
      teams_names %>%
      map(
        function(x)
          get_nba_team_season_coach_safe(
            team = x,
            year_season_end = yse,
            return_message = message
          )
      ) %>%
      compact %>%
      bind_rows()

    return(all_coaches)
  }

get_all_teams_season_coaches_safe <-
  failwith(NULL, get_all_teams_season_coaches)




get_all_teams_seasons_coaches <-
  function(seasons = 2005:2006,
           return_message = T) {
    all_years <-
      seasons %>%
      map(
        function(x)
          get_all_teams_season_coaches_safe(year_season_end = x, message = return_message)
      ) %>%
      compact %>%
      bind_rows()
    return(all_years)
  }
