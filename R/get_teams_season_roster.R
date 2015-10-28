#' Title
#'
#' @param team
#' @param include_coaches
#' @param return_message
#'
#' @return
#' @export
#'
#' @examples
get_nba_teams_seasons_roster <- function(team,
                                         year_season_end = 2016,
                                         include_coaches = F,
                                         return_message = T){
  packages <- #need all of these installed including some from github
    c(
      'dplyr',
      'magrittr',
      'jsonlite',
      'tidyr',
      'stringr',
      'lubridate',
      'stringr',
      'tidyr'
    )
  options(warn = -1)
  lapply(packages, library, character.only = T)
  teams <-
    c("76ers", "Bucks", "Bulls", "Cavaliers", "Celtics", "Clippers",
      "Grizzlies", "Hawks", "Heat", "Hornets", "Jazz", "Kings", "Knicks",
      "Lakers", "Magic", "Mavericks", "Nets", "Nuggets", "Pacers",
      "Pelicans", "Pistons", "Raptors", "Rockets", "Spurs", "Suns",
      "Thunder", "Timberwolves", "Trail Blazers", "Warriors", "Wizards"
    )
  team %<>%
    str_to_title()
  if(team %>% grep(teams,fixed = T) %>% length == 0 ){
    stop("Teams can only be " %>% paste0(paste0(teams,collapse = ', ')))
  } else{
    team <-
      teams[team %>% grep(teams, fixed = T)]
  }
  year_season_start <-
    year_season_end - 1

  id.season <-
    year_season_start %>%
    paste(year_season_end %>% substr(start = 3, stop = 4),
          sep = "-")

  t <-
    team %>%
    str_to_title()

  if (t %in%  c("76ers", "Bucks", "Bulls", "Cavaliers", "Celtics", "Clippers",
                "Grizzlies", "Hawks", "Heat", "Hornets", "Jazz", "Kings", "Knicks",
                "Lakers", "Magic", "Mavericks", "Nets", "Nuggets", "Pacers",
                "Pelicans", "Pistons", "Raptors", "Rockets", "Spurs", "Suns",
                "Thunder", "Timberwolves", "Trail Blazers", "Warriors", "Wizards"
  )
  ){
    teams <-
      get_nba_franchise_data(return_franchises = 'current') %>%
      rename(id.team = team_id, city.team = team_city, name.team = team_name) %>%
      mutate(team = paste(city.team, name.team)) %>%
      ungroup %>%
      mutate(id.team = id.team %>% as.numeric)

    teams_ids <-
      teams %>%
      dplyr::select(id.team, city.team, name.team, team)

    team_id <-
      teams_ids %>%
      dplyr::filter(name.team == t) %>%
      .$id.team

    if (year_season_end-1 < teams %>%
        dplyr::filter(id.team == team_id) %>%
        .$start_year) {

      "Sorry " %>%
        paste0(year_season_end, ' is not a valid season for the ',
               teams_ids %>%
                 dplyr::filter(id.team == team_id) %>%
                 .$team) %>%
        message()

    }

    roster_url <-
      'http://stats.nba.com/stats/commonteamroster?LeagueID=00&Season=' %>%
      paste0(id.season, '&TeamID=', team_id)

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

    json_data <-
      roster_url %>%
      fromJSON(simplifyDataFrame = T, flatten = F)

    names_roster <-
      json_data$resultSets$headers[1] %>%
      unlist %>%
      str_to_lower

    data_roster <-
      json_data$resultSets$rowSet[1] %>%
      data.frame %>%
      tbl_df

    names(data_roster) <-
      names_roster

    data_roster %<>%
      rename(id.team = teamid,
             id.player = player_id,
             name.player = player,
             number = num,
             date.birth = birth_date,
             years.experience = exp,
             weight.lbs = weight,
             number.jersey = num
      ) %>%
      dplyr::select(-c(leagueid, season)) %>%
      mutate(is.rookie = ifelse(years.experience == "R", T, F),
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
      dplyr::select(id.season,season.year_end, id.player, name.player, everything()) %>%
      separate(position, sep = '\\-',into = c('id.position', 'id.position.secondary')) %>%
      left_join(teams_ids)

    if (include_coaches == T) {
      data_roster %<>%
        mutate(is.coach = T)

      names_coaches <-
        json_data$resultSets$headers[2] %>%
        unlist %>%
        str_to_lower()

      data_coaches <-
        json_data$resultSets$rowSet[2] %>%
        data.frame %>%
        tbl_df

      names(data_coaches) <-
        names_coaches

      data_coaches %<>%
        dplyr::select(-c(season, first_name, last_name, sort_sequence, coach_code)) %>%
        rename(id.team = team_id,
               id.coach = coach_id,
               name.coach = coach_name,
               type.coach = coach_type,
               id.coach_type = is_assistant) %>%
        mutate(is.head_coach = ifelse(id.coach_type == "1", T, F),
               id.team = id.team %>% as.numeric,
               id.coach_type = id.coach_type %>% as.numeric,
               id.season,
               season.year_end = year_season_end,
               is.coach = T
        ) %>%
        dplyr::select(id.season, season.year_end, everything()) %>%
        separate(school, into = c('type.school', 'school'), sep = '\\-') %>%
        mutate(school = school %>% str_trim,
               type.school = type.school %>% str_trim) %>%
        left_join(teams_ids)

      data <-
        list(data_roster,
             data_coaches)
      names(data) <-
        c('roster', 'coaches')
    } else {
      data <-
        data_roster
    }
    if (return_message == T) {

      "You got the " %>%
        paste0(id.season,' roster data for the ',
               data_roster$team %>% unique) %>%
        message()
    }
    return(data)
  } else {
    "Sorry " %>%
      paste0(t,' is not a valid team name.') %>%
      message
  }
}
