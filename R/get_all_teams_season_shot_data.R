#' Get Teams Shot Data
#'
#' @param team
#' @param year_roster
#' @param year_data
#' @param use_shot_zone_side
#' @param season_type
#' @param positions
#' @param shots_type
#' @param shot_areas
#' @param vs_conference
#' @param vs_division
#' @param quarter_range
#' @param minute_range
#' @param against_team
#' @param game_location
#' @param game_month
#' @param outcome
#' @param season_segment
#' @param exclude_backcourt
#' @param return_message
#'
#' @return
#' @export
#'
#' @examples
get_team_season_shot_data <- function(team,
                                      year_roster = 2015,
                                      year_data = 2015,
                                      use_shot_zone_side = F,
                                      season_type = "Regular Season",
                                      # Regular Season, Preseason, Playoffs, All Star
                                      positions = c('G', 'F', 'C'),
                                      # G, FA, C
                                      shots_type = c('Dunk', 'Layup', 'Jump Shot', "Fadeaway", "Bank", "Tip",
                                                     'Hook'),
                                      shot_areas = c('Three Point', "Paint", "Mid-Range"),
                                      vs_conference = NA,
                                      # NA or East, West,
                                      vs_division = NA,
                                      # Atlantic, Central, Northwest, Pacific, Southeast, Southwest
                                      quarter_range = 1:12,
                                      minute_range = 0:12,
                                      against_team = NA,
                                      # NA Any Team Name
                                      game_location = NA,
                                      # NA or Home, Road
                                      game_month = NA,
                                      # NA or 1:12
                                      outcome = NA,
                                      # NA or W, L
                                      season_segment = NA,
                                      #NA Post All-Star, Pre All-Star
                                      exclude_backcourt = T,
                                      return_message = T) {
  packages <- #need all of these installed including some from github
    c('dplyr',
      'magrittr',
      'jsonlite',
      'tidyr',
      'stringr',
      'data.table',
      'tidyr')
  options(warn = -1)
  lapply(packages, library, character.only = T)
  if (year_data < 1997) {
    stop.message <-
      "Sorry NBA Shooting data exists only since the 1996-97 season!!"

    stop(stop.message)
  }
  t <-
    team %>%
    str_to_title()

  yr <-
    year_roster

  yd <-
    year_data

  rm <-
    F

  roster_data <-
    get_nba_teams_seasons_roster(
      team = t,
      year_season_end = yr,
      include_coaches = F,
      return_message = rm
    )

  uid <-
    use_shot_zone_side
  st <-
    season_type
  sa <-
    shot_areas
  shot_t <-
    shots_type
  eb <-
    exclude_backcourt
  vs_conf <-
    vs_conference
  vs_div <-
    vs_division
  q_range <-
    quarter_range
  min_range <-
    minute_range
  ag_team <-
    against_team
  game_loc <-
    game_location
  game_mon <-
    game_month
  out <-
    outcome

  season_seg <-
    season_segment

  players <-
    roster_data %>%
    dplyr::filter(years.experience > 0, id.position %in% positions) %>%
    .$name.player

  all_params <-
    data_frame()

  all_shots <-
    data_frame()

  for (p in players) {
    data <-
      get_player_season_shot_data(
        player = p,
        year_season_end = yd,
        use_shot_zone_side = uid,
        season_type = st,
        shots_type = shot_t,
        shot_areas = sa,
        vs_conference = vs_conf,
        vs_division = vs_div,
        quarter_range = q_range,
        minute_range = min_range,
        against_team = ag_team,
        game_location = game_loc,
        game_month = game_mon,
        outcome = out,
        season_segment = season_seg,
        exclude_backcourt = eb,
        return_message = rm
      )

    all_params %<>%
      bind_rows(data$parameters)

    all_shots %<>%
      bind_rows(data$shots)
    }
  all_shots %<>%
    left_join(roster_data %>%
                dplyr::select(id.player, name.player, id.position))

  data <-
    list(all_params,
         all_shots)

  names(data) <-
    c('parameters', 'shots')
  return(data)
}
