#' GET NBA PLAYER SHOT DETAIL DATA
#'
#' @param player
#' @param year_season_end
#' @param use_shot_zone_side
#' @param season_type
#' @param shot_types
#' @param shot_areas
#' @param vs_conference
#' @param vs_division
#' @param quarter_range
#' @param minute_range
#' @param against_team
#' @param game_location
#' @param game_month
#' @param outcome
#' @param position
#' @param season_segment
#' @param exclude_backcourt
#' @param return_message
#'
#' @return
#' @export 
#'
#' @examples get_player_season_shot_data(player = "K.J. Mcdaniels", year_season_end = 2015)
get_player_season_shot_data <- function(player,
                                        year_season_end = 2015,
                                        use_shot_zone_side = F,
                                        season_type = "Regular Season",
                                        # Regular Season, Preseason, Playoffs, All Star
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
                                        position = NA,
                                        # NA or Guard, Center, Forward
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
  options(warn = -1, show.error.messages = F)
  lapply(packages, library, character.only = T)
  if (year_season_end < 1997) {
    stop.message <-
      "Sorry NBA Shooting data exists only since the 1996-97 Season!!"
    stop(stop.message, call. = F)
  } else {
    p <-
      player %>%
      str_to_lower() %>% 
      str_replace_all('\\.','')
    
    year_season_start <-
      year_season_end - 1
    
    id.season <-
      year_season_start %>%
      paste(year_season_end %>% substr(start = 3, stop = 4),
            sep = "-")
    
    players <-
      nbastatR::get_nba_players_ids()
    
    if (players %>% mutate(name.player = name.player %>% str_to_lower %>% str_replace_all('\\.','')) %>% dplyr::filter(name.player == p) %>% .$id.player %>% length == 0) {
      paste0(player,
             ' is not a valid player, try capitalizing or checking spelling') %>%
        message
    }
    
    id.player <-
      players %>%
      dplyr::mutate(name.player = name.player %>% str_to_lower %>% str_replace_all('\\.','')) %>%
      dplyr::filter(name.player == p) %>%
      .$id.player
    
    ## Teams
      teams_ids <-
        get_nba_franchise_data(return_franchises = 'current') %>%
        select(team_id, team_city, team_name) %>%
        mutate(team = paste(team_city, team_name)) %>%
        rename(
          id.team = team_id,
          name.team = team_name,
          id.game_event = game_event_id,
          city.team = team_city
        )
      
      against_team %<>%
        str_to_title()
      
      against_team_id <-
        teams_ids %>%
        dplyr::filter(name.team == against_team) %>%
        .$id.team %>%
        as.numeric()
      
      against_team_stem <-
        'OpponentTeamID=' %>%
        paste0(against_team_id)
    } else {
      against_team_stem <-
        'OpponentTeamID=0'
    }
    
    ## URL construction
    
    base_url <-
      'http://stats.nba.com/stats/shotchartdetail?CFID=33&CFPARAMS=' %>%
      paste0(
        id.season,
        '&ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=&GameID=&GameSegment=&LastNGames=0&LeagueID=00'
      )
    
    player_stem <-
      'PlayerID=' %>%
      paste0(id.player)
    
    season_type_stem <-
      'SeasonType=' %>%
      paste0(season_type %>% str_replace('\\ ', '+'))
    
    season_stem <-
      "Season=" %>%
      paste0(id.season)
    
    
    if (game_location %>% is.na()) {
      location_stem <-
        'Location='
    } else {
      location_stem <-
        'Location=' %>%
        paste0(game_location)
    }
    
    if (game_month %>% is.na()) {
      month_stem <-
        'Month=0'
    } else {
      game_month <-
        'Month=' %>%
        paste0(game_month)
    }
    
    if (outcome %>% is.na) {
      outcome_stem <-
        'Outcome='
    } else {
      outcome_stem <-
        'Outcome=' %>%
        paste0(outcome)
    }
    
    if (position %>% is.na) {
      position_stem <-
        'Position='
    } else {
      position_stem <-
        'Position=' %>%
        paste0(position)
    }
    
    if (season_segment %>% is.na) {
      season_segment_stem <-
        'SeasonSegment='
    } else {
      season_segment_stem <-
        'SeasonSegment=' %>%
        paste0(season_segment %>% str_replace('\\ ', '+'))
    }
    
    if (vs_conference %>% is.na) {
      conference_stem <-
        'VsConference='
    } else {
      conference_stem <-
        'VsConference=' %>%
        paste0(vs_conference)
    }
    
    if (vs_division %>% is.na) {
      division_stem <-
        'VsDivision='
    } else {
      division_stem <-
        'VsDivision=' %>%
        paste0(vs_division)
    }
    
    final_stem <-
      'mode=Advanced&showDetails=0&showShots=1&showZones=1'
    
    shot_data_url <-
      base_url %>%
      paste(
        location_stem,
        'MeasureType=Base',
        month_stem,
        against_team_stem,
        outcome_stem,
        'PaceAdjust=N&PerMode=PerGame&Period=0',
        player_stem,
        'PlusMinus=N',
        position_stem,
        'Rank=N&RookieYear=',
        season_stem,
        season_segment_stem,
        season_type_stem,
        conference_stem,
        division_stem,
        final_stem,
        'TeamID=0',
        sep = '&'
      )
    
    ## parameters
    
    parameter_df <-
      data_frame(
        id.season,
        year_season_end,
        player,
        id.player,
        use_shot_zone_side,
        outcome,
        game_month,
        game_location,
        against_team,
        vs_division,
        vs_conference,
        url.player.photo = 'http://stats.nba.com/media/players/230x185/' %>%
          paste0(id.player, '.png'),
        season_type,
        shot_areas = ifelse(
          shot_areas %>% length > 1,
          shot_areas %>% paste0(collapse = ', '),
          shot_areas
        ),
        minute_range = ifelse(
          minute_range %>% length > 1,
          minute_range %>% paste0(collapse = ', '),
          minute_range
        ),
        quarter_range = ifelse(
          position %>% length > 1,
          position %>% paste0(collapse = ', '),
          position
        ),
        date.data = Sys.Date()
      )
    
    
    json_data <-
      shot_data_url %>%
      jsonlite::fromJSON(simplifyDataFrame = T)
    
    if (json_data$resultSets$rowSet %>%
        .[1] %>%
        data.frame %>%
        tbl_df %>% nrow > 0) {
      data.shots <-
        json_data$resultSets$rowSet %>%
        .[1] %>%
        data.frame %>%
        tbl_df
      
      names(data.shots) <-
        json_data$resultSets$headers %>%
        .[1] %>%
        unlist %>%
        str_to_lower()
      
      data.shots %<>%
        mutate_each(funs(as.numeric), matches("loc")) %>%
        mutate_each(funs(as.numeric), matches("remaining")) %>%
        mutate_each(funs(as.numeric), matches("id")) %>%
        mutate_each(funs(as.numeric), matches("distance")) %>%
        mutate(
          period = period %>% as.numeric,
          shot_attempted_flag = "1" %>% grepl(shot_attempted_flag),
          shot_made_flag = "1" %>% grepl(shot_made_flag)
        )
      
      if (exclude_backcourt == T) {
        data.shots %<>%
          dplyr::filter(!shot_zone_basic == 'Backcourt')
      }
      
      sides <-
        c(
          "Above the Break 3, BC",
          "Above the Break 3, C",
          "Above the Break 3, LC",
          "Above the Break 3, RC",
          "In The Paint (Non-RA), C",
          "In The Paint (Non-RA), L",
          "In The Paint (Non-RA), R",
          "Left Corner 3, L",
          "Mid-Range, C",
          "Mid-Range, L",
          "Mid-Range, LC",
          "Mid-Range, R",
          "Mid-Range, RC",
          "Restricted Area, C",
          "Right Corner 3, R"
        )
      
      shot_zone_df <-
        data_frame(
          shot_zone_basic = c(
            'Left Corner 3',
            'Right Corner 3',
            'Above the Break 3',
            'Mid-Range',
            'In The Paint (Non-RA)',
            'Restricted Area'
          ),
          shot_area = c(rep('Three Point', 3), 'Mid-Range', rep('Paint', 2))
        )
      
      shot_zone_detail <-
        data_frame(shot_side = sides) %>%
        separate(
          col = shot_side,
          into = c('shot_zone_basic', 'id.side'),
          sep = '\\, ',
          remove = F
        ) %>%
        left_join(shot_zone_df)
      
      data.shots %<>%
        dplyr::filter(period %in% quarter_range) %>%
        mutate(minute = 12 - minutes_remaining) %>%
        dplyr::filter(minute %in% minute_range)
      
      shots_type %<>%
        str_to_lower %>%
        paste0(collapse = "|")
      
      data.shots %<>%
        rename(
          id.player = player_id,
          id.game = game_id,
          name.team = team_name,
          id.team = team_id,
          name.player = player_name
        ) %>%
        select(-grid_type) %>%
        mutate(
          id.season,
          is.shot_made = ifelse(shot_made_flag == T, T, F),
          shot_made = ifelse(shot_made_flag == T, "YES", "NO")
        ) %>%
        separate(shot_zone_area, '\\(', into = c('side', 'id.side')) %>%
        mutate(
          id.side = id.side %>% str_replace('\\)', ''),
          shot_side = shot_zone_basic %>% paste0(', ', id.side)
        ) %>%
        left_join(shot_zone_detail) %>%
        dplyr::filter(shot_area %in% shot_areas) %>%
        mutate(st = action_type %>% str_to_lower()) %>%
        dplyr::filter(st %like% shots_type) %>%
        select(-st) %>%
        select(id.season,
               name.player,
               id.player,
               is.shot_made,
               shot_area,
               everything())
      
      data.shots %<>%
        mutate(
          second = 60 - seconds_remaining,
          time.game = (12 * (period - 1) + minute) %>% paste0(' min ', second, ' sec')
        )
      
      data <-
        list(parameter_df, data.shots)
      
      names(data) <-
        c('parameters', 'shots')
      
      if (return_message == T) {
        "Congrats, you got " %>%
          paste0(data.shots %>% nrow,
                 ' shots for ',
                 player,
                 ' during the ',
                 id.season,
                 ' season') %>% message
      }
      return(data)
    } else{
      'No shots for ' %>%
        paste0(player, ' during the ', id.season,
               ' season') %>%
        message()
    }
  }
}
