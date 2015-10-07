#' Gets NBA team Bokeh Shot Chart
#'
#' @param team  can be = #c("76ers", "Bucks", "Bulls", "Cavaliers", "Celtics", "Clippers", 
#"Grizzlies", "Hawks", "Heat", "Hornets", "Jazz", "Kings", "Knicks", 
#"Lakers", "Magic", "Mavericks", "Nets", "Nuggets", "Pacers", 
#"Pelicans", "Pistons", "Raptors", "Rockets", "Spurs", "Suns", 
#"Thunder", "Timberwolves", "Trail Blazers", "Warriors", "Wizards")
#' @param year_roster - year of the roster
#' @param year_data 
#' @param plot_hex - T or F, shows
#' @param author 
#' @param use_shot_zone_side 
#' @param season_type - Regular Season, Pre Season, Playoffs, All Star
#' @param positions  - G, FA, C
#' @param shots_type -  ('Dunk', 'Layup', 'Jump Shot', "Fadeaway", "Bank", "Tip",'Hook')
#' @param shot_areas  - c('Three Point', "Paint", "Mid-Range")
#' @param vs_conference - NA or East, West,
#' @param vs_division  - Atlantic, Central, Northwest, Pacific, Southeast, Southwest
#' @param quarter_range - 0:8
#' @param minute_range - 0:12
#' @param against_team - Same as team
#' @param game_location - NA or Home, Road
#' @param game_month 
#' @param outcome  - NA, W, L
#' @param season_segment NA, Post All-Star, Pre All-Star
#' @param exclude_backcourt 
#'
#' @return
#' @export
#'
#' @examples nba_team_bokeh_shotchart(team = "Nets", plot_hex = F, quarter_range = c(3,4))

nba_team_season_bokeh_shotchart <- function(team = "Nets",
                                     year_roster = 2016,
                                     year_data = 2015,
                                     plot_hex = T,
                                     author = "Alex Bresler",
                                     use_shot_zone_side = T,
                                     season_type = "Regular Season",
                                     positions = c('G', 'F', 'C'), 
                                     shots_type = c('Dunk', 'Layup', 'Jump Shot', "Fadeaway", "Bank", "Tip",
                                                    'Hook'),
                                     shot_areas = c('Three Point', "Paint", "Mid-Range"),
                                     vs_conference = NA,
                                     vs_division = NA,
                                     quarter_range = 1:12,
                                     minute_range = 0:12,
                                     against_team = NA,
                                     game_location = NA,
                                     game_month = NA,
                                     outcome = NA,
                                     season_segment = NA,
                                     exclude_backcourt = T) {
  packages <- c(
    'dplyr',
    'magrittr',
    'jsonlite',
    'tidyr',
    'stringr',
    'formattable',
    #devtools::install_github(renkun-ken/formattable)
    'rbokeh',
    #devtools::install_github(hafen/rbokeh)
    'hexbin',
    'stringr',
    'tidyr'
  )
  options(warn = -1)
  lapply(packages, library, character.only = T)
  
  
  t <-
    team %>%
    str_to_title()
  
  ysr <-
    year_roster
  
  ysd <-
    year_data
  
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
  pos <-
    positions
  season_seg <-
    season_segment
  
  data <-
    nbastatR::get_team_season_shot_data(
      team = t,
      year_roster = ysr,
      year_data = ysd,
      use_shot_zone_side = uid,
      season_type = st,
      positions = pos,
      shots_type = shot_t,
      shot_areas = sa,
      vs_conference = vs_conf,
      vs_division = vs_div,
      quarter_range = quarter_range,
      minute_range = minute_range,
      against_team = ag_team,
      game_location = game_loc,
      game_month = game_mon,
      outcome = out,
      season_segment = season_seg,
      exclude_backcourt = eb,
      return_message = F
    )
  
  year_season_start <-
    ysd - 1
  
  id.season <-
    year_season_start %>%
    paste(ysd %>% substr(start = 3, stop = 4),
          sep = "-")
  
  data.shots <-
    data$shots
  
  data.shots %<>%
    mutate(shot_made = ifelse(shot_made_flag == T, "YES", "NO"))
  
  if (exclude_backcourt == T) {
    data.shots %<>%
      dplyr::filter(!id.side %in% 'BC')
  }
  parameters <-
    data$parameters
  
  summary_shots <-
    data.shots %>%
    group_by(shot_made_flag) %>%
    summarise(shots = n())
  
  accuracy_data <-
    data.shots %>%
    group_by(shot_zone_basic) %>%
    mutate(shot_value = ifelse(shot_made_flag == TRUE, 1, 0))
  
  nba_ids <-
    data_frame(
      team = c(
        "Atlanta Hawks",
        "Boston Celtics",
        "Brooklyn Nets",
        "Charlotte Hornets",
        "Chicago Bulls",
        "Cleveland Cavaliers",
        "Dallas Mavericks",
        "Denver Nuggets",
        "Detroit Pistons",
        "Golden State Warriors",
        "Houston Rockets",
        "Indiana Pacers",
        "Los Angeles Clippers",
        "Los Angeles Lakers",
        "Memphis Grizzlies",
        "Miami Heat",
        "Milwaukee Bucks",
        "Minnesota Timberwolves",
        "New Orleans Pelicans",
        "New York Knicks",
        "Oklahoma City Thunder",
        "Orlando Magic",
        "Philadelphia 76ers",
        "Phoenix Suns",
        "Portland Trail Blazers",
        "Sacramento Kings",
        "San Antonio Spurs",
        "Toronto Raptors",
        "Utah Jazz",
        "Washington Wizards"
      ),
      name = c(
        "Hawks",
        "Celtics",
        "Nets",
        "Hornets",
        "Bulls",
        "Cavaliers",
        "Mavericks",
        "Nuggets",
        "Pistons",
        "Warriors",
        "Rockets",
        "Pacers",
        "Clippers",
        "Lakers",
        "Grizzlies",
        "Heat",
        "Bucks",
        "Timberwolves",
        "Pelicans",
        "Knicks",
        "Thunder",
        "Magic",
        "76ers",
        "Suns",
        "Trail Blazers",
        "Kings",
        "Spurs",
        "Raptors",
        "Jazz",
        "Wizards"
      ),
      slug_current =
        c(
          "ATL",
          "BOS",
          "BKN",
          "CHA",
          "CHI",
          "CLE",
          "DAL",
          "DEN",
          "DET",
          "GSW",
          "HOU",
          "IND",
          "LAC",
          "LAL",
          "MEM",
          "MIA",
          "MIL",
          "MIN",
          "NOP",
          "NYK",
          "OKC",
          "ORL",
          "PHI",
          "PHO",
          "POR",
          "SAC",
          "SAS",
          "TOR",
          "UTA",
          "WAS"
        )
    ) %>%
    mutate(url.logo = 'http://stats.nba.com/media/img/teams/logos/' %>% paste0(slug_current, '_logo.svg'))
  
  url.team.logo <-
    nba_ids %>%
    dplyr::filter(name == t) %>%
    .$url.logo
  
  
  ## Parameter Label
  if (minute_range  == 0:12) {
    minute_label <-
      'All Minutes'
  } else {
    minute_label <-
      minute_range %>%
      paste0(collapse = ', ') %>%
      paste0('Minutes ', .)
  }
  
  if (quarter_range %>% length == 1) {
    quarter_label <-
      "Quarter " %>%
      paste0(quarter_range)
  }
  
  if (quarter_range %>% length > 6) {
    quarter_label <-
      "All Quarters"
  }
  
  if (quarter_range %>% length ==  2 |
      quarter_range %>% length == 3) {
    quarter_label <-
      "Quarters " %>%
      paste0(quarter_range %>% paste0(collapse = " and "))
  }
  
  if (outcome %>% is.na) {
    outcome_label <-
      ''
  } else {
    outcome_label <-
      outcome %>%
      str_replace("W", "In Wins ") %>%
      str_replace("L", "In Losses ")
  }
  
  if (against_team %>% is.na) {
    opponent_label <-
      'VS. All Opponents'
  } else {
    opponent_label <-
      ' VS.' %>%
      paste(against_team)
  }
  
  if (vs_conference %>% is.na) {
    conf_label <-
      ''
  } else {
    conf_label <-
      ', ' %>%
      paste0(vs_conference, 'ern Conference')
  }
  
  if (vs_division %>% is.na) {
    division_label <-
      ''
  } else {
    division_label <-
      ', ' %>%
      paste(vs_division, 'Division')
  }
  
  if (game_location %>% is.na) {
    location_label <-
      ''
  } else {
    location_label <-
      game_location %>%
      str_replace("Home", " at Home") %>%
      str_replace("Road", " on Road")
  }
  
  if (pos %>% length == 3) {
    position_label <-
      ", All Positions"
  }
  if (pos %>% length == 2) {
    position_label <-
      pos %>%
      paste0(collapse = ', ') %>%
      paste0(', ', .)
  }
  
  if (pos %>% length == 2) {
    position_label <-
      paste0(' ', pos)
  }
  
  if (game_month %>% is.na) {
    month_label <-
      ''
  } else {
    month_label <-
      ' During Month ' %>%
      paste0(game_month)
  }
  
  if (season_segment %>% is.na) {
    season_segment_label <-
      ''
  } else {
    season_segment_label <-
      season_segment %>%
      paste0(', ', .)
  }
  
  if (shot_areas %>% length == 3) {
    shot_area_label <-
      ', All Shot Areas'
  } else {
    shot_area_label <-
      ' from ' %>%
      paste0(shot_areas %>% paste0(collapse = ' & the '))
  }
  
  team_label <- 
    nba_ids %>%
    dplyr::filter(name == t) %>%
    .$team %>% 
    paste0(" Shot Chart", shot_area_label) %>%
    str_trim
  
  if (ysd == ysr) {
    season_label <- 
      paste0(id.season)
  } else {
    id.season.roster <-
      paste0(ysr - 1, "-", substr(ysr, 3, 4))
    
    season_label <-
      paste0(id.season, ' Data, ', id.season.roster, ' Roster ')
  }
  
  line_2_label <-
    paste(season_label, season_type, season_segment_label) %>%
    str_trim
  
  line_3_label <-
    quarter_label %>%
    paste0(', & ', minute_label, month_label)
  
  line_4_label <-
    outcome_label %>%
    paste0(opponent_label,
           conf_label,
           division_label,
           location_label,
           position_label) %>%
    str_trim
  
  if (use_shot_zone_side == F) {
    ad <-
      data.shots %>%
      select(shot_zone_basic, shot_area) %>%
      distinct() %>%
      arrange((shot_zone_basic)) %>%
      left_join(
        accuracy_data %>%
          group_by(shot_zone_basic) %>%
          summarise(
            attempts = n(),
            made = sum(shot_value),
            accuracy = made / attempts,
            accuracy_label = percent(accuracy)
          )
      )
    
    shot_zone_df <-
      data_frame(
        shot_zone_basic = c(
          "Above the Break 3",
          "In The Paint (Non-RA)",
          "Left Corner 3",
          "Mid-Range",
          "Restricted Area",
          "Right Corner 3"
        ),
        loc_x = c(0, 0, -209, 0, 0, 209),
        loc_y = c(280, 90, 180, 160, -25, 180),
        angle = c(0, 0, -175, 0, 0, 150),
        color = c("white", 'white', 'white', 'white', 'white', 'white')
      )
    
    accuracy_plot_data <-
      shot_zone_df %>%
      left_join(ad %>% select(accuracy, shot_area, shot_zone_basic)) %>%
      mutate(accuracy_label = accuracy %>% percent %>%
               paste(shot_zone_basic, ., sep = "\n")) %>%
      dplyr::filter(shot_area %in% shot_areas)
  } else {
    ad <-
      data.shots %>%
      select(shot_side, shot_area) %>%
      distinct() %>%
      arrange((shot_side)) %>%
      left_join(
        accuracy_data %>%
          group_by(shot_side) %>%
          summarise(
            attempts = n(),
            made = sum(shot_value),
            accuracy = made / attempts,
            accuracy_label = percent(accuracy)
          )
      )
    
    shot_side_label_df <-
      data_frame(
        shot_side = c(
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
        ),
        loc_x = c(0,
                  -105,
                  105,
                  0,
                  -90,
                  90,
                  -209,
                  0,
                  -195,
                  -95,
                  185,
                  90,
                  0,
                  200),
        loc_y = c(275,
                  250,
                  250,
                  110,
                  55,
                  55,
                  170,
                  165,
                  25,
                  170,
                  25,
                  170,
                  -25,
                  180),
        angle = c(0,
                  0,
                  0,
                  0,
                  0,
                  0,
                  -175,
                  0,
                  1.571,
                  -175,
                  4.712,
                  175,
                  0,
                  150),
        color = c(
          'white',
          'white',
          'white',
          'white',
          'white',
          'white',
          'white',
          'white',
          'white',
          'white',
          'white',
          'white',
          'white',
          'white'
        )
      )
    
    accuracy_plot_data <-
      shot_side_label_df %>%
      left_join(ad %>% select(shot_side, accuracy, shot_area)) %>%
      mutate(accuracy_label = accuracy %>% percent %>%
               paste(shot_side, ., sep = " ")) %>%
      dplyr::filter(shot_area %in% shot_areas)
    
  }
  
  performance_name <-
    summary_shots$shots[2] %>% comma(digits = 0) %>%
    paste0(
      ' FGM -- ',
      data.shots %>% nrow() %>% comma(digits = 0),
      ' FGA --  ',
      summary_shots$shots[2] / data.shots %>% nrow * 100 %>% digits(2),
      '% FG PCT'
    )
  
  title_df <-
    data_frame(
      loc_x = c(-200, -200, 0, 0, 0, 0, 0),
      loc_y = c(385, 370, 385, 370, 355, 340, 325),
      color = c("red", "red", "black", "black", "black", "black", "black"),
      label = c(
        "Authored by",
        author,
        team_label,
        line_2_label,
        line_3_label,
        line_4_label,
        performance_name
      )
    )
  
  data.shots %<>%
    mutate(url.photo = 'http://stats.nba.com/media/players/230x185/' %>% paste0(id.player, '.png'))
  
  names(data.shots) %<>%
    gsub('\\.', '\\_', .)
  
  aspect <-
    993 / 1155
  
  tools <-
    c("reset",
      'box_select',
      "crosshair",
      "box_zoom")
  
  court <-
    'https://raw.githubusercontent.com/weinfz/nba_shot_value_charts/master/court.png'
  
  html_point_hover <-
    '<div>
  <div>
  <img src="@url_photo" height="30.833" alt="@url_photo" width="38.33" style="float: left; margin: 0px 15px 15px 0px;" border="2"></img>
  </div>
  <div>
  <span style="font-size: 12px; font-weight: bold;">@name_player</span>
  </div>
  <div>
  <span style="font-size: 10px; font-weight: bold;">Shot Distance:</span>
  <span style="font-size: 10px">@shot_distance</span>
  </div>
  <div>
  <span style="font-size: 10px; font-weight: bold;">Time of Game:</span>
  <span style="font-size: 10px">@time_game</span>
  </div>
  <div>
  <span style="font-size: 10px; font-weight: bold;">Shot Made:</span>
  <span style="font-size: 10px">@is_shot_made</span>
  </div>
  <div>
  <span style="font-size: 10px; font-weight: bold;">Shot Type:</span>
  <span style="font-size: 10px">@action_type</span>
  </div>
  <div>
  <span style="font-size: 10px; font-weight: bold;">Quarter:</span>
  <span style="font-size: 10px">@period</span>
  </div>
  <div>
  <span style="font-size: 10px; font-weight: bold;">Minute:</span>
  <span style="font-size: 10px">@minute</span>
  </div>
  </div>
  '
  if (plot_hex == T) {
    p <-
      figure(
        width = 600,
        height = 600 * aspect,
        padding_factor = 1,
        xgrid = F,
        ygrid = F,
        tools = tools,
        xlab = NULL,
        ylab = NULL,
        xaxes = F,
        yaxes = F,
        ylim = c(-45, 400),
        xlim = c(-250, 250),
        toolbar_location = "right"
      ) %>%
      ly_image_url(
        x = -270,
        y = -50,
        url = court,
        w = 535,
        h = 465,
        anchor = 'bottom_left'
      ) %>%
      ly_image_url(
        x = 250,
        y = 400,
        url = url.team.logo,
        w = 512 * .15,
        h = 512 * .15,
        anchor = 'top_right'
      ) %>%
      ly_hexbin(
        loc_x,
        loc_y,
        shape = 1,
        data = data.shots,
        alpha = .35,
        palette = "Spectral10",
        hover = '',
        line = FALSE
      ) %>%
      ly_points(
        loc_x,
        loc_y,
        data =
          data.shots,
        color = "black",
        fill_color = "black",
        glyph = 20,
        alpha = .75,
        size = 2,
        hover = html_point_hover
      ) %>%
      ly_points(
        loc_x,
        loc_y,
        data = data.shots %>%
          dplyr::filter(shot_made == "NO"),
        color = "red",
        fill_color = "red",
        glyph = 4,
        alpha = .75,
        size = 2,
        hover = html_point_hover
      ) %>%
      ly_text(
        x = loc_x,
        y = loc_y,
        text = accuracy_label,
        angle = angle,
        align = "center",
        baseline = "middle",
        font_size = "8pt",
        color = color,
        font_style = "bold",
        data = accuracy_plot_data
      ) %>%
      ly_text(
        x = loc_x,
        y = loc_y,
        text = label,
        align = "center",
        baseline = "middle",
        font_size = "7pt",
        color = color,
        font_style = "bold",
        data = title_df
      )
  } else {
    p <-
      figure(
        width = 600,
        height = 600 * aspect,
        padding_factor = 1,
        xgrid = F,
        ygrid = F,
        tools = tools,
        xlab = NULL,
        ylab = NULL,
        xaxes = F,
        yaxes = F,
        ylim = c(-45, 400),
        xlim = c(-250, 250),
        toolbar_location = "right"
      ) %>%
      ly_image_url(
        x = -270,
        y = -50,
        url = court,
        w = 535,
        h = 465,
        anchor = 'bottom_left'
      ) %>%
      ly_image_url(
        x = 250,
        y = 400,
        url = url.team.logo,
        w = 512 * .15,
        h = 512 * .15,
        anchor = 'top_right'
      ) %>%
      ly_points(
        loc_x,
        loc_y,
        data =
          data.shots,
        color = "black",
        fill_color = "black",
        glyph = 20,
        alpha = .75,
        size = 2,
        hover = html_point_hover
      ) %>%
      ly_points(
        loc_x,
        loc_y,
        data = data.shots %>%
          dplyr::filter(shot_made == "NO"),
        color = "red",
        fill_color = "red",
        glyph = 4,
        alpha = .75,
        size = 2,
        hover = html_point_hover
      ) %>%
      ly_text(
        x = loc_x,
        y = loc_y,
        text = accuracy_label,
        angle = angle,
        align = "center",
        baseline = "middle",
        font_size = "8pt",
        color = color,
        font_style = "bold",
        data = accuracy_plot_data
      ) %>%
      ly_text(
        x = loc_x,
        y = loc_y,
        text = label,
        align = "center",
        baseline = "middle",
        font_size = "7pt",
        color = color,
        font_style = "bold",
        data = title_df
      )
  }
  p
}