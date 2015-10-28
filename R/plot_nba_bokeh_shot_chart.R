#' Get's NBA player's season shot chart with specific parameters
#'
#' @param player
#' @param year_season_end
#' @param plot_hex
#' @param author
#' @param use_shot_zone_side
#' @param season_type
#' @param shot_areas
#' @param shot_types
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
#'
#' @return
#' @export
#'
#' @examples  plot_nba_player_bokeh_shotchart( player = "Thomas Robinson", exclude_backcourt = T, shots_type = c("Dunk", "Layup", "Hook", "Jump Shot"))
#
#'
#' shot_areas = c('Paint', 'Mid-Range'), use_shot_zone_side = T, season_segment = 'Post All-Star',
#' year_season_end = 2015, author = "Alex Bresler")
plot_nba_player_bokeh_shotchart <- function(player,
                                       year_season_end = 2015,
                                       plot_hex = T,
                                       author = "Alex Bresler",
                                       use_shot_zone_side = T,
                                       season_type = "Regular Season",
                                       # Regular Season, Pre Season, Playoffs, All Star
                                       shot_areas = c('Three Point', "Paint", "Mid-Range"),
                                       shots_type = c('Dunk', 'Layup', 'Jump Shot', "Fadeaway", "Bank", "Tip",
                                                      'Hook'),
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
                                       exclude_backcourt = T
) {
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
  p <-
    player

  yse <-
    year_season_end
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
    position
  season_seg <-
    season_segment

  data <-
    get_player_season_shot_data(player = p,year_season_end = yse,use_shot_zone_side = uid,
                                 season_type = st,shots_type = shot_t,
                                 shot_areas = sa,
                                 vs_conference = vs_conf,vs_division = vs_div,
                                 quarter_range = q_range, minute_range = min_range,
                                 against_team = ag_team, game_location = game_loc,
                                 game_month = game_mon,outcome = out,position = pos,
                                 season_segment = season_seg, exclude_backcourt = eb,
                                 return_message = T
    )

  year_season_start <-
    yse - 1

  id.season <-
    year_season_start %>%
    paste(yse %>% substr(start = 3, stop = 4),
          sep = "-")


  data.shots <-
    data$shots %>%
    tbl_df

  data.shots %<>%
    mutate(shot_made = ifelse(shot_made_flag == T, "YES", "NO"))

  data.shots %>%
    dplyr::filter(shot_made == 'YES')

  if(exclude_backcourt == T){
    data.shots %<>%
      dplyr::filter(!id.side %in% 'BC')
  }

  parameters <-
    data$parameters

  url.player.photo <-
    parameters$url.player.photo

  summary_shots <-
    data.shots %>%
    group_by(shot_made_flag) %>%
    summarise(shots = n())

  accuracy_data <-
    data.shots %>%
    group_by(shot_zone_basic) %>%
    mutate(shot_value = ifelse(shot_made_flag == TRUE, 1, 0))

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

  if (position %>% is.na) {
    position_label <-
      ''
  } else {
    position_label <-
      ', as a ' %>%
      paste0(position)
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

  if(shot_areas %>% length == 3){
    shot_area_label <-
      ' All Shot Areas'
  } else {
    shot_area_label <-
      ' from ' %>%
      paste0(shot_areas %>% paste0(collapse = ' & the '))
  }

  player_name <-
    player %>%
    paste0(" Shot Chart", shot_area_label)

  if(shots_type %>% length < 7){
    if(shots_type %>% length > 1){
    shot_type_label <-
      " On " %>%
      paste0(shots_type %>% paste0(collapse = ", "), " Shots")
  } else {
    shot_type_label <-
      " On " %>%
      paste0(shots_type,' Shots')
  }
  } else{
    shot_type_label <-
      ''
  }

  line_2_label <-
    id.season %>%
    paste(season_type, season_segment_label) %>%
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
           position_label,
           shot_type_label) %>%
    str_trim

  if (use_shot_zone_side == F ) {
    ad <-
      data.shots %>%
      dplyr::select(shot_zone_basic, shot_area) %>%
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
        shot_zone_basic = c("Above the Break 3", "In The Paint (Non-RA)", "Left Corner 3",
                            "Mid-Range", "Restricted Area", "Right Corner 3"),
        loc_x = c(0, 0, -209, 0, 0, 209),
        loc_y = c(280, 90, 180, 160, -25, 180),
        angle = c(0, 0, -175, 0, 0, 150),
        color = c("#1100a8", '#1100a8', 'white', '#1100a8', 'white', 'white')
      )

    accuracy_plot_data <-
      shot_zone_df %>%
      left_join(ad %>% dplyr::select(accuracy, shot_area, shot_zone_basic)) %>%
      mutate(accuracy_label = accuracy %>% percent %>%
               paste(shot_zone_basic, ., sep = "\n")) %>%
      dplyr::filter(shot_area %in% shot_areas)
  } else {
    ad <-
      data.shots %>%
      dplyr::select(shot_side, shot_area) %>%
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
          '#1100a8',
          'white',
          'white',
          'white',
          '#1100a8',
          '#1100a8',
          '#1100a8',
          '#1100a8',
          '#1100a8',
          'white',
          'white'
        )
      )

    accuracy_plot_data <-
      shot_side_label_df %>%
      left_join(ad %>% dplyr::select(shot_side, accuracy, shot_area)) %>%
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
      loc_y = c(385, 365, 385, 365, 345, 325, 305),
      color = c("red", "red", "black", "black", "black", "black", "black"),
      label = c(
        "Authored By",
        author,
        player_name,
        line_2_label,
        line_3_label,
        line_4_label,
        performance_name
      )
    )

  data.shots %<>%
    mutate(url_photo = 'http://stats.nba.com/media/players/230x185/' %>% paste0(id.player, '.png'))

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
        padding_factor = 0,
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
        font_size = "9pt",
        color = color,
        font_style = "bold",
        data = title_df
      ) %>%
      ly_image_url(
        x = 250,
        y = 400,
        url = url.player.photo,
        w = 230 * .45,
        h = 185 * .45,
        anchor = 'top_right'
      ) %>%
      ly_hexbin(
        loc_x,
        loc_y,
        shape = 1,
        data = data.shots,
        alpha = .35,
        palette = "Spectral11",
        hover = FALSE,
        line = FALSE
      ) %>%
      ly_points(
        loc_x,
        loc_y,
        data =
          data.shots %>%
          dplyr::filter(shot_made == "YES"),
        color = "black",
        fill_color = "black",
        glyph = 20,
        alpha = 1,
        size = 2.5,
        hover =
          html_point_hover
      ) %>%
      ly_points(
        loc_x,
        loc_y,
        data = data.shots %>%
          dplyr::filter(shot_made == "NO"),
        color = "red",
        fill_color = "red",
        glyph = 4,
        alpha = 1,
        size = 2.5,
        hover = html_point_hover
      )
  } else {
    p <-
      figure(
        width = 600,
        height = 600 * aspect,
        padding_factor = 0,
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
        font_size = "9pt",
        color = color,
        font_style = "bold",
        data = title_df
      ) %>%
      ly_image_url(
        x = 250,
        y = 400,
        url = url.player.photo,
        w = 230 * .45,
        h = 185 * .45,
        anchor = 'top_right'
      ) %>%
      ly_points(
        loc_x,
        loc_y,
        data = data.shots %>%
          dplyr::filter(shot_made == "YES"),
        color = "black",
        fill_color = "black",
        glyph = 20,
        alpha = 1,
        size = 2.5,
        hover = html_point_hover
      ) %>%
      ly_points(
        loc_x,
        loc_y,
        data =
          data.shots %>%
          dplyr::filter(shot_made == "NO"),
        color = "red",
        fill_color = "red",
        glyph = 4,
        alpha = 1,
        size = 2.5,
        hover = html_point_hover
      )
  }
  p
}
