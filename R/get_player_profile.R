packages <- #need all of these installed including some from github
  c('dplyr',
    'magrittr',
    'jsonlite',
    'tidyr',
    'purrr',
    'stringr',
    'lubridate',
    'tidyr')
options(warn = -1)
players <-
  get_nba_players_ids()

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

weight_in_lbs <- function(x){

}

get_player_profile <- function(player,
                               id.player = NULL,
                               include_headline_stat = T,
                               return_message = T) {
  if (id.player %>% is.null()) {
    id.player <-
      players %>%
      dplyr::filter(name.player == player) %>%
      .$id.player

  } else {
    id <-
      id.player
    player <-
      players %>%
      dplyr::filter(id.player == id) %>%
      .$name.player
  }

  active_player <-
    players %>%
    dplyr::filter(id.player == id) %>%
    .$is.active

  ## Build URL
  url_json <-
    'http://stats.nba.com/stats/commonplayerinfo?LeagueID=00&PlayerID=' %>%
    paste0(id.player)

  json_data <-
    url_json %>%
    fromJSON(simplifyDataFrame = T, flatten = T)

  headers_df <-
    get_headers()

  headers <-
    json_data$resultSets$headers[1] %>%
    unlist %>%
    str_to_lower()

  data <-
    json_data$resultSets$rowSet[1] %>%
    data.frame %>%
    tbl_df

  actual_names <-
    1:length(headers) %>%
    purrr::map(
      function(x)
        data_frame(
          name.actual =
            headers_df %>%
            mutate(name.nba = name.nba %>% str_to_lower) %>%
            dplyr::filter(name.nba == headers[x]) %>%
            .$name.actual
        )
    ) %>%
    bind_rows()

  names(data) <-
    actual_names$name.actual

  data %<>%
    separate(date.birth, into = c('date.birth', 'ignore'), sep = 'T') %>%
    dplyr::select(-c(name.first, name.last, name.last.display, name.middle.display, gp.flag, ignore, status.roster)) %>%
    mutate(
      is.rookie = ifelse(years.experience == "R", T, F),
      years.experience = years.experience %>% str_replace("R", 0) %>% as.numeric(),
      id.team = id.team %>% as.numeric,
      jersey = jersey %>% as.numeric,
      height.inches = height %>% lapply(height_in_inches) %>% unlist,
      weight.lbs = weight.lbs %>% as.numeric,
      date.birth = date.birth %>% ymd %>% as.Date(),
      id.player = id.player %>% as.numeric,
      is.active_player = active_player,
      team = city.team %>% paste(name.team),
      bmi = (weight.lbs / height.inches ^ 2) * 703,
      has.d_league_data = has.d_league_data %>% str_detect("Y")
    ) %>%
    dplyr::select(name.player, id.player, is.rookie, is.active_player, team, position, jersey, height, height.inches, weight.lbs, bmi, years.experience, year.from, year.to, everything())

  if (include_headline_stat == T) {
    headers <-
      json_data$resultSets$headers[2] %>%
      unlist %>%
      str_to_lower()

    stat <-
      json_data$resultSets$rowSet[2] %>%
      data.frame %>%
      tbl_df

    actual_names <-
      1:length(headers) %>%
      purrr::map(
        function(x)
          data_frame(
            name.actual =
              headers_df %>%
              mutate(name.nba = name.nba %>% str_to_lower) %>%
              dplyr::filter(name.nba == headers[x]) %>%
              .$name.actual
          )
      ) %>%
      bind_rows()

    names(stat) <-
      actual_names$name.actual

    stat %<>%
      mutate_each_(funs(extract_numeric), vars =
                     stat %>%
                     dplyr::select(id.player, pts:pie) %>% names) %>%
      rename(id.season.recent = id.season)

    names(stat)[4:length(names(stat))] %<>%
      paste0('.per_game.recent')

    data <-
      stat %>%
      left_join(data)
  }


  if (return_message == T) {
    "Congrats, you got " %>%
      paste0(player, "'s profile data") %>%
      message()
  }
  return(data)
}
