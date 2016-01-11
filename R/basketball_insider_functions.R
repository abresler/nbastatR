packages <- #need all of these installed including some from github
  c(
    'dplyr',
    'magrittr',
    'jsonlite',
    'tidyr',
    'httr',
    'rvest',
    'purrr',
    'data.table',
    'stringr',
    'lubridate',
    'tidyr'
  )
options(warn = -1)
lapply(packages, library, character.only = T)

get_basketball_insider_team_ids <- function() {
  data <-
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
      slug.team = c(
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
    left_join(data_frame(
      id.bi = c(
        1,
        2,
        3,
        4,
        5,
        6,
        7,
        8,
        9,
        10,
        11,
        12,
        13,
        14,
        15,
        16,
        17,
        18,
        19,
        20,
        21,
        22,
        23,
        24,
        25,
        26,
        27,
        28,
        29,
        30
      ),
      slug.team = c(
        "BOS",
        "BKN",
        "NYK",
        "PHI",
        "TOR",
        "GSW",
        "LAC",
        "LAL",
        "PHO",
        "SAC",
        "CHI",
        "CLE",
        "DET",
        "IND",
        "MIL",
        "DAL",
        "HOU",
        "MEM",
        "NOP",
        "SAS",
        "ATL",
        "CHA",
        "MIA",
        "ORL",
        "WAS",
        "DEN",
        "MIN",
        "OKC",
        "POR",
        "UTA"
      )
    )) %>%
    mutate(
      url.bi.salary = paste0(
        'http://hw-files.com/tools/salaries/salaries_widget_new.php?team_id=',
        id.bi
      )
    )
  return(data)
}

get_team_salary_cap_table <-
  function(team_name = "Brooklyn Nets",
           team_slug = NA,
           assume_player_opt_out = T,
           assume_team_doesnt_exercise = T,
           return_message = T) {
    team_name_df <-
      get_basketball_insider_team_ids()

    teams <-
      team_name_df$team %>%
      str_to_lower()

    if (!team_name %>% is.na &
        (!team_name %>% str_to_lower() %in% teams)) {
      team_message <-
        paste0(team_name_df$team, collapse = '\n')

      "Team can only be:\n" %>%
        paste0(team_message) %>%
        stop()
    }

    if (!team_slug %>% is.na &
        (team_slug %>% str_to_lower() %in% team_name_df$slug.team)) {
      team_message <-
        paste0(team_name_df$slug.team, collapse = '\n')

      "Team slugs can only be:\n" %>%
        paste0(team_message) %>%
        stop()
    }

    if (team_slug %>% is.na()) {
      url <-
        team_name_df %>%
        dplyr::filter(team == team_name) %>%
        .$url.bi.salary
    } else {
      url <-
        team_name_df %>%
        dplyr::filter(slug.team == team_slug) %>%
        .$url.bi.salary

      team_name <-
        team_name_df %>%
        dplyr::filter(slug.team == team_slug) %>%
        .$team
    }

    types <-
      c(
        'Player Option',
        'Team Option',
        'Qualifying Offer',
        'Non-Guaranteed',
        'Early Termination',
        'Not With Team',
        'Qualifying Offer'
      )
    colors <-
      c('#0486B0',
        '#990099',
        '#009933',
        '#FF3300',
        '#0000FF',
        '#808080',
        '#009900')

    color_df <-
      data_frame(color = colors,
                 type = types)

    page <-
      url %>%
      html()

    salary_table <-
      page %>%
      html_table(header = F) %>%
      data.frame %>%
      tbl_df

    names_salary_table <-
      c(
        'name.player',
        salary_table %>%
          slice(1) %>%
          gather(item, value) %>%
          slice(-1) %>%
          .$value
      )

    names(salary_table) <-
      names_salary_table
    salary_table %<>%
      slice(-1)

    salary_table %<>%
      dplyr::filter(!name.player %like% 'Total') %>%
      mutate(team = team_name) %>%
      dplyr::select(team, everything())

    salary_data <-
      salary_table %>%
      separate(name.player,
               into = c('name.player', 'status'),
               sep = '\\(') %>%
      mutate(
        name.player = name.player %>% str_trim(),
        status = status %>% str_replace("\\)", ''),
        status = ifelse(status %>% is.na, "current roster", status),
        is.on_roster = ifelse(status == "current roster", T, F)
      ) %>%
      dplyr::select(team, name.player:status, is.on_roster, everything()) %>%
      gather(id.season, value, -c(team, name.player, status, is.on_roster)) %>%
      mutate(value = value %>% extract_numeric) %>%
      dplyr::select(team, id.season, name.player, status, is.on_roster, value) %>%
      dplyr::filter(!value %>% is.na())


    year_1_salary <-
      page %>%
      html_nodes('tbody td:nth-child(2)') %>%
      html_text() %>%
      extract_numeric()

    year_1_salary <-
      year_1_salary[1:(year_1_salary %>% length() - 1)] %>%
      .[!is.na(.)]

    year_1_salary_color <-
      page %>%
      html_nodes('tbody td:nth-child(2) span') %>%
      html_attr('style') %>%
      str_replace('\\\"color:', '') %>%
      gsub("[^A-Z a-z#0-9]", '', .)

    year_1_df <-
      data_frame(id.season = names_salary_table[2],
                 color = year_1_salary_color,
                 value = year_1_salary)

    year_2_salary <-
      page %>%
      html_nodes('tbody td:nth-child(3)') %>%
      html_text() %>%
      extract_numeric()

    year_2_salary <-
      year_2_salary[1:(year_2_salary %>% length() - 1)] %>%
      .[!is.na(.)]

    year_2_salary_color <-
      page %>%
      html_nodes('tbody td:nth-child(3) span') %>%
      html_attr('style') %>%
      str_replace('\\\"color:', '') %>%
      gsub("[^A-Z a-z#0-9]", '', .)

    year_2_df <-
      data_frame(id.season = names_salary_table[3],
                 color = year_2_salary_color,
                 value = year_2_salary)

    year_3_salary <-
      page %>%
      html_nodes('tbody td:nth-child(4)') %>%
      html_text() %>%
      extract_numeric()

    year_3_salary <-
      year_3_salary[1:(year_3_salary %>% length() - 1)] %>%
      .[!is.na(.)]

    year_3_salary_color <-
      page %>%
      html_nodes('tbody td:nth-child(4) span') %>%
      html_attr('style') %>%
      str_replace('\\\"color:', '') %>%
      gsub("[^A-Z a-z#0-9]", '', .)

    year_3_df <-
      data_frame(id.season = names_salary_table[4],
                 color = year_3_salary_color,
                 value = year_3_salary)

    year_4_salary <-
      page %>%
      html_nodes('tbody td:nth-child(5)') %>%
      html_text() %>%
      extract_numeric()
    year_4_salary <-
      year_4_salary[1:(year_4_salary %>% length() - 1)] %>%
      .[!is.na(.)]

    year_4_salary_color <-
      page %>%
      html_nodes('tbody td:nth-child(5) span') %>%
      html_attr('style') %>%
      str_replace('\\\"color:', '') %>%
      gsub("[^A-Z a-z#0-9]", '', .)

    year_4_df <-
      data_frame(id.season = names_salary_table[5],
                 color = year_4_salary_color,
                 value = year_4_salary)

    year_5_salary <-
      page %>%
      html_nodes('tbody td:nth-child(6)') %>%
      html_text() %>%
      extract_numeric()

    if ( year_5_salary[1:(year_5_salary %>% length() - 1)] %>%
        .[!is.na(.)] %>% length == 0 ) {
      year_5_salary <-
        NA

      year_5_salary_color <-
        NA
    } else {
      year_5_salary_color <-
        page %>%
        html_nodes('tbody td:nth-child(6) span') %>%
        html_attr('style') %>%
        str_replace('\\\"color:', '') %>%
        gsub("[^A-Z a-z#0-9]", '', .)

      year_5_salary <-
      year_5_salary[1:(year_5_salary %>% length() - 1)] %>%
      .[!is.na(.)]
    }

    year_5_df <-
      data_frame(id.season = names_salary_table[6],
                 color = year_5_salary_color,
                 value = year_5_salary)

    contract_color_df <-
      year_1_df %>%
      bind_rows(year_2_df) %>%
      bind_rows(year_3_df) %>%
      bind_rows(year_4_df) %>%
      bind_rows(year_5_df)

    all_data <-
      salary_data %>%
      left_join(contract_color_df) %>%
      left_join(color_df) %>%
      mutate(
        is.non.guaranteed = ifelse(type == 'Non-Guaranteed', T, F),
        is.non.guaranteed = ifelse(is.non.guaranteed %>% is.na(), F, is.non.guaranteed),
        is.team_option = ifelse(type == 'Team Option', T, F),
        is.team_option = ifelse(is.team_option %>% is.na(), F, is.team_option),
        is.player_option = ifelse(type == 'Player Option', T, F),
        is.player_option = ifelse(is.player_option %>% is.na(), F, is.player_option)
      ) %>%
      dplyr::select(
        id.season,
        team:is.on_roster,
        is.non.guaranteed,
        is.team_option,
        is.player_option,
        type,
        value
      )

    if (assume_player_opt_out == T) {
      player_final_seasons <-
        all_data %>%
        dplyr::filter(is.player_option  == F)
    }  else {
      player_final_seasons <-
        all_data
    }


    if (assume_team_doesnt_exercise == T) {
      player_final_seasons <-
        player_final_seasons %>%
        dplyr::filter(is.team_option == F)
    }
    player_final_seasons %<>%
      group_by(name.player) %>%
      dplyr::select(name.player, id.season, is.on_roster) %>%
      dplyr::filter(id.season == max(id.season)) %>%
      mutate(
        is.final_season = T,
        is.final_season = ifelse(is.on_roster == F, NA, is.on_roster)
      ) %>%
      distinct()

    all_data %<>%
      left_join(player_final_seasons) %>%
      dplyr::select(id.season:status, is.final_season, everything())

    all_data %<>%
      mutate(name.player = name.player %>% gsub("[^A-Z a-z # ' 0-9]", '', .)) %>%
      distinct()

    if (return_message == T) {
      "You got salary data for " %>%
        paste0(team_name) %>%
        message()
    }
    return(all_data)

  }

get_all_team_salaries <-
  function(assume_player_opt_out = T,
           assume_team_doesnt_exercise = T) {
    apo <-
      assume_player_opt_out

    atde <-
      assume_team_doesnt_exercise
    all_teams <-
      c(
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
      )

    all_salaries <-
      all_teams %>%
      purrr::map(
        function(x)
          get_team_salary_cap_table(
            team_name = x,
            assume_player_opt_out = apo,
            assume_team_doesnt_exercise = atde,
            return_message = F
          )
      ) %>%
      compact %>%
      bind_rows()

    return(all_salaries)
  }
