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
get_hoopshype_teams_ids <-
  function() {
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
        ),
        team.hoopshype =
          c(
            "Atlanta",
            "Boston",
            "Brooklyn",
            "Charlotte",
            "Chicago",
            "Cleveland",
            "Dallas",
            "Denver",
            "Detroit",
            "Golden State",
            "Houston",
            "Indiana",
            "LA Clippers",
            "LA Lakers",
            "Memphis",
            "Miami",
            "Milwaukee",
            "Minnesota",
            "New Orleans",
            "New York",
            "Oklahoma City",
            "Orlando",
            "Philadelphia",
            "Phoenix",
            "Portland",
            "Sacramento",
            "San Antonio",
            "Toronto",
            "Utah",
            "Washington"
          ),
        url.team.salary = c(
          "http://hoopshype.com/salaries/atlanta_hawks/",
          "http://hoopshype.com/salaries/boston_celtics/",
          "http://hoopshype.com/salaries/brooklyn_nets/",
          "http://hoopshype.com/salaries/charlotte_hornets/",
          "http://hoopshype.com/salaries/chicago_bulls/",
          "http://hoopshype.com/salaries/cleveland_cavaliers/",
          "http://hoopshype.com/salaries/dallas_mavericks/",
          "http://hoopshype.com/salaries/denver_nuggets/",
          "http://hoopshype.com/salaries/detroit_pistons/",
          "http://hoopshype.com/salaries/golden_state_warriors/",
          "http://hoopshype.com/salaries/houston_rockets/",
          "http://hoopshype.com/salaries/indiana_pacers/",
          "http://hoopshype.com/salaries/los_angeles_clippers/",
          "http://hoopshype.com/salaries/los_angeles_lakers/",
          "http://hoopshype.com/salaries/memphis_grizzlies/",
          "http://hoopshype.com/salaries/miami_heat/",
          "http://hoopshype.com/salaries/milwaukee_bucks/",
          "http://hoopshype.com/salaries/minnesota_timberwolves/",
          "http://hoopshype.com/salaries/new_orleans_pelicans/",
          "http://hoopshype.com/salaries/new_york_knicks/",
          "http://hoopshype.com/salaries/oklahoma_city_thunder/",
          "http://hoopshype.com/salaries/orlando_magic/",
          "http://hoopshype.com/salaries/philadelphia_76ers/",
          "http://hoopshype.com/salaries/phoenix_suns/",
          "http://hoopshype.com/salaries/portland_trail_blazers/",
          "http://hoopshype.com/salaries/sacramento_kings/",
          "http://hoopshype.com/salaries/san_antonio_spurs/",
          "http://hoopshype.com/salaries/toronto_raptors/",
          "http://hoopshype.com/salaries/utah_jazz/",
          "http://hoopshype.com/salaries/washington_wizards/"
        )


      )
    return(data)
  }

get_hoopshype_teams_summary_salary_table <-
  function(return_wide = T,
           return_message = T) {
    url <-
      'http://hoopshype.com/salaries/'
    page <-
      url %>%
      html()

    table_df <-
      page %>%
      html_table(header = T) %>%
      data.frame %>%
      tbl_df

    team_name_df <-
      get_hoopshype_teams_ids()

    salary_table_df <-
      table_df %>%
      dplyr::rename(team.hoopshype = Team) %>%
      left_join(team_name_df) %>%
      dplyr::select(team, slug.team, everything()) %>%
      dplyr::select(-c(Var.1, team.hoopshype))

    salary_data <-
      salary_table_df %>%
      gather(id.season, value, -c(team, slug.team, url.team.salary)) %>%
      mutate(
        id.season = id.season %>% str_replace('\\.', '\\-') %>% str_replace("X", '') %>% factor(ordered = T),
        value = value %>% extract_numeric()
      )

    if (return_wide == T) {
      salary_data %>%
        mutate(id.season = "X" %>% paste0(id.season)) %>%
        spread(id.season, value)
    }

    if (return_message == T) {
      "You got NBA salary data" %>%
        message
    }

    return(salary_data)

  }

get_hoopshype_team_salary_table <-
  function(team_name = "Charlotte Hornets",
           team_slug = NA,
           assume_player_opt_out = T,
           assume_team_doesnt_exercise = T,
           return_message = T) {
    team_name_df <-
      get_hoopshype_teams_ids()

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
        !(team_slug %>% str_to_lower() %in% team_name_df$slug.team)) {
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
        .$url.team.salary
    } else {
      url <-
        team_name_df %>%
        dplyr::filter(slug.team == team_slug) %>%
        .$url.team.salary

      team_name <-
        team_name_df %>%
        dplyr::filter(slug.team == team_slug) %>%
        .$team
    }

    types <-
      c(NA,
        'Player Option',
        'Team Option',
        'Qualifying Offer',
        'Amnestied')
    colors <-
      c('black',
        'rgb(4, 134, 176)',
        'rgb(255, 0, 0)',
        'rgb(0, 153, 0)',
        'rgb(168, 0, 212)')
    color_df <-
      data_frame(color = colors,
                 type = types)

    page <-
      url %>%
      html()

    name.player <-
      page %>%
      html_nodes('#content-container tbody .name') %>%
      html_text %>%
      str_trim

    url.hoopshype.player <-
      page %>%
      html_nodes('#content-container .name a') %>%
      html_attr('href')

    url.hoopshype.player <-
      url.hoopshype.player[!url.hoopshype.player %>% is.na]

    players_urls_df <-
      data_frame(
        name.player.lower = url.hoopshype.player %>%
          str_replace('http://hoopshype.com/player/', '') %>%
          str_replace('\\/', '') %>%
          str_replace('\\-', ' '),
        url.hoopshype.player
      )

    table_headers <-
      c('name.player',
        page %>%
          html_nodes('#content-container td') %>%
          html_text() %>%
          .[2:7])

    table_headers %<>%
      str_replace('\\/', '-')

    salary.1 <-
      page %>%
      html_nodes('tbody .hh-salaries-sorted') %>%
      html_text() %>%
      extract_numeric()

    salary.1.colors <-
      page %>%
      html_nodes('tbody .hh-salaries-sorted') %>%
      html_attr('style') %>%
      str_replace("color:", '')

    salary.2 <-
      page %>%
      html_nodes('tbody .hh-salaries-sorted+ td') %>%
      html_text %>%
      extract_numeric()

    salary.2.colors <-
      page %>%
      html_nodes('tbody .hh-salaries-sorted+ td') %>%
      html_attr('style') %>%
      str_replace("color:", '')

    salary.3 <-
      page %>%
      html_nodes('tbody td:nth-child(4)') %>%
      html_text %>%
      extract_numeric()

    salary.3.colors <-
      page %>%
      html_nodes('tbody td:nth-child(4)') %>%
      html_attr('style') %>%
      str_replace("color:", '')

    salary.4 <-
      page %>%
      html_nodes('tbody td:nth-child(5)') %>%
      html_text %>%
      extract_numeric()

    salary.4.colors <-
      page %>%
      html_nodes('tbody td:nth-child(5)') %>%
      html_attr('style') %>%
      str_replace("color:", '')

    salary.5 <-
      page %>%
      html_nodes('tbody td:nth-child(6)') %>%
      html_text %>%
      extract_numeric()

    salary.5.colors <-
      page %>%
      html_nodes('tbody td:nth-child(6)') %>%
      html_attr('style') %>%
      str_replace("color:", '')

    salary.6 <-
      page %>%
      html_nodes('tbody td:nth-child(7)') %>%
      html_text %>%
      extract_numeric()

    salary.6.colors <-
      page %>%
      html_nodes('tbody td:nth-child(7)') %>%
      html_attr('style') %>%
      str_replace("color:", '')

    player_salary_df <-
      data_frame(name.player,
                 salary.1,
                 salary.2,
                 salary.3,
                 salary.4,
                 salary.5,
                 salary.6)
    names(player_salary_df) <-
      table_headers

    player_contract_df <-
      data_frame(
        name.player,
        salary.1.colors,
        salary.2.colors,
        salary.3.colors,
        salary.4.colors,
        salary.5.colors,
        salary.6.colors
      )

    names(player_contract_df) <-
      table_headers

    salary_data <-
      player_salary_df %>%
      gather(id.season, value, -c(name.player)) %>%
      mutate(id.season = id.season %>% factor(ordered = T))

    contract_data <-
      player_contract_df %>%
      gather(id.season, color, -c(name.player)) %>%
      mutate(id.season = id.season %>% factor(ordered = T)) %>%
      left_join(color_df) %>%
      dplyr::select(-color) %>%
      mutate(
        is.player_option = ifelse(type == "Player Option", T, F),
        is.player_option = ifelse(is.player_option == T, T, F),
        is.team_option = ifelse(type == "Team Option", T, F),
        is.team_option = ifelse(is.team_option == T, T, F)
      )

    all_data <-
      salary_data %>%
      left_join(contract_data) %>%
      dplyr::select(id.season,
                    name.player:is.team_option,
                    type:is.team_option,
                    value)

    all_data %<>%
      dplyr::filter(value > 0)

    if (assume_player_opt_out == T) {
      player_final_seasons <-
        all_data %>%
        dplyr::filter(is.player_option %>% is.na)
    }  else {
      player_final_seasons <-
        all_data
    }


    if (assume_team_doesnt_exercise == T) {
      player_final_seasons <-
        player_final_seasons %>%
        dplyr::filter(is.team_option %>% is.na)
    }
    player_final_seasons %<>%
      group_by(name.player) %>%
      dplyr::select(name.player, id.season) %>%
      dplyr::filter(id.season == max(id.season)) %>%
      mutate(is.final_season = T)

    all_data %<>%
      left_join(player_final_seasons) %>%
      mutate(team = team_name,
             name.player.lower = name.player %>% str_to_lower()) %>%
      left_join(players_urls_df) %>%
      dplyr::select(team, id.season:name.player, is.final_season, everything()) %>%
      dplyr::select(-name.player.lower) %>%
      mutate(is.final_season = ifelse(is.final_season %>% is.na(), F, T))

    if (return_message == T) {
      "You got " %>%
        paste0(team_name, " Hoopshype salary data") %>%
        message()
    }
    return(all_data)
  }

get_all_hoopshype_team_salaries <-
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
          get_hoopshype_team_salary_table(
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
