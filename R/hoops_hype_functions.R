
get_hoopshype_teams_ids <-
  function() {
    data <-
      data_frame(
        nameTeam = c(
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
        slugTeam = c(
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
        teamHoopsHype =
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
        urlTeamSalaryHoopsHype = c(
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

#' Get HoopsHype NBA teams salaries
#'
#' @param spread_data if \code{TRUE} spreads data
#' @param return_message if \code{TRUE} returns a message
#'
#' @return a \code{data_frame()}
#' @export
#' @import dplyr purrr stringr rvest xml2 tidyr readr
#' @examples
#' get_hoops_hype_teams_salary_summary(spread_data = TRUE)
get_hoops_hype_teams_salary_summary <-
  function(spread_data = F,
           return_message = T) {
    url <-
      'http://hoopshype.com/salaries/'
    page <-
      url %>%
      read_html()

    table_df <-
      page %>%
      html_table(header = T) %>%
      data.frame() %>%
      tbl_df()

    team_name_df <-
      get_hoopshype_teams_ids()

    salary_table_df <-
      table_df %>%
      dplyr::rename(teamHoopsHype = Team) %>%
      left_join(team_name_df) %>%
      dplyr::select(nameTeam, slugTeam, everything()) %>%
      dplyr::select(-c(Var.1, teamHoopsHype)) %>%
      suppressMessages()


    salary_table_df <-
      salary_table_df %>%
      mutate_at(salary_table_df %>% select(matches("X")) %>% names(),
                funs(. %>% readr::parse_number()))

    salary_data <-
      salary_table_df %>%
      gather(slugSeason,
             amountSalary,
             -c(nameTeam, nameTeam, slugTeam, urlTeamSalaryHoopsHype)) %>%
      mutate(
        slugSeason = slugSeason %>% str_replace('\\.', '\\-') %>% str_replace("X", '') %>% factor(ordered = T),
        amountSalary = amountSalary %>% readr::parse_number()
      )

    if (spread_data) {
      salary_data <-
        salary_data %>%
        mutate(slugSeason = "X" %>% paste0(slugSeason)) %>%
        spread(slugSeason, amountSalary)
    }

    if (return_message) {
      "You got Hoops Hype NBA Team Salary Data" %>% message()
    }
    salary_data

  }

parse_hoops_hype_salary_url <-
  function(url = "http://hoopshype.com/salaries/charlotte_hornets/",
           assume_player_opt_out = T,
           assume_team_doesnt_exercise = T) {
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
      read_html()

    namePlayer <-
      page %>%
      html_nodes('#content-container tbody .name') %>%
      html_text() %>%
      str_trim()

    urlHoopsHypePlayer <-
      page %>%
      html_nodes('#content-container .name a') %>%
      html_attr('href')

    urlHoopsHypePlayer <-
      urlHoopsHypePlayer[!urlHoopsHypePlayer %>% is.na()]

    players_urls_df <-
      data_frame(urlHoopsHypePlayer) %>%
      mutate(
        namePlayerLower = urlHoopsHypePlayer %>% str_replace_all("http://hoopshype.com/player/|/salary/", "") %>% str_replace_all("\\-", " ")
      )

    table_headers <-
      c('namePlayer',
        page %>%
          html_nodes('#content-container td') %>%
          html_text() %>%
          .[2:7])

    table_headers <-
      table_headers %>%
      str_replace('\\/', '-')

    salary.1 <-
      page %>%
      html_nodes('tbody .hh-salaries-sorted') %>%
      html_text() %>%
      readr::parse_number() %>%
      suppressWarnings()

    salary.1.colors <-
      page %>%
      html_nodes('tbody .hh-salaries-sorted') %>%
      html_attr('style') %>%
      str_replace("color:", '')

    salary.2 <-
      page %>%
      html_nodes('tbody .hh-salaries-sorted+ td') %>%
      html_text() %>%
      readr::parse_number() %>%
      suppressWarnings()

    salary.2.colors <-
      page %>%
      html_nodes('tbody .hh-salaries-sorted+ td') %>%
      html_attr('style') %>%
      str_replace("color:", '')

    salary.3 <-
      page %>%
      html_nodes('tbody td:nth-child(4)') %>%
      html_text() %>%
      readr::parse_number() %>%
      suppressWarnings()

    salary.3.colors <-
      page %>%
      html_nodes('tbody td:nth-child(4)') %>%
      html_attr('style') %>%
      str_replace("color:", '')

    salary.4 <-
      page %>%
      html_nodes('tbody td:nth-child(5)') %>%
      html_text() %>%
      readr::parse_number() %>%
      suppressWarnings()

    salary.4.colors <-
      page %>%
      html_nodes('tbody td:nth-child(5)') %>%
      html_attr('style') %>%
      str_replace("color:", '')

    salary.5 <-
      page %>%
      html_nodes('tbody td:nth-child(6)') %>%
      html_text() %>%
      readr::parse_number() %>%
      suppressWarnings()

    salary.5.colors <-
      page %>%
      html_nodes('tbody td:nth-child(6)') %>%
      html_attr('style') %>%
      str_replace("color:", '')

    salary.6 <-
      page %>%
      html_nodes('tbody td:nth-child(7)') %>%
      html_text %>%
      readr::parse_number() %>%
      suppressWarnings()

    salary.6.colors <-
      page %>%
      html_nodes('tbody td:nth-child(7)') %>%
      html_attr('style') %>%
      str_replace("color:", '')

    player_salary_df <-
      data_frame(namePlayer,
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
        namePlayer,
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
      gather(slugSeason, amountContract, -c(namePlayer)) %>%
      mutate(slugSeason = slugSeason %>% factor(ordered = T))

    contract_data <-
      player_contract_df %>%
      gather(slugSeason, color, -c(namePlayer)) %>%
      mutate(slugSeason = slugSeason %>% factor(ordered = T)) %>%
      left_join(color_df) %>%
      dplyr::select(-color) %>%
      mutate(
        isPlayerOption = ifelse(type == "Player Option", T, F),
        isPlayerOption = ifelse(isPlayerOption == T, T, F),
        isTeamOption = ifelse(type == "Team Option", T, F),
        isTeamOption = ifelse(isTeamOption == T, T, F)
      ) %>%
      suppressMessages()

    all_data <-
      salary_data %>%
      left_join(contract_data) %>%
      dplyr::select(slugSeason,
                    namePlayer:isPlayerOption,
                    type:isTeamOption,
                    amountContract) %>%
      suppressMessages()

   all_data <-
     all_data %>%
      dplyr::filter(amountContract > 0)

    if (assume_player_opt_out) {
      player_final_seasons <-
        all_data %>%
        dplyr::filter(isPlayerOption %>% is.na())
    }  else {
      player_final_seasons <-
        all_data
    }


    if (assume_team_doesnt_exercise) {
      player_final_seasons <-
        player_final_seasons %>%
        dplyr::filter(isTeamOption %>% is.na())
    }
   player_final_seasons <-
     player_final_seasons %>%
      group_by(namePlayer) %>%
      dplyr::select(namePlayer, slugSeason) %>%
      dplyr::filter(slugSeason == max(slugSeason)) %>%
      mutate(isFinalSeason = T) %>%
     ungroup()

    all_data %>%
      left_join(player_final_seasons) %>%
      mutate(namePlayerLower = namePlayer %>% str_to_lower()) %>%
      left_join(players_urls_df) %>%
      dplyr::select(-namePlayerLower) %>%
      mutate(isFinalSeason = ifelse(isFinalSeason %>% is.na(), F, T),
             urlTeamSalaryHoopsHype = url) %>%
      suppressMessages()
  }

parse_hoops_hype_salary_urls <-
  function(urls = c(
    "http://hoopshype.com/salaries/indiana_pacers/",
    "http://hoopshype.com/salaries/washington_wizards/",
    "http://hoopshype.com/salaries/portland_trail_blazers/"
  ),
  assume_player_opt_out = T,
  assume_team_doesnt_exercise = T,
  return_message = TRUE) {
    df <-
      data_frame()

    success <- function(res) {
      url <-
        res$url

      if (return_message) {
        glue::glue("Parsing {url}") %>%
          message()
      }
      parse_hoops_hype_salary_url_safe <-
        purrr::possibly(parse_hoops_hype_salary_url, data_frame())

      all_data <-
        parse_hoops_hype_salary_url_safe(url = url,
                                         assume_player_opt_out = assume_player_opt_out,
                                         assume_team_doesnt_exercise = assume_team_doesnt_exercise) %>%
        suppressWarnings()


      df <<-
        df %>%
        bind_rows(all_data)
    }
    failure <- function(msg) {
      data_frame()
    }
    urls %>%
      map(function(x) {
        curl_fetch_multi(url = x, success, failure)
      })
    multi_run()
    df
  }

#' Get HoopsHype Salaries by Teams
#'
#' @param teams vector of team names
#' @param all_teams if \code{TRUE} returns all teams
#' @param assume_player_opt_out if \code{TRUE} assumes player opt out of options
#' @param assume_team_doesnt_exercise if \code{TRUE} assunmes teams don't excerise options
#' @param nest_data if \code{TRUE} nests data
#' @param return_message if \code{TRUE} returns a messag
#'
#' @return
#' @export
#' @import dplyr rvest xml2 readr curl stringr tidyr
#' @examples
#' get_teams_hoopshype_salaries(all_teams = TRUE, nest_data = F, return_message = T)
get_teams_hoopshype_salaries <-
  function(teams = NULL,
           all_teams = TRUE,
           assume_player_opt_out = T,
           assume_team_doesnt_exercise = T,
           nest_data = F,
           return_message = TRUE) {

    if (!'df_hoopshype_team_salaries' %>% exists()) {
      df_hoopshype_team_salaries <- get_hoops_hype_teams_salary_summary()

      assign( 'df_hoopshype_team_salaries', df_hoopshype_team_salaries, envir = .GlobalEnv)
    }
    urls <- c()


    if (!teams %>% purrr::is_null()) {

      urls_teams <-
        df_hoopshype_team_salaries %>%
        mutate(nameTeamLower = nameTeam %>% str_to_lower())
        filter(nameTeam %>% str_detect(teams %>% str_to_lower() %>% str_c(collapse = "|"))) %>%
          pull(urlTeamSalaryHoopsHype) %>%
          sort() %>%
          unique()

        urls <-
          urls %>%
          append(urls_teams)
    }

    if (all_teams) {
      urls <-
        df_hoopshype_team_salaries %>%
        pull(urlTeamSalaryHoopsHype) %>%
        unique() %>%
        sort()
    }

    all_data <-
      urls %>%
      parse_hoops_hype_salary_urls(assume_player_opt_out = assume_player_opt_out,
                                   assume_team_doesnt_exercise = assume_team_doesnt_exercise,
                                   return_message = return_message)

    all_data <-
      all_data %>%
      left_join(df_hoopshype_team_salaries %>% select(nameTeam, urlTeamSalaryHoopsHype)) %>%
      select(nameTeam, everything()) %>%
      suppressMessages() %>%
      distinct()

    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-c(nameTeam, urlTeamSalaryHoopsHype), .key = 'dataSalaries')
    }
    all_data
  }
