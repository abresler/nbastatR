get_basketball_insider_team_ids <- function() {
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
      slugTeamBI = c(
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
      idBI = c(
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
      slugTeamBI = c(
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
      urlSalaryBI = paste0(
        'http://hw-files.com/tools/salaries/salaries_widget_new.php?team_id=',
        idBI
      )
    ) %>%
    suppressMessages()
  data
}

#' NBA team salary
#'
#' Returns information about an NBA teams salary
#'
#' @param team_name NBA team name
#' @param team_slug NBA team slug
#' @param assume_player_opt_out if `TRUE` assumes NBA player opts out
#' @param assume_team_doesnt_exercise if `TRUE` assumes teams don't exercise team option
#' @param spread_data if `TRUE` returns wide data
#' @param return_message if `TRUE` returns a message
#'
#' @return a `data_frame`
#' @export
#' @import dplyr rvest stringr purrr tidyr readr
#' @family salaries
#' @examples
#' get_nba_team_salaries(team_name = "Brooklyn Nets")
get_nba_team_salaries <-
  function(team_name = "Brooklyn Nets",
           team_slug = NA,
           assume_player_opt_out = T,
           assume_team_doesnt_exercise = T,
           spread_data = FALSE,
           return_message = T) {
    team_name_df <-
      get_basketball_insider_team_ids()

    teams <-
      team_name_df$nameTeam %>%
      str_to_lower()

    if (!team_name %>% is.na() &
        (!team_name %>% str_to_lower() %in% teams)) {
      team_message <-
        paste0(team_name_df$nameTeam, collapse = '\n')

      "Team can only be:\n" %>%
        paste0(team_message) %>%
        stop()
    }

    if (!team_slug %>% is.na() &
        (team_slug %>% str_to_lower() %in% team_name_df$slugTeamBI)) {
      team_message <-
        paste0(team_name_df$slugTeamBI, collapse = '\n')

      "Team slugs can only be:\n" %>%
        paste0(team_message) %>%
        stop()
    }

    if (team_slug %>% is.na()) {
      url <-
        team_name_df %>%
        dplyr::filter(nameTeam == team_name) %>%
        .$urlSalaryBI
    } else {
      url <-
        team_name_df %>%
        dplyr::filter(slugTeamBI == team_slug) %>%
        .$urlSalaryBI

      team_name <-
        team_name_df %>%
        dplyr::filter(slugTeamBI == team_slug) %>%
        .$nameTeam
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
                 typeContractDetail = types)

    page <-
      url %>%
      read_html()

    salary_table <-
      page %>%
      html_table(header = F) %>%
      purrr::flatten_df()

    names_salary_table <-
      c(
        'namePlayer',
        salary_table %>%
          slice(1) %>%
          gather(item, value) %>%
          slice(-1) %>%
          .$value
      )

    names(salary_table) <-
      names_salary_table
    salary_table <-
      salary_table  %>%
      slice(-1)

    salary_table <-
      salary_table %>%
      dplyr::filter(!namePlayer %>% str_detect("Total")) %>%
      mutate(nameTeam = team_name) %>%
      dplyr::select(nameTeam, everything())

    salary_data <-
      salary_table %>%
      separate(namePlayer,
               into = c('namePlayer', 'statusPlayer'),
               sep = '\\(') %>%
      mutate(
        isWaived = namePlayer %>% str_detect("wavied"),
        namePlayer = namePlayer %>% str_replace('waived', '') %>% str_trim(),
        namePlayer = namePlayer %>% str_trim(),
        statusPlayer = statusPlayer %>% str_replace("\\)", ''),
        statusPlayer = ifelse(statusPlayer %>% is.na, "current roster", statusPlayer),
        isOnRoster = ifelse(statusPlayer == "current roster", T, F)
      ) %>%
      dplyr::select(nameTeam, namePlayer:statusPlayer, isOnRoster, everything()) %>%
      gather(slugSeason,
             value,
             -c(nameTeam, namePlayer, statusPlayer, isOnRoster, isWaived)) %>%
      mutate(value = value %>% parse_number()) %>%
      dplyr::select(nameTeam,
                    slugSeason,
                    namePlayer,
                    statusPlayer,
                    isWaived,
                    isOnRoster,
                    value) %>%
      dplyr::filter(!value %>% is.na()) %>%
      suppressWarnings()


    salary_data <-
      salary_data %>%  mutate_if(is.character,
                                 funs(. %>% str_replace_all("\\\\", "") %>% str_trim()))


    year_1_salary <-
      page %>%
      html_nodes('tbody td:nth-child(2)') %>%
      html_text() %>%
      parse_number() %>%
      suppressWarnings()

    year_1_salary <-
      year_1_salary[1:(year_1_salary %>% length() - 1)]

    year_1_salary_color <-
      page %>%
      html_nodes('tbody td:nth-child(2) span') %>%
      html_attr('style') %>%
      str_replace('\\\"color:', '') %>%
      gsub("[^A-Z a-z#0-9]", '', .)

    year_1_df <-
      data_frame(slugSeason = names_salary_table[2],
                 color = year_1_salary_color,
                 value = year_1_salary[1:length(year_1_salary_color)])

    year_2_salary <-
      page %>%
      html_nodes('tbody td:nth-child(3)') %>%
      html_text() %>%
      parse_number() %>%
      suppressWarnings()

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
      data_frame(slugSeason = names_salary_table[3],
                 color = year_2_salary_color,
                 value = year_2_salary[1:length(year_2_salary_color)])

    year_3_salary <-
      page %>%
      html_nodes('tbody td:nth-child(4)') %>%
      html_text() %>%
      parse_number() %>%
      suppressWarnings()

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
      data_frame(slugSeason = names_salary_table[4],
                 color = year_3_salary_color,
                 value = year_3_salary[1:length(year_3_salary_color)])

    year_4_salary <-
      page %>%
      html_nodes('tbody td:nth-child(5)') %>%
      html_text() %>%
      parse_number() %>%
      suppressWarnings()

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
      data_frame(slugSeason = names_salary_table[5],
                 color = year_4_salary_color,
                 value = year_4_salary[1:length(year_4_salary_color)])

    year_5_salary <-
      page %>%
      html_nodes('tbody td:nth-child(6)') %>%
      html_text() %>%
      parse_number() %>%
      suppressWarnings()

    if (year_5_salary[1:(year_5_salary %>% length() - 1)] %>%
        .[!is.na(.)] %>% length == 0) {
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
      data_frame(slugSeason = names_salary_table[6],
                 color = year_5_salary_color,
                 value = year_5_salary[1:length(year_5_salary_color)])

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
        isWaived = ifelse(namePlayer %>% str_detect('waived'), T, F),
        isNonGuaranted = ifelse(typeContractDetail == 'Non-Guaranteed', T, F),
        isNonGuaranted = ifelse(isNonGuaranted %>% is.na(), F, isNonGuaranted),
        isTeamOption = ifelse(typeContractDetail == 'Team Option', T, F),
        isTeamOption = ifelse(isTeamOption %>% is.na(), F, isTeamOption),
        isPlayerOption = ifelse(typeContractDetail == 'Player Option', T, F),
        isPlayerOption = ifelse(isPlayerOption %>% is.na(), F, isPlayerOption)
      ) %>%
      dplyr::select(
        slugSeason,
        nameTeam:isOnRoster,
        isNonGuaranted,
        isTeamOption,
        isPlayerOption,
        typeContractDetail,
        value
      ) %>%
      suppressMessages()

    if (assume_player_opt_out == T) {
      player_final_seasons <-
        all_data %>%
        dplyr::filter(isPlayerOption  == F)
    }  else {
      player_final_seasons <-
        all_data
    }


    if (assume_team_doesnt_exercise == T) {
      player_final_seasons <-
        player_final_seasons %>%
        dplyr::filter(isTeamOption == F)
    }
    player_final_seasons %<>%
      group_by(namePlayer) %>%
      dplyr::select(namePlayer, slugSeason, isOnRoster) %>%
      dplyr::filter(slugSeason == max(slugSeason)) %>%
      mutate(
        isFinalSeason = T,
        isFinalSeason = ifelse(isOnRoster == F, NA, isOnRoster)
      ) %>%
      distinct()

    all_data <-
      all_data %>%
      left_join(player_final_seasons) %>%
      dplyr::select(slugSeason:statusPlayer, isFinalSeason, everything()) %>%
      suppressMessages()

    all_data <-
      all_data %>%
      mutate(namePlayer = namePlayer %>% gsub("[^A-Z a-z # ' 0-9]", '', .)) %>%
      distinct()

    if (return_message) {
      "You got salary data for the " %>%
        paste0(team_name) %>%
        message()
    }

    all_data <-
      all_data %>%
      group_by(namePlayer, slugSeason) %>%
      slice(1) %>%
      ungroup()

    if (spread_data) {
      all_data <-
        all_data %>%
        select(nameTeam, namePlayer, slugSeason, value) %>%
        spread(slugSeason, value)
    }
    all_data
  }

#' NBA team salaries
#'
#' Gets information about NBA teams salaries
#'
#' @param assume_player_opt_out if `TRUE` assumes player opts out of a player option
#' @param assume_team_doesnt_exercise if `TRUE` assumes teams do not exercise team option
#' @param spread_data if `TRUE` spreads data
#' @param return_message if `TRUE` returns a message
#'
#' @return a `data_frame`
#' @export
#' @family salaries
#' @import dplyr rvest stringr purrr tidyr readr
#' @examples
#' get_all_nba_teams_salaries(assume_player_opt_out = T, assume_team_doesnt_exercise = T, return_message = TRUE)

get_all_nba_teams_salaries <-
  function(assume_player_opt_out = T,
           assume_team_doesnt_exercise = T,
           spread_data = F,
           return_message = T) {
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

    get_nba_team_salaries_safe <-
      purrr::possibly(get_nba_team_salaries, data_frame())

    all_salaries <-
      all_teams %>%
      purrr::map_df(
        function(x)
          get_nba_team_salaries_safe(
            team_name = x,
            assume_player_opt_out = apo,
            assume_team_doesnt_exercise = atde,
            spread_data = spread_data,
            return_message = return_message
          )
      )

    all_salaries <-
      all_salaries %>%
      mutate(
        isWaived = namePlayer %>% str_detect('waived'),
        namePlayer = namePlayer %>% str_replace('waived', '') %>% str_trim()
      )
    all_salaries
  }
