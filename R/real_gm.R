function_packages <- c(
  'dplyr',
  'magrittr',
  'Ecfun',
  'jsonlite',
  'xml2',
  'tidyr',
  'httr',
  'rvest',
  'purrr',
  'stringr',
  'lubridate',
  'tidyr'
)
camelParse <- function (x, except = c("De", "Mc", "Mac"))  {
  x. <- strsplit(x, "")
  nx <- length(x)
  out <- vector("list", length = nx)
  names(out) <- names(x)
  for (ix in 1:nx) {
    xi <- x.[[ix]]
    lower <- (xi %in% letters)
    upper <- (xi %in% LETTERS)
    ni <- length(xi)
    camel <- which(lower[-ni] & upper[-1])
    begin <- c(1, camel + 1)
    end <- c(camel, ni)
    X <- substring(x[ix], begin, end)
    for (ex in except) {
      ei <- regexpr(ex, X)
      ej <- (ei + 2 - nchar(X))
      ej[ei < 0] <- -1
      ek <- which(ej > 0)
      for (ik in rev(ek)) {
        X[ik] <- paste(X[ik], X[ik + 1], sep = "")
        X <- X[-(ik + 1)]
      }
    }
    out[[ix]] <- X
  }
  out
}
install_needed_packages <-
  function(required_packages = function_packages) {
    needed_packages <-
      required_packages[!(required_packages %in% installed.packages()[, "Package"])]

    if (length(needed_packages) > 0) {
      if (!require("pacman"))
        install.packages("pacman")
      pacman::p_load(needed_packages)
    }
  }
load_needed_packages <-
  function(required_packages = function_packages) {
    loaded_packages <-
      gsub('package:', '', search())

    package_to_load <-
      required_packages[!required_packages %in% loaded_packages]
    if (length(package_to_load) > 0) {
      lapply(package_to_load, library, character.only = T)
    }
  }


height_in_inches <-
  function(height) {
    load_needed_packages(function_packages)
    height_ft_in <-
      height %>%
      str_split("-") %>%
      unlist %>%
      as.numeric()
    height_in <-
      height_ft_in[1] * 12 + height_ft_in[2]
    return(height_in)
  }

get_player_resolution_df <- function() {
  player_df <-
    data_frame(
      namePlayer = c(
        "Edy Tavares",
        "R.J. Hunter",
        "P.J. Hairston",
        "Matt Dellavedova",
        "J.J. Barea",
        "J.J. Hickson",
        "James McAdoo",
        "K.J. McDaniels",
        "C.J. Miles",
        "J.J. Redick",
        "C.J. Wilcox",
        "Marcelinho Huertas",
        "Larry Nance, Jr.",
        "C.J. Watson",
        "P.J. Tucker",
        "T.J. Warren",
        "C.J. McCollum",
        "Raulzinho Neto",
        "Maybyner Nene",
        "Kelly Oubre, Jr."
      ),
      namePlayerNBA = c(
        "Walter Tavares",
        "RJ Hunter",
        "PJ Hairston",
        "Matthew Dellavedova",
        "Jose Juan Barea",
        "JJ Hickson",
        "James Michael McAdoo",
        "KJ McDaniels",
        "CJ Miles",
        "JJ Redick",
        "CJ Wilcox",
        "Marcelo Huertas",
        "Larry Nance Jr.",
        "CJ Watson",
        "PJ Tucker",
        "TJ Warren",
        "CJ McCollum",
        "Raul Neto",
        "Nene",
        "Kelly Oubre"
      )
    )
  return(player_df)
}



get_contract_status_ids <- function() {
  contract_df <-
    data_frame(statusContract = c(NA, "Unrestricted Free Agent", "Unsigned Draft Pick", "Draft Eligible",
                                  "Restricted Free Agent"),
               idContractStatus = c('OT', 'UFA', 'UDP', 'DE', 'RFA')
    )

  return(contract_df)
}

get_leagues_teams_df <- function() {
  nba_teams <-
    data_frame(nameTeam = c("Atlanta Hawks", "Boston Celtics", "Brooklyn Nets", "Charlotte Hornets",
                            "Chicago Bulls", "Cleveland Cavaliers", "Dallas Mavericks", "Denver Nuggets",
                            "Detroit Pistons", "Golden State Warriors", "Houston Rockets",
                            "Indiana Pacers", "Los Angeles Clippers", "Los Angeles Lakers",
                            "Memphis Grizzlies", "Miami Heat", "Milwaukee Bucks", "Minnesota Timberwolves",
                            "New Jersey Nets", "New Orleans Hornets", "New Orleans Pelicans",
                            "New York Knicks", "Oklahoma City Thunder", "Orlando Magic",
                            "Philadelphia 76ers", "Phoenix Suns", "Portland Trail Blazers",
                            "Sacramento Kings", "San Antonio Spurs", "Toronto Raptors", "Utah Jazz",
                            "Washington Wizards"),
               idLeague = 'NBA'
    )

  nba_dl_teams <-
    data_frame(nameTeam = c("Delaware 87ers", "Erie BayHawks", "Maine Red Claws", "Raptors 905",
                            "Westchester Knicks", "Canton Charge", "Fort Wayne Mad Ants",
                            "Grand Rapids Drive", "Iowa Energy", "Sioux Falls Skyforce",
                            "Austin Spurs", "Oklahoma City Blue", "Rio Grande Valley Vipers",
                            "Texas Legends", "Bakersfield Jam", "Idaho Stampede", "Los Angeles D-Fenders",
                            "Reno Bighorns", "Santa Cruz Warriors"),
               idLeague = 'NBDL'
    )

  team_df <-
    nba_teams %>%
    bind_rows(nba_dl_teams)
  return(team_df)
}
get_agents_urls <- function() {
  load_needed_packages(function_packages)
  url <-
    'http://basketball.realgm.com/info/agent-relationships'

  page <-
    url %>%
    read_html()

  name.agent <-
    page %>%
    html_nodes('td:nth-child(8) a') %>%
    html_text()

  url.agent.realgm <-
    page %>%
    html_nodes('td:nth-child(8) a') %>%
    html_attr('href') %>%
    paste0('http://basketball.realgm.com',. )

  agent_df <-
    data_frame(nameAgent = name.agent, urlAgentRealGM = url.agent.realgm) %>%
    distinct() %>%
    arrange(nameAgent)

  return(agent_df)

}

parse_agent_metadata <-
  function(page) {
    load_needed_packages(function_packages)
    nameAgent <-
      page %>%
      html_nodes('.force-table tr:nth-child(1) td') %>%
      html_text()

    nameAgency <-
      page %>%
      html_nodes('.force-table tr:nth-child(2) td') %>%
      html_text()

    urlAgency <-
      page %>%
      html_nodes('.force-table tr:nth-child(3) td a') %>%
      html_attr('href')

    agent_metadata_df <-
      data_frame(nameAgent, nameAgency, urlAgency)

    return(agent_metadata_df)
  }
parse_agent_player_table <- function(page) {
  load_needed_packages(function_packages)
  player <-
    page %>%
    html_nodes('td:nth-child(1)') %>%
    html_text() %>%
    str_trim()

  url.player.realgm <-
    page %>%
    html_nodes('td:nth-child(1) a') %>%
    html_attr('href') %>%
    paste0('http://basketball.realgm.com', .)

  team <-
    page %>%
    html_nodes('td:nth-child(2)') %>%
    html_text() %>%
    str_trim()

  start_teams <-
    team %>% grep('http', .) + 1

  team <-
    team[start_teams:(length(team))]

  team %<>%
    gsub("N/A", NA, .)

  id.position <-
    page %>%
    html_nodes('td:nth-child(3)') %>%
    html_text() %>%
    str_trim()

  height <-
    page %>%
    html_nodes('td:nth-child(4)') %>%
    html_text() %>%
    str_trim()

  weight <-
    page %>%
    html_nodes('td:nth-child(5)') %>%
    html_text() %>%
    readr::parse_number()

  date.birth <-
    page %>%
    html_nodes('td:nth-child(6)') %>%
    html_text() %>%
    strptime('%b %d, %Y') %>%
    as.Date()

  age <-
    page %>%
    html_nodes('td:nth-child(7)') %>%
    html_text() %>%
    readr::parse_number()


  statusContract <-
    page %>%
    html_nodes('td:nth-child(8)') %>%
    html_text()

  years.played <-
    page %>%
    html_nodes('td:nth-child(9)') %>%
    html_text() %>%
    readr::parse_number()

  team_df <-
    get_leagues_teams_df()
  contract_status_df <-
    get_contract_status_ids()
  player_df <-
    get_player_resolution_df()
  player_agent_df <-
    data_frame(
      namePlayer = player,
      nameTeam = team,
      idPosition = id.position,
      heightPlayer = height,
      weightLBS = weight,
      dateBirth = date.birth,
      agePlayer = age,
      yearsPlayed = years.played,
      statusContract,
      urlPlayerNBA = url.player.realgm
    ) %>%
    left_join(team_df) %>%
    left_join(player_df) %>%
    suppressMessages()

  player_agent_df <-
    player_agent_df %>%
    mutate(
      namePlayer = ifelse(!namePlayerNBA %>% is.na(), namePlayerNBA, namePlayer),
      heightInches = heightPlayer %>% lapply(height_in_inches) %>% unlist,
      idLeague = ifelse(idLeague %>% is.na, 'O', idLeague)
    ) %>%
    dplyr::select(-namePlayerNBA) %>%
    separate(idPosition,
             into = c('idPosition', 'idPositionSecondary'),
             sep = '\\/') %>%
    dplyr::select(
      idLeague,
      nameTeam,
      namePlayer,
      agePlayer,
      idPosition,
      idPositionSecondary,
      heightPlayer,
      heightInches,
      everything()
    ) %>%

    arrange((idLeague), nameTeam) %>%
    separate(
      statusContract,
      into = c('statusContract', 'teamRightsOwnedBy'),
      sep = '\\('
    ) %>%
    mutate(
      teamRightsOwnedBy = teamRightsOwnedBy %>% str_replace('\\)', '') %>% str_trim(),
      statusContract = statusContract %>% str_trim %>% str_replace('\\,', '\\:')
    ) %>%
    separate(statusContract,
             into = c('statusContract', 'dateContract'),
             sep = '\\: ') %>%
    mutate(
      statusContract = ifelse(statusContract == team, NA, statusContract),
      dateContract = dateContract %>% strptime('%b %d, %Y') %>%
        as.Date()
    ) %>%
    mutate(statusContract = statusContract %>% str_trim %>% str_replace(' in ', ':')) %>%
    separate(
      statusContract,
      into = c('statusContract', 'yearDraftEligible'),
      sep = '\\:'
    ) %>%
    mutate(yearDraftEligible =  yearDraftEligible %>% as.numeric()) %>%
    left_join(contract_status_df) %>%
    dplyr::select(idLeague:statusContract, idContractStatus,
                  everything()) %>%
    suppressMessages()
  return(player_agent_df)

}

get_agent_metadata_df <-
  function(agent = "Jeff Schwartz") {
    if(!'agent' %>% exists()) {
      stop("Please enter an agent")
    }
    load_needed_packages(function_packages)
    install_needed_packages(function_packages)
    if(!'all_agent_df' %>% exists){
      all_agent_df <-
        get_agents_urls()
    }

    if (!agent  %in% all_agent_df$nameAgent) {
      stop("Sorry " %>% paste0(agent, ' is not a valid agent.'))
    }

    url <-
      all_agent_df %>%
      dplyr::filter(nameAgent == agent) %>%
      .$urlAgentRealGM

    page <-
      url %>%
      read_html()

    metadata_df <-
      page %>%
      parse_agent_metadata()

    return(metadata_df)
  }

get_agent_player_df <-
  function(agent = "Jeff Schwartz") {
    if(!'agent' %>% exists()) {
      stop("Please enter an agent")
    }
    load_needed_packages(function_packages)
    install_needed_packages(function_packages)
    if(!'all_agent_df' %>% exists){
      all_agent_df <-
        get_agents_urls()
    }

    if (!agent  %in% all_agent_df$nameAgent) {
      stop("Sorry " %>% paste0(agent, ' is not a valid agent.'))
    }

    url <-
      all_agent_df %>%
      dplyr::filter(nameAgent == agent) %>%
      .$urlAgentRealGM

    page <-
      url %>%
      read_html()

    agent_player_df <-
      page %>%
      parse_agent_player_table()

    return(agent_player_df)
  }

get_agent_player_data_df <-
  function(agent = "Jeff Schwartz", return_message = T) {
    if(!'agent' %>% exists()) {
      stop("Please enter an agent")
    }
    load_needed_packages(function_packages)
    install_needed_packages(function_packages)
    if(!'all_agent_df' %>% exists){
      all_agent_df <-
        get_agents_urls()
    }

    if (!agent  %in% all_agent_df$nameAgent) {
      stop("Sorry " %>% paste0(agent, ' is not a valid agent.'))
    }

    url <-
      all_agent_df %>%
      dplyr::filter(nameAgent == agent) %>%
      .$urlAgentRealGM

    page <-
      url %>%
      read_html()

    agent_player_df <-
      page %>%
      parse_agent_player_table()

    agent_metadata_df <-
      page %>%
      parse_agent_metadata()

    agent_data <-
      agent_metadata_df %>%
      left_join(agent_player_df %>%
                  mutate("nameAgent" = agent))
    if (return_message == T) {
      "You data for all " %>%
        paste0(agent_data %>% nrow(), " players represented by\n",
               agent,' from ', agent_metadata_df$nameAgency)
    }
    return(agent_data)

  }

#' Get agents players
#'
#' @param agents vector of agents
#' @param all_agents if \code{TRUE} scrapes all agents
#' @param message
#'
#' @return
#' @export
#' @import dplyr rvest stringr tidyr purrr xml2 readr magrittr
#' @examples
get_agents_player_data_df <-
  function(agents = "Jeff Schwartz",
           all_agents = FALSE,
           return_message = FALSE) {
    if (all_agents == F & !'agents' %>% exists()) {
      stop("Please enter agents")
    }

    if (all_agents == T) {
      if (!'all_agent_df' %>% exists) {
        all_agent_df <-
          get_agents_urls()
      }
      agents <-
        all_agent_df$nameAgent
    }
    get_agent_player_data_df_safe <-
      purrr::possibly(get_agent_player_data_df, data_frame())

    all_agents_players <-
      agents %>%
      map_df(function(x) {
        get_agent_player_data_df_safe(x)
      })

    return(all_agents_players)
  }

#' get players agents
#'
#' @param return_message
#'
#' @return
#' @export
#' @import dplyr rvest stringr tidyr purrr xml2 readr magrittr
#' @examples
get_players_agents <-
  function(return_message = F) {
    load_needed_packages(function_packages)
    install_needed_packages(function_packages)

    url <-
      'http://basketball.realgm.com/info/agent-relationships'

    page <-
      url %>%
      read_html()

    table <-
      page %>%
      html_table() %>%
      data.frame() %>%
      tbl_df()

    player <-
      page %>%
      html_nodes('td:nth-child(1)') %>%
      html_text() %>%
      str_trim()

    urlPlayerRealGM <-
      page %>%
      html_nodes('td:nth-child(1) a') %>%
      html_attr('href') %>%
      paste0('http://basketball.realgm.com', .)

    nameTeam <-
      page %>%
      html_nodes('td:nth-child(2)') %>%
      html_text() %>%
      str_trim()


    idPosition <-
      page %>%
      html_nodes('td:nth-child(3)') %>%
      html_text() %>%
      str_trim()

    heightPlayer <-
      page %>%
      html_nodes('td:nth-child(4)') %>%
      html_text() %>%
      str_trim()

    weightLBS <-
      page %>%
      html_nodes('td:nth-child(5)') %>%
      html_text() %>%
      readr::parse_number()

    dateBirth <-
      page %>%
      html_nodes('td:nth-child(6)') %>%
      html_text() %>%
      strptime('%b %d, %Y') %>%
      as.Date()

    agePlayer <-
      page %>%
      html_nodes('td:nth-child(7)') %>%
      html_text() %>%
      readr::parse_number()

    agentPlayer <-
      page %>%
      html_nodes('td:nth-child(8)') %>%
      html_attr('rel')

    statusContract <-
      page %>%
      html_nodes('td:nth-child(9)') %>%
      html_attr('rel')

    players.agents <-
      table$Agent %>% camelParse()
    all_agents <-
      data_frame()
    for (a in 1:length(players.agents)) {
      agents <-
        players.agents[a] %>%
        unlist() %>%
        paste0(collapse = ', ')

      agent_df <- data_frame(nameAgent = agents)
      all_agents %<>%
        bind_rows(agent_df)
    }
    player.agents <-
      all_agents$nameAgent
    player_df <-
      get_player_resolution_df()
    team_df <-
      get_leagues_teams_df()
    contract_status_df <-
      get_contract_status_ids()
    players_agents_df <-
      data_frame(
        namePlayer = player,
        nameAgent = player.agents,
        nameTeam,
        idPosition,
        heightPlayer,
        weightLBS,
        dateBirth,
        agePlayer,
        statusContract,
        urlPlayerRealGM
      ) %>%
      left_join(team_df) %>%
      left_join(player_df) %>%
      mutate(
        namePlayer = ifelse(!namePlayerNBA %>% is.na(), namePlayerNBA, namePlayer),
        heightInches = heightPlayer %>% lapply(height_in_inches) %>% unlist,
        idLeague = ifelse(idLeague %>% is.na, 'O', idLeague)
      ) %>%
      separate(idPosition,
               into = c('idPosition', 'idPositionSecondary'),
               sep = '\\/') %>%
      dplyr::select(
        idLeague,
        nameTeam,
        namePlayer,
        agePlayer,
        nameAgent,
        idPositionSecondary,
        idPositionSecondary,
        heightPlayer,
        heightInches,
        everything()
      ) %>%
      arrange((idLeague), nameTeam) %>%
      suppressWarnings() %>%
      suppressMessages()

    players_agents_df <-
      players_agents_df %>%
      separate(
        statusContract,
        into = c('statusContract', 'teamRightsOwnedBy'),
        sep = '\\('
      ) %>%
      mutate(
        teamRightsOwnedBy = teamRightsOwnedBy %>% str_replace('\\)', '') %>% str_trim(),
        statusContract = statusContract %>% str_trim %>% str_replace('\\,', '\\:')
      ) %>%
      separate(
        statusContract,
        into = c('statusContract', 'dateContract'),
        sep = '\\: '
      ) %>%
      mutate(
        statusContract = ifelse(statusContract == nameTeam, NA, statusContract),
        dateContract = dateContract %>% strptime('%b %d, %Y') %>%
          as.Date()
      ) %>%
      mutate(statusContract = statusContract %>% str_trim() %>% str_replace(' in ', ':')) %>%
      separate(
        statusContract,
        into = c('statusContract', 'yearDraftEligible'),
        sep = '\\:'
      ) %>%
      mutate(yearDraftEligible =  yearDraftEligible %>% as.numeric()) %>%
      left_join(contract_status_df) %>%
      dplyr::select(idLeague:statusContract, idContractStatus,
                    everything()) %>%
      suppressWarnings() %>%
      suppressMessages()

    if (return_message) {
      "You got all realGM contract data" %>%
        message()
    }
    return(players_agents_df)
  }
