parse_status <-
  function(status){
    is_retired <- status %>% str_detect("Retired")
    is_ufa <- status %>% str_detect("Unrestricted Free Agent|Restricted Free Agent")
    is_dead <- status %>% str_detect("Deceased")
    is_de <-
      status %>% str_detect("Draft Eligible")
    is_udp <- status %>% str_detect("Unsigned Draft Pick")

    if (is_dead) {
      parts <- status %>% str_split("\\(") %>% flatten_chr() %>% str_replace_all("\\)", "") %>% str_trim()
      return(data_frame(statusContract = status,
                        dateDeath = parts[[2]] %>% lubridate::mdy(),
                        isActivePlayer = F))
    }
    if (is_udp) {
      parts <-
        status %>% str_split("\\(") %>% flatten_chr() %>% str_replace_all("\\)", "") %>% str_trim()

      if (parts %>% length() == 1) {
        return(data_frame(statusContract = status, isActivePlayer = TRUE))
      }

      return(data_frame(statusContract = parts[1], nameTeamDraftRights = parts[2], isActivePlayer = T))
    }

    if (is_de) {
      data <-
        data_frame(statusContract = status,
                   yearDraftEligible = parse_number(status)) %>% suppressWarnings()
      return(data)
    }

    if (is_retired) {
      parts <- status %>% str_split("\\(") %>% flatten_chr() %>% str_replace_all("\\)", "") %>% str_trim()
      return(data_frame(statusContract = status,
                        dateRetired = parts[[2]] %>% lubridate::mdy(),
                        isActivePlayer = F))
    }

    if (is_ufa) {
      return(data_frame(statusContract = status, isActivePlayer = TRUE))
    }

    parts <-
      status %>% sub("\\, ", "\\;", .) %>%
      str_split("\\;") %>%
      flatten_chr()

    is_2way <-
      parts %>% str_detect("Two-Way") %>% sum(na.rm = T) >0

    parts <- parts %>% str_replace_all("\\Two-Way", "") %>% str_trim()



    data_frame(statusContract = status,
               isActivePlayer = TRUE,
               nameTeamContract = parts[1],
               dateContract = parts[[2]] %>% lubridate::mdy(),
               isTwoWayContract = is_2way)


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


dictionary_contract_status <- function() {
  contract_df <-
    data_frame(statusContract = c(NA, "Unrestricted Free Agent", "Unsigned Draft Pick", "Draft Eligible",
                                  "Restricted Free Agent"),
               slugStatusContract = c('OT', 'UFA', 'UDP', 'DE', 'RFA')
    )

  contract_df
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
  agent_df
}

parse_agent_metadata <-
  function(page) {
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
      data_frame(nameAgent)

    if (nameAgency %>% length() > 0) {
      agent_metadata_df <-
        agent_metadata_df %>%
        mutate(nameAgency)
    }

    if (urlAgency %>% length() > 0) {
      agent_metadata_df <-
        agent_metadata_df %>%
        mutate(urlAgency)
    }

    agent_metadata_df
  }
parse_agent_player_table <-
  function(page) {
  player <-
    page %>%
    html_nodes('td:nth-child(1)') %>%
    html_text() %>%
    str_trim()

  url_player_real_gm <-
    page %>%
    html_nodes('td:nth-child(1) a') %>%
    html_attr('href') %>%
    paste0('http://basketball.realgm.com', .)

  teams <-
    page %>%
    html_nodes('td:nth-child(2)') %>%
    html_text() %>%
    str_trim()

  start_teams <-
      length(teams) - length(player) + 1


  team <-
    teams[start_teams:length(teams)] %>%
    suppressWarnings() %>%
    suppressMessages()

  teams <- team %>% str_replace_all("N/A", NA_character_)

  positions <-
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
    as.character() %>%
    readr::parse_number() %>%
    suppressWarnings()

  birth_dates <-
    page %>%
    html_nodes('td:nth-child(6)') %>%
    html_text() %>%
    strptime('%b %d, %Y') %>%
    as.Date()

  age <-
    page %>%
    html_nodes('td:nth-child(7)') %>%
    html_text() %>%
    as.character() %>%
    readr::parse_number() %>%
    suppressWarnings()


  statusContract <-
    page %>%
    html_nodes('td:nth-child(8)') %>%
    html_text()

  years_played <-
    page %>%
    html_nodes('td:nth-child(9)') %>%
    html_text() %>%
    as.character() %>%
    readr::parse_number() %>%
    suppressWarnings()

  team_df <-
    get_leagues_teams_df()
  contract_status_df <-
    dictionary_contract_status()

  player_df <-
    get_player_resolution_df()

  player_agent_df <-
    data_frame(
      namePlayer = player,
      nameTeam = teams,
      slugPosition = positions,
      heightPlayer = height,
      weightLBS = weight,
      dateBirth = birth_dates,
      agePlayer = age,
      yearsPlayed = years_played,
      statusContract,
      urlPlayerNBA =  url_player_real_gm
    ) %>%
    left_join(team_df) %>%
    left_join(player_df) %>%
    suppressMessages()

  player_agent_df <-
    player_agent_df %>%
    mutate(
      namePlayer = ifelse(!namePlayerNBA %>% is.na(), namePlayerNBA, namePlayer),
      heightInches = heightPlayer %>% map_dbl(height_in_inches),
      idLeague = ifelse(idLeague %>% is.na, 'O', idLeague)
    ) %>%
    dplyr::select(-namePlayerNBA) %>%
    separate(
      slugPosition,
      into = c('slugPosition', 'slugPositionSecondary'),
      sep = '\\/'
    ) %>%
    dplyr::select(
      idLeague,
      nameTeam,
      namePlayer,
      agePlayer,
      slugPosition,
      slugPositionSecondary,
      heightPlayer,
      heightInches,
      everything()
    ) %>%

    arrange((idLeague), nameTeam) %>%
    suppressWarnings()

  statuses <-
    player_agent_df$statusContract %>%
    unique()

  df_status <-
    statuses %>%
    map_df(function(status){
      parse_status(status = status)
    })

  player_agent_df <-
    player_agent_df %>%
    left_join(df_status) %>%
    suppressMessages()


  player_agent_df

}

parse_agent_url <-
  function(url = "http://basketball.realgm.com/info/agent_clients/Aaron-Goodwin/1"){
    page <-
      url %>%
      read_html()

    metadata_df <-
      page %>%
      parse_agent_metadata() %>%
      mutate(urlAgentRealGM = url)

    agent_players_df <-
      page %>%
      parse_agent_player_table() %>%
      mutate(urlAgentRealGM = url) %>%
      suppressWarnings()

    agent_players_df <-
      agent_players_df %>%
      left_join(metadata_df) %>%
      dplyr::select(one_of(c("nameAgency","nameAgent")), everything()) %>%
      dplyr::select(which(colMeans(is.na(.)) < 1)) %>%
      suppressMessages()
    agent_players_df
  }

parse_agent_urls <-
  function(urls = c("http://basketball.realgm.com/info/agent_clients/James-Dennis/754",
                    "http://basketball.realgm.com/info/agent_clients/Lance-Young/63",
                    "http://basketball.realgm.com/info/agent_clients/Lubomir-Rysavy/548",
                    "http://basketball.realgm.com/info/agent_clients/Sam-Goldfeder/44",
                    "http://basketball.realgm.com/info/agent_clients/Francis-Bolden/338"
  ),
  return_message = TRUE
  ) {
    df <-
        data_frame()

      success <- function(res) {
        url <-
          res$url

        if (return_message) {
          glue::glue("Parsing {url}") %>%
            message()
        }
        parse_agent_url_safe <-
          purrr::possibly(parse_agent_url, data_frame())

        all_data <-
          parse_agent_url_safe(url = url)


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


#' Players Agents
#'
#' Acquires NBA players agents
#'
#' @param agents vector of agents
#' @param all_agents if \code{TRUE} scrapes all agents
#' @param return_message if \code{TRUE} returns a message
#'
#' @return a  \code{data_frame}
#' @export
#' @family agents
#' @import dplyr rvest stringr tidyr purrr xml2 readr magrittr
#' @examples
#' get_agents_players(agents = c("Jeff Schwartz"))
#'
get_agents_players <-
  function(agents = "Jeff Schwartz",
           all_agents = F,
           return_message = FALSE) {
    if (!all_agents & agents %>% purrr::is_null()) {
      stop("Please enter agents")
    }
    if (!'df_all_agent_urls' %>% exists()) {
      df_all_agent_urls <-
        get_agents_urls()
    }

    if (all_agents) {
      urls <-
        df_all_agent_urls$urlAgentRealGM
    } else {
      urls <-
        df_all_agent_urls %>%
        filter(nameAgent %>% str_detect(agents %>% str_c(collapse = "|")))
    }

    all_data <-
      parse_agent_urls(urls = urls, return_message = return_message) %>%
      arrange(nameAgent)

    all_data

  }

#' Get NBA Players Agents
#'
#' @param return_message if \code{TRUE} returns player url
#'
#' @return a `data_frame()`
#' @export
#' @family agents
#' @import dplyr rvest stringr tidyr purrr xml2 readr magrittr curl
#' @examples
#' library(dplyr)
#' df_players_agents <-
#'  get_players_agents()
#' df_players_agents %>%
#' glimpse()
get_players_agents <-
  function(nest_data = F,
           return_message = T) {

    url <-
      'http://basketball.realgm.com/info/agent-relationships'
    con <- url %>% curl()
    page <-
      con %>%
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


    slugPosition <-
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
      as.character() %>%
      readr::parse_number() %>%
      suppressWarnings()

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
      as.character() %>%
      readr::parse_number() %>%
      suppressWarnings()

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
      all_agents <-
        all_agents %>%
        bind_rows(agent_df)
    }
    player.agents <-
      all_agents$nameAgent
    player_df <-
      get_player_resolution_df()
    team_df <-
      get_leagues_teams_df()
    contract_status_df <-
      dictionary_contract_status()
    players_agents_df <-
      data_frame(
        namePlayer = player,
        nameAgent = player.agents,
        nameTeam,
        slugPosition,
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
        heightInches = heightPlayer %>% map_dbl(height_in_inches),
        idLeague = ifelse(idLeague %>% is.na, 'O', idLeague)
      ) %>%
      separate(
        slugPosition,
        into = c('slugPosition', 'slugPositionSecondary'),
        sep = '\\/'
      ) %>%
      dplyr::select(
        idLeague,
        nameTeam,
        namePlayer,
        agePlayer,
        nameAgent,
        slugPositionSecondary,
        slugPositionSecondary,
        heightPlayer,
        heightInches,
        everything()
      ) %>%
      arrange((idLeague), nameTeam) %>%
      suppressWarnings() %>%
      suppressMessages()

    df_status <-
      players_agents_df$statusContract %>%
      unique() %>%
      map_df(function(status) {
        parse_status(status = status)
      })

    players_agents_df <-
      players_agents_df %>%
      left_join(df_status) %>%
      suppressMessages()

    if (return_message) {
      "You got all realGM contract data" %>%
        message()
    }

    if (nest_data) {
      players_agents_df <-
        players_agents_df %>%
        nest(-nameAgent, .key = dataNBAPlayers)
    }

    players_agents_df
  }
