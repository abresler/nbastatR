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
      search() %>%
      gsub('package:', '', .)
    
    package_to_load <-
      required_packages[!required_packages %in% loaded_packages]
    if (package_to_load %>% length > 0) {
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
      name.player = c(
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
      name.player.nba = c(
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
    data_frame(contract.status = c(NA, "Unrestricted Free Agent", "Unsigned Draft Pick", "Draft Eligible", 
    "Restricted Free Agent"),
    id.contract.status = c('OT', 'UFA', 'UDP', 'DE', 'RFA')
    )
  
  return(contract_df)
}

get_leagues_teams_df <- function() {
  nba_teams <- 
    data_frame(team = c("Atlanta Hawks", "Boston Celtics", "Brooklyn Nets", "Charlotte Hornets", 
                      "Chicago Bulls", "Cleveland Cavaliers", "Dallas Mavericks", "Denver Nuggets", 
                      "Detroit Pistons", "Golden State Warriors", "Houston Rockets", 
                      "Indiana Pacers", "Los Angeles Clippers", "Los Angeles Lakers", 
                      "Memphis Grizzlies", "Miami Heat", "Milwaukee Bucks", "Minnesota Timberwolves", 
                      "New Jersey Nets", "New Orleans Hornets", "New Orleans Pelicans", 
                      "New York Knicks", "Oklahoma City Thunder", "Orlando Magic", 
                      "Philadelphia 76ers", "Phoenix Suns", "Portland Trail Blazers", 
                      "Sacramento Kings", "San Antonio Spurs", "Toronto Raptors", "Utah Jazz", 
                      "Washington Wizards"),
             id.league = 'NBA'
  )
  
  nba_dl_teams <- 
    data_frame(team = c("Delaware 87ers", "Erie BayHawks", "Maine Red Claws", "Raptors 905", 
                 "Westchester Knicks", "Canton Charge", "Fort Wayne Mad Ants", 
                 "Grand Rapids Drive", "Iowa Energy", "Sioux Falls Skyforce", 
                 "Austin Spurs", "Oklahoma City Blue", "Rio Grande Valley Vipers", 
                 "Texas Legends", "Bakersfield Jam", "Idaho Stampede", "Los Angeles D-Fenders", 
                 "Reno Bighorns", "Santa Cruz Warriors"),
               id.league = 'NBDL'
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
    data_frame(agent = name.agent, url.agent.realgm) %>% 
    distinct() %>% 
    arrange(agent)
  
  return(agent_df)
  
}

parse_agent_metadata <- function(page) {
  agent <- 
    page %>% 
    html_nodes('.force-table tr:nth-child(1) td') %>% 
    html_text()
  
  agency <- 
    page %>% 
    html_nodes('.force-table tr:nth-child(2) td') %>% 
    html_text()
  
  url.agency <- 
    page %>% 
    html_nodes('.force-table tr:nth-child(3) td a') %>% 
    html_attr('href')
  
  agent_metadata_df <- 
    data_frame(agent, agency, url.agency)
  
  return(agent_metadata_df)
}
parse_agent_player_table <- function(page) {
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
    team %>% grep('http',.) + 1
  
  team <- 
    team[start_teams:(length(team))]
  
  team %<>% 
    gsub("N/A", NA,.)
  
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
    extract_numeric()
  
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
    extract_numeric()
  
  
  contract.status <- 
    page %>% 
    html_nodes('td:nth-child(8)') %>% 
    html_text()
  
  years.played <- 
    page %>% 
    html_nodes('td:nth-child(9)') %>% 
    html_text() %>% 
    extract_numeric()
  
  team_df <-
    get_leagues_teams_df()
  contract_status_df <- 
    get_contract_status_ids()
  player_df <- 
    get_player_resolution_df()
  player_agent_df <- 
    data_frame(name.player = player,
               team,
               id.position,
               height,
               weight.lbs = weight,
               date.birth,
               age,
               years.played,
               contract.status,
               url.player.realgm
    ) %>% 
    left_join(team_df) %>% 
    left_join(player_df) %>% 
    mutate(name.player = ifelse(!name.player.nba %>% is.na(), name.player.nba, name.player),
           height.inches = height %>% lapply(height_in_inches) %>% unlist,
           id.league = ifelse(id.league %>% is.na, 'O', id.league)) %>% 
    dplyr::select(-name.player.nba) %>% 
    separate(id.position, into = c('id.position', 'id.position.secondary'), sep = '\\/') %>% 
    dplyr::select(id.league, team, 
                  name.player, age, id.position,
                  id.position.secondary, height, height.inches,
                  everything()) %>% 
    
    arrange((id.league), team) %>% 
    separate(contract.status, into = c('contract.status', 'team.has_rights'),
             sep = '\\(') %>% 
    mutate(team.has_rights = team.has_rights %>% str_replace('\\)', '') %>% str_trim(),
           contract.status = contract.status %>% str_trim %>% str_replace('\\,','\\:' )
    ) %>% 
    separate(contract.status, into = c('contract.status', 'date.contract'), 
             sep = '\\: ') %>% 
    mutate(contract.status = ifelse(contract.status == team, NA, contract.status),
           date.contract = date.contract %>% strptime('%b %d, %Y') %>% 
             as.Date()) %>% 
    mutate(contract.status = contract.status %>% str_trim %>% str_replace(' in ',':' )) %>% 
    separate(contract.status, into = c('contract.status', 'year.draft_eligible'), 
             sep = '\\:') %>% 
    mutate(year.draft_eligible =  year.draft_eligible %>% as.numeric()) %>% 
    left_join(contract_status_df) %>% 
    dplyr::select(id.league:contract.status, id.contract.status,
                  everything())
  return(player_agent_df)
  
}

get_agent_metadata_df <- 
  function(agent_name = "Jeff Schwartz") {
  if(!'agent_name' %>% exists()) {
    stop("Please enter an agent")
  }
  load_packages(function_packages)
  install_needed_packages(function_packages)
  if(!'all_agent_df' %>% exists){
  all_agent_df <- 
    get_agents_urls()
  }
  
  if (!agent_name  %in% all_agent_df$agent) {
    stop("Sorry " %>% paste0(agent_name, ' is not a valid agent.'))
  }
  
  url <- 
    all_agent_df %>% 
    dplyr::filter(agent == agent_name) %>% 
    .$url.agent.realgm
  
  page <-
    url %>% 
    read_html()
  
  metadata_df <- 
    page %>% 
    parse_agent_metadata()
  
  return(metadata_df)
  }

get_agent_player_df <- 
  function(agent_name = "Jeff Schwartz") {
    if(!'agent_name' %>% exists()) {
      stop("Please enter an agent")
    }
    load_packages(function_packages)
    install_needed_packages(function_packages)
    if(!'all_agent_df' %>% exists){
      all_agent_df <- 
        get_agents_urls()
    }
    
    if (!agent_name  %in% all_agent_df$agent) {
      stop("Sorry " %>% paste0(agent_name, ' is not a valid agent.'))
    }
    
    url <- 
      all_agent_df %>% 
      dplyr::filter(agent == agent_name) %>% 
      .$url.agent.realgm
    
    page <-
      url %>% 
      read_html()
    
    agent_player_df <- 
      page %>% 
      parse_agent_player_table()
    
    return(agent_player_df)
  }

get_agent_player_data_df <- 
  function(agent_name = "Jeff Schwartz", return_message = T) {
    if(!'agent_name' %>% exists()) {
      stop("Please enter an agent")
    }
    load_packages(function_packages)
    install_needed_packages(function_packages)
    if(!'all_agent_df' %>% exists){
      all_agent_df <- 
        get_agents_urls()
    }
    
    if (!agent_name  %in% all_agent_df$agent) {
      stop("Sorry " %>% paste0(agent_name, ' is not a valid agent.'))
    }
    
    url <- 
      all_agent_df %>% 
      dplyr::filter(agent == agent_name) %>% 
      .$url.agent.realgm
    
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
                 mutate(agent = agent_name))
   if (return_message == T) {
     "You data for all " %>% 
       paste0(agent_data %>% nrow(), " players represented by\n",
              agent_name,' from ', agent_metadata_df$agency)
   }
   return(agent_data)
     
  }
get_agent_player_data_df_safe <- 
  failwith(NULL, get_agent_player_data_df)

get_agents_player_data_df <- function(agents_names, is.all_agents = T,
                         message = F) {
  if (is.all_agents == F &!'agents_names' %>% exists()){
    stop("Please enter agents")
  }
  
  if(is.all_agents == T) {
    if(!'all_agent_df' %>% exists) {
      all_agent_df <- 
        get_agents_urls()
    }
    agents_names <- 
      all_agent_df$agent
  }
  all_agents_players <- 
    agents_names %>% 
    map(function(x) get_agent_player_data_df_safe(x)) %>% 
    compact() %>% 
    bind_rows
  return(all_agents_players)
}

get_players_agents <- function(return_message = F) { 
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
    data.frame()
  
  
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
    extract_numeric()
  
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
    extract_numeric()
    
  player.agent <- 
    page %>% 
    html_nodes('td:nth-child(8)') %>% 
    html_attr('rel')
  
  contract.status <- 
    page %>% 
    html_nodes('td:nth-child(9)') %>% 
    html_attr('rel')
  
  players.agents <- 
    table$Agent %>% Ecfun::camelParse()
  all_agents <- 
    data_frame()
  for(a in 1:length(players.agents)) {
    agents_names <-
      players.agents[a] %>% 
      unlist %>% 
      paste0(collapse = ', ')
    
    agent_df <- data_frame(agent = agents_names)
    all_agents %<>% 
      bind_rows(agent_df)
  }
  player.agents <- 
    all_agents$agent
  player_df <-
    get_player_resolution_df()
  team_df <-
    get_leagues_teams_df()
  contract_status_df <- 
    get_contract_status_ids()
  players_agents_df <- 
    data_frame(name.player = player,
               agent = player.agents,
               team,
               id.position,
               height,
               weight.lbs = weight,
               date.birth,
               age,
               contract.status,
               url.player.realgm
               ) %>% 
    left_join(team_df) %>% 
    left_join(player_df) %>% 
    mutate(name.player = ifelse(!name.player.nba %>% is.na(), name.player.nba, name.player),
           height.inches = height %>% lapply(height_in_inches) %>% unlist,
           id.league = ifelse(id.league %>% is.na, 'O', id.league)) %>% 
    separate(id.position, into = c('id.position', 'id.position.secondary'), sep = '\\/') %>% 
    dplyr::select(id.league, team, 
                  name.player, age, agent, id.position,
                  id.position.secondary, height, height.inches,
                  everything()) %>% 
    
    arrange((id.league), team)
  
  players_agents_df %<>% 
    separate(contract.status, into = c('contract.status', 'team.has_rights'),
             sep = '\\(') %>% 
    mutate(team.has_rights = team.has_rights %>% str_replace('\\)', '') %>% str_trim(),
           contract.status = contract.status %>% str_trim %>% str_replace('\\,','\\:' )
    ) %>% 
    separate(contract.status, into = c('contract.status', 'date.contract'), 
             sep = '\\: ') %>% 
    mutate(contract.status = ifelse(contract.status == team, NA, contract.status),
           date.contract = date.contract %>% strptime('%b %d, %Y') %>% 
             as.Date()) %>% 
  mutate(contract.status = contract.status %>% str_trim %>% str_replace(' in ',':' )) %>% 
  separate(contract.status, into = c('contract.status', 'year.draft_eligible'), 
           sep = '\\:') %>% 
    mutate(year.draft_eligible =  year.draft_eligible %>% as.numeric()) %>% 
    left_join(contract_status_df) %>% 
    dplyr::select(id.league:contract.status, id.contract.status,
                  everything())
  
  if (return_message == T) {
    "You got all realGM contract data" %>% 
      message()
  }
  return(players_agents_df)
  }