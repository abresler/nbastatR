options(warn = -1)
packages <-
  c(
    'rvest',
    'stringr',
    'lubridate' ,
    'stringi',
    'readr',
    'urltools',
    'data.table',
    'curl',
    'tidyr',
    'magrittr',
    'formattable',
    'httr',
    'dplyr',
    'readxl'
  )

lapply(packages, library, character.only = T)
get_bref_season_player_stats <- function(year_end_season, use_traded_player_team_total = F,
           stat_type = c("Advanced", "Totals", "Per Minute", "Per Game"),
           league = 'NBA') {
    bref_team_base <-
      'http://www.basketball-reference.com/leagues/'
    bref_base <-
      'http://www.basketball-reference.com'
    
    if (!stat_type %in% c("Advanced", "Totals", "Per Minute", "Per Game")) {
      stop.message <-
        "Not a valid table try " %>%
        paste0(c("Advanced", "Totals", "Per Minute", "Per Game") %>%
                 sample(size = 1))
      stop(stop.message)
    }
    
    stat_type <-
      stat_type %>%
      tolower %>%
      gsub("\\ ", '_', .)
    
    url <-
      bref_team_base %>%
      paste0(league,
             '_',
             year_end_season,
             '_',
             stat_type,
             '.html')
    css_page <-
      paste0('#', stat_type)
    
    css_player <-
      "td:nth-child(2) a"
    
    page <- 
      url %>%
      read_html()
    
    table <-
      page %>% 
      html_nodes(css_page) %>%
      html_table %>%
      data.frame %>%
      tbl_df
    
    names(table) %<>%
      tolower()
    
    table %<>%
      dplyr::filter(!g == 'G')
    
    numeric_cols <-
      table[, !names(table) %in% c('player', 'pos', 'tm')] %>% names
    
    table %<>%
      mutate_each_(funs(as.numeric), vars = numeric_cols) %>%
      select(-rk) %>%
      mutate(year.season = year_end_season) %>%
      select(year.season, everything())
    
    url.bref.player <-
      page %>% 
      html_nodes(css_player) %>%
      html_attr('href') %>% 
      paste0('http://www.basketball-reference.com',.)
    
    stem.player <- 
      page %>% 
      html_nodes(css_player) %>%
      html_attr('href') %>% 
      str_replace_all('/players/|.html', '') # eliminates the unnecassary words to help us get at the clean ID
    
    player <-
      page %>% 
      html_nodes(css_player) %>% 
      html_text
      
    players_urls <- # This will create a data frame with the information we want
      data_frame(player, url.bref.player, stem.player) %>%
      separate(stem.player, c('letter.first', 'id.bref.player'), #  Separates the remaining 2 parts by its delimiter the / we only want the second column which contains the id
               sep = '/') %>%
      select(-letter.first) %>%  # removes the unneeded column
      distinct
    
    table %<>% 
      left_join(players_urls)
    
    traded_players <- 
      table %>% 
      dplyr::filter(tm == 'TOT') %>% 
      .$player
    
    table %<>% 
      mutate(is.traded_player = ifelse(player %in% traded_players, T, F))
    
    if (use_traded_player_team_total == T) {
      traded_table <- 
        table %>% 
        dplyr::filter(player %in% traded_players, tm == "TOT")
      
      non_traded <-
        table %>% 
        dplyr::filter(!player %in% traded_players)
      
      data <- 
        non_traded %>% 
        bind_rows(traded_table) %>% 
        arrange(id.bref.player)
      
    } else {
      traded_table <- 
        table %>% 
        dplyr::filter(player %in% traded_players, !tm == "TOT")
      
      non_traded <-
        table %>% 
        dplyr::filter(!player %in% traded_players)
      
      data <- 
        non_traded %>% 
        bind_rows(traded_table) %>% 
        arrange(id.bref.player)
    }
    "Congrats you got BREF data for the " %>% 
    paste0(year_end_season, ' season') %>% 
      message
    return(data)
  }