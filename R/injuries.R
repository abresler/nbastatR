.get_player_names <-
  memoise::memoise(function(){
  data <-
    tibble(
    namePlayer = c("J.J. Barea", "Amare Stoudemire", "C.J. Watson", "Nene Hilario", "Wes Johnson", "A.J. Hammons",
                   "C.J. Wilcox"),
    nameNBA = c("Jose Juan Barea", "Amar'e Stoudemire", "CJ Watson", "Nene",
                "Wesley Johnson", "AJ Hammons",
                "CJ Wilcox")

  )
  return(data)

})


nba_injuries <-
  function(filter_returning_today = T) {
  url <-
    'http://www.rotoworld.com/teams/injuries/nba/all/'

  page <-
    url %>%
    read_html()

  players <-
    page %>%
    html_nodes('td:nth-child(1) a') %>%
    html_text()

  injury <-
    page %>%
    html_nodes('td:nth-child(2) span') %>%
    html_text()

  injuryTimeline <-
    page %>%
    html_nodes('td:nth-child(2) .impact') %>%
    html_text() %>%
    str_to_lower() %>%
    str_trim()

  dateInformation <-
    page %>%
    html_nodes('td:nth-child(2) .date') %>%
    html_text() %>%
    paste0(", ",
           Sys.Date() %>% year()
           ) %>%
    mdy() %>%
    as.Date()

  dateInjury <-
    page %>%
    html_nodes('tr+ tr td:nth-child(5)') %>%
    html_text() %>%
    paste0(", ",
           Sys.Date() %>% year()
    ) %>%
    mdy() %>%
    as.Date()

  injuryDetail <-
    page %>%
    html_nodes('.report') %>%
    html_text()

  injury_data <-
    tibble(dateInformation,
               namePlayer = players,
               dateInjury,
               injury,
               injuryTimeline,
               injuryDetail)

  player_ids <-
    nba_players()

  player_missing_df <-
    .get_player_names()

  injury_data <-
    injury_data %>%
    left_join(player_missing_df) %>%
    mutate(namePlayer = ifelse(nameNBA %>% is.na(), namePlayer, nameNBA)) %>%
    dplyr::select(-nameNBA) %>%
    suppressMessages()

  injury_data <-
    injury_data %>%
    left_join(
      player_ids %>%
        dplyr::select(
          namePlayer,
          slugTeam,
          urlPlayerHeadshot,
          idPlayer,
          urlPlayerActionPhoto
        )
    ) %>%
    dplyr::filter(!is.na(slugTeam)) %>%
    dplyr::select(slugTeam, namePlayer, everything()) %>%
    arrange(slugTeam) %>%
    suppressMessages()

  injury_data <-
    injury_data %>%
    mutate(isGTD = ifelse(injuryTimeline == 'day-to-day', T, F)) %>%
    dplyr::select(slugTeam, namePlayer, isGTD, everything())

  injury_data <-
    injury_data %>%
    separate(injuryTimeline, into = c('remove','dateReturn'),sep = 'target',
             remove = F) %>%
    dplyr::select(-remove) %>%
    mutate(dateReturn = dateReturn %>% gsub('\\ing','',.),
           dateReturn = dateReturn %>% gsub('\\.','',.) %>% str_trim(),
           dateReturn = ifelse(!dateReturn %>% is.na() & dateReturn %>% nchar < 7,
                                yes = dateReturn %>%
                                  paste0(", ",
                                         Sys.Date() %>% year()
                                  ) %>%
                                  mdy() %>%
                                  as.Date() %>% as.character, dateReturn)
           ) %>%
    suppressWarnings()

  if (filter_returning_today){
    active_players <-
      injury_data %>%
      dplyr::filter(
        dateReturn == Sys.Date() %>% as.character() |
          dateReturn == Sys.Date() %>% weekdays() %>% str_to_lower()
      ) %>%
      .$namePlayer
    if (active_players %>% length() >0 ) {
      injury_data <-
        injury_data %>%
        dplyr::filter(!namePlayer %in% active_players)
    }

  }
  injury_data
}
