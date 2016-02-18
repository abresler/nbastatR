source('R/utilities.R')

function_packages <-
  c(
    'dplyr',
    'magrittr',
    'tidyr',
    'stringr',
    'rvest',
    'lubridate',
    'httr',
    'purrr',
    'stringr',
    'data.table',
    'tidyr'
  )


# all star games ----------------------------------------------------------

get_all_nba_all_star_game_scores <-
  function(include_aba = T,
           return_message = T) {
    install_needed_packages(function_packages)
    load_needed_packages(function_packages)
    page <-
      'http://www.basketball-reference.com/allstar/' %>%
      read_html()

    year.season_end <-
      page %>%
      html_nodes('td:nth-child(1)') %>%
      html_text() %>%
      extract_numeric()

    id.season <-
      (year.season_end - 1) %>% as.character() %>%
      paste0("-", year.season_end %>% substr(start = 3, stop = 4))

    id.league <-
      page %>%
      html_nodes('td:nth-child(2)') %>%
      html_text

    url.season.league.bref <-
      page %>%
      html_nodes('td:nth-child(2) a') %>%
      html_attr('href') %>%
      paste0('http://www.basketball-reference.com', .)

    date.game <-
      page %>%
      html_nodes('td:nth-child(3)') %>%
      html_text %>%
      strptime('%b %d, %Y') %>%
      as.Date()

    scores_raw <-
      page %>%
      html_nodes('td:nth-child(5)') %>%
      html_text %>%
      str_replace("East,", "East") %>%
      str_replace("West,", "West") %>%
      str_replace("All Stars,", "All Stars")

    location <-
      page %>%
      html_nodes('td:nth-child(7)') %>%
      html_text %>%
      str_replace("Toronto,", "Toronto, ON")

    mvps <-
      page %>%
      html_table() %>%
      compact() %>%
      bind_rows() %>%
      .$MVP %>%
      str_replace("(?<=[a-z])(?=[A-Z])", ", ") %>%
      str_replace("Le, Bron", 'LeBron') %>%
      str_trim()


    ## Voting
    url.season.all_star_game.league.voting.bref <-
      page %>%
      html_nodes('td:nth-child(4) a') %>%
      html_attr('href') %>%
      paste0('http://www.basketball-reference.com', .)

    voting_df <-
      data_frame(
        url.season.all_star_game.league.voting.bref,
        year_league = url.season.all_star_game.league.voting.bref %>% str_replace('http://www.basketball-reference.com/allstar/', '') %>% str_replace('_voting.html', '')
      ) %>%
      separate(year_league,
               into = c('id.league', 'year.season_end'),
               sep = '\\_') %>%
      mutate(year.season_end = year.season_end %>% as.numeric,
             has.all_star.voting = T) %>%
      dplyr::select(id.league,
                    year.season_end,
                    has.all_star.voting,
                    everything())

    all_star_data <-
      data_frame(id.league,
                 id.season,
                 year.season_end,
                 date.game,
                 location,
                 name.mvp = mvps,
                 scores_raw) %>%
      mutate(
        url.season.all_star_game.league.bref = 'http://www.basketball-reference.com/allstar/' %>% paste0(id.league, "_", year.season_end, '.html')
      ) %>%
      separate(location,
               into = c('venue', 'city', 'state'),
               sep = '\\, ') %>%
      mutate(
        has.overtime = scores_raw %>% str_detect("OT"),
        scores_raw =
          scores_raw %>% str_replace_all("(OT)", '') %>% str_replace_all("(2OT)", '') %>%
          gsub('\\(2)', '', .) %>% str_replace('\\(', '') %>% str_replace('\\)', '')
      ) %>%
      separate(scores_raw,
               into = c('team.winner', 'team.loser'),
               sep = '\\, ') %>%
      mutate(
        score.winner = team.winner %>% extract_numeric,
        score.loser = team.loser %>% extract_numeric(),
        team.winner = team.winner %>% str_replace_all("[[:digit:]]", "") %>% str_trim,
        team.loser = team.loser %>% str_replace_all("[[:digit:]]", "") %>% str_trim
      ) %>%
      left_join(voting_df) %>%
      dplyr::select(
        id.league:state,
        has.overtime,
        has.all_star.voting,
        name.mvp,
        team.winner,
        score.winner,
        team.loser,
        score.loser,
        everything()
      ) %>%
      arrange(desc(year.season_end)) %>%
      suppressMessages() %>%
      suppressWarnings()


    if (include_aba == F) {
      all_star_data %<>%
        dplyr::filter(id.league == "NBA")
    }
    if (return_message == T) {
      "You got data for all " %>%
        paste0(all_star_data %>% nrow, " all star games.") %>%
        message()
    }
    return(all_star_data)
  }

