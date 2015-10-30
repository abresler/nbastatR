packages <-
  c('rvest',
    'dplyr',
    'magrittr',
    'httr',
    'purrr',
    'tidyr',
    'stringr',
    'lubridate')
lapply(packages, library, character.only = T)

get_headers_css_data <- function() {
  headers <-
    data_frame(
      name.bref = c(
        "2p.1" ,
        "2p.2",
        "2pa",
        "3p.1",
        "3p.2",
        "3pa",
        "3par",
        "age",
        "ast",
        "ast%",
        "blk",
        "blk%",
        "bpm",
        "dbpm",
        "drb",
        "drb%",
        "drtg",
        "dws",
        "efg%",
        "fg",
        "fg%",
        "fga",
        "ft",
        "ft%",
        "fta",
        "ftr",
        "g",
        "gs",
        "mp",
        "na",
        "obpm",
        "orb",
        "orb%",
        "ortg",
        "ows",
        "per",
        "pf",
        "player",
        "pos",
        "pts",
        "rk",
        "stl",
        "stl%",
        "tm",
        "tov",
        "tov%",
        "trb",
        "trb%",
        "ts%",
        "usg%",
        "vorp",
        "ws",
        "ws/48"
      ),
      name.actual =
        c(
          "fg2m",
          'pct.fg2m',
          "fg2a",
          "fg3m",
          'pct.fg3m',
          "f3a",
          "fg3.par",
          "age.player",
          "ast",
          "pct.ast",
          "blk",
          "pct.blk",
          "bpm",
          "bpm.defense",
          "dreb",
          "pct.dreb",
          "drtg",
          "ws.defense",
          "pct.efg",
          "fgm",
          "pct.fg",
          "fga",
          "ftm",
          "pct.ft",
          "fta",
          "ftr",
          "gp",
          "gs",
          "min",
          "na",
          "bpm.offense",
          "oreb",
          "pct.oreb",
          "ortg",
          "ws.offense",
          "per",
          "pf",
          "name.player",
          "id.pos",
          "pts",
          "rk",
          "stl",
          "pct.stl",
          "slug.team.bref",
          "tov",
          "pct.tov",
          "reb",
          "pct.reb",
          "pct.ts",
          "pct.usg",
          "vorp",
          "ws",
          "ws.48"
        ),
      id.row = 1:length(name.actual)
    )
  tables <-
    data_frame(
      name.table = c(
        "Totals",
        "Per Game",
        "Per 36 Minutes",
        "Per 100 Poss",
        "Advanced"
      ),
      css.table = c(
        "#totals",
        "#per_game",
        "#per_minute",
        '#per_poss',
        "#advanced"
      ),
      slug.table = c("totals",
                     "per_game", "per_minute", 'per_poss', "advanced")
    )
  data <-
    list(tables, headers)

  names(data) <-
    c('tables', 'headers')
  return(data)
}
get_bref_player_season_stat_table <-
  function(season_start = 2015,
           stat_type = 'Totals',
           use_totals = T,
           return_message = T) {
    base <-
      'http://www.basketball-reference.com/leagues/'
    season_end <-
      season_start + 1

    season <-
      season_start %>%
      paste0("-", season_end %>% substr(3, 4))

    league <-
      'NBA'

    stat_types <-
      c("Totals",
        "Per Game",
        "Per 36 Minutes",
        "Per 100 Poss",
        "Advanced")

    if (!stat_type %in% stat_types) {
      "Sorry you can only pick " %>%
        paste0(stat_types %>% paste0(collapse = ", ")) %>%
        stop(call. = F)
    }

    th <-
      get_headers_css_data()

    tables <-
      th$tables

    headers_df <-
      th$headers

    slug <-
      tables %>%
      dplyr::filter(name.table == stat_type) %>%
      .$slug.table

    css <-
      '#' %>%
      paste0(slug)

    url <-
      'http://www.basketball-reference.com/leagues/NBA_' %>%
      paste0(season_end, '_', slug, '.html')

    page <-
      url %>%
      read_html()

    table <-
      page %>%
      html_nodes(css) %>%
      html_table(header = F) %>%
      data.frame %>%
      tbl_df

    headers <-
      table %>% slice(1) %>% as.character() %>%
      str_to_lower()

    names_bref_df <-
      data_frame(name.bref = headers, val = 1) %>%
      group_by(name.bref) %>%
      mutate(count = cumsum(val)) %>%
      left_join(data_frame(name.bref = headers) %>%
                  group_by(name.bref) %>%
                  tally()) %>%
      dplyr::rename(total.count = n) %>%
      mutate(names.bref = ifelse(
        total.count == 1,
        name.bref,
        name.bref %>% paste0('.', count)),
        names.bref = ifelse(name.bref == "na", "na", names.bref)
      )

    table %<>%
      slice(2:nrow(table))

    headers <-
      names_bref_df$names.bref

    actual_names <-
      1:length(headers) %>%
      purrr::map(
        function(x)
          data_frame(
            name.actual =
              headers_df %>%
              mutate(name.bref = name.bref %>% str_to_lower) %>%
              dplyr::filter(name.bref == headers[x]) %>%
              .$name.actual
          )
      ) %>%
      bind_rows()

    names(table) <-
      actual_names$name.actual

    if ("na" %in% (table %>% names)) {
      table <-
        table[, !table %>% names %in% "na"]
    }

    table %<>%
      dplyr::filter(!rk == 'Rk') %>%
      dplyr::select(-rk) %>%
      mutate(
        id.season = season,
        name.table = stat_type,
        year.season_start = season_start
      ) %>%
      dplyr::select(id.season,
                    name.table,
                    name.player,
                    id.pos,
                    slug.team.bref,
                    everything())

    table %<>%
      mutate_each_(funs(extract_numeric), vars =
                     table %>% dplyr::select(-c(id.season:slug.team.bref)) %>% names) %>%
      mutate(
        is.hof = name.player %>% str_detect('\\*'),
        name.player = name.player %>% str_replace("\\*", '')
      )

    ## Player ID Extraction
    player <-
      page %>%
      html_nodes('td:nth-child(2) a') %>% # This is the column where the player IDs live
      html_text %>% # this takes the html output and returns text
      str_trim # To be safe this trims the code in case there are any unceassary white spaces

    url.bref.player <- # This creates the player URL
      page %>%
      html_nodes('td:nth-child(2) a') %>%
      html_attr('href') %>% # This function pulls in the html attribute, in this case the stem
      paste0('http://www.basketball-reference.com', .) # We need to append the base URL!

    stem.player <-
      # Here we are going to extract out from the steam the exact player ID
      page %>%
      html_nodes('td:nth-child(2) a') %>%
      html_attr('href') %>%
      str_replace_all('/players/|.html', '') # eliminates the unnecassary words to help us get at the clean ID

    players_urls <-
      # This will create a data frame with the information we want
      data_frame(name.player = player, url.bref.player, stem.player) %>%
      separate(stem.player, c('letter.first', 'id.bref.player'), #  Separates the remaining 2 parts by its delimiter the / we only want the second column which contains the id
               sep = '/') %>%
      dplyr::select(-letter.first) # removes the unneeded column

    ## Resolve the team ids

    table %<>%
      left_join(players_urls)

    teams_ids <-
      'http://asbcllc.com/data/nba/bref/nba_teams_ids.csv' %>%
      read_csv # imports my team data

    teams_ids %<>%
      dplyr::rename(slug.team.bref = id.bref.team,
             slug.team.bref.current = id.bref.current_team)

    table %<>%
      left_join(teams_ids) %>%
      distinct

    if (use_totals == T) {
      traded <-
        table %>%
        dplyr::filter(slug.team.bref == "TOT")

      non.trade <-
        table %>%
        dplyr::filter(!player %in% traded$name.player)

      table <-
        traded %>%
        bind_rows(non.trade) %>%
        arrange(name.player)
    } else {
      table %<>%
        dplyr::filter(!slug.team.bref == "TOT")
    }

    if (return_message == T) {
      "You got " %>%
        paste0(stat_type, " Stats for the ",
               season, " Season") %>% message()

    }
    return(table)
  }
