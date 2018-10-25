.dictionary_nbadraft_names <-
  memoise::memoise(function() {
    data_frame(nameBad = c("date.data.updated", "year.draft", "id.round", "id.pick", "name.player",
                           "id.position", "id.position.secondary", "team", "slug.team",
                           "slug.team.current", "is.traded_pick", "height", "height.inches",
                           "weight.lbs", "is.combo_player", "school", "slug.class", "url.player.nbadraft_net",
                           "id.class"),
    nameActual = c("dateUpdated", "yearDraft", "numberRound", "numberPickOverall", "namePlayer",
                   "slugPosition", "slugPositionSecondary", "nameTeamProjection", "slugTeamProjection",
                   "slugTeamCurrentProjection", "isPickTraded", "heightPlayer", "heightInches",
                   "weightLBS", "isComboPlayer", "nameSchool", "slugClass", "urlPlayerNBADraftNet",
                   "numberClass")
    )

  })

.get_nba_player_resolver_df <-
  memoise::memoise(function() {
  player_df <-
    data_frame(
      name.player = c(
        "BJ Mullens",
        "Darius Johnson-O...",
        "E'twaun Moore",
        "Giannis Adetokoubo",
        "Glenn Robinson III",
        "Guillermo Hernan...",
        "JaJuan Johnson P...",
        "James McAdoo",
        "Jeff Pendergraph",
        "Jeffery Taylor",
        "Joseph Young",
        "Kentavious Caldw...",
        "Kostas Papanikol...",
        "Marshon Brooks",
        "Matthew Bryan-Am...",
        "Michael Carter-W...",
        "Michael Kidd-Gil...",
        "Moe Harkless",
        "Patrick Mills",
        "Ricardo Ledo",
        "Rondae Hollis-Je...",
        "Thanasis Antetok...",
        "Tim Hardaway Jr",
        "Vyacheslav Kravt...",
        "Willie Cauley-St...",
        "DeAndre Bembry",
        "Dennis Smith",
        "Guillermo Hernangomez",
        "Kelly Oubre",
        "Kahlil Felder",
        "Johnny O'Bryant",
        "Otto Porter",
        "Sindarius Thornw...",
        "Timothe Luwawu",
        "Wade Baldwin",
        "Wesley Iwundu",
        "Walter Tavares",
        "Perry Jones",
        "James Ennis",
        "DJ Wilson",
        "Georgios Papagia..."
      ),
      name.player.nba =  c(
        "Byron Mullens",
        "Darius Johnson-Odom",
        "E'Twaun Moore",
        "Giannis Antetokounmpo",
        "Glenn Robinson",
        "Guillermo Hernangomez",
        "JaJuan Johnson",
        "James Michael McAdoo",
        "Jeff Ayres",
        "Jeff Taylor",
        "Joe Young",
        "Kentavious Caldwell-Pope",
        "Kostas Papanikolaou",
        "MarShon Brooks",
        "Matthew Bryan-Amaning",
        "Michael Carter-Williams",
        "Michael Kidd-Gilchrist",
        "Maurice Harkless",
        "Patty Mills",
        "Ricky Ledo",
        "Rondae Hollis-Jefferson",
        "Thanasis Antetokounmpo",
        "Tim Hardaway Jr.",
        "Vyacheslav Kravtsov",
        "Willie Cauley-Stein",
        "DeAndre' Bembry",
        "Dennis Smith Jr.",
        "Willy Hernangomez",
        "Kelly Oubre Jr.",
        "Kay Felder",
        "Johnny O'Bryant III",
        "Otto Porter Jr.",
        "Sindarius Thornwell",
        "Timothe Luwawu-Cabarrot",
        "Wade Baldwin IV",
        "Wes Iwundu",
        "Edy Tavares",
        "Perry Jones III",
        "James Ennis III",
        "Wilson, D.J.",
        "Georgios Papagiannis"
      )
    )
  return(player_df)
})

.get_school_df  <-
  memoise::memoise(function() {
  school_df <-
    data_frame(
      school = c(
        "Bosnia & Herz...",
        "Long Beach St...",
        "Louisiana Laf...",
        "Rep. of Georgia",
        "South Dakota ...",
        "Washington St...",
        "Wisconsin Gre..."
      ),
      school.actual = c(
        "Bosnia",
        "Long Beach State",
        "Louisiana Lafayette",
        "Republic of Georgia",
        "South Dakota State",
        "Washington State",
        "Wisconsin Green Bay"
      )
    )
  return(school_df)
})

.get_nba_draft_net_team_df <-
  memoise::memoise(function() {
  team_df <-
    data_frame(
      name.team.nbadraft_net = c(
        "Atlanta",
        "Boston",
        "Brooklyn",
        "Charlotte",
        "Chicago",
        "Cleveland",
        "Dallas",
        "Denver",
        "Detroit",
        "Golden St.",
        "Houston",
        "Indiana",
        "LA Clippers",
        "LA Lakers",
        "Memphis",
        "Miami",
        "Milwaukee",
        "Minnesota",
        "New Jersey",
        "New Orleans",
        "New Orleans Old",
        "New York",
        "Oklahoma Cty",
        "Orlando",
        "Philadelphia",
        "Phoenix",
        "Portland",
        "Sacramento",
        "San Antonio",
        "Toronto",
        "Utah",
        "Washington",
        'Charlotte Old'
      ),
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
        "New Jersey Nets",
        "New Orleans Pelicans",
        "New Orleans Hornets",
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
        "Washington Wizards",
        "Charlotte Bobcats"
      ),
      team.current = c(
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
        "New Jersey Nets",
        "New Orleans Pelicans",
        "New Orleans Hornets",
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
        "Washington Wizards",
        "Charlotte Hornets"
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
        "NJN",
        "NOP",
        "NOH",
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
        "WAS",
        'CHA'
      ),
      slug.team.current = c(
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
        "BKN",
        "NOP",
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
        "WAS",
        'CHA'
      )
    )

  return(team_df)
})


.get_nba_draftnet_year_mock_draft <-
  function(draft_year = 2013,
           return_message = T) {
    if (!'draft_year' %>% exists()) {
      stop('Please enter a draft year')
    }
    year_now <-
      Sys.Date() %>%
      lubridate::year()

    month_now <-
      Sys.Date() %>%
      lubridate::month()

    if (month_now %in% c(1:6)) {
      max_draft_year <-
        draft_year + 1
    } else {
      max_draft_year <-
        draft_year + 2
    }

    if (draft_year > max_draft_year) {
      stop("Sorry mock drafts only go through " %>% paste0(max_draft_year))
    }

    if (draft_year < 2009) {
      stop("Sorry mock drafts only go from 2009")
    }
    class_df <-
      data_frame(
        slug.class = c("fr", 'so', 'jr', 'sr', "intl"),
        id.class = c(1:4, NA)
      )
    url <-
      'http://www.nbadraft.net/' %>%
      paste0(draft_year, 'mock_draft')

    page <-
      url %>%
      xml2::read_html()

    last_update <-
      page %>%
      html_nodes('.updated') %>%
      html_text() %>%
      str_replace('Updated: ', '') %>%
      str_split('\\ ') %>%
      unlist %>%
      .[1] %>%
      as.Date('%m/%d/%y')

    name.team.nbadraft_net <-
      page %>%
      html_nodes('strong') %>%
      html_text() %>%
      str_trim()

    name.player <-
      page %>%
      html_nodes('td:nth-child(3)') %>%
      html_text() %>%
      str_trim()

    url.player.nbadraft_net <-
      page %>%
      html_nodes('td:nth-child(3) a') %>%
      html_attr('href') %>%
      paste0('http://www.nbadraft.net/players', .)

    height <-
      page %>%
      html_nodes('td:nth-child(4)') %>%
      html_text()

    weight.lbs <-
      page %>%
      html_nodes('td:nth-child(5)') %>%
      html_text() %>%
      as.character() %>%
      readr::parse_number()

    id.position <-
      page %>%
      html_nodes('td:nth-child(6)') %>%
      html_text()

    school <-
      page %>%
      html_nodes('td:nth-child(7)') %>%
      html_text()

    slug.class <-
      page %>%
      html_nodes('td:nth-child(8)') %>%
      html_text() %>%
      str_replace('\\.', '') %>%
      str_to_lower()

    team_df <-
      .get_nba_draft_net_team_df()

    player_df <-
      .get_nba_player_resolver_df()
    school_df <-
      .get_school_df()

    draft_data <-
      data_frame(
        date.data.updated = last_update,
        year.draft = draft_year,
        id.pick = seq_along(name.player),
        name.player,
        height,
        weight.lbs,
        id.position,
        school,
        slug.class,
        url.player.nbadraft_net
      ) %>%
      left_join(player_df) %>%
      left_join(school_df) %>%
      suppressMessages()

    if (name.team.nbadraft_net %>% length() > 0 ){
      draft_data <-
        draft_data %>%
        mutate(
          name.team.nbadraft_net,
          is.traded_pick = name.team.nbadraft_net %>% str_detect('\\*'),
          name.team.nbadraft_net = name.team.nbadraft_net %>% str_replace('\\*', '')
        )

      if (draft_year <= 2013) {
        draft_data$name.team.nbadraft_net  <-
          draft_data$name.team.nbadraft_net %>%
          str_replace('Charlotte', "Charlotte Old")
      }
      draft_data <-
        draft_data %>%
        left_join(team_df) %>%
        suppressMessages()
    } else {
      draft_data <-
        draft_data %>%
        mutate(
          name.team.nbadraft_net = NA)
    }
    draft_data <-
      draft_data %>%
      mutate(
        name.player = ifelse(!name.player.nba %>% is.na(), name.player.nba, name.player),
        height.inches = height %>% lapply(height_in_inches) %>% unlist,
        id.round = ifelse(id.pick <= 30, 1, 2),
        school = ifelse(!school.actual %>% is.na, school.actual, school),
        is.combo_player = id.position %>% str_detect('\\/')
      ) %>%
      separate(
        id.position,
        into = c('id.position', 'id.position.secondary'),
        sep = '\\/'
      ) %>%
      suppressMessages() %>%
      suppressWarnings()

    draft_data <-
      draft_data %>%
      left_join(class_df) %>%
      dplyr::select(-one_of(c(
        "name.team.nbadraft_net",
        "school.actual",
        "name.player.nba",
        "team.current"
      ))) %>%
      suppressMessages() %>%
      suppressWarnings()

    if (return_message) {
      "You the got nbadraft.net mock draft for the " %>%
        paste0(draft_year, ' NBA Draft\nData last updated on ', last_update) %>%
        cat(fill = T)
    }
    draft_data
  }


#' NBADraft.net mock drafts
#'
#' Returns mock drafts from nbadraft.net
#' for specified years
#'
#' @param years vector of draft years
#' @param merge_nba_data if \code{TRUE} merges NBA player data
#' @param nest_data if \code{TRUE} nests data by draft
#' @param return_message  if \code{TRUE} returns a message
#'
#' @return a \code{data_frame()}
#' @export
#' @import dplyr lubridate purrr stringr tibble tidyr rvest
#' @importFrom glue glue
#' @family draft
#' @examples
#' nbadraftnet_mock_drafts(years = 2012:2017,
#' merge_nba_data = TRUE,
#' nest_data = F,
#' return_message = T)

nbadraftnet_mock_drafts <-
  function(years = 2009:2018,
           merge_nba_data = T,
           nest_data = F,
           return_message = T) {
    .get_nba_draftnet_year_mock_draft_safe <-
      purrr::possibly(.get_nba_draftnet_year_mock_draft, data_frame())

    all_data <-
      years %>%
      future_map_dfr(function(draft_year){
        .get_nba_draftnet_year_mock_draft_safe(draft_year = draft_year,
                                              return_message = return_message)
      })

    df_actual_names <-
        .dictionary_nbadraft_names()

      actual_names <-
        names(all_data) %>%
        map_chr(function(name){
          no_name <-
            df_actual_names %>%
            filter(nameBad == name) %>%
            nrow() == 0

          if (no_name) {
            glue::glue("Missing {name} in dictionary") %>% cat(fill = T)
            return(name)
          }
          df_actual_names %>%
            filter(nameBad == name) %>%
            pull(nameActual) %>%
            unique() %>%
            .[[1]]
        })

    all_data <-
      all_data %>%
      purrr::set_names(actual_names)

    if (merge_nba_data) {
      df_nba_player_dict <- nba_players()

      all_data <-
        all_data %>%
        left_join(df_nba_player_dict %>% select(
          one_of(
            "isActive",
            "isRookie",
            "namePlayer",
            "idPlayer",
            "countSeasons",
            "yearSeasonFirst",
            "yearSeasonLast",
            "slugPlayer",
            "idTeam",
            "cityTeam",
            "teamName",
            "slugTeam",
            "codeTeam",
            "hasGamesPlayedFlag",
            "urlPlayerStats",
            "urlPlayerThumbnail",
            "urlPlayerHeadshot",
            "urlPlayerActionPhoto"
          )
        )) %>%
        suppressMessages()

      all_data <-
        all_data %>%
        mutate(isNBAPlayer = ifelse(!idPlayer %>% is.na(), TRUE, FALSE),
               heightInches = heightPlayer %>% map_dbl(height_in_inches),
               slugClass = slugClass %>% str_to_upper() ) %>%
        dplyr::select(dateUpdated:namePlayer, isNBAPlayer, everything())
    }

    if (nest_data) {
      all_data <-
        all_data %>%
        tidyr::nest(-c(yearDraft), .key = dataDraft)
    }
    all_data
  }
