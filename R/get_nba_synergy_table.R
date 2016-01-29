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

get_synergy_headers <- function() {
  synergy_option_df <-
    data_frame(
      name.table = c(
        "Post-Up",
        "Pick & Roll, Ball Handler",
        "Cut",
        "Handoff",
        "Isolation",
        "Miscellanous",
        "Off Screen",
        "Offensive Rebounds",
        "Pick & Roll, Roll Man",
        "Spot-Up",
        "Transition"
      ),
      stem.table = c(
        'Postup',
        "PRBallHandler",
        "Cut",
        "Handoff",
        "Isolation",
        "Misc",
        "OffScreen",
        "OffRebound",
        "PRRollMan",
        "Spotup",
        "Transition"
      )
    )

  header_df <-
    data_frame(
      name.nba = c(
        "GP",
        "Poss",
        "Time",
        "Points",
        "FGA",
        "FGM",
        "PPP",
        "WorsePPP",
        "BetterPPP",
        "TeamIDSID",
        "TeamName",
        "TeamNameAbbreviation",
        "TeamShortName",
        "PossG",
        "PPG",
        "FGAG",
        "FGMG",
        "FGmG",
        "FGm",
        "FG",
        "aFG",
        "FT",
        "TO",
        "SF",
        "PlusOne",
        "Score",
        "PlayerIDSID",
        "PlayerFirstName",
        "PlayerLastName",
        "PlayerNumber",
        "P"
      ),
      name.actual = c(
        "gp",
        "possesions",
        "pct.play_type",
        "pts",
        "fga",
        "fgm",
        "ppp",
        "ppp.worse",
        "ppp.better",
        "id.team",
        "team",
        "slug.team",
        "city.team",
        "possesions.per_game",
        "pts.per_game",
        "fga.per_game",
        "fgm.per_game",
        "fg.miss.per_game",
        "rank",
        "pct.fg",
        "pct.efg",
        "pct.ft_achieved",
        "pct.to",
        "pct.shooting_foul",
        "pct.and_1",
        "pct.scored",
        "id.player",
        "name.first",
        "name.last",
        "jersey",
        "id.position"
      )

    )

  data <-
    list(synergy_option_df, header_df)

  names(data) <-
    c('options', 'headers')

  return(data)

}
get_nba_synergy_stats <-
  function(table_name = "Transition",
           include_defense = T,
           include_offense = T,
           type_table = "team",
           return_message = T) {
    function_packages <- #need all of these installed including some from github
      c(
        'dplyr',
        'magrittr',
        'jsonlite',
        'tidyr',
        'purrr',
        'formattable',
        'stringr',
        'lubridate',
        'tidyr'
      )
    options(warn = -1)
    lapply(function_packages, library, character.only = T)
    install_needed_packages(function_packages)
    load_needed_packages(function_packages)

    if (!type_table %>% str_to_lower() %in% c("team", "player")) {
      stop("Sorry type of table can only be team or player", call. = F)
    }
    names.table <-
      c(
        "Post-Up",
        "Pick & Roll, Ball Handler",
        "Cut",
        "Handoff",
        "Isolation",
        "Miscellanous",
        "Off Screen",
        "Offensive Rebounds",
        "Pick & Roll, Roll Man",
        "Spot-Up",
        "Transition"
      )
    if (!table_name %in% names.table) {
      "Sorry must be either " %>%
        paste0(names.table %>% paste0(collapse = ", ")) %>%
        stop(call. = F)
    }

    table_metadata <-
      get_synergy_headers()

    headers_df <-
      table_metadata$headers

    synergy_option_df <-
      table_metadata$options

    stem <-
      synergy_option_df %>%
      dplyr::filter(name.table == table_name) %>%
      .$stem.table

    base <-
      'http://stats.nba.com/js/data/playtype/'

    if (type_table %>% str_to_lower == "team") {
      type <-
        "team_"
    } else {
      type <-
        "player_"
    }

    url.json <-
      base %>%
      paste0(type, stem, ".js")

    json_data <-
      url.json %>%
      fromJSON(simplifyDataFrame = T, flatten = T)

    headers <-
      json_data$resultSets$headers[1] %>% unlist

    data <-
      json_data$resultSets$rowSet[1] %>%
      data.frame %>%
      tbl_df

    actual_names <-
      1:length(headers) %>%
      purrr::map(function(x)
        data_frame(
          name.actual =
            headers_df %>%
            dplyr::filter(name.nba == headers[x]) %>%
            .$name.actual
        )) %>%
      bind_rows()

    names(data) <-
      actual_names$name.actual
    num_vars <-
      names(data)[!names(data) %in% c(
        'slug.team',
        'team',
        'city.team',
        'id.position',
        'name.first',
        'name.last',
        "id.team",
        "jersey"
      )]
    data %<>%
      mutate_each_(funs(extract_numeric(.) %>% digits(3)),
                   vars = num_vars) %>%
      mutate(
        is.offense = T,
        name.table = table_name,
        id.season = "2015-16",
        id.team = id.team %>% as.numeric,
        stem.table = stem
      ) %>%
      dplyr::select(name.table, id.season, is.offense, team, everything())

    if (include_defense == T) {
      if (json_data$resultSets$rowSet[2] %>%
          data.frame %>%
          tbl_df %>% nrow() > 0) {
        defense <-
          json_data$resultSets$rowSet[2] %>%
          data.frame %>%
          tbl_df

        names(defense) <-
          actual_names$name.actual

        num_vars <-
          names(defense)[!names(defense) %in% c(
            'slug.team',
            'team',
            'city.team',
            'id.position',
            'name.first',
            'name.last',
            "id.team",
            "jersey"
          )]

        defense %<>%
          mutate_each_(funs(extract_numeric(.) %>% digits(3)), vars = num_vars)

        defense %<>%
          mutate_each_(funs(extract_numeric(.) %>% digits(3)),
                       vars = num_vars) %>%
          mutate(
            is.offense = F,
            name.table = table_name,
            id.season = "2015-16",
            id.team = id.team %>% as.numeric(),
            stem.table = stem
          ) %>%
          dplyr::select(name.table, id.season, is.offense, team, everything())

        data <-
          data %>%
          bind_rows(defense) %>%
          arrange(team)

      }
    }

    if (type_table %>% str_to_lower() == "player") {
      data %<>%
        mutate(
          name.player = name.first %>% paste(name.last) %>% str_trim,
          id.player = id.player %>% as.numeric,
          jersey = jersey %>% as.numeric
        ) %>%
        dplyr::select(-c(name.first, name.last)) %>%
        dplyr::select(name.table,
                      id.season,
                      is.offense,
                      name.player,
                      team,
                      everything())
    }

    if (include_offense == F) {
      data %<>%
        dplyr::filter(is.offense == F)
    }

    if (return_message == T) {
      "Congrats you pulled in Synergy data for " %>%
        paste0(table_name %>% str_to_lower, " for ", type_table, "s") %>%
        message
    }
    return(data)

  }

get_all_team_synergy_stats <-
  function(include_defense = T,
           include_offense = T,
           return_message = T,
           tidy_data = T) {
    inc_o <-
      include_offense
    inc_d <-
      include_defense
    tables <-
      c(
        "Post-Up",
        "Pick & Roll, Ball Handler",
        "Cut",
        "Handoff",
        "Isolation",
        "Miscellanous",
        "Off Screen",
        "Offensive Rebounds",
        "Pick & Roll, Roll Man",
        "Spot-Up",
        "Transition"
      )

    all_data <-
      tables %>%
      purrr::map(
        function(x)
          get_nba_synergy_stats(
            table_name = x,
            type_table = "team",
            include_defense = inc_d,
            include_offense = inc_o,
            return_message = T
          )
      ) %>%
      compact %>%
      bind_rows

    if (tidy_data == T) {
      all_data %<>%
        dplyr::select(-c(name.table, id.team, city.team)) %>%
        gather(item,
               value,
               -c(id.season, team, slug.team, gp, stem.table, is.offense)) %>%
        mutate(
          item = item %>% as.character(),
          name.item = item %>% paste0('.', stem.table %>% str_to_lower())
        ) %>%
        dplyr::select(-c(stem.table, item)) %>%
        spread(name.item, value) %>%
        mutate(date.date = Sys.Date())

    }
    return(all_data)

  }

get_all_player_synergy_stats <-
  function(include_defense = T,
           include_offense = T,
           return_message = F,
           tidy_data = T) {
    inc_o <-
      include_offense
    inc_d <-
      include_defense
    tables <-
      c(
        "Post-Up",
        "Pick & Roll, Ball Handler",
        "Cut",
        "Handoff",
        "Isolation",
        "Miscellanous",
        "Off Screen",
        "Offensive Rebounds",
        "Pick & Roll, Roll Man",
        "Spot-Up",
        "Transition"
      )

    all_data <-
      tables %>%
      purrr::map(
        function(x)
          get_nba_synergy_stats(
            table_name = x,
            type_table = "player",
            include_defense = inc_d,
            include_offense = inc_o
          )
      ) %>%
      compact %>%
      bind_rows

    if (tidy_data == T) {
      ad <- all_data %>%
        dplyr::select(-c(name.table, id.team, city.team)) %>%
        gather(
          item,
          value,
          -c(
            id.season,
            team,
            slug.team,
            gp,
            stem.table,
            is.offense,
            name.player,
            jersey,
            id.player,
            id.position
          )
        ) %>%
        mutate(
          item = item %>% as.character(),
          name.item = item %>% paste0('.', stem.table %>% str_to_lower()),
          value = value %>% as.numeric()
        ) %>%
        dplyr::select(-c(item)) %>%
        spread(name.item, value) %>%
        mutate(date.date = Sys.Date()) %>%
        dplyr::select(-stem.table)

    }
    if (return_message == T) {
      "You got all player Synergy data" %>%
        message()
    }
    return(all_data)

  }



# get_all_team_tables -----------------------------------------------------

function() {

}


# get_all_player_tables ---------------------------------------------------

function() {

}
