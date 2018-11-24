# gdeltr2::load_needed_packages(c("dplyr", "curl", "tidyr", "rvest", "tidyr", "stringr", "readr", "purrr", "glue"))


# csv ---------------------------------------------------------------------

.munge_seasons <-
  memoise::memoise(function(season) {
    if (!season %>% str_detect("\\-")) {
      s <- as.numeric(season)
      df <-
        data_frame(
          slugSeasons = season,
          seasonFirst = s,
          seasonLast = s,
          countSeasons = seasonLast - seasonFirst
        )

      return(df)
    }
    season_parts <-
      season %>% str_split("\\-") %>% flatten_chr() %>% as.numeric()
    df <-
      data_frame(
        slugSeasons = season,
        seasonFirst = season_parts[1],
        seasonLast = season_parts[2],
        countSeasons = seasonLast - seasonFirst
      )
    df
  })

#' Basketball Reference coach dictionary
#'
#' @return a `data_frame`
#' @export
#' @import dplyr purrr readr stringr
#' @importFrom glue glue
#' @family dictionary
#' @family BREF
#' @family Coaching
#' @examples
#' dictionary_bref_coaches()
dictionary_bref_coaches <-
  memoise::memoise(function() {
    data <-
      "https://d2cwpp38twqe55.cloudfront.net/short/inc/coaches_search_list.csv" %>%
      read_csv(col_names = F) %>%
      dplyr::select(1:4) %>%
      purrr::set_names(c(
        "slugCoachBREF",
        "nameCoach",
        "slugSeasons",
        "isActiveCoach"
      )) %>%
      mutate(isActiveCoach = isActiveCoach %>% as.logical()) %>%
      suppressWarnings() %>%
      suppressMessages()

    data <-
      data %>%
      mutate(
        urlCoachBREF = glue::glue(
          "https://www.basketball-reference.com/coaches/{slugCoachBREF}.html"
        ) %>% as.character()
      )

    data <-
      data %>%
      mutate(slugSeasons = ifelse(slugSeasons == "-", NA, slugSeasons))

    seasons <-
      data %>%
      filter(!is.na(slugSeasons)) %>%
      distinct(slugSeasons) %>%
      pull(slugSeasons)

    df_seasons <-
      seasons %>%
      future_map_dfr(function(season) {
        .munge_seasons(season = season)
      })

    data <-
      data %>%
      left_join(df_seasons) %>%
      select(
        nameCoach,
        slugCoachBREF,
        isActiveCoach,
        slugSeasons,
        seasonFirst,
        seasonLast,
        everything()
      ) %>%
      suppressMessages()
    data
  })

#' Basketball Reference team dictionary
#'
#'
#' @return a `data_frame`
#' @export
#' @import dplyr purrr readr stringr
#' @importFrom glue glue
#' @family dictionary
#' @family BREF
#' @examples
#' dictionary_bref_teams()
dictionary_bref_teams <-
  memoise::memoise(function() {
    data <-
      "https://d2cwpp38twqe55.cloudfront.net/short/inc/teams_search_list.csv" %>%
      read_csv(col_names = F) %>%
      dplyr::select(1:4) %>%
      purrr::set_names(c(
        "slugTeamBREF",
        "nameTeamBREF",
        "slugSeasons",
        "isActiveTeam"
      )) %>%
      mutate(isActiveTeam = isActiveTeam %>% as.logical()) %>%
      suppressWarnings() %>%
      suppressMessages()

    data <-
      data %>%
      mutate(
        urlTeamBREF = glue::glue(
          "https://www.basketball-reference.com/teams/{slugTeamBREF}.html"
        ) %>% as.character()
      )

    data <-
      data %>%
      mutate(slugSeasons = ifelse(slugSeasons == "-", NA, slugSeasons))

    seasons <-
      data %>%
      filter(!is.na(slugSeasons)) %>%
      distinct(slugSeasons) %>%
      pull(slugSeasons)

    df_seasons <-
      seasons %>%
      future_map_dfr(function(season) {
        .munge_seasons(season = season)
      })

    data <-
      data %>%
      left_join(df_seasons) %>%
      select(
        nameTeamBREF,
        slugTeamBREF,
        isActiveTeam,
        slugSeasons,
        seasonFirst,
        seasonLast,
        everything()
      ) %>%
      suppressMessages()
    data
  })

# dicitonaries --------------------------------------------------------------



.dictionary_bref_tables <-
  memoise::memoise(function() {
    data_frame(
      idTable = c(
        "all_awards",
        "confs_standings_E",
        "confs_standings_W",
        "divs_standings_",
        "divs_standings_E",
        "divs_standings_W",
        "misc_stats",
        "opponent_shooting",
        "opponent-stats-base",
        "opponent-stats-per_game",
        "opponent-stats-per_poss",
        "team_shooting",
        "team-stats-base",
        "team-stats-per_game",
        "team-stats-per_poss"
      ),
      slugTable = c(
        "Awards",
        "StandingsConf",
        "StandingsConf",
        "StandingsDiv",
        "StandingsDiv",
        "StandingsDiv",
        "Misc",
        "Shooting",
        "Totals",
        "PerGame",
        "PerPoss",
        "Shooting",
        "Totals",
        "PerGame",
        "PerPoss"
      )

    )
  })

get_bref_name_df <-
  memoise::memoise(function() {
  data_frame(
    nameBREF = c(
      "slugSeason",
      "yearSeason",
      "Rk",
      "idPlayer",
      "Player",
      "Pos",
      "Age",
      "Tm",
      "G",
      "GS",
      "MP",
      "FG",
      "FGA",
      "FG%",
      "3P",
      "3PA",
      "3P%",
      "2P",
      "2PA",
      "2P%",
      "eFG%",
      "FT",
      "FTA",
      "FT%",
      "ORB",
      "DRB",
      "TRB",
      "AST",
      "STL",
      "BLK",
      "TOV",
      "PF",
      "PS/G",
      "urlData",
      "PTS",
      "PER",
      "TS.",
      "X3PAr",
      "FTr",
      "ORB.",
      "DRB.",
      "TRB.",
      "AST.",
      "STL.",
      "BLK.",
      "TOV.",
      "USG.",
      "OWS",
      "DWS",
      "WS",
      "WS.48",
      "OBPM",
      "DBPM",
      "BPM",
      "VORP",
      "FG.",
      "X3P",
      "X3PA",
      "X3P.",
      "X2P",
      "X2PA",
      "X2P.",
      "eFG.",
      "FT.",
      "PS.G",
      "Team"

    ),
    nameActual = c(
      "slugSeason",
      "yearSeason",
      "idRank",
      "idPlayer",
      "namePlayer",
      "idPosition",
      "agePlayer",
      "slugTeamBREF",
      "countGames",
      "countGamesStarted",
      "minutes",
      "fgm",
      "fga",
      "pctFG",
      "fg3m",
      "fg3a",
      "pctFG3",
      "fg2m",
      "fg2a",
      "pctFG2",
      "pctEFG",
      "ftm",
      "fta",
      "pctFT",
      "orb",
      "drb",
      "trb",
      "ast",
      "stl",
      "blk",
      "tov",
      "pf",
      "pts",
      "urlData",
      "pts",
      "ratioPER",
      "pctTrueShooting",
      "pct3PRate",
      "pctFTRate",
      "pctORB",
      "pctDRB",
      "pctTRB",
      "pctAST",
      "pctSTL",
      "pctBLK",
      "pctTOV",
      "pctUSG",
      "ratioOWS",
      "ratioDWS",
      "ratioWS",
      "ratioWSPer48",
      "ratioOBPM",
      "ratioDBPM",
      "ratioBPM",
      "ratioVORP",
      "pctFG",
      "fg3m",
      "fg3a",
      "pctFG3",
      "fg2m",
      "fg2a",
      "pctFG2",
      "pctEFG",
      "pctFT",
      "pts",
      "nameTeam"
    )
  )
})


# munge -------------------------------------------------------------------

#' Widens basketball reference table data
#'
#' @param data
#'
#' @return a `data_frame`
#' @export
#' @import dplyr stringr tidyr
#'
#' @examples
widen_bref_data <-
  memoise::memoise(function(data) {
    gather_cols <-
      names(data)[!names(data) %in% (data %>% select(
        dplyr::matches(
          "slugSeason|yearSeasonEnd|urlSeasonBREF|typeData|timeframeData|name|isPlayoffTeam|url[A-Z]"
        )
      ) %>% names())]

    data <-
      data %>%
      gather_('metric', 'value', gather_cols, na.rm = TRUE) %>%
      unite(metric, metric, timeframeData, typeData, sep = "")

    base_cols <-
      data %>% dplyr::select(-one_of(c("metric", "value"))) %>% names()

    col_order <- c(base_cols, data$metric %>% unique())

    data %>%
      spread(metric, value) %>%
      dplyr::select(one_of(col_order))

  })

.assign.bref.teams <-
  memoise::memoise(function(all_data,
           widen_data = TRUE,
           join_data = FALSE,
           assign_to_environment = TRUE) {
    if (!all_data %>% tibble::has_name("slugTable")) {
      return(all_data)
    }

    table_names <- all_data %>% pull(slugTable) %>% unique()


    all_data <-
      table_names %>%
      future_map(function(table) {
        table %>% cat(fill = T)
        df_table <-
          all_data %>%
          filter(slugTable == table) %>%
          select(slugSeason,
                 yearSeasonStart,
                 slugTable,
                 urlSeasonBREF,
                 dataTable) %>%
          dplyr::rename(yearSeason = yearSeasonStart) %>%
          mutate(yearSeason = yearSeason + 1) %>%
          unnest()

        if (table == "Awards") {
          if (assign_to_environment) {
            table_name <-
              glue::glue("dataBREFTeam{table}")

            assign(x = table_name,
                   value = eval(df_table),
                   envir = .GlobalEnv)
          }
          return(invisible())
        }

        if (!"df_dict_nba_players" %>% exists()) {
          assign_nba_players()
        }

        if (!"df_dict_nba_teams" %>% exists()) {
          assign_nba_teams()
        }

        df_teams <-
          df_dict_nba_teams %>%
          mutate(nameTeam = nameTeam %>% str_replace_all("LA Clippers", "Los Angeles Clippers")) %>%
          select(nameTeam, idTeamNBA = idTeam, urlThumbnailTeam) %>% distinct() %>% group_by(nameTeam) %>% filter(idTeamNBA == min(idTeamNBA)) %>% ungroup()

        df_table <-
          df_table %>%
          left_join(df_teams) %>%
          suppressMessages()

        df_table <-
          df_table %>%
          gather_data(
            numeric_ids = c(
              "year",
              "idTeam",
              "countGames",
              "attendance",
              "ratio",
              "wins",
              "losses",
              "margin",
              "rank"
            ),
            use_logical_keys = TRUE,
            use_factor_keys = T,
            use_date_keys = F
          )

        add_names <-
          c(
            "minutes",
            "countGames",
            "attendance",
            "rating",
            "wins",
            "losses",
            "margin",
            "rank"
          ) %>% str_c(collapse = "|")
        names(df_table)[names(df_table) %>% str_detect(add_names)] <-
          names(df_table)[names(df_table) %>% str_detect(add_names)] %>%
          str_c("Team", sep = "")

        names(df_table)[names(df_table) %>% str_detect("TeamTeam")]  <-
          names(df_table)[names(df_table) %>% str_detect("TeamTeam")] %>% str_replace_all("TeamTeam", "Team")

        names(df_table)[names(df_table) %>% str_detect("OpponentTeam")]  <-
          names(df_table)[names(df_table) %>% str_detect("OpponentTeam")] %>% str_replace_all("OpponentTeam", "Opponent")

        df_table <-
          df_table %>%
          unite(item, item, slugTable, sep = "")

        if (df_table %>% tibble::has_name("typeData")) {
          df_table <-
            df_table %>%
            unite(item, item, typeData, sep = "")
        }
        df_table <-
          df_table %>%
          dplyr::select(-one_of("timeframeData", "typeData")) %>%
          suppressWarnings()

        col_order <-
          c(names(df_table)[!names(df_table) %>% str_detect("value|item")], "item", "value")
        df_table <-
          df_table %>%
          select(one_of(col_order)) %>%
          distinct()

        if (widen_data) {
          df_table <-
            df_table %>%
            spread_data(
              variable_name = "item",
              value_name = "value",
              perserve_order = TRUE,
              unite_columns = NULL,
              separate_columns = NULL
            )
        }

        if (assign_to_environment) {
          table_name <-
            glue::glue("dataBREF{table}Teams")

          assign(x = table_name,
                 value = eval(df_table),
                 envir = .GlobalEnv)
        }
        data_frame(nameTable = table, dataTable = list(df_table))
      }) %>%
      purrr::reduce(bind_rows)

    if (join_data) {
      all_data <-
        all_data %>%
        mutate(idRow = 1:n())

      df_tables_joined <-
        all_data %>%
        filter(nameTable %in% c("PerGame", "Totals", "PerPoss", 'Misc', "Shooting")) %>%
        select(dataTable) %>%
        purrr::flatten()

      df_tables_joined <-
        df_tables_joined %>%
        purrr::reduce(left_join) %>%
        suppressMessages()

      df_standings <-
        all_data %>%
        filter(nameTable %in% c("StandingsConf", "StandingsDiv")) %>%
        select(dataTable) %>%
        purrr::flatten()

      df_standings <-
        df_standings %>%
        purrr::reduce(left_join) %>%
        suppressMessages()

      df_tables_joined <-
        df_standings %>%
        left_join(df_tables_joined) %>%
        suppressMessages()

      if (assign_to_environment) {
        table <- "dataBREFTeamJoined"
        assign(x = table,
               value = df_tables_joined,
               envir = .GlobalEnv)
      }


      if (assign_to_environment) {
        table <- "dataBREFStandings"
        assign(x = table,
               value = df_standings,
               envir = .GlobalEnv)
      }


      all_data <-
        data_frame(nameTable = c("Team Data"),
                   dataTable = list(df_tables_joined)) %>%
        bind_rows(all_data %>% filter(
          !nameTable %in% c(
            "StandingsConf",
            "StandingsDiv",
            "PerGame",
            "Totals",
            "PerPoss",
            'Misc',
            "Shooting"
          )
        ) %>% select(nameTable, dataTable))
    }

    all_data
  })

.assign.bref.players <-
  memoise::memoise(function(all_data,
           widen_data = TRUE,
           join_data = FALSE,
           include_all_nba = F,
           assign_to_environment = TRUE) {
    if (!all_data %>% tibble::has_name("typeData")) {
      return(all_data)
    }

    table_names <-
      all_data %>% pull(typeData) %>% unique()


    all_data <-
      table_names %>%
      future_map_dfr(function(table) {
        table %>% cat(fill = T)
        df_table <-
          all_data %>%
          filter(typeData == table) %>%
          unnest()

        if (df_table %>% tibble::has_name("yearSeason")) {
          df_table <-
            df_table %>%
            mutate(yearSeason = yearSeason - 1)
        }

        df_table <-
          df_table %>%
          .resolve.players(site = "bref")

        if (!table == "Advanced") {
          df_table <-
            df_table %>%
            gather_data(
              numeric_ids = c(
                "year",
                "agePlayer",
                "pct",
                "countGames",
                "ratio",
                "idPlayer"
              ),
              use_logical_keys = TRUE,
              use_factor_keys = T,
              use_date_keys = F,
              unite_columns = list(
                new_column = "item",
                column_1 = "item",
                column_2 = "typeData",
                sep = ""
              )
            )
          col_order <-
            c(names(df_table)[!names(df_table) %>% str_detect("value|item")], "item", "value")

          df_table <-
            df_table %>%
            select(one_of(col_order))
        } else {
          df_table <-
            df_table %>%
            gather_data(
              numeric_ids = c("year", "agePlayer", "countGames", "id"),
              use_logical_keys = TRUE,
              use_factor_keys = T,
              use_date_keys = F,
              unite_columns = NULL
            )


          col_order <-
            c(names(df_table)[!names(df_table) %>% str_detect("value|item")], "item", "value")

          df_table <-
            df_table %>%
            select(one_of(col_order))
        }

        df_table <-
          df_table %>%
          dplyr::select(-one_of("typeData")) %>%
          suppressWarnings()

        if (df_table %>% tibble::has_name("yearSeasonStart")) {
          df_table <-
            df_table %>%
            rename(yearSeason = yearSeasonStart) %>%
            mutate(yearSeason = yearSeason + 1)
        }

        if (widen_data) {
          df_table <-
            df_table %>%
            spread_data(
              variable_name = "item",
              value_name = "value",
              perserve_order = TRUE,
              unite_columns = NULL,
              separate_columns = NULL
            )
        }

        if (df_table %>% tibble::has_name("minutesAdvanced")) {
          df_table <-
            df_table %>%
            dplyr::rename(minutesTotals = minutesAdvanced)
        }

        if (assign_to_environment) {
          table_name <-
            glue::glue("dataBREFPlayer{table}")

          assign(x = table_name,
                 value = eval(df_table),
                 envir = .GlobalEnv)
        }
        data_frame(nameTable = table, dataTable = list(df_table))
      }) %>%
      suppressMessages()

    if (join_data) {
      if (widen_data) {
        all_data <-
          all_data %>%
          select(dataTable) %>%
          purrr::flatten() %>%
          purrr::reduce(left_join) %>%
          suppressMessages() %>%
          dplyr::select(yearSeason,
                        slugSeason,
                        namePlayer,
                        idPosition,
                        everything())
      } else {
        all_data <-
          all_data %>%
          purrr::reduce(bind_rows) %>%
          suppressMessages()

        col_order <-
          c(names(all_data)[!names(all_data) %>% str_detect("value|item")], "item", "value")

        all_data <-
          all_data %>%
          select(one_of(col_order))
      }

      all_data <-
        all_data %>%
        mutate(
          groupPosition = ifelse(
            idPosition %>% nchar() == 1,
            idPosition,
            idPosition %>% substr(2, 2)
          ),
          isHOFPlayer = ifelse(isHOFPlayer %>% is.na(), FALSE, isHOFPlayer)
        ) %>%
        mutate(groupPosition = ifelse(
          groupPosition == "-",
          substr(idPosition, 1, 1),
          groupPosition
        )) %>%
        dplyr::select(slugSeason:namePlayer, groupPosition, everything())

      if (include_all_nba) {
        df_all_nba <-
          get_bref_all_nba_teams(return_message = F) %>%
          dplyr::rename(namePlayerBREF = namePlayer)

        all_data <-
          all_data %>%
          left_join(
            df_all_nba %>% dplyr::select(
              namePlayer,
              slugSeason,
              numberAllNBATeam,
              isAllNBA:isAllNBA3
            )
          ) %>%
          distinct() %>%
          suppressMessages()

        all_data <-
          all_data %>%
          tidyr::replace_na(
            list(
              numberAllNBATeam = 0,
              isAllNBA = FALSE,
              isAllNBA1 = FALSE,
              isAllNBA2 = FALSE,
              isAllNBA3 = FALSE
            )
          )
      }

      all_data <-
        all_data %>%
        mutate(
          urlPlayerBREF = list(
            'http://www.basketball-reference.com/players/',
            idPlayer %>% substr(start = 1, stop = 1),
            '/',
            idPlayer,
            '.html'
          ) %>% purrr::reduce(paste0)
        )
    }
    all_data
  })

#' Assign nested BREF data to environment
#'
#' @param data a \code{data_frame} of tables
#' @param type type of BREF data are `teams` and `players`
#' @param widen_data if \code{TRUE} widens data
#' @param join_data if \code{TRUE} joins tables
#' @param nest_data if \code{TRUE} nests data
#' @param assign_to_environment if \code{TRUE} assigns data to your environment
#' @param include_all_nba if `TRUE` include all NBA teams
#'
#' @return a `data_frame`
#' @export
#' @import dplyr purrr stringr tibble tidyr
#' @examples
assign_bref_data <-
  memoise::memoise(function(data,
           type = "Players",
           widen_data = TRUE,
           include_all_nba = F,
           join_data = TRUE,
           nest_data = FALSE,
           assign_to_environment = TRUE) {
    type_slug <-
      type %>% str_to_lower()

    if (!type_slug %in% c('players', 'teams')) {
      stop("Type can only be players or teams")
    }

    if (type_slug == "players") {
      data <-
        .assign.bref.players(
          all_data = data,
          widen_data = widen_data,
          join_data = join_data,
          include_all_nba = include_all_nba,
          assign_to_environment = assign_to_environment
        )
    }

    if (type_slug == "teams") {
      data <-
        .assign.bref.teams(
          all_data = data,
          widen_data = widen_data,
          join_data = join_data,
          assign_to_environment = assign_to_environment
        )
    }

    if (nest_data) {
      if (data %>% tibble::has_name("slugSeason")) {
        data <-
          data %>%
          mutate(typeBREFData = type) %>%
          nest(-c(slugSeason, typeBREFData, yearSeason), .key = dataSeason)
      }
    }
    data
  })

.parse_years_player <-
  memoise::memoise(function(years = 2017) {
    years_player <-
      years %>% str_split("\\-") %>% flatten_chr() %>% as.numeric()
    rookie_season <-
      generate_season_slug(season = years_player[[1]])

    if (length(years_player) == 1) {
      data <-
        data_frame(yearsPlayer = years, slugSeasonRookie = rookie_season,
                   slugSeasonLast = rookie_season, countSeasons = 1)

      return(data)
    }
    count_seasons <-
      (years_player[[2]] - years_player[[1]]) + 1
    last_season <-
      generate_season_slug(season = years_player[2])
    data_frame(yearsPlayer = years, slugSeasonRookie = rookie_season,
               slugSeasonLast = last_season, countSeasons = count_seasons)
  })

# csvs --------------------------------------------------------------------

#' Basketball Reference player dictionary
#'
#'
#' @return a `data_frame`
#' @export
#' @family BREF
#' @family dictionary
#' @family NBA players
#'
#' @examples
#' dictionary_bref_players()
dictionary_bref_players <-
  memoise::memoise(function() {
    data <-
      readr::read_csv(
        "https://d2cwpp38twqe55.cloudfront.net/short/inc/players_search_list.csv",
        col_names = F
      ) %>%
      dplyr::select(1:4) %>%
      purrr::set_names(c("slugPlayerBREF", "namePlayerBREF", "yearsPlayer", "isActive")) %>%
      mutate(isActive = as.logical(isActive)) %>%
      suppressWarnings() %>%
      suppressMessages()


    data <-
      data %>%
      mutate(
        yearsPlayer = ifelse(yearsPlayer == "-", NA, yearsPlayer),
        letterLastName = substr(slugPlayerBREF, 1, 1),
        urlPlayerBioBREF =
          glue::glue(
            "https://www.basketball-reference.com/players/{letterLastName}/{slugPlayerBREF}.html"
          ) %>% as.character()
      ) %>%
      select(-one_of("letterLastName"))

    all_years <-
      data %>%
      filter(!is.na(yearsPlayer)) %>%
      pull(yearsPlayer) %>%
      unique() %>%
      sort()

    df_years <-
      all_years %>%
      future_map_dfr(function(years){
        .parse_years_player(years = years)
      })

    data <-
      data %>%
      left_join(df_years) %>%
      select(namePlayerBREF, slugPlayerBREF, isActive, yearsPlayer,
             countSeasons, slugSeasonRookie, slugSeasonLast, everything()) %>%
      suppressMessages()

    data
  })

# all_nba -----------------------------------------------------------------

#' All NBA Teams
#'
#' @param return_message
#'
#' @return \code{data_frame}
#' @export
#'
#' @examples
all_nba_teams <-
  memoise::memoise(function(return_message = TRUE) {
  if (return_message) {
    "Acquiring ALL NBA Teams" %>% cat(fill = T)
  }
  page <-
      "http://www.basketball-reference.com/awards/all_league.html" %>%
      read_html()

    df <-
      page %>%
      html_table(fill = TRUE) %>%
      .[[1]] %>%
      data.frame(stringsAsFactors = F) %>%
      tbl_df() %>%
      dplyr::rename(slugSeason = Season,
                    slugLeague = Lg,
                    classAllNBA = Tm) %>%
      filter(!slugSeason == '') %>%
      tidyr::gather(value,
                    namePlayerPosition,
                    -c(slugSeason, slugLeague, classAllNBA)) %>%
      select(-value) %>%
      tidyr::separate(
        slugSeason,
        into = c('yearSeasonStart', 'remove'),
        sep = '\\-',
        remove = F
      ) %>%
      dplyr::select(-remove) %>%
      mutate(
        yearSeasonStart = yearSeasonStart %>% as.numeric(),
        yearSeason = yearSeasonStart + 1,
        numberAllNBATeam = classAllNBA %>% as.character() %>% readr::parse_number(),
        namePlayer =
          ifelse(
            yearSeasonStart >= 1955,
            namePlayerPosition %>% substr(start = 1, stop = nchar(namePlayerPosition) - 2) %>% str_trim(),
            namePlayerPosition
          ),
        groupPosition = ifelse(
          yearSeasonStart >= 1955,
          namePlayerPosition %>% substr(
            start = nchar(namePlayerPosition) - 1,
            stop = nchar(namePlayerPosition)
          ) %>% str_trim(),
          NA
        )
      ) %>%
      tidyr::separate(namePlayer,
                      into = c('namePlayer', 'namePlayer2'),
                      sep = '\\, ') %>%
      select(slugLeague,
             slugSeason,
             yearSeason,
             groupPosition,
             numberAllNBATeam,
             namePlayer) %>%
      filter(slugLeague == "NBA") %>%
      arrange(desc(yearSeason), numberAllNBATeam) %>%
      suppressMessages() %>%
      suppressWarnings()

    player_names <-
      page %>%
      html_nodes(css = 'td a') %>%
      html_text()

    slugs <-
      page %>%
      html_nodes(css = 'td a') %>%
      html_attr(name = 'href')

    df_people <-
      seq_along(slugs) %>%
      future_map_dfr(function(x) {
        namePlayer <-
          player_names[[x]]
        urlPlayerBREF <-
          str_c('http://www.basketball-reference.com', slugs[[x]], collapse = '')
        is_player <-
          slugs[[x]] %>% str_detect('/players/')

        if (is_player) {
          idPlayer <-
            slugs[[x]] %>% str_replace_all('.html', '') %>% str_split('/players/') %>% flatten_chr() %>% .[[2]] %>% str_split('/') %>% flatten_chr() %>% .[[2]]
        } else {
          idPlayer <-
            slugs[[x]] %>% str_replace_all('.html', '') %>% str_split('/leagues/') %>% flatten_chr() %>% .[[2]] %>% str_split('_') %>% flatten_chr() %>% .[[1]]
        }
        data_frame(idPlayer, namePlayer, urlPlayerBREF)
      }) %>%
      distinct() %>%
      mutate_all(str_trim)

    df_people <-
      df_people %>%
      select(namePlayer, idPlayer, urlPlayerBREF) %>%
      distinct() %>%
      filter(!idPlayer %>% is.na())

    df <-
      df %>%
      mutate(namePlayer = namePlayer %>% str_trim()) %>%
      left_join(df_people) %>%
      suppressMessages() %>%
      mutate(
        idPlayerSeason = list(idPlayer, '_', yearSeason) %>% purrr::reduce(paste0),
        isAllNBA = TRUE
      )

    df <-
      df %>%
      mutate(
        isAllNBA1 = numberAllNBATeam == 1,
        isAllNBA2 = numberAllNBATeam == 2,
        isAllNBA3 = numberAllNBATeam == 3
      )

    if (return_message) {
      list(
        "Returned All-NBA teams from ",
        df$yearSeason %>% min() %>% unique(),
        ' to ',
        df$yearSeason %>% max() %>% unique()
      ) %>% purrr::reduce(paste0)
    }

    return(df)
  })


# .parse_page -------------------------------------------------------


.parse_page <-
  memoise::memoise(function(url = "http://www.basketball-reference.com/players/g/gillke01.html") {
    page <-
      url %>%
      curl() %>%
      read_html()

    page
  })


# .parse_page_season --------------------------------------------------------------

.generate_years_urls <-
  memoise::memoise(function(table = "per_game",
           seasons = 1951:2017) {
    tables <-
      c('per_game', 'advanced', 'totals', 'per_minute', 'per_poss')

    if (!table %in% tables) {
      stop(str_c("Tables can only be: " , tables %>% paste(collapse = ', ')))
    }

    urls <-
      list('http://www.basketball-reference.com/leagues/NBA_',
           seasons,
           '_',
           table,
           '.html') %>%
      purrr::reduce(paste0)
    return(urls)
  })

.parse_player_season <-
  memoise::memoise(function(url = "http://www.basketball-reference.com/leagues/NBA_1997_per_game.html",
           return_message = TRUE) {
    ## case_when

    page <-
      url %>%
      read_html()

    url_df <- url %>% httr::parse_url() %>% flatten_df()

    url_path <-
      url_df$path %>% str_replace_all(".html|leagues/NBA_", '')

    year_season_end <-
      url_path %>% str_split('\\_') %>% flatten_chr() %>% .[[1]] %>% as.character() %>% readr::parse_number()

    name_slug <-
      url_path %>%
      map_chr(function(x) {
        parts <-
          x %>% str_split('\\_') %>% flatten_chr()

        parts[2:length(parts)] %>% str_to_title() %>% str_c(collapse = '')
      })

    id_season <-
      list(year_season_end - 1, '-', year_season_end %>% str_sub(3, 4)) %>%
      purrr::reduce(paste0)

    players <-
      page %>%
      html_nodes('th+ .left a') %>%
      html_text()

    player_id <-
      page %>%
      html_nodes('th+ .left a') %>%
      html_attr('href') %>%
      str_replace_all('/players/', '')

    player_ids <-
      player_id %>%
      map_chr(function(x) {
        x %>%
          str_replace_all('.html', '') %>%
          str_split('/') %>%
          flatten_chr() %>%
          .[[2]]
      })

    df_players <-
      data_frame(idPlayer = player_ids, namePlayer = players) %>%
      distinct()

    df <-
      page %>%
      html_table() %>%
      .[[1]] %>%
      data.frame(stringsAsFactors = FALSE) %>%
      tbl_df() %>%
      dplyr::select(-dplyr::matches("Var"))

    df <-
      df %>%
      mutate_at(df %>% dplyr::select(-one_of(c(
        "Tm", "Player", "Pos"
      ))) %>% names(),
      funs(. %>% as.numeric())) %>%
      filter(!Rk %>% is.na()) %>%
      suppressWarnings() %>%
      dplyr::select(-dplyr::matches("Rk"))

    df_names <-
      get_bref_name_df()

    bref_names <-
      names(df)

    actual_names <-
      seq_along(bref_names) %>%
      map_chr(function(x) {
        actual <-
          df_names %>%
          filter(nameBREF == bref_names[x]) %>%
          .$nameActual

        if (actual == 'MinutesPlayed') {
          if (!name_slug == 'pergame') {
            actual <-
              str_c('total', actual)
          } else {
            actual <-
              str_c('pergame', actual)
          }
          return(actual)
        }

        if (actual %>% substr(1, 1) %in% LETTERS) {
          actual <-
            str_c(name_slug, actual)
        }
        return(actual)
      })


    df <-
      df %>%
      purrr::set_names(actual_names) %>%
      mutate(
        isHOFPlayer = namePlayer %>% str_detect('\\*'),
        namePlayer = namePlayer %>% str_replace_all('\\*', '')
      ) %>%
      left_join(df_players) %>%
      distinct() %>%
      suppressMessages() %>%
      mutate(slugSeason = id_season,
             yearSeason = year_season_end,
             urlData = url) %>%
      dplyr::select(slugSeason, yearSeason,
                    idPlayer, everything())

    df <-
      df %>%
      mutate_at(df %>% dplyr::select(dplyr::matches("pct")) %>% names(),
                funs(ifelse(. >= 1, . / 100, .))) %>%
      mutate(typeData = name_slug) %>%
      dplyr::select(typeData, everything())

    if (return_message) {
      list("parsed ", url) %>%
        purrr::reduce(paste0) %>%
        cat(fill = T)
    }
    gc()
    return(df)
  })

# player seasons -----------------------------------------------------------------

.get_data_bref_player_seasons <-
  function(seasons = 1980:2017,
           table = "advanced",
           only_totals = TRUE,
           return_message = TRUE) {
    urls <-
      .generate_years_urls(table = table, seasons = seasons)
    .parse_player_season_safe <-
      purrr::possibly(.parse_player_season, data_frame())
    all_data <-
      urls %>%
      future_map_dfr(function(x) {
        .parse_player_season_safe(url = x, return_message = return_message)
      })

    all_data <-
      all_data %>%
      dplyr::select(-dplyr::matches("urlData")) %>%
      tidyr::unite(idPlayerSeason, idPlayer, yearSeason, remove = F)


    all_data <-
      all_data %>%
      mutate(yearSeason = slugSeason %>% substr(1, 4) %>% as.numeric() + 1)


    all_data <-
      all_data %>%
      arrange(yearSeason)

    if (only_totals) {
      all_data <-
        all_data %>%
        group_by(yearSeason, idPlayer) %>%
        filter(countGames == max(countGames)) %>%
        ungroup()
    }
    gc()

    all_data <-
      all_data %>%
      nest(-c(typeData, slugSeason, yearSeason), .key = dataTable)
   all_data
  }


#' Basketball Reference Player Season Tables
#'
#' @param tables player table \itemize{
#' \item \code{totals}: Totals
#' \item \code{per_game}: Per game
#' \item \code{advanced}: Advanced
#' \item \code{per_minute}: Per 36 minutes
#' \item \code{per_poss}: Per Possession
#' }
#' @param include_all_nba if \code{TRUE} include all_nba teams
#' @param seasons vector of years 1951 to current season
#' @param only_totals if \code{TRUE} returns only a player's total statistics
#' @param nest_data if \code{TRUE} returns a nested data frame
#' @param return_message  if \code{TRUE} returns a message
#' @param assign_to_environment if `TRUE` assigns to environment
#' @param widen_data if `TRUE` widens data
#' @param join_data if `TRUE` joins `data_frames`
#'
#' @return a \code{data_frame}
#' @family BREF
#' @family player statistics
#' @export
#' @import curl dplyr tidyr httr xml2 rvest tidyr stringr purrr readr
#' @examples
#' bref_players_stats(seasons = 2017:2018, tables = c("advanced", "totals"))
bref_players_stats <-
  function(seasons = 2018,
           tables = c('advanced', 'totals'),
           include_all_nba = F,
           only_totals = TRUE,
           nest_data = FALSE,
           assign_to_environment = TRUE,
           widen_data = TRUE,
           join_data = TRUE,
           return_message = TRUE) {
    tables <-
      tables %>% str_replace_all("\\ ", "_") %>% str_to_lower()

    .get_data_bref_player_seasons_safe <-
      purrr::possibly(.get_data_bref_player_seasons, data_frame())

    all_data <-
      tables %>%
      future_map_dfr(function(x) {
        .get_data_bref_player_seasons_safe(
          table = x,
          seasons = seasons,
          only_totals = only_totals,
          return_message = return_message
        )
      })

    all_data <-
      assign_bref_data(
        data = all_data,
        type = "Players",
        widen_data = widen_data,
        include_all_nba = include_all_nba,
        join_data = join_data,
        nest_data = nest_data,
        assign_to_environment = assign_to_environment
      )

    all_data <-
      all_data %>%
      mutate(yearSeason = slugSeason %>% substr(1, 4) %>% as.numeric() + 1)

    all_data <-
      all_data %>%
      dplyr::rename(slugPlayerBREF = idPlayer)

    all_data
  }


# team seasons -----------------------------------------------------------------
.read_page <-
  function(url) {
    page <-
      url %>%
      readr::read_lines() %>%
      str_replace_all("<!--|-->", "") %>%
      str_trim() %>%
      stringi::stri_trans_general("Latin-ASCII") %>%
      str_c(collapse = "") %>%
      xml2::read_html()

    page
  }

.parse.bref.team.conference <-
  function(data) {
    conference <-
      data %>% slice(1) %>% pull(X1) %>% str_replace_all("Conference", "") %>% str_trim()
    data %>%
      slice(2:nrow(data)) %>%
      purrr::set_names(
        c(
          "nameTeamRank",
          "winsTeam",
          "lossesTeam",
          "pctWins",
          "gamesBehind1Conference",
          "ptsTeamPerGame",
          "ptsOppPerGame",
          "ratingStrengthOfSchedule"
        )
      ) %>%
      tidyr::separate(nameTeamRank,
                      sep = "\\(",
                      into = c("nameTeam", "rankConference")) %>%
      mutate_all(str_trim) %>%
      mutate(rankConference = rankConference %>% str_replace_all('\\)', "")) %>%
      mutate_at(c("rankConference", "winsTeam", "lossesTeam"),
                funs(. %>% as.integer())) %>%
      mutate_at(
        c(
          "pctWins",
          "gamesBehind1Conference",
          "ptsTeamPerGame",
          "ptsOppPerGame",
          "ratingStrengthOfSchedule"
        ),
        funs(. %>% as.character() %>% readr::parse_number() %>% as.numeric())
      ) %>%
      mutate(
        gamesBehind1Conference = ifelse(
          gamesBehind1Conference %>% is.na(),
          0,
          gamesBehind1Conference
        ),
        nameConference = conference
      ) %>%
      mutate(
        isPlayoffTeam = nameTeam %>% str_detect("\\*"),
        nameTeam = nameTeam %>% str_replace_all("\\*", "")
      ) %>%
      dplyr::select(nameConference, isPlayoffTeam, everything()) %>%
      select(nameConference, everything()) %>%
      suppressWarnings()
  }

.parse.bref.team.division <-
  function(data) {
    conference <-
      data %>% slice(1) %>% pull(X1) %>% str_replace_all("Conference", "") %>% str_trim()
    data <-
      data %>%
      slice(2:nrow(data)) %>%
      mutate(idRow = 1:n())

    df_divisions <-
      data %>%
      filter(X1 %>% str_detect("Division")) %>%
      select(idRow, nameDivision = X1) %>%
      mutate(idRow = idRow + 1)

    data %>%
      select(-idRow) %>%
      purrr::set_names(
        c(
          "nameTeamRank",
          "winsTeam",
          "lossesTeam",
          "pctWins",
          "gamesBehind1Division",
          "ptsTeamPerGame",
          "ptsOppPerGame",
          "ratingStrengthOfSchedule"
        )
      ) %>%
      tidyr::separate(nameTeamRank,
                      sep = "\\(",
                      into = c("nameTeam", "rankConference")) %>%
      mutate_all(str_trim) %>%
      mutate(rankConference = rankConference %>% str_replace_all('\\)', "")) %>%
      mutate_at(c("rankConference", "winsTeam", "lossesTeam"),
                funs(. %>% as.integer())) %>%
      mutate(idRow = 1:n()) %>%
      left_join(df_divisions) %>%
      filter(!rankConference %>% is.na()) %>%
      select(-idRow) %>%
      select(nameDivision, everything()) %>%
      fill(nameDivision) %>%
      mutate_at(
        c(
          "pctWins",
          "gamesBehind1Division",
          "ptsTeamPerGame",
          "ptsOppPerGame",
          "ratingStrengthOfSchedule"
        ),
        funs(. %>% as.character() %>% readr::parse_number() %>% as.numeric())
      ) %>%
      mutate(
        gamesBehind1Division = ifelse(gamesBehind1Division %>% is.na(), 0, gamesBehind1Division),
        nameConference = conference
      ) %>%
      select(nameConference, everything()) %>%
      mutate(
        isPlayoffTeam = nameTeam %>% str_detect("\\*"),
        nameTeam = nameTeam %>% str_replace_all("\\*", "")
      ) %>%
      dplyr::select(nameConference, nameDivision, isPlayoffTeam, everything()) %>%
      suppressWarnings() %>%
      suppressMessages()
  }

.parse.bref.team.pg <-
  function(data) {
    names_bref <-
      data %>% slice(1) %>% as.character()

    data <-
      data %>%
      slice(2:nrow(data))

    df_bref_names <-
      get_bref_name_df()

    actual_names <-
      names_bref %>%
      map_chr(function(x) {
        df_bref_names %>%
          mutate(BREFLower = str_to_lower(nameBREF)) %>%
          filter(x %>% str_to_lower() == BREFLower) %>%
          pull(nameActual)

      })

    df_names <- data_frame(nameActual = actual_names) %>% mutate(idColumn = 1:n())

    df_names <- df_names %>% group_by(nameActual) %>% filter(idColumn == min(idColumn)) %>% ungroup() %>% arrange(idColumn)

    data <-
      data[,df_names$idColumn] %>%
      purrr::set_names(df_names$nameActual) %>%
      dplyr::select(-one_of("idRank"))


    numeric_names <-
      data %>% dplyr::select(-dplyr::matches("name|arena")) %>% names()

    data %>%
      mutate(
        isPlayoffTeam = nameTeam %>% str_detect("\\*"),
        nameTeam = nameTeam %>% str_replace_all("\\*", "")
      ) %>%
      mutate_at(numeric_names,
                as.numeric) %>%
      dplyr::select(nameTeam, isPlayoffTeam, everything()) %>%
      suppressWarnings()

  }


.parse.bref.team.pg.opp <-
  function(data) {
    names_bref <-
      data %>% slice(1) %>% as.character()

    data <-
      data %>%
      slice(2:nrow(data))

    df_bref_names <-
      get_bref_name_df()

    actual_names <-
      c(
        "idRank",
        "nameTeam",
        "countGames",
        "minutes",
        "fgm",
        "fga",
        "pctFG",
        "fg3m",
        "fg3a",
        "pctFG3",
        "fg2m",
        "fg2a",
        "pctFG2",
        "ftm",
        "fta",
        "pctFT",
        "orb",
        "drb",
        "trb",
        "ast",
        "stl",
        "blk",
        "tov",
        "pf",
        "pts"
      )


    data <-
      data %>%
      purrr::set_names(actual_names) %>%
      dplyr::select(-one_of("idRank"))

    numeric_names <-
      data %>% dplyr::select(-dplyr::matches("name|arena")) %>% names()

    data %>%
      mutate(
        isPlayoffTeam = nameTeam %>% str_detect("\\*"),
        nameTeam = nameTeam %>% str_replace_all("\\*", "")
      ) %>%
      mutate_at(numeric_names,
                as.numeric) %>%
      dplyr::select(nameTeam, isPlayoffTeam, everything()) %>%
      suppressWarnings()

  }


.parse.bref.team.misc <-
  function(data) {
    data <-
      data %>%
      slice(3:nrow(data))

    df_bref_names <- get_bref_name_df()

    actual_names <-
      c(
        "idRank",
        "nameTeam",
        "ageMean",
        "wins",
        "losses",
        "winsPythag",
        "lossesPythag",
        "marginVictory",
        "ratingStrengthOfSchedule",
        "ratingSimpleSystem",
        "ortgTeam",
        "drtgTeam",
        "paceTeam",
        "pctFTRate",
        "pct3PRate",
        "pctTrueShootingeTeam",
        "pctEFGTeam",
        "pctTOVTeam",
        "pctORBTeam",
        "ratioFTtoFGATeam",
        "pctEFGTeamOpp",
        "pctTOVOpponent",
        "pctDRBOpponent",
        "ratioFTtoFGAOpponent",
        "nameArena",
        "attendanceArena",
        "attendancePerGame"
      )
    actual_names <- actual_names[1:ncol(data)]
    data <-
      data %>%
      purrr::set_names(actual_names) %>%
      dplyr::select(-one_of("idRank"))

    numeric_names <-
      data %>% dplyr::select(-dplyr::matches("nameTeam|nameArena")) %>% names()

    data %>%
      mutate(nameArena = ifelse(nameArena == "", NA, nameArena)) %>%
      mutate(
        isPlayoffTeam = nameTeam %>% str_detect("\\*"),
        nameTeam = nameTeam %>% str_replace_all("\\*", "")
      ) %>%
      mutate_at(numeric_names,
                funs(. %>% as.character() %>% readr::parse_number())) %>%
      dplyr::select(nameTeam, isPlayoffTeam, everything()) %>%
      suppressWarnings()

  }

.parse.bref.team.shooting <-
  function(data) {
    data <-
      data %>%
      slice(4:nrow(data))

    actual_names <-
      c(
        "idRank",
        "nameTeam",
        "countGames",
        "minutes",
        "pctFG",
        "avgDistanceShot",
        "pctFGAFG2Shots",
        "pctFGA0to3Shots",
        "pctFGA3to10Shots",
        "pctFGA10to16Shots",
        "pctFGA16Shots",
        "pctFGAFG3Shots",
        "pctFG2",
        "pctFG0to3",
        "pctFG3to10",
        "pctFG10to16",
        "pctFG16",
        "pctFG3",
        "pctFG2MAst",
        "pctFGADunksShots",
        "countDunks",
        "pctFGALayupsShots",
        "countLayups",
        "pctFG3MAst",
        "pctFG3ACornerShots",
        "pctFG3ACorner",
        "hlfA",
        "hlfM"
      )
    data <-
      data %>%
      purrr::set_names(actual_names[1:ncol(data)]) %>%
      dplyr::select(-one_of("idRank"))

    numeric_names <-
      data %>% dplyr::select(-dplyr::matches("nameTeam|nameArena")) %>% names()

    data %>%
      mutate(
        isPlayoffTeam = nameTeam %>% str_detect("\\*"),
        nameTeam = nameTeam %>% str_replace_all("\\*", "")
      ) %>%
      mutate_at(numeric_names,
                funs(. %>% as.character() %>% readr::parse_number())) %>%
      dplyr::select(nameTeam, isPlayoffTeam, everything()) %>%
      suppressWarnings()

  }


.parse_season_url <-
  function(url = "https://www.basketball-reference.com/leagues/NBA_2018.html") {
    page <-
      url %>%
      .read_page()

    xml_tables <-
      page %>%
      html_nodes(xpath = "//*[contains(@class, 'sortable')]")

    all_data <-
      seq_along(xml_tables) %>%
      future_map_dfr(function(x) {
        table_id <-
          xml_tables[x] %>%
          html_attr("id")

        table_name <-
          xml_tables[[x]] %>%
          xml_nodes("caption") %>%
          html_text()

        is_ap <-
          table_id == "all_playoffs"

        is_awards <- table_id == "all_awards"

        if (is_ap) {
          return(invisible())
        }

        data <-
          xml_tables[[x]] %>%
          html_table(header = F,
                     trim = T,
                     fill = F) %>%
          tibble::as_data_frame()

        team_nodes <-
          xml_tables[x] %>%
          html_nodes("a") %>%
          html_attr('href')

        team_slugs <-
          team_nodes %>% str_replace_all("/teams/", "") %>%
          substr(1, 3)

        url_team <-
          team_nodes %>%
          str_c("https://www.basketball-reference.com", .)

        name_team <-
          xml_tables[x] %>%
          html_nodes("a") %>%
          html_text()

        df_urls <-
          data_frame(
            slugTeamBREF = team_slugs,
            nameTeam = name_team,
            urlBREFTeamData = url_team
          )

        is_misc <- table_id %>% str_detect("misc_stats")

        is_conf <-
          table_id %>% str_detect("confs_standings")

        is_division <-
          table_id %>% str_detect("divs_standing")

        is_team_stats_pg <-
          table_id %>% str_detect("team-stats-per_game")

        is_opp_stats_pg <-
          table_id %>% str_detect("opponent-stats-per_game")

        is_team_base <-
          table_id %>% str_detect("team-stats-base")

        is_opp_total <-
          table_id %>% str_detect("opponent-stats-base")

        is_team_100 <-
          table_id %>% str_detect("team-stats-per_poss")

        is_opp_100 <-
          table_id %>% str_detect("opponent-stats-per_poss")

        is_team_shooting <-
          table_id %>% str_detect("team_shooting")

        is_opponent_shooting <-
          table_id %>% str_detect("opponent_shooting")

        if (is_team_shooting) {
          table_name <- "Team Shooting"
          table_data <-
            data %>%
            .parse.bref.team.shooting() %>%
            mutate(typeData = "Team",
                   timeframeData = "Shooting") %>%
            select(typeData, timeframeData, everything()) %>%
            filter(!nameTeam %>% str_detect("Average"))
        }

        if (is_opponent_shooting) {
          table_name <- "Opponent Shooting"
          table_data <-
            data %>%
            .parse.bref.team.shooting() %>%
            mutate(typeData = "Opponent",
                   timeframeData = "Shooting") %>%
            select(typeData, timeframeData, everything()) %>%
            filter(!nameTeam %>% str_detect("Average"))
        }

        if (is_awards) {
          df_urls <-
            df_urls[c(F, T), ]

          df_urls <-
            df_urls %>%
            select(-one_of(c("slugTeamBREF"))) %>%
            purrr::set_names(c("namePlayer", "urlPlayer")) %>%
            suppressWarnings()

          table_name <-
            c("Player Awards")

          table_data <-
            data %>%
            slice(-1) %>%
            purrr::set_names(c("nameAward", "namePlayer"))
        }

        if (is_misc) {
          table_name <- "Miscellaneous Stats"
          table_data <-
            data %>%
            .parse.bref.team.misc() %>%
            filter(!nameTeam %>% str_detect("Average"))
        }

        if (is_opp_100) {
          table_name <- "Opponent Stats Per 100 Possessions"
          table_data <-
            data %>%
            .parse.bref.team.pg() %>%
            mutate(typeData = "Opponent",
                   timeframeData = "Per100Poss") %>%
            select(typeData, timeframeData, everything()) %>%
            filter(!nameTeam %>% str_detect("Average"))
        }

        if (is_opp_stats_pg) {
          table_name <- "Opponent Stats Per Game"
          table_data <-
            data %>%
            .parse.bref.team.pg.opp() %>%
            mutate(typeData = "Opponent",
                   timeframeData = "PerGame") %>%
            select(typeData, timeframeData, everything()) %>%
            filter(!nameTeam %>% str_detect("Average"))
        }

        if (is_team_100) {
          table_name <- "Team Stats Per 100 Possessions"
          table_data <-
            data %>%
            .parse.bref.team.pg() %>%
            mutate(typeData = "Team",
                   timeframeData = "Per100Poss") %>%
            select(typeData, timeframeData, everything()) %>%
            filter(!nameTeam %>% str_detect("Average"))
        }

        if (is_team_base) {
          table_name <- "Team Stats Totals"
          table_data <-
            data %>%
            .parse.bref.team.pg() %>%
            mutate(typeData = "Team",
                   timeframeData = "Total") %>%
            select(typeData, timeframeData, everything()) %>%
            filter(!nameTeam %>% str_detect("Average"))
        }

        if (is_opp_total) {
          table_name <- "Opponent Stats Totals"
          table_data <-
            data %>%
            .parse.bref.team.pg() %>%
            mutate(typeData = "Opponent",
                   timeframeData = "Total") %>%
            select(typeData, timeframeData, everything()) %>%
            filter(!nameTeam %>% str_detect("Average"))
        }

        if (is_team_stats_pg) {
          table_name <- "Team Stats Per Game"
          table_data <-
            data %>%
            .parse.bref.team.pg() %>%
            mutate(typeData = "Team",
                   timeframeData = "PerGame") %>%
            select(typeData, timeframeData, everything()) %>%
            filter(!nameTeam %>% str_detect("Average"))
        }

        if (is_conf) {
          table_name <- "Conference Standings"
          table_data <-
            data %>%
            .parse.bref.team.conference()
        }

        if (is_division) {
          table_name <- "Division Standings"
          table_data <-
            data %>%
            .parse.bref.team.division()
        }

        table_data <-
          table_data %>%
          left_join(df_urls) %>%
          suppressMessages()

        data_frame(
          idTable = table_id,
          nameTable = table_name,
          dataTable = list(table_data)
        )
      }) %>%
      suppressWarnings() %>%
      mutate(urlSeasonBREF = url)

    all_data
  }

.generate_season_urls <-
  function(seasons = 1950:2018) {
    data_frame(
      yearSeasonEnd = seasons,
      yearSeasonStart = yearSeasonEnd - 1,
      yearSeason = yearSeasonEnd,
      slugSeason = glue::glue("{yearSeasonStart}-{substr(yearSeasonEnd, 3, 4)}") %>% as.character(),
      urlSeasonBREF = glue::glue(
        "https://www.basketball-reference.com/leagues/NBA_{yearSeasonEnd}.html"
      ) %>% as.character()
    )
  }

.parse.bref_season_urls <-
  function(urls = c(
    "https://www.basketball-reference.com/leagues/NBA_1970.html",
    "https://www.basketball-reference.com/leagues/NBA_2012.html",
    "https://www.basketball-reference.com/leagues/NBA_1998.html"
  ),
  return_message = TRUE) {
    df <-
      data_frame()

    success <- function(res) {
      url <-
        res$url

      if (return_message) {
        glue::glue("Parsing {url}") %>%
          cat(fill = T)
      }
      .parse_season_url.safe <-
        purrr::possibly(.parse_season_url, data_frame())

      all_data <-
        .parse_season_url.safe(url = url)


      df <<-
        df %>%
        bind_rows(all_data)
    }
    failure <- function(msg) {
      data_frame()
    }
    urls %>%
      future_map(function(x) {
        curl_fetch_multi(url = x, success, failure)
      })
    multi_run()
    df
  }

#' Basketball Reference teams seasons data
#'
#' Get all available team tables from BREF
#' for specified seasons
#'
#' @param seasons vector of years from 1950 to current
#' @param return_message if \code{TRUE} returns message
#' @param assign_to_environment  if \code{TRUE} assigns data
#' @param widen_data if \code{TRUE} returns data in wide form
#'
#' @return a \code{data_frame} with a list of \code{data_frames}
#' @family BREF
#' @family team statistics
#' @export
#' @import purrr dplyr curl stringr tidyr readr glue rvest
#' @examples
#' bref_teams_stats(seasons = 2017:2018)
bref_teams_stats <-
  function(seasons = 2019,
           return_message = TRUE,
           assign_to_environment = TRUE,
           nest_data = FALSE,
           join_data = TRUE,
           widen_data = TRUE) {
    df_urls <-
      .generate_season_urls(seasons = seasons) %>%
      mutate(urlSeasonBREF = as.character(urlSeasonBREF))

    .parse_season_url.safe <-
      purrr::possibly(.parse_season_url, data_frame())

    all_data <-
      df_urls$urlSeasonBREF %>%
      future_map_dfr(function(x) {
        if (return_message) {
          glue::glue("Parsing {x}") %>% cat(fill = T)
        }
        .parse_season_url.safe(url = x)
      })

    all_data <-
      all_data %>%
      left_join(df_urls) %>%
      left_join(.dictionary_bref_tables()) %>%
      select(slugTable, everything()) %>%
      suppressMessages()

    all_data <-
      assign_bref_data(
        data = all_data,
        type = "Teams",
        widen_data = widen_data,
        join_data = join_data,
        nest_data = nest_data,
        assign_to_environment = assign_to_environment
      )

    all_data
  }


# allstar -----------------------------------------------------------------

# all star games ----------------------------------------------------------

#' NBA All Star Games
#'
#' Returns scores from All-Star Games
#'
#' @param include_aba if `TRUE` includes ABA scores
#' @param return_message if `TRUE` returns a message
#'
#' @return a `data_frame`
#' @export
#' @import dplyr purrr stringr rvest readr
#' @examples
#' library(dplyr)
#' library(nbastatR)
#'df_asg <-
#' all_star_games()
#'df_asg %>% glimpse()
#'df_asg %>% count(namePlayerMVP, sort = T)
all_star_games <-
  function(include_aba = T,
           return_message = T) {
    page <-
      'http://www.basketball-reference.com/allstar/' %>%
      read_html()

    year.season_end <-
      page %>%
      html_nodes('td:nth-child(1)') %>%
      html_text() %>%
      parse_number()

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
        urlASGVotingBREF = url.season.all_star_game.league.voting.bref,
        season = url.season.all_star_game.league.voting.bref %>% str_replace('http://www.basketball-reference.com/allstar/', '') %>% str_replace('_voting.html', '')
      ) %>%
      separate(season,
               into = c('slugLeague', 'yearSeason'),
               sep = '\\_') %>%
      mutate(yearSeason = yearSeason %>% as.numeric(),
             hasASGVoting = T) %>%
      dplyr::select(slugLeague,
                    yearSeason,
                    hasASGVoting,
                    everything())

    all_star_data <-
      data_frame(
        slugLeague = id.league,
        slugSeason = id.season,
        yearSeason = year.season_end,
        dateGame = date.game,
        locationGame = location,
        namePlayerMVP = mvps,
        rawScores = scores_raw
      ) %>%
      mutate(urlASGSeason = 'http://www.basketball-reference.com/allstar/' %>% paste0(id.league, "_", year.season_end, '.html')) %>%
      separate(
        locationGame,
        into = c('nameVenue', 'cityVenue', 'stateVenue'),
        sep = '\\, '
      ) %>%
      mutate(
        hasOverTime = rawScores %>% str_detect("OT"),
        rawScores =
          rawScores %>% str_replace_all("(OT)", '') %>% str_replace_all("(2OT)", '') %>%
          gsub('\\(2)', '', .) %>% str_replace('\\(', '') %>% str_replace('\\)', '')
      )

    all_star_data <-
      all_star_data %>%
      separate(rawScores,
               into = c('teamWinner', 'teamLoser'),
               sep = '\\, ') %>%
      mutate(
        ptsWinner = teamWinner %>% parse_number(),
        ptsLoser = teamLoser %>% parse_number(),
        teamWinner = teamWinner %>% str_replace_all("[[:digit:]]", "") %>% str_trim(),
        teamLoser = teamLoser %>% str_replace_all("[[:digit:]]", "") %>% str_trim()
      ) %>%
      left_join(voting_df) %>%
      arrange(desc(yearSeason)) %>%
      suppressMessages() %>%
      suppressWarnings()


    if (!include_aba) {
      all_star_data <-
        all_star_data %>%
        dplyr::filter(slugLeague == "NBA")
    }



    if (return_message) {
      "You got data for all " %>%
        paste0(all_star_data %>% nrow(), " all star games.") %>%
        cat(fill = T)
    }

    all_star_data <-
      all_star_data %>%
      mutate_if(is.character,
                funs(. %>% str_trim()))

    all_star_data
  }



# active_injuries ---------------------------------------------------------

#' Active injuries
#'
#' Returns information of
#' active NBA player injuries from
#' Basketball-Reference
#'
#' @return a \code{data_frame()}
#' @export
#'
#' @examples
#' bref_injuries()
bref_injuries <-
  function() {
    page <-
      "https://www.basketball-reference.com/friv/injuries.fcgi" %>%
      .read_page()

    data <-
      page %>%
      html_table(fill = F) %>%
      flatten_df() %>%
      purrr::set_names(c(
        "namePlayer",
        "nameTeam",
        "dateInjury",
        "descriptionInjury"
      ))
    data <-
      data %>%
      mutate(dateInjury = dateInjury %>% lubridate::mdy()) %>%
      arrange(desc(dateInjury))

    data <- data %>%
      tidyr::separate(descriptionInjury, into = c("statusTypeInjury", "descriptionInjury"), sep = "\\ - ") %>%
      separate(statusTypeInjury, into = c("statusGame", "typeInjury"), sep = "\\(") %>%
      mutate(typeInjury = typeInjury %>% str_remove_all("\\)")) %>%
      mutate_if(is.character, funs(. %>% str_trim())) %>%
      mutate(isOut = statusGame %>% str_to_lower() %>% str_detect("out"))

    data
  }

# awards ------------------------------------------------------------------

#' Basketball Reference Awards
#'
#' @return a \code{data_frame}
#' @export
#'
#' @examples
#' dictionary_bref_awards()
dictionary_bref_awards <-
  function() {
    data_frame(
      nameAward = c(
        "Most Valuable Player",
        "Rookie of the Year",
        "Defensive Player of the Year",
        "Sixth Man of the Year",
        "Most Improved Player",
        "Teammate of the Year",
        "J. Walter Kennedy Citizenship Award",
        "NBA Finals Most Valuable Player",
        "ABA Playoffs Most Valuable Player",
        "All-Star Game Most Valuable Player",
        "Comeback Player of the Year",
        "Sporting News MVP",
        "Sporting News Rookie of the Year",
        "Coach of the Year",
        "Executive of the Year"
      ),
      slugAward = c(
        "mvp",
        "roy",
        "dpoy",
        "smoy",
        "mip",
        "tmoy",
        "citizenship",
        "finals_mvp",
        "playoffs_mvp",
        "all_star_mvp",
        "cpoy",
        "tsn_mvp",
        "tsn_roy",
        "coy",
        "eoy"
      )
    )

  }

.parse_bref_award_url <-
  function(url = "https://www.basketball-reference.com/awards/roy.html") {
    slug_award <-
      url %>% str_replace_all("https://www.basketball-reference.com/awards/|.html", "")

    table_css <-
      glue::glue("#{slug_award}NBA") %>% as.character()
    page <- .parse_page(url = url)
    tables <- page %>% html_table(header = F)
    data <-
      tables[[1]] %>% as_data_frame()

    player_links <-
      page %>%
      html_nodes("td:nth-child(3) a")

    df_players <-
      player_links %>%
      future_map_dfr(function(player_link) {
        player <- player_link %>% html_text()
        slug_player <-
          player_link %>% html_attr('href')
        url_player <-
          str_c("https://www.basketball-reference.com", slug_player)
        slug_player <-
          slug_player %>% str_replace_all("/players/|.html", "") %>%
          str_split("/") %>%
          flatten_chr() %>%
          .[[2]]
        data_frame(
          namePlayer = player,
          slugPlayerBREF = slug_player,
          urlPlayerBREF = url_player
        )
      })

    if (slug_award %in% c("mvp", "citizenship", "tmoy", "dpoy", "mip",
                          "roy", "smoy")) {
      data <-
        data %>%
        dplyr::slice(3:nrow(data)) %>%
        dplyr::select(c(1:3, 5:6))

      data <-
        data %>%
        purrr::set_names(c(
          "slugSeason",
          "slugLeague",
          "namePlayer",
          "agePlayer",
          "slugTeamBREF"
        ))

    }

    if (slug_award == "all_star_mvp") {
      data <-
        data %>%
        dplyr::slice(3:nrow(data)) %>%
        dplyr::select(c(1:4)) %>%
        purrr::set_names(c(
          "slugSeason",
          "slugLeague",
          "namePlayer",
          "slugTeamBREF"
        ))
    }

    if ( slug_award %in% c("finals_mvp",
                "playoffs_mvp")) {
      data <-
        data %>%
        dplyr::slice(3:nrow(data)) %>%
        dplyr::select(c(1:5)) %>%
        purrr::set_names(c(
          "slugSeason",
          "slugLeague",
          "namePlayer",
          "agePlayer",
          "slugTeamBREF"
        ))
    }

    if (slug_award == "coy") {
      data <- data %>%
        dplyr::slice(3:nrow(data)) %>%
        select(1:4) %>%
        purrr::set_names(c(
          "slugSeason",
          "slugLeague",
          "nameCoach",
          "slugTeamBREF"
        ))
    }

    if (slug_award == "eoy") {
      data <- data %>%
        dplyr::slice(3:nrow(data)) %>%
        select(1:4) %>%
        purrr::set_names(c(
          "slugSeason",
          "slugLeague",
          "nameExecutive",
          "slugTeamBREF"
        )) %>%
        mutate(
          isTie = nameExecutive %>% str_detect("Tie"),
          nameExecutive = nameExecutive %>% gsub("\\(Tie)", "", .) %>% str_trim()
        ) %>%
        mutate_if(is.character,
                  funs(str_trim))
    }

  if (data %>% tibble::has_name("agePlayer")) {
    data <-
      data %>%
      mutate_at("agePlayer",
                funs(. %>% as.numeric()))

  }
    if (data %>% tibble::has_name("namePlayer")) {
      data <-
        data %>%
        mutate(
          isTie = namePlayer %>% str_detect("Tie"),
          namePlayer = namePlayer %>% gsub("\\(Tie)", "", .) %>% str_trim()
        ) %>%
        mutate_if(is.character,
                  funs(str_trim))

      data <-
        data %>%
        left_join(df_players) %>%
        suppressMessages() %>%
        distinct()

    }
    data <-
      data %>%
      mutate(slugAward = slug_award,
             urlBREF = url) %>%
      select(slugAward, everything()) %>%
      distinct()


    data
  }

.parse_bref_award_urls <-
  function(urls = c("https://www.basketball-reference.com/awards/dpoy.html"),
           return_message = TRUE) {
    df <-
      data_frame()

    success <- function(res) {
      url <-
        res$url

      if (return_message) {
        glue::glue("Parsing {url}") %>%
          cat(fill = T)
      }
      .parse_bref_award_url.safe <-
        purrr::possibly(.parse_bref_award_url, data_frame())

      all_data <-
        .parse_bref_award_url.safe(url = url)


      df <<-
        df %>%
        bind_rows(all_data)
    }
    failure <- function(msg) {
      data_frame()
    }
    urls %>%
      future_map(function(x) {
        curl_fetch_multi(url = x, success, failure)
      })
    multi_run()
    df
  }

.generate_award_url <-
  function(award = c("Most Improved Player")) {
    award_slug <- award %>% str_to_upper()
    dict_awards <-
      dictionary_bref_awards() %>%
      mutate(titleAward = nameAward %>% str_to_upper())
    df_aw <-
      dict_awards %>%
      filter(titleAward %in% award_slug)

    if (df_aw %>% nrow() == 0) {
      stop("Sorry not a valid award")
    }

    df_aw %>%
      select(nameAward, slugAward) %>%
      mutate(
        urlBREF = glue::glue(
          "https://www.basketball-reference.com/awards/{slugAward}.html"
        ) %>% as.character()
      )
  }

#' Basketball reference awards
#'
#' Award winners from basketball-reference
#'
#' @param awards vector of awards options include \itemize{
#' \item Most Valuable Player
#' \item Rookie of the Year
#' \item Defensive Player of the Year
#' \item Sixth Man of the Year
#' \item Most Improved Player
#' \item Teammate of the Year
#' \item J. Walter Kennedy Citizenship Award
#' \item NBA Finals Most Valuable Player
#' \item ABA Playoffs Most Valuable Player
#' \item All-Star Game Most Valuable Player
#' \item Comeback Player of the Year
#' \item Sporting News MVP
#' \item Sporting News Rookie of the Year
#' \item Coach of the Year
#' \item Executive of the Year
#' }
#' @param return_message if \code{TRUE} returns a message
#'
#' @return a \code{data_frame}
#' @export
#' @family BREF
#' @family awards
#' @examples
#' bref_awards(awards = c("Most Valuable Player", "Rookie of the Year", "Defensive Player of the Year"))
bref_awards <-
  function(awards = c("Most Valuable Player", "Rookie of the Year", "Defensive Player of the Year",
                      "Sixth Man of the Year", "Most Improved Player", "Teammate of the Year",
                      "J. Walter Kennedy Citizenship Award", "NBA Finals Most Valuable Player",
                      "ABA Playoffs Most Valuable Player", "All-Star Game Most Valuable Player",
                      "Comeback Player of the Year", "Sporting News MVP", "Sporting News Rookie of the Year",
                      "Coach of the Year", "Executive of the Year"),
           return_message = T) {
    if (awards %>% purrr::is_null()) {
      stop("Enter an award")
    }
    df_urls <-
      awards %>%
      future_map_dfr(function(award) {
        .generate_award_url(award = award)
      })

    all_data <-
      .parse_bref_award_urls(url = df_urls$urlBREF)

    all_data %>%
      arrange(slugSeason)
  }


# votes -------------------------------------------------------------------


.dictionary_award_css <-
  memoise::memoise(function() {
    data_frame(
      numberColumn = 2:8,
      nameItem = c(
        "namePlayer",
        "agePlayer",
        "slugTeam",
        "votesFirst",
        "pointsVote",
        "pointsMax",
        "pctVote"
      )
    )
  })

.generate_vote_url <-
  memoise::memoise(function(season = 2018) {
    if (season < 1964) {
      stop("Seasons start in 1964")
    }
    url <-
      glue::glue("https://www.basketball-reference.com/awards/awards_{season}.html")

    data_frame(yearSeason = season, urlBREF = url)
  })

.parse_vote_url <-
  memoise::memoise(function(url = "https://www.basketball-reference.com/awards/awards_1985.html") {
    page <-
      .read_page(url = url)
    season_end <- url %>% str_split("awards_") %>% flatten_chr() %>% .[[2]] %>% str_replace_all("\\.html", "") %>% as.numeric()
    season <- generate_season_slug(season = season_end)
    ids <-
      page %>% html_nodes('div') %>% html_attr('id') %>% unique() %>% discard(is.na)

    dict_cols <- .dictionary_award_css()
    award_ids <- c("div_mvp", "div_dpoy","div_smoy", "div_roy", "div_mip")

    match_ids <- ids[ids %in% award_ids]

    all_data <-
      match_ids %>%
      future_map_dfr(function(id){
        id_css <- glue::glue("#{id} table")

        table_node <-
          page %>%
          html_nodes(id_css)

        df_items <-
          2:8 %>%
          future_map_dfr(function(x){
            no_css <-
              table_node %>%
              html_nodes(glue::glue("td:nth-child({x})"))

            values <- no_css %>% html_text() %>% as.character()

            data_frame(value = values) %>%
              mutate(numberColumn = x,
                     idRow = 1:n())


          })

        df <-
          df_items %>%
          left_join(dict_cols) %>%
          select(idRow, nameItem, value) %>%
          mutate(slugTable = id %>% str_replace_all("div_", "")) %>%
          suppressMessages()

        df <-
          df %>%
          spread(nameItem, value) %>%
          mutate_at(c("agePlayer", "pctVote", "pointsVote", "votesFirst"),
                    funs(as.numeric)) %>%
          select(slugTable, idRow:namePlayer, slugTeam, pctVote, votesFirst, everything())

        df

      })

    player_nodes <-
      page %>%
      html_nodes("th+ .left a")

    df_players <-
      seq_along(player_nodes) %>%
      future_map_dfr(function(x){
        player <- player_nodes[[x]] %>% html_text()
        slug <- player_nodes[[x]] %>% html_attr('href') %>%
          str_replace_all("/players/|.html", "") %>%
          str_split("/") %>%
          flatten_chr() %>%
          .[[2]]
        data_frame(namePlayer = player, slugPlayerBREF = slug)
      })

    all_data <-
      all_data %>%
      left_join(df_players) %>%
      mutate(yearSeason = season_end,
             slugSeason = season,
             urlBREF = url) %>%
      select(yearSeason, slugSeason, slugTable:namePlayer, slugPlayerBREF, everything()) %>%
      select(-idRow) %>%
      suppressMessages() %>%
      distinct()

    all_data <-
      all_data %>%
      group_by(slugTable) %>%
      mutate(rankVotes = min_rank(-pointsVote)) %>%
      ungroup() %>%
      select(yearSeason:slugTable, rankVotes, namePlayer, everything())

    all_data <-
      all_data %>%
      nest(-c(slugSeason, yearSeason, slugTable, urlBREF), .key = dataVotes)

    all_data
  })

.parse_bref_vote_urls <-
  memoise::memoise(function(urls ="https://www.basketball-reference.com/awards/awards_1985.html",
           return_message = TRUE) {
    df <-
      data_frame()

    success <- function(res) {
      url <-
        res$url

      if (return_message) {
        glue::glue("Parsing {url}") %>%
          cat(fill = T)
      }
      .parse_vote_url_safe <-
        purrr::possibly(.parse_vote_url, data_frame())

      all_data <-
        .parse_vote_url_safe(url = url)


      df <<-
        df %>%
        bind_rows(all_data)
    }
    failure <- function(msg) {
      data_frame()
    }
    urls %>%
      future_map(function(x) {
        curl_fetch_multi(url = x, success, failure)
      })
    multi_run()
    df
  })

#' Basketball Reference award votes
#'
#' Acquires seasons awards votes
#'
#' @param seasons vector of seasons starting in 1964
#' @param assign_to_environment if \code{TRUE} assigns a data frame for each
#' nested table into your global environment
#' @param return_message if \code{TRUE} returns a message
#'
#' @return a \code{data_frame()}
#' @family BREF
#' @family awards

#' @export
#'
#' @examples
#' bref_awards_votes(2017)
bref_awards_votes <-
  function(seasons = 2017,
           assign_to_environment = TRUE,
           return_message = TRUE) {
    df_urls <-
      seasons %>%
      future_map_dfr(function(season) {
        .generate_vote_url(season = season)
      })

    all_data <-
      .parse_bref_vote_urls(urls = df_urls$urlBREF, return_message = return_message)

    all_data <-
      all_data %>%
      arrange(yearSeason)

    all_data <-
      all_data %>%
      unnest() %>%
      nest(-c(slugTable), .key = 'dataTable')

    if (assign_to_environment) {
      tables <- all_data$slugTable
      tables %>%
        walk(function(table){
          slug_table <- table %>% str_to_upper()
          df_table <-
            all_data %>%
            filter(slugTable == table) %>%
            select(dataTable) %>%
            unnest()

          df_table <-
            df_table %>%
            mutate(isWinner = ifelse(rankVotes == 1, TRUE, FALSE))
          table_name <-
            glue::glue("data{slug_table}Votes")

          assign(table_name, value = df_table, envir = .GlobalEnv)
        })
    }

    all_data
  }
