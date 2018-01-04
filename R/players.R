
parse_player_json <- function(json, player = player, season = season, mode, measure, season_type) {
  table_length <-
    json$resultSets$rowSet %>% length()
  table_slug <- json$resource

  season_slug <-
    generate_season_slug(season = season)

  all_data <-
    1:table_length %>%
    map_df(function(x) {
      x %>% message()
      table_name <-
        json$resultSets$name[x]

      df_parameters <- json$parameters %>% flatten_df()

      df_parameters <-
        df_parameters %>%
        purrr::set_names(names(df_parameters) %>% resolve_nba_names()) %>%
        munge_nba_data()
      df_parameters <-
        df_parameters %>%
        mutate_at(df_parameters %>% dplyr::select(matches("is[A-Z]")) %>% names(),
                  funs(ifelse(. == "Y", 1, 0) %>% as.logical())) %>%
        mutate(numberTable = x,) %>%
        select(one_of(c("numberTable", "typeMeasure", "modeSearch")), everything()) %>%
        suppressWarnings()

      json_names <-
        json$resultSets$headers[[x]]

      actual_names <-
        json_names %>%
        resolve_nba_names()

      data <-
        json$resultSets$rowSet[[x]] %>%
        dplyr::as_data_frame()

      if (data %>% nrow() == 0) {
        return(invisible())
      }

      data <-
        data  %>%
        purrr::set_names(actual_names) %>%
        munge_nba_data() %>%
        mutate(numberTable = x,
               namePlayer = player,
               slugSeason = season_slug,
               typeMeasure = measure,
               modeSearch = mode,
               typeSeason = season_type) %>%
        dplyr::select(one_of(
          c(
            "typeSeason",
            "modeSearch",
            "typeMeasure",
            "numberTable",
            "slugSeason",
            "namePlayer"
          )
        ), everything()) %>%
        suppressWarnings()

      if (table_name == "NextNGames") {
        data <-
          data %>%
          unite(nameTeamHome, cityTeamHome, teamNameHome,  sep =  " ") %>%
          unite(nameTeamAway, cityTeamAway, teamNameAway,  sep =  " ") %>%
          mutate(dateGame = dateGame %>% lubridate::mdy())
      }

      if (data %>% tibble::has_name("typeShot")){
        data <-
          data %>%
          dplyr::rename(typeFilter = typeShot)
      }

      if (table_name == "PlayersSeasonTotals"){
        if (data %>% has_name("namePlayer")) {
          data <-
            data %>%
            dplyr::rename(typeFilter = namePlayer)
        }

      }

      if (table_name == "TeamOverall"){
      data <-
        data %>%
          mutate(nameGroup = "Players")

      }
      if (table_name == "OverallPlayerDashboard") {
        return(invisible())
      }

      if (table_name %in% c("OverallPlayerDashboard", "ByYearPlayerDashboard")){
        data <-
          data %>%
          dplyr::rename(slugSeasonSearch = slugSeason,
                        slugSeason = typeFilter) %>%
          mutate(modeSearch = mode,
                 typeMeasure = measure,
                 namePlayer = player)

      }

      data <-
        data %>%
        left_join(df_parameters) %>%
        dplyr::select(one_of(names(df_parameters)), everything()) %>%
        suppressMessages() %>%
        select(-numberTable) %>%
        mutate(nameTable = table_name) %>%
        select(nameTable, everything()) %>%
        dplyr::select(-one_of("idLeague")) %>%
        remove_zero_sum_cols() %>%
        mutate(slugTable = table_slug,
               yearSeason = season) %>%
        suppressWarnings() %>%
        suppressMessages()

      data <-
        data %>%
        dplyr::select(nameTable:slugSeason, yearSeason, everything())

      data <-
        data %>%
        dplyr::select(-one_of(c( "resultGame", "locationGame",
                                 "nameSeasonMonth", "segmentSeason", "rangeDaysRest"))) %>%
        suppressWarnings()

      if (table_name == "ByYearTeamDashboard") {
        if (data %>% tibble::has_name("slugSeason")) {
        data <-
          data %>%
          dplyr::rename(slugSeasonSearch = slugSeason)
        }

        if (data %>% tibble::has_name("groupByYear")) {
          data <-
            data %>%
            dplyr::rename(slugSeason = groupByYear)
        }
      }


      if (table_name %in% c("OverallTeamDashboard", "OverallTeamPlayerOnOffSummary")) {
        return(invisible())
      }

      if (table_name == "AssistedBy") {
        assign_nba_players()
        data <-
          data %>%
          dplyr::rename(idPlayerAssistedBy = idPlayer) %>%
          dplyr::select(-one_of("namePlayer")) %>%
          left_join(
            df_dict_nba_players %>% select(idPlayerAssistedBy = idPlayer,
                                           namePlayerAssistedBy = namePlayer)
          ) %>%
          suppressMessages()

        data <-
          data %>%
          dplyr::select(matches("type|mode|^is|^id|^name"),
                        everything())
      }

      key_cols <-
        c("slugTable", "nameTable","typeSeason", "slugSeason", "yearSeason", "slugSeasonSearch", "namePlayer",
          "typeMeasure", "modeSearch") %>% unique()

      nest_cols <-
        names(data)[!names(data) %in% key_cols]

      data <-
        data %>%
        nest_(key_col = 'dataTable', nest_cols = nest_cols)
      data
    })

  all_data

}

dictionary_player_tables <-
  function() {
    data_frame(
      nameTable = c(
        "passes",
        "clutch",
        "game splits",
        "general splits",
        "opponent",
        "next n games",
        "player on off details",
        "defense",
        "game logs",
        "rebounding",
        "shot chart detail",
        "shots",
        "year over year",
        "fantasy profile"

      ),
      slugTable = c(
        "playerdashptpass",
        "playerdashboardbyclutch",
        "playerdashboardbygamesplits",
        "playerdashboardbygeneralsplits",
        "playerdashboardbyopponent",
        "playernextngames",
        "leagueplayerondetails",
        "playerdashptshotdefend",
        "playergamelogs",
        "playerdashptreb",
        "shotchartdetail",
        "playerdashptshots",
        "playerdashboardbyyearoveryear",
        "playerfantasyprofile"
      )
    )
  }

get_player_table_data <-
  function(player_id = 1627747,
           table = "year over year",
           measure = "Base",
           season = 2018,
           mode = "PerGame",
           season_type = "Regular Season",
           game_id = NA,
           n_game = NA,
           context_measure = "FGM",
           playoff_round = NA,
           is_plus_minus = F,
           is_rank = F,
           is_pace_adjusted = F,
           outcome = NA,
           location = NA,
           month = NA,
           season_segment = NA,
           date_from = NA,
           date_to = NA,
           opponent_id = NA,
           vs_conf = NA,
           vs_division = NA,
           game_segment = NA,
           period = NA,
           shot_clock =  NA,
           last_n_games = NA,
           return_message = TRUE) {
    df_player_slug_tables <-
      dictionary_player_tables()
    assign_nba_players()

    player <-
      df_dict_nba_players %>%
      filter(idPlayer == player_id) %>%
      pull(namePlayer)

    if (return_message) {
      glue::glue("Acquiring {player} {season} {season_type} {measure} {table} {mode} data") %>% message()
    }

    table_slug <-
      df_player_slug_tables %>%
      filter(nameTable  == (str_to_lower(table))) %>%
      pull(slugTable)
    URL <- gen_url(table_slug)
    measure_slug <-
      generate_call_slug(x = str_to_title(measure), default_value = "Base")
    mode_slug <-
      generate_call_slug(x = mode, default_value = "PerGame")
    context_measure_slug = generate_call_slug(x = context_measure, default_value = "")
    season_slug <- generate_season_slug(season = season)
    game_id_slug <-
      generate_call_slug(x = game_id, default_value = 0)
    season_type_slug  = generate_call_slug(x = season_type, default_value = "Regular+Season")
    playoff_round_slug = generate_call_slug(x = playoff_round, default_value = 0)
    plus_minus_slug <-
      generate_call_slug(x = is_plus_minus , default_value = "N")
    rank_slug <-
      generate_call_slug(x = is_rank , default_value = "N")
    pace_slug <-
      generate_call_slug(x = is_pace_adjusted , default_value = "N")
    outcome_slug <-
      generate_call_slug(x = outcome , default_value = "")
    location_slug <-
      generate_call_slug(x = location , default_value = "")
    month_slug <- generate_call_slug(x = month , default_value = 0)
    season_segment_slug <-
      generate_call_slug(x = season_segment , default_value = "")
    date_from_slug <-
      generate_call_slug(x = date_from , default_value = "")
    date_to_slug <-
      generate_call_slug(x = date_to , default_value = "")
    opponent_id_slug <-
      generate_call_slug(x = opponent_id , default_value = 0)
    vs_conf_slug <-
      generate_call_slug(x = season_segment , default_value = "")
    vs_division_slug <-
      generate_call_slug(x = vs_division , default_value = "")
    game_segment_slug  <-
      generate_call_slug(x = game_segment , default_value = "")
    period_slug <-
      generate_call_slug(x = period , default_value = 0)
    shot_clock_slug <-
      generate_call_slug(x = shot_clock , default_value = "")
    last_n_games_slug <-
      generate_call_slug(x = last_n_games , default_value = 0)
    game_number_slug <-
      generate_call_slug(x = n_game , default_value = 0)
    params <-
      list(
        measureType = measure_slug,
        perMode = mode_slug,
        plusMinus = plus_minus_slug,
        contextMeasure = context_measure_slug,
        paceAdjust = pace_slug,
        rank = rank_slug,
        leagueId = "00",
        season = season_slug,
        seasonType = season_type,
        GameID = game_id_slug,
        TeamID = 0,
        GROUP_ID = 0,
        numberOfGames = game_number_slug,
        poRound = playoff_round_slug,
        playerID = player_id,
        outcome = outcome_slug,
        location = location_slug,
        month = month_slug,
        seasonSegment = season_segment_slug,
        dateFrom = date_from_slug,
        dateTo = date_to_slug,
        opponentTeamId = opponent_id_slug,
        vsConference = vs_conf_slug,
        vsDivision = vs_division_slug,
        gameSegment = game_segment_slug,
        period = period_slug,
        shotClockRange = shot_clock_slug,
        lastNGames = last_n_games_slug
      )
    if (table_slug == "teamvsplayer") {
      names(params)[names(params) %>% str_detect("teamId")] <-
        "playerId"
    }
    nba_h <- get_nba_headers()

    http_call <-
      httr::GET(url = URL, query = params, nba_h)
    url <- http_call$url
    resp <-
      http_call %>%
      httr::content("text", encoding = "UTF-8")

    json <-
      resp %>% jsonlite::fromJSON(simplifyVector = T)
    all_data <-
      parse_player_json(
        json = json,
        player = player,
        season = season,
        mode = mode,
        measure = measure,
        season_type = season_type
      )
    all_data
  }


#' Title
#'
#' @param teams
#' @param team_ids
#' @param all_active_teams
#' @param tables
#' @param measures
#' @param seasons
#' @param modes
#' @param season_types
#' @param playoff_rounds
#' @param is_plus_minus
#' @param is_rank
#' @param is_pace_adjusted
#' @param outcomes
#' @param locations
#' @param months
#' @param season_segments
#' @param date_from
#' @param date_to
#' @param opponent_ids
#' @param vs_confs
#' @param vs_divisions
#' @param game_segments
#' @param periods
#' @param shot_clocks
#' @param last_n_games
#' @param assign_to_environment
#' @param return_messages
#'
#' @return
#' @export
#'
#' @examples
get_players_tables_data <-
  function(players = NULL,
           player_ids = NULL,
           tables = c("year over year", "passes", "game splits"),
           measures = "Base",
           seasons = 2018,
           modes = c("PerGame", "Totals"),
           season_types = "Regular Season",
           playoff_rounds = NA,
           is_plus_minus = F,
           n_games = 20,
           is_rank = F,
           is_pace_adjusted = F,
           outcomes = NA,
           locations = NA,
           months = NA,
           season_segments = NA,
           date_from = NA,
           date_to = NA,
           opponent_ids = NA,
           vs_confs = NA,
           vs_divisions = NA,
           game_segments = NA,
           periods = NA,
           shot_clocks =  NA,
           last_n_games = NA,
           assign_to_environment = T,
           return_message = TRUE) {
    if (!'df_nba_player_dict' %>% exists()) {
      df_nba_player_dict <-
        get_nba_players()

      assign(x = 'df_nba_player_dict', df_nba_player_dict, envir = .GlobalEnv)
    }
    ids <-
      get_nba_players_ids(player_ids = player_ids,
                          players = players)

    input_df <-
      expand.grid(
        player_id = ids,
        table = tables,
        measure = measures,
        season = seasons,
        mode = modes,
        n_game = n_games,
        season_type = season_types,
        playoff_round = playoff_rounds,
        is_plus_minus = is_plus_minus,
        is_rank = is_rank,
        is_pace_adjusted = is_pace_adjusted,
        outcome =  outcomes,
        location = locations,
        month  = months,
        season_segment = season_segments,
        date_from = date_from,
        date_to = date_to,
        opponent_id = opponent_ids,
        vs_conf = vs_confs,
        vs_division = vs_divisions,
        game_segment = game_segments,
        period = periods,
        shot_clock = shot_clocks,
        last_n_games = last_n_games,
        stringsAsFactors = F
      ) %>%
      dplyr::as_data_frame()
    get_player_table_data_safe <-
      purrr::possibly(get_player_table_data, data_frame())

    all_data <-
      1:nrow(input_df) %>%
      map_df(function(x) {
        df_row <-
          input_df %>% slice(x)
        df_row %$%
          get_player_table_data(
            player_id = player_id,
            table = table,
            measure = measure,
            season = season,
            mode = mode,
            season_type = season_type,
            game_id = NA,
            context_measure = NA,
            playoff_round = playoff_round,
            is_plus_minus = is_plus_minus,
            is_rank = is_rank,
            is_pace_adjusted = is_pace_adjusted,
            outcome = outcome,
            location = location,
            month = month,
            season_segment = season_segment,
            date_from = date_from,
            date_to = date_to,
            opponent_id = opponent_id,
            vs_conf = vs_conf,
            vs_division = vs_division,
            game_segment = game_segment,
            period = period,
            shot_clock = shot_clock,
            last_n_games = last_n_games,
            return_message = return_message
          )
      })
    df_dict_table_names <-
      dictionary_player_tables()

    table_names <-
      df_dict_table_names$nameTable %>% map_chr(function(x) {
        generate_data_name(x = x, result = "Player")
      })

    df_dict_table_names <-
      df_dict_table_names %>%
      mutate(tableName = table_names) %>%
      select(-nameTable) %>%
      dplyr::rename(tableSlugName = tableName)

    all_data <-
      all_data %>%
      left_join(df_dict_table_names) %>%
      select(tableSlugName, nameTable, everything()) %>%
      suppressMessages()

    if (assign_to_environment) {
      all_tables <-
        all_data$tableSlugName %>%
        unique()
      all_tables %>%
        walk(function(table) {
          df_tables <-
            all_data %>%
            filter(tableSlugName == table) %>%
            select(-one_of(c("slugTable", "tableSlugName"))) %>%
            unnest() %>%
            remove_na_columns()

          measures <- df_tables$typeMeasure %>% unique()
          if (measures %>% length() >0 ) {
          measures %>%
            walk(function(measure) {
              table_name <-
                table %>%
                str_c(measure)
              df_table <-
                df_tables %>%
                filter(typeMeasure == measure) %>%
                unnest() %>%
                remove_na_columns()
              assign(x = table_name,
                     value = df_table,
                     envir = .GlobalEnv)
            })
          } else{
            table_name <-
              table
            assign(x = table_name,
                   value = df_tables,
                   envir = .GlobalEnv)
            df_tables
          }
        })
    }
    all_data %>%
      remove_na_columns()
  }
