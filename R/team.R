

.parse_team_json <- function(json, team_id, season, season_type) {
  table_length <-
    json$resultSets$rowSet %>% length()
  table_slug <- json$resource

  df_team_season <-
    .get_team_season_info(season = season,
                         team_id = team_id,
                         season_type = season_type) %>%
    select(one_of(
      c(
        "yearSeason",
        "idTeam",
        "nameTeam",
        "nameConference",
        "nameDivision",
        "urlTeamSeasonLogo"
      )
    ))

  all_data <-
    1:table_length %>%
    future_map_dfr(function(x) {
      table_name <-
        json$resultSets$name[x]

      df_parameters <- json$parameters %>% flatten_df()

      df_parameters <-
        df_parameters %>%
        purrr::set_names(names(df_parameters) %>% resolve_nba_names()) %>%
        munge_nba_data()

      df_parameters <-
        df_parameters %>%
        mutate_at(
          df_parameters %>% dplyr::select(matches("is[A-Z]")) %>% names(),
          funs(ifelse(. == "Y", 1, 0) %>% as.logical())
        ) %>%
        mutate(numberTable = x) %>%
        select(numberTable, everything())

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
        mutate(numberTable = x)

      if (data %>% tibble::has_name("typeShot")) {
        data <-
          data %>%
          dplyr::rename(typeFilter = typeShot)
      }

      if (table_name == "PlayersSeasonTotals") {
        if (data %>% has_name("namePlayer")) {
          data <-
            data %>%
            dplyr::rename(typeFilter = namePlayer)
        }

      }

      if (table_name == "TeamOverall") {
        data <-
          data %>%
          mutate(nameGroup = "Players")

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
        left_join(df_team_season) %>%
        mutate(slugTable = table_slug,
               yearSeason = season) %>%
        suppressWarnings() %>%
        suppressMessages()

      data <-
        data %>%
        dplyr::select(nameTable:slugSeason, yearSeason, everything())

      data <-
        data %>%
        dplyr::select(-one_of(
          c(
            "resultGame",
            "locationGame",
            "nameSeasonMonth",
            "segmentSeason",
            "rangeDaysRest"
          )
        )) %>%
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
            df_dict_nba_players %>% select(
              idPlayerAssistedBy = idPlayer,
              namePlayerAssistedBy = namePlayer
            )
          ) %>%
          suppressMessages()

        data <-
          data %>%
          dplyr::select(matches("type|mode|^is|^id|^name"),
                        everything())
      }

      key_cols <-
        c(
          "slugTable",
          "nameTable",
          "yearSeason",
          "slugSeasonSearch",
          names(df_parameters),
          names(df_team_season)
        ) %>% unique()

      nest_cols <-
        names(data)[!names(data) %in% key_cols]

      data %>%
        nest_(key_col = 'dataTable', nest_cols = nest_cols)
    })

  all_data

}

.get_team_season_info <-
  function(season = 2019,
           team_id = 1610612751,
           season_type = "Regular Season",
           return_message = T) {
    season_slug <-
      generate_season_slug(season)
    season_type_slug <-
      season_type %>%
      clean_to_stem()

    url <-
      glue::glue(
        "https://stats.nba.com/stats/teaminfocommon/?leagueId=00&season={season_slug}&seasonType={season_type_slug}&teamId={team_id}"
      ) %>%
      as.character()

    json <-
      url %>%
      curl_json_to_vector()

    names_md <-
      json$resultSets$headers[[1]] %>% resolve_nba_names()

    df_md <-
      json$resultSets$rowSet[[1]] %>%
      dplyr::as_data_frame() %>%
      purrr::set_names(names_md) %>%
      munge_nba_data()

    names_md <-
      json$resultSets$headers[[2]] %>% resolve_nba_names()

    df_md2 <-
      json$resultSets$rowSet[[2]] %>%
      dplyr::as_data_frame() %>%
      purrr::set_names(names_md) %>%
      munge_nba_data() %>%
      dplyr::rename(idSeason = slugSeason) %>%
      mutate(idSeason = idSeason %>% as.numeric())

    data <-
      df_md %>%
      left_join(df_md2) %>%
      mutate(yearSeason = season) %>%
      tidyr::unite(nameTeam,
                   cityTeam,
                   teamName,
                   sep = " ",
                   remove = F) %>%
      suppressMessages() %>%
      mutate(urlTeamSeasonLogo = generate_team_season_logo(season = yearSeason, slug_team = slugTeam))

    num_names <-
      data %>% select_if(is.numeric) %>% dplyr::select(-matches("^id|^year")) %>% names()
    no_teams <-
      num_names[!num_names %>% str_detect("Team")]
    names(data)[names(data) %in% no_teams] <-
      str_c(names(data)[names(data) %in% no_teams], "Team")
    if (return_message) {
      glue::glue("Acquired {data$nameTeam %>% unique()} {season_slug} team information") %>% cat(fill = T)
    }
    data

  }

#' NBA teams seasons information
#'
#' Acquires information for a teams season
#'
#'
#' @param teams vector of team names
#' @param team_ids vector of team ids
#' @param all_active_teams if \code{TRUE} returns all active teams
#' @param seasons vector of seasons
#' @param season_types type of season options include \itemize{
#' \item Regular Season
#' \item Playoffs
#' \item Pre Season
#' }
#' @param nest_data if `TRUE` nests data
#' @param return_message if `TRUE` returns a message
#'
#' @return a `data_frame`
#' @export
#'
#' @examples
#' teams_seasons_info(teams = "Brooklyn Nets", seasons = c(1984, 1990, 1995, 2018), season_types = "Regular Season")

teams_seasons_info <-
  function(teams = NULL,
           team_ids = NULL,
           all_active_teams = T,
           seasons = 2019,
           season_types = "Regular Season",
           nest_data = F,
           return_message = T) {
    assign_nba_teams()
    team_ids <-
      nba_teams_ids(teams = teams,
                        team_ids = team_ids,
                        all_active_teams = all_active_teams)
    .get_team_season_info_safe <-
      purrr::possibly(.get_team_season_info, data_frame())
    df_input <-
      expand.grid(
        team_id = team_ids,
        season_type =  season_types,
        season = seasons,
        stringsAsFactors = F
      ) %>%
      as_data_frame()

    all_data <-
      1:nrow(df_input) %>%
      future_map_dfr(function(x) {
        df_row <- df_input %>% slice(x)

        data <-
          df_row %$%
          .get_team_season_info_safe(
            season = season,
            team_id = team_id,
            season_type = season_type,
            return_message = return_message
          )
        Sys.sleep(time = 3)
        data
      })

    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-c(slugSeason), .key = dataTeamSeasonPerformance)
    }
    all_data
  }

.dictionary_team_tables <-
  memoise::memoise(function() {
    data_frame(
      nameTable = c(
        "passes",
        "clutch",
        "splits",
        "lineup",
        "opponent",
        "performance",
        "player on off details",
        "player on off summary",
        "player",
        "rebounding",
        "shooting",
        "shot chart detail",
        "shots",
        "team vs player",
        "year over year"

      ),
      slugTable = c(
        "teamdashptpass",
        "teamdashboardbyclutch",
        "teamdashboardbygeneralsplits",
        "teamdashlineups",
        "teamdashboardbyopponent",
        "teamdashboardbyteamperformance",
        "teamplayeronoffdetails",
        "teamplayeronoffsummary",
        "teamplayerdashboard",
        "teamdashptreb",
        "teamdashboardbyshootingsplits",
        "shotchartlineupdetail",
        "teamdashptshots",
        "teamvsplayer",
        "teamdashboardbyyearoveryear"
      )
    )
  })


# general -----------------------------------------------------------------

.get_team_table_data <-
  function(team_id = 1610612751,
           table = "year over year",
           measure = "Base",
           season = 2018,
           mode = "PerGame",
           season_type = "Regular Season",
           game_id = NA,
           vs_player_id = NA,
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
    df_team_slug_tables <-
      .dictionary_team_tables()

    if (return_message) {
      glue::glue("Acquiring {team_id} {season} {season_type} {measure} {table} {mode} data") %>% cat(fill = T)
    }

    table_slug <-
      df_team_slug_tables %>%
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
    vs_player_id_slug = generate_call_slug(x = vs_player_id, default_value = 0)
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
    params <-
      list(
        measureType = measure_slug,
        perMode = mode_slug,
        plusMinus = plus_minus_slug,
        contextMeasure = context_measure_slug,
        paceAdjust = pace_slug,
        rank = rank_slug,
        leagueId = "00",
        VsPlayerID = vs_player_id_slug,
        season = season_slug,
        seasonType = season_type,
        GameID = game_id_slug,
        GROUP_ID = 0,
        poRound = playoff_round_slug,
        teamId = team_id,
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
    slug_param <-
      .generate_param_slug(params = params)

    url <-
      glue::glue("{URL}?{slug_param}") %>% as.character()

    resp <-
      url %>%
      curl() %>%
      readr::read_lines()

    json <-
      resp %>% jsonlite::fromJSON(simplifyVector = T)
    all_data <-
      .parse_team_json(
        json = json,
        team_id = team_id,
        season = season,
        season_type = season_type
      ) %>%
      mutate(idTeam = team_id,
             typeMeasure = measure,
             modeSearch = mode,
             slugSeason = season_slug,
             yearSeason = season) %>%
      dplyr::select(one_of(c("nameTable", "typeMeasure", "modeSearch", "slugSeason", "yearSeason",
                             "typeSeason", "slugSeasonSearch",
                             "idTeam", "nameTeam", "nameConference", "nameDivision", "slugTable",
                             "urlTeamSeasonLogo",
                             "dataTable")
      ), everything()) %>%
      suppressWarnings()
    all_data
  }


#' NBA Team table data by season
#'
#' Returns NBA team data for specified teams
#' and parameters by seasons
#'
#' @param teams vector of NBA team names
#' @param team_ids vector of team ids
#' @param all_active_teams if \code{TRUE} returns data for all active teams
#' @param tables vector of table names options include \itemize{
#' \item splits
#' \item passes
#' \item clutch
#' \item lineup
#' \item opponent
#' \item performance
#' \item player on off details
#' \item player on off summary
#' \item player
#' \item rebounding
#' \item shooting
#' \item shots
#' \item team vs player
#' \item year over year
#' }
#' @param seasons vector of seasons
#' @param modes vector of modes options include \itemize{
#' \item PerGame
#' \item Totals
#' \item MinutesPer
#' \item Per48
#' \item Per40
#' \item Per36
#' \item PerMinute
#' \item PerPossession
#' \item PerPlay
#' \item Per100Possessions
#' \item Per100Plays
#' }#'
#' @param measures vector of measure types options include \itemize{
#' \item Base
#' \item Advanced
#' \item Misc
#' \item Scoring
#' \item Four Factors
#' \item Opponent
#' \item Usage
#' \item Defense
#' }
#' @param season_types vector of season types options include \itemize{
#' \item Regular Season
#' \item Pre Season
#' \item Playoffs
#' \item All Star
#' }
#' @param playoff_rounds vector of playoff rounds options include code{0:4}
#' @param is_plus_minus \code{TRUE} returns plus minus
#' @param is_rank if \code{TRUE} returns rank
#' @param is_pace_adjusted if \code{TRUE} adjusts for pace
#' @param outcomes vector of outcomes options include \itemize{
#' \item NA
#' \item Wins
#' \item Losses
#' }
#' @param locations vector of locations options include \itemize{
#' \item NA
#' \item Home
#' \item Road
#' }
#' @param months vector of game months options include \code{0:12}
#' @param season_segments vector of season segments, options include \itemize{
#' \item NA
#' \item Post All-Star
#' \item Pre All-Star
#' }
#' @param date_from \code{NA} or date from
#' @param date_to \code{NA} or date to
#' @param opponent_ids vector of opponent ids
#' @param vs_confs vector of conferences against options include  \itemize{
#' \item NA
#' \item East
#' \item West
#' }
#' @param vs_divisions vector of divisions against options include \itemize{
#' \item NA
#' \item Atlantic
#' \item Central
#' \item Northwest
#' \item Pacific
#' \item Southeast
#' \item Southwest
#' }
#' @param game_segments vector of game segments options include \itemize{
#' \item NA
#' \item First Half
#' \item Second Half
#' \item Overtime
#' }
#' @param periods vector of periods \code{0:12}
#' @param shot_clock_ranges vector of shot clock ranges options include \itemize{
#' \item  NA,
#' \item 24-22
#' \item 22-18 Very Early
#' \item 18-15 Early
#' \item 15-7 Average
#' \item 7-4 Late
#' \item 4-0 Very Late
#' \item ShotClock Off
#' }
#' @param last_n_games vector of last_n games \code{0:82}
#' @param assign_to_environment if \code{TRUE} assigns data to environment
#' @param return_messages if \code{TRUE} returns message
#'
#' @return a \code{data_frame}
#' @export
#'
#' @examples
#' teams_tables(teams = c("Brooklyn Nets", "New York Knicks"),
#'  seasons = 2017:2018, tables = c("splits", "shooting"), measures = "Base", modes = c("PerGame", "Totals"))
#'
teams_tables <-
  function(teams = NULL,
           team_ids = NULL,
           all_active_teams = F,
           seasons = NULL,
           tables = NULL,
           measures = NULL,
           modes = NULL,
           season_types = "Regular Season",
           playoff_rounds = NA,
           is_plus_minus = F,
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
           assign_to_environment = TRUE,
           return_message = TRUE) {
    if (tables %>% purrr::is_null()) {
      stop("Please enter tables")
    }

    if (modes %>% purrr::is_null()) {
      stop("Please enter modes")
    }

    if (seasons %>% purrr::is_null()) {
      stop("Enter seasons")
    }


    if (measures %>% purrr::is_null()) {
      stop("Please enter measures")
    }
    team_ids <-
      nba_teams_ids(teams = teams,
                        team_ids = team_ids,
                        all_active_teams = all_active_teams)

    input_df <-
      expand.grid(
        team_id = team_ids,
        table = tables,
        measure = measures,
        season = seasons,
        mode = modes,
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
    .get_team_table_data_safe <-
      purrr::possibly(.get_team_table_data, data_frame())

    all_data <-
      1:nrow(input_df) %>%
      future_map_dfr(function(x) {
        df_row <-
          input_df %>% slice(x)
        df_row %$%
          .get_team_table_data_safe(
            team_id = team_id,
            table = table,
            measure = measure,
            season = season,
            mode = mode,
            season_type = season_type,
            game_id = NA,
            vs_player_id = NA,
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
      .dictionary_team_tables()

    table_names <-
      df_dict_table_names$nameTable %>% map_chr(function(x) {
        generate_data_name(x = x, result = "Team")
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
      suppressMessages() %>%
      unique()

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

          has_measure <- df_tables %>% tibble::has_name("typeMeasure")

          if (has_measure) {
            measures <-
            df_tables$typeMeasure %>% unique()
          measures %>%
            walk(function(measure) {
              table_name <-
                table %>%
                str_c(measure)
              df_table <-
                df_tables %>%
                filter(typeMeasure == measure) %>%
                unnest() %>%
                remove_na_columns() %>%
                distinct()
              assign(x = table_name,
                     value = df_table,
                     envir = .GlobalEnv)
            })
          } else{
            df_table <-
              df_tables %>%
              unnest() %>%
              remove_na_columns() %>%
              distinct()

            assign(x = table,
                   value = df_table,
                   envir = .GlobalEnv)
          }
        })
    }
    all_data %>%
      remove_na_columns()
  }



# Shot Chart --------------------------------------------------------------

.get_team_shot_chart <-
  function(season = 2018,
           team_id = 1610612739,
           season_type =  "Regular Season",
           game_id = NA,
           opponent_id = NULL,
           measure = "FGA",
           period = 0,
           month = 0,
           date_from = NA,
           date_to  = NA,
           show_shots = TRUE,
           return_message = T) {
    assign_nba_teams()
   team <-  df_dict_nba_teams %>%
      filter(idTeam == team_id) %>%
      pull(nameTeam)
    table_id <- 1
    player_id <- 0
    slugSeason <- generate_season_slug(season = season)
    if (return_message) {
      glue::glue("{team} {slugSeason} shot data") %>% cat(fill = T)
    }
    URL <- gen_url("shotchartdetail")

    show_shot_slug <- case_when(show_shots ~ 1,
                                TRUE ~ 0)

    game_id_slug <-
      ifelse(game_id %>% is.na(), "",
             game_id)

    date_from_slug <-
      ifelse(date_from %>% is.na(), "",
             date_from %>% clean_to_stem())

    date_to_slug <-
      ifelse(date_to %>% is.na(), "",
             date_to %>% clean_to_stem())


    params <- list(
      SeasonType = season_type,
      LeagueID = "00",
      Season = slugSeason,
      PlayerID = player_id,
      TeamID = team_id,
      GameID = game_id_slug,
      ContextMeasure = measure,
      PlayerPosition = "",
      DateFrom = date_to_slug,
      DateTo = date_from_slug,
      GameSegment = "",
      LastNGames = "0",
      Location = "",
      Month = "0",
      OpponentTeamID = "0",
      Outcome = "",
      SeasonSegment = "",
      VSConference = "",
      VSDivision = "",
      RookieYear = "",
      Period = period,
      StartPeriod = "",
      EndPeriod = "",
      showShots = show_shot_slug
    )

    #params <- utils::modifyList(params, list(...))

    slug_param <-
      .generate_param_slug(params = params)

    url <-
      glue::glue("{URL}?{slug_param}") %>% as.character()

    resp <-
      url %>%
      curl() %>%
      readr::read_lines()

    json <-
      resp %>% jsonlite::fromJSON(simplifyVector = T)

    df_params <- json$parameters %>% flatten_df() %>% as_data_frame()
    param_names <- names(df_params) %>% resolve_nba_names()

    df_params <-
      df_params %>%
      purrr::set_names(param_names) %>%
      mutate(numberTable = table_id,
             nameTeam = team)

    data <-
      json %>%
      nba_json_to_df(table_id = table_id) %>%
      mutate(numberTable = table_id,
             slugSeason = slugSeason,
             yearSeason = season) %>%
      munge_nba_data()

    data <-
      data %>%
      left_join(df_params) %>%
      select(one_of(param_names), everything()) %>%
      remove_zero_sum_cols() %>%
      select(-one_of(c("numberTable", "idLeague"))) %>%
      mutate_if(is.character,
                funs(ifelse(. == "", NA, .))) %>%
      remove_na_columns() %>%
      mutate_at(c("locationX", "locationY"),
                funs(. %>% as.character() %>% readr::parse_number())) %>%
      suppressWarnings() %>%
      suppressMessages() %>%
      select(matches("yearSeason", "slugSeason", "nameTeam"), everything()) %>%
      tidyr::separate(zoneArea, into = c("nameZone", "slugZone"), sep = "\\(") %>%
      mutate(slugZone = slugZone %>% str_replace_all("\\)", ""))

    data
  }


#' Get teams seasons shot charts
#'
#' @param teams vector of team names
#' @param team_ids vector of team ids
#' @param all_active_teams if `TRUE` returns all active teams
#' @param season_types vector of season types options \itemize{
#' \item Pre Season
#' \item Regular Season
#' \item Playoffs
#' \item All Star
#' }
#' @param seasons vector of seasons
#' @param measures vector of measures
#' @param periods vector of periods
#' @param months vector of months
#' @param date_from date from
#' @param date_to date to
#' @param nest_data if `TRUE` nests data
#' @param return_message if `TRUE` returns a message
#'
#' @return a \code{data_frame}
#' @export
#'
#' @examples
#' teams_shots(teams = "Brooklyn Nets",
#' seasons = 2018)
teams_shots <-
  function(teams = NULL ,
           team_ids = NULL,
           all_active_teams = F,
           season_types = "Regular Season",
           seasons =  2018,
           measures = "FGA",
           periods = 0,
           months = 0,
           date_from = NA,
           date_to  = NA,
           nest_data = F,
           return_message = T
  ){
    team_ids <-
      nba_teams_ids(teams = teams,
                        team_ids = team_ids,
                        all_active_teams = all_active_teams)

    input_df <-
      expand.grid(
        team_id = team_ids,
        season_type = season_types,
        season = seasons,
        measure = measures,
        period = periods,
        month = months,
        date_from = NA,
        date_to  = NA,
        stringsAsFactors = F
      ) %>%
      as_data_frame()

    .get_team_shot_chart_safe <-
      purrr::possibly(.get_team_shot_chart, data_frame())

    all_data <-
      1:nrow(input_df) %>%
      future_map_dfr(function(x) {
        df_row <-
          input_df %>% slice(x)
        df_row %$%
          .get_team_shot_chart_safe(
            team_id = team_id,
            season_type = season_type,
            season = seasons,
            return_message = return_message
          )
      })

    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-c('yearSeason', "slugSeason", "idTeam", "nameTeam"), .key = dataShotChart)
    }
    all_data
    }
