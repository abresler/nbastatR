dictionary_nba_queries <-
  function() {
    data_frame(typeQuery = c("splits", "splits", "dash", "dash"),
               slugQuery = c("teamdashboardbygeneralsplits", "playerdashboardbygeneralsplits", "leaguedashteamstats", "leaguedashplayerstats"),
               typeSearch = c("team", "player", "team", "player"))
  }

#' Clean to Stem
#'
#' @param x
#'
#' @return
#' @export
#' @importFrom stringr str_replace
#' @examples
clean_to_stem <- function(x) {
  x <-
    x %>%
    str_replace('\\ ', '\\+') %>%
    str_replace('\\/', '\\2F') %>%
    str_replace("\\'", '%27')

  return(x)

}

get_nba_team_dict <-
  function(teams = c("Washington Wizards", "Utah Jazz", "Toronto Raptors", "San Antonio Spurs",
                     "Sacramento Kings", "Portland Trail Blazers", "Phoenix Suns",
                     "Philadelphia 76ers", "Orlando Magic", "Oklahoma City Thunder",
                     "New York Knicks", "New Orleans Pelicans", "Minnesota Timberwolves",
                     "Milwaukee Bucks", "Miami Heat", "Memphis Grizzlies", "Los Angeles Lakers",
                     "Los Angeles Clippers", "Indiana Pacers", "Houston Rockets",
                     "Golden State Warriors", "Detroit Pistons", "Denver Nuggets",
                     "Dallas Mavericks", "Cleveland Cavaliers", "Chicago Bulls", "Charlotte Hornets",
                     "Brooklyn Nets", "Boston Celtics", "Atlanta Hawks")) {

    if (!'df_nba_team_dict' %>% exists()) {
      df_nba_team_dict <- get_nba_teams()

      assign('df_nba_team_dict', df_nba_team_dict, envir = .GlobalEnv)
    }
    team_slugs <- teams %>% str_c(collapse = "|")

    df_nba_team_dict %>%
      filter(nameTeam %>% str_detect(team_slugs)) %>%
      pull(idTeam)
  }

# url ---------------------------------------------------------------------


generate_split_url <-
  function(type = "team",
           query_type = "splits",
           id = 1610612751,
           season = 2018,
           season_type =  "Regular Season",
           measure = "Base",
           mode = "PerGame",
           is_plus_minus = F,
           is_pace_adjusted = F,
           period = 0,
           is_rank = F,
           game_segment = NA ,
           division_against = NA,
           conference_against =  NA,
           date_from = NA,
           date_to = NA,
           last_n_games = 0,
           location = NA,
           month = 0,
           season_segment =  NA,
           opponent = NA,
           outcome = NA,
           playoff_round = 0,
           shot_clock_range = NA) {

    year_season_start <-
      season - 1

    if (!'df_nba_team_dict' %>% exists()) {
      df_nba_team_dict <- get_nba_teams()

      assign('df_nba_team_dict', df_nba_team_dict, envir = .GlobalEnv)
    }
    df_query_dict <-
      dictionary_nba_queries()

    query_slug <-
      df_query_dict %>%
      filter(typeQuery %>% str_detect(str_to_lower(query_type))) %>%
      filter(typeSearch %>% str_detect(str_to_lower(type))) %>%
      pull(slugQuery)


    base <-
      glue::glue("http://stats.nba.com/stats/{query_slug}?") %>%
      as.character()

    slug_type <-
      case_when(type %>% str_to_lower() == "team" ~ "TeamID",
                TRUE ~  'PlayerID')
    if (year_season_start < 1996) {
      stop("Sorry data only goes back to the 1996-97 Season")
    }

    slugSeason <-
      year_season_start %>%
      paste0("-", (year_season_start + 1) %>% substr(3, 4))

    if (!opponent %>% is.na()) {
      if (!opponent %>% str_to_lower() %in% df_nba_team_dict$nameTeam %>% str_to_lower()) {
        "Opponent must be either " %>%
          paste0(df_nba_team_dict$nameTeam %>% paste0(collapse = ', ')) %>%
          stop()
      }

      opponent_stem <-
        df_nba_team_dict %>%
        mutate(nameTeam = nameTeam %>% str_to_lower()) %>%
        dplyr::filter(nameTeam == opponent %>% str_to_lower()) %>%
        .$idTeam
    } else {
      opponent_stem <-
        0
    }

    if (!conference_against %>% is.na()) {
      if (!conference_against %in% c("East", "West")) {
        stop("Sorry conference against can only be East or West")
      }
      conference_against_stem <-
        conference_against %>% clean_to_stem()
    } else {
      conference_against_stem <-
        ''
    }


    if (!date_from %>% is.na()) {
      date_from_stem <-
        date_from %>% clean_to_stem()
    } else {
      date_from_stem <-
        ''
    }

    if (!date_to %>% is.na()) {
      date_to_stem <-
        date_to %>% clean_to_stem()
    } else {
      date_to_stem <-
        ''
    }

    if (!division_against %>% is.na()) {
      if (!division_against %in% c("Atlantic",
                                   "Central",
                                   "Northwest",
                                   "Pacific",
                                   "Southeast",
                                   "Southwest")) {
        stop(
          "Sorry division against can only be Atlantic, Central\nNorthwest, Pacific, Southeast, orSouthwest"
        )
      }
      division_against_stem <-
        division_against[1] %>% clean_to_stem
    } else {
      division_against_stem <-
        ''
    }

    if (!game_segment %>% is.na()) {
      GameSegment = c("First Half", "Second Half", "Overtime")

      if (!game_segment %in% GameSegment) {
        "Sorry game segment can only be " %>%
          paste0(GameSegment %>% paste0(collapse = ', ')) %>%
          stop(call. = F)
      }
      game_segment_stem <-
        game_segment[1] %>% clean_to_stem()
    } else {
      game_segment_stem <-
        ''
    }

    if (!last_n_games %>% is.na()) {
      if (!last_n_games >= 0) {
        stop("Last N games must be over 0")
      }
      last_n_games_stem <-
        last_n_games
    } else {
      last_n_games_stem <-
        0
    }

    if (!location %>% is.na()) {
      if (!location %in% c("Home", "Road")) {
        stop("Sorry location can only be Home or Road")
      }
      location_stem <-
        location %>% clean_to_stem()
    } else {
      location_stem <-
        ''
    }

    if (!measure %>% is.na()) {
      MeasureType = c("Base",
                      "Advanced",
                      "Defense",
                      "Misc",
                      "Scoring",
                      "Usage",
                      "Four Factors",
                      "Opponent")

      if (!measure %in% MeasureType) {
        "Sorry measure type can only be " %>%
          paste0(measure %>% paste0(collapse = ', ')) %>%
          stop(call. = F)
      }
      measure_type_stem <-
        measure %>% clean_to_stem()
    } else {
      measure_type_stem <-
        'Base'
    }

    if (!outcome %>% is.na()) {
      Outcome = c("W", "L")

      if (!outcome %in% Outcome) {
        "Sorry outcome can only be " %>%
          paste0(Outcome %>% paste0(collapse = ', ')) %>%
          stop(call. = F)
      }
      outcome_stem <-
        outcome %>% clean_to_stem
    } else {
      outcome_stem <-
        ''
    }

    if (playoff_round > 4) {
      stop("Playoff round can only be zero to 4")
    } else {
      playoff_round_stem <-
        playoff_round
    }

    if (is_pace_adjusted) {
      pace_stem <-
        "Y"
    } else {
      pace_stem <-
        "N"
    }

    if (!mode %>% is.na()) {
      PerMode = c(
        "Totals",
        "PerGame",
        "MinutesPer",
        "Per48",
        "Per40",
        "Per36",
        "PerMinute",
        "PerPossession",
        "PerPlay",
        "Per100Possessions",
        "Per100Plays"
      )
      mode <-
        mode[1] %>%
        str_replace('\\ ', '')
      if (!mode[1] %>% clean_to_stem() %in% PerMode) {
        "Sorry per mode can only be " %>%
          paste0(mode[1] %>% paste0(collapse = ', ')) %>%
          stop(call. = F)
      }
      per_mode_type_stem <-
        mode[1] %>% clean_to_stem()
    } else {
      per_mode_stem <-
        'Totals'
    }

    if (month > 12) {
      stop("Month can only be zero to 12")
    } else {
      month_stem <-
        month
    }

    if (period > 14) {
      stop("Period can only be zero to 14")
    } else {
      period_stem <-
        period
    }


    if (is_plus_minus) {
      plus_minus_stem <-
        "Y"
    } else {
      plus_minus_stem <-
        "N"
    }

    if (is_rank) {
      rank_stem <-
        "Y"
    } else {
      rank_stem <-
        "N"
    }

    if (!season_segment %>% is.na()) {
      SeasonSegment = c("Post All-Star", "Pre All-Star")

      if (!season_segment[1] %in% SeasonSegment) {
        "Sorry season segment can only be " %>%
          paste0(SeasonSegment %>% paste0(collapse = ', ')) %>%
          stop(call. = F)
      }
      season_segment_stem <-
        season_segment %>% clean_to_stem()
    } else {
      season_segment_stem <-
        ''
    }

    if (!season_type %>% is.na()) {
      SeasonType = c('Regular Season', 'Pre Season', 'Playoffs', 'All Star')

      if (!season_type %in% SeasonType) {
        "Sorry season type can only be " %>%
          paste0(SeasonType %>% paste0(collapse = ', ')) %>%
          stop(call. = F)
      }
      season_type_stem <-
        season_type %>% clean_to_stem()
    } else {
      season_type_stem <-
        'Regular+Season'
    }

    if (!shot_clock_range %>% is.na()) {
      ShotClockRange = c(
        "24-22",
        "22-18 Very Early",
        "18-15 Early",
        "15-7 Average",
        "7-4 Late",
        "4-0 Very Late",
        "ShotClock Off"
      )

      if (!shot_clock_range %in% SeasonSegment) {
        "Sorry shot clock range can only be " %>%
          paste0(ShotClockRange %>% paste0(collapse = ', ')) %>%
          stop(call. = F)
      }
      shot_clock_range_stem <-
        shot_clock_range %>% clean_to_stem()
    } else {
      shot_clock_range_stem <-
        ''
    }

    url <-
      glue::glue("{base}&DateFrom={date_from_stem}&DateTo={date_to_stem}&GameSegment={game_segment_stem}&LastNGames={last_n_games_stem}&LeagueID=00&Location={location_stem}&MeasureType={measure_type_stem}&LastNGames={last_n_games_stem}&Month={month_stem}&OpponentTeamID={opponent_stem}&Outcome={outcome_stem}&PORound={playoff_round_stem}&PaceAdjust={pace_stem}&PerMode={per_mode_type_stem}&Period={period_stem}&PlusMinus={plus_minus_stem}&Rank={rank_stem}&Season={slugSeason}&SeasonSegment={season_segment_stem}&SeasonType={season_type_stem}&ShotClockRange={shot_clock_range_stem}&{slug_type}={id}&VsConference={conference_against_stem}&VsDivision={division_against_stem}") %>%
      as.character()

    url

  }

# teams -------------------------------------------------------------------
get_nba_team_season_split <-
  function(team_id = 1610612751,
           season = 2018,
           season_type =  "Regular Season",
           measure = "Advanced",
           mode = "PerGame",
           is_plus_minus = F,
           is_pace_adjusted = F,
           period = 0,
           is_rank = F,
           game_segment = NA ,
           division_against = NA,
           conference_against =  NA,
           date_from = NA,
           date_to = NA,
           last_n_games = 0,
           location = NA,
           month = 0,
           season_segment =  NA,
           opponent = NA,
           outcome = NA,
           playoff_round = 0,
           shot_clock_range = NA,
           return_message = T,
           ...) {

    if (!'df_nba_team_dict' %>% exists()) {
      df_nba_team_dict <- get_nba_teams()

      assign('df_nba_team_dict', df_nba_team_dict, envir = .GlobalEnv)
    }

    if (df_nba_team_dict %>%
      filter(idTeam %in% team_id) %>%
      nrow() == 0) {
      stop("Enter valid team id")
    }

    df_team <-
      df_nba_team_dict %>%
      filter(idTeam %in% team_id)

    slugSeason <-
      year_season_start %>%
      paste0("-", (year_season_start + 1) %>% substr(3, 4))

    if (return_message) {
      glue::glue("Acquiring {df_team$nameTeam} team {mode[1]} split tables for the {slugSeason} season") %>% message()
    }

    url_json <-
      generate_split_url(
        type = "team",
        query_type = "splits",
        id = team_id,
        season = season,
        season_type =  season_type,
        measure = measure,
        mode = mode,
        is_plus_minus = is_plus_minus,
        is_pace_adjusted = is_pace_adjusted,
        period = period,
        is_rank = is_rank,
        game_segment = game_segment,
        division_against = division_against,
        conference_against =  conference_against,
        date_from = date_from,
        date_to = date_to,
        last_n_games = last_n_games,
        location = location,
        month = month,
        season_segment =  season_segment,
        opponent = opponent,
        outcome = outcome,
        playoff_round = playoff_round,
        shot_clock_range = shot_clock_range
      )
    con <- curl(url_json)

    json <-
      con %>%
      fromJSON(simplifyVector = T)

    table_length <-
      json$resultSets$rowSet %>% length()

    all_data <-
      1:table_length %>%
      map_df(function(table_id) {
        table_name <-
          json$resultSets$name[table_id]

        df_parameters <- json$parameters %>% flatten_df()

        df_parameters <-
          df_parameters %>%
          purrr::set_names(names(df_parameters) %>% resolve_nba_names())

        df_parameters <-
          df_parameters %>%
          mutate_at(
            df_parameters %>% dplyr::select(matches("is[A-Z]")) %>% names(),
            funs(ifelse(. == "Y", 1, 0) %>% as.logical())
          ) %>%
          mutate(numberTable = table_id) %>%
          select(numberTable, everything())

        df_table <-
          json %>%
          nba_json_to_df(table_id = table_id) %>%
          mutate(numberTable = table_id) %>%
          select(numberTable, everything())

        df_table <-
          df_table %>%
          left_join(df_parameters) %>%
          dplyr::select(one_of(names(df_parameters)), everything()) %>%
          suppressMessages() %>%
          select(-numberTable)


        df_table <-
          df_table %>%
          mutate(nameTable = table_name) %>%
          select(nameTable, modeSearch, everything())


        df_table <-
          df_table %>%
          dplyr::select(-one_of("idLeague")) %>%
          dplyr::select(-matches("Group")) %>%
          mutate(nameTeam = df_team$nameTeam) %>%
          remove_zero_sum_cols() %>%
          nest(-c(nameTable, modeSearch, slugSeason, nameTeam, idTeam),
               .key = 'dataTable')

        df_table
      })

    all_data
  }
#' Get NBA Team data by split
#'
#' @param team_ids vector of team ids
#' @param teams vector of team names
#' @param seasons vector of the start years
#' @param season_types vector of season types options include \itemize{
#' \item Regular Season
#' \item Pre Season
#' \item Playoffs
#' \item All Star
#' }
#' @param measures vector of measure types options include \itemize{
#' \item Base
#' \item Advanced
#' \item Misc
#' \item Scoring
#' \item Four Factors
#' \item Opponent
#' \item Usage
#' }
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
#' }
#' @param is_plus_minus is plus minus \code{TRUE} or \code{FALSE}
#' @param is_pace_adjusted is pace adjusted \code{TRUE} or \code{FALSE}
#' @param periods vector of periods \code{0:12}
#' @param is_rank is rank \code{TRUE} or \code{FALSE}
#' @param game_segments vector of game segments options include \itemize{
#' \item NA
#' \item First Half
#' \item Second Half
#' \item Overtime
#' }
#' @param divisions_against vector of divisions against options include \itemize{
#' \item NA
#' \item Atlantic
#' \item Central
#' \item Northwest
#' \item Pacific
#' \item Southeast
#' \item Southwest
#' }
#' @param conferences_against vector of conferences against options include  \itemize{
#' \item NA
#' \item East
#' \item West
#' }
#' @param date_from vector of dates from
#' @param date_to vector of dates to
#' @param last_n_games vector of last_n games \code{0:82}
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
#' @param opponents vector of opponent names
#' if \code{NA} all teams
#' @param outcomes vector of outcomes options include \itemize{
#' \item NA
#' \item Wins
#' \item Losses
#' }
#' @param playoff_rounds vector of playoff rounds options include code{0:4}
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
#' @param return_message if \code{TRUE} add mode nanmes
#' @param assign_to_environment if \code{TRUE} assigns data to environment
#' @param add_mode_names if \code{TRUE} adds data to environment
#'
#' @return
#' @export
#' @importFrom glue glue
#' @import jsonlite dplyr purrr tibble stringr tidyr magrittr curl readr
#' @examples
#' get_teams_season_stat_splits(teams = c("Brooklyn Nets"), season = 2018, measures = c("Base", "Opponent"), modes = c("PerGame"), assign_to_environment = T, add_mode_names = T, return_message = TRUE)

get_teams_season_stat_splits <-
  function(teams = NULL,
           team_ids = NULL,
           season = 2018,
           season_types = c("Regular Season"),
           # c('Regular Season', 'Pre Season', 'Playoffs', 'All Star'),
           measures = "Base",
           #c("Base", "Advanced", "Misc", "Scoring", "Four Factors", "Opponent"),
           modes = c("PerGame"),
           # c( "PerGame",  "Totals",   "MinutesPer",  "Per48", "Per40", "Per36", "PerMinute", "PerPossession",  "PerPlay",  "Per100Possessions", "Per100Plays"
           is_plus_minus = F,
           is_pace_adjusted = F,
           periods = 0,
           is_rank = F,
           game_segments = NA ,
           # c(NA, "First Half", "Second Half", "Overtime"),
           divisions_against = NA,
           #c(NA, "Atlantic", "Central", "Northwest", "Pacific", "Southeast", "Southwest"),
           conferences_against = NA,
           #c(NA, "East", "West"),
           date_from = NA,
           date_to = NA,
           last_n_games = 0,
           locations = NA,
           #c(NA, "Home", "Road"),
           months = 0,
           season_segments = NA,
           #c(NA, "Post All-Star", "Pre All-Star"),
           opponents = NA,
           outcomes = NA,
           # c(NA, "Wins", "Losses")
           playoff_rounds = 0,
           shot_clock_ranges = NA,
           #c( NA, "24-22",  "22-18 Very Early", "18-15 Early", "15-7 Average", "7-4 Late", "4-0 Very Late",  "ShotClock Off")
           return_message = TRUE,
           assign_to_environment = TRUE,
           add_mode_names = T

  ) {

    if (modes %>% purrr::is_null()){
      stop("Please enter a valid mode")
    }
    ids <-
      get_nba_teams_ids(teams = teams, team_ids = team_ids)
    input_df <-
      expand.grid(
        team_id = ids,
        season = seasons,
        season_type =  season_types,
        measure = measures,
        mode = modes,
        is_plus_minus = is_plus_minus,
        is_pace_adjusted = is_pace_adjusted,
        period = periods,
        is_rank = is_rank,
        game_segment = game_segments,
        division_against = divisions_against,
        conference_against =  conferences_against,
        date_from = date_from,
        date_to = date_to,
        last_n_games = last_n_games,
        location =  locations,
        month = months,
        season_segment = season_segments,
        opponent = opponents,
        outcome = outcomes,
        playoff_round = playoff_rounds,
        shot_clock_range = shot_clock_ranges,
        stringsAsFactors = F
      ) %>%
      dplyr::as_data_frame()

    all_data <-
      1:nrow(input_df) %>%
      map_df(function(x) {
        df_row <-
          input_df %>% slice(x)

        df_row %$%
          get_nba_team_season_split(
            team_id = team_id,
            year_season_start = year_season_start,
            season_type = season_type,
            measure = measure,
            mode = mode,
            is_plus_minus = is_plus_minus,
            is_pace_adjusted = is_pace_adjusted,
            period = period,
            is_rank = is_rank,
            game_segment = game_segment,
            division_against = division_against,
            conference_against = conference_against,
            date_from = date_from,
            date_to = date_to,
            last_n_games = last_n_games,
            location = location,
            month = month ,
            season_segment = season_segment,
            opponent = opponent,
            outcome = outcome ,
            playoff_round = playoff_round,
            shot_clock_range = shot_clock_range,
            return_message = return_message
          )
      })

    if (assign_to_environment) {
      all_data %>%
        assign_tables_modes(stat_type = "Team", add_mode_names = add_mode_names)
    }

    all_data

  }

# players -----------------------------------------------------------------

get_nba_player_season_split <-
  function(player_id = 201960,
           season = 2018,
           season_type =  "Regular Season",
           measure = "Advanced",
           mode = "PerGame",
           is_plus_minus = F,
           is_pace_adjusted = F,
           period = 0,
           is_rank = F,
           game_segment = NA ,
           division_against = NA,
           conference_against =  NA,
           date_from = NA,
           date_to = NA,
           last_n_games = 0,
           location = NA,
           month = 0,
           season_segment =  NA,
           opponent = NA,
           outcome = NA,
           playoff_round = 0,
           shot_clock_range = NA,
           return_message = T,
           ...) {

    if (!'df_nba_team_dict' %>% exists()) {
      df_nba_team_dict <- get_nba_teams()

      assign('df_nba_team_dict', df_nba_team_dict, envir = .GlobalEnv)
    }


    if (!'df_nba_player_dict' %>% exists()) {
      df_nba_player_dict <- get_nba_players()

      assign('df_nba_player_dict', df_nba_team_dict, envir = .GlobalEnv)
    }

    if (df_nba_player_dict %>%
        filter(idPlayer %in% player_id) %>%
        nrow() == 0) {
      stop("Enter valid player id")
    }

    df_player <-
      df_nba_player_dict %>%
      filter(idPlayer %in% player_id)

    slugSeason <-
      season %>% generate_season_slug()

    if (return_message) {
      glue::glue("Acquiring {df_player$namePlayer} {mode[1]} {measure} split tables for the {slugSeason} season") %>% message()
    }

    url_json <-
      generate_split_url(
        query_type = 'split',
        type = "player",
        id = player_id,
        season = season,
        season_type =  season_type,
        measure = measure,
        mode = mode,
        is_plus_minus = is_plus_minus,
        is_pace_adjusted = is_pace_adjusted,
        period = period,
        is_rank = is_rank,
        game_segment = game_segment,
        division_against = division_against,
        conference_against =  conference_against,
        date_from = date_from,
        date_to = date_to,
        last_n_games = last_n_games,
        location = location,
        month = month,
        season_segment =  season_segment,
        opponent = opponent,
        outcome = outcome,
        playoff_round = playoff_round,
        shot_clock_range = shot_clock_range
      )
    con <-
      curl::curl(url_json)

    json <-
      con %>%
      fromJSON(simplifyVector = T)

    table_length <-
      json$resultSets$rowSet %>% length()

    all_data <-
      1:table_length %>%
      map_df(function(table_id) {
        table_name <-
          json$resultSets$name[table_id]

        df_parameters <- json$parameters %>% flatten_df()

        df_parameters <-
          df_parameters %>%
          purrr::set_names(names(df_parameters) %>% resolve_nba_names())

        df_parameters <-
          df_parameters %>%
          mutate_at(
            df_parameters %>% dplyr::select(matches("is[A-Z]")) %>% names(),
            funs(ifelse(. == "Y", 1, 0) %>% as.logical())
          ) %>%
          mutate(numberTable = table_id) %>%
          select(numberTable, everything())
        data <-
          json$resultSets$rowSet[[table_id]] %>% as_data_frame()

        actual_names <-
          json$resultSets$headers[[table_id]] %>% resolve_nba_names()

        if (data %>% nrow() == 0) {
          return(invisible())
        }

        df_table <-
          data %>%
          purrr::set_names(actual_names) %>%
          mutate(numberTable = table_id) %>%
          mutate(idPlayer = player_id)

        df_table <-
          df_table %>%
          left_join(df_parameters) %>%
          dplyr::select(one_of(names(df_parameters)), everything()) %>%
          suppressMessages() %>%
          select(-numberTable)


        df_table <-
          df_table %>%
          mutate(nameTable = table_name) %>%
          mutate(modeSearch = mode) %>%
          select(nameTable, modeSearch, everything())


        df_table <-
          df_table %>%
          dplyr::select(-one_of("idLeague")) %>%
          dplyr::select(-matches("Group")) %>%
          mutate(namePlayer = df_player$namePlayer) %>%
          remove_zero_sum_cols() %>%
          nest(-c(nameTable, modeSearch, slugSeason, namePlayer, idPlayer),
               .key = 'dataTable')

        df_table
      })
    closeAllConnections()
    all_data
  }

#' Get Player Splits
#'
#' @param player_ids vector of player ids
#' @param players  vector of player names
#' @param seasons vector of the start years
#' @param season_types vector of season types options include \itemize{
#' \item Regular Season
#' \item Pre Season
#' \item Playoffs
#' \item All Star
#' }
#' @param measures vector of measure types options include \itemize{
#' \item Base
#' \item Advanced
#' \item Misc
#' \item Scoring
#' \item Usage
#' }
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
#' }
#' @param is_plus_minus is plus minus \code{TRUE} or \code{FALSE}
#' @param is_pace_adjusted is pace adjusted \code{TRUE} or \code{FALSE}
#' @param periods vector of periods \code{0:12}
#' @param is_rank is rank \code{TRUE} or \code{FALSE}
#' @param game_segments vector of game segments options include \itemize{
#' \item NA
#' \item First Half
#' \item Second Half
#' \item Overtime
#' }
#' @param divisions_against vector of divisions against options include \itemize{
#' \item NA
#' \item Atlantic
#' \item Central
#' \item Northwest
#' \item Pacific
#' \item Southeast
#' \item Southwest
#' }
#' @param conferences_against vector of conferences against options include  \itemize{
#' \item NA
#' \item East
#' \item West
#' }
#' @param date_from vector of dates from
#' @param date_to vector of dates to
#' @param last_n_games vector of last_n games \code{0:82}
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
#' @param opponents vector of opponent names
#' if \code{NA} all teams
#' @param outcomes vector of outcomes options include \itemize{
#' \item NA
#' \item Wins
#' \item Losses
#' }
#' @param playoff_rounds vector of playoff rounds options include code{0:4}
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
#' @param return_message if \code{TRUE} add mode nanmes
#' @param assign_to_environment if \code{TRUE} assigns data to environment
#' @param add_mode_names if \code{TRUE} adds data to environment
#'
#' @return
#' @export
#' @importFrom glue glue
#' @import jsonlite dplyr purrr tibble stringr tidyr magrittr curl
#' @examples
#' get_players_season_stat_splits(players = "Jarrett Allen", measures = c("Base", "Advanced"))
get_players_season_stat_splits <-
  function(players =  NULL,
           player_ids = NULL,
           seasons =  2018,
           season_types = c("Regular Season"),
           measures = c("Base"),
           modes = c("PerGame"),
           is_plus_minus = F,
           is_pace_adjusted = F,
           periods = 0,
           is_rank = F,
           game_segments = NA ,
           divisions_against = NA,
           conferences_against = NA,
           date_from = NA,
           date_to = NA,
           last_n_games = 0,
           locations = NA,
           months = 0,
           season_segments = NA,
           opponents = NA,
           outcomes = NA,
           playoff_rounds = 0,
           shot_clock_ranges = NA,
           return_message = TRUE,
           assign_to_environment = TRUE,
           add_mode_names = T
  ) {

    if (seasons %>% purrr::is_null()) {
      "Please enter seasons"
    }

   ids <-
     get_nba_players_ids(players = players,
                        player_ids = player_ids)

    input_df <-
      expand.grid(
        player_id = ids,
        season = seasons,
        season_type =  season_types,
        measure = measures,
        mode = modes,
        is_plus_minus = is_plus_minus,
        is_pace_adjusted = is_pace_adjusted,
        period = periods,
        is_rank = is_rank,
        game_segment = game_segments,
        division_against = divisions_against,
        conference_against =  conferences_against,
        date_from = date_from,
        date_to = date_to,
        last_n_games = last_n_games,
        location =  locations,
        month = months,
        season_segment = season_segments,
        opponent = opponents,
        outcome = outcomes,
        playoff_round = playoff_rounds,
        shot_clock_range = shot_clock_ranges,
        stringsAsFactors = F
      ) %>%
      dplyr::as_data_frame()
    get_nba_player_season_split_safe <-
      purrr::possibly(get_nba_player_season_split, data_frame())
    all_data <-
      1:nrow(input_df) %>%
      map_df(function(x) {
        df_row <-
          input_df %>% slice(x)

        get_nba_player_season_split(
            player_id = df_row$player_id,
            season =  df_row$season,
            season_type =  df_row$season_type,
            measure =  df_row$measure,
            mode =  df_row$mode,
            is_plus_minus =  df_row$is_plus_minus,
            is_pace_adjusted =  df_row$is_pace_adjusted,
            period =  df_row$period,
            is_rank =  df_row$is_rank,
            game_segment =  df_row$game_segment,
            division_against =  df_row$division_against,
            conference_against =  df_row$conference_against,
            date_from =  df_row$date_from,
            date_to =  df_row$date_to,
            last_n_games =  df_row$last_n_games,
            location =  df_row$location,
            month = df_row$month ,
            season_segment =  df_row$season_segment,
            opponent =  df_row$opponent,
            outcome =  df_row$outcome ,
            playoff_round =  df_row$playoff_round,
            shot_clock_range =  df_row$shot_clock_range,
            return_message = return_message
          )
      })

    if (assign_to_environment) {
      all_data %>%
        assign_tables_modes(stat_type = "Player", add_mode_names = add_mode_names)
    }

    all_data

  }
