generate_dash_url <-
  function(type = "team",
           query_type = "dash",
           id = "",
           year_season_start = 2017,
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
           player_experience = NA, # c(NA, "Rookie", "Sophomore", "Veteran"),
           player_position =  NA, # c(NA, "F", "C", "G", "C-F", "F-C", "F-G", "G-F"),
           college = NA,
           draft_pick = NA, #c( NA, "First Round", "2nd Round", "1st Pick", "Lottery Pick", "Top 5 Pick", "Top 10 Pick", "Top 15 Pick", "Top 20 Pick", "Top 25 Pick", "Picks 11 Thru 20",  "Picks 21 Thru 30", "Undrafted")

           draft_year = NA,
           game_scope =  NA, #c(NA, "Yesterday", "Last 10"),
           height = NA, # c(NA, "LT 6-0", "GT 6-0", "LT 6-4", "GT 6-4", "LT 6-7", "GT 6-7", "LT 6-10", "GT 6-10", "LT 7-0", "GT 7-0"),
           shot_clock_range = NA, #c(NA, "24-22", "22-18 Very Early", "18-15 Early", "15-7 Average", "7-4 Late", "4-0 Very Late", "ShotClock Off")
           starter_bench = NA # c(NA, "Starters", "Bench"),
  ) {

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
        game_segment %>% clean_to_stem()
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

    if (!draft_pick %>% is.na()) {
      DraftPick = c(
        "First Round",
        "2nd Round",
        "1st Pick",
        "Lottery Pick",
        "Top 5 Pick",
        "Top 10 Pick",
        "Top 15 Pick",
        "Top 20 Pick",
        "Top 25 Pick",
        "Picks 11 Thru 20",
        "Picks 21 Thru 30",
        "Undrafted"
      )
      if (!draft_pick %in% DraftPick) {
        "Sorry draft pick can only be " %>%
          paste0(DraftPick %>% paste0(collapse = ', ')) %>%
          stop(call. = F)
      }
      draft_pick_stem <-
        draft_pick %>% clean_to_stem()
    } else {
      draft_pick_stem <-
        ''
    }

    if (!draft_year %>% is.na()) {
      if (!draft_year > 1947) {
        stop("Draft year must be after 1947")
      }
      draft_year_stem <-
        draft_year
    } else {
      draft_year_stem <-
        ''
    }

    if (!game_scope %>% is.na()) {
      GameScope = c("Yesterday", "Last 10")

      if (!game_scope %in% GameScope) {
        "Sorry game scope can only be " %>%
          paste0(GameScope %>% paste0(collapse = ', ')) %>%
          stop(call. = F)
      }
      game_scope_stem <-
        game_scope %>% clean_to_stem()
    } else {
      game_scope_stem <-
        ''
    }

    if (!player_position %>% is.na()) {
      PlayerPosition = c("F", "C", "G", "C-F", "F-C", "F-G", "G-F")

      if (!player_position %in% PlayerPosition) {
        "Sorry player position can only be " %>%
          paste0(PlayerPosition %>% paste0(collapse = ', ')) %>%
          stop(call. = F)
      }
      player_position_stem <-
        player_position[1] %>% clean_to_stem()
    } else {
      player_position_stem <-
        ''
    }

    if (!height %>% is.na) {
      Height = c(
        "LT 6-0",
        "GT 6-0",
        "LT 6-4",
        "GT 6-4",
        "LT 6-7",
        "GT 6-7",
        "LT 6-10",
        "GT 6-10",
        "LT 7-0",
        "GT 7-0"
      )

      if (!height %in% Height) {
        "Sorry height can only be " %>%
          paste0(Height %>% paste0(collapse = ', ')) %>%
          stop(call. = F)
      }
      height_stem <-
        height %>% clean_to_stem()
    } else {
      height_stem <-
        ''
    }


    if (!college %>% is.na()) {
      college_stem <-
        college[1] %>% clean_to_stem()
    } else {
      college_stem <-
        ''
    }

    if (!country %>% is.na()) {
      country_stem <-
        country %>% clean_to_stem
    } else {
      country_stem <-
        ''
    }
    if (!starter_bench %>% is.na()) {
      StarterBench = c("Starters", "Bench")

      if (!starter_bench %in% SeasonSegment) {
        "Sorry starter/bench can only be " %>%
          paste0(StarterBench %>% paste0(collapse = ', ')) %>%
          stop(call. = F)
      }
      starter_bench_stem <-
        starter_bench[1] %>% clean_to_stem
    } else {
      starter_bench_range_stem <-
        ''
    }

    if (!weight %>% is.na) {
      Weight = c(
        "LT 200",
        "GT 200",
        "LT 225",
        "GT 225",
        "LT 250",
        "GT 250",
        "LT 275",
        "GT 275",
        "LT 300",
        "GT 300"
      )

      if (!weight %in% Weight) {
        "Sorry starter/bench can only be " %>%
          paste0(Weight %>% paste0(collapse = ', ')) %>%
          stop(call. = F)
      }
      weight_stem <-
        weight[1] %>% clean_to_stem()
    } else {
      weight_stem <-
        ''
    }

    if (!player_experience %>% is.na()) {
      PlayerExperience = c("Rookie", "Sophomore", "Veteran")

      if (!player_experience %in% PlayerExperience) {
        "Sorry player experience can only be " %>%
          paste0(PlayerExperience %>% paste0(collapse = ', ')) %>%
          stop(call. = F)
      }
      player_experience_stem <-
        player_experience %>% clean_to_stem()
    } else {
      player_experience_stem <-
        ''
    }

    url <-
      glue::glue("{base}&DateFrom={date_from_stem}&DateTo={date_to_stem}&GameSegment={game_segment_stem}&LastNGames={last_n_games_stem}&LeagueID=00&Location={location_stem}&MeasureType={measure_type_stem}&LastNGames={last_n_games_stem}&Month={month_stem}&OpponentTeamID={opponent_stem}&Outcome={outcome_stem}&PORound={playoff_round_stem}&PaceAdjust={pace_stem}&PerMode={per_mode_type_stem}&Period={period_stem}&PlusMinus={plus_minus_stem}&Rank={rank_stem}&Season={slugSeason}&SeasonSegment={season_segment_stem}&SeasonType={season_type_stem}&ShotClockRange={shot_clock_range_stem}&{slug_type}={id}&VsConference={conference_against_stem}&VsDivision={division_against_stem}&DraftYear={draft_year_stem}&GameScope={game_scope_stem}&PlayerPosition={player_position_stem}&height={height_stem}&Country={country_stem}&College={college_stem}&DraftPick={draft_pick_stem}&Weight={weight_stem}&StarterBench={starter_bench_range_stem}&PlayerExperience={player_experience_stem}") %>%
      as.character()

    url

  }

get_player_season_summary_stats <-
  function(year_season_start = 2017,
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
           player_experience = NA,
           player_position =  NA,
           college = NA,
           draft_pick = NA,
           draft_year = NA,
           game_scope =  NA,
           height = NA,
           shot_clock_range = NA,
           starter_bench = NA,
           return_message = TRUE) {
    if (!'df_nba_team_dict' %>% exists()) {
      df_nba_team_dict <- get_nba_teams()

      assign('df_nba_team_dict', df_nba_team_dict, envir = .GlobalEnv)
    }


    slugSeason <-
      year_season_start %>%
      paste0("-", (year_season_start + 1) %>% substr(3, 4))

    if (return_message) {
      glue::glue("Acquiring all player {mode[1]} {measure} split tables for the {slugSeason} season") %>% message()
    }

    url_json <-
      generate_dash_url(
        type = "player",
        query_type = "dash",
        id = "",
        year_season_start = 2017,
        season_type =  "Regular Season",
        measure = "Base",
        mode = "PerGame",
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
        player_experience = player_experience,
        player_position =  player_position,
        college = college,
        draft_pick = draft_pick,
        draft_year = draft_year,
        game_scope = game_scope,
        height = height,
        shot_clock_range = shot_clock_range,
        starter_bench = starter_bench
      )

    json <-
       url_json %>%
      curl_json_to_vector()

    table_length <-
      json$resultSets$rowSet %>% length()

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
          left_join(df_parameters %>% select(-one_of(c("idTeam")))) %>%
          dplyr::select(one_of(names(df_parameters)), everything()) %>%
          suppressMessages() %>%
          select(-numberTable) %>%
          suppressWarnings()

        df_table <-
          df_table %>%
          mutate(nameTable = table_name,
                 modeSearch = mode,
                 slugSeason,) %>%
          select(nameTable, modeSearch, everything())

        df_table <-
          df_table %>%
          dplyr::select(-one_of("idLeague")) %>%
          dplyr::select(-matches("Group")) %>%
          remove_zero_sum_cols() %>%
          nest(-c(nameTable, slugSeason, modeSearch, typeSeason),
               .key = 'dataTable')
        df_table
      })

  }

get_team_season_summary_stats <-
  function(year_season_start = 2017,
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
           player_experience = NA,
           player_position =  NA,
           college = NA,
           draft_pick = NA,
           draft_year = NA,
           game_scope =  NA,
           height = NA,
           shot_clock_range = NA,
           starter_bench = NA,
           return_message = TRUE) {
    if (!'df_nba_team_dict' %>% exists()) {
      df_nba_team_dict <- get_nba_teams()

      assign('df_nba_team_dict', df_nba_team_dict, envir = .GlobalEnv)
    }


    slugSeason <-
      year_season_start %>%
      paste0("-", (year_season_start + 1) %>% substr(3, 4))

    if (return_message) {
      glue::glue("Acquiring all team {mode[1]} {measure} split tables for the {slugSeason} season") %>% message()
    }

    url_json <-
      generate_dash_url(
        type = "team",
        query_type = "dash",
        id = "",
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
        player_experience = player_experience,
        player_position =  player_position,
        college = college,
        draft_pick = draft_pick,
        draft_year = draft_year,
        game_scope = game_scope,
        height = height,
        shot_clock_range = shot_clock_range,
        starter_bench = starter_bench
      )

    json <-
      url_json %>%
      curl_json_to_vector()

    table_length <-
      json$resultSets$rowSet %>% length()

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
          left_join(df_parameters %>% select(-one_of(c("idTeam")))) %>%
          dplyr::select(one_of(names(df_parameters)), everything()) %>%
          suppressMessages() %>%
          select(-numberTable) %>%
          suppressWarnings()

        df_table <-
          df_table %>%
          mutate(nameTable = table_name,
                 slugSeason,) %>%
          select(nameTable, modeSearch, everything())

        df_table <-
          df_table %>%
          dplyr::select(-one_of("idLeague")) %>%
          dplyr::select(-matches("Group")) %>%
          remove_zero_sum_cols() %>%
          nest(-c(nameTable, modeSearch,slugSeason, typeSeason),
               .key = 'dataTable')
        df_table
      })

  }

#' Get Players Seasons Summary Statistics
#'
#' @param years_season_start
#' @param season_types
#' @param measures
#' @param modes
#' @param is_plus_minus
#' @param is_pace_adjusted
#' @param periods
#' @param is_rank
#' @param game_segments
#' @param divisions_against
#' @param conferences_against
#' @param date_from
#' @param date_to
#' @param last_n_games
#' @param locations
#' @param months
#' @param season_segments
#' @param opponents
#' @param outcomes
#' @param playoff_rounds
#' @param players_experience
#' @param players_positions
#' @param colleges
#' @param draft_picks
#' @param draft_years
#' @param game_scopes
#' @param heights
#' @param shot_clock_ranges
#' @param starters_bench
#' @param assign_to_environment
#' @param add_mode_names
#' @param return_message
#'
#' @return
#' @export
#'
#' @examples
get_player_seasons_summary_stats <-
  function(years_season_start = 2017,
           season_types =  "Regular Season",
           measures = "Base",
           modes = "PerGame",
           is_plus_minus = F,
           is_pace_adjusted = F,
           periods = 0,
           is_rank = F,
           game_segments = NA ,
           divisions_against = NA,
           conferences_against =  NA,
           date_from = NA,
           date_to = NA,
           last_n_games = 0,
           locations = NA,
           months = 0,
           season_segments =  NA,
           opponents = NA,
           outcomes = NA,
           playoff_rounds = 0,
           players_experience = NA,
           players_positions =  NA,
           colleges = NA,
           draft_picks = NA,
           draft_years = NA,
           game_scopes =  NA,
           heights = NA,
           shot_clock_ranges = NA,
           starters_bench = NA,
           assign_to_environment = TRUE,
           add_mode_names = T,
           return_message = TRUE) {

    input_df <-
      expand.grid(year_season_start =  years_season_start,
                  season_type =  season_types,
                  measure = measures,
                  mode = modes,
                  is_plus_minus = is_plus_minus,
                  is_pace_adjusted = is_pace_adjusted,
                  period = periods,
                  is_rank = is_rank,
                  game_segment =game_segments,
                  division_against = divisions_against,
                  conference_againsts = conferences_against,
                  date_from = date_from,
                  date_to = date_to,
                  last_n_games = last_n_games,
                  location = locations,
                  month = months,
                  season_segment =  season_segments,
                  opponent = opponents,
                  outcome = outcomes,
                  playoff_round = playoff_rounds,
                  player_experience = players_experience,
                  player_position =  players_positions,
                  college = colleges,
                  draft_pick = draft_picks,
                  draft_year = draft_years,
                  game_scopes =  game_scopes,
                  height = heights,
                  shot_clock_range = shot_clock_ranges,
                  starter_bench = starters_bench
                  ,stringsAsFactors = F) %>%
      dplyr::as_data_frame()

    all_data <-
      1:nrow(input_df) %>%
      map_df(function(x) {
        df_row <-
          input_df %>% slice(x)

        df_row %$%
          get_player_season_summary_stats(
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
            player_experience = player_experience,
            player_position =  player_position,
            college = college,
            draft_pick = draft_pick,
            draft_year = draft_year,
            game_scope = game_scope,
            height = height,
            shot_clock_range = shot_clock_range,
            starter_bench = starter_bench,
            return_message = return_message
          )
      })

    if (assign_to_environment) {
      all_data %>%
        assign_tables_modes(stat_type = "Player", add_mode_names = add_mode_names)
    }

    all_data

  }

#' Get Teams Summary Stastics
#'
#' @param years_season_start
#' @param season_types
#' @param measures
#' @param modes
#' @param is_plus_minus
#' @param is_pace_adjusted
#' @param periods
#' @param is_rank
#' @param game_segments
#' @param divisions_against
#' @param conferences_against
#' @param date_from
#' @param date_to
#' @param last_n_games
#' @param locations
#' @param months
#' @param season_segments
#' @param opponents
#' @param outcomes
#' @param playoff_rounds
#' @param players_experience
#' @param players_positions
#' @param colleges
#' @param draft_picks
#' @param draft_years
#' @param game_scopes
#' @param heights
#' @param shot_clock_ranges
#' @param starters_bench
#' @param assign_to_environment
#' @param add_mode_names
#' @param return_message
#'
#' @return
#' @export
#'
#' @examples
get_team_seasons_summary_stats <-
  function(years_season_start = 2017,
           season_types =  "Regular Season",
           measures = "Base",
           modes = "PerGame",
           is_plus_minus = F,
           is_pace_adjusted = F,
           periods = 0,
           is_rank = F,
           game_segments = NA ,
           divisions_against = NA,
           conferences_against =  NA,
           date_from = NA,
           date_to = NA,
           last_n_games = 0,
           locations = NA,
           months = 0,
           season_segments =  NA,
           opponents = NA,
           outcomes = NA,
           playoff_rounds = 0,
           players_experience = NA,
           players_positions =  NA,
           colleges = NA,
           draft_picks = NA,
           draft_years = NA,
           game_scopes =  NA,
           heights = NA,
           shot_clock_ranges = NA,
           starters_bench = NA,
           assign_to_environment = TRUE,
           add_mode_names = T,
           return_message = TRUE) {

    input_df <-
      expand.grid(year_season_start =  years_season_start,
                  season_type =  season_types,
                  measure = measures,
                  mode = modes,
                  is_plus_minus = is_plus_minus,
                  is_pace_adjusted = is_pace_adjusted,
                  period = periods,
                  is_rank = is_rank,
                  game_segment =game_segments,
                  division_against = divisions_against,
                  conference_againsts = conferences_against,
                  date_from = date_from,
                  date_to = date_to,
                  last_n_games = last_n_games,
                  location = locations,
                  month = months,
                  season_segment =  season_segments,
                  opponent = opponents,
                  outcome = outcomes,
                  playoff_round = playoff_rounds,
                  player_experience = players_experience,
                  player_position =  players_positions,
                  college = colleges,
                  draft_pick = draft_picks,
                  draft_year = draft_years,
                  game_scopes =  game_scopes,
                  height = heights,
                  shot_clock_range = shot_clock_ranges,
                  starter_bench = starters_bench
                  ,stringsAsFactors = F) %>%
      dplyr::as_data_frame()

    all_data <-
      1:nrow(input_df) %>%
      map_df(function(x) {
        df_row <-
          input_df %>% slice(x)

        df_row %$%
          get_team_season_summary_stats(
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
            player_experience = player_experience,
            player_position =  player_position,
            college = college,
            draft_pick = draft_pick,
            draft_year = draft_year,
            game_scope = game_scope,
            height = height,
            shot_clock_range = shot_clock_range,
            starter_bench = starter_bench,
            return_message = return_message
          )
      })

    if (assign_to_environment) {
      all_data %>%
        assign_tables_modes(stat_type = "Team", add_mode_names = add_mode_names)
    }

    all_data

  }
