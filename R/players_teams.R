.dictionary_nba_queries <-
  memoise(function() {
    tibble(typeQuery = c("splits", "splits", "general", "general", "defense", "defense", "clutch", "clutch",
                             "hustle", "hustle", "shots", "shots", "shot locations", "shot locations"),
               slugQuery = c("teamdashboardbygeneralsplits", "playerdashboardbygeneralsplits", "leaguedashteamstats", "leaguedashplayerstats" ,"leaguedashptdefend", "leaguedashptdefend", "leaguedashteamclutch", "leaguedashplayerclutch","leaguehustlestatsteam", "leaguehustlestatsplayer", "leaguedashteamptshot", "leaguedashplayerptshot",
                             "leaguedashteamshotlocations", "leaguedashplayershotlocations"),
               typeSearch = c("team", "player", "team", "player", "team", "player", "team", "player", "team", "player", "team", "player", "team", "player"))
  })

.generate_dash_url <-
  function(query_type = "general",
           type = "team",
           id = "",
           season = 2017,
           season_type =  "Regular Season",
           measure = "Base",
           mode = "PerGame",
           defense = "Overall",
           is_plus_minus = F,
           is_pace_adjusted = F,
           period = 0,
           is_rank = F,
           game_segment = NA ,
           division_against = NA,
           conference_against =  NA,
           date_from = NA,
           date_to = NA,
           weight = NA,
           last_n_games = 0,
           location = NA,
           month = 0,
           season_segment =  NA,
           opponent = NA,
           outcome = NA,
           playoff_round = 0,
           ahead_or_behind = "Ahead or Behind",
           general_range = "Overall",
           dribble_range = "0 Dribbles",
           touch_time_range = NA,
           closest_defender_range = NA,
           shot_distance_range =  "By Zone",
           point_diff = 5,
           country = NA,
           player_experience = NA, # c(NA, "Rookie", "Sophomore", "Veteran"),
           player_position =  NA, # c(NA, "F", "C", "G", "C-F", "F-C", "F-G", "G-F"),
           college = NA,
           draft_pick = NA, #c( NA, "First Round", "2nd Round", "1st Pick", "Lottery Pick", "Top 5 Pick", "Top 10 Pick", "Top 15 Pick", "Top 20 Pick", "Top 25 Pick", "Picks 11 Thru 20",  "Picks 21 Thru 30", "Undrafted")

           draft_year = NA,
           clutch_time = "Last 5 Minutes",
           game_scope =  NA, #c(NA, "Yesterday", "Last 10"),
           height = NA, # c(NA, "LT 6-0", "GT 6-0", "LT 6-4", "GT 6-4", "LT 6-7", "GT 6-7", "LT 6-10", "GT 6-10", "LT 7-0", "GT 7-0"),
           shot_clock_range = NA, #c(NA, "24-22", "22-18 Very Early", "18-15 Early", "15-7 Average", "7-4 Late", "4-0 Very Late", "ShotClock Off")
           starter_bench = NA # c(NA, "Starters", "Bench"),
  ) {

    if (!'df_nba_team_dict' %>% exists()) {
      df_nba_team_dict <- nba_teams()

      assign('df_nba_team_dict', df_nba_team_dict, envir = .GlobalEnv)
    }
    df_query_dict <-
      .dictionary_nba_queries()

    query_slug <-
      df_query_dict %>%
      filter(typeQuery %>% str_detect(str_to_lower(query_type))) %>%
      filter(typeSearch %>% str_detect(str_to_lower(type))) %>%
      pull(slugQuery)


    base <-
      glue("https://stats.nba.com/stats/{query_slug}?") %>%
      as.character()

    slug_type <-
      case_when(type %>% str_to_lower() == "team" ~ "TeamID",
                TRUE ~  'PlayerID')
    if (season < 1996) {
      stop("Sorry data only goes back to the 1996-97 Season")
    }

    slugSeason <-
      generate_season_slug(season = season)

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

      if (!shot_clock_range %in% ShotClockRange) {
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

      if (!starter_bench %in% StarterBench) {
        "Sorry starter/bench can only be " %>%
          paste0(StarterBench %>% paste0(collapse = ', ')) %>%
          stop(call. = F)
      }
      starter_bench_stem <-
        starter_bench[1] %>% clean_to_stem()
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

    clutch_stem <-
      clutch_time %>% str_replace_all("\\ ", "+")

    ahead_behind_stem <-
      ahead_or_behind %>% str_replace_all("\\ ", "+")
    defense_stem <-
      defense %>%
      str_replace_all("\\ ", "+")

    general_range_stem <-
      general_range %>%   str_replace_all("\\ ", "+")
    dribble_range_stem <-
      dribble_range %>%
      str_replace_all("\\ ", "+")

    shot_distance_range_stem <-
      shot_distance_range %>%
      str_replace_all("\\ ", "+")
    touch_time_range_stem <-
      case_when(touch_time_range %>% is.na() ~ "",
                TRUE ~ str_replace_all(touch_time_range, "\\ ", "\\+"))
    closest_defender_range_stem <-
      case_when(closest_defender_range %>% is.na() ~ "",
                TRUE ~ str_replace_all(closest_defender_range, "\\ ", "\\+"))

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
      glue("{base}&DateFrom={date_from_stem}&DateTo={date_to_stem}&GameSegment={game_segment_stem}&LastNGames={last_n_games_stem}&LeagueID=00&Location={location_stem}&MeasureType={measure_type_stem}&LastNGames={last_n_games_stem}&Month={month_stem}&OpponentTeamID={opponent_stem}&Outcome={outcome_stem}&PORound={playoff_round_stem}&PaceAdjust={pace_stem}&PerMode={per_mode_type_stem}&Period={period_stem}&PlusMinus={plus_minus_stem}&Rank={rank_stem}&Season={slugSeason}&SeasonSegment={season_segment_stem}&SeasonType={season_type_stem}&ShotClockRange={shot_clock_range_stem}&{slug_type}={id}&VsConference={conference_against_stem}&VsDivision={division_against_stem}&DraftYear={draft_year_stem}&GameScope={game_scope_stem}&PlayerPosition={player_position_stem}&height={height_stem}&Country={country_stem}&College={college_stem}&DraftPick={draft_pick_stem}&Weight={weight_stem}&StarterBench={starter_bench_range_stem}&PlayerExperience={player_experience_stem}&clutchTime={clutch_stem}&aheadBehind={ahead_behind_stem}&defenseCategory={defense_stem}&pointDiff={point_diff}&closeDefDistRange={closest_defender_range_stem}&touchTimeRange={touch_time_range_stem}&generalRange={general_range_stem}&dribbleRange={dribble_range_stem}&distanceRange={shot_distance_range_stem}") %>%
      as.character()

    url

  }

.players_teams_season_summary <-
  function(season = 2017,
           type = "player",
           table = "general",
           season_type =  "Regular Season",
           measure = "Base",
           mode = "PerGame",
           defense = "Overall",
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
           country = NA,
           weight = NA,
           height = NA,
           clutch_time = "Last 5 minutes",
           ahead_or_behind = "Ahead or Behind",
           point_diff = 5,
           general_range = "Overall",
           dribble_range = "0 Dribbles",
           shot_distance_range = "By Zone",
           touch_time_range = NA,
           closest_defender_range = NA,
           shot_clock_range = NA,
           starter_bench = NA,
           return_message = TRUE) {
    if (!'df_nba_team_dict' %>% exists()) {
      df_nba_team_dict <- nba_teams()

      assign('df_nba_team_dict', df_nba_team_dict, envir = .GlobalEnv)
    }


    slugSeason <-
      generate_season_slug(season = season)

    if (return_message) {
      glue("Acquiring all {type} {mode} {table} {measure} split tables for the {slugSeason} season") %>% cat(fill = T)
    }

    url_json <-
      .generate_dash_url(
        type =type,
        query_type = table,
        id = "",
        season = season,
        defense = defense,
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
        clutch_time = clutch_time,
        playoff_round = playoff_round,
        player_experience = player_experience,
        player_position =  player_position,
        college = college,
        country = country,
        weight = weight,
        draft_pick = draft_pick,
        draft_year = draft_year,
        game_scope = game_scope,
        height = height,
        shot_clock_range = shot_clock_range,
        starter_bench = starter_bench,
        ahead_or_behind = ahead_or_behind,
        point_diff = point_diff,
        general_range = general_range,
        shot_distance_range = shot_distance_range,
        dribble_range = dribble_range,
        touch_time_range = touch_time_range,
        closest_defender_range = closest_defender_range
      )

    json <-
      url_json %>%
      .curl_chinazi()

    table_length <-
      json$resultSets$rowSet %>% length()

    if (table %>% str_to_lower() == "shot locations") {
      table_length <- 1
    }

    data <-
      1:table_length %>%
      future_map_dfr(function(table_id) {

        table_name <-
          json$resultSets$name[table_id]

        df_parameters <- json$parameters %>% flatten_df()
        df_parameters <-
          df_parameters %>%
          set_names(names(df_parameters) %>% resolve_nba_names())

        df_parameters <-
          df_parameters %>%
          mutate_at(
            df_parameters %>% dplyr::select(dplyr::matches("is[A-Z]")) %>% names(),
            funs(ifelse(. == "Y", 1, 0) %>% as.logical())
          ) %>%
          mutate(numberTable = table_id) %>%
          select(numberTable, everything())


        if (table_name == "ShotLocations") {
          if (type %>% str_to_lower() == "player") {
            df_table <-
              json$resultSets$rowSet %>%
              data.frame(stringsAsFactors = F) %>%
              as_tibble()

            actual_names <-
              c(
                "idPlayer",
                "namePlayer",
                "idTeam",
                "slugTeam",
                "agePlayer",
                "fgmRestrictedArea",
                "fgaRestrictedArea",
                "pctFGRestrictedArea",
                "fgmInThePaintNonRA",
                "fgaInThePaintNonRA",
                "pctFGInThePaintNonRA",
                "fgmMidRange",
                "fgaMidRange",
                "pctFGMidRange",
                "fgmLeftCorner3",
                "fgaLeftCorner3",
                "pctFGLeftCorner3",
                "fgmRightCorner3",
                "fgaRightCorner3",
                "pctFGRightCorner3",
                "fgmAbovetheBreak3",
                "fgaAbovetheBreak3",
                "pctFGAbovetheBreak3",
                "fgmBackcourt",
                "fgaBackcourt",
                "pctFGBackcourt"
              )

            df_table <-
              df_table %>%
              set_names(actual_names) %>%
              munge_nba_data() %>%
              mutate(numberTable = table_id) %>%
              select(numberTable, everything())

          } else {
            df_table <-
              json$resultSets$rowSet %>%
              data.frame(stringsAsFactors = F) %>%
              as_tibble()
            actual_names <-
              c(
                "idTeam",
                "nameTeam",
                "fgmRestrictedArea",
                "fgaRestrictedArea",
                "pctFGRestrictedArea",
                "fgmInThePaintNonRA",
                "fgaInThePaintNonRA",
                "pctFGInThePaintNonRA",
                "fgmMidRange",
                "fgaMidRange",
                "pctFGMidRange",
                "fgmLeftCorner3",
                "fgaLeftCorner3",
                "pctFGLeftCorner3",
                "fgmRightCorner3",
                "fgaRightCorner3",
                "pctFGRightCorner3",
                "fgmAbovetheBreak3",
                "fgaAbovetheBreak3",
                "pctFGAbovetheBreak3",
                "fgmBackcourt",
                "fgaBackcourt",
                "pctFGBackcourt"
              )

            df_table <-
              df_table %>%
              set_names(actual_names) %>%
              munge_nba_data() %>%
              mutate(numberTable = table_id) %>%
              select(numberTable, everything())
          }

        } else {

          df_table <-
            json %>%
            nba_json_to_df(table_id = table_id) %>%
            mutate(numberTable = table_id) %>%
            select(numberTable, everything())
        }

        df_table <-
          df_table %>%
          left_join(df_parameters %>% select(-one_of(c("idTeam")))) %>%
          dplyr::select(one_of(names(df_parameters)), everything()) %>%
          suppressMessages() %>%
          select(-one_of(c("numberTable"))) %>%
          suppressWarnings()

        df_table <-
          df_table %>%
          mutate(nameTable = table_name,
                 modeSearch = mode,
                 slugSeason,
                 yearSeason = season) %>%
          select(nameTable, modeSearch, everything())

        df_table <-
          df_table %>%
          dplyr::select(-one_of("idLeague")) %>%
          dplyr::select(-dplyr::matches("Group")) %>%
          nest(-c(nameTable, slugSeason, yearSeason, modeSearch, typeSeason),
               .key = 'dataTable')
        return(df_table)
      }) %>%
      mutate(nameTable = table,
             typeResult = type) %>%
      dplyr::select(nameTable, typeResult, everything())
    data
  }



#' NBA teams and player statistics
#'
#' Gets NBA summary statistics tables

#' @param types type of data options include \itemize{
#' \item player
#' \item team
#' }
#' @param tables type of table options include \itemize{
#' \item clutch
#' \item defense
#' \item general
#' \item hustle
#' \item shot locations
#' \item shots
#' \item splits
#' }
#' @param seasons vector of seasons
#' @param season_types vector of season types options \itemize{
#' \item Pre Season
#' \item Regular Season
#' \item Playoffs
#' \item All Star
#' }
#' @param measures vector of measures options \itemize{
#' \item Base
#' \item Advanced
#' \item Defense
#' \item Four Factors
#' \item Misc
#' \item Opponent
#' \item Scoring
#' \item Usage
#' }
#' @param modes vector of modes options \itemize{
#' \item Totals
#' \item MinutesPer
#' \item PerGame
#' \item Per48
#' \item Per40
#' \item Per36
#' \item PerMinute
#' \item PerPossession
#' \item PerPlay
#' \item Per100Possessions
#' \item Per100Plays
#' }
#' @param is_plus_minus if `TRUE` uses plus minus
#' @param is_pace_adjusted if `TRUE` is pace adjusted
#' @param periods vector of periods
#' @param is_rank if `TRUE` returns rank
#' @param game_segments vector of game segments options \itemize{
#' \item NA - All
#' \item First Half
#' \item Second Half
#' \item Overtime
#' }
#' @param divisions_against vector of seasons options \itemize{
#' \item NA - ALL
#' \item Atlantic
#' \item Central
#' \item Northwest
#' \item Pacific
#' \item Southeast
#' \item Southwest
#' \item East
#' \item West
#' }
#' @param conferences_against vector of conferences against options \itemize{
#' \item NA - ALL
#' \item East
#' \item West
#' }
#' @param date_from dates from
#' @param date_to dates to
#' @param last_n_games vector games
#' @param locations vector of locations
#' @param months vector of months 0:12
#' @param season_segments vector of seasons segments options \itemize{
#' \item NA
#' \item Pre All-Star
#' \item Post All-Star
#' }
#' @param opponents vector of opponent ids
#' @param outcomes vector of outcomes options \itemize{
#' \item NA - all
#' \item W
#' \item L
#' }
#' @param playoff_rounds vector of playoff rounds options \code{0:4}
#' @param players_experience vector of experience options \itemize{
#' \item NA
#' \item Rookie
#' \item Sophomore
#' \item Veteran
#' }
#' @param colleges vector of colleges
#' @param draft_picks vector of draft picks options \itemize{
#' \item NA
#' \item 	1st Round
#' \item 2nd Round
#' \item 1st Pick
#' \item Lottery Pick
#' \item Top 5 Pick
#' \item Top 10 Pick
#' \item Top 15 Pick
#' \item Top 20 Pick
#' \item Top 25 Pick
#' \item Picks 11 Thru 20
#' \item Picks 21 Thru 30
#' \item Undrafted
#' }
#' @param draft_years numeric vector vector of draft years
#' @param game_scopes vector game scopes options \itemize{
#' \item NA
#' \item Last 10
#' \item Yesterday
#' }
#' @param heights vector of heights options \itemize{
#' \item NA
#' \item 	LT 6-0
#' \item GT 6-0
#' \item LT 6-4
#' \item GT 6-4
#' \item LT 6-7
#' \item GT 6-7
#' \item LT 6-10
#' \item GT 6-10
#' \item LT 7-0
#' \item GT 7-0
#' }
#' @param shot_clock_ranges character vector of shot clock ranges options \itemize{
#' \item NA
#' \item 24-22
#' \item 22-18 Very Early
#' \item 18-15 Early
#' \item 15-7 Average
#' \item 7-4 Late
#' \item 4-0 Very Late
#' \item ShotClock Off
#' }
#' @param starters_bench vector of starter type options \itemize{
#' \item NA
#' \item Bench
#' \item Starters
#' }
#' @param assign_to_environment if `TRUE` assigns tables to environment
#' @param weights vector of weights options \itemize{
#' \item NA
#' \item LT 200
#' \item GT 200
#' \item LT 225
#' \item GT 225
#' \item LT 250
#' \item GT 250
#' \item LT 275
#' \item GT 275
#' \item LT 300
#' \item GT 300
#' }
#' @param countries vector of countries
#' @param add_mode_names if `TRUE` adds mode names
#' @param players_positions vector of player positions options \itemize{
#' \item NA
#' \item C
#' \item F
#' \item G
#' }
#' @param clutch_times clutch options options \itemize{
#' \item NA
#' \item Last 5 Minutes
#' \item Last 4 Minutes
#' \item Last 3 Minutes
#' \item Last 2 Minutes
#' \item Last 1 Minute
#' \item Last 30 Seconds
#' \item Last 10 Seconds
#' }
#' @param ahead_or_behind ahead of behind type options \itemize{
#' \item Ahead or Behind
#' \item Behind or Tied
#' \item Ahead or Tied
#' }
#' @param defenses defense options include \itemize{
#' \item 	Overall
#' \item 3 Pointers
#' \item 2 Pointers
#' \item Less Than 6Ft
#' \item Less Than 10Ft
#' \item Greater Than 15Ft
#' }
#' @param general_ranges general shop type ranges options include \itemize{
#' \item Overall
#' \item Catch and Shoot
#' \item Less Than 10 ft
#' \item Pullups
#' }
#' @param dribble_ranges range of dribbles options include \itemize{
#' \item 0 Dribbles
#' \item 1 Dribble
#' \item 2 Dribbles
#' \item 3-6 Dribbles
#' \item 7+ Dribbles
#' }
#' @param shot_distance_ranges shot distance
#' @param touch_time_ranges touch time range options include \itemize{
#' \item Touch < 2 Seconds
#' \item Touch 2-6 Seconds
#' \item Touch 6+ Seconds
#'
#' }
#' @param closest_defender_ranges closest defender range options include \itemize{
#' \item NA
#' \item 0-2 Feet - Very Tight
#' \item 2-4 Feet - Tight
#' \item 4-6 Feet - Open
#' \item 6+ Feet - Wide Open
#' }
#' @param point_diffs numeric vector between \code{1:5}
#' @param return_message if `TRUE` returns a message
#' @family players
#' @family teams
#' @family summary statistics
#' @return a `tibble`
#' @export
#'
#' @examples
#' teams_players_stats(seasons = 2018, types = c("player", "team"),
#'  modes = c("PerGame", "Totals"),
#'  tables = c("general", "defense", "clutch", "hustle", "shots", "shot locations"))
#'  )

teams_players_stats <-
  function(seasons = 2018,
           types = c("player", "team"),
           tables = c("defense"),
           season_types =  "Regular Season",
           measures = "Base",
           modes = "PerGame",
           defenses = "Overall",
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
           countries = NA,
           weights = NA,
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
           clutch_times = "Last 5 Minutes",
           ahead_or_behind = "Ahead or Behind",
           general_ranges = "Overall",
           dribble_ranges = "0 Dribbles",
           shot_distance_ranges = "By Zone",
           touch_time_ranges = NA,
           closest_defender_ranges = NA,
           point_diffs = 5,
           starters_bench = NA,
           assign_to_environment = TRUE,
           add_mode_names = T,
           return_message = TRUE) {


    if (types %>% str_to_lower() %in% c("player", "team") %>% sum(na.rm = T) == 0) {
      stop("Result type can only be player and/or team")
    }

    input_df <-
      expand.grid(season =  seasons,
                  type = types,
                  table = tables,
                  season_type =  season_types,
                  measure = measures,
                  defense = defenses,
                  mode = modes,
                  general_range =general_ranges,
                  dribble_range = dribble_ranges,
                  touch_time_range = touch_time_ranges,
                  closest_defender_range = closest_defender_ranges,
                  is_plus_minus = is_plus_minus,
                  is_pace_adjusted = is_pace_adjusted,
                  period = periods,
                  is_rank = is_rank,
                  game_segment =game_segments,
                  division_against = divisions_against,
                  conference_against = conferences_against,
                  date_from = date_from,
                  shot_distance_range = shot_distance_ranges,
                  date_to = date_to,
                  last_n_games = last_n_games,
                  location = locations,
                  month = months,
                  clutch_time = clutch_times,
                  ahead_or_behind = ahead_or_behind,
                  point_diff = point_diffs,
                  country = countries,
                  weight = weights,
                  season_segment =  season_segments,
                  opponent = opponents,
                  outcome = outcomes,
                  playoff_round = playoff_rounds,
                  player_experience = players_experience,
                  player_position =  players_positions,
                  college = colleges,
                  draft_pick = draft_picks,
                  draft_year = draft_years,
                  game_scope =  game_scopes,
                  height = heights,
                  shot_clock_range = shot_clock_ranges,
                  starter_bench = starters_bench
                  ,stringsAsFactors = F) %>%
      dplyr::as_tibble()
    players_teams_season_summary_safe <-
      possibly(.players_teams_season_summary, tibble())
    all_data <-
      1:nrow(input_df) %>%
      future_map_dfr(function(x) {
        df_row <-
          input_df %>% slice(x)

        df_row %$%
          players_teams_season_summary_safe(
            season = season,
            table = table,
            type = type,
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
            defense = defense,
            country = country,
            clutch_time = clutch_time,
            weight = weight,
            ahead_or_behind = ahead_or_behind,
            point_diff = point_diff,
            general_range = general_range,
            dribble_range = dribble_range,
            touch_time_range = touch_time_range,
            closest_defender_range = closest_defender_range,
            shot_distance_range = shot_distance_range,
            return_message = return_message
          )
      })

    if (assign_to_environment) {
      types <- all_data$typeResult %>% unique()
      types %>%
        walk(function(type){
          result_type <-
            glue("{type}s") %>%
            str_to_title()
          df_table <-
            all_data %>%
            filter(typeResult == type) %>%
            select(-typeResult)

          tables <-
            df_table$nameTable %>% unique()



          tables %>%
            walk(function(table){
              tn <- table %>%
                str_split("\\ ") %>%
                flatten_chr() %>%
                str_to_title() %>%
                str_c(collapse = "")

              table_name <-
                glue("data{tn}{result_type}") %>%
                as.character()

              data <-
                df_table %>%
                filter(nameTable == table) %>%
                select(-nameTable) %>%
                unnest() %>%
                remove_zero_sum_cols()

              assign(table_name, data, envir = .GlobalEnv)
            })

        })
    }

    all_data

  }
