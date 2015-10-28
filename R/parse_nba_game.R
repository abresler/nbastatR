game_id = "0011500022"

nba_json_to_df <-
  . %>%
  data.frame %>%
  tbl_df

valid_df <-
  . %>%
  data.frame %>%
  nrow > 0



parse_nba_game <- function(game_id) {
  packages <- #need all of these installed including some from github
    c('dplyr',
      'magrittr',
      'jsonlite',
      'tidyr',
      'stringr',
      'tidyjson')
  options(warn = -1)
  lapply(packages, library, character.only = T)

  base <-
    'http://stats.nba.com/stats/boxscoresummaryv2?GameID='

  url_boxscore <-
    base %>%
    paste0(game_id)

  json_data <-
    url_boxscore %>%
    fromJSON(flatten = T, simplifyDataFrame = T)

  table_names <-
    json_data$resultSets$name

  if ('GameSummary' %in% table_names) {
    if (json_data$resultSets$rowSet[1] %>% data.frame %>% nrow > 0) {
      game_summary_data <-
        json_data$resultSets$rowSet[1] %>% nba_json_to_df
      names(game_summary_data) <-
        c(
          "date.game",
          "sequence.game",
          "id.game",
          "id.game_status",
          "text.games_status",
          "gamecode",
          "id.home_team",
          "id.away_team",
          "year.season_start",
          "period.live",
          "live_pc_time",
          "id.tv_broadcast",
          "period.live_broadcast",
          "status.wh"
        )
      game_summary_data %<>%
        separate(gamecode, sep = '\\/', c('date.game', 'teamslugs')) %>%
        separate(date.game,
                 into = c('date', 'drop'),
                 sep = 'T') %>%
        dplyr::select(-drop) %>%
        mutate(
          slug.away_team = teamslugs %>% substr(start = 1, stop = 3),
          slug.home_team = teamslugs %>% substr(4, 6),
          date = date %>% as.Date('%Y-%m-%d'),
          is.final = text.games_status %>% str_detect("Final")
        ) %>%
        dplyr::select(
          -c(
            live_pc_time,
            period.live_broadcast,
            teamslugs,
            text.games_status,
            date.game
          )
        ) %>%
        dplyr::select(
          date.game = date,
          sequence.game,
          id.game,
          is.final,
          slug.home_team,
          slug.away_team,
          everything()
        )

      names_numeric_cols <-
        game_summary_data %>%
        dplyr::select(-c(
          date.game,
          is.final,
          id.game,
          slug.home_team,
          slug.away_team,
          id.tv_broadcast
        )) %>%
        names

      game_summary_data %<>%
        mutate_each_(funs(as.numeric), vars = names_numeric_cols)
    }
  } else {
    game_summary_data <-
      data_frame()
  }

  if(game_summary_data %>% nrow > 0) {
    game_day <-
      game_info_data$date.game %>% day
    game_month <-
      game_info_data$date.game %>% month

    base_year <-
      game_info_data$date.game %>% year

    if (game_month < 11 & game_month > 6 & game_day <= 27) {
      season_type <-
        'Pre Season'
    }
    if (game_month  %in% c(10, 11, 1, 2, 3, 4)) {
      if (game_month == 10) {
        if (game_day >= 27) {
          season_type <-
            c('Regular Season')
        }
      }
      if (game_month %in% c(11, 1, 2, 3, 4)) {
        season_type <-
          c('Regular Season')
      }
      if (game_month == 4) {
        if (game_day <= 15) {
          season_type <-
            'Regular Season'
        }
      }
    }

    if (game_month  %in% c(4, 5, 6)) {
      if (game_month == 4) {
        if (game_day > 15) {
          season_type <-
            'Playoffs'
        }
      }
      if (game_month %in% c(5, 6)) {
        season_type <-
          'Playoffs'
      }
    }

  }

    if(game_info_data$date.game %>% month > 6){
      year_season_start <-
        game_info_data$date.game %>% year
    } else{
      year_season_start <-
        game_info_data$date.game %>% year - 1
      game_summary_data %<>%
        mutate(id.season, type.season = seaon_type) %>%
        dplyr::select(id.season, date.game, id.game, everything()
        )
    }

    end_year <-
      (year_season_start + 1) %>% as.character() %>% substr(start = 3,stop = 4)
    id.season <-
      year_season_start %>%
      paste0('-', end_year)
  } else {
    id.season <-
      NA
  }

  if ('OtherStats' %in% table_names) {
    if (json_data$resultSets$rowSet[2] %>% data.frame %>% nrow > 0) {
      other_stat_data <-
        json_data$resultSets$rowSet[2] %>%
        nba_json_to_df

      names(other_stat_data) <-
        c(
          'id.league',
          'id.team',
          'slug.team',
          'team.city',
          'pts.paint',
          'pts.second_chance',
          'pts.fast_break',
          'lead.largest',
          'lead.changes',
          'game.ties'
        )

      names_numeric_cols <-
        other_stat_data %>%
        dplyr::select(pts.paint:game.ties) %>% names

      other_stat_data %<>%
        dplyr::select(-id.league) %>%
        mutate(id.season)
        mutate_each_(funs(as.numeric), vars = names_numeric_cols)
    }
  } else {
    other_stat_data <-
      data_frame()
  }


  if ('Officials' %in% table_names) {
    if (json_data$resultSets$rowSet[3] %>% data.frame %>% nrow > 0) {
      ref_data <-
        json_data$resultSets$rowSet[3] %>%
        nba_json_to_df

      names(ref_data) <-
        c('id.official',
          'name.first',
          'name.last',
          'jersey.official')

      ref_data %<>%
        mutate(
          jersey.official = jersey.official %>% str_trim %>% as.numeric,
          official = paste(name.first, name.last),
          id.game = game_id
        ) %>%
        dplyr::select(id.game, id.official, everything()) %>%
        dplyr::select(-c(name.first, name.last))
    }
  } else {
    ref_data
      data_frame()
  }

  if ('InactivePlayers' %in% table_names) {
    if (json_data$resultSets$rowSet[4] %>% data.frame %>% nrow > 0) {
      inactive_data <-
        json_data$resultSets$rowSet[4] %>%
        nba_json_to_df

      inactive_data <-
        c(
          "id.player",
          "name.first",
          "name.last",
          "jersey",
          "id.team",
          "city.team",
          "name.team",
          "slug.team"
        )

      inactive_data %<>%
        mutate(
          player = paste(first_name, last_name),
          id.game = game_id,
          jersey = jersey %>% str_trim %>% as.numeric
        ) %>%
        dplyr::select(id.game, id.player, everything()) %>%
        dplyr::select(-c(name.first, name.last)))
    }
  } else {
    inactive_data <-
      data_frame()
  }

  if ('GameInfo' %in% table_names) {
    if (json_data$resultSets$rowSet[5] %>% data.frame %>% nrow > 0) {
      game_info_data <-
        json_data$resultSets$rowSet[5] %>%
        nba_json_to_df

      names(game_info_data) <-
        c("date.game", "attendance", "game_time")

      game_info_data %<>%
        separate(game_time, into = c('hours', 'minutes'), sep = '\\:') %>%
        mutate(id.game = game_id,
               date.game = date.game %>% as.Date('%A, %B %d, %Y'),
               attendance = attendance %>% as.numeric,
               time.game = ((hours %>% as.numeric * 60) + (minutes %>% as.numeric())
               )
               ) %>%
        dplyr::select(id.game, everything()) %>%
        dplyr::select(-c(hours, minutes))

    }
  } else {
    game_info_data <-
      data_frame()
  }


  if ('LineScore' %in% table_names) {
    if (json_data$resultSets$rowSet[6] %>% data.frame %>% nrow > 0) {
      line_score_data <-
        json_data$resultSets$rowSet[6] %>%
        nba_json_to_df

      names(line_score_data) <-
        c("game_date_est", "sequence.game", "id.game", "id.team", "slug.team",
          "city.team",'name.team', "team.record", "pts.qtr1", "pts.qtr2",
          "pts.qtr3", "pts.qtr4", "pts.ot1", "pts.ot2", "pts.ot3", "pts.ot4",
          "pts.ot5", "pts.ot6", "pts.ot7", "pts.ot8", "pts.ot9", "pts.ot10",
          "pts")

      line_score_data %<>%
        separate(team.record, sep = '\\-', c('wins', 'losses')) %>%
        dplyr::select(-game_date_est) %>%
        dplyr::select(id.game,everything())

      names_numeric_cols <-
        line_score_data %>%
        dplyr::select(-c(slug.team, city.team, id.game, name.team)) %>%
        names

      line_score_data %<>%
        mutate_each_(funs(as.numeric), vars = names_numeric_cols)

      line_score_data %<>%
        left_join(
          line_score_data %>%
            group_by(id.game) %>%
            dplyr::filter(pts == max(pts)) %>%
            dplyr::select(id.game, slug.winner = slug.team)
        ) %>%
        mutate(is.winning_team = ifelse(slug.winner == slug.team, T, F)) %>%
        dplyr::select(id.game, is.winning_team, everything())
    }
  } else {
    line_score_data <-
      data_frame()
  }

  if ('LastMeeting' %in% table_names) {
    if (json_data$resultSets$rowSet[7] %>% data.frame %>% nrow > 0) {
      last_meeting_data <-
        json_data$resultSets$rowSet[7] %>%
        nba_json_to_df

      last_meeting_data <-
        c("id.game", "id.game.last", "last_game_date_est", "id.home_team.last",
          "city.home_team.last", "name.home_team.last", "slug.home_team.last",
          "pts.home_team.last", "id.away_team.last", "city.away_team.last",
          "name.away_team.last", "slug.away_team.last",
          "pts.away_team.last")

      last_meeting_data %<>%
        separate(last_game_date_est,
                 sep = 'T',
                 into = c('date.last', 'ignore')) %>%
        dplyr::select(-ignore) %>%
        mutate(date.last = date.last %>% ymd %>% as.Date)

      names_numeric_cols <-
        last_meeting_data %>%
        dplyr::select(
          c(
            id.home_team.last,
            id.away_team.last,
            pts.home_team.last,
            pts.away_team.last
          )
        ) %>%
        names

      last_meeting_data %<>%
        mutate_each_(funs(as.numeric), vars = names_numeric_cols) %>%
        dplyr::select(
          date.last,
          slug.home_team.last,
          slug.away_team.last,
          pts.home_team.last,
          pts.away_team.last,
          everything()
        )

    }
  } else {
    last_meeting_data <-
      data_frame()
  }

  if ('SeasonSeries' %in% table_names) {
    if (json_data$resultSets$rowSet[8] %>% data.frame %>% nrow > 0) {
      series_standing_data <-
        json_data$resultSets$rowSet[8] %>%
        nba_json_to_df

      names(series_standing_data) <-
        c(
          "id.game",
          "id.home_team",
          "id.away_team",
          "game_date_est",
          "wins.home_team",
          "losses.home_team",
          "city.series_leader"
        )

      series_standing_data %<>%
        dplyr::select(-game_date_est)

      names_numeric_cols <-
        series_standing_data %>%
        dplyr::select(-c(city.series_leader, id.game)) %>%
        names

      series_standing_data %<>%
        mutate_each_(funs(as.numeric), vars = names_numeric_cols)
    }
  } else {
    series_standing_data <-
      data_frame()
  }

  if ('SeasonSeries' %in% table_names) {
    if (json_data$resultSets$rowSet[9] %>% data.frame %>% nrow > 0) {
      video_data <-
        json_data$resultSets$rowSet[9] %>%
        nba_json_to_df

      names(video_data) <-
        c('id.game', 'is.video_available', 'is.player_tracking_available',
          'is.movement_available', 'status.wh')

      video_data %<>%
        mutate_each(funs(. %>% str_detect("1")), starts_with('is.'))
    }
  } else {
    video_data <-
      data_frame()
  }
