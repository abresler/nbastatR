packages <- #need all of these installed including some from github
  c('dplyr',
    'magrittr',
    'jsonlite',
    'tidyr',
    'purrr',
    'stringr',
    'lubridate',
    'tidyr')
options(warn = -1)
lapply(packages, library, character.only = T)


#' get headers
#'
#' @return
#' @import dplyr
#'
#' @examples
get_headers <- function() {
  headers_df <-
    data_frame(
      name.nba = c(
        "PLAYER_ID",
        "SEASON_ID",
        "LEAGUE_ID",
        "TEAM_ID",
        "TEAM_ABBREVIATION",
        "PLAYER_AGE",
        "GP",
        "GS",
        "MIN",
        "FGM",
        "FGA",
        "FG_PCT",
        "FG3M",
        "FG3A",
        "FG3_PCT",
        "FTM",
        "FTA",
        "FT_PCT",
        "OREB",
        "DREB",
        "REB",
        "AST",
        "STL",
        "BLK",
        "TOV",
        "PF",
        "PTS",
        "ORGANIZATION_ID",
        "SCHOOL_NAME",
        "RANK_MIN",
        "RANK_FGM",
        "RANK_FGA",
        "RANK_FG_PCT",
        "RANK_FG3M",
        "RANK_FG3A",
        "RANK_FG3_PCT",
        "RANK_FTM",
        "RANK_FTA",
        "RANK_FT_PCT",
        "RANK_OREB",
        "RANK_DREB",
        "RANK_REB",
        "RANK_AST",
        "RANK_STL",
        "RANK_BLK",
        "RANK_TOV",
        "RANK_PTS",
        "RANK_EFF",
        "PLUS_MINUS",
        "WL",
        "MATCHUP",
        "VIDEO_AVAILABLE",
        "GAME_DATE",
        "Game_ID",
        "PERSON_ID",
        "FIRST_NAME",
        "LAST_NAME",
        "DISPLAY_FIRST_LAST",
        "DISPLAY_LAST_COMMA_FIRST",
        "DISPLAY_FI_LAST",
        "BIRTHDATE",
        "SCHOOL",
        "COUNTRY",
        "LAST_AFFILIATION",
        "HEIGHT",
        "WEIGHT",
        "SEASON_EXP",
        "JERSEY",
        "POSITION",
        "ROSTERSTATUS",
        "TEAM_NAME",
        "TEAM_CODE",
        "TEAM_CITY",
        "PLAYERCODE",
        "FROM_YEAR",
        "TO_YEAR",
        "DLEAGUE_FLAG",
        "GAMES_PLAYED_FLAG",
        "PLAYER_NAME",
        "TimeFrame",
        "PIE",
        "AGE",
        "W",
        "L",
        "W_PCT",
        "BLKA",
        "PFD",
        "DD2",
        "TD3",
        "CFID",
        "CFPARAMS",
        "OFF_RATING",
        "DEF_RATING",
        "NET_RATING",
        "AST_PCT",
        "AST_TO",
        "AST_RATIO",
        "OREB_PCT",
        "DREB_PCT",
        "REB_PCT",
        "TM_TOV_PCT",
        "EFG_PCT",
        "TS_PCT",
        "USG_PCT",
        "PACE",
        "FGM_PG",
        "FGA_PG",
        "PTS_OFF_TOV",
        "PTS_2ND_CHANCE",
        "PTS_FB",
        "PTS_PAINT",
        "OPP_PTS_OFF_TOV",
        "OPP_PTS_2ND_CHANCE",
        "OPP_PTS_FB",
        "OPP_PTS_PAINT",
        "PCT_FGA_2PT",
        "PCT_FGA_3PT",
        "PCT_PTS_2PT",
        "PCT_PTS_2PT_MR",
        "PCT_PTS_3PT",
        "PCT_PTS_FB",
        "PCT_PTS_FT",
        "PCT_PTS_OFF_TOV",
        "PCT_PTS_PAINT",
        "PCT_AST_2PM",
        "PCT_UAST_2PM",
        "PCT_AST_3PM",
        "PCT_UAST_3PM",
        "PCT_AST_FGM",
        "PCT_UAST_FGM",
        "PCT_FGM",
        "PCT_FGA",
        "PCT_FG3M",
        "PCT_FG3A",
        "PCT_FTM",
        "PCT_FTA",
        "PCT_OREB",
        "PCT_DREB",
        "PCT_REB",
        "PCT_AST",
        "PCT_TOV",
        "PCT_STL",
        "PCT_BLK",
        "PCT_BLKA",
        "PCT_PF",
        "PCT_PFD",
        "PCT_PTS",
        "season",
        "height_wo_shoes",
        "height_wo_shoes_ft_in",
        "height_w_shoes",
        "height_w_shoes_ft_in",
        "wingspan",
        "wingspan_ft_in",
        "standing_reach",
        "standing_reach_ft_in",
        "body_fat_pct",
        "hand_length",
        "hand_width",
        "standing_vertical_leap",
        "max_vertical_leap",
        "lane_agility_time",
        "modified_lane_agility_time",
        "three_quarter_sprint",
        "bench_press",
        "spot_fifteen_corner_left",
        "spot_fifteen_break_left",
        "spot_fifteen_top_key",
        "spot_fifteen_break_right",
        "spot_fifteen_corner_right",
        "spot_college_corner_left",
        "spot_college_break_left",
        "spot_college_top_key",
        "spot_college_break_right",
        "spot_college_corner_right",
        "spot_nba_corner_left",
        "spot_nba_break_left",
        "spot_nba_top_key",
        "spot_nba_break_right",
        "spot_nba_corner_right",
        "off_drib_fifteen_break_left",
        "off_drib_fifteen_top_key",
        "off_drib_fifteen_break_right",
        "off_drib_college_break_left",
        "off_drib_college_top_key",
        "off_drib_college_break_right",
        "on_move_fifteen",
        "on_move_college"
      ),
      name.actual = c(
        "id.player",
        "code.season",
        "id.league",
        "id.team",
        "slug.team",
        "age.player",
        "gp",
        "gs",
        "min",
        "fgm",
        "fga",
        "pct.fg",
        "fg3m",
        "fg3a",
        "pct.fg3",
        "ftm",
        "fta",
        "pct.ft",
        "oreb",
        "dreb",
        "reb",
        "ast",
        "stl",
        "blk",
        "tov",
        "fouls",
        "pts",
        "id.organization",
        "name.school",
        "rank.min",
        "rank.fgm",
        "rank.fga",
        "rank.pct.fg",
        "rank.fg3m",
        "rank.fg3a",
        "rank.pct.fg3",
        "rank.ftm",
        "rank.fta",
        "rank.pct.ft",
        "rank.oreb",
        "rank.dreb",
        "rank_reb",
        "rank.ast",
        "rank.stl",
        "rank.blk",
        "rank.tov",
        "rank.pts",
        "rank.eff",
        "plus.minus",
        "wl",
        "matchup",
        "is.video_available",
        "date.game",
        "id.game",
        "id.player",
        "name.first",
        "name.last",
        "name.player",
        "name.last.display",
        "name.middle.display",
        "date.birth",
        "school",
        "country",
        "college.non_nba_team",
        "height",
        "weight.lbs",
        "years.experience",
        "jersey",
        "position",
        "status.roster",
        "team",
        "code.team",
        "city.team",
        "slug.player",
        "year.from",
        "year.to",
        "has.d_league_data",
        "gp.flag",
        "name.player",
        "id.season",
        "pie",
        "age",
        "wins",
        "losses",
        "pct.wins",
        "fga.blocked",
        "fouls.drawn",
        "double_doubles",
        "triple_doubles",
        "cfid",
        "cfparms",
        "ortg",
        "drtg",
        "netrtg",
        "pct.ast",
        "ratio.ast.to",
        "ratio.ast",
        "pct.oreb",
        "pct.dreb",
        "pct.reb",
        "ratio.to",
        "pct.efg",
        "pct.ts",
        "pct.usg",
        "pace",
        "fgm.per_game",
        "fga.per_game",
        "pts.off_to",
        "pts.2nd_chance",
        "pts.fastbreak",
        "pts.paint",
        "pts.off_to.opponent",
        "pts.2nd_chance.opponent",
        "pts.fastbreak.opponent",
        "pts.paint.opponent",
        "pct.fga2a",
        "pct.fga3a",
        "pct.pts.fg2m",
        "pct.pts.mid_range_2",
        "pct.pts.fg3m",
        "pct.pts.fast_break",
        "pct.pts.ft",
        "pct.pts.off_tos",
        "pct.paints.paint",
        "pct.fg2m.assisted",
        "pct.fg2m.unassisted",
        "pct.fg3m.assisted",
        "pct.fg3m.unassisted",
        "pct.fgm.assisted",
        "pct.fgm.unassisted",
        "pct.fgm",
        "pct.fga",
        "pct.fg3m",
        "pct.fg3a",
        "pct.ftm",
        "pct.fta",
        "pct.oreb",
        "pct.dreb",
        "pct.reb",
        "pct.ast",
        "pct.tov",
        "pct.stl",
        "pct.blk",
        "pct.blocked",
        "pct.fouls",
        "pct.fouls.drawn",
        "pct.pts",
        "year.combine",
        "height.wo_shoes.in",
        "height.wo_shoes.ft.in",
        "height.wt_shoes.in",
        "height.wt_shoes.ft.in",
        "wingspan.in",
        "wingspan.ft.in",
        "standing_reach.in",
        "standing_reach.ft.in",
        "pct.body_fat",
        "length.hand.in",
        "width.hand.in",
        "standing_vertical_leap.in",
        "max_vertical_leap.in",
        "lane_agility_time.sec",
        "modified_lane_agility_time.sec",
        "three_quarter_sprint.sec",
        "bench_press.185.reps",
        "spot_fifteen_corner_left.shots",
        "spot_fifteen_break_left.shots",
        "spot_fifteen_top_key.shots",
        "spot_fifteen_break_right.shots",
        "spot_fifteen_corner_right.shots",
        "spot_college_corner_left.shots",
        "spot_college_break_left.shots",
        "spot_college_top_key.shots",
        "spot_college_break_right.shots",
        "spot_college_corner_right.shots",
        "spot_nba_corner_left.shots",
        "spot_nba_break_left.shots",
        "spot_nba_top_key.shots",
        "spot_nba_break_right.shots",
        "spot_nba_corner_right.shots",
        "off_drib_fifteen_break_left.shots",
        "off_drib_fifteen_top_key.shots",
        "off_drib_fifteen_break_right.shots",
        "off_drib_college_break_left.shots",
        "off_drib_college_top_key.shots",
        "off_drib_college_break_right.shots",
        "on_move_fifteen.shots",
        "on_move_college.shots"
      ),
      id.row = 1:length(name.actual)
    )
  return(headers_df)
}

#' get shot pct
#'
#' @param x
#'
#' @return
#' @import stringr
#'
#' @examples
get_shot_pct <- function(x) {
  shots <-
    x %>%
    str_split('\\-') %>%
    unlist %>%
    as.numeric()

  shot.pct <-
    shots[1] / shots[2]

  return(shot.pct)

}

#' Get Year Draft Combines
#'
#' @param combine_year
#' @param return_message
#'
#' @return
#' @import dplyr stringr magrittr jsonlite purrr tidyr lubridate
#' @export
#' @examples
get_year_draft_combine <-
  function(combine_year = 2014,
           return_message = T) {
    if (combine_year < 2000) {
      stopifnot("Sorry data starts in the 2000-2001 season")
    }
    id.season <-
      combine_year %>%
      paste0("-", (combine_year + 1) %>% substr(3, 4))

    base <-
      'http://stats.nba.com/stats/draftcombinestats?LeagueID=00&SeasonYear='
    url <-
      base %>%
      paste0(id.season)

    json_data <-
      url %>%
      fromJSON(simplifyDataFrame = T)

    tbl_data <-
      json_data$resultSets$rowSet %>%
      data.frame(stringsAsFactors = F) %>%
      tbl_df

    headers <-
      json_data$resultSets$headers %>% unlist %>%
      str_to_lower()

    headers_df <-
      get_headers() %>%
      mutate(name.nba = name.nba %>% str_to_lower()) %>%
      distinct()

    actual_names <-
      1:length(headers) %>%
      purrr::map(
        function(x)
          data_frame(
            name.actual =
              headers_df %>%
              mutate(name.nba = name.nba) %>%
              dplyr::filter(name.nba == headers[x]) %>%
              .$name.actual
          )
      ) %>%
      bind_rows()

    names(tbl_data) <-
      actual_names$name.actual

    numeric_cols <-
      tbl_data %>%
      dplyr::select(-c(id.player, name.first, name.last)) %>%
      dplyr::select(
        -c(
          name.player,
          position,
          height.wo_shoes.ft.in,
          height.wt_shoes.ft.in,
          wingspan.ft.in,
          standing_reach.ft.in
        )
      ) %>%
      dplyr::select(-matches(".shots")) %>%
      names()
    combine_data <-
      tbl_data %>%
      dplyr::select(-c(id.player, name.first, name.last)) %>%
      mutate_each_(funs(as.numeric(.)),
                   vars = numeric_cols) %>%
      mutate(
        name.player = name.player %>% gsub('\\?', "'", .),
        name.player = name.player %>% str_replace_all('\\.', ''),
        pct.body_fat = pct.body_fat / 100
      )
    if ('off_drib_college_break_right.shots' %in% names(combine_data))
      combine_data <-
      combine_data %>%
      mutate_each_(
        funs(. %>% lapply(get_shot_pct) %>% unlist %>% as.character),
        vars =
          combine_data %>%
          dplyr::select(contains(".shot")) %>%
          names()
      )
    if (return_message == T) {
      "You got draft combine data for the " %>%
        paste0(combine_year, " draft combine")
    }
    return(combine_data)
  }
#' get years draft combines
#'
#' @param combine_years
#'
#' @return
#' @export
#'
#' @examples
get_all_draft_combines <- function(combine_years = 2000:2016) {
  all_draft_combines <-
    combine_years %>%
    purrr::map(function(x)
      get_year_draft_combine(combine_year =  x,
                             return_message = T)) %>%
    compact %>%
    bind_rows

  all_draft_combines %<>%
    mutate_each_(funs(as.numeric(.)),
                 vars =
                   all_draft_combines %>%
                   dplyr::select(contains(".shots")) %>% names)

  return(all_draft_combines)

}
