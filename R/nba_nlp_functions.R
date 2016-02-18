function_packages <-
  c(
    'dplyr',
    'magrittr',
    'jsonlite',
    'tidyr',
    'stringr',
    'lubridate',
    'readr',
    'purrr',
    'stringr',
    'tidyr'
  )

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

get_headers <- function() {
  function_packages <-
    c("dplyr", "magrittr", "tidyr", "purrr", "stringr")
  load_needed_packages(function_packages)

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
        "FTA_RATE",
        "OPP_EFG_PCT",
        "OPP_FTA_RATE",
        "OPP_TOV_PCT",
        "OPP_OREB_PCT",
        "OPP_FGM",
        "OPP_FGA",
        "OPP_FG_PCT",
        "OPP_FG3M",
        "OPP_FG3A",
        "OPP_FG3_PCT",
        "OPP_FTM",
        "OPP_FTA",
        "OPP_FT_PCT",
        "OPP_OREB",
        "OPP_DREB",
        "OPP_REB",
        "OPP_AST",
        "OPP_TOV",
        "OPP_STL",
        "OPP_BLK",
        "OPP_BLKA",
        "OPP_PF",
        "OPP_PFD",
        "OPP_PTS",
        "GROUP_SET",
        "GROUP_VALUE",
        "TEAM_GAME_LOCATION",
        "GAME_RESULT",
        'SEASON_MONTH_NAME',
        'SEASON_SEGMENT',
        'TEAM_DAYS_REST_RANGE',
        'SCHOOL_TYPE'
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
        "rate.fta",
        "pct.efg.opp",
        "rate.fta.opp",
        "pct.tov.opp",
        "pct.oreb.opp",
        "fgm.opp",
        "fga.opp",
        "pct.fg.opp",
        "fg3m.opp",
        "fg3a.opp",
        "pct.fg3.opp",
        "ftm.opp",
        "fta.opp",
        "pct.ft.opp",
        "oreb.opp",
        "dreb.opp",
        "reb.opp",
        "ast.opp",
        "tov.opp",
        "stl.opp",
        "blk.opp",
        "fga.blocked.opp",
        "fouls.opp",
        "fouls.drawn.opp",
        "pts.opp",
        "group.name",
        "group.value",
        "location",
        'outcome',
        'name.month',
        'season.segment',
        'days.rest',
        'school.type'
      ),
      id.row = 1:length(name.actual)
    )
  return(headers_df)
}

ask_nba_api_nlp_question <-
  function(question = "HOW MANY POINTS DOES CARMELO SCORE IN THE 2ND HALF?",
           return_similar_questions = T,
           return_message = T,
           resolve_headers = F) {
    question_parsed <-
      question %>%
      str_replace_all('\\ ', '\\%20') %>%
      str_replace_all('\\"', '\\%22') %>%
      str_replace_all("\\'", '\\%27') %>%
      str_replace_all("\\-", '%2D') %>%
      str_replace_all("\\+", '%2B') %>%
      str_replace_all("\\?", '%3F')

    base <-
      'http://stats.nba.com/stats/textanalysis/?LeagueID=00&Question='

    json_url <-
      base %>%
      paste0(question_parsed)

    json_data <-
      json_url %>%
      fromJSON(flatten = T, simplifyDataFrame = T)

    answer_df <-
      json_data$resultSets[[1]]$rowSet %>%
      data.frame %>%
      tbl_df

    if(answer_df %>% nrow == 0){
      stop("Sorry " %>% paste0(question, " is not valid question"), call. = F)
    }

    headers <-
      json_data$resultSets[[1]]$headers %>% unlist %>%
      str_replace_all('\\ ', '\\') %>%
      str_replace_all('\\%', '\\') %>%
      str_replace_all('__','_') %>%
      str_to_lower()

    names(answer_df) <-
      headers

    if (resolve_headers == T) {
      headers_df <-
        get_headers()

      actual_names <-
        1:length(headers) %>%
        purrr::map(
          function(x)
            data_frame(
              name.actual =
                headers_df %>%
                mutate(name.nba = name.nba %>% str_to_lower) %>%
                dplyr::filter(name.nba == headers[x]) %>%
                .$name.actual
            )
        ) %>%
        compact %>%
        bind_rows()
      names(answer_df) <-
        actual_names$name.actual
    }

    answer_df %<>%
      mutate(question) %>%
      dplyr::select(question, everything())

    answer_df %<>%
      mutate_each_(funs(parse_guess),
                   names(answer_df)
      )


    if (return_similar_questions == T) {
      "You answered this question:\n" %>%
        paste0(question) %>%
        message()
    }

    if (return_similar_questions == T) {
      "NBA recommends these other questions:\n" %>%
        paste0(json_data$resultSets[[3]] %>% .$rowSet %>% .[, 1] %>%
                 paste0(collapse = '\n')) %>%
        message()
    }

    return(answer_df)

  }
ask_nba_api_nlp_question_safe <-
  failwith(NULL, ask_nba_api_nlp_question)

ask_nba_api_nlp_questions <- function(
  questions = c(
    "Wayne Ellington catch and shoot field goals",
    "Joe Johnson catch and shoot field goals"
  ),
  similar = T,
  message = T,
  headers = F
) {
 all_questions <-
  questions %>%
    map(function(x) ask_nba_api_nlp_question_safe(question = x,
                                             return_similar_questions = similar,
                                             return_message = message,
                                             resolve_headers = headers)
      ) %>%
    compact %>%
    bind_rows()
 return(all_questions)
}
