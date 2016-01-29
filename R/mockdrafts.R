function_packages <- c(
  c(
    "dplyr",
    "magrittr",
    "jsonlite",
    "xml2",
    "tidyr",
    "httr",
    "rvest",
    "stringr",
    "purrr",
    "data.table",
    "lubridate"
  )
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

get_fd_name_df <- function() {
  fd_nba_name_df <-
    data_frame(
      name.fanduel = c(
        "Louis Amundson",
        "Ishmael Smith",
        "C.J. Wilcox",
        "Glenn Robinson III",
        "Joseph Young",
        "Luc Richard Mbah a Moute",
        "T.J. Warren",
        "Nene Hilario",
        "P.J. Tucker",
        "J.J. Redick",
        "C.J. Miles",
        "C.J. McCollum",
        "Brad Beal",
        "Roy Devyn Marble",
        "K.J. McDaniels",
        "C.J. Watson",
        "J.J. Hickson",
        "Jose Juan Barea"
      ),
      name.nba =  c(
        "Lou Amundson",
        "Ish Smith",
        "CJ Wilcox",
        "Glenn Robinson",
        "Joe Young",
        "Luc Mbah a Moute",
        "TJ Warren",
        "Nene",
        "PJ Tucker",
        "JJ Redick",
        "CJ Miles",
        "CJ McCollum",
        "Bradley Beal",
        "Devyn Marble",
        "KJ McDaniels",
        "CJ Watson",
        "JJ Hickson",
        "Jose Juan Barea"
      ),
      is.different_name = T
    )
  return(fd_nba_name_df)
}

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
        "OPP_PTS"
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
        "rep.opp",
        "ast.opp",
        "tov.opp",
        "stl.opp",
        "blk.opp",
        "fga.blocked.opp",
        "fouls.opp",
        "fouls.drawn.opp",
        "pts.opp"
      ),
      id.row = 1:length(name.actual)
    )
  return(headers_df)
}

get_nba_players_ids <- function(active_only = F) {
  players.url <-
    "http://stats.nba.com/stats/commonallplayers?IsOnlyCurrentSeason=0&LeagueID=00&Season=2015-16"

  json_data <-
    players.url %>%
    jsonlite::fromJSON(simplifyDataFrame = T)

  data <-
    json_data$resultSets$rowSet %>%
    data.frame(stringsAsFactors = F) %>%
    tbl_df

  headers <-
    json_data$resultSets$headers %>%
    unlist %>%
    str_to_lower()

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
    bind_rows()

  names(data) <-
    actual_names$name.actual

  names_df <-
    data$name.last.display %>%
    str_split_fixed(pattern = '\\,', 2) %>%
    data.frame() %>%
    tbl_df

  names(names_df) <-
    c('name.last', 'name.first')

  names_df %<>%
    mutate(player = name.first %>% str_trim %>% paste(name.last %>% str_trim)) %>%
    dplyr::select(player)

  data$name.player <-
    names_df$player
  data %<>%
    mutate(
      id.player = id.player %>% as.numeric,
      is.active_player = ifelse(id.team == 0, FALSE, TRUE),
      id.team = id.team %>% as.numeric
    ) %>%
    dplyr::select(-c(status.roster, name.last.display)) %>%
    mutate_each(funs(extract_numeric), starts_with("year.")) %>%
    mutate(
      id.team = ifelse(id.team == 0, NA, id.team),
      city.team = ifelse(city.team == '', NA, city.team),
      code.team = ifelse(code.team == '', NA, code.team),
      slug.team = ifelse(slug.team == '', NA, slug.team),
      team = ifelse(city.team %>% is.na, NA, paste(city.team, team)),
      seasons.played = year.to - year.from,
      url.player = id.player %>% paste0('http://stats.nba.com/player/#!/', .),
      image.player = id.player %>% paste0('http://stats.nba.com/media/players/132x132/', ., '.png')
    ) %>%
    dplyr::select(
      name.player,
      id.player,
      team,
      id.team,
      is.active_player,
      seasons.played,
      year.from,
      year.to,
      everything()
    )

  if (active_only == T) {
    data %<>%
      dplyr::filter(is.active_player == T)
  }

  return(data)
}

get_nba_player_resolver_df <- function() {
  player_df <-
    data_frame(
      name.player = c(
        "BJ Mullens",
        "Darius Johnson-O...",
        "E'twaun Moore",
        "Giannis Adetokoubo",
        "Glenn Robinson III",
        "Guillermo Hernan...",
        "JaJuan Johnson P...",
        "James McAdoo",
        "Jeff Pendergraph",
        "Jeffery Taylor",
        "Joseph Young",
        "Kentavious Caldw...",
        "Kostas Papanikol...",
        "Marshon Brooks",
        "Matthew Bryan-Am...",
        "Michael Carter-W...",
        "Michael Kidd-Gil...",
        "Moe Harkless",
        "Patrick Mills",
        "Ricardo Ledo",
        "Rondae Hollis-Je...",
        "Thanasis Antetok...",
        "Tim Hardaway Jr",
        "Vyacheslav Kravt...",
        "Willie Cauley-St..."
      ),
      name.player.nba =  c(
        "Byron Mullens",
        "Darius Johnson-Odom",
        "E'Twaun Moore",
        "Giannis Antetokounmpo",
        "Glenn Robinson",
        "Guillermo Hernangomez",
        "JaJuan Johnson",
        "James Michael McAdoo",
        "Jeff Ayres",
        "Jeff Taylor",
        "Joe Young",
        "Kentavious Caldwell-Pope",
        "Kostas Papanikolaou",
        "MarShon Brooks",
        "Matthew Bryan-Amaning",
        "Michael Carter-Williams",
        "Michael Kidd-Gilchrist",
        "Maurice Harkless",
        "Patty Mills",
        "Ricky Ledo",
        "Rondae Hollis-Jefferson",
        "Thanasis Antetokounmpo",
        "Tim Hardaway Jr.",
        "Vyacheslav Kravtsov",
        "Willie Cauley-Stein"
      )
    )
  return(player_df)
}

get_school_df  <- function() {
  school_df <-
    data_frame(
      school = c(
        "Bosnia & Herz...",
        "Long Beach St...",
        "Louisiana Laf...",
        "Rep. of Georgia",
        "South Dakota ...",
        "Washington St...",
        "Wisconsin Gre..."
      ),
      school.actual = c(
        "Bosnia",
        "Long Beach State",
        "Louisiana Lafayette",
        "Republic of Georgia",
        "South Dakota State",
        "Washington State",
        "Wisconsin Green Bay"
      )
    )
  return(school_df)
}

get_nba_draft_net_team_df <- function() {
  team_df <-
    data_frame(
      name.team.nbadraft_net = c(
        "Atlanta",
        "Boston",
        "Brooklyn",
        "Charlotte",
        "Chicago",
        "Cleveland",
        "Dallas",
        "Denver",
        "Detroit",
        "Golden St.",
        "Houston",
        "Indiana",
        "LA Clippers",
        "LA Lakers",
        "Memphis",
        "Miami",
        "Milwaukee",
        "Minnesota",
        "New Jersey",
        "New Orleans",
        "New Orleans Old",
        "New York",
        "Oklahoma Cty",
        "Orlando",
        "Philadelphia",
        "Phoenix",
        "Portland",
        "Sacramento",
        "San Antonio",
        "Toronto",
        "Utah",
        "Washington",
        'Charlotte Old'
      ),
      team = c(
        "Atlanta Hawks",
        "Boston Celtics",
        "Brooklyn Nets",
        "Charlotte Hornets",
        "Chicago Bulls",
        "Cleveland Cavaliers",
        "Dallas Mavericks",
        "Denver Nuggets",
        "Detroit Pistons",
        "Golden State Warriors",
        "Houston Rockets",
        "Indiana Pacers",
        "Los Angeles Clippers",
        "Los Angeles Lakers",
        "Memphis Grizzlies",
        "Miami Heat",
        "Milwaukee Bucks",
        "Minnesota Timberwolves",
        "New Jersey Nets",
        "New Orleans Pelicans",
        "New Orleans Hornets",
        "New York Knicks",
        "Oklahoma City Thunder",
        "Orlando Magic",
        "Philadelphia 76ers",
        "Phoenix Suns",
        "Portland Trail Blazers",
        "Sacramento Kings",
        "San Antonio Spurs",
        "Toronto Raptors",
        "Utah Jazz",
        "Washington Wizards",
        "Charlotte Bobcats"
      ),
      team.current = c(
        "Atlanta Hawks",
        "Boston Celtics",
        "Brooklyn Nets",
        "Charlotte Hornets",
        "Chicago Bulls",
        "Cleveland Cavaliers",
        "Dallas Mavericks",
        "Denver Nuggets",
        "Detroit Pistons",
        "Golden State Warriors",
        "Houston Rockets",
        "Indiana Pacers",
        "Los Angeles Clippers",
        "Los Angeles Lakers",
        "Memphis Grizzlies",
        "Miami Heat",
        "Milwaukee Bucks",
        "Minnesota Timberwolves",
        "New Jersey Nets",
        "New Orleans Pelicans",
        "New Orleans Hornets",
        "New York Knicks",
        "Oklahoma City Thunder",
        "Orlando Magic",
        "Philadelphia 76ers",
        "Phoenix Suns",
        "Portland Trail Blazers",
        "Sacramento Kings",
        "San Antonio Spurs",
        "Toronto Raptors",
        "Utah Jazz",
        "Washington Wizards",
        "Charlotte Hornets"
      ),
      slug.team = c(
        "ATL",
        "BOS",
        "BKN",
        "CHA",
        "CHI",
        "CLE",
        "DAL",
        "DEN",
        "DET",
        "GSW",
        "HOU",
        "IND",
        "LAC",
        "LAL",
        "MEM",
        "MIA",
        "MIL",
        "MIN",
        "NJN",
        "NOP",
        "NOH",
        "NYK",
        "OKC",
        "ORL",
        "PHI",
        "PHO",
        "POR",
        "SAC",
        "SAS",
        "TOR",
        "UTA",
        "WAS",
        'CHA'
      ),
      slug.team.current = c(
        "ATL",
        "BOS",
        "BKN",
        "CHA",
        "CHI",
        "CLE",
        "DAL",
        "DEN",
        "DET",
        "GSW",
        "HOU",
        "IND",
        "LAC",
        "LAL",
        "MEM",
        "MIA",
        "MIL",
        "MIN",
        "BKN",
        "NOP",
        "NOP",
        "NYK",
        "OKC",
        "ORL",
        "PHI",
        "PHO",
        "POR",
        "SAC",
        "SAS",
        "TOR",
        "UTA",
        "WAS",
        'CHA'
      )
    )

  return(team_df)
}

height_in_inches <-
  function(height) {
    height_ft_in <-
      height %>%
      str_split("-") %>%
      unlist %>%
      as.numeric()
    height_in <-
      height_ft_in[1] * 12 + height_ft_in[2]
    return(height_in)
  }


get_nba_draftnet_year_mock_draft <-
  function(draft_year = 2013,
           return_message = T) {
    load_needed_packages(function_packages)
    install_needed_packages(function_packages)
    if (!'draft_year' %>% exists()) {
      stop('Please enter a draft year')
    }
    year_now <-
      Sys.Date() %>%
      year

    month_now <-
      Sys.Date() %>% month()

    if (month_now %in% c(1:6)) {
      max_draft_year <-
        draft_year + 1
    } else {
      max_draft_year <-
        draft_year + 2
    }

    if (draft_year > max_draft_year) {
      stop("Sorry mock drafts only go through " %>% paste0(max_draft_year))
    }

    if (draft_year < 2009) {
      stop("Sorry mock drafts only go from 2009")
    }
    class_df <-
      data_frame(
        slug.class = c("fr", 'so', 'jr', 'sr', "intl"),
        id.class = c(1:4, NA)
      )
    url <-
      'http://www.nbadraft.net/' %>%
      paste0(draft_year, 'mock_draft')

    page <-
      url %>%
      read_html()

    last_update <-
      page %>%
      html_nodes('.updated') %>%
      html_text() %>%
      str_replace('Updated: ', '') %>%
      str_split('\\ ') %>%
      unlist %>%
      .[1] %>%
      as.Date('%m/%d/%y')

    name.team.nbadraft_net <-
      page %>%
      html_nodes('strong') %>%
      html_text() %>%
      str_trim()

    name.player <-
      page %>%
      html_nodes('td:nth-child(3)') %>%
      html_text() %>%
      str_trim()

    url.player.nbadraft_net <-
      page %>%
      html_nodes('td:nth-child(3) a') %>%
      html_attr('href') %>%
      paste0('http://www.nbadraft.net/players', .)

    height <-
      page %>%
      html_nodes('td:nth-child(4)') %>%
      html_text()

    weight.lbs <-
      page %>%
      html_nodes('td:nth-child(5)') %>%
      html_text() %>%
      extract_numeric()

    id.position <-
      page %>%
      html_nodes('td:nth-child(6)') %>%
      html_text()

    school <-
      page %>%
      html_nodes('td:nth-child(7)') %>%
      html_text()

    slug.class <-
      page %>%
      html_nodes('td:nth-child(8)') %>%
      html_text() %>%
      str_replace('\\.', '') %>%
      str_to_lower()

    team_df <-
      get_nba_draft_net_team_df()

    player_df <-
      get_nba_player_resolver_df()
    school_df <-
      get_school_df()

    draft_data <-
      data_frame(
        date.data.updated = last_update,
        year.draft = draft_year,
        id.pick = 1:length(name.player),
        name.team.nbadraft_net,
        name.player,
        height,
        weight.lbs,
        id.position,
        school,
        slug.class,
        url.player.nbadraft_net
      ) %>%
      left_join(player_df) %>%
      left_join(school_df) %>%
      mutate(
        name.player = ifelse(!name.player.nba %>% is.na, name.player.nba, name.player),
        height.inches = height %>% lapply(height_in_inches) %>% unlist,
        id.round = ifelse(id.pick <= 30, 1, 2),
        is.traded_pick = name.team.nbadraft_net %>% str_detect('\\*'),
        name.team.nbadraft_net = name.team.nbadraft_net %>% str_replace('\\*', ''),
        school = ifelse(!school.actual %>% is.na, school.actual, school),
        is.combo_player = id.position %>% str_detect('\\/')
      ) %>%
      separate(
        id.position,
        into = c('id.position', 'id.position.secondary'),
        sep = '\\/'
      )
    if (draft_year <= 2013) {
      draft_data$name.team.nbadraft_net %<>%
        str_replace('Charlotte', "Charlotte Old")
    }
    draft_data %<>%
      left_join(team_df) %>%
      left_join(class_df) %>%
      dplyr::select(-c(
        name.team.nbadraft_net,
        school.actual,
        name.player.nba,
        team.current
      )) %>%
      dplyr::select(
        date.data.updated,
        year.draft,
        id.round,
        id.pick,
        name.player,
        id.position,
        id.position.secondary,
        team,
        slug.team,
        slug.team.current,
        is.traded_pick,
        height,
        height.inches,
        weight.lbs,
        is.combo_player,
        everything()
      ) %>%
      suppressMessages()

    if (return_message == T) {
      "You the got nbadraft.net mock draft for the " %>%
        paste0(draft_year, ' NBA Draft\ndata last updated on ', last_update) %>%
        message()
    }
    return(draft_data)
  }


get_nba_draftnet_years_mock_draft <-
  function(draft_years = 2009:2016,
           merge_nba_data = T,
           message = T) {
    all_draft_data <-
      draft_years %>%
      map(
        function(x)
          get_nba_draftnet_year_mock_draft(draft_year = x, return_message = message)
      ) %>%
      compact %>%
      bind_rows()
    if (merge_nba_data == T) {
      nba_players <-
        get_nba_players_ids()

      all_draft_data %<>%
        left_join(
          nba_players %>%
            group_by(name.player) %>%
            dplyr::filter(year.from == max(year.from)) %>%
            ungroup %>%
            dplyr::select(
              name.player,
              is.active_player,
              team.player.current = team,
              id.player
            )
        ) %>%
        mutate(is.nba_player = ifelse(!id.player %>% is.na(), T, F))
    }
    return(all_draft_data)
  }
