options(stringsAsFactors = F)
function_packages <-
  c('dplyr',
    'magrittr',
    'data.table',
    'jsonlite',
    'tidyr',
    'stringr',
    'lubridate',
    'stringr',
    'tidyr')

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
load_needed_packages(function_packages)
install_needed_packages(function_packages)
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

get_nba_franchise_data <-
  function(return_franchises = c('all', 'active', 'current'),
           return_message = T) {
    install_needed_packages(function_packages)
    load_needed_packages(function_packages)
    
    franchise <- 
    	return_franchises[1]
 
    
    team_history_url <-
      'http://stats.nba.com/stats/franchisehistory?LeagueID=00'

    team_data <-
      team_history_url %>%
      fromJSON(simplifyDataFrame = T, flatten = T)

    names_active <-
      team_data$resultSets$headers[1] %>%
      unlist %>%
      str_to_lower

    names_defunct <-
      team_data$resultSets$headers[2] %>%
      unlist %>%
      str_to_lower

    active_data <-
      team_data$resultSets$rowSet[1] %>%
    	data.frame %>% 
      tbl_df()

    names(active_data) <-
      names_active

    active_data %<>%
      mutate(is.active = T)

    defunct_data <-
      team_data$resultSets$rowSet[2] %>%
    	data.frame %>% 
      tbl_df()

    names(defunct_data) <-
      names_defunct

    defunct_data %<>%
      mutate(is.active = F)

    data <-
      active_data %>%
      dplyr::bind_rows(defunct_data)

    num_cols <-
      data %>%
      dplyr::select(-c(matches("team")), -is.active) %>%
      names

    data %<>%
      mutate_each_(funs(as.numeric), vars = num_cols)

    names(data) <-
      c(
        "id.league",
        "id.team",
        "city.team",
        "name.team",
        "year.start.team",
        "year.end.team",
        "team.seasons",
        "team.games",
        "team.wins",
        "team.losses",
        "pct.wins",
        "team.po_appearances",
        "team.div_titles",
        "team.conf_titles",
        "team.league_titles",
        "is.active"
      )

    data %<>%
      mutate(team = city.team %>% paste(name.team),
             id.team = id.team %>% as.numeric()) %>%
      dplyr::select(-id.league) %>%
      dplyr::select(id.team, team, everything())

    if (franchise == 'current') {
      data %<>%
        mutate(id.row = 1:nrow(.)) %>%
        group_by(id.team) %>%
        dplyr::filter(id.row == min(id.row), is.active == T) %>%
        dplyr::select(-id.row)
    }

    if (franchise == 'active') {
      data %<>%
        dplyr::filter(is.active == T)
    }
    if (return_message == T) {
      "You got NBA franchise data" %>%
        message
    }
    return(data)
  }


get_nba_team_season_roster <- function(team = "Denver Nuggets",
                                       year_season_end = 1992,
                                       return_message = T) {
  install_needed_packages(function_packages)
  load_needed_packages(function_packages)
  if (!'team' %>% exists) {
    stop("Please enter a team")
  }

  year_season_start <-
    year_season_end - 1

  id.season <-
    year_season_start %>%
    paste(year_season_end %>% substr(start = 3, stop = 4),
          sep = "-")

  teams <-
    get_nba_franchise_data(return_franchises = 'all', return_message = F)

  teams_ids <-
    teams %>%
    dplyr::select(id.team, city.team, name.team, team, year.start.team)

  t <-
    team

  team_id <-
    teams_ids %>%
    dplyr::filter(team == t) %>%
    .$id.team %>%
    unique()

  if (team_id %>% length > 1) {
    team_id <-
      team_id[1]
  }

  if (year_season_end - 1 <
      teams %>%
      dplyr::filter(id.team == team_id) %>%
      dplyr::filter(year.start.team == min(year.start.team)) %>%
      .$year.start.team %>% unique()) {
    "Sorry " %>%
      paste0(
        year_season_end,
        ' is not a valid season for the ',
        teams_ids %>%
          dplyr::filter(id.team == team_id) %>%
          .$team
      ) %>%
      message()

  }

  roster_url <-
    'http://stats.nba.com/stats/commonteamroster?LeagueID=00&Season=' %>%
    paste0(id.season, '&TeamID=', team_id)

  json_data <-
    roster_url %>%
    fromJSON(simplifyDataFrame = T, flatten = T)

  names_roster <-
    json_data$resultSets$headers[1] %>%
    unlist %>%
    str_to_lower

  data_roster <-
    json_data$resultSets$rowSet[1] %>%
    data.frame(stringsAsFactors = F) %>%
    tbl_df

  names(data_roster) <-
    names_roster

  data_roster %<>%
    rename(
      id.team = teamid,
      id.player = player_id,
      name.player = player,
      number = num,
      date.birth = birth_date,
      years.experience = exp,
      weight.lbs = weight,
      number.jersey = num
    ) %>%
    dplyr::select(-c(leagueid, season)) %>%
    mutate(
      is.rookie = ifelse(years.experience == "R", T, F),
      years.experience = years.experience %>% str_replace("R", 0) %>% as.numeric(),
      id.team = id.team %>% as.numeric,
      number.jersey = number.jersey %>% as.numeric,
      height.inches = height %>% lapply(height_in_inches) %>% unlist,
      weight.lbs = weight.lbs %>% as.numeric,
      date.birth = date.birth %>% as.Date('%b %d, %Y'),
      id.player = id.player %>% as.numeric,
      id.season,
      season.year_end = year_season_end
    ) %>%
    dplyr::select(id.season,
                  season.year_end,
                  id.player,
                  name.player,
                  everything()) %>%
    separate(
      position,
      sep = '\\-',
      into = c('id.position', 'id.position.secondary')
    ) %>%
    left_join(
      teams_ids %>%
        group_by(id.team) %>%
        dplyr::filter(team == t) %>%
        dplyr::filter(year.start.team == min(year.start.team)) %>%
        ungroup
    ) %>%
    suppressMessages() %>%
    dplyr::select(-c(city.team, name.team, season.year_end, year.start.team)) %>%
    dplyr::select(
      id.season,
      id.team,
      team,
      id.position,
      id.position.secondary,
      is.rookie,
      name.player,
      weight.lbs,
      height.inches,
      everything()
    ) %>%
    suppressWarnings()

  if (return_message == T) {
    "You got the NBA roster for the " %>%
      paste0(t, " for the ", id.season, ' season.') %>%
      message()
  }
  return(data_roster)
}
get_nba_players_ids <-
	function(league = "NBA",
					 active_only = F) {
		function_packages <-
			#need all of these installed including some from github
			c('dplyr',
				'magrittr',
				'jsonlite',
				'tidyr',
				'stringr',
				'purrr',
				'data.table',
				'tidyr')
		install_needed_packages(function_packages)
		load_needed_packages(function_packages)
		if (!'league' %>% exists) {
			stop("Please enter either NBA or NBDL")
		}
		
		if (league == "NBA") {
			id.stem <-
				"00"
		}
		
		if (league == "NBDL") {
			id.stem <-
				"20"
		}
		
		base_url <-
			'http://stats.nba.com/stats/commonallplayers?IsOnlyCurrentSeason=0&LeagueID='
		
		players.url <-
			base_url %>%
			paste0(id.stem, '&Season=2015-16')
		
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
			dplyr::bind_rows()
		
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
				is.on_roster = ifelse(id.team == 0, FALSE, TRUE),
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
			mutate(league) %>%
			dplyr::select(
				league,
				name.player,
				id.player,
				team,
				id.team,
				is.on_roster,
				seasons.played,
				year.from,
				year.to,
				everything()
			)
		
		if ('is.nba_assigned' %in% (names(data))) {
			data %<>%
				mutate(is.nba_assigned = is.nba_assigned %>% as.logical())
		}
		
		if (active_only == T) {
			data %<>%
				dplyr::filter(is.on_roster == T)
		}
		if ('gp.flag' %in% names(data)) {
			data %<>%
				dplyr::select(-gp.flag)
		}
		return(data)
	}

get_headers <- function() {
	install_needed_packages(function_packages)
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
				'is_nba_assigned',
				'nba_assigned_team_id'
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
				"pts.opp",
				'is.nba_assigned',
				'id.team.nba_assigned'
			),
			id.row = 1:length(name.actual)
		)
	return(headers_df)
}

get_nba_team_season_roster_safe <-
  failwith(NULL, get_nba_team_season_roster)

get_player_season_shot_data <- function(player = "Stephen Curry",
                                        year_season_end = 2016,
                                        return_df_only = F,
                                        use_shot_zone_side = F,
                                        season_type = "Regular Season",
                                        # Regular Season, Preseason, Playoffs, All Star
                                        shots_type = c('Dunk', 'Layup', 'Jump Shot', "Fadeaway", "Bank", "Tip",
                                                       'Hook'),
                                        shot_areas = c('Three Point', "Paint", "Mid-Range"),
                                        vs_conference = NA,
                                        # NA or East, West,
                                        vs_division = NA,
                                        # Atlantic, Central, Northwest, Pacific, Southeast, Southwest
                                        quarter_range = 1:12,
                                        minute_range = 0:12,
                                        against_team = NA,
                                        # NA Any Team Name
                                        game_location = NA,
                                        # NA or Home, Road
                                        game_month = NA,
                                        # NA or 1:12
                                        outcome = NA,
                                        # NA or W, L
                                        position = NA,
                                        # NA or Guard, Center, Forward
                                        season_segment = NA,
                                        #NA Post All-Star, Pre All-Star
                                        exclude_backcourt = T,
                                        return_message = T) {
	options(stringsAsFactors = F)
	function_packages <-
		c('dplyr',
			'magrittr',
			'data.table',
			'jsonlite',
			'tidyr',
			'stringr',
			'lubridate',
			'stringr',
			'tidyr')

  if (year_season_end < 1997) {
    stop.message <-
      "Sorry NBA Shooting data exists only since the 1996-97 Season!!"
    stop(stop.message)
  }
  load_needed_packages(function_packages)
  p <-
    player %>%
    str_to_lower() %>%
    str_replace_all('\\.', '')

  year_season_start <-
    year_season_end - 1

  id.season <-
    year_season_start %>%
    paste(year_season_end %>% substr(start = 3, stop = 4),
          sep = "-")

  players <-
    get_nba_players_ids()

  if (players %>% mutate(name.player = name.player %>% str_to_lower %>% str_replace_all('\\.', '')) %>% dplyr::filter(name.player == p) %>% .$id.player %>% length == 0) {
    stop(paste0(player,
           ' is not a valid player, try capitalizing or checking spelling') %>%
      message)
  }

  id.player <-
    players %>%
    dplyr::mutate(name.player = name.player %>% str_to_lower %>% str_replace_all('\\.', '')) %>%
    dplyr::filter(name.player == p) %>%
    .$id.player

  ## Teams
  teams_ids <-
    get_nba_franchise_data(return_franchises = 'current')

  if (!against_team %>% is.na) {
    against_team_id <-
      teams_ids %>%
      dplyr::filter(name.team == against_team) %>%
      .$id.team %>%
      as.numeric()

    against_team_stem <-
      'OpponentTeamID=' %>%
      paste0(against_team_id)
  } else {
    against_team_stem <-
      'OpponentTeamID=0'
  }

  ## URL construction
  base_url <-
    'http://stats.nba.com/stats/shotchartdetail?CFID=33&CFPARAMS=' %>%
    paste0(
      id.season,
      '&ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=&GameID=&GameSegment=&LastNGames=0&LeagueID=00'
    )

  player_stem <-
    'PlayerID=' %>%
    paste0(id.player)

  season_type_stem <-
    'SeasonType=' %>%
    paste0(season_type %>% str_replace('\\ ', '+'))

  season_stem <-
    "Season=" %>%
    paste0(id.season)

  if (game_location %>% is.na()) {
    location_stem <-
      'Location='
  } else {
    location_stem <-
      'Location=' %>%
      paste0(game_location)
  }

  if (game_month %>% is.na()) {
    month_stem <-
      'Month=0'
  } else {
    game_month <-
      'Month=' %>%
      paste0(game_month)
  }

  if (outcome %>% is.na) {
    outcome_stem <-
      'Outcome='
  } else {
    outcome_stem <-
      'Outcome=' %>%
      paste0(outcome)
  }

  if (position %>% is.na) {
    position_stem <-
      'Position='
  } else {
    position_stem <-
      'Position=' %>%
      paste0(position)
  }

  if (season_segment %>% is.na) {
    season_segment_stem <-
      'SeasonSegment='
  } else {
    season_segment_stem <-
      'SeasonSegment=' %>%
      paste0(season_segment %>% str_replace('\\ ', '+'))
  }

  if (vs_conference %>% is.na) {
    conference_stem <-
      'VsConference='
  } else {
    conference_stem <-
      'VsConference=' %>%
      paste0(vs_conference)
  }

  if (vs_division %>% is.na) {
    division_stem <-
      'VsDivision='
  } else {
    division_stem <-
      'VsDivision=' %>%
      paste0(vs_division)
  }

  final_stem <-
    'mode=Advanced&showDetails=0&showShots=1&showZones=1'

  shot_data_url <-
    base_url %>%
    paste(
      location_stem,
      'MeasureType=Base',
      month_stem,
      against_team_stem,
      outcome_stem,
      'PaceAdjust=N&PerMode=PerGame&Period=0',
      player_stem,
      'PlusMinus=N',
      position_stem,
      'Rank=N&RookieYear=',
      season_stem,
      season_segment_stem,
      season_type_stem,
      conference_stem,
      division_stem,
      final_stem,
      'TeamID=0',
      sep = '&'
    )

  parameter_df <-
    data_frame(
      id.season,
      year_season_end,
      player,
      id.player,
      use_shot_zone_side,
      outcome,
      game_month,
      game_location,
      against_team,
      vs_division,
      vs_conference,
      url.player.photo = 'http://stats.nba.com/media/players/230x185/' %>%
        paste0(id.player, '.png'),
      season_type,
      shot_areas = ifelse(
        shot_areas %>% length > 1,
        shot_areas %>% paste0(collapse = ', '),
        shot_areas
      ),
      minute_range = ifelse(
        minute_range %>% length > 1,
        minute_range %>% paste0(collapse = ', '),
        minute_range
      ),
      quarter_range = ifelse(
        position %>% length > 1,
        position %>% paste0(collapse = ', '),
        position
      ),
      date.data = Sys.Date()
    )


  json_data <-
    shot_data_url %>%
    jsonlite::fromJSON(simplifyDataFrame = T, flatten = T)
  if(json_data$resultSets$rowSet %>%
  	.[1] %>%
  	data.frame(stringsAsFactors = F) %>%
  	tbl_df %>% nrow == 0) {
  	stop("Sorry no shot data for this player")
  }
  
  data.shots <-
      json_data$resultSets$rowSet %>%
      .[1] %>%
      data.frame(stringsAsFactors = F) %>%
      tbl_df

    names(data.shots) <-
      json_data$resultSets$headers %>%
      .[1] %>%
      unlist %>%
      str_to_lower()

    data.shots %<>%
    	mutate_each_(funs(as.numeric(.)), data.shots %>% dplyr::select(matches("loc|remaining|_id|distance")) %>% names) %>% 
      mutate(
        period = period %>% as.numeric,
        shot_attempted_flag = "1" %>% grepl(shot_attempted_flag),
        shot_made_flag = "1" %>% grepl(shot_made_flag)
      )

    if (exclude_backcourt == T) {
      data.shots %<>%
        dplyr::filter(!shot_zone_basic == 'Backcourt')
    }

    sides <-
      c(
        "Above the Break 3, BC",
        "Above the Break 3, C",
        "Above the Break 3, LC",
        "Above the Break 3, RC",
        "In The Paint (Non-RA), C",
        "In The Paint (Non-RA), L",
        "In The Paint (Non-RA), R",
        "Left Corner 3, L",
        "Mid-Range, C",
        "Mid-Range, L",
        "Mid-Range, LC",
        "Mid-Range, R",
        "Mid-Range, RC",
        "Restricted Area, C",
        "Right Corner 3, R"
      )

    shot_zone_df <-
      data_frame(
        shot_zone_basic = c(
          'Left Corner 3',
          'Right Corner 3',
          'Above the Break 3',
          'Mid-Range',
          'In The Paint (Non-RA)',
          'Restricted Area'
        ),
        shot_area = c(rep('Three Point', 3), 'Mid-Range', rep('Paint', 2))
      )

    shot_zone_detail <-
      data_frame(shot_side = sides) %>%
      tidyr::separate(
        col = shot_side,
        into = c('shot_zone_basic', 'id.side'),
        sep = '\\, ',
        remove = F
      ) %>%
      left_join(shot_zone_df)

    data.shots %<>%
      dplyr::filter(period %in% quarter_range) %>%
      mutate(minute = 12 - minutes_remaining) %>%
      dplyr::filter(minute %in% minute_range)

    shots_type %<>%
      str_to_lower %>%
      paste0(collapse = "|")

    data.shots %<>%
      dplyr::rename(
        id.player = player_id,
        id.game = game_id,
        name.team = team_name,
        id.team = team_id,
        name.player = player_name
      ) %>%
      dplyr::select(-grid_type) %>%
      mutate(
        id.season,
        is.shot_made = ifelse(shot_made_flag == T, T, F),
        shot_made = ifelse(shot_made_flag == T, "YES", "NO")
      ) %>%
      separate(shot_zone_area, '\\(', into = c('side', 'id.side')) %>%
      mutate(
        id.side = id.side %>% str_replace('\\)', ''),
        shot_side = shot_zone_basic %>% paste0(', ', id.side)
      ) %>%
      left_join(shot_zone_detail) %>%
      dplyr::filter(shot_area %in% shot_areas) %>%
      mutate(st = action_type %>% str_to_lower()) %>%
      dplyr::filter(st %like% shots_type) %>%
      dplyr::select(-st) %>%
      dplyr::select(id.season,
                    name.player,
                    id.player,
                    is.shot_made,
                    shot_area,
                    everything())

    data.shots %<>%
      mutate(
        second = 60 - seconds_remaining,
        time.game = (12 * (period - 1) + minute) %>% paste0(' min ', second, ' sec')
      )

    if (return_df_only == T) {
      data <-
        data.shots %>%
        as_data_frame()
      } else {
        data <-
        	list(parameter_df, data.shots)
        
        names(data) <-
        	c('parameters', 'shots')
       }
    
    if (return_message == T) {
      "Congrats, you got " %>%
        paste0(data.shots %>% nrow,
               ' shots for ',
               player,
               ' during the ',
               id.season,
               ' season') %>% message
    }
    return(data)
}


get_team_season_shot_data <- function(team = "Brooklyn Nets",
                                      year_roster = 2016,
                                      year_data = 2016,
                                      use_shot_zone_side = F,
                                      season_type = "Regular Season",
                                      # Regular Season, Preseason, Playoffs, All Star
                                      positions = c('G', 'F', 'C'),
                                      # G, FA, C
                                      shots_type = c('Dunk', 'Layup', 'Jump Shot', "Fadeaway", "Bank", "Tip",
                                                     'Hook'),
                                      shot_areas = c('Three Point', "Paint", "Mid-Range"),
                                      vs_conference = NA,
                                      # NA or East, West,
                                      vs_division = NA,
                                      # Atlantic, Central, Northwest, Pacific, Southeast, Southwest
                                      quarter_range = 1:12,
                                      minute_range = 0:12,
                                      against_team = NA,
                                      # NA Any Team Name
                                      game_location = NA,
                                      # NA or Home, Road
                                      game_month = NA,
                                      # NA or 1:12
                                      outcome = NA,
                                      # NA or W, L
                                      season_segment = NA,
                                      #NA Post All-Star, Pre All-Star
                                      exclude_backcourt = T,
                                      return_message = T) {

  if (year_data < 1997) {
    stop.message <-
      "Sorry NBA Shooting data exists only since the 1996-97 season!!"

    stop(stop.message)
  }
  t <-
    team

  yr <-
    year_roster

  yd <-
    year_data

  rm <-
    F

  roster_data <-
    get_nba_team_season_roster(
      team = t,
      year_season_end = yr,
      return_message = rm
    )

  uid <-
    use_shot_zone_side
  st <-
    season_type
  sa <-
    shot_areas
  shot_t <-
    shots_type
  eb <-
    exclude_backcourt
  vs_conf <-
    vs_conference
  vs_div <-
    vs_division
  q_range <-
    quarter_range
  min_range <-
    minute_range
  ag_team <-
    against_team
  game_loc <-
    game_location
  game_mon <-
    game_month
  out <-
    outcome

  season_seg <-
    season_segment

  players <-
    roster_data %>%
    dplyr::filter(years.experience > 0, id.position %in% positions) %>%
    .$name.player

  all_params <-
    data_frame()

  all_shots <-
    data_frame()

  for (p in players) {
    data <-
      get_player_season_shot_data(
        player = p,
        year_season_end = yd,
        use_shot_zone_side = uid,
        season_type = st,
        shots_type = shot_t,
        shot_areas = sa,
        vs_conference = vs_conf,
        vs_division = vs_div,
        quarter_range = q_range,
        minute_range = min_range,
        against_team = ag_team,
        game_location = game_loc,
        game_month = game_mon,
        outcome = out,
        season_segment = season_seg,
        exclude_backcourt = eb,
        return_message = rm
      )

    all_params %<>%
      dplyr::bind_rows(data$parameters)

    all_shots %<>%
      dplyr::bind_rows(data$shots)
  }
  all_shots %<>%
    left_join(roster_data %>%
                dplyr::select(id.player, name.player, id.position))

  data <-
    list(all_params,
         all_shots)

  names(data) <-
    c('parameters', 'shots')
  return(data)
}

plot_nba_team_season_bokeh_shotchart <- function(team = "Brooklyn Nets",
                                                 year_roster = 2016,
                                                 year_data = 2016,
                                                 plot_hex = T,
                                                 author = "Alex Bresler",
                                                 use_shot_zone_side = T,
                                                 season_type = "Regular Season",
                                                 positions = c('G', 'F', 'C'),
                                                 shots_type = c('Dunk', 'Layup', 'Jump Shot', "Fadeaway", "Bank", "Tip",
                                                                'Hook'),
                                                 shot_areas = c('Three Point', "Paint", "Mid-Range"),
                                                 vs_conference = NA,
                                                 vs_division = NA,
                                                 quarter_range = 1:12,
                                                 minute_range = 0:12,
                                                 against_team = NA,
                                                 game_location = NA,
                                                 game_month = NA,
                                                 outcome = NA,
                                                 season_segment = NA,
                                                 exclude_backcourt = T) {
  t <-
    team

  ysr <-
    year_roster

  ysd <-
    year_data

  uid <-
    use_shot_zone_side
  st <-
    season_type
  sa <-
    shot_areas
  shot_t <-
    shots_type
  eb <-
    exclude_backcourt
  vs_conf <-
    vs_conference
  vs_div <-
    vs_division
  q_range <-
    quarter_range
  min_range <-
    minute_range
  ag_team <-
    against_team
  game_loc <-
    game_location
  game_mon <-
    game_month
  out <-
    outcome
  pos <-
    positions
  season_seg <-
    season_segment

  data <-
    get_team_season_shot_data(
      team = t,
      year_roster = ysr,
      year_data = ysd,
      use_shot_zone_side = uid,
      season_type = st,
      positions = pos,
      shots_type = shot_t,
      shot_areas = sa,
      vs_conference = vs_conf,
      vs_division = vs_div,
      quarter_range = quarter_range,
      minute_range = minute_range,
      against_team = ag_team,
      game_location = game_loc,
      game_month = game_mon,
      outcome = out,
      season_segment = season_seg,
      exclude_backcourt = eb,
      return_message = F
    )

  year_season_start <-
    ysd - 1

  id.season <-
    year_season_start %>%
    paste(ysd %>% substr(start = 3, stop = 4),
          sep = "-")

  data.shots <-
    data$shots

  data.shots %<>%
    mutate(shot_made = ifelse(shot_made_flag == T, "YES", "NO"))

  if (exclude_backcourt == T) {
    data.shots %<>%
      dplyr::filter(!id.side %in% 'BC')
  }
  parameters <-
    data$parameters

  summary_shots <-
    data.shots %>%
    group_by(shot_made_flag) %>%
    summarise(shots = n())

  accuracy_data <-
    data.shots %>%
    group_by(shot_zone_basic) %>%
    mutate(shot_value = ifelse(shot_made_flag == TRUE, 1, 0))

  nba_ids <-
    data_frame(
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
        "New Orleans Pelicans",
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
        "Washington Wizards"
      ),
      name = c(
        "Hawks",
        "Celtics",
        "Nets",
        "Hornets",
        "Bulls",
        "Cavaliers",
        "Mavericks",
        "Nuggets",
        "Pistons",
        "Warriors",
        "Rockets",
        "Pacers",
        "Clippers",
        "Lakers",
        "Grizzlies",
        "Heat",
        "Bucks",
        "Timberwolves",
        "Pelicans",
        "Knicks",
        "Thunder",
        "Magic",
        "76ers",
        "Suns",
        "Trail Blazers",
        "Kings",
        "Spurs",
        "Raptors",
        "Jazz",
        "Wizards"
      ),
      slug_current =
        c(
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
          "WAS"
        )
    ) %>%
    mutate(url.logo = 'http://stats.nba.com/media/img/teams/logos/' %>% paste0(slug_current, '_logo.svg'))

  url.team.logo <-
    nba_ids %>%
    dplyr::filter(team == t) %>%
    .$url.logo


  ## Parameter Label
  if (minute_range  == 0:12) {
    minute_label <-
      'All Minutes'
  } else {
    minute_label <-
      minute_range %>%
      paste0(collapse = ', ') %>%
      paste0('Minutes ', .)
  }

  if (quarter_range %>% length == 1) {
    quarter_label <-
      "Quarter " %>%
      paste0(quarter_range)
  }

  if (quarter_range %>% length > 6) {
    quarter_label <-
      "All Quarters"
  }

  if (quarter_range %>% length ==  2 |
      quarter_range %>% length == 3) {
    quarter_label <-
      "Quarters " %>%
      paste0(quarter_range %>% paste0(collapse = " and "))
  }

  if (outcome %>% is.na) {
    outcome_label <-
      ''
  } else {
    outcome_label <-
      outcome %>%
      str_replace("W", "In Wins ") %>%
      str_replace("L", "In Losses ")
  }

  if (against_team %>% is.na) {
    opponent_label <-
      'VS. All Opponents'
  } else {
    opponent_label <-
      ' VS.' %>%
      paste(against_team)
  }

  if (vs_conference %>% is.na) {
    conf_label <-
      ''
  } else {
    conf_label <-
      ', ' %>%
      paste0(vs_conference, 'ern Conference')
  }

  if (vs_division %>% is.na) {
    division_label <-
      ''
  } else {
    division_label <-
      ', ' %>%
      paste(vs_division, 'Division')
  }

  if (game_location %>% is.na) {
    location_label <-
      ''
  } else {
    location_label <-
      game_location %>%
      str_replace("Home", " at Home") %>%
      str_replace("Road", " on Road")
  }

  if (pos %>% length == 3) {
    position_label <-
      ", All Positions"
  }
  if (pos %>% length == 2) {
    position_label <-
      pos %>%
      paste0(collapse = ', ') %>%
      paste0(', ', .)
  }

  if (pos %>% length == 2) {
    position_label <-
      paste0(' ', pos)
  }

  if (game_month %>% is.na) {
    month_label <-
      ''
  } else {
    month_label <-
      ' During Month ' %>%
      paste0(game_month)
  }

  if (season_segment %>% is.na) {
    season_segment_label <-
      ''
  } else {
    season_segment_label <-
      season_segment %>%
      paste0(', ', .)
  }

  if (shot_areas %>% length == 3) {
    shot_area_label <-
      ', All Shot Areas'
  } else {
    shot_area_label <-
      ' from ' %>%
      paste0(shot_areas %>% paste0(collapse = ' & the '))
  }

  team_label <-
    nba_ids %>%
    dplyr::filter(name == t) %>%
    .$team %>%
    paste0(" Shot Chart", shot_area_label) %>%
    str_trim

  if (ysd == ysr) {
    season_label <-
      paste0(id.season)
  } else {
    id.season.roster <-
      paste0(ysr - 1, "-", substr(ysr, 3, 4))

    season_label <-
      paste0(id.season, ' Data, ', id.season.roster, ' Roster ')
  }

  line_2_label <-
    paste(season_label, season_type, season_segment_label) %>%
    str_trim

  line_3_label <-
    quarter_label %>%
    paste0(', & ', minute_label, month_label)

  line_4_label <-
    outcome_label %>%
    paste0(opponent_label,
           conf_label,
           division_label,
           location_label,
           position_label) %>%
    str_trim

  if (use_shot_zone_side == F) {
    ad <-
      data.shots %>%
      dplyr::select(shot_zone_basic, shot_area) %>%
      distinct() %>%
      arrange((shot_zone_basic)) %>%
      left_join(
        accuracy_data %>%
          group_by(shot_zone_basic) %>%
          summarise(
            attempts = n(),
            made = sum(shot_value),
            accuracy = made / attempts,
            accuracy_label = percent(accuracy)
          )
      )

    shot_zone_df <-
      data_frame(
        shot_zone_basic = c(
          "Above the Break 3",
          "In The Paint (Non-RA)",
          "Left Corner 3",
          "Mid-Range",
          "Restricted Area",
          "Right Corner 3"
        ),
        loc_x = c(0, 0, -209, 0, 0, 209),
        loc_y = c(280, 90, 180, 160, -25, 180),
        angle = c(0, 0, -175, 0, 0, 150),
        color = c("white", 'white', 'white', 'white', 'white', 'white')
      )

    accuracy_plot_data <-
      shot_zone_df %>%
      left_join(ad %>% dplyr::select(accuracy, shot_area, shot_zone_basic)) %>%
      mutate(accuracy_label = accuracy %>% percent %>%
               paste(shot_zone_basic, ., sep = "\n")) %>%
      dplyr::filter(shot_area %in% shot_areas)
  } else {
    ad <-
      data.shots %>%
      dplyr::select(shot_side, shot_area) %>%
      distinct() %>%
      arrange((shot_side)) %>%
      left_join(
        accuracy_data %>%
          group_by(shot_side) %>%
          summarise(
            attempts = n(),
            made = sum(shot_value),
            accuracy = made / attempts,
            accuracy_label = percent(accuracy)
          )
      )

    shot_side_label_df <-
      data_frame(
        shot_side = c(
          "Above the Break 3, C",
          "Above the Break 3, LC",
          "Above the Break 3, RC",
          "In The Paint (Non-RA), C",
          "In The Paint (Non-RA), L",
          "In The Paint (Non-RA), R",
          "Left Corner 3, L",
          "Mid-Range, C",
          "Mid-Range, L",
          "Mid-Range, LC",
          "Mid-Range, R",
          "Mid-Range, RC",
          "Restricted Area, C",
          "Right Corner 3, R"
        ),
        loc_x = c(0,
                  -105,
                  105,
                  0,
                  -90,
                  90,
                  -209,
                  0,
                  -195,
                  -95,
                  185,
                  90,
                  0,
                  200),
        loc_y = c(275,
                  250,
                  250,
                  110,
                  55,
                  55,
                  170,
                  165,
                  25,
                  170,
                  25,
                  170,
                  -25,
                  180),
        angle = c(0,
                  0,
                  0,
                  0,
                  0,
                  0,
                  -175,
                  0,
                  1.571,
                  -175,
                  4.712,
                  175,
                  0,
                  150),
        color = c(
          'white',
          'white',
          'white',
          'white',
          'white',
          'white',
          'white',
          'white',
          'white',
          'white',
          'white',
          'white',
          'white',
          'white'
        )
      )

    accuracy_plot_data <-
      shot_side_label_df %>%
      left_join(ad %>% dplyr::select(shot_side, accuracy, shot_area)) %>%
      mutate(accuracy_label = accuracy %>% percent %>%
               paste(shot_side, ., sep = " ")) %>%
      dplyr::filter(shot_area %in% shot_areas)

  }

  performance_name <-
    summary_shots$shots[2] %>% comma(digits = 0) %>%
    paste0(
      ' FGM -- ',
      data.shots %>% nrow() %>% comma(digits = 0),
      ' FGA --  ',
      summary_shots$shots[2] / data.shots %>% nrow * 100 %>% digits(2),
      '% FG PCT'
    )

  title_df <-
    data_frame(
      loc_x = c(-200, -200, 0, 0, 0, 0, 0),
      loc_y = c(385, 370, 385, 370, 355, 340, 325),
      color = c("red", "red", "black", "black", "black", "black", "black"),
      label = c(
        "Authored by",
        author,
        team_label,
        line_2_label,
        line_3_label,
        line_4_label,
        performance_name
      )
    )

  data.shots %<>%
    mutate(url.photo = 'http://stats.nba.com/media/players/230x185/' %>% paste0(id.player, '.png'))

  names(data.shots) %<>%
    gsub('\\.', '\\_', .)

  aspect <-
    993 / 1155

  tools <-
    c("reset",
      'box_select',
      "crosshair",
      "box_zoom")

  court <-
    'https://raw.githubusercontent.com/weinfz/nba_shot_value_charts/master/court.png'

  html_point_hover <-
    "<img src=@url_photo</img>"


  if (plot_hex == T) {
    p <-
      figure(
        width = 600,
        height = 600 * aspect,
        padding_factor = 1,
        xgrid = F,
        ygrid = F,
        tools = tools,
        xlab = NULL,
        ylab = NULL,
        xaxes = F,
        yaxes = F,
        ylim = c(-45, 400),
        xlim = c(-250, 250),
        toolbar_location = "right"
      ) %>%
      ly_image_url(
        x = -270,
        y = -50,
        image_url = court,
        w = 535,
        h = 465,
        anchor = 'bottom_left'
      ) %>%
      ly_image_url(
        x = 250,
        y = 400,
        image_url = url.team.logo,
        w = 512 * .15,
        h = 512 * .15,
        anchor = 'top_right'
      ) %>%
      ly_points(
        x = loc_x,
        y = loc_y,
        data =
          data.shots,
        color = "black",
        fill_color = "black",
        glyph = 20,
        alpha = .75,
        size = 2,
        hover = '<div>
        <div>
        <img src="@url_photo" height="30.833" alt="@url_photo" width="38.33" style="float: left; margin: 0px 15px 15px 0px;" border="2"></img>
        </div>
        <div>
        <span style="font-size: 12px; font-weight: bold;">@name_player</span>
        </div>
        <div>
        <span style="font-size: 10px; font-weight: bold;">Shot Distance:</span>
        <span style="font-size: 10px">@shot_distance</span>
        </div>
        <div>
        <span style="font-size: 10px; font-weight: bold;">Time of Game:</span>
        <span style="font-size: 10px">@time_game</span>
        </div>
        <div>
        <span style="font-size: 10px; font-weight: bold;">Shot Made:</span>
        <span style="font-size: 10px">@is_shot_made</span>
        </div>
        <div>
        <span style="font-size: 10px; font-weight: bold;">Shot Type:</span>
        <span style="font-size: 10px">@action_type</span>
        </div>
        <div>
        <span style="font-size: 10px; font-weight: bold;">Quarter:</span>
        <span style="font-size: 10px">@period</span>
        </div>
        <div>
        <span style="font-size: 10px; font-weight: bold;">Minute:</span>
        <span style="font-size: 10px">@minute</span>
        </div>
        </div>
        '
      ) %>%
      ly_hexbin(
        x = loc_x,
        y = loc_y,
        shape = 1,
        data = data.shots,
        alpha = .35,
        palette = "Spectral10",
        line = FALSE
      ) %>%
      ly_points(
        loc_x,
        loc_y,
        data = data.shots %>%
          dplyr::filter(shot_made == "NO"),
        color = "red",
        fill_color = "red",
        glyph = 4,
        alpha = .75,
        size = 2,
        hover = '<div>
        <div>
        <img src="@url_photo" height="30.833" alt="@url_photo" width="38.33" style="float: left; margin: 0px 15px 15px 0px;" border="2"></img>
        </div>
        <div>
        <span style="font-size: 12px; font-weight: bold;">@name_player</span>
        </div>
        <div>
        <span style="font-size: 10px; font-weight: bold;">Shot Distance:</span>
        <span style="font-size: 10px">@shot_distance</span>
        </div>
        <div>
        <span style="font-size: 10px; font-weight: bold;">Time of Game:</span>
        <span style="font-size: 10px">@time_game</span>
        </div>
        <div>
        <span style="font-size: 10px; font-weight: bold;">Shot Made:</span>
        <span style="font-size: 10px">@is_shot_made</span>
        </div>
        <div>
        <span style="font-size: 10px; font-weight: bold;">Shot Type:</span>
        <span style="font-size: 10px">@action_type</span>
        </div>
        <div>
        <span style="font-size: 10px; font-weight: bold;">Quarter:</span>
        <span style="font-size: 10px">@period</span>
        </div>
        <div>
        <span style="font-size: 10px; font-weight: bold;">Minute:</span>
        <span style="font-size: 10px">@minute</span>
        </div>
        </div>
        '
      ) %>%
      ly_text(
        x = loc_x,
        y = loc_y,
        text = accuracy_label,
        angle = angle,
        align = "center",
        baseline = "middle",
        font_size = "8pt",
        color = color,
        font_style = "bold",
        data = accuracy_plot_data
      ) %>%
      ly_text(
        x = loc_x,
        y = loc_y,
        text = label,
        align = "center",
        baseline = "middle",
        font_size = "7pt",
        color = color,
        font_style = "bold",
        data = title_df
      )
  } else {
    p <-
      figure(
        width = 600,
        height = 600 * aspect,
        padding_factor = 1,
        xgrid = F,
        ygrid = F,
        tools = tools,
        xlab = NULL,
        ylab = NULL,
        xaxes = F,
        yaxes = F,
        ylim = c(-45, 400),
        xlim = c(-250, 250),
        toolbar_location = "right"
      ) %>%
      ly_image_url(
        x = -270,
        y = -50,
        url = court,
        w = 535,
        h = 465,
        anchor = 'bottom_left'
      ) %>%
      ly_image_url(
        x = 250,
        y = 400,
        url = url.team.logo,
        w = 512 * .15,
        h = 512 * .15,
        anchor = 'top_right'
      ) %>%
      ly_points(
        loc_x,
        loc_y,
        data =
          data.shots,
        color = "black",
        fill_color = "black",
        glyph = 20,
        alpha = .75,
        size = 2,
        hover = '<div>
        <div>
        <img src="@url_photo" height="30.833" alt="@url_photo" width="38.33" style="float: left; margin: 0px 15px 15px 0px;" border="2"></img>
        </div>
        <div>
        <span style="font-size: 12px; font-weight: bold;">@name_player</span>
        </div>
        <div>
        <span style="font-size: 10px; font-weight: bold;">Shot Distance:</span>
        <span style="font-size: 10px">@shot_distance</span>
        </div>
        <div>
        <span style="font-size: 10px; font-weight: bold;">Time of Game:</span>
        <span style="font-size: 10px">@time_game</span>
        </div>
        <div>
        <span style="font-size: 10px; font-weight: bold;">Shot Made:</span>
        <span style="font-size: 10px">@is_shot_made</span>
        </div>
        <div>
        <span style="font-size: 10px; font-weight: bold;">Shot Type:</span>
        <span style="font-size: 10px">@action_type</span>
        </div>
        <div>
        <span style="font-size: 10px; font-weight: bold;">Quarter:</span>
        <span style="font-size: 10px">@period</span>
        </div>
        <div>
        <span style="font-size: 10px; font-weight: bold;">Minute:</span>
        <span style="font-size: 10px">@minute</span>
        </div>
        </div>
        '
      ) %>%
      ly_points(
        loc_x,
        loc_y,
        data = data.shots %>%
          dplyr::filter(shot_made == "NO"),
        color = "red",
        fill_color = "red",
        glyph = 4,
        alpha = .75,
        size = 2,
        hover = '<div>
        <div>
        <img src="@url_photo" height="30.833" alt="@url_photo" width="38.33" style="float: left; margin: 0px 15px 15px 0px;" border="2"></img>
        </div>
        <div>
        <span style="font-size: 12px; font-weight: bold;">@name_player</span>
        </div>
        <div>
        <span style="font-size: 10px; font-weight: bold;">Shot Distance:</span>
        <span style="font-size: 10px">@shot_distance</span>
        </div>
        <div>
        <span style="font-size: 10px; font-weight: bold;">Time of Game:</span>
        <span style="font-size: 10px">@time_game</span>
        </div>
        <div>
        <span style="font-size: 10px; font-weight: bold;">Shot Made:</span>
        <span style="font-size: 10px">@is_shot_made</span>
        </div>
        <div>
        <span style="font-size: 10px; font-weight: bold;">Shot Type:</span>
        <span style="font-size: 10px">@action_type</span>
        </div>
        <div>
        <span style="font-size: 10px; font-weight: bold;">Quarter:</span>
        <span style="font-size: 10px">@period</span>
        </div>
        <div>
        <span style="font-size: 10px; font-weight: bold;">Minute:</span>
        <span style="font-size: 10px">@minute</span>
        </div>
        </div>
        '
      ) %>%
      ly_text(
        x = loc_x,
        y = loc_y,
        text = accuracy_label,
        angle = angle,
        align = "center",
        baseline = "middle",
        font_size = "8pt",
        color = color,
        font_style = "bold",
        data = accuracy_plot_data
      ) %>%
      ly_text(
        x = loc_x,
        y = loc_y,
        text = label,
        align = "center",
        baseline = "middle",
        font_size = "7pt",
        color = color,
        font_style = "bold",
        data = title_df
      )
  }
  return(p)
}
get_player_season_shot_data_safe <-
  failwith(NULL, get_player_season_shot_data)
