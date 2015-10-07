
# GameHeader --------------------------------------------------------------
name_alex <- 
  c(
    "game_date_est",
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

name_nba <- 
  c("game_date_est", "game_sequence", "game_id", "game_status_id", 
  "game_status_text", "gamecode", "home_team_id", "visitor_team_id", 
  "season", "live_period", "live_pc_time", "natl_tv_broadcaster_abbreviation", 
  "live_period_time_bcast", "wh_status")

name_table <- 
  c('GameHeader')


# LineScore ---------------------------------------------------------------
name_table <-
  c('LineScore')

name_nba <- 
  c("game_date_est", "sequence.game", "id.game", "id.team", "slug.team", 
  "city.team", "team.record", "pts.qtr1", "pts.qtr2", 
  "pts.qtr3", "pts.qtr4", "pts.ot1", "pts.ot2", "pts.ot3", "pts.ot4", 
  "pts.ot5", "pts.ot6", "pts.ot7", "pts.ot8", "pts.ot9", "pts.ot10", 
  "pts", "pct.fg", "pct.ft", "pct.fg3", "ast", "reb", "tov")

name_alex <- 
  c("game_date_est", "sequence.game", "id.game", "id.team", "slug.team", 
    "city.team", "team.record", "pts.qtr1", "pts.qtr2", 
    "pts.qtr3", "pts.qtr4", "pts.ot1", "pts.ot2", "pts.ot3", "pts.ot4", 
    "pts.ot5", "pts.ot6", "pts.ot7", "pts.ot8", "pts.ot9", "pts.ot10", 
    "pts", "pct.fg", "pct.ft", "pct.fg3", "ast", "reb", "tov")


# SeriesStandings ---------------------------------------------------------
name_table <-
  'SeriesStandings'

name_nba <-
  c("game_id", "home_team_id", "visitor_team_id", "game_date_est", 
    "home_team_wins", "home_team_losses", "series_leader")

name_alex <-
  c("id.game", "id.home_team", "id.away_team", "game_date_est", 
    "wins.home_team", "losses.home_team", "city.series_leader")


# LastMeeting -------------------------------------------------------------
json_data$resultSets$headers[4] %>% unlist %>% str_to_lower %>% emacs
name_table <- 
  'LastMeeting'

names_nba <- 
  c("game_id", "last_game_id", "last_game_date_est", "last_game_home_team_id", 
    "last_game_home_team_city", "last_game_home_team_name", "last_game_home_team_abbreviation", 
    "last_game_home_team_points", "last_game_visitor_team_id", "last_game_visitor_team_city", 
    "last_game_visitor_team_name", "last_game_visitor_team_city1", 
    "last_game_visitor_team_points")

names_alex <- 
  c("id.game", "id.game.last", "last_game_date_est", "id.home_team.last", 
    "city.home_team.last", "name.home_team.last", "slug.home_team.last", 
    "pts.home_team.last", "id.away_team.last", "city.away_team.last", 
    "name.away_team.last", "slug.away_team.last", 
    "pts.away_team.last")


# EastConfStandingsByDay --------------------------------------------------
json_data$resultSets$headers[5] %>% unlist %>% str_to_lower %>% emacs
name_table <- 
  'EastConfStandingsByDay'
names_nba <- 
  c("team_id", "league_id", "season_id", "standingsdate", "conference", 
    "team", "g", "w", "l", "w_pct", "home_record", "road_record")

names_alex <- 
  c("id.team", "id.league", "id.season", "date.standings", "name.conference", 
    "city.team", "games", "wins", "losses", "pct.w", "home_record", "road_record")


# EastConfStandingsByDay --------------------------------------------------
json_data$resultSets$headers[6] %>% unlist %>% str_to_lower %>% emacs
name_table <- 
  'WestConfStandingsByDay'
names_nba <- 
  c("team_id", "league_id", "season_id", "standingsdate", "conference", 
    "team", "g", "w", "l", "w_pct", "home_record", "road_record")

names_alex <- 
  c("id.team", "id.league", "id.season", "date.standings", "name.conference", 
    "city.team", "games", "wins", "losses", "pct.w", "home_record", "road_record")

# EastConfStandingsByDay --------------------------------------------------
json_data$resultSets$headers[7] %>% unlist %>% str_to_lower %>% emacs
name_table <- 
  'Available'
names_nba <- 
  c("game_id", "pt_available")


names_alex <- 
  c("id.game", "pt_available")



# Leaders -----------------------------------------------------------------

json_data$resultSets$headers[8] %>% unlist %>% str_to_lower %>% emacs

name_table <- 
  'TeamLeaders'
names_nba <- 
  c("game_id", "team_id", "team_city", "team_nickname", "team_abbreviation", 
    "pts_player_id", "pts_player_name", "pts", "reb_player_id", "reb_player_name", 
    "reb", "ast_player_id", "ast_player_name", "ast")

names_alex <- 
  c("id.game", "id.team", "city.team", "name.team", "slug.team", 
    "id.player.pts_leader", "name.player.pts_leader", "pts", "id.player.reb_leader", "name.player.reb_leader", 
    "reb", "id.player.ast_leader", "name.player.ast_leader", "ast")
