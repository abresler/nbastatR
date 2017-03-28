library(nbastatR)
df <- get_data_bref_players_seasons(years = 2003,)

df %>%
  generate_som(assign_to_environment = TRUE)

dataPlayers %>%
  visualize_similar_players(names = c('Ray Allen', 'Allen Iverson'))
dataPlayers %>%
  visualize_topology_network(title = "2002-2003 Season, 250 Minute Minimum, Scaled Per Minute Played and to Mean Zero -- @abresler")
