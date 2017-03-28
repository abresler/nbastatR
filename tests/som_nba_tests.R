library(nbastatR)
df <-
  get_data_bref_players_seasons(
    years = 2017,
    only_totals = TRUE,
    tables = c('advanced',
               'totals')
  )

df %>%
  generate_som(topology_parameters = list(topology = "hexagonal",
                                          seed = NULL,
                                          clusters = 10,
                                          include_neural_gas = TRUE,
                                          override_max_nodes = TRUE),
               assign_to_environment = TRUE)

dataPlayers %>%
  visualize_similar_players(
    names = c("Jeremy Lin", "Russell Westbrook"),
    column_order = c('idSeason', 'nameSearch','groupSOM', 'namePlayer'),
    use_radial_network = F,
    use_som_group = TRUE,
    group_distance = 2
  )
dataPlayers %>%
  visualize_topology_network(title = "2002-2003 Season, 250 Minute Minimum, Scaled Per Minute Played and to Mean Zero -- @abresler")
