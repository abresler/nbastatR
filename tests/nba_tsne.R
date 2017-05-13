ssetwd("~")
gdeltr2::load_needed_packages(c('dplyr', 'nbastatR', 'highcharter', 'glue', 'purrr', 'stringr'))
df_nba <- "Desktop/abresler.github.io/data/nba/nba_1950_2017_stats.rda" %>%
  asbmisc::read_rda_file()

df_nba %>%
  filter(yearSeason >= 1980) %>%
  generate_som()

ig <-
  serverGNG %>%
  gmum.r::convertToIGraph()

asbmisc::visualize_gng_node_graph(igraph_object = ig, title = "NBA GNG Node Graph 1980-2017")

harden_gng <-
  dataPlayers %>%
  filter(namePlayer %>% str_detect("Harden")) %>%
  filter(yearSeason == 2017) %>%
  .$GroupNeuralGas

dataPlayers %>%
  filter(GroupNeuralGas == harden_gng)