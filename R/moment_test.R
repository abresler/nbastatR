library(DiagrammeR)
library(magrittr)
library(visNetwork)

moments <- 
  'https://gist.githubusercontent.com/rich-iannone/9c4e50cd5d811b841349/raw/65bc97ddac2f9e52a4252fcf2060007fe1afeb7d/moment.csv' %>% 
  read_csv()

# Generate the court
court <- 
  create_nodes(nodes = "court",
               x = 0,
               y = 0,
               group = "court")

# Provide the location the ball
ball <-
  DiagrammeR::create_nodes(nodes = "ball",
                           x = moments$x_loc[1] * 19.92,
                           y = moments$y_loc[1] * 19.8,
                           group = "ball")

# Provide the location the players
players <- 
  create_nodes(nodes = moments$player_id[-1],
               x = moments$x_loc[-1] * 19.92,
               y = moments$y_loc[-1] * 19.8,
               group = letters[1:10]
  )

pics <- 
  moments %>% 
  slice(-1) %>% 
  select(team_id, player_id) %>% 
  mutate(pics = moments$player_id[-1] %>% paste0('http://stats.nba.com/media/players/230x185/',.,'.png')
  ) %>% 
  .$pics

# Combine the node data frames
combined <- combine_nodes(court, players, ball)

# Render using 'visNetwork'

visNetwork(nodes = combined) %>%
  visNodes(physics = TRUE,
           fixed = TRUE) %>%
  visPhysics(stabilization = list(enabled = TRUE,
                                  onlyDynamicEdges = FALSE,
                                  fit = TRUE)) %>%
  visGroups(groupname = "court",
            shape = "image",
            image = "http://stats.nba.com/media/img/fullcourt.svg",
            size = 1000) %>%
  visGroups(groupname = "ball",
            shape = "image",
            image = "https://raw.githubusercontent.com/rich-iannone/DiagrammeR/master/inst/examples/orange-circle.png",
            size = 30) %>%
  visGroups(groupname = "a",
            shape = "image",
            image = pics[1],
            size = 50) %>%
  visGroups(groupname = "b",
            shape = "image",
            image = pics[2],
            size = 50) %>%
  visGroups(groupname = "c",
            shape = "image",
            image = pics[3],
            size = 50) %>%
  visGroups(groupname = "d",
            shape = "image",
            image = pics[4],
            size = 50) %>%
  visGroups(groupname = "e",
            shape = "image",
            image = pics[5],
            size = 50) %>%
  visGroups(groupname = "f",
            shape = "image",
            image = pics[6],
            size = 50) %>%
  visGroups(groupname = "g",
            shape = "image",
            image = pics[7],
            size = 50) %>%
  visGroups(groupname = "h",
            shape = "image",
            image = pics[8],
            size = 50) %>%
  visGroups(groupname = "i",
            shape = "image",
            image = pics[9],
            size = 50) %>%
  visGroups(groupname = "j",
            shape = "image",
            image = pics[10],
            size = 50) %>%
  visInteraction(dragNodes = TRUE)
