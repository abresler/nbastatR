

# pre-process -------------------------------------------------------------

scale_per_minute <-
  function(data, scale_columns = c(
    "totalsFGM",
    "totalsFGA",
    "totalsFG3M",
    "totalsFG3A",
    "totalsFG2M",
    "totalsFG2A",
    "totalsFTM",
    "totalsFTA",
    "totalsORB",
    "totalsDRB",
    "totalsAST",
    "totalsSTL",
    "totalsBLK",
    "totalsTOV",
    "totalsPF",
    "totalsPTS"
  )) {
    if (!'totalMinutesPlayed' %in% names(data)) {
      return(data)
    }
  data <-
    data %>%
    mutate_at(
      scale_columns,
      funs(. / totalMinutesPlayed)
    )

  return(data)
}

normalize_data <-
  function(data,
         minute_minimum = 250,
         scale_to_minute = TRUE,
         id_column = 'idPlayerSeason',
         data_columns = c("totalsFG3M", "totalsFG3A", "totalsFG2M", "totalsFG2A",
                          "totalsFTM", "totalsFTA", "totalsORB", "totalsDRB",
                          "totalsAST", "totalsSTL", "totalsBLK", "totalsTOV", "totalsPF",
                          "totalsPTS", "pct3PRate", "pctFTRate", "pctORB", "pctDRB", "pctAST",
                          "pctSTL", "pctBLK", "pctTOV")
         ) {
    if (scale_to_minute %>% is_null()){
      scale_to_minute <- TRUE
    }

    if (data_columns %>% is_null()) {
      data_columns <-
        c(
          "totalsFG3M",
          "totalsFG3A",
          "totalsFG2M",
          "totalsFG2A",
          "totalsFTM",
          "totalsFTA",
          "totalsORB",
          "totalsDRB",
          "totalsAST",
          "totalsSTL",
          "totalsBLK",
          "totalsTOV",
          "totalsPF",
          "totalsPTS",
          "pct3PRate",
          "pctFTRate",
          "pctORB",
          "pctDRB",
          "pctAST",
          "pctSTL",
          "pctBLK",
          "pctTOV"
        )
    }

    if (!purrr::is_null(minute_minimum)) {
    data <-
      data %>%
      filter(totalMinutesPlayed > minute_minimum)

    }

    if (scale_to_minute) {
      data <-
        data %>%
        scale_per_minute()
    }

    df_matrix <-
      data %>%
      dplyr::select(one_of(data_columns)) %>%
      dplyr::select(which(colMeans(is.na(.)) < 1)) %>%
      as.matrix() %>%
      scale(center = TRUE, scale = TRUE)

    rownames(df_matrix) <-
      data[, id_column] %>% magrittr::extract2(1)

    return(df_matrix)
  }

# som ---------------------------------------------------------------------
find_topology <- function(data, return_message = TRUE) {
  D <-
    data %>% as.matrix()

  dlen <- dim(data)[1]
  dim <- dim(data)[2]
  munits <-
    ceiling(5 * dlen ^ 0.5)
  ## Estimate
  A <- matrix(Inf, nrow = dim, ncol = dim)
  for (i in 1:dim) {
    D[, i] <-
      D[, i] - mean(D[is.finite(D[, i]), i])
  }

  for (i in 1:dim) {
    for (j in i:dim) {
      c <- D[, i] * D[, j]
      c <- c[is.finite(c)]

      A[i, j] <- sum(c) / length(c)
      A[j, i] <- A[i, j]
    }
  }

  VS <- eigen(A)
  eigval <- sort(VS$values)

  if (eigval[length(eigval)] == 0 |
      eigval[length(eigval) - 1] * munits < eigval[length(eigval)]) {
    ratio = 1
  } else{
    ratio <- sqrt(eigval[length(eigval)] / eigval[length(eigval) - 1])
  }

  size1 <-
    min(munits, round(sqrt(munits / ratio * sqrt(0.75))))
  size2 <- round(munits / size1)

  sizes <- sort(c(size1, size2), decreasing = TRUE)
  df <-
    data_frame(x_dim = sizes[1],
               y_dim = sizes[2],
               optimal_groups = munits)

  if (return_message) {
    list("Optimal groups: ", munits, '\nX: ', sizes[1], '\nY: ', sizes[2]) %>%
      purrr::reduce(paste0) %>%
      message()
  }
  return(df)
}

generate_som_model <-
  function(data,
           x_dim = NULL,
           y_dim = NULL,
           topology = "hexagonal",
           seed = 998,
           rlength = 1000,
           clusters = 10,
           include_neural_gas = TRUE,
           include_neighbors = TRUE,
           override_max_nodes = TRUE,
           return_message = TRUE) {
    if (!class(data) == 'matrix') {
      data <-
        data %>%
        as.matrix()
    }
    if (clusters %>% is_null()) {
      clusters <- 10
    }

    if (topology %>% is_null()) {
      clusters <- "hexagonal"
    }
    if (include_neural_gas %>% is_null()) {
      include_neural_gas <-
        TRUE
    }
    if (include_neighbors %>% is_null()) {}
    if (!purrr::is_null(seed)) {
      set.seed(908)
    }
    generate_topology <-
      x_dim %>% purrr::is_null() &
      y_dim %>% purrr::is_null()

    if (generate_topology) {
      df_topo <-
        data %>%
        find_topology(return_message = return_message)

      x_dim <-
        df_topo$x_dim
      y_dim <-
        df_topo$y_dim

    }

    if (!topology %>% str_to_lower() %in% c("rectangular", "hexagonal")) {
      stop("Topology can only be rectangular or hexagonal")
    }

    som_grid <-
      kohonen::somgrid(xdim = x_dim,
                       ydim = y_dim,
                       topo = topology)
    if (rlength %>% purrr::is_null()) {
      rlength <-
        10000
    }

    som_model <-
      data %>%
      kohonen::supersom(
        grid = som_grid,
        rlen = rlength,
        alpha = c(.05, .01),
        keep.data = TRUE,
        mode = c("online", "batch", "pbatch"),
        cores = -1
        )

    som_cluster <-
      som_model$codes %>%
      data.frame(stringsAsFactors = FALSE) %>%
      dist() %>%
      hclust() %>%
      cutree(clusters) %>%
      as.numeric()

  cluster_name <-
    som_cluster[som_model$unit.classif]

  som_df <-
    data.frame(data,
               idPlayerSeason = rownames(data),
               stringsAsFactors = FALSE) %>%
    tbl_df() %>%
    mutate(groupSOM = som_model$unit.classif,
           clusterSOM = cluster_name) %>%
    dplyr::select(idPlayerSeason, clusterSOM, groupSOM, everything())

  if (include_neural_gas) {
    if (override_max_nodes) {
      if (!'df_topo' %>% exists()) {
        df_topo <-
          data %>% find_topology()
      }
      max_nodes <- df_topo$optimal_groups
    } else {
      max_nodes <- 10000
    }

    gng_nba <-
      data %>%
      gmum.r::GNG(max.nodes = max_nodes, max.edge.age = 5000)

    gng_clusters <-
      gng_nba$getClustering()

    som_df <-
      som_df %>%
      mutate(groupNeuralGas = gng_clusters) %>%
      dplyr::select(idPlayerSeason, clusterSOM, groupSOM, groupNeuralGas, everything())

    if (include_neighbors) {
      df_neighbors <-
        1:numberNodes(gng_nba) %>%
        map_df(function(x){
          countNeighbors <- length(node(gng_nba, x)$neighbours)
          node_neighbors <- node(gng_nba, x)$neighbours %>% str_c(collapse = ', ')
          data_frame(groupNeuralGas = x, countNeighbors, groupNeuralGasNeighbors = node_neighbors)
        })
      som_df <-
        som_df %>%
        left_join(df_neighbors) %>%
        dplyr::select(idPlayerSeason:groupNeuralGas, countNeighbors, groupNeuralGasNeighbors, everything()) %>%
        suppressMessages()
    }
  }

  data <-
    data_frame(nameItem = c('SOM Model', 'Codebook'), dataItem = list(som_model, som_df))

  if (include_neural_gas) {
    data <-
      data %>%
      bind_rows(data_frame(nameItem = 'GNG Server', dataItem = list(gng_nba)))
  }
  return(data)
  }

#' Generate SOM
#'
#' @param data a \code{data_frame}
#' @param normalization_parameters a \code{list} containing \itemize{
#' \item minute minimum
#' \item scale_to_minute
#' \item id_column
#' \item data columns
#' }
#' @param topology_parameters a list containing \itemize{
#' \item x_dim
#' \item y_dim
#' \item seed
#' \item clusters
#' \item rlength
#' \item include_neural_gas
#' \item include_neighbors
#' \item override max_nodes
#' }
#' @param filter_na
#' @param assign_to_environment if \code{TRUE} assigns to environment
#' @param return_message
#'
#' @return
#' @export
#' @import gmum.r kohonen purrr readr tidyr dplyr stringr
#' @examples
generate_som <-
  function(data,
           normalization_parameters = list(minute_minimum = 250, scale_to_minute = TRUE, id_column = 'idPlayerSeason', data_columns = c(
      "totalsFG3M",
      "totalsFG3A",
      "totalsFG2M",
      "totalsFG2A",
      "totalsFTM",
      "totalsFTA",
      "totalsORB",
      "totalsDRB",
      "totalsAST",
      "totalsSTL",
      "totalsBLK",
      "totalsTOV",
      "totalsPF",
      "totalsPTS",
      "pct3PRate",
      "pctFTRate",
      "pctORB",
      "pctDRB",
      "pctAST",
      "pctSTL",
      "pctBLK",
      "pctTOV"
    )
  ),
  topology_parameters = list(
    x_dim = NULL,
    y_dim = NULL,
    topology = "hexagonal",
    seed = 998,
    clusters = 10,
    rlength = 1000,
    include_neural_gas = TRUE,
    override_max_nodes = TRUE
  ),
  filter_na = TRUE,
  assign_to_environment = TRUE,
  return_message = TRUE
  ) {
    scaled_data <-
      data %>%
      normalize_data(
        minute_minimum = normalization_parameters$minute_minimum,
        scale_to_minute = normalization_parameters$scale_to_minute,
        id_column = normalization_parameters$id_column,
        data_columns = normalization_parameters$data_columns
      )

    all_data <-
      scaled_data %>%
      generate_som_model(
        x_dim = topology_parameters$x_dim,
        y_dim = topology_parameters$y_dim,
        topology = topology_parameters$topology,
        seed = topology_parameters$seed,
        clusters = topology_parameters$clusters,
        rlength = topology_parameters$rlength,
        include_neural_gas = topology_parameters$include_neural_gas,
        override_max_nodes = topology_parameters$override_max_nodes
      )

    df <-
      data %>%
      left_join(all_data$dataItem[[2]] %>%
                  dplyr::select(
                    one_of(
                      normalization_parameters$id_column,
                      'clusterSOM',
                      'groupSOM',
                      'groupNeuralGas',
                      'groupNeuralGasNeighbors',
                      'countNeighbors'
                    )
                  )) %>%
      dplyr::select(idSeason:namePlayer, one_of(
        c(
          'clusterSOM',
          'groupSOM',
          'groupNeuralGas',
          'groupNeuralGasNeighbors',
          'countNeighbors'
        )
      ), everything()) %>%
      suppressMessages()

    if (filter_na) {
      df <-
        df %>%
        filter(!clusterSOM %>% is.na())
    }

    df_scaled <-
      all_data$dataItem[[2]] %>%
      left_join(data %>% dplyr::select(
        one_of(
          'idSeason',
          'idPlayerSeason',
          'namePlayer',
          'slugTeamBREF',
          'idPosition'
        )
      )) %>%
      dplyr::select(idSeason,
                    idPlayerSeason,
                    namePlayer,
                    slugTeamBREF,
                    idPosition,
                    everything()) %>%
      suppressMessages()

    final_data <-
      data_frame(
        nameItem = c('dataPlayers', 'modelSOM', 'dataPlayersScaled'),
        dataItem = list(df, all_data$dataItem[[1]], df_scaled)
      )

    if ("GNG Server" %in% all_data$nameItem) {
      final_data <-
        final_data %>%
        bind_rows(
          data_frame(
            nameItem = c('serverGNG'),
            dataItem = list(all_data$dataItem[[3]])
          )
        )
    }

    if (assign_to_environment) {
      1:nrow(final_data) %>%
        purrr::walk(function(x) {
          table <-
            final_data %>%
            slice(x)
          assign(x = table$nameItem,
                 eval(table$dataItem[[1]]),
                 envir = .GlobalEnv)
        })
    }

    return(all_data)
  }

# plot --------------------------------------------------------------------

#' Plot Topology Network
#'
#' @param data a \code{data_frame}
#' @param name_player_id_column \code if \code{TRUE} uses player season id instead of name for nodes
#' @param group_node Grouping node \itemize{
#' \item \code{SOM}: Self Organizing Map
#' \item \code{GNG, Neural Gas}: Neural Gas Node
#' }
#' @param title Plot Title
#' @param subtitle Plot subtitle
#' @param footer Plot footer
#'
#' @return
#' @export
#' @import visNetwork dplyr stringr
#' @examples
visualize_topology_network <-
  function(data,
           use_name_player_id_column = FALSE,
           group_node = 'SOM',
           include_gng_neighbors = TRUE,
           title =  "NBA Player Topology: 2015-16 Season, 250 Minute Minimum -- Alex Bresler",
           subtitle = 'data normalized per minute, scaled to mean zero',
           footer = '') {
    if (group_node %>% str_to_lower() %in% c('gng', 'neural gas')) {
      data <-
        data %>%
        dplyr::select(-groupSOM) %>%
        dplyr::rename(groupSOM = groupNeuralGas)
      group_type <- ' Neural Gas Nodes '
    } else {
      group_type <- ' SOM Groups '
    }

    if (use_name_player_id_column) {
      data <-
        data %>%
        dplyr::select(-namePlayer) %>%
        dplyr::rename(namePlayer = idPlayerSeason)
    }

    subtitle <-
      list(
        data$clusterSOM %>% unique() %>% length(),
        ' Clusters and ',
        data$groupSOM %>% unique() %>% length(),
        group_type,
        subtitle
      ) %>%
      purrr::reduce(paste0)
    name_nodes <-
      data %>%
      dplyr::select(label = namePlayer, group = idPosition) %>%
      mutate(id = paste0("N", 1:nrow(.))) %>%
      dplyr::select(id, label, group)

    group_nodes <-
      data %>%
      dplyr::select(groupSOM) %>%
      distinct() %>%
      arrange(groupSOM) %>%
      mutate(label = paste('Group', groupSOM),
             id = paste0('G', groupSOM)) %>%
      dplyr::select(id, label)

    cluster_nodes <-
      data   %>%
      dplyr::select(clusterSOM) %>%
      distinct() %>%
      arrange(clusterSOM) %>%
      mutate(label = paste('Cluster', clusterSOM),
             id = paste0('C', 1:nrow(.))) %>%
      dplyr::select(id, label)

    nodes <-
      cluster_nodes %>%
      bind_rows(group_nodes, name_nodes)

    ##
    id_clusters <-
      data %>%
      mutate(label = paste("Cluster", clusterSOM)) %>%
      dplyr::select(label, clusterSOM) %>%
      distinct() %>%
      left_join(nodes) %>%
      rename(nameCluster = clusterSOM,
             idCluster = id) %>%
      arrange(nameCluster) %>%
      suppressMessages()

    id_groups <-
      data %>%
      mutate(label = paste("Group", groupSOM)) %>%
      dplyr::select(label, groupSOM) %>%
      distinct() %>%
      left_join(nodes) %>%
      rename(nameGroup = groupSOM,
             idGroup = id) %>%
      suppressMessages()

    id_names <-
      data %>%
      dplyr::select(label = namePlayer, group = idPosition) %>%
      dplyr::select(label, group) %>%
      left_join(nodes) %>%
      rename(idName = id) %>%
      suppressMessages()

    cluster_group_edges <-
      data %>%
      dplyr::select(nameCluster = clusterSOM, nameGroup = groupSOM) %>%
      distinct() %>%
      left_join(id_clusters) %>%
      suppressMessages()

    cluster_group_edges <-
      cluster_group_edges %>%
      dplyr::select(nameGroup) %>%
      left_join(id_groups) %>%
      dplyr::select(-label, -group) %>%
      left_join(cluster_group_edges) %>%
      dplyr::select(from = idCluster, to = idGroup) %>%
      suppressMessages()

    name_edges <-
      data %>%
      dplyr::select(label = namePlayer, nameGroup = groupSOM) %>%
      left_join(id_names) %>%
      dplyr::select(nameGroup, to = idName) %>%
      left_join(id_groups) %>%
      dplyr::select(from = idGroup, to) %>%
      distinct() %>%
      suppressMessages()


    edges <-
      bind_rows(cluster_group_edges,
                name_edges)
    viz <-
      nodes %>%
      mutate(group = id,
             title = label) %>%
      visNetwork(
        edges,
        width = '100%',
        main = title,
        submain = subtitle,
        footer = footer
      ) %>%
      visGroups(groupname = "PG", color = "lightgreen") %>%
      visPhysics(stabilization = TRUE, maxVelocity = 10) %>%
      visEdges(smooth = TRUE, arrows = "to") %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visLayout(randomSeed = 123) %>%
      visInteraction(navigationButtons = TRUE)


    return(viz)
  }


# similar_players ---------------------------------------------------------


get_similar_player <- function(data,
                               use_som_group = TRUE,
                               name = 'Jeremy Lin',
                               group_distance = 2) {
  if (!'groupNeuralGas' %in% names(data)) {
    use_som_group <- TRUE
  }

  if (use_som_group) {
    uniques <-
      data %>%
      filter(namePlayer %in% c(name)) %>%
      .$groupSOM %>%
      unique()
  } else {
    uniques <-
      data %>%
      filter(namePlayer %in% c(name)) %>%
      .$groupNeuralGas %>%
      unique()
  }

  group_name <-
    uniques

  if (!group_distance == 0) {
    start_num <-
      uniques - group_distance

    end_num <-
      uniques + group_distance

    uniques <-
      start_num:end_num

  }
  if (use_som_group) {
    df_matches <-
      data %>%
      filter(groupSOM %in% c(uniques)) %>%
      mutate(
        nameSearch = name,
        groupName = group_name[[1]],
        isSameGroup = ifelse(groupSOM == groupName, TRUE, FALSE)
      ) %>%
      dplyr::select(nameSearch,
                    groupName,
                    isSameGroup,
                    idSeason,
                    namePlayer:groupSOM,
                    matches("groupNeuralGas"))
  } else {

    df_matches <-
      data %>%
      filter(groupSOM %in% c(uniques)) %>%
      mutate(
        nameSearch = name,
        groupName = group_name[[1]],
        isSameGroup = ifelse(groupNeuralGas == groupName, TRUE, FALSE)
      ) %>%
      dplyr::select(nameSearch,
                    groupName,
                    isSameGroup,
                    idSeason,
                    namePlayer:groupSOM,
                    matches("groupNeuralGas"))
  }

  return(df_matches)
}

get_similar_players_som <-
  function(data,
           use_som_group = TRUE,
           names =
             c("Trevor Booker", "Spencer Dinwiddie"),
           group_distance = 0) {
    get_similar_player_safe <-
      purrr::possibly(get_similar_player, data_frame())
    df_similar <-
      names %>%
      map_df(function(x) {
        get_similar_player_safe(
          data = data,
          name = x,
          use_som_group = use_som_group,
          group_distance = group_distance
        )
      }) %>%
      arrange(nameSearch, groupSOM) %>%
      dplyr::select(nameSearch,
                    idSeason,
                    namePlayer,
                    isSameGroup,
                    matches("groupNeuralGas"),
                    everything())

    return(df_similar)
  }

visualize_network <-
  function(data,
           radial_title = "Documents",
           nodes = c('typeDocument', 'nameParty', 'nameSearch'),
           use_radial_network = TRUE) {
    data <-
      data %>%
      dplyr::select(one_of(nodes)) %>%
      distinct() %>%
      mutate(titleRadial = radial_title)

    unite_nodes <-
      c('titleRadial', nodes)

    data <-
      data %>%
      unite_("pathString",
             unite_nodes,
             sep = '|',
             remove = FALSE) %>%
      data.tree::as.Node(pathDelimiter = "|") %>%
      data.tree::ToListExplicit(unname = TRUE)

    if (use_radial_network) {
      viz <-
        data %>%
        networkD3::radialNetwork(
          fontSize = 8,
          fontFamily = "Arial",
          nodeStroke = "black",
          nodeColour = 'red',
          linkColour = "#AAA",
          opacity = 0.9
        )
    } else {
      viz <-
        data %>%
        networkD3::diagonalNetwork(
          fontSize = 9,
          fontFamily = "Arial",
          nodeStroke = "black",
          nodeColour = 'red',
          linkColour = "#AAA",
          opacity = 0.9
        )
    }
    return(viz)
  }

#' Visualize Similar Players
#'
#' @param data
#' @param names
#' @param group_distance
#' @param use_som_group
#' @param column_order
#' @param use_radial_network
#' @param title
#'
#' @return
#' @export
#' @import data.tree networkD3 dplyr tidyr stringr
#'
#' @examples
visualize_similar_players <-
  function(data,
           names = c("Russell Westbrook", "Bradley Beal"),
           group_distance = 1,
           use_som_group = TRUE,
           column_order = c('nameSearch', 'clusterSOM', 'groupSOM', 'namePlayer'),
           use_radial_network = TRUE,
           title = "Similar Players") {

      df_network <-
        data %>%
      get_similar_players_som(names = names,
                              group_distance = group_distance,
                              use_som_group = use_som_group) %>%
      mutate(
        isSameGroup = isSameGroup %>% as.numeric(),
        clusterSOM = "Cluster: " %>% str_c(clusterSOM),
        groupSOM = "SOM: " %>% str_c(groupSOM),
        groupNeuralGas = "GNG: " %>% str_c(groupNeuralGas)
      )

      network <-
        df_network %>%
        visualize_network(
          radial_title = title,
          nodes =
            column_order,
          use_radial_network = use_radial_network
        )

    return(network)
  }
