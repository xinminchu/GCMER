# Graph representation conversion

#graph: dense graph, sparse graph
#nodes and edges; direct/undirected; weighted/unweighted

# Graph Representation: adjacency matrix, adjacency list, tri-column table



graph.conversion <- function(g, from = "adj_mat", to = "adj_list"){
  # to know what the current form of graph 'g'

  # adj_mat to adj_list

  # adj_list to 3col_tab

  # adj_mat to 3col_tab

  # 3col_tab to adj_mat


}


#################################################
# Convert adjacency matrix to neighborhood list
#################################################


adj_matrix_to_neighborhood_list <- function(adj_matrix) {
  # n <- nrow(adj_matrix)
  # neighborhood_list <- vector("list", n)
  #
  # for (i in 1:n) {
  #   neighbors <- which(adj_matrix[i, ] == 1)
  #   neighborhood_list[[i]] <- neighbors
  # }

  neighborhood_list <- apply(adj_matrix, 1, function(row) which(row != 0))

  return(neighborhood_list)
}



#################################################
# Convert adjacency matrix to 3-column table
# 3-column table / edge table form:
# first column "from",
# second column "to",
# third column: "weight"
#################################################

adj_matrix_to_3column_table <- function(adj_matrix) {
  # n <- nrow(adj_matrix)
  # edges <- which(adj_matrix != 0, arr.ind = TRUE)
  #
  # three_column_table <- data.frame(
  #   From = edges[, 1],
  #   To = edges[, 2],
  #   Weight = adj_matrix[edges]
  # )

  # Get the indices of non-zero elements
  indices <- which(adj_matrix != 0, arr.ind = TRUE)

  # Create the three-column table
  three_column_table <- data.frame(
    From = indices[, 1],
    To = indices[, 2],
    Weight = adj_matrix[indices]
  )

  return(three_column_table)
}


#################################################
# Convert three column table to adjacency matrix
# 3-column table / edge table form:
# first column "from",
# second column "to",
# third column: "weight"
#################################################

edge_table_to_adj_matrix <- function(edge_table, thres = 0) {
  # Find the maximum vertex label to determine the size of the matrix
  max_vertex <- max(edge_table$From, edge_table$To)

  # Initialize an adjacency matrix with zeros
  adj_matrix <- matrix(0, nrow = max_vertex, ncol = max_vertex)

  # Fill in the adjacency matrix
  for (i in 1:nrow(edge_table)) {
    adj_matrix[edge_table[i, 1], edge_table[i, 2]] <- edge_table[i, 3]
  }

  if (!all(adj_matrix %in% c(0, 1))){
    adj_matrix <- (adj_matrix >= thres)
  }

  ## Convert graph to adjacency list in view of graph coloring
  adj_matrix <- adj_matrix + t(adj_matrix)

  return(adj_matrix)
}


#################################################
# Convert neighborhood list to three column table
# 3-column table / edge table form:
# first column "from",
# second column "to",
# third column: "weight"
#################################################

neighborhood_list_to_3column_table <- function(neighborhood_list, default_weight = 1) {
  # Initialize vectors to store the 'From', 'To', and 'Weight' data
  from <- numeric()
  to <- numeric()
  weight <- numeric()

  # Iterate over the neighborhood list to populate the vectors
  for (node in seq_along(neighborhood_list)) {
    nbrs <- neighborhood_list[[node]]
    n_edges <- length(nbrs)

    from <- c(from, rep(node, n_edges))
    to <- c(to, nbrs)
    weight <- c(weight, rep(default_weight, n_edges))
  }

  # Create a data frame from the vectors
  edge_table <- data.frame(From = from, To = to, Weight = weight)

  return(edge_table)
}

#directed / undirected??


# Examples
adj_matrix <- make.adj.mat(8, "none")
adj_matrix <- make.adj.mat(8, "random")
adj_matrix

edge_table <- data.frame(
  From = c(1, 2, 2, 3, 4),
  To = c(2, 1, 3, 4, 3),
  Weight = c(1, 1, 1, 1, 1)
)

neighborhood_list <- list(
  c(2),       # Neighbors of node 1
  c(1, 3, 4), # Neighbors of node 2
  c(4),
  c(3)
)

system.time(
  #test <- convert_to_neighborhood_list(adj_matrix)
  #test <- convert_to_three_column_table(adj_matrix)
  #test <- edge_table_to_adj_matrix(edge_table)
  test <- neighborhood_list_to_3column_table(neighborhood_list)
)

test
