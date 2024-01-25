# Graph conversion between different representations

#graph: dense graph, sparse graph
#nodes and edges; direct/undirected; weighted/unweighted

# Graph Representation in 'igraph' R package:
# Graph Representation: adjacency matrix, adjacency list, tri-column table


#######################################
# Function to convert adjacency matrix
# to adjacency list
#######################################


adjmat2list <- function(mat)
{
  stopifnot(is.matrix(mat))
  stopifnot(identical(rownames(mat), colnames(mat)))
  stopifnot(is.numeric(mat[1]) || is.logical(mat[1]))
  stopifnot(nrow(mat) == ncol(mat))
  stopifnot(all(mat == t(mat)))

  if (is.numeric(mat[1])) {
    stopifnot(all(mat %in% c(0, 1)))
    #stopifnot(all(diag(mat) == 0))
    if(!all(diag(mat) == 0)){
      diag(mat) == 0
    }
    out <- apply(mat == 1, 2, which)
  } else {
    stopifnot(all(!diag(mat)))
    out <- apply(mat, 2, which)
  }
  names(out) <- rownames(mat)
  out
}


#####################################
# Function to convert adjacency list
# to adjacency matrix
#####################################

adjlist2mat <- function(ll)
{
  stopifnot(is.list(ll))
  ll <- lapply(ll, as.integer)
  stopifnot(all(unlist(ll) >= 1 & unlist(ll) <= length(ll)))
  n <- length(ll)
  out <- matrix(0L, n, n)
  nnb <- sapply(ll, length)
  out[cbind(rep(1:n, nnb), unlist(ll))] <- 1L
  if (any(diag(out) == 1L))
    stop("Invalid adjacency list: please remove any vertex from the list of its neighbors.")
  if (!all(out == t(out)))
    stop("Invalid adjacency list: please make sure that the",
         "adjacency relationship is symmetric (if vertex A is",
         "adjacent to vertex B, then vertex B must be adjacent",
         "to vertex A).")
  out
}



#####################################
# Function to convert similarity table
# to adjacency matrix
#####################################

simtab2adj <- function(simtab, threshold=1){
  g0 <- as.matrix(simtab)

  n.nodes <- max(g0[,1:2])
  g1 <- matrix(0, ncol = n.nodes, nrow = n.nodes)

  for(i in 1:nrow(g0)){
    g1[g0[i,1], g0[i,2]] <- g0[i,3]
  }

  if (!all(g1 %in% c(0, 1))) g1 <- (g1 >= threshold)
  ## Convert graph to adjacency list in view of graph coloring
  g1[g1!=t(g1)] <- 1
  return(g1)
}

#####################################
# Function to convert adjacency matrix
# to similarity table
#####################################

adjmat2simtab <- function(mat, threshold=1){
  mat[mat<threshold] <- 0
  simtab <- which(mat != 0, arr.ind = TRUE)
  out <- simtab[simtab[,1] < simtab[,2],]
  out
}


#####################################
# Function to obtain neighbors
# from similarity table
#####################################

#name: threeCol2list
#weighted / unweighted
neighbors <- function(simtab, threshold=0){
  #first column: start, second column: end, third: similarity
  stopifnot(ncol(simtab) == 3)

  simtab[simtab[,3] < threshold,] <- NA
  simtab <- simtab[!is.na(simtab[,1]),]
  n.nodes <- max(simtab[,1:2])
  nbor <- vector("list", n.nodes)
  for(i in 1: n.nodes){
    nbor[[i]] <- sort(unique(c(simtab[simtab[,1]==i, 2], simtab[simtab[,2]==i, 1])))
  }
  #nbor[sapply(nbor, length) == 0] <- NULL # double check if appropriate
  nbor
}

#directed / undirected??


# Examples
adj_matrix <- make.adj.mat(8, "none")
adj_matrix <- make.adj.mat(8, "random")
adj_matrix

edge_table <- data.frame(
  From = c(1, 2, 2, 3, 4),
  To = c(2, 1, 3, 4, 3),
  #Weight = c(1, 1, 1, 1, 1)
  Weight = c(1, 0.4, 0.8, 0.3, 1)
)

neighborhood_list <- list(
  c(2),       # Neighbors of node 1
  c(1, 3, 4), # Neighbors of node 2
  c(4),
  c(3)
)

system.time(
  #test <- adjmat2list(adj_matrix);
  #test2 <- adjlist2mat(test)
  test3 <- simtab2adj(edge_table, 0.5)
  #test4 <- neighbors(edge_table)
  #test5 <- adjmat2simtab(adj_matrix)
)

test
