
## Graph conversion functions between different representations

# Graph Representation: adjacency matrix, adjacency list,
# and three columns table (similarity table)

# Graph G(V, E): neighbors list for each vertex v --> N(v)


#######################################
# Function to convert adjacency matrix
# to adjacency list
#######################################


adjmat2list <- function(x)
{
  stopifnot(is.matrix(x))
  stopifnot(nrow(x) == ncol(x))
  stopifnot(all(x == t(x)))  
  xnames <- rownames(x)
  if (!is.logical(x)) 
    x <- matrix(as.logical(x), nrow(x), ncol(x))
  if (any(diag(x))) diag(x) <- FALSE
  out <- apply(x, 2, which, simplify = FALSE)
  names(out) <- xnames
  out
}


#####################################
# Function to convert adjacency list
# to adjacency matrix
#####################################

adjlist2mat <- function(nbor)
{
  stopifnot(is.list(nbor))
  nbor <- lapply(nbor, as.integer)
  stopifnot(all(unlist(nbor) >= 1 & unlist(nbor) <= length(nbor)))
  n <- length(nbor)
  out <- matrix(0L, n, n)
  nnb <- sapply(nbor, length)
  out[cbind(rep(1:n, nnb), unlist(nbor))] <- 1L
  if (any(diag(out) == 1L))
    stop("Invalid adjacency list: please remove any vertex from the list of its neighbors.")
  if (!all(out == t(out)))
    stop("Invalid adjacency list: please make sure that the",
         "adjacency relationship is symmetric (if vertex A is",
         "adjacent to vertex B, then vertex B must be adjacent",
         "to vertex A).")
  out
}



##########################################
# Function to convert similarity table
# to adjacency matrix
##########################################

twocol2adjmat <- function(x, n = NULL) 
{
  stopifnot(is.matrix(x) && ncol(x) == 2)
  if (is.character(x)) {
  	xnames <- unique(c(x))
    n <- length(xnames)
  	out <- matrix(0L, n, n)
  	rownames(out) <- colnames(out) <- xnames
  } else {
    n <- ifelse(is.null(n), length(unique(c(x))), 
      as.integer(n))
    out <- matrix(0L, n, n)
  }
  out[x] <- 1L
  out[x[,2:1]] <- 1L
  diag(x) <- 0L
  return(out)
}


##########################################
# Function to convert adjacency matrix
# to two-column matrix
##########################################

adjmat2twocol <- function(x)
{
  stopifnot(is.matrix(x) && nrow(x) == ncol(x))
  which(as.logical(x), TRUE)
}


#########################################
# Function to convert similarity table
# to adjacency list
#########################################

twocol2adjlist <- function(simtab, threshold=0){
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


##########
# Example
##########
# adj_matrix <- make.adj.mat(8, "none")
# adj_matrix <- make.adj.mat(8, "random")
# adj_matrix
#
# edge_table <- data.frame(
#   From = c(1, 2, 2, 3, 4),
#   To = c(2, 1, 3, 4, 3),
#   #Weight = c(1, 1, 1, 1, 1)
#   Weight = c(1, 0.4, 0.8, 0.3, 1)
# )
#
# neighborhood_list <- list(
#   c(2),       # Neighbors of node 1
#   c(1, 3, 4), # Neighbors of node 2
#   c(4),
#   c(3)
# )
#
#
# test1 <- adjmat2list(adj_matrix)
# test2 <- adjlist2mat(test1)
# test3 <- simtab2adjmat(edge_table, 0.5)
# test4 <- adjmat2simtab(adj_matrix)
# test5 <- simtab2adjlist(test4)


