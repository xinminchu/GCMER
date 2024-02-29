
## Graph conversion functions between different representations

# Graph Representation: adjacency (square) matrix, adjacency list,
# and edge list (two-column matrix) 

# Graph G(V, E): neighbors list for each vertex v --> N(v)


#######################################
# Function to convert adjacency matrix
# to adjacency list
#######################################


adjmat2adjlist <- function(x)
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

adjlist2adjmat <- function(x)
{
  stopifnot(is.list(x))
  u <- unique(unlist(x))
  n <- length(x)
  out <- matrix(0L, n, n)
  rownames(out) <- colnames(out) <- names(x)
  for (i in 1:n) 
  	out[i,x[[i]]] <- out[x[[i]],i] <- 1L
  out	
}



################################
# Function to convert edge list
# to adjacency matrix
################################

edges2adjmat <- function(x) 
{
  stopifnot(is.matrix(x) && ncol(x) == 2)
  stopifnot(is.numeric(x) || is.character(x))
  u <- unique(c(x))
  n <- if (is.numeric(u)) max(u) else length(u)
  out <- matrix(0L, n, n)
  if (is.character(x))
    rownames(out) <- colnames(out) <- u
  out[x] <- 1L
  out <- out + t(out)
  diag(out) <- 0L
  out  
}


#######################################
# Function to convert adjacency matrix
# to edge list
#######################################

adjmat2edges <- function(x)
{
  stopifnot(is.matrix(x) && nrow(x) == ncol(x))
  x[lower.tri(x, TRUE)] <- 0
  which(as.logical(x), TRUE)
}


################################
# Function to convert edge list 
# to adjacency list
################################

edges2adjlist <- function(x)
{
  stopifnot(is.matrix(x) && ncol(x) == 2)
  stopifnot(is.numeric(x) || is.character(x))
  u <- unique(c(x))
  n <- length(u)
  out <- vector("list", n)
  names(out) <- u
  for (i in 1:n) {
  	idx1 <- (x[,1] == u[i])
  	idx2 <- (x[,2] == u[i])
    vals <- c(x[idx1,2], x[idx2,1])
    out[[i]] <- unique(vals)   
  }
  out
}


# ###############################
# # Function to threshold matrix
# ###############################

# threshold.matrix <- function(x, thres)
# {
  
	
# }


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


