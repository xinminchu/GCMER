
#######################################
# Function to retrieve entity/clusters
# from assignment vector
#######################################


get.entity <- function(color.vector)
{
  split(seq_along(color.vector), color.vector)
}



##########################################
# Function to generate adjacency matrix
# for weighted or unweighted random graph
##########################################


make.adj.mat <- function(n, weight = c("none", "random")){
  adj.mat <- matrix(0, n, n) # create an adjacency matrix
  idx <- sample(1:(n^2), 2 * n) # randomly create edges
  if(weight == "random"){
    weight <- runif(2*n)
    adj.mat[idx] <- weight
  }else{
    adj.mat[idx] <- 1
  }
  diag(adj.mat) <- 0
  adj.mat <- adj.mat + t(adj.mat) # make sure it's symmetric
  adj.mat[adj.mat > 1] <- 1
  return(adj.mat)
}



########################################
# Function to generate adjacency matrix
# of random Erdos-Renyi graph
########################################


make.er.graph <- function(n, p = 0.5) {
if (n == 1)
	return(matrix(0, 1, 1))
adj.mat <- matrix(0L, n, n)
lower <- which(lower.tri(adj.mat))
edge <- as.logical(rbinom(choose(n,2), 1, p))
adj.mat[lower[edge]] <- 1L
adj.mat + t(adj.mat)
}


####################################
# Function to generate random graph
# with given chromatic number
####################################

make.graph.chrom <- function(n, k, p = 0.5)
{
color <- sample.int(k, n, TRUE)
sets <- split(1:n, color) # independent sets
adj.mat <- make.er.graph(n, p)
for (i in 1:k)
	adj.mat[sets[[i]], sets[[i]]] <- 0L
idx <- t(combn(sapply(sets, "[", 1), 2))
adj.mat[idx] <- 1L
adj.mat[idx[,2:1]] <- 1L
list(graph = adj.mat, k = k, sets = sets)
}



###################################
# Function to get number of colors
# in graph coloring
###################################

get.chromatic <- function(color) length(unique(color))


###################################
# Function to check validity of a
# graph coloring
###################################


is.valid.coloring <- function(adjmat, color)
{
stopifnot(is.list(adjmat) || is.matrix(adjmat))
n <- length(color)
if (is.list(adjmat)) {
	stopifnot(length(adjmat) == length(color))
	for (i in 1:n) {
		if (color[i] %in% color[adjmat[[i]]]) return(FALSE)
	}
	return(TRUE)
}
stopifnot(ncol(adjmat) == length(color))
if (!is.logical(adjmat)) adjmat <- (adjmat == 1)
for (i in 1:n) {
	neighbors <- which(adjmat[,i])
	if (color[i] %in% color[neighbors]) return(FALSE)
}
return(TRUE)
}
