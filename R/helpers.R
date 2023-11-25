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
	stopifnot(all(diag(mat) == 0))
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

sim.tab2adj <- function(sim.tab, threshold){
  g0 <- as.matrix(sim.tab)

  n.nodes <- max(g0[,1:2])
  g1 <- matrix(0, ncol = n.nodes, nrow = n.nodes)

  for(i in 1:nrow(g0)){
    g1[g0[i,1], g0[i,2]] <- g0[i,3]
  }

  if (!all(g1 %in% c(0, 1))) g1 <- (g1 >= threshold)
  ## Convert graph to adjacency list in view of graph coloring
  g1 <- g1 + t(g1)
  g1
}


#####################################
# Function to obtain neighbors
# from similarity table
#####################################

#name: threeCol2list
#weighted / unweighted
neighbors <- function(sim.tab, threshold){
  #first column: start, second column: end, third: similarity
  stopifnot(ncol(sim.tab) == 3)

  sim.tab[sim.tab[,3] < threshold,] <- NA
  sim.tab <- sim.tab[!is.na(sim.tab[,1]),]
  n.nodes <- max(sim.tab[,1:2])
  nbor <- vector("list", n.nodes)
  for(i in 1: n.nodes){
    nbor[[i]] <- sort(unique(c(sim.tab[sim.tab[,1]==i, 2], sim.tab[sim.tab[,2]==i, 1])))
  }
  #nbor[sapply(nbor, length) == 0] <- NULL # double check if appropriate
  nbor
}




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
