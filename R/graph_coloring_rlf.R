########################################
# Recursive Largest First (RLF) method 
# for graph coloring
########################################


## Worker function
get_independent_set <- function(adjmat)
{
n <- NCOL(adjmat) 
if (n == 1) return(1L)

## Initialize independent set 
## with vertex of highest degree 
degree <- colSums(adjmat == 1)
colored <- which.max(degree)
uncolored <- (1:n)[-colored]

## Partition uncolored vertices into those that have 
## no colored neighbor and those that do	
has_colored_neighbor <- 
	(colSums(adjmat[colored, uncolored, drop = FALSE]) > 0) 
U1 <- uncolored[!has_colored_neighbor]
U2 <- uncolored[has_colored_neighbor]

## Add subsequent vertices 
while (length(U1) > 0) {
	## Numbers of neighbors in U2 for vertices of U1
	dU2 <- colSums(adjmat[U2, U1, drop = FALSE] == 1) 
	## Candidates for coloring
	idx <- which(dU2 == max(dU2))
	if (length(idx) > 1) { # break ties 
		dU1 <- colSums(adjmat[U1, U1[idx]])
		idx <- idx[which.min(dU1)]
	}
	i <- U1[idx]
	uncolored <- setdiff(uncolored, i)
	U1 <- U1[-idx]
	idx <- which(adjmat[i,U1] == 1)
	if (length(idx) > 0) {
		U2 <- c(U2, U1[idx])			
		U1 <- U1[-idx]
	}
}
colored <- if (length(uncolored) > 0) {
	(1:n)[-uncolored] } else (1:n)
colored	
}



## Wrapper function
graph_coloring_rlf <- function(graph)
{
stopifnot(is.list(graph) || is.matrix(graph))
if (is.list(graph)) {
	graph <- adjlist2adjmat(graph)
} else if (ncol(graph) == 2) {
	graph <- edges2adjmat(graph)
}
n <- NCOL(graph)
if (n == 1) return(1L)
color <- integer(n)
current_color <- 0L
uncolored <- 1:n 
while (length(uncolored) > 0) {
	current_color <- current_color + 1L
	# cat("new color", current_color, "\n")
	idx <- get_independent_set(graph[uncolored, uncolored])
	color[uncolored[idx]] <- current_color
	uncolored <- uncolored[-idx]
}	
color
}