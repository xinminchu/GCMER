########################
# Function to perform 
# greedy graph coloring 
########################

graph_coloring_greedy <- function(graph, method = c("lf", "sl")) 
{
stopifnot(is.list(graph))
method <- match.arg(method)
n <- length(graph)
coloring <- integer(n)
all_colors <- 1:n
degree <- sapply(graph, length)
if (method == "lf") {
	sorted_vertices <- order(degree, decreasing = TRUE)
} else {
	sorted_vertices <- integer(n)
	for (i in n:1) {
		idx <- which.min(degree)
		sorted_vertices[i] <- idx
		if (degree[idx] > 0)
			degree[graph[[idx]]] <- degree[graph[[idx]]] - 1L
		degree[idx] <- NA
	}
}
for (vertex in sorted_vertices) {
	used_colors <- unique(coloring[graph[[vertex]]])
	used_colors <- used_colors[used_colors > 0L]
	coloring[vertex] <- 	if (length(used_colors) > 0) {
		all_colors[-used_colors][1] } else 1L
}  
coloring
}
