########################
# Function to perform 
# greedy graph coloring 
########################

graph_coloring_greedy <- function(adj_list, method = c("lf", "sl")) 
{
stopifnot(is.list(adj_list))
method <- match.arg(method)
n <- length(adj_list)
coloring <- integer(n)
all_colors <- 1:n
degree <- sapply(adj_list, length)
if (method == "lf") {
	sorted_vertices <- order(degree, decreasing = TRUE)
} else {
	sorted_vertices <- integer(n)
	for (i in n:1) {
		idx <- which.min(degree)
		sorted_vertices[i] <- idx
		if (degree[idx] > 0)
			degree[adj_list[[idx]]] <- degree[adj_list[[idx]]] - 1L
		degree[idx] <- NA
	}
}
for (vertex in sorted_vertices) {
	used_colors <- unique(coloring[adj_list[[vertex]]])
	used_colors <- used_colors[used_colors > 0L]
	coloring[vertex] <- 	if (length(used_colors) > 0) {
		all_colors[-used_colors][1] } else 1L
}  
coloring
}
