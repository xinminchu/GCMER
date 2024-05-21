graph_coloring <- function(graph, 
	method = c("lf", "sl", "dsatur", "rlf", "msc", "lmxrlf", "tabu"))
{
stopifnot(is.matrix(graph) || is.list(graph))
if (is.matrix(graph)) {
	test <- is.logical(graph) || all(graph %in% c(0,1))
	graph <- if (test) adjmat2adjlist(graph) else edges2adjlist(graph)
} 
method <- match.arg(method, several.ok = TRUE)
n <- length(graph)
out <- matrix(0, n, length(method))
colnames(out) <- method
for (m in method)
	out[,m] <- switch(m, 
		lf = graph_coloring_greedy(graph, "lf"),
		sl = graph_coloring_greedy(graph, "sl"),
		dsatur = graph_coloring_dsatur(graph),
		rlf = graph_coloring_rlf(graph),
		msc = graph_coloring_msc(graph),
		lmxrlf = graph_coloring_lmxrlf(graph),
		tabu = graph_coloring_tabucol(graph, ...))

out
}		

