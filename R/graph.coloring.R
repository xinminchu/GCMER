
######################################
# Wrapper function for graph coloring 
######################################

graph.coloring <- function(adj.list, method = NULL)
{
## Argument checks
# stopifnot(is.list(adj.list))
# stopifnot(is.numeric(unlist(adj.list)) || 
	# is.character(unlist(adj.list)))
# if (is.numeric(adj.list[[1]]))
	# stopifnot(all(unlist(adj.list) %in% seq(1, length(adj.list))))
# if (is.character(adj.list[[1]]))
	# stopifnot(all(unlist(adj.list) %in% names(adj.list)))
if (is.matrix(adj.list)) 
	adj.list <- adjmat2list(adj.list)

all.methods <- c("lf", "sl", "dsatur", "rlf", "msc", "lmxrlf", "tabu")
if (is.null(method)) {
	method <- all.methods[1:5]
} else {
	idx <- na.omit(pmatch(method, all.methods))
	method <- all.methods[idx]	
}
if (length(method) == 0) 
	stop("No valid values in 'method'.")
out <- vector("list", length(method))
names(out) <- method
for (m in method)
	out[[m]] <- switch(m, 
		lf = graph_coloring_greedy(adj.list, "lf"),
		sl = graph_coloring_greedy(adj.list, "sl"),
		dsatur = graph_coloring_dsatur(adj.list),
		rlf = graph_coloring_rlf(adj.list),
		msc = graph_coloring_msc(adj.list),
		lmxrlf = graph_coloring_lmxrlf(adj.list),
		tabu = graph_coloring_tabucol(adj.list, ...))

out
}		

