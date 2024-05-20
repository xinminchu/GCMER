find.connected.components <- function(x)
{
stopifnot(is.matrix(x))
stopifnot(nrow(x) == ncol(x))
stopifnot(all(x == t(x)))
n <- ncol(x)
if (!is.logical(x)) x <- matrix(as.logical(x), n, n)
if (any(diag(x))) diag(x) <- FALSE
comps <- integer(n)
unmarked <- rep(TRUE, n)
ncomps <- 0
while (any(comps == 0)) {
	ncomps <- ncomps + 1
	i <- which(unmarked)[1]
	comps[i] <- ncomps
	unmarked[i] <- FALSE	
	pool <- x[,i] & unmarked
	while (any(pool)) {	
		comps[pool] <- ncomps		
		i <- which(pool)[1]
		unmarked[i] <- FALSE
		pool <- (pool | x[,i]) & unmarked
	}
}
comps <- match(comps, unique(comps))
comps
}