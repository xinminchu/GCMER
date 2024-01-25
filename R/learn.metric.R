

##############################################
# Metric learning: function to return weights 
# such that linear combination of features
# has minimum sum of squared dissimilarities 
# within blocks
##############################################
 

# Dissimilarity matrices D(i,j), i = 1,...,n (block), j = 1, ..., p (features)
# Weights w(1), ..., w(p)

# Goal: minimize sum(i=1:n) || sum(j=1:p) w(j) D(i,j) ||_F^2
# with respect to w(1), ..., w(p)


# Inputs: 
# D:	array (n,n,p) (n = total number of records, number of features)
# block: integer vector of length n (block indicator)

# Output: 
# weights w
# matrix D' such that objective = || D' w ||^2


learn.metric <- function(D, block)
{	
nblock <- length(unique(block))
p <- dim(D)[3]
Dlist <- vector("list", nblock*p)
dim(Dlist) <- c(nblock, p)
for (i in 1:nblock) {
	idx <- which(block == i)
	for (j in 1:p) {
		mat <- D[idx,idx,j]
		Dlist[[i,j]] <- mat[lower.tri(mat)]
	}
}
Dmat <- matrix(unlist(Dlist), ncol = p)
svdDmat <- svd(Dmat)
w <- svdDmat$v[,p]
return(list(D = Dmat, w = w))
}


##########
# Variant
##########

# min || D' w ||^2  such that w >= 0 sum(w) = 1
library(quadprog)

learn.metric2 <- function(D, block)
{
  nblock <- length(unique(block))
  p <- dim(D)[3]
  Dlist <- vector("list", nblock*p)
  dim(Dlist) <- c(nblock, p)
  for (i in 1:nblock) {
    idx <- which(block == i)
    for (j in 1:p) {
      mat <- D[idx,idx,j]
      Dlist[[i,j]] <- mat[lower.tri(mat)]
    }
  }
  Dmat <- matrix(unlist(Dlist), ncol = p)
  
  
  
  # use funtion solve.QP
  # to minimize ||D'w||^2 = w'DD'w = w'Vw
  V <- crossprod(Dmat)
  Amat <- cbind(rep(1,p), diag(1,p)) 
  # sum weights = 1, all weights >= 0
  dvec <- rep(0, p)
  bvec <- c(1, rep(0,p))
  
  sol <- solve.QP(V, dvec, Amat, bvec, meq=1)
  w <- sol$solution
  
  return(list(D = Dmat, w = w))
  
}


# A^T
# 1 1 ... 1 
# 1 0 ... 0
# 0 1 0 ... 0
# ... 
# 0 ... 0 1

##########
# Example
##########


n <- 100
p <- 5

## Generate dissimilarity matrices
D <- array(dim = c(n,n,p))
for (j in 1:p) {
	x <- matrix(runif(10 * n), n, 10)
	D[,,j] <- as.matrix(dist(x))
}

## Generate blocks
nblock <- 10
block <- sample(nblock, n, TRUE)

## Test
test1 <- learn.metric(D, block)
test2 <- learn.metric2(D, block)

## What of some weights are negative? 

library(quadprog)
# Use function solve.QP
# Problem is to minimize || Dnew w ||^2 
# subject to w(j) >= 0 for all j and sum(j=1:p) w(j) = 1 
Dnew1 <- test1$D
Dnew1

Dnew2 <- test2$D
Dnew2


