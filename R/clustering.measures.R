
#####################################
### Measures based on Counting Pairs
#####################################

# Contingency table of two clusterings
#m <- table(part.C1, part.C2)

#########################
# Chi squared Coefficient
#########################

# Formula: chi(C1, C2) = sum_i=1:k sum_j=1:l (m.ij - E.ij)^2 / E.ij
# E.ij = |C1i||C2j| / n

# Inputs 'x' and 'y' should be (integer or character) vectors
# indicating cluster membership
# OR 'x' can be a contingency table with 'y' being NULL

chi2.coeff <- function(x, y = NULL) {
  tab <- if (is.null(y)) x else table(x, y) # contingency table
  n <- sum(tab) # number of elements
  rsum <- rowSums(tab)
  csum <- colSums(tab)
  E <- tcrossprod(rsum, csum) / n # expected counts
  sum((m-E)^2 / E)
}



######################
# General Rand index
######################

# Formula: R(C1, C2) = 2(n11+n00) / n(n-1)

rand.index <- function(x, y = NULL) {
  tab <- if (is.null(y)) x else table(x, y)
  n <- sum(tab)
  rsum <- rowSums(tab)
  csum <- colSums(tab)
  1 - (sum(rsum^2) + sum(csum^2) - 2*sum(tab^2)) / (n*(n-1))
}



######################
# Adjusted rand index
######################

# Formula: R_adj(C1, C2) = sum_i=1^K sum_j=1^L choose(tab.ij, 2) - t3
# t1 = sum choose(|C1|, 2), t2 = sum choose(|C2|, 2)
# t3 = 2*t1*t2 / n(n-1)


adj.rand.index <- function(x, y = NULL) {
  tab <- if (is.null(y)) x else table(x, y)
  n <- sum(tab)
  rsum <- rowSums(tab)
  csum <- colSums(tab)
  t1 <- sum(choose(rsum, 2))
  t2 <- sum(choose(csum, 2))
  t3 <- t1 * t2 / choose(n, 2)
  num <- sum(choose(tab, 2)) - t3
  den <- (t1 + t2) / 2 - t3
  num / den
}




#######################
# Fowlkes-Mallow Index
#######################

# Formula: FM(C1, C2) = n11 / sqrt((n11+n10)(n11+n01))

Fowlkes.Mallow.index <- function(x, y = NULL) {
  tab <- if (is.null(y)) x else table(x, y)
  n <- sum(tab)
  n11 <- sum(choose(tab,2)) # sum_{i,j} C(m_ij, 2)
  rsum <- rowSums(tab)
  csum <- colSums(tab)
  n01 <- sum(choose(csum, 2)) - sum(choose(tab, 2))
  n10 <- sum(choose(rsum, 2)) - sum(choose(tab, 2))
  n11 / sqrt((n11 + n10) * (n11 + n01))
}



#######################
# Mirkin Metric
#######################

# Formula: M(C1, C2) = sum |C1.i|^2 + sum |C2.j|^2 - 2 sum sum m.ij^2
# = 2(n01+n10) = n(n-1) (1-R(C1, C2))

Mirkin.Metric <- function(x, y = NULL){
  tab <- if (is.null(y)) x else table(x, y)
  rsum <- rowSums(tab)
  csum <- colSums(tab)
  n01 <- sum(choose(csum, 2)) - sum(choose(tab, 2))
  n10 <- sum(choose(rsum, 2)) - sum(choose(tab, 2))
  2*(n10 + n01)
}



################
# Jaccard Index
################

# Formula: J(C1, C2) = n11 / (n11 + n10 + n01)

Jaccard.index <- function(x, y = NULL) {
  tab <- if (is.null(y)) x else table(x, y)
  n11 <- sum(choose(tab,2)) # sum_{i,j} C(m_ij, 2)
  rsum <- rowSums(tab)
  csum <- colSums(tab)
  n01 <- sum(choose(csum, 2)) - sum(choose(tab, 2))
  n10 <- sum(choose(rsum, 2)) - sum(choose(tab, 2))
  n11 / (n11 + n10 + n01)
}



########################
# Partition Difference
########################

# Formula: PD(C1, C2) = n00

partition.diff <- function(x, y = NULL) {
  tab  <- if (is.null(y)) x else table(x, y)
  n    <- sum(tab)
  n11  <- sum(choose(tab,2)) # sum_{i,j} C(m_ij, 2)
  rsum <- rowSums(tab)
  csum <- colSums(tab)
  n01  <- sum(choose(csum, 2)) - sum(choose(tab, 2))
  n10  <- sum(choose(rsum, 2)) - sum(choose(tab, 2))
  n00  <- choose(n, 2) - (n01 + n10 + n11)
  n00
}



#####################
# True positive rate 
#####################

# Rows of 'x' (or 'x' itself if 'y' is NULL) 
# should refer to the true partition

TPR <- function(x, y = NULL) {
  tab <- if (is.null(y)) x else table(x, y)
  rsum <- rowSums(tab)
  TP <- sum(choose(tab, 2)) # true positive (n11)
  P  <- sum(choose(rsum, 2)) # positive (n01 + n11)
  TP / P
}

######################
# False positive rate 
######################

# Rows of 'x' (or 'x' itself if 'y' is NULL) 
# should refer to the true partition

FPR <- function(x, y = NULL) {
  tab <- if (is.null(y)) x else table(x, y)
  rsum <- rowSums(tab)
  csum <- colSums(tab)
  FP <- sum(choose(csum, 2)) - sum(choose(tab, 2)) # false positive
  N  <- choose(sum(tab), 2) - sum(choose(rsum, 2)) # negative
  FP / N
}


###################################
### Measures based on Set Overlaps
###################################

#######################
# F measures
#######################

# F(C1, C2) = F(C2) = (1/n) sum_1:K ni max_1:L F(C1i, C2j)
# F(C1i, C2j) = 2 * r.ij * p.ij / (r.ij + p.ij) = 2|C1i||C2j| / (|C1i| + |C2j|) #this is wrong
# F(C1i, C2j) = 2 * r.ij * p.ij / (r.ij + p.ij) = 2 m / (|C1i| + |C2j|)


F.measure <- function(x, y = NULL) {
  tab <- if (is.null(y)) x else table(x, y)
  n <- sum(tab)
  rsum <- rowSums(tab)
  csum <- colSums(tab)
  nr <- nrow(tab)
  nc <- ncol(tab)
  den <- rep(rsum, nc) + rep(csum, each = nr)
  Fmat <- 2 * m / den
  sum(rsum * apply(Fmat, 1, max)) / n
}



######################
# Meila-Heckerman
######################

# paper: Meila (2001): An experimental comparison of model-based clustering methods
# Expectation-Maximization (EM) algorithm (Dempster, Laird, Rubin, 1977)
# Classification EM (CEM) algorithm (Celeux, Govaert, 1992)
# model-based agglomerative clustering (AC) (e.g. Banfield, Raftery, 1993)


# Formula: MH(optC, C1) = (1/n) sum_1:k max_C1 \in optC m.ij

# Rows of 'x' (or 'x' itself if 'y' is NULL) 
# should refer to the true partition

Meila.Heckerman <- function(x, y = NULL) {
  tab <- if (is.null(y)) x else table(x, y)
  n <- sum(tab)
  rmax <- apply(tab, 2, max)
  sum(rmax) / n
}


########################
# Maximum-Match-measure
########################

# Formula: MH(optC, C1) = (1/n) sum_i=1:min(k,l) m.ii'

Max.match <- function(x, y = NULL) {
  tab <- if (is.null(y)) x else table(x, y)
  n <- length(x)
  out <- 0
  nr <- nrow(tab)
  nc <- ncol(tab)
  for (i in 1:min(nr,nc)) {
    idx <- arrayInd(which.max(tab), c(nr, nc))
    out <- out + tab[idx[1], idx[2]]
    tab[idx[1],] <- 0
    tab[,idx[2]] <- 0
  }
  out
}


######################
# Van Dongen-measure
######################

# Formula: D(C1, C2) = 2n - sum_i=1:k max_j m.ij - sum_j=1:l max_i m.ij

Van.Dongen <- function(C1, C2) {
  x <- sapply(1:n, function(i) which(sapply(1:K, function(k) i %in% C1[[k]]) == TRUE))
  y <- sapply(1:n, function(i) which(sapply(1:L, function(l) i %in% C2[[l]]) == TRUE))
  m <- table(x, y)
  n <- sum(tab)
  2 * n - sum(apply(m, 2, max)) - sum(apply(m, 1, max))
}



#########################################
### Measures based on Mutual Information
#########################################

# Entropy associated with clustering
# Formula: H(C) = - sum_i=1:k P(i) log_2 P(i)

# assuming all elements of X have the same probability of being picked
# choosing an element of X at random p = 1/n
# the probability that this element is in cluster Ci \in C is
# P(i) = |Ci| / n


# Mutual information
# Formula: I(C1, C2) = sum_i=1:k sum_j=1:l P(i,j) log2 P(i,j) / (P(i)*P(j))
# P(i,j) = |C1i \cap C2j| / n

mutual.info <- function(x, y = NULL) {
  tab <- if (is.null(y)) x else table(x, y)
  n <- sum(tab)  
  px <- rowSums(tab) / n 
  py <- colSums(tab) / n
  if (any(px == 0) || any(py == 0)) {
  	tab <- tab[px > 0, py > 0, drop = FALSE]
  	px <- px[px > 0]
  	py <- py[py > 0]
  }
  Hx <- - sum(px * log2(px)) 
  Hy <- - sum(py * log2(py))
  pxy <- tab / n 
  I <- sum(pxy * log2(pxy / tcrossprod(px, py))) # mutual information (MI)
  SG <- I / sqrt(Hx * Hy) #normalized MI by Strehl & Ghosh (2002)
  FJ <- 2 * I / (Hx + Hy) #normalized MI by Fred & Jain (2003)
  VI <- Hx + Hy - 2 * I # Variation of Information by Meila (2003)
  c(MI = I, G = SG, FJ = FJ, VI = VI)
}
