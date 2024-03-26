
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
  m <- if (is.null(y)) x else table(x, y) # contingency table
  n <- sum(m) # number of elements
  rsum <- rowSums(m)
  csum <- colSums(m)
  E <- tcrossprod(rsum, csum) / n # expected counts
  sum((m-E)^2 / E)
}



######################
# General rand index
######################

# Formula: R(C1, C2) = 2(n11+n00) / n(n-1)

rand.index <- function(x, y = NULL) {
  m <- if (is.null(y)) x else table(x, y)
  n <- sum(m)
  rsum <- rowSums(m)
  csum <- colSums(m)
  1 - (sum(rsum^2) + sum(csum^2) - 2*sum(m^2)) / (n*(n-1))
}



######################
# Adjusted rand index
######################

# Formula: R_adj(C1, C2) = sum_i=1^K sum_j=1^L choose(m.ij, 2) - t3
# t1 = sum choose(|C1|, 2), t2 = sum choose(|C2|, 2)
# t3 = 2*t1*t2 / n(n-1)


adj.rand.index <- function(x, y){
  m <- table(x, y)
  n <- sum(m)
  t1 <- sum(choose(x, 2))
  t2 <- sum(choose(y, 2))
  t3 <- 2*t1*t2 / (n*(n-1))
  (sum(choose(m, 2)) - t3) / (((t1 + t2) / 2) - t3)
}




#######################
# Fowlkes-Mallow Index
#######################

# Formula: FM(C1, C2) = n11 / sqrt((n11+n10)(n11+n01))

Fowlkes.Mallow.index <- function(x, y = NULL){
  m <- if (is.null(y)) x else table(x, y)
  n <- sum(m)
  n11 <- sum(choose(m,2)) # sum_{i,j} C(m_ij, 2)
  rsum <- rowSums(m)
  csum <- colSums(m)
  n01 <- sum(choose(csum, 2)) - sum(choose(m, 2))
  n10 <- sum(choose(rsum, 2)) - sum(choose(m, 2))
  n11 / sqrt((n11 + n10) * (n11 + n01))
}



#######################
# Mirkin Metric
#######################

# Formula: M(C1, C2) = sum |C1.i|^2 + sum |C2.j|^2 - 2 sum sum m.ij^2
# = 2(n01+n10) = n(n-1) (1-R(C1, C2))

Mirkin.Metric <- function(x, y = NULL){
  m <- if (is.null(y)) x else table(x, y)
  rsum <- rowSums(m)
  csum <- colSums(m)
  n01 <- sum(choose(csum, 2)) - sum(choose(m, 2))
  n10 <- sum(choose(rsum, 2)) - sum(choose(m, 2))
  2*(n10 + n01)
}



################
# Jaccard Index
################

# Formula: J(C1, C2) = n11 / (n11 + n10 + n01)

Jaccard.index <- function(x, y = NULL){
  m <- if (is.null(y)) x else table(x, y)
  n11 <- sum(choose(m,2)) # sum_{i,j} C(m_ij, 2)
  rsum <- rowSums(m)
  csum <- colSums(m)
  n01 <- sum(choose(csum, 2)) - sum(choose(m, 2))
  n10 <- sum(choose(rsum, 2)) - sum(choose(m, 2))
  n11 / (n11 + n10 + n01)
}



########################
# Partition Difference
########################

# Formula: PD(C1, C2) = n00

partition.diff <- function(x, y = NULL){
  m <- if (is.null(y)) x else table(x, y)
  n <- sum(m)
  n11 <- sum(choose(m,2)) # sum_{i,j} C(m_ij, 2)
  rsum <- rowSums(m)
  csum <- colSums(m)
  n01 <- sum(choose(csum, 2)) - sum(choose(m, 2))
  n10 <- sum(choose(rsum, 2)) - sum(choose(m, 2))
  n00 <- choose(n, 2) - (n01 + n10 + n11)
  n00
}



#####################
# True positive rate 
#####################

# Rows of 'x' (or 'x' itself if 'y' is NULL) 
# should refer to the true partition

TPR <- function(x, y = NULL) {
  m <- if (is.null(y)) x else table(x, y)
  rsum <- rowSums(m)
  TP <- sum(choose(m, 2)) # true positive (n11)
  P  <- sum(choose(rsum, 2)) # positive (n01 + n11)
  TP / P
}

######################
# False positive rate 
######################

# Rows of 'x' (or 'x' itself if 'y' is NULL) 
# should refer to the true partition

FPR <- function(x, y = NULL) {
  m <- if (is.null(y)) x else table(x, y)
  rsum <- rowSums(m)
  csum <- colSums(m)
  FP <- sum(choose(csum, 2)) - sum(choose(m, 2)) # false positive
  N  <- choose(sum(m), 2) - sum(choose(rsum, 2)) # negative
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
  m <- if (is.null(y)) x else table(x, y)
  n <- sum(m)
  rsum <- rowSums(m)
  csum <- colSums(m)
  nr <- nrow(m)
  nc <- ncol(m)
  den <- rep(rsum, nc) + rep(csum, each = nr)
  Fmat <- 2 * m / den
  sum(rsum * apply(Fmat, 1, max)) / n
}



######################
# Meila-Heckerman
######################

# paper: Meila (2001): An experimental comparison of model-based clustering methods
# Expectation-Maximization (EM) algorithm (Dempaster, Laird, Rubin, 1977)
# Classification EM (CEM) algorithm (Celeux, Govaert, 1992)
# model-based agglomerative clustering (AC) (e.g. Banfield, Raftery, 1993)


# Formula: MH(optC, C1) = (1/n) sum_1:k max_C1 \in optC m.ij

# Rows of 'x' (or 'x' itself if 'y' is NULL) 
# should refer to the true partition

Meila.Heckerman <- function(x, y = NULL) {
  m <- if (is.null(y)) x else table(x, y)
  n <- sum(m)
  rmax <- apply(m, 1, max)
  sum(rmax) / n
}


########################
# Maximum-Match-measure
########################

# Formula: MH(optC, C1) = (1/n) sum_i=1:min(k,l) m.ii'

Max.match <- function(x, y = NULL) {
  m <- if (is.null(y)) x else table(x, y)
  n <- length(x)
  out <- 0
  nr <- nrow(m)
  nc <- ncol(m)
  for (i in 1:min(nr,nc)) {
    idx <- arrayInd(which.max(m), c(nr, nc))
    out <- out + m[idx[1], idx[2]]
    m[idx[1],] <- 0
    m[,idx[2]] <- 0
  }
  out
}


######################
# Van Dongen-measure
######################

# Formula: D(C1, C2) = 2n - sum_i=1:k max_j m.ij - sum_j=1:l max_i m.ij

Van.Dongen <- function(C1, C2){
  x <- sapply(1:n, function(i) which(sapply(1:K, function(k) i %in% C1[[k]]) == TRUE))
  y <- sapply(1:n, function(i) which(sapply(1:L, function(l) i %in% C2[[l]]) == TRUE))
  m <- table(x, y)
  n <- sum(m)
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


# Function to get entropy for a clustering of set X (|X| = n)
entropy.clst <- function(clustering){
  n <- max(unlist(clustering))
  P.i <- sapply(clustering, length) / n
  H.clst <- - sum(P.i * log2(P.i))
  return(H.clst)
}

# Mutual information
# Formula: I(C1, C2) = sum_i=1:k sum_j=1:l P(i,j) log2 P(i,j) / (P(i)*P(j))
# P(i,j) = |C1i \cap C2j| / n

mutual.info <- function(C1, C2){
  H.C1 <- entropy.clst(C1)
  H.C2 <- entropy.clst(C2)

  p.i <- sapply(C1, length) / n
  p.j <- sapply(C2, length) / n

  p.ij <- I.ij <- matrix(0, nrow = length(C1), ncol = length(C2))
  for(i in 1:length(C1)){
    for(j in 1:length(C2)){
      p.ij[i,j] <- length(intersect(C1[[i]], C2[[j]])) / n
      if(p.ij[i,j] != 0){
        I.ij[i,j] <- p.ij[i,j] * log2(p.ij[i,j] / (p.i[i]* p.j[j]))
      }

    }
  }
  I <- sum(I.ij) # Mutual Information

  norm.SG <- I / sqrt(H.C1 * H.C2) #normalized mutual information by Strehl & Ghosh (2002)
  norm.FJ <- 2 * I / (H.C1 + H.C2) #normalized mutual information by Fred & Jain (2003)
  VoI <- H.C1 + H.C2 - 2 * I # Variation of Information by Meila (2003)
  return(c("Mutual.Info" = I,
                    "Norm.Strehl.Ghosh" = norm.SG,
                    "Norm.Fred.Jain" = norm.FJ,
                    "Vari.Info" = VoI))
}
