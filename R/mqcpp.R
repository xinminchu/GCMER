# ILP for QCPP
# integer linear programming for Quasi-Clique Partition Problem
# with initialization method Multi-start greedy randomized heuristic (MSH)
# Melo et al. (2022) The minimum quasi-clique partitioning problem:
# Complexity, formulations, and a computational study

# Section 3 Integer programming formulations
# 3.1 Standard formulation


mqcpp <- function(n, edges, gamma, ub = NULL)
{
  ## Preprocessing
  n <- as.integer(n)
  if (!all(edges %in% (1:n)))
    edges <- matrix(match(edges, unique(edges)), ncol = 2)
  nedges <- nrow(edges)
  stopifnot(all(edges <= n))
  stopifnot(gamma > 0 && gamma <= 1)
  if (is.null(ub))
    ub <- floor(.5 + .5 * sqrt(1 + 8 * nedges / gamma))

  ## Define objective
  nvars <- ub + (n * ub) + choose(n,2) * ub
  npairs <- choose(n,2)
  pairs <- combn(n, 2)
  # indexing: y (ub-vector), x (ub x n matrix), w (ub x npairs)
  obj <- numeric(nvars)
  obj[1:ub] <- 1

  ## Define constraints
  ncnstr <- c(n, ub * n, rep(ub * npairs, 3), ub, ub - 1)#, ub, ub * n, ub * npairs)
  csumcnstr <- cumsum(ncnstr)
  totcnstr <- tail(csumcnstr,1) #csumcnstr[10]
  A <- matrix(0, totcnstr, nvars) # each row = 1 constraint

  ## Variable indices
  idxy <- 1:ub
  idxx <- matrix(seq(ub+1, ub*(n+1)), ub, n)
  idxw <- matrix(seq(ub*(n+1)+1, nvars), ub, npairs)


  # Constraints (4) in paper sum_1:ub (x_iv) = 1
  for (v in 1:n)
    A[v, idxx[,v]] <- 1

  # Constraints (5) in paper x_iv - y_i <= 0 ## choose '<' for all inequality
  idxrow <- (csumcnstr[1]+1):csumcnstr[2]
  count <- 0
  for (i in 1:ub) {
    for (v in 1:n) {
      count <- count + 1
      A[idxrow[count], c(idxx[i,v], idxy[i])] <- c(1, -1)
    }}

  # Constraints (6) x_iu + x_iv - w_iuv <= 1
  idxrow <- (csumcnstr[2]+1):csumcnstr[3]
  count <- 0
  for (i in 1:ub) {
    for (u in 1:(n-1)){
      for (v in (u+1):n) {
        count <- count + 1
        A[idxrow[count], c(idxx[i, u], idxx[i, v], idxw[i, which(pairs[1,] == u & pairs[2, ] == v)])] <- c(1, 1, -1)
      }
    }
  }

  # Constraints (7) w_iuv <= x_iu
  idxrow <- (csumcnstr[3]+1):csumcnstr[4]
  count <- 0
  for (i in 1:ub) {
    for (u in 1:(n-1)){
      for (v in (u+1):n) {
        count <- count + 1
        A[idxrow[count], c(idxx[i, u], idxw[i, which(pairs[1,] == u & pairs[2, ] == v)])] <- c(1, -1)
      }
    }
  }

  # Constraints (8) w_iuv <= x_iv
  idxrow <- (csumcnstr[4]+1):csumcnstr[5]
  count <- 0
  for (i in 1:ub) {
    for (u in 1:(n-1)){
      for (v in (u+1):n) {
        count <- count + 1
        A[idxrow[count], c(idxx[i, v], idxw[i, which(pairs[1,] == u & pairs[2, ] == v)])] <- c(1, -1)
      }
    }
  }


  # Constraints (9) gamma * sum_{u<v} w_iuv - sum_{u<v, uv \in E} w_iuv <= 0
  idxrow <- (csumcnstr[5]+1):csumcnstr[6]
  for (i in 1:ub) {
    for (u in 1:(n-1)){
      for (v in (u+1):n) {
        A[idxrow[i], idxw[i, which(pairs[1,] == u & pairs[2, ] == v)]] <-
          ifelse(sum(edges[,1] == u & edges[,2] == v) == 1, -1, gamma)
      }
    }
  }

  # Constraints (10) y_i >= y_i+1 dual: y_i+1 - y_i <= 0
  idxrow <- (csumcnstr[6]+1):csumcnstr[7]
  for (i in 1:(ub - 1)){
    A[idxrow[i], c(idxy[i], idxy[i+1])] <- c(-1, 1)
  }




  ## Run GUROBI
  model <- list(A = A, obj = obj,
                modelsense = "min",
                vtype = rep("B", nvars),
                sense = c(rep('=', n), rep('<', (totcnstr-n))),
                rhs = rep(c(1,0,1,0,0,0,0), ncnstr)
                )
  gurobi(model)

}



##########
# Example
##########
# n <- 28
# gamma <- 0.6
# ub <- NULL
#
#
#
# library(igraph)
# g <- graph("Frucht") # The Frucht Graph is the smallest cubical graph
# #whose automorphism group consists only of the identity element.
# #It has 12 vertices and 18 edges.
# plot(g, main = "Frucht Graph")
#
# g <- graph("Coxeter") # A non-Hamiltonian cubic symmetric graph with 28 vertices and 42 edges.
# plot(g, main = "Coxeter Graph")
#
# g <- graph("Zachary") # Social network of friendships between 34 members of
# #a karate club at a US university in the 1970s.
# plot(g, main = "Zachary Graph")
#
# edges <- as_edgelist(g)
# n <- nrow(edges)
#
# system.time(
#   test <- mqcpp(n, edges, gamma = 0.4, ub = NULL)
# )
#
