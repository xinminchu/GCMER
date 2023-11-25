# Graph clustering and minimum cut trees
# Flake et al. (2004)

####################################
# Min Cut Clustering Algorithm
# with Gomory-Hu tree
####################################

# generate a E.R. graph
g <- make.er.graph(n = n, p = 0.5)
g


mincut.clustering <- function(g, alpha=0.5){#input a symmetric matrix
  n <- nrow(g)
  
  if(isSymmetric(g)){
    edges <- which(g == 1, TRUE)
    edges <- edges[edges[,1] < edges[,2],]
    g <- cbind(edges, rep(1, nrow(edges))) # generate 3-column arcs (edge + weight)
  }
  
  
  # Insert an artificial node "t = number of nodes +1"
  v <- 1:(n + 1)
  
  # Connect with all nodes with same weight "alpha"
  exp.g <- rbind(g, matrix(c(rep(n+1, n), 1:n, rep(alpha, n)), ncol = 3))
  
  
  # download packages optrees_1.0.tar.gz to local directory
  # change the working directory to the directory above
  test <- require(optrees)
  if (!test) {
    setwd("D:/Github/Data-Analytics-Lab-Prof.Degras/EntityResolution/Software/")
    install.packages("optrees_1.0.tar.gz", type = "source", repos =  NULL)
    
    library(optrees)
  }
  
  # Obtain Gomory-Hu tree through package 'optrees'
  ghTree <- ghTreeGusfield(v, exp.g)
  
  # remove node 't'?
  
  #getMinimumCutTree(v, exp.g)
  
  # return all connected components as the clusters of G?
  
  getClusters(ghTree)
  
  
}


# Obtain the clusters from Gomory-Hu cut tree
getClusters <- function(ghTree){
  nodes <- ghTree$tree.nodes
  res.arcs <- ghTree$tree.arcs[,1:2]
  
  min.cut.nodes <- ghTree$tree.arcs[idx,1:2]
  min.cut <- res.arcs[-idx,]
  
  part1 <- unique(c(setdiff(min.cut[res.arcs == min.cut.nodes[1]], min.cut.nodes[2]), min.cut.nodes[1]))
  part2 <- unique(c(setdiff(min.cut[res.arcs == min.cut.nodes[2]], min.cut.nodes[1]), min.cut.nodes[2]))
  
  if(sum(res.arcs == sort(part1)) == 2){
    part1 <- sort(part1)
    part2 <- sort(setdiff(nodes, part1))
  }else{
    part1 <- setdiff(nodes, part2)
    part2 <- sort(part2)
  }
  return(list("cluster1"=part1, "cluster2"=part2))
}



