###########################################
# MAIN FILE FOR EXAMPLES OF GRAPH COLORING
###########################################

# R package for entity resolution with data 'cora'

#the package should contain not only graph coloring functions, but also
#convenience functions to threshold dissimilarity matrices, form the
#complement of a graph, convert a type of graph representation to another
#(adjacency matrix, 3 columns, list of neighbors),
#and also functions to measure the agreement between two clusterings.



rm(list = ls())


###################
# Install packages
###################


## Test if the package 'graphcoloring' is installed
## If not, install and load it
test <- require(graphcoloring)
if (!test) {
  #path <- file.path("~/Downloads", "graphcoloring") # change path as needed
  #library(Rcpp)
  #compileAttributes()
  #setwd("~/GitHub/Data-Analytics-Lab-Prof.Degras/EntityResolution/Software")
  setwd("D:\\Github\\Data-Analytics-Lab-Prof.Degras\\EntityResolution\\Software")
  install.packages("graphcoloring", type = "source", repos = NULL)
  library(graphcoloring)
}

library(igraph)
library(microbenchmark) # for speed comparisons

require("textreuse") # text reuse/document similarity
require("tokenizers")# shingles



##############################
# Define additional functions
##############################


#code.dir <- "~/Documents/GitHub/Data-Analytics-Lab-Prof.Degras/EntityResolution/graph.coloring.dedup"#DD
code.dir <- "D:\\Github\\graph.coloring.dedup"#XM
source(file.path(code.dir, "graph_coloring_greedy.R"))
source(file.path(code.dir, "graph_coloring_rlf.R"))
source(file.path(code.dir, "graph.coloring.R"))
source(file.path(code.dir, "helpers.R"))
source(file.path(code.dir, "clustering.measures.R"))
source(file.path(code.dir, "comparing.clusterings.R"))



##################
# Import data
##################

setwd("D:/Github/graph.coloring.dedup/")
source("R/distance.calc.R")
#source("R/graph.representation.R")


library(cora)
library(stringdist)



data(cora)

system.time(d.cora <- d.matrix(dat = cora, features = names(cora)[2:5], size = 0.5))


thres <- 0.25

cora[which(d.cora[12,] <= thres), ]


blocks <- apply(d.cora <= thres, 2, which)
blen <- sapply(blocks, length)
blocks <- blocks[blen > 0]

blocks <- blocks[blen > 1]
blocks <- unique(blocks)

nblocks <- length(blocks)


for (b in 1:nblocks) {
  b <- 1
  idx <- unlist(blocks[b])
  sim.mat <- d.cora[idx, idx]

  comp.mat <- !(sim.mat > thres)
  adj.list <- adj_matrix_to_neighborhood_list(comp.mat)
  graph.coloring(adj.list)

}




###################################
# Similarity matrix for each block
###################################






###################################
# Graph coloring on CSMs
# (Complementary Similarity Matrix)
###################################







