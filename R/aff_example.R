# affiliation example

# import functions from packages or local computer
rm(list = ls())
library(stringdist)
require(quadprog)

#source("C:/Users/xinmi/GCMER/R/distance.calc.R")
source("C://Users//xinmi//GCMER//R//learn.metric.R")



data.dir <- "D://Github//Data-Analytics-Lab-Prof.Degras//EntityResolution//MoreCode//data analysis//affiliation//" #XC
aff <- read.csv(file.path(data.dir, "clean_affiliations_final.csv"))
aff[1:30,]



# wrap up
samplesize <- 100
data <- aff
method <- "lv"
iters <- 100
iter <- 1
iter <- iter+1

iter.learn.metric <- function(samplesize, data, label, method = "lv", iters = 100){
  w <- matrix(, ncol = 2, nrow = iters)
  for(iter in 1: iters){
    idx <- sample(nrow(data), samplesize)
    block <- as.vector(label[idx])
    ## Metric learning
    samples <- data[idx, -1]
    n <- samplesize
    p <- ncol(samples)
    samples[is.na(samples)] <- ""

    sim.list <- vector("list", p)

    for(i in 1:p){
      sim.list[[i]] <- stringsimmatrix(samples[, i], method = method)
    }

    sim.vec <- unlist(sim.list)
    sim.array <- array(sim.vec, dim = c(n, n, p))
    opt.metric <- learn.metric(sim.array, block)
    opt.metric2 <- learn.metric2(sim.array, block)

    w[iter,1] <- which.max(opt.metric$w)
    w[iter,2] <- which.max(opt.metric2$w)
  }
  return(w)

}

system(
  test <- iter.learn.metric(samplesize = 500, data = aff, label = label, iters = 1000)
)



# Error message:
#Error in solve.QP(V, dvec, Amat, bvec, meq = 1) :
#  matrix D in quadratic function is not positive definite!

#Possible reason: sample size is too small!!!





################
# Use 'learn.metric2' and column 'Name1' for whole affiliation data
################


system.time(
  sim.aff <- stringsimmatrix(aff$Name1, method = "lv")
)

#user  system elapsed
#4.53    0.03    1.66

str(sim.aff)

## Graph coloring
g <- sim.aff
threshold <- as.numeric(quantile(g, 0.75)) # third quantile as threshold
## Binarize graph if necessary
if (!all(g %in% c(0, 1))) g <- (g >= threshold)
g[1:10, 1:20]

diag(g) <- 0

## 4. Graph coloring
# Graph coloring on complement graph from dissimilarity matrix
# with different methods
## Convert graph to adjacency list in view of graph coloring
code.dir <- "C:/Users/xinmi/GCMER/R"#XM
source(file.path(code.dir, "graph.conversion.R"))
source(file.path(code.dir, "graph_coloring_greedy.R"))
source(file.path(code.dir, "graph_coloring_rlf.R"))
source(file.path(code.dir, "graph.coloring.R"))
source(file.path(code.dir, "helpers.R"))

adj.list <- adjmat2list(g)
edges <- which(g == 1, TRUE)
edges <- edges[edges[,1] < edges[,2]]
#gg <- make_undirected_graph(edges)

## Perform the coloring with various methods - greedy + largest first for example

coloring <- graph_coloring_greedy(adj.list, "lf")

get.chromatic(coloring)
is.valid.coloring(coloring, adjmat = g)






#####################################################################
# generate features combination choose(2:10, k), k = 2:9
idx <- t(combn(2:10,2))
comb.list <- lapply(2:9, function(k) combn(2:10, k, simplify = FALSE))
comb.list <- do.call(c, comb.list)

s.mat.aff <- vector("list", length(comb.list))

for(i in 1:length(comb.list)){
  idx <- comb.list[[i]]
  features <- apply(sample100[,idx], 1, function(row) paste(row, collapse=" "))
  s.mat.aff[[i]] <- stringsimmatrix(features, method = "lv")
}

str(s.mat.aff)
summary.sim.mat <- sapply(s.mat.aff, function(obj) c(summary(as.vector(obj[upper.tri(obj, diag = FALSE)]))))
summary.sim.mat <- t(summary.sim.mat)
dim(summary.sim.mat)

summary.sim.mat[,5]


# Blocking


get.blocks <- function(dat, features, size=0.5, thres=0.25){
  d <- d.matrix(dat = dat, features = features, size = size)
  blocks <- apply(d <= thres, 2, which)
  blen <- sapply(blocks, length)
  blocks <- blocks[blen > 0]
  blocks <- unique(blocks)
  return(blocks)
}

names(labeled.data)
#[1] "Entity"  "ID"      "Name1"   "Name2"   "Name3"   "Street1" "Street2" "City"    "State"   "Zipcode" "Country"


#Metric learning

test1 <- get.blocks(labeled.data, names(labeled.data)[3:5], 0.5, 0.25) # Name1+2+3
test2 <- get.blocks(labeled.data, names(labeled.data)[3:5], 0.5, 0.25) # Name1+2+3+street1
test3 <- get.blocks(labeled.data, names(labeled.data)[3], 0.5, 0.25)
test4 <- get.blocks(labeled.data, names(labeled.data)[4], 0.5, 0.25) #nonsense
test5 <- get.blocks(labeled.data, names(labeled.data)[5], 0.5, 0.25) #nonsense
test6 <- get.blocks(labeled.data, names(labeled.data)[6:7], 0.5, 0.25) #nonsense
test7 <- get.blocks(labeled.data, names(labeled.data)[3:7], 0.5, 0.25)
test8 <- get.blocks(labeled.data, names(labeled.data)[6:11], 0.5, 0.25) #nonsense

truth[1:10]
test1[1:10]#
test2[1:10]#
test3[1:10]
test4[1:10]
test5[1:10]
test6[1:10]
test7[1:10]#
test8[1:10]

names(data)
#[1] "ID"      "Name1"   "Name2"   "Name3"   "Street1" "Street2" "City"    "State"   "Zipcode" "Country"

# Learned metrics for full data
test01 <- get.blocks(data, names(data)[2:4], 0.5, 0.25) #Name1,2,3
test00 <- get.blocks(data, names(data)[2:5], 0.5, 0.25) #Name1,2,3 and street1
test02 <- get.blocks(data, names(data)[2], 0.5, 0.25) #Name1 #giving 924 entities***
test03 <- get.blocks(data, names(data)[2:6], 0.5, 0.25)

test01[1:5]
test00[1:5]
test02[1:5]
test03[1:5]

length(test01);length(test00);length(test02);length(test03)
sapply(test02, length)
