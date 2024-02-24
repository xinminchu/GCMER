# Main for Graph coloring method for Entity Resolution /Deduplication

rm(list = ls())
code.dir <- "C:/Users/xinmi/GCMER/R"#XM
source((file.path(code.dir, "jaccard.similarity.R")))
source(file.path(code.dir, "graph.conversion.R"))
source(file.path(code.dir, "helpers.R"))
source(file.path(code.dir, "blocking.R"))
source(file.path(code.dir, "graph_coloring_greedy.R"))
source(file.path(code.dir, "graph_coloring_rlf.R"))
source(file.path(code.dir, "graph.coloring.R"))

## 1. Data preprocessing (clean dataset)
#cora
#affiliation

#10Kfull synthetic dataset
data.dir <- "D://Github//Data-Analytics-Lab-Prof.Degras//EntityResolution//DataResource//10Kfull.RData"
load(data.dir)
ls() # display the data name "D10K"
n <- length(D10K) #10000

# data.dir <- "D://Github//Data-Analytics-Lab-Prof.Degras//EntityResolution//DataResource//"
# d10kfull <- read.csv(paste0(data.dir, "10Kfull.csv"))
# str(d10kfull)
# d10kfull[101,]
# d10kfull[8933,]

data.dir <- "D://Github//Data-Analytics-Lab-Prof.Degras//EntityResolution//DataResource//"
d10kdup <- read.csv(paste0(data.dir, "10Kduplicates.csv"))
d10kdup.mat <- matrix(, ncol = 2, nrow = nrow(d10kdup))
d10kdup.mat[,1] <- sapply(strsplit(d10kdup[1:nrow(d10kdup),], "|", fixed = TRUE), "[[", 1)
d10kdup.mat[,2] <- sapply(strsplit(d10kdup[1:nrow(d10kdup),], "|", fixed = TRUE), "[[", 2)

D10K[as.integer(d10kdup.mat[1,])]



len <- sapply(D10K, function(i) nchar(i))
names(len) <- NULL

D10K[which.max(len)]; max(len)
D10K[which.min(len)]; min(len)


# Remove special characters (parentheses)
D10K <- sapply(D10K, function(obj) gsub("[()]", "", obj))
names(D10K) <- NULL

## check if any record is the substring of others
sb <- matrix(, ncol = n, nrow = n)

system.time(
  for(i in 1:n){
    sb[i,] <- sapply(D10K[1:n], function(obj)
      ifelse(nchar(D10K[i]) <= nchar(obj), grepl(D10K[i], obj, ignore.case = TRUE), FALSE))
  }
)
#user  system elapsed
#890.22    1.14 2122.73

sum(sb) #=10000 means no row is included in any other rows.



## 2. Blocking
#generate k blocks
require(stringdist)

d.vec <- stringdistmatrix(D10K)
summary(d.vec)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 6.0    68.0    73.0    73.4    78.0   115.0

# stringsim computes pairwise string similarities between elements of character
# vectors a and b, where the vector with less elements is recycled. stringsimmatrix
# computes the string similarity matrix with rows according to a and columns according to b.
system.time(
  s.mat <- stringsimmatrix(D10K, method = "lv")
)
# user  system elapsed
# 1392.07    2.37  130.64
save(s.mat, file = "simMatrix.RData")
load("simMatrix.RData")


# s.triangle <- s.mat
# s.triangle[lower.tri(s.mat, diag = TRUE)] <- 0
# colnames(s.triangle) <- NULL
# hist(s.triangle[1,])
# hist(s.triangle[2,])
# hist(s.triangle[100,])
#
# par(mfrow = c(1,2))
# hist(s.triangle[1,], breaks = 20)
# hist(s.mat[1,], breaks = 20)
# par(mfrow = c(1,1))
#
# par(mfrow = c(3,3))
# for(i in 1:9){
#   hist(s.mat[i,])
# }
# par(mfrow = c(1,1))
#
# which(s.triangle[1,] >= quantile(s.triangle[1,], 0.95))
# which(s.triangle[2,] >= quantile(s.triangle[2,], 0.95))
#
# sub1 <- which(s.mat[1,] >= quantile(s.mat[1,], 0.95))
# sub2 <- which(s.mat[2,] >= quantile(s.mat[2,], 0.95))
# intersect(sub1, sub2)



D10K.letters <- gsub("[0-9]", "", D10K)
#D10K.letters <- trimws(D10K.letters)
# Count the words?
D10K.words <- get.words(D10K.letters, n)

idx <- as.vector(which(sapply(D10K, function(obj) sum(grepl(paste0("\\b", D10K.words[1], "\\b"), obj)))>0))
D10K[idx]

system.time(
  idx.list <- sapply(D10K.words, function(wd) as.vector(which(sapply(D10K, function(obj) sum(grepl(paste0("\\b", wd, "\\b"), obj)))>0)))
)

# user  system elapsed
# 1448.05    1.22 2266.25

#save(idx.list, file="wordsList.RData")
load("wordsList.RData")
idx.list[1:10]
D10K[idx.list[[3]]]


# check the words frequency (how many rows have the specific word)
idx.list.len <- sapply(idx.list, length)
words.list <- names(sort(idx.list.len, decreasing = TRUE))

match("101", idx.list)
pmatch("101", idx.list, duplicates.ok = TRUE)
which(names(idx.list.len) == D10K.words[468])
idx.list.len[468]

match("8933", idx.list)
D10K.words[15082]

idx.list[[468]]
names(idx.list[[15082]])

D10K[101]
D10K[8933]


# Blocking by words
system.time(
  blks <- blocking.by.words(D10K)
)

# user  system elapsed
# 24.27    0.03   49.67

length(blks) #4883
sapply(blks, length)
blks[[100]]

#Dmat (dissimilarity matrix) by jac.sim (start point, initialization)
#Learning metric n rows*m features -> w (metric/weights)


## 3. Dissimilarity matrix
#Generate dissimilarity matrix by w
#Obtain Binary graph

## block1
block1 <- s.mat[blks[[1]], blks[[1]]]

block1[1:20, 1:20]

g <- block1
threshold <- as.numeric(quantile(g, 0.75)) # third quantile as threshold
## Binarize graph if necessary
if (!all(g %in% c(0, 1))) g <- (g >= threshold)
g[1:10, 1:20]

diag(g) <- 0

## 4. Graph coloring
# Graph coloring on complement graph from dissimilarity matrix
# with different methods
## Convert graph to adjacency list in view of graph coloring
adj.list <- adjmat2list(g)
edges <- which(g == 1, TRUE)
edges <- edges[edges[,1] < edges[,2]]
#gg <- make_undirected_graph(edges)

## Perform the coloring with various methods - greedy + largest first for example

coloring <- graph_coloring_greedy(adj.list, "lf")

get.chromatic(coloring)
is.valid.coloring(coloring, adjmat = g)



## 5. Analysis on the graph coloring results

save.image(file = "D10K.coloring.RData")





