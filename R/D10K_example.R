#synthetic dataset example
# D10Kfull.csv

# 10Kfull synthetic dataset
data.dir <- "D://Github//Data-Analytics-Lab-Prof.Degras//EntityResolution//DataResource//10Kfull.RData" #XC
load(data.dir)
ls() # display the data name "D10K"
n <- length(D10K) #10000

# Truth
data.dir <- "D://Github//Data-Analytics-Lab-Prof.Degras//EntityResolution//DataResource//" #XC
d10kdup <- read.csv(paste0(data.dir, "10Kduplicates.csv"))
d10kdup.mat <- matrix(, ncol = 2, nrow = nrow(d10kdup))
d10kdup.mat[,1] <- sapply(strsplit(d10kdup[1:nrow(d10kdup),], "|", fixed = TRUE), "[[", 1)
d10kdup.mat[,2] <- sapply(strsplit(d10kdup[1:nrow(d10kdup),], "|", fixed = TRUE), "[[", 2)

nrow(d10kdup) #8705 matches

# look at the true match
D10K[as.integer(d10kdup.mat[1,])]
D10K[as.integer(d10kdup.mat[100,])]
D10K[as.integer(tail(d10kdup.mat,1))]


#generate k blocks
require(stringdist)

# stringsim computes pairwise string similarities between elements of character
# vectors a and b, where the vector with less elements is recycled. stringsimmatrix
# computes the string similarity matrix with rows according to a and columns according to b.

# system.time(
#   s.mat <- stringsimmatrix(D10K, method = "lv")
# )

# user  system elapsed
# 1392.07    2.37  130.64

#save(s.mat, file = "simMatrix.RData")


load("simMatrix.RData")
str(s.mat)
s.mat[1:10,1:10]

thres <- as.vector(apply(s.mat, 1, quantile, probs = 0.90))
blocks <- sapply(1:nrow(s.mat), function(row) which(s.mat[row, ] >= thres[row]))

intersect(which(s.mat[1,] > thres[1]), which(s.mat[2,] > thres[2]))

# if we want to have k blocks, randomly partition 1:n to k parts
# set intersection with each part to get a block


idx <- sample(n, 2);idx

length(blocks[[idx[1]]]);length(blocks[[idx[2]]])
length(intersect(blocks[[idx[1]]], blocks[[idx[2]]]))




