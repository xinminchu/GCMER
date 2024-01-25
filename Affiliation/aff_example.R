# affiliation example

rm(list = ls())
library(stringdist)

data.dir <- "D:/GitHub/Data-Analytics-Lab-Prof.Degras/EntityResolution/MoreCode/data analysis/affiliation/" # for Xinmin
data <- read.csv(file.path(data.dir, "new.clean_affiliations.csv"))
data[1:30,]

source("C:/Users/xinmi/GCMER/R/distance.calc.R")

## Metric learning
# Take 100 rows randomly
#s100 <- sample(nrow(data), 100, replace = FALSE)

# set.seed(2023)
# strt <- sample(nrow(data)-101, 1)

strt <- 978
idx <- strt:(strt+100)
name <- data[data$ID %in% data[idx, 1], 2]

data[idx,]
write.csv(data[idx,], file = file.path(data.dir, "sample100.csv"))
head(data[idx,], 10)

# manually label the sampled 100 rows

labeled.data <- read.csv(file.path(data.dir, "labeled.sample100.csv"))
labeled.data[,"Entity"]
head(labeled.data, 10)

truth <- list(vector("list", max(labeled.data$Entity)))
for(i in 1:max(labeled.data$Entity)){
  truth[[i]] <- which(labeled.data$Entity == i)
}


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
