
###################################
# Sampling from affiliation
###################################

# Take 100 rows randomly
#s100 <- sample(nrow(data), 100, replace = FALSE)

# set.seed(2023)
# strt <- sample(nrow(data)-101, 1)

samplesize <- 100
start <- 978
idx <- start:(start + samplesize - 1)
#name <- aff[aff$ID %in% aff[idx, 1], 2]

sampled100 <- aff[idx,]
write.csv(sampled100, file = file.path(data.dir, "sampled100.csv"))
head(sampled100, 10)


# manually label the sampled 100 rows
data.dir <- "C://Users//xinmi//GCMER//Affiliation//" #XC
mapping.aff <- read.csv(file.path(data.dir, "affiliationstrings_mapping.csv"))

label <- integer(samplesize)
count <- 1
for(i in 1:samplesize){
  if(label[i] == 0){
    label[i] <- count
    mp <- intersect(mapping.aff[mapping.aff[, 1] == sampled100[i, 1], 2], sampled100[, 1])
    idx <- which(sampled100[,1] %in% mp)
    label[idx[idx > i]] <- count
    count <- count + 1
  }else{
    i <- i + 1
  }

}
label


truth <- cbind(sampled100, label)
head(truth, 10)
write.csv(truth, file = file.path(data.dir, "labeled100.csv"))


# Label the whole affiliation dataset
label <- integer(nrow(aff))
count <- 1
for(i in 1:nrow(aff)){
  if(label[i] == 0){
    label[i] <- count
    mp <- intersect(mapping.aff[mapping.aff[, 1] == aff[i, 1], 2], aff[, 1])
    idx <- which(aff[,1] %in% mp)
    label[idx[idx > i]] <- count
    count <- count + 1
  }else{
    i <- i + 1
  }

}
length(label)

length(unique(label))
