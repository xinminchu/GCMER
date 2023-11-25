# data cora
# for blocking
rm(list = ls())
library(cora)
library(stringdist)

data(cora)

cora_gold
# This data set includes the matched record pairs based on ID from the CORA
# data set. This data set can be used to evaluate the performance of record
# linkage methods performed on the CORA data set.
head(cora_gold, 30)

names(cora)
#[1] "id"          "title"       "book_title"  "authors"     "address"     "date"        "year"        "editor"
#[9] "journal"     "volume"      "pages"       "publisher"   "institution" "type"        "tech"        "note"

source("R/distance.calc.R")

system.time(d.cora <- d.matrix(dat = cora, features = names(cora)[2:5], size = 0.5))


thres <- 0.25

cora[which(d.cora[12,] <= thres), ]


blocks <- apply(d.cora <= thres, 2, which)
blen <- sapply(blocks, length)
blocks <- blocks[blen > 0]
nblocks <- length(blocks)

blocks[1:10]
for (b in 1:nblocks) {


}




dglobal <- pmin(dtitle, dbook, dauthor)

stringdist("ab", "cde")


# package 'cd'
# Duplicated music data (pre-processed and formatted) for entity resolution.
#The total size of the data set is 9763
install.packages("cd")
library(cd)
str(cd)
dim(cd)
names(cd)
#[1] "pk"       "id"       "artist"   "title"    "category" "genre"    "cdextra"  "year"     "track01"  "track02"

cd_gold


# package 'representr'
# Create Representative Records After Entity Resolution

install.packages("representr")
library(representr)
