library(cora)
library(stringdist)

data(cora)

n <- nrow(cora)

names(cora)

#[1] "id"          "title"       "book_title"  "authors"     "address"     "date"        "year"        "editor"
#[9] "journal"     "volume"      "pages"       "publisher"   "institution" "type"        "tech"        "note"

# NA issue
# replace NA with empty ""
cora[is.na(cora)] <- ""

sim.cora <- vector("list", (ncol(cora)-1))

for(i in 1:(ncol(cora)-1)){
  sim.cora[[i]] <- stringsimmatrix(cora[,(i+1)], method = "lv")
}




## define blocks to speed up calculations
n1 <- 900
n2 <- n - n1
x1 <- head(cora$title, n1)
x2 <- tail(cora$title, n2)


## Levenshtein distance between strings of each block
d11 <- stringdistmatrix(x1, x1, method = "lv")
d22 <- stringdistmatrix(x2, x2, method = "lv")
d12 <- stringdistmatrix(x1, x2, method = "lv")

length1 <- nchar(x1)
length2 <- nchar(x2)

## Calculate maximal length
maxlength11 <- outer(length1, length1, "pmax")
maxlength22 <- outer(length2, length2, "pmax")
maxlength12 <- outer(length1, length2, "pmax")

## Normalize dissimilarities to [0,1]
d11 <- d11 / maxlength11
d22 <- d22 / maxlength22
d12 <- d12 / maxlength12


## Choose threshold
d11copy <- d11
d12copy <- d12
d22copy <- d22

thres <- .25

## Combine blocks
dtitle <- rbind(cbind(d11, d12), cbind(t(d12), d22))

## Do same thing with 'booktitle' and 'authors'
# ...
# dauthors

d = pmin(dtitle, dauthors, dbooktitle, na.rm = TRUE)


which(d[12,] <= thres)

blocks <- apply(d <= thres, 2, which)
blen <- sapply(blocks, length)
blocks <- blocks[blen > 0]
nblocks <- length(blocks)
for (b in 1:nblocks) {


}




dglobal <- pmin(dtitle, dbook, dauthor)

stringdist("ab", "cde")
