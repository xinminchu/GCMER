# distance calculation with selected features
d.feature <- function(dat, feature, size=0.5){
  n <- nrow(dat)
  n1 <- as.integer(n * 0.5)
  n2 <- n - n1
  x1 <- head(dat[, feature], n1)
  x2 <- tail(dat[, feature], n2)

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

  ## Combine blocks
  return(rbind(cbind(d11, d12), cbind(t(d12), d22)))
}


d.matrix <- function(dat, features, size = 0.5){
  d <- matrix(1, ncol = nrow(dat), nrow = nrow(dat))
  for(i in 1:length(features)){
    dd <- d.feature(dat = dat, feature = features[i], size = size)
    d <- pmin(d, dd, na.rm = TRUE)
  }
  return(d)
}
