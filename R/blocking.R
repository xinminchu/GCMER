
# Obtain blocks by 'stringdist' with given threshold length
# data cleaning and blocking
# input 'df' is a column with strings

blocking.stringdist <- function(df, threshold = 3L) {
  require(dplyr)
  require(stringdist)

  if(!is.null(dim(df))){ #multiple features be combined
    df <- apply(df, 1, function(obj) paste(obj, collapse = " "))
  }
  # Data cleaning
  df <- tolower(df) # lowercase
  df <- gsub("[^a-z]", "", df) # remove non-letter characters
  #df <- gsub("^\\s+|\\s+$", "", df) # trim white space

  bkv <- unique(df) #block key values

  blocks <- lapply(bkv, function(x) which(stringdist(x, df) <= threshold))

  unique(blocks)
}
