

#####################################
# function to obtain Jaccard similarity
# matrix based on shingles
#####################################

jac.sim <- function(data){ # input 'data' is nx1 text data frame
  test <- require(textreuse) & require(tokenizers)
  if (!test) {
    install.packages("textreuse")
    library("textreuse") # text reuse/document similarity

    install.packages("tokenizers")
    library("tokenizers")# shingles
  }
  n <- length(data)
  shingles <- lapply(data, function(x) tokenize_character_shingles(data, n = 3)[[1]])

  jaccard <- expand.grid(record1 = seq_len(n),#empty holder for similarities
                         record2 = seq_len(n))
  jaccard <- jaccard[jaccard$record1 < jaccard$record2,]

  jaccard$similarity <- apply(jaccard, 1, function(pair){
    jaccard_similarity(shingles[[pair[1]]], shingles[[pair[2]]])
  })

  return(jaccard)
}
