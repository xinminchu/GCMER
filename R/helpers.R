

# Function to create confusion matrix from two 
# integer vectors representing clusterings or 
# from their contingency table

get.confusion <- function(x, y = NULL) {
	contingency <- if (is.null(y)) x else table(x,y)
	n <- sum(contingency)
	n11 <- sum(choose(contingency,2))
	n01 <- sum(choose(colSums(contingency),2)) - n11
	n10 <- sum(choose(rowSums(contingency),2)) - n11
	n00 <- choose(n,2) - n01 - n10 - n11
	confusion <- matrix(c(n00,n10,n01,n11), 2, 2)
	confusion
}



#######################################
# Function to retrieve entity/clusters
# from assignment vector
#######################################


get.entity <- function(color.vector)
{
  split(seq_along(color.vector), color.vector)
}



##########################################
# Function to generate adjacency matrix
# for weighted or unweighted random graph
##########################################


make.adj.mat <- function(n, weight = c("none", "random")){
  adj.mat <- matrix(0, n, n) # create an adjacency matrix
  idx <- sample(1:(n^2), 2 * n) # randomly create edges
  if(weight == "random"){
    weight <- runif(2*n)
    adj.mat[idx] <- weight
  }else{
    adj.mat[idx] <- 1
  }
  diag(adj.mat) <- 0
  adj.mat <- adj.mat + t(adj.mat) # make sure it's symmetric
  adj.mat[adj.mat > 1] <- 1
  return(adj.mat)
}



########################################
# Function to generate adjacency matrix
# of random Erdos-Renyi graph
########################################


make.er.graph <- function(n, p = 0.5) {
if (n == 1)
	return(matrix(0, 1, 1))
adj.mat <- matrix(0L, n, n)
lower <- which(lower.tri(adj.mat))
edge <- as.logical(rbinom(choose(n,2), 1, p))
adj.mat[lower[edge]] <- 1L
adj.mat + t(adj.mat)
}


####################################
# Function to generate random graph
# with given chromatic number
####################################

make.graph.chrom <- function(n, k, p = 0.5)
{
color <- sample.int(k, n, TRUE)
sets <- split(1:n, color) # independent sets
adj.mat <- make.er.graph(n, p)
for (i in 1:k)
	adj.mat[sets[[i]], sets[[i]]] <- 0L
idx <- t(combn(sapply(sets, "[", 1), 2))
adj.mat[idx] <- 1L
adj.mat[idx[,2:1]] <- 1L
list(graph = adj.mat, k = k, sets = sets)
}



###################################
# Function to get number of colors
# in graph coloring
###################################

get.chromatic <- function(color) length(unique(color))


###################################
# Function to check validity of a
# graph coloring
###################################


is.valid.coloring <- function(adjmat, color)
{
stopifnot(is.list(adjmat) || is.matrix(adjmat))
n <- length(color)
if (is.list(adjmat)) {
	stopifnot(length(adjmat) == length(color))
	for (i in 1:n) {
		if (color[i] %in% color[adjmat[[i]]]) return(FALSE)
	}
	return(TRUE)
}
stopifnot(ncol(adjmat) == length(color))
if (!is.logical(adjmat)) adjmat <- (adjmat == 1)
for (i in 1:n) {
	neighbors <- which(adjmat[,i])
	if (color[i] %in% color[neighbors]) return(FALSE)
}
return(TRUE)
}


#######################################
# Function fill upper triangle of an
# n x n matrix with vector elements
#######################################

fill.upper.triangle <- function(vec, n) {
  # Ensure the length of the vector matches n choose 2
  if (length(vec) != choose(n, 2)) {
    stop("Vector length does not match n choose 2 for the given n.")
  }

  # Create an n x n matrix filled with zeros
  mat <- matrix(0, nrow = n, ncol = n)

  # Counter for the vector's elements
  vec_counter <- 1

  # Iterate over the matrix to fill the upper triangle
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      mat[i, j] <- vec[vec_counter]
      vec_counter <- vec_counter + 1
    }
  }

  return(mat)
}


# # count the words
#
#
#remove no-real-sense words (prep, art, conj, adv, pron, ...?)
# how to get all those enumerated? impossible.
# it depends on data (need to learn and auto-generate it for each dataset?)

usefulwords <- function(sentence){
  # List of common prepositions in English
  prepositions <- c("about", "above", "across", "after", "against", "along",
                    "amid", "among", "around", "as", "at", "before", "behind",
                    "below", "beneath", "beside", "between", "beyond", "but",
                    "by", "concerning", "considering", "despite", "down", "during",
                    "except", "for", "from", "in", "inside", "into", "like", "near",
                    "of", "off", "on", "onto", "out", "outside", "over", "past",
                    "regarding", "round", "since", "through", "to", "toward",
                    "under", "underneath", "until", "unto", "up", "upon",
                    "via", "vs", "with", "within", "without")

  # List of common articles in English
  articles <- c("a", "an", "the")

  # List of common conjunctions in English
  conjunctions <- c("and", "but", "or", "nor", "for", "so", "yet")

  # List of common adverbs in English
  adverbs <- c("quickly", "slowly", "suddenly", "always", "never", "often", "rarely",
               "sometimes", "generally", "now", "today", "tomorrow", "yesterday",
               "soon", "already", "almost", "very", "too", "quite", "well", "badly",
               "hard", "late", "early", "everywhere", "here", "there", "everywhere",
               "everywhen", "carefully", "correctly", "directly", "loudly", "quietly",
               "efficiently", "happily", "accurately", "angrily", "seriously", "eagerly")


  # List of common pronouns in English
  pronouns <- c("I", "you", "he", "she", "it", "we", "you", "they", "myself",
                "yourself", "himself", "herself", "itself", "ourselves",
                "yourselves", "themselves", "mine", "yours", "his", "hers",
                "its", "ours", "yours", "theirs", "this", "that", "these",
                "those", "who", "whom", "whose", "which", "what", "when",
                "where", "why", "how")

  # Add any words that you consider as useless in your program
  others <- c()

  # Split the sentence into words
  words <- unlist(strsplit(sentence, "\\s+"))

  # Filter out prepositions, articles, and conjunctions
  filtered_words <- words[!(words %in% c(prepositions, articles, conjunctions, adverbs, pronouns, others))]

  filtered_words
}


get.words <- function(df, num = NULL){
  if(is.null(num)){
    num <- as.integer(0.5 * length(df))
  }
  df <- strsplit(tolower(df), "\\s+")
  bkv <- usefulwords(unlist(df))

  return(unique(bkv))
}

blocking.by.words <- function(df, num = NULL){
  if(is.null(num)){
    num <- as.integer(0.5 * length(df))
  }
  df <- strsplit(tolower(df), "\\s+")
  bkv <- usefulwords(unlist(df))
  bkv <- names(sort(table(bkv), decreasing = TRUE)[1:num])
  #bkv

  blks.words <- vector("list", length(bkv))
  names(blks.words) <- bkv
  for(i in 1:length(bkv)){
    blks.words[[i]] <- which(sapply(df, function(obj) bkv[i] %in% obj))
  }

  unique(blks.words)
}

