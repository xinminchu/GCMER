
############################################
# Wrapper function for comparing clusterings 
############################################

comparing.clusterings <- function(x, y, method = NULL){
  ## Argument checks
  
  m <- table(x, y)
  n <- sum(m)
  
  all.methods <- c("chi2.coeff", "rand.index", "adj.rand.index", 
                   "Fowlks.Mallow", "Mirkin.Metric", 
                   "Jaccard", "partition.diff", 
                   "F.measure", "F.measure2",
                   "Meila.Heckerman", "Max.match"#, "Van.Dongen", "mutual.info"
  )
  if (is.null(method)) {
    method <- c("rand.index", 
                "Fowlks.Mallow", 
                "Jaccard", 
                "F.measure"#, "mutual.info"
    )
  } else {
    idx <- na.omit(pmatch(method, all.methods))
    method <- all.methods[idx]	
  }
  if (length(method) == 0) 
    stop("No valid values in 'method'.")
  out <- vector("list", length(method))
  names(out) <- method
  for (mth in method)
    out[[mth]] <- switch(mth,
                         chi2.coeff = chi2.coeff(x, y),
                         rand.index = rand.index(x, y),
                         adj.rand.index = adj.rand.index(x, y),
                         Fowlks.Mallow = Fowlks.Mallow.index(x, y),
                         Mirkin.Metric = Mirkin.Metric(x, y),
                         Jaccard = Jaccard.index(x, y),
                         partition.diff = partition.diff(x, y),
                         F.measure = F.measure(x, y),
                         Meila.Heckerman = Meila.Heckerman(x, y),
                         Max.match = Max.match(x, y)#,
                         #Van.Dongen = Van.Dongen(C1, C2),
                         #mutual.info = mutual.info(C1, C2)
    )
  
  out
}		