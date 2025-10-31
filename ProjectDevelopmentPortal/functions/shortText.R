shortText <- function(x, lim = 60) {
  
  if (nchar(x) <= lim) { return(x) }
  
  nToRm <- nchar(x) - (lim -3) # -3 for '...'
  mid <- floor(nchar(x)/2)
  
  head <- substr(x = x, start = 1, stop = (mid - ceiling(nToRm/2)))
  tail <- substr(x = x, start = (mid +1 + ceiling(nToRm/2)), stop = nchar(x))
  res <- sprintf("%s...%s", head, tail)

  return(res)
}

# x <- sample(x = letters, size = 120, replace = TRUE)
# x <- paste(x, sep = "", collapse = "")
