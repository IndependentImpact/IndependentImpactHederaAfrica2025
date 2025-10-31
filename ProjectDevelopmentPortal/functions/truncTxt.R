# Function to recursively truncate all character elements in a nested list.
truncTxt <- function(x, nChar = 70) {
  
  if (is.list(x)) {
    x <- lapply(X = x, FUN = truncTxt, nChar = nChar)
    return(x)
  }
  
  if (!is.character(x)) { return(x) }
  
  if (nchar(x) <= nChar) { return(x) }
  
  splts <- strsplit(x = x, split = "[[:blank:]]")[[1]]
  splts <- sapply(X = splts, FUN = function(y) {
    
    if (nchar(y) <= nChar) { return(y) }
    
    ys <- c()
    yn <- ceiling(nchar(y) / nChar)
    for (k in 1:yn) {
      ys <- c(ys, substr(x = y, 
                         start = (k-1)*nChar, 
                         stop = (k*nChar)-1))
    }
    y <- paste(ys, collapse = "... ")
    return(y)
    
  })
  
  x <- paste(splts, collapse = " ")
  return(x)
  
}
