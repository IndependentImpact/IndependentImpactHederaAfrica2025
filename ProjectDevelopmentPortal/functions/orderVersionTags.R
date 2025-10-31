
#'@param x Character. A vector of version tags of the MAJOR.MINOR.PATCH format.
#'@param asc Logical. Should the result be ordered ascendingly? If FALSE, the 
#'  result will be order descendingly.
#'@return Character. x, ordered.
#'
orderVersionTags <- function(x, asc = TRUE) {
  
  if (length(x) < 2) { return(x) }
  
  splts <- strsplit(x = x, split = ".", fixed = TRUE)
  
  df <- data.frame(
    x = x,
    major = as.integer(sapply(X = splts, FUN = function(y) y[1])),
    minor = as.integer(sapply(X = splts, FUN = function(y) y[2])),
    patch = as.integer(sapply(X = splts, FUN = function(y) y[3])))

  df <- df[
    order(df$major, df$minor, df$patch, decreasing = !asc),]
  
  return(df$x)
}
