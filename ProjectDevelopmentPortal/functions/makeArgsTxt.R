makeArgsTxt <- function(lsX, xNm) {
  
  txtArgs <- ""
  
  if (length(lsX) == 0) { 
    return(txtArgs)
  }
  
  txtArgs <- c(
    txtArgs,
    sapply(X = 1:length(lsX), FUN = function(k) {
      
      nm <- names(lsX)[k]
      if (length(nm) > 0) {
        if (nchar(nm) > 0) {
          return(sprintf("%s = %s$%s", nm, xNm, nm))
        }
      }
      
      return(sprintf("%s[[%d]]", xNm, k))
      
    }))
  
  txtArgs <- paste(txtArgs, collapse = ", ")
  
  return(txtArgs)
}