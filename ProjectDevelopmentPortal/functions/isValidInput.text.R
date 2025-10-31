
isValidInput.text <- function(x, 
                              bRequired = TRUE, 
                              nCharMin = 0, 
                              nCharMax = 100) {
  
  if (length(x) == 0) {
    return(!bRequired)
  }
  
  if (length(x) > 1) {
    warning("length(x) > 1. Only the first element will be used.")
    x <- x[[1]]
  }
  
  if (is.na(x)) {
    return(!bRequired)
  }
  
  strippedInput <- gsub(pattern = "[^[:alnum:]]", 
                        replacement = "", 
                        ignore.case = TRUE,
                        x = x)
  
  if (nchar(strippedInput) == 0) {
    return(!bRequired)
  }
  
  if (nCharMin > 0) {
    if (nchar(strippedInput) > 0) {
      if (nchar(x) < nCharMin) {
        return(FALSE)
      }
    }
  }
  
  if (nchar(x) > nCharMax) {
    return(FALSE)
  }
  
  return(TRUE)
}