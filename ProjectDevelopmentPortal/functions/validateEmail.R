
validateEmail <- function(x) {
  
  if (!isValidInput.text(x = x, 
                         bRequired = TRUE, 
                         nCharMin = 5, 
                         nCharMax = 50)) {
    return(FALSE)
  }
  
  if (length(
    grep(
      pattern = "[[:print:]]{1,}@[[:print:]]{2,}", 
      x = x, 
      fixed = FALSE)) != 1) {
    return(FALSE)
  }
  
  # Validate the local and domain parts of the address separately.
  splts <- strsplit(x = x, 
                    split = "@", 
                    fixed = TRUE)[[1]]
  
  # Validate the local (prefix) part.
  if (length(
    grep(
      pattern = "^[[:alnum:]]{1}[A-Za-z0-9._-]{1,}[[:alnum:]]{1}$", 
      x = splts[1],
      fixed = FALSE)) != 1) {
    return(FALSE)
  }
  
  # Validate the domain part.
  {
    if (length(grep(pattern = ".", x = splts[2], fixed = TRUE)) != 1) {
      return(FALSE)
    }
    
    # Validate the parts before and after the '.' separately.
    splts <- strsplit(x = splts[2], split = ".", fixed = TRUE)[[1]]
    if (length(splts) == 1) {
      return(FALSE)
    } 
    
    if (nchar(splts[1]) == 0 | nchar(splts[2]) == 0) {
      return(FALSE)
    }
    
    if (length(
      grep(
        pattern = "^[[:alnum:]]{1}[A-Za-z0-9._-]{0,}[[:alnum:]]{1}$", 
        x = splts[1], 
        fixed = FALSE)) != 1) {
      return(FALSE)
    }
    
    if (length(
      grep(
        pattern = "^[[:alnum:]]{1}[A-Za-z0-9._-]{0,}[[:alnum:]]{1}$", 
        x = splts[2], 
        fixed = FALSE)) != 1) {
      return(FALSE)
    }
  }
  
  return(TRUE)
  
}