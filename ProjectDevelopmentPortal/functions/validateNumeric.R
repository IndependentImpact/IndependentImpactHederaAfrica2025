
validateNumeric <- function(val, min = -Inf, max = Inf) {
  
  res <- list(is_valid = FALSE,
              val = val,
              msg = "")
  
  if (nchar(val) == 0) {
    return(res)
  }
  
  # Remove any alphabetic characters.
  val2 <- gsub(pattern = "[[:alpha:]]", 
               replacement = "", 
               x = val, 
               ignore.case = TRUE)
  if (val2 != val) {
    res$msg <- 'Invalid input. No alphabetic characters allowed.'
    return(res)
  }
  
  # Remove any commas (only one period is allowed per input).
  val2 <- gsub(pattern = ",", 
               replacement = "", 
               x = val, 
               fixed = TRUE)
  if (val2 != val) {
    res$msg <- "Invalid input. Please use a '.' instead of a ',' to indicate decimals. Tip: Use spaces to separate groups of digits for readablity."
    return(res)
  }
  
  # Remove any spaces.
  val <- gsub(pattern = "[[:blank:]]", 
              replacement = "", 
              x = val)
  if (nchar(val) == 0) {
    res$msg <- "Invalid value."
    return(res)
  }
  
  # Attempt to convert to numeric class.
  val <- as.numeric(val)
  if (is.na(val)) {
    res$msg <- "Invalid value."
    return(res)
  }
  
  # Check against lower and upper limits.
  if (val < min) {
    res$msg <- sprintf("Invalid value. Cannot be less than %s.", min)
    return(res)
  }
  if (val > max) {
    res$msg <- sprintf("Invalid value. Cannot be greater than %s.", max)
    return(res)
  }
  
  # We seem to be good.
  res$is_valid <- TRUE
  res$val <- val
  return(res)
  
}