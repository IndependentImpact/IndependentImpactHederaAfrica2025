validatePassword <- function(pwd, emailAddr = NULL) {
  
  res <- list(
    valid = FALSE,
    msg = NULL)
  
  # Password must be at least 8 chars in length.
  if (nchar(pwd) < 8) {
    res$msg <- "Password must be at least 8 characters in length."
    return(res)
  }
  
  # Password cannot be the same as the username.
  if (length(emailAddr) == 1) {
    if (pwd == emailAddr) {
      res$msg <- "Password cannot be the same as the username!"
      return(res)
    }
  }
  
  # Password cannot contain any spaces.
  {
    bHasSpace <- length(grep(pattern = "[[:blank:]]", 
                             x = pwd, 
                             fixed = FALSE)) > 0
    if (bHasSpace) {
      res$msg <- "Password cannot contain any spaces."
      return(res)
    }
  }
  
  # Count number of letters in password.
  {
    idxxLettrs <- setdiff(as.integer(gregexec(pattern = "[[:alpha:]]", 
                                              text = pwd, 
                                              fixed = FALSE, 
                                              ignore.case = TRUE)[[1]]), 
                          y = -1)
    nLettrs <- length(idxxLettrs)
  }
  
  # Count number of digits in password.
  {
    idxxDigits <- setdiff(as.integer(gregexec(pattern = "[[:digit:]]", 
                                              text = pwd, 
                                              fixed = FALSE)[[1]]), 
                          y = -1)
    nDigits <- length(idxxDigits)
  }
  
  # Count number of special characters in password.
  {
    idxxSpecChars <- setdiff(as.integer(gregexec(pattern = "#|@|!|_", 
                                                 text = pwd, 
                                                 fixed = FALSE)[[1]]), 
                             y = -1)
    nSpecChars <- length(idxxSpecChars)
  }
  
  # Count number of periods in the password.
  {
    idxxPeriods <- setdiff(as.integer(gregexec(pattern = ".", 
                                               text = pwd, 
                                               fixed = TRUE)[[1]]), 
                           y = -1)
    nPeriods <- length(idxxPeriods)
  }
  
  # Password must have at least one letter, one digit and one special char.
  if (nLettrs < 1 | nDigits < 1 | nSpecChars < 1) {
    res$msg <- "Password must have at least one letter, one digit and one special character (#,@,! or _)."
    return(res)
  }
  
  # Password is not allowed to have any characters other than letters, digits, '#', '!', '@', '_' and periods.
  if (nchar(pwd) != (nLettrs + nDigits + nSpecChars + nPeriods)) {
    res$msg <- "No special characters other than '#', '@', '!' and '_' are allowed."
    return(res)
  }
  
  res$valid <- TRUE
  res$msg <- "Good password!"
  
  return(res)
}