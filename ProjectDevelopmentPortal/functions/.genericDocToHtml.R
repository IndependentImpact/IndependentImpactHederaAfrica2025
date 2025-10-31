.genericDocToHtml <- function(x, nm = NULL, lv = 1) {
  
  if (length(nm) == 1) {
    if (is.na(nm)) {
      nm <- NULL
    } else {
      if (nchar(gsub(pattern = "[[:blank:]]", 
                     replacement = "", 
                     x = nm)) == 0) {
        nm <- NULL
      } 
    }
  }
  
  if (length(x) == 1) {
    if (is.na(x)) { 
      x <- c()
    } else {
      if (nchar(gsub(pattern = "[[:blank:]]", 
                     replacement = "", 
                     x = x)) == 0) {
        x <- c()
      }
    }
  }
  
  if (length(x) == 0) {
    if (length(nm) == 0) { return(c()) }
    x <- ""
  }
  
  spcs <- paste(rep("&emsp;", times = max(c(0, lv-1))), 
                collapse = "")
  
  if (!is.list(x)) {
    
    if (nchar(x) > 70) {
      xs <- c()
      xn <- ceiling(nchar(x) / 70)
      for (k in 1:xn) {
        xs <- c(xs, substr(x = x, start = (k-1)*70, stop = (k*70)-1))
      }
      x <- paste(xs, collapse = "... ")
    }
    
    if (length(nm) == 0) {
      return(sprintf("%s%s", spcs, x))
    } 
    
    return(sprintf("<h%d>%s%s</h%d>%s%s", 
                   lv, spcs, nm, lv, spcs, x))
  }
  
  # Reaching this point means x is a list.
  
  txt <- paste(
    c(sprintf("<h%d>%s%s</h%d>", lv, spcs, nm, lv),
      sapply(X = 1:length(x), FUN = function(k) { 
        .genericDocToHtml(
          x = x[[k]], 
          nm = names(x)[k], 
          lv = lv+1) })), 
    collapse = "")
  
  return(txt)
  
}
