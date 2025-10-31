prepDfForSQL <- function(df, 
                         vnms = NULL, 
                         enquoteChar = TRUE) {

  if (is.null(vnms)) { vnms <- names(df) }
  idxxCols <- which(names(df) %in% vnms)
  
  for (i in idxxCols) {
    
    if (any(c("integer", "numeric") %in% class(df[[i]]))) { next }
    
    # Sometimes it happens that dates arrive at this point as integers; 
    # convert those to standard format.
    if (any(c("POSIXt", "POSIXct", "Date") %in% class(df[[i]]))) { 
      
      df[[i]] <- as.character(df[[i]]) 
      idxx <- grep(pattern = "-", x = df[[i]], fixed = TRUE)
      idxx <- setdiff(1:nrow(df), idxx)

      if ("Date" %in% class(df[[i]])) {
        df[[i]][idxx] <- as.character(
          as.POSIXct(as.integer(df[[i]][idxx]),
                     origin = as.Date("1970-01-01")))
      } else {
        df[[i]][idxx] <- as.character(
          as.POSIXct(as.integer(df[[i]][idxx]),
                     origin = as.POSIXct("1970-01-01 00:00:00")))
      }
      
    }
    
    # Convert factors to character.
    if (is.factor(df[[i]])) { df[[i]] <- as.character(df[[i]]) }
    
    # Enquote character fields.
    if (is.character(df[[i]])) { 
      
      # First remove quoting characters from values.
      df[[i]] <- gsub(pattern = "'", 
                      replacement = "^", 
                      x = df[[i]], 
                      fixed = TRUE)
      
      # Enquote.
      if (enquoteChar) {
        df[[i]] <- sprintf("'%s'", df[[i]])
      }
      
    }
  } 
  
  return(df)
}