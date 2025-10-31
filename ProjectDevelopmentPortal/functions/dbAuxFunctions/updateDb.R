#'@note
#' This function calls 'prepDfForSQL' on your behalf, so do not do it yourself!
#'  
updateDb <- function(df, tblNm, idVars, dbCon) {
  
  df <- prepDfForSQL(df)
  
  updateVars <- setdiff(names(df), idVars)
  
  for (r in 1:nrow(df)) {
    q <- sprintf("UPDATE %s SET %s WHERE %s;",
                 tblNm,
                 paste(sprintf("%s = %s",
                               updateVars,
                               unlist(df[r, updateVars])), 
                       sep = "", collapse = ", "),
                 paste(sprintf("%s = %s",
                               idVars,
                               unlist(df[r, idVars])), 
                       sep = "", collapse = " AND "))
  }
  
  res <- dbSendStatement(conn = dbCon, statement = q)
  dbClearResult(res)
  
  return(invisible(0))
}