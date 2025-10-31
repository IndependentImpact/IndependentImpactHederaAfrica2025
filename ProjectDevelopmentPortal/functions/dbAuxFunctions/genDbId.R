
genDbId <- function(dbCon, tblNm, n) {

  oidxMax <- dbFetch(
    dbSendQuery(conn = dbCon, 
                statement = sprintf("SELECT MAX(oidx) FROM %s;",
                                    tblNm)))
  oidxMax <- oidxMax$max
  if (is.na(oidxMax)) { oidxMax <- 0 }
  ids <- sprintf("%s_%d", tblNm, (1:n + oidxMax))
  ids <- sapply(X = ids, FUN = digest::digest, algo = "md5") 
  names(ids) <- NULL
  return(ids)
  
}
