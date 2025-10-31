
#'@param clearExisting If TRUE, will clear (terminate) all other existing 
#'  connections to the database. Defaults to FALSE.
getDbCon <- function(clearExisting = FALSE) {
  
  drv <- dbDriver("PostgreSQL")
  
  if (clearExisting) {
    lsConns <- dbListConnections(drv = drv)
    if (length(lsConns) > 0) {
      for (k in 1:length(lsConns)) {
        message(sprintf("Disconnecting from db (%d)...", k))
        lsRes <- dbListResults(conn = lsConns[[k]])
        for (res in lsRes) {
          dbClearResult(res)
        }
        dbDisconnect(conn = lsConns[[k]])
      }
    }; rm(lsConns)
  }
  
  dbCon <-  dbConnect(drv, 
                      dbname = DB_NAME,
                      host = DB_HOST, 
                      port = DB_HOST_PORT,
                      user = iwefdj$POSTGRES_UN, 
                      password = iwefdj$POSTGRES_PW)
  
  return(dbCon)
}