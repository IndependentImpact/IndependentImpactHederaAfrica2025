
# The opposite of populateDb().

clearDb <- function(dbCon, excl = NULL) {
  
  tblNms <- dbListTables(dbCon)
  
  # Remove the contents of all tables except those specified by the user.
  {
    tblNms <- setdiff(tblNms, excl)
    
    sapply(X = tblNms, FUN = function(x) {
      q <- sprintf("DELETE FROM %s;", x)
      dbExecute(conn = dbCon, statement = q)
    })  
  }
}
