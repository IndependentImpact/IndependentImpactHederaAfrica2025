
#'@param vnmsChckEx Character. The name(s) of the variables to use for checking
#' if an entity in dfAdd is already in the database.

addToDb <- function(dfAdd, 
                    tblNm, 
                    vnmsChckEx, 
                    dbCon, 
                    calcIds = TRUE, 
                    returnIds = FALSE,
                    verbose = FALSE) {
  
  if (length(dfAdd) == 0) { 
    if (returnIds) { return(NULL) }
    return(invisible(0)) 
  }
  if (nrow(dfAdd) == 0) { 
    if (returnIds) { return(NULL) }
    return(invisible(0)) 
  }
  
  # Remove variables that do not exist in the db table. 
  {
    vnmsKeep <- dbListFields(conn = dbCon, name = tblNm)
    vnmsRm <- setdiff(names(dfAdd), vnmsKeep)
    if (length(vnmsRm) > 0) {
      message(
        sprintf("WARNING: The following variables do not exist in table '%s' and will be removed: %s", 
                tblNm, 
                paste(vnmsRm, collapse = ", ")))
    }
    dfAdd <- dfAdd[,intersect(names(dfAdd), vnmsKeep)]
  }
  
  if (length(vnmsChckEx) == 0) {
    warning("length(vnmsChckEx) = 0. Not performing any checks for duplicates.")
  } else {
    
    # Remove duplicates internal to dfAdd.
    idxx <- which(duplicated(dfAdd[,vnmsChckEx]))
    if (length(idxx) > 0) {
      dfAdd <- dfAdd[-idxx,]
    }
    
    # Add temporary auxiliary vars to dfAdd.
    dfAdd$id_tmp <- dfAdd[[vnmsChckEx[1]]]
    if (length(vnmsChckEx) > 1) {
      for (k in 2:length(vnmsChckEx)) {
        dfAdd$id_tmp <- sprintf("%s_%s", dfAdd$id_tmp, dfAdd[[vnmsChckEx[k]]])
      }
    }
    
    # Fetch existing records.
    # TODO: This will be a too expensive query once we have a lot of data 
    # in [tblNm]. Improve it.
    dfEx <- dbFetch(
      dbSendQuery(conn = dbCon, 
                  statement = sprintf("SELECT %s FROM %s;", 
                                      paste(vnmsChckEx, collapse = ", "),
                                      tblNm)))
    
    if (nrow(dfEx) > 0) {
      
      # Add temporary auxiliary vars to dfEx
      dfEx$id_tmp <- dfEx[[vnmsChckEx[1]]]
      if (length(vnmsChckEx) > 1) {
        for (k in 2:length(vnmsChckEx)) {
          dfEx$id_tmp <- sprintf("%s_%s", dfEx$id_tmp, dfEx[[vnmsChckEx[k]]])
        }
      }
      
      # Exclude entries already in <tblNm>.
      {
        idxxRm <- which(dfAdd$id_tmp %in% dfEx$id_tmp)
        if (length(idxxRm) > 0) { 
          dfAdd <- dfAdd[-idxxRm,] 
          if (nrow(dfAdd) == 0) { 
            if (returnIds) { return(NULL) }
            return(invisible(0)) 
          }
        }
      }
      
      rm(dfEx)
    }
    
    dfAdd$id_tmp <- NULL
    
  }
  
  # Remove empty variables EXCEPT the 'id' var.
  {
    idxx <- which(sapply(X = dfAdd, FUN = function(x) {all(is.na(x))}))
    vnms <- names(dfAdd)[idxx]
    vnms <- setdiff(vnms, "id")
    if (length(vnms) > 0) {
      dfAdd <- dfAdd[,setdiff(names(dfAdd), vnms)]
      if (length(dfAdd) == 0) { 
        if (returnIds) { return(NULL) }
        return(invisible(0)) 
      }
    }
  }
  
  # Create db IDs for the entries to be added.
  if (calcIds) {
    dfAdd$id <- genDbId(dbCon = dbCon, 
                        tblNm = tblNm, 
                        n = nrow(dfAdd))    
    if (returnIds) {
      newIds <- dfAdd$id
    }
  }
  
  # Prep df for SQL ops.
  dfAdd <- prepDfForSQL(dfAdd)
  
  # Add to database.
  # Note: dbAppendTable() seems unable to work with fields of type 'serial',
  # which is why we are doing it the slow way here.
  for (r in 1:nrow(dfAdd)) {
    q <- sprintf("INSERT INTO %s(%s) VALUES(%s);",
                 tblNm,
                 paste(names(dfAdd), collapse = ","),
                 paste(dfAdd[r,], collapse = ","))
    dbSendStatement(conn = dbCon, statement = q)
  }
  
  if (verbose) {
    message("Added ", nrow(dfAdd), " entries to table ", tblNm, ".")
  }
  
  # Return.
  if (calcIds & returnIds) { return(newIds) }
  return(invisible(0))
}
