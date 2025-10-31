issueHederaTokens <- function(idToken, 
                              qty,
                              idDocument,
                              dbCon) {
  
  
  # If we have already kicked off an issuance based on this document submission,
  # don't do anything.
  {
    q <- sprintf("SELECT * FROM tbl_token_issuances WHERE id_token_h = '%s' AND id_document = '%s';",
                 idToken, idDocument)
    dfIss <- dbGetQuery(conn = dbCon, statement = q)
    if (nrow(dfIss) > 0) {
      return(invisible(0))
    }
  }
  
  # Reaching this point means we need to kick off a new token issuance.
  dfAdd <- data.frame(
    id_token_h = idToken,
    id_workflow = idWorkflow,  
    step_workflow = stepWorkflow, 
    id_document = idDocument,
    date_started = lubridate::now(tzone = "UTC"),
    status = "PENDING")
  issId <- addToDb(
    dfAdd = dfAdd, 
    tblNm = "tbl_token_issuances", 
    vnmsChckEx = c("id_token_h", "id_document"), 
    dbCon = dbCon, 
    calcIds = TRUE, 
    returnIds = TRUE)
  
  # Now mint and track the minting using tbl_token_issuance_tracker.
  stop("TODO.")
  
}
