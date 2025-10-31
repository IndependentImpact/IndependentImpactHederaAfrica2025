#'@param stepWorkflow Character. The label/tag/name of the reviewer's workflow 
#'  step.
getApprovingDRs <- function(dbCon,
                            idProject,
                            stepWorkflow,
                            all = FALSE) {
  
  # Get the Postgres db document metadata for all documents that have been 
  # submitted to [stepWorkflow] for this project.
  {
    q <- sprintf("SELECT * FROM tbl_document_metadata WHERE id_entity = '%s' AND step_workflow = '%s' AND status = 'PUBLISHED';",
                 idProject, stepWorkflow)
    dfDocMdRev <- dbGetQuery(conn = dbCon, statement = q)
    
    if (length(dfDocMdRev) == 0) {
      return(NULL)
    }
    if (nrow(dfDocMdRev) == 0) {
      return(NULL)
    }
  }
  
  # Get the original documents corresponding to these reviews.
  {
    q <- sprintf(
      "SELECT id, id_message_h, status, outcome FROM tbl_document_metadata WHERE id_message_h IN(%s) AND outcome = 'APPROVED';",
      paste(sprintf("'%s'", dfDocMdRev$id_msg_pred), sep = "", collapse = ","))  
    dfDocMdOg <- dbGetQuery(conn = dbCon, statement = q)
    
    if (length(dfDocMdOg) == 0) {
      return(NULL)
    }
    if (nrow(dfDocMdOg) == 0) {
      return(NULL)
    }
  }
  
  dfDocMdRev <- dfDocMdRev[which(dfDocMdRev$id_msg_pred %in% dfDocMdOg$id_message_h),]
  
  # Return all the approving reviews for this project, if requested.
  if (all) { return(dfDocMdRev) } 
  
  # Return only the most recent one still left at this point in time.
  return(dfDocMdRev[which.max(dfDocMdRev$date_modified),])
}
