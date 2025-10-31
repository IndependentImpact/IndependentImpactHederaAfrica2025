# Get all the documents required to determine the policy state.
#
getWorkflowStateDocs <- function(idPrimAgents, idEntity, nmWrkflwHndlr, dbCon) {
  
  dfDocMd <- dbFetch(
    dbSendQuery(
      conn = dbCon, 
      statement = sprintf("SELECT * FROM tbl_document_metadata WHERE id_entity = '%s';", idEntity)))
  if (nrow(dfDocMd) == 0) {
    return(NULL)
  }
  
  # Get the IDs of all versions of this workflow, so that we can search
  # this entity's documents across all versions of the workflow.
  wrkflwIds <- dbFetch(
    dbSendQuery(
      conn = dbCon,
      statement = sprintf("SELECT id FROM tbl_workflows WHERE handler_r = '%s';",
                          nmWrkflwHndlr)))[["id"]]
  
  
  # Subset to documents belonging to this policy (but all its versions) only.
  dfDocMd <- dfDocMd[which(dfDocMd$id_workflow %in% wrkflwIds),]
  if (nrow(dfDocMd) == 0) {
    return(NULL)
  }
  
  # Subset to the documents created by the primary agents of this policy version.
  didsPrimAgents <- dbFetch(
    dbSendQuery(
      conn = dbCon, 
      statement = sprintf("SELECT did FROM tbl_link_agents_x_dids WHERE id_agent IN(%s);", 
                          paste(sprintf("'%s'", idPrimAgents),
                                collapse = ","))))
  didsPrimAgents <- didsPrimAgents$did
  dfDocMd <- dfDocMd[which(dfDocMd$did_author %in% didsPrimAgents),]
  if (nrow(dfDocMd) == 0) {
    return(NULL)
  }
  
  # Order the documents by 'date_modified', DESCENDING.
  dfDocMd <- dfDocMd[order(dfDocMd$date_modified, 
                           decreasing = TRUE),]
  rownames(dfDocMd) <- 1:nrow(dfDocMd)
  
  # Where multiple documents with the same status (not outcome) for a certain 
  # step exists, keep only the most recent one.
  dfDocMd <- dfDocMd[which(!duplicated(dfDocMd[,c("step_workflow", "status")])),]
  rownames(dfDocMd) <- 1:nrow(dfDocMd)
  
  return(dfDocMd)
}
