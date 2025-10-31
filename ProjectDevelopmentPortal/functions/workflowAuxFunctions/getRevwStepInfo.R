# Returns the details of the workflow step, module and schema that must be used to
# review a document that has been submitted to stepWorkflow of idWorkflow.
#
getRevwStepInfo <- function(stepWorkflow,
                             idWorkflow,
                             nmWrkflwHndlr = NULL,
                             dbCon) {
  
  if (length(nmWrkflwHndlr) == 0) {
    nmWrkflwHndlr <- dbFetch(
      dbSendQuery(
        conn = dbCon, 
        statement = sprintf("SELECT handler_r FROM tbl_workflows WHERE id = '%s';",
                            idWorkflow)))[["handler_r"]]
  }
  
  dfStepMap <- getWorkflowStepMap(
    nmWrkflwHndlr = nmWrkflwHndlr,
    idWorkflow = idWorkflow,
    dbCon = dbCon)
  
  # Return the info for the requested review block.
  idxO <- which(dfStepMap$step_workflow == stepWorkflow)
  idxR <- which(dfStepMap$step_workflow == dfStepMap$review_with[idxO])
  
  df <- dfStepMap[idxR,]

  return(df)
  
}
