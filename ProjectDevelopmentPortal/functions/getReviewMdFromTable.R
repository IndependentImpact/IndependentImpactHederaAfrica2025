
getReviewMdFromTable <- function(workflowId,
                                 workflowStepOrig,
                                 nmWrkflwHndlr = NULL,
                                 entityId, 
                                 dbCon, 
                                 lsFilters = NULL) {
  
  # Get the label of the workflow step that must have been used to review the 
  # documents submitted to block workflowStepOrig, and let 
  # getPubWrkflwStepDocsMd() handle the rest.
  
  dfRevwBlockInfo <- getRevwStepInfo(
    stepWorkflow = workflowStepOrig, 
    idWorkflow = workflowId, 
    dbCon = dbCon)
  if (nrow(dfRevwBlockInfo) == 0) {
    message(
      sprintf(
        "ERROR: No review step information found for step '%s' of workflow %s.", 
        workflowStepOrig, workflowId))
    return(NULL)
  }
  
  return(
    getPubWrkflwStepDocsMd(
      workflowId = workflowId,
      nmWrkflwHndlr = nmWrkflwHndlr,
      workflowStep = dfRevwBlockInfo$review_with,
      entityId = entityId, 
      dbCon = dbCon, 
      lsFilters = lsFilters))
}
