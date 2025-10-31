# Initialise a workflow state variable.
#
initWorkflowState <- function(nmWrkflwHndlr, idWorkflow, dbCon)  {
  
  defaultStates <- getDefaultStepStates(nmWrkflwHndlr)
  
  dfState <- getWorkflowStepMap(
    nmWrkflwHndlr = nmWrkflwHndlr,
    idWorkflow = idWorkflow,
    dbCon = dbCon)
  #dfState <- dfState[which(dfState$step_workflow %in% names(defaultStates)),]
  dfState$state <- NA_character_
  idxx <- match(x = dfState$step_workflow, table = names(defaultStates))
  dfState$state[which(!is.na(idxx))] <- defaultStates[idxx[!is.na(idxx)]]
  
  return(dfState)
}
