# Auxiliary function for use by the MR schema module(s).
getApprovingDRPRRs <- function(dbCon,
                               idProject,
                               all = FALSE) {
  
  return(
    getApprovingDRs(
      dbCon = dbCon, 
      idProject = idProject, 
      stepWorkflow = "rvcdb_standardBody_DR_PRR", 
      all = all))
  
}
