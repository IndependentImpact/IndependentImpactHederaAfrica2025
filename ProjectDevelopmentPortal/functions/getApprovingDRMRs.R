# Auxiliary function for use by module TIRschema, VICIR (and others).
getApprovingDRMRs <- function(dbCon,
                              idProject,
                              all = FALSE) {
  
  return(
    getApprovingDRs(
      dbCon = dbCon, 
      idProject = idProject, 
      stepWorkflow = "rvcdb_verifier_DR_MR", 
      all = all))
  
}
