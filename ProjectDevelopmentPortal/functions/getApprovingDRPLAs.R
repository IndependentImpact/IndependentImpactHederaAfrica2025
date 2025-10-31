# Auxiliary function for use by the PDDX<X>schema modules.
getApprovingDRPLAs <- function(dbCon,
                               idProject,
                               all = FALSE) {
  
  return(
    getApprovingDRs(
      dbCon = dbCon, 
      idProject = idProject, 
      stepWorkflow = "rvcdb_standardBody_DR_PLA", 
      all = all))
  
}
