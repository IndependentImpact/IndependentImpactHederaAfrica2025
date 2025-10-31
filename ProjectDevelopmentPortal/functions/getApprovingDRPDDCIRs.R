# Auxiliary function for use by the schema modules.

getApprovingDRPDDCIRs <- function(dbCon,
                                  idProject,
                                  all = FALSE) {
  
  return(
    getApprovingDRs(
      dbCon = dbCon, 
      idProject = idProject, 
      stepWorkflow = "rvcdb_vldtr_DR_CIR", 
      all = all))
  
}
