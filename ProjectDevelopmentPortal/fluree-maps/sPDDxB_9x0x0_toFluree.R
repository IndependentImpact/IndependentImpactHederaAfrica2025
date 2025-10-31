sPDDxB_9x0x0_toFluree <- function(lsData, projectId) {
  
  return(
    lapply(
      X = lsData$impacts, 
      FUN = sImpact_3x0x0_toFluree, 
      projectId = projectId))
  
}
