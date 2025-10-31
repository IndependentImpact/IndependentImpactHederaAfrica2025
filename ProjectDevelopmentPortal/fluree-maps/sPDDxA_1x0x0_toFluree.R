sPDDxA_1x0x0_toFluree <- function(lsData) {
  
  res <- list(
    '@context' = list(
      "indimp" = "https://independentimpact.org/ns/",
      "schema" = "https://schema.org/",
      "aiao" = "https://w3id.org/aiao/",
      "dcterms" = "http://purl.org/dc/terms/"),
    
    '@id' = paste0("indimp:activities/", lsData$headers$id_subject),
    
    '@type' = "aiao:Project",
    
    'indimp:title' = lsData$title_project,
    'indimp:purpose' = list(
      '@type' = "aiao:Control",
      'schema:description' = lsData$purpose_project),
    'indimp:locationShapefile' = lapply(
      X = lsData$location_project, 
      FUN = function(x) {
        list('@type' = "dcterms:Resource",
             'indimp:resourceIpfsUri' = x)
      }),
    'indimp:technologyOrMeasure' = lapply(
      X = lsData$techmeas_project, 
      FUN = sTechnologyOrMeasure_1x0x0_toFluree),
    'indimp:projectHistory' = lsData$history_project)
  
  return(res)
}


# lsData <- getPubDoc(
#   docId = "66b658a5596f789333442dbaf66c7bbe",
#   dbCon = dbCon,
#   contentOnly = TRUE)
