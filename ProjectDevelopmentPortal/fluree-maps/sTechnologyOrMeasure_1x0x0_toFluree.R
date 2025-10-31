
sTechnologyOrMeasure_1x0x0_toFluree <- function(lsData) {
  
  res <- list(
    '@context' = list(
      "indimp" = "https://independentimpact.org/ns/",
      "schema" = "https://schema.org/"),
    '@type' = "indimp:TechnologyOrMeasure",
    'indimp:techMeasType' = lsData$type_techmeas,
    'schema:description' = lsData$description)

  if (length(jellyfi3shR::emptyOrMissingAsNull(lsData$age_current)) == 1) {
    res <- c(
      res, 
      list('indimp:currentAgeInYears' = lsData$age_current))
  }
  
  if (length(jellyfi3shR::emptyOrMissingAsNull(lsData$lifespan_estimated)) == 1) {
    res <- c(
      res, 
      list('indimp:estimatedLifespanInYears' = lsData$lifespan_estimated))
  }
  
  return(res)
  
}