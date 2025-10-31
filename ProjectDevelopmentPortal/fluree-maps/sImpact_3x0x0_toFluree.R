sImpact_3x0x0_toFluree <- function(lsData, projectId) {
  
  res <- list(
    '@context' = list(
      "indimp" = "https://independentimpact.org/ns/",
      "schema" = "https://schema.org/",
      "impactont" = "https://w3id.org/impactont/"),
    
    '@type' = "impactont:Impact",
    
    'indimp:impactIntentionality' = lsData$intentionality,
    'indimp:beneficialOrAdverse' = lsData$beneficial_or_adverse,
    'schema:description' = lsData$description,
    'indimp:monitored' = lsData$monitored)
  
  if (length(jellyfi3shR::emptyOrMissingAsNull(lsData$not_monitored_justifcation)) == 1) {
    res <- c(
      res,
      list('indimp:notMonitoredJustification' = lsData$not_monitored_justification))
  } else {
    
    res <- c(
      res,
      list(
        'indimp:additionalityJustification' = lsData$additionality,
        'impactont:hasStateA' = state_toFluree(
          dtStart = lsData$crediting_period$datetime_start, 
          dtEnd = lsData$crediting_period$datetime_end, 
          mod = "counterfactual", # for the baseline scenario
          indic = indicator_toFluree(
            lbl = lsData$indicator_label, 
            uom = lsData$indicator_unit_of_measure), 
          indicId = NULL, 
          val = lsData$state_baseline),
        
        'impactont:hasStateB' = state_toFluree(
          dtStart = lsData$crediting_period$datetime_start, 
          dtEnd = lsData$crediting_period$datetime_end, 
          mod = "real", # for the project scenario
          indic = indicator_toFluree(
            lbl = lsData$indicator_label, 
            uom = lsData$indicator_unit_of_measure), 
          indicId = NULL, 
          val = lsData$state_project),
        
        'impactont:hasProvenance' = list(
          '@id' = paste0("indimp:activities/", projectId)),
        
        'indimp:monitoringPeriod' = lapply(X = lsData$monitoring_periods, FUN = function(p) {
          return(dateTimePeriod_toFluree(
            start = p$datetime_start, 
            end = p$datetime_end))
        }), # < list of lists because it can have multiple monitoring periods 
        # during a single crediting period.
        
        'indimp:creditingPeriod' = creditingPeriod_toFluree(
          start = lsData$crediting_period$datetime_start, 
          end = lsData$crediting_period$datetime_end, 
          renewable = lsData$crediting_period_type == "RENEWABLE"),
        
        'indimp:indicatorMethodology' = lapply(
          X = lsData$methodologies, 
          FUN = function(m) {
            return(sMethodology_1x0x0_toFluree(
              title = m$methodology$title, 
              label = m$methodology$label, 
              versionTag = m$methodology$version))
          })
      ))
  }
  
  return(res)
}





# lsData <- getPubDoc(
#   docId = "d690c9263025d246c20484e7c79980a2",
#   dbCon = dbCon,
#   contentOnly = TRUE)

