#'@param dtStart character If the state spans a period, this must be the ISO 
#'  8601 date-time string that marks the start of the period; if the state is
#'  reported only for a single point in time, this must be the ISO 8601 
#'  date-time string that identifies that specific moment.
#'@param dtEnd character If the state spans a period, this must be the ISO 
#'  8601 date-time string that marks the end of the period; if the state is
#'  reported only for a single point in time, this must be left NULL.
#'@param mod character The modality of the state, either 'COUNTERFACTUAL' or 'REAL'.
#'@param indic List The state-defining indicator as a JSON-LD ready list. Will 
#'  be ingored if `indicId` is provided.
#'@param indicId character The identifier (IRI) of the indicator that defines 
#'  the state. Can be left NULL if `indic` is provided. If both `indic` and 
#'  `indicId` are provided, `indic` will be ignored.
#'@param val numeric or character Optional. The value of the indicator at the 
#'  point in time or for the period of time in question.
#'  
state_toFluree <- function(
    dtStart, 
    dtEnd = NULL, 
    mod, 
    indic = NULL, 
    indicId = NULL, 
    val = NULL) {
  
  res <- list(
    '@context' = list(
      "impactont" = "https://w3id.org/impactont/"), 
    '@type' = "impactont:State",
    'impactont:hasTemporalLocation' = if (length(dtEnd) == 0) {
      dateTimeInstant_toFluree(dtStart) 
    } else {
      dateTimePeriod_toFluree(start = dtStart, end = dtEnd) },
    'impactont:hasModality' = tolower(mod),
    'impactont:isDefinedByIndicator' = if (length(indicId) == 0)
      { indic } else { list('@id' = indicId) })
  
  if (length(val) == 1) {
    res <- c(
      res, 
      list('impactont:hasValue' = val))
  }
  
  return(res) 
  
}
