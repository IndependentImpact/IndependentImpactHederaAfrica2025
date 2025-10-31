#'@param start An ISO 8601 date-time string.
#'@param end An ISO 8601 date-time string.
#'
dateTimePeriod_toFluree <- function(start, end) {
  
  res <- list(
    '@context' = list(
      'time' = "http://www.w3.org/2006/time#"),
    "@type" = "time:Interval",
    "time:hasBeginning" = list(
      "@type" = "time:Instant",
      "time:inXSDDateTimeStamp" = as.character(start)),
    "time:hasEnd" = list(
      "@type" = "time:Instant",
      "time:inXSDDateTimeStamp" = as.character(end)))
  
  return(res)
  
}