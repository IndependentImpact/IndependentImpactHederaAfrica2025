creditingPeriod_toFluree <- function(start, end, renewable) {
  
  res <- list(
    '@context' = list(
      'time' = "http://www.w3.org/2006/time#",
      'indimp' = "https://independentimpact.org/ns/"),
    "@type" = "indimp:CreditingPeriod",
    "time:hasBeginning" = list(
      "@type" = "time:Instant",
      "time:inXSDDateTimeStamp" = start),
    "time:hasEnd" = list(
      "@type" = "time:Instant",
      "time:inXSDDateTimeStamp" = end),
    "indimp:creditingPeriodIsRenewable" = renewable)
  
  return(res)
  
}