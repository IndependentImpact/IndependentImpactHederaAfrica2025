dateTimeInstant_toFluree <- function(x) {
  return(
    list(
      '@context' = list(
        'time' = "http://www.w3.org/2006/time#"),
      '@type' = "time:Instant",
      'time:inXSDDateTimeStamp' = as.character(x)))
}