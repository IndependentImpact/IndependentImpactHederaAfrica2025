indicator_toFluree <- function(lbl, uom) {
  
  res <- list(
    '@context' = list(
      "indimp" = "https://independentimpact.org/ns/",
      "rdfs" = "http://www.w3.org/2000/01/rdf-schema#",
      "impactont" = "https://w3id.org/impactont/"),
    '@type' = "impactont:Indicator",
    'rdfs:label' = lbl,
    'indimp:unitOfMeasure' = uom)
  
  return(res)
  
}