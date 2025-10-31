sMethodology_1x0x0_toFluree <- function(title, label, versionTag) {
  
  res <- list(
    '@context' = list(
      "indimp" = "https://independentimpact.org/ns/",
      "dcterms" = "http://purl.org/dc/terms/",
      "rdfs" = "http://www.w3.org/2000/01/rdf-schema#",
      "data" = "http://jellyfiiish.xyz/ns/"),
    '@type' = "indimp:Methodology",
    'dcterms:title' = title,
    'rdfs:label' = label,
    'data:versionTag' = versionTag)
  
  return(res)
  
}