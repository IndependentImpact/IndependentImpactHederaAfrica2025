
# Example:
# schema <- Guardener::GgetSchema(
#             schemaId = "65c0d068320d203db7dd6bcf", 
#             returndf = FALSE, ...)
# x <- schema$document
# x <- jsonlite::fromJSON(x,
#                         simplifyDataFrame = FALSE,
#                         simplifyVector = TRUE,
#                         simplifyMatrix = FALSE,
#                         flatten = TRUE)
# calcPolicyHash(x)
# calcSchemaHash(x, ignoreOrder = TRUE)

calcSchemaHash <- function(x, ignoreOrder = FALSE) {
  
  # Remove a bunch of fields not relevant to schema comparisons.
  x[c("$id", "$comment", "type", "additionalProperties")] <- NULL
  x$properties[c("@context", "type", "id", "policyId", "ref")] <- NULL
  x$properties <- lapply(X = x$properties, FUN = function(y) {
    y["$comment"] <- NULL
    return(y)
  })

  # Deal with any nested schemas.
  nmDefs <- "$defs"
  if (length(x[nmDefs]) > 0) {
    
    # Call calcSchemaHash on any nested schemas.
    x[nmDefs] <- lapply(X = x[nmDefs], FUN = function(d) {

      if (length(intersect(c("title", "description", "properties"),
                names(d))) != 3) {
        d <- lapply(X = d, FUN = calcSchemaHash, ignoreOrder = ignoreOrder)
      } else {
        d <- calcSchemaHash(d, ignoreOrder = ignoreOrder)
      }
    
      return(d)
    })
    
    lsDefs <- x[[nmDefs]]
    x[[nmDefs]] <- NULL
    
    # Replace any references to nested schemas with their hashes.
    x$properties <- lapply(X = x$properties, FUN = function(y) {
      if (!("$ref" %in% names(y))) { return(y) }
      y[["$ref"]] <- lsDefs[[y[["$ref"]]]]
      return(y)
    })
    
  } 
    
  if (ignoreOrder) {
    x$properties <- x$properties[order(names(x$properties))]
  }
  
  # Convert JSON.
  x <- jsonlite::toJSON(x, auto_unbox = TRUE)

  # Calculate the hash.
  hsh <- digest::digest(x, algo = "sha256", serialize = FALSE)
  
  # Done.
  return(hsh)

}




