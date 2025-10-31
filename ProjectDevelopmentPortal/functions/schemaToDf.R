
#' @param schema One of the items in the list returned by Guardener::GgetSchemas()
#'  or a schema definition read from IPFS.
#' @param inclMd Logical. If TRUE, will return a separate, second data frame
#'  giving the schema title, version number and description; if FALSE, will
#'  return only one data frame containing the schema field definitions. Defaults
#'  to FALSE.
schemaToDf <- function(schema, defs = NULL, inclMd = FALSE) {
  
  rbind.fill <- plyr::rbind.fill
  
  if ("document" %in% names(schema)) {
    doc <- schema$document
    #doc <- jsonlite::fromJSON(doc)
  } else {
    doc <- schema
  }
  
  # Get the meta data ready for export, if required.
  if (inclMd) {
    
    dfMd <- data.frame(
      title = schema$title,
      version = NA_character_,
      descr = schema$description)
    
    vrsn <- strsplit(x = schema[["$id"]], split = "&", fixed = TRUE)[[1]]
    vrsn <- vrsn[length(vrsn)]
    dfMd$version <- vrsn
    
  }
  
  lsFields <- doc$properties
  lsFields[c("@context", "type", "id", "policyId", "ref")] <- NULL
  lsFields <- lapply(X = lsFields, FUN = function(x) {
    
    #message(x$title)
    
    # Simplify the details for array fields.
    x$repeated <- FALSE
    if ("type" %in% names(x)) {
      
      x$repeated <- x$type == "array"
      
      if (x$repeated) {
        
        # Note: Every field will either have a 'type' variable or a '$ref'
        # variable, but never both (well, we haven't seen one thus far with 
        # both...).
        
        if ("type" %in% names(x$items)) {
          x$type <- x$items$type
          if ("enum" %in% names(x$items)) {
            x$enum <- x$items$enum
          }
        }
        if ("$ref" %in% names(x$items)) {
          x$type <- NA
          x[["$ref"]] <- x$items[["$ref"]]
        }
        
        x$items <- NULL
        
      }
      
    }
  
    # collapse enums
    if ("enum" %in% names(x)) {
      x$enum <- paste(x$enum, sep = "", collapse = ",")
    }
    
    # extract field key
    x$key <- jsonlite::fromJSON(x$`$comment`)$term
    
    # extract IPFS URI of the schema, if this field is a schema field itself
    x$url_ipfs <- jsonlite::fromJSON(x$`$comment`)[["@id"]]
    x$url_ipfs <- strsplit(x = x$url_ipfs, split = "#", fixed = TRUE)[[1]][1]
    
    # remove the $comment field
    x[["$comment"]] <- NULL
    
    # to data frame
    x <- as.data.frame(x)
    
    # done
    return(x)
    
  })
  dfFields <- do.call("rbind.fill", lsFields)
  
  # Mass clean-up of 'url_ipfs' var
  idxx <- grep(pattern = "www.schema.org", 
               x = dfFields$url_ipfs, 
               fixed = TRUE)
  dfFields$url_ipfs[idxx] <- NA_character_
  
  # add 'iri_schema' var
  dfFields$iri_schema <- NA_character_
  
  # copy $ref to iri_schema and then remove $ref
  if ("X.ref" %in% names(dfFields)) {
    idxx <- which(!is.na(dfFields[["X.ref"]]))
    dfFields$iri_schema[idxx] <- dfFields[idxx, "X.ref"]
    dfFields$type[idxx] <- "complex"
    dfFields[["X.ref"]] <- NULL
  }
  
  # mark fields as required or not
  cReq <- doc$required
  dfFields$required <- dfFields$key %in% doc$required
  
  # drop the read-only fields
  dfFields <- dfFields[which(!dfFields$readOnly),]
  dfFields$readOnly <- NULL
  
  # rearrange
  dfFields <- dfFields[,intersect(c("key", "title", "description", 
                                    "required", "repeated", "type", "format",
                                    "enum", "iri_schema", "url_ipfs"), 
                                  names(dfFields))]
  
  # deal with the 'format' column
  if ("format" %in% names(dfFields)) {
    idxx <- which(!is.na(dfFields$format))
    dfFields$type[idxx] <- sprintf("%s:%s", dfFields$type[idxx], dfFields$format[idxx])
  } 
  dfFields$format <- NA_character_
  dfFields$format <- lapply(X = dfFields$format, FUN = as.list)
  
  # If the schema does not contain any fields that are schemas in themselves,
  # we can just return dfFields.
  if (all(is.na(dfFields$iri_schema))) {
    if (!inclMd) { return(dfFields) }
    return(list(dfFields = dfFields, dfMd = dfMd))
  }
  
  # If the schema contains fields that are other schemas in themselves, build
  # data frames for those schemas too and nest them in the 'format' column
  if (length(doc[["$defs"]]) > 0) {
    defs <- doc[["$defs"]]
  }
  idxx <- grep(pattern = "^#", x = dfFields$iri_schema)
  if (length(idxx) > 0 & length(defs) == 0) {
    stop("$defs is empty, but embedded schema present.")
  } 
  for (idx in idxx) {
    if (!(dfFields$iri_schema[idx] %in% names(defs))) {
      stop(sprintf("Definition for embedded scheme %s not included in current schema.", 
                   dfFields$iri_schema[idx]))
    }
    dfFields$format[[idx]] <- schemaToDf(defs[[dfFields$iri_schema[idx]]], 
                                         defs = defs, 
                                         inclMd = FALSE)
  }
  
  # Done.
  if (!inclMd) { return(dfFields) }
  return(list(dfFields = dfFields, dfMd = dfMd))
  
}

# Example:
# 
# schemas <- Guardener::GgetSchemas(refreshToken = resLogin$refreshToken,
#                                   topicId = "0.0.3889333")
# dfSch <- schemaToDf(schemas[[21]])
