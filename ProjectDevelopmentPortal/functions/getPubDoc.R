# Function to retrieve a published document.
#'@param contentOnly Logical. If TRUE, will discard any and all metadata. If
#'  FALSE, will return root.document as is.
#'@return If contentOnly = TRUE, a list of length = the number of credential 
#'  subjects in the document. Most documents have only one cs, so the result 
#'  will be a list of length = 1. Each of the cs items in the result will, in 
#'  turn, be a list of length = the number of fields in the credential subject.
#'  If contentOnly = FALSE, a list of length = the number of fields in the 
#'  document.
#'  
getPubDoc <- function(
    docId,
    dbCon,
    contentOnly = FALSE,
    decrypt = TRUE) {
  
  # Get the necessary metadata from the db.
  q <- sprintf("SELECT uri_ipfs, encrypted, type_doc FROM tbl_document_metadata WHERE id = '%s';", docId)
  dfDocMd <- dbGetQuery(conn = dbCon, statement = q)
  
  # Retrieve the document from IPFS.
  res <- httr::GET(url = ipfsUriToUrl(dfDocMd$uri_ipfs))
  if (res$status_code != 200) {
    stop("Failed to retrieve document from IPFS.")
  }
  
  # If this is an encrypted document and decrypt = FALSE, return the raw,
  # encrypted document as is.
  if (dfDocMd$encrypted & !decrypt) {
    return(content(x = res, as = "raw"))
  }
  
  # If this is an encrypted document and decrypt = TRUE, extract and decrypt the 
  # content now, otherwise just extract the content.
  if (dfDocMd$encrypted & decrypt) {
    
    # Create our key pair.
    key <- cyphr::keypair_openssl(
      pub = iwefdj$KEYPTH_CYPHR, 
      key = iwefdj$KEYPTH_CYPHR, 
      envelope = TRUE,
      password = iwefdj$PW_CYPHR) 
    
    # Decrypt to simple char.
    cont <- rawToChar(
      cyphr::decrypt_data(
        data = httr::content(x = res, as = "raw"), 
        key = key))

  } else {
    # Extract content as simple char.
    cont <- rawToChar(httr::content(x = res, as = "raw"))
  }

  # Fix invalid UTF-8 characters (replace with ASCII equivalents or '?')
  cont <- iconv(cont, from = "UTF-8", to = "UTF-8", sub = "byte")
  
  # Parse as JSON.
  doc <- jsonlite::fromJSON(
    txt = cont, 
    simplifyDataFrame = FALSE, 
    simplifyMatrix = FALSE)
  
  # If the user wants any possible signing, VC- or VP-specific metadata as well, 
  # there's nothing left for us to do, so return the doc as is.
  if (!contentOnly) { return(doc) }
  
  # Reaching this point means the user wants us to strip the metadata.
  
  if (dfDocMd$type_doc == "REGULAR_SIGNED") { 
    # Return only the $document part, not the $signature part too.
    return(doc$document) 
  }

  if (dfDocMd$type_doc == "VP") {
    
    doc <- doc$verifiableCredential
    doc <- lapply(X = doc, FUN = function(x) {
      x <- lapply(X = x$credentialSubject, FUN = function(y) {
        y[c("policyId", "@context", "id", "type")] <- NULL  
        return(y)
      })
      if (length(x) == 1) {
        x <- x[[1]]
      }
      x <- rmCtx(x) # Remove '@context' and 'type' from any nested schema fields.
      return(x)
    })
    
    return(doc)
  }
  
  if (dfDocMd$type_doc == "VC") {
    doc <- doc$credentialSubject
    doc <- lapply(X = doc, FUN = function(x) {
      x[c("policyId", "@context", "id", "type")] <- NULL
      return(x)
    })
    
    # Remove '@context' and 'type' from any nested schema fields.
    doc <- rmCtx(doc)
    
    return(doc)
  }
  
  message(sprintf("WARNING: Unknown document type encountered: '%s'", dfDocMd$type_doc))
  return(doc)
}

# Aux function to getPubDoc().
# Recursively removes the 'type' and '@context' fields from a document.
#
rmCtx <- function(x) {
  
  if (is.list(x)) {
    x <- lapply(X = x, FUN = rmCtx)
  }
  
  if (all(c("type", "@context") %in% names(x))) {
    x[c("type", "@context")] <- NULL
  }
  
  return(x)
  
}
