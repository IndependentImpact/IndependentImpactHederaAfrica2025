
#'@param x List. res$config where res = GgetPolicy(...).
getIDs <- function(x) {
  
  ids <- c()
  
  if ("id" %in% names(x)) {
    ids <- x$id
  } 

  if ("children" %in% names(x)) {
    ids <- c(ids, unlist(sapply(X = x$children, FUN = getIDs)))
  }
  
  return(ids)
}


#'@param x List. res$config where res = GgetPolicy(...).
getNames <- function(x) {
  
  nms <- c()
  
  if ("tag" %in% names(x)) {
    nms <- x$tag
  } 
  
  if ("children" %in% names(x)) {
    nms <- c(nms, unlist(sapply(X = x$children, FUN = getNames)))
  }
  
  return(nms)
}


#'@param p List. res$config where res = GgetPolicy(...).
#'@param dfSchInfo Data frame. Columns 'iri' and 'hash_ign_id_keep_ordr' from 
#'  tbl_schemas for the schemas used in this policy.
#'  
calcPolicyHash <- function(p, dfSchInfo, deId = TRUE, deName = TRUE) {
  
  dfIds <- NULL
  dfNms <- NULL
  
  if (deId) {
    
    # Get a list of all the IDs in the policy.
    IDs <- getIDs(p)
    names(IDs) <- NULL
    
    if (length(IDs) > 0) {
      # Create replacement IDs.
      IDs <- unique(IDs)
      dfIds <- data.frame(
        id = IDs,
        id_new = sprintf("_replacementId_%d_", 1:length(IDs))) 
    }
  }
  
  if (deName) {
    
    # Get a list of all the block names in the policy.
    nms <- getNames(p)
    names(nms) <- NULL
    nms <- nms[which(nchar(nms) > 0)]
    
    if (length(nms) > 0) {
      # Create replacement names.
      nms <- unique(nms)
      dfNms <- data.frame(
        nm = nms,
        nm_new = sprintf("_replacementNm_%d_", 1:length(nms)))
    }
  }
  
  # Convert p to JSON.
  p <- jsonlite::toJSON(p, auto_unbox = TRUE)

  # Replace schema references with their hashes.
  if (length(dfSchInfo) > 0) {
    for (r in 1:nrow(dfSchInfo)) {
      p <- gsub(
        pattern = dfSchInfo$iri[r], 
        replacement = dfSchInfo$hash_ign_id_keep_ordr[r], 
        x = p, 
        fixed = TRUE)
    }
  }
  
  # Replace the existing IDs with the new IDs.
  if (deId & length(dfIds) > 0) { 
    for (r in 1:nrow(dfIds)) {
      p <- gsub(
        pattern = dfIds$id[r], 
        replacement = dfIds$id_new[r], 
        x = p, 
        fixed = TRUE)
    }
  }
  
  # Replace the existing names with the new names.
  if (deName & length(dfNms) > 0) {
    for (r in 1:nrow(dfNms)) {
      p <- gsub(
        pattern = dfNms$nm[r], 
        replacement = dfNms$nm_new[r], 
        x = p, 
        fixed = TRUE)
    }
  }
  
  # Calculate and return the hash of p.
  hsh <- digest::digest(p, algo = "sha256", serialize = FALSE)
  
  # Done.
  return(hsh)
  
  # TODO. What about schema versions (e.g., '&1.0.0')?
  # TODO: Replace schema IDs with their hashes and then do NOT wipe those hashes.
} 

