
# Retrieves (via tracing) the realised document chain of a document from the
# Hedera network.

getRealDocChain <- function(msgId = NA_character_,
                            ipfsUrl = NA_character_) {

  # Input checking.
  {
    if (length(msgId) == 0) {
      msgId <- NA_character_
    }
    if (length(ipfsUrl) == 0) {
      ipfsUrl <- NA_character_
    }
    
    if (is.na(msgId) & is.na(ipfsUrl)) {
      stop("You must provide either a message ID or an IPFS URL.")
    }
  }
  
  # If message ID were provided, but not IPFS URL, try to get the IPFS URL from
  # the message ID.
  {
    if (!is.na(msgId) & is.na(ipfsUrl)) {
      
      msg <- hedera::getMessage(
        messageId = msgId, 
        network = hederaNetwork, 
        decode = TRUE)[["message"]]
      if ("uri" %in% names(msg)) {
        ipfsUrl <- msg$uri
      } else {
        ipfsUrl <- msg$documentIpfsUri
      }
    }
    
    if (is.na(ipfsUrl)) {
      message(sprintf("ERROR: Failed to retrieve IPFS URL for message %s.", msgId))
      return(NULL)
    }
  }
  
  # Get the IPFS URL in the correct format.
  ipfsUrl <- ipfsUriToUrl(ipfsUrl)

  # Retrieve the document from IPFS.
  { 
    doc <- httr::GET(url = ipfsUrl)

    doc <- tryCatch({
      content(x = doc, as = "parsed")
    }, error = function(e) {
      x <- base::rawToChar(x = doc$content)
      tryCatch({
        jsonlite::fromJSON(x)
      }, error = function(ee) {
        jsonify::from_json(x)
      })
    })
  }
  
  # Now, the document can be a VP, a VC or a regular, signed document. Prep the
  # content so that we can deal with all types in the same way.
  if ("verifiableCredential" %in% names(doc)) { 
    
    # This is a VP containing multiple VCs.
    lsVcs <- doc$verifiableCredential
    
  } else {
    
    if ("credentialSubject" %in% names(doc)) {
      
      # This is a single VC.

      lsVcs <- list(doc)
      
    } else {
      
      if (all(c("document", "signature") %in% names(doc))) {
        
        # This is a regular, signed doc.
        lsVcs <- list(list(credentialSubject = doc$document)) # cheats
        
      } else {
        message("ERROR: Unable to determine document type of ", ipfsUrl, ".")
        return(NULL)
      }
      
    }
  }
  
  # message(sprintf("length(lsVcs) = %d", length(lsVcs))) # TODO. Remove after debugging.
  
  # Interlude: If the user only provided the document's IPFS URL and not the 
  # message ID as well, use the retrieved information above to try and get the
  # message ID of the document.
  {
    # TODO. Use findVCpubMsg() to get the message ID from the IPFS URL.
    # OR perhaps first from our own Postgres db.
  }
  
  
  # Retrieve the details of the first links of the trust chain for the current 
  # document.
  {
    lsDfFirstLinks <- lapply(X = lsVcs, FUN = function(vc) {
      
      # TODO: This code will fail if credentialSubject is actually an array of
      # multiple credential sets.
      
      # If it credentialSubject is an array of credential sets, but there is only
      # element in the array, convert it to a non-array.
      if (length(vc$credentialSubject) == 1 && length(names(vc$credentialSubject)) == 0) {
        vc$credentialSubject <- vc$credentialSubject[[1]]
      }
      
      nmsIdMsgPred <- c(
        grep(
          pattern = "id_msg_pred", 
          x = names(vc$credentialSubject$headers), 
          value = TRUE),
        grep(
          pattern = "id_msg_pred", 
          x = names(vc$credentialSubject), 
          value = TRUE))
      nmsUrlIpfsPred <- c(
        grep(
          pattern = "url_ipfs_pred", 
          x = names(vc$credentialSubject$headers), 
          value = TRUE),
        grep(
          pattern = "url_ipfs_pred", 
          x = names(vc$credentialSubject), 
          value = TRUE))
      
      # The links to the data lineage reports (DLRs) are a bit different, so 
      # let's look for them specifically (which will only be relevant if the 
      # current VC is a monitoring report (MR).
      if ("datasets" %in% names(vc$credentialSubject)) {
        nmsIdMsgPred <- c(nmsIdMsgPred, "id_msg_drdlr")
        nmsUrlIpfsPred <- c(nmsUrlIpfsPred, "uri_ipfs_drdlr")
      }
      
      # The links inside the VIC are also a bit different, so let's look for 
      # them specifically (which will only be relevant if the current document
      # is a VIC).
      if (all(c("certificate_number", "extent_impact", "indicator_impact") %in% names(vc$credentialSubject))) {
        nmsIdMsgPred <- c(nmsIdMsgPred, "id_msg_drmr")
        nmsUrlIpfsPred <- c(nmsUrlIpfsPred, "uri_ipfs_drmr")
      }
      
      # message(sprintf("nmsIdMsgPred = %s", paste(nmsIdMsgPred, sep = "", collapse = ","))) # TODO: Remove after debugging.
      # message(sprintf("nmsUrlIpfsPred = %s", paste(nmsUrlIpfsPred, sep = "", collapse = ","))) # TODO: Remove after debugging.
      
      if (length(nmsIdMsgPred) > length(nmsUrlIpfsPred)) {
        nmsUrlIpfsPred <- NULL
      } else {
        if (length(nmsUrlIpfsPred) > length(nmsIdMsgPred)) {
          nmsIdMsgPred <- NULL
        }
      }
    
      msgIdsPreds <- NA_character_
      ipfsUrlsPreds <- NA_character_
      if (length(nmsIdMsgPred) > 0) {
        msgIdsPreds <- c(
          unlist(vc$credentialSubject$headers[nmsIdMsgPred]),
          unlist(vc$credentialSubject[nmsIdMsgPred]))
        if ("id_msg_drdlr" %in% nmsIdMsgPred) {
          msgIdsPreds <- c(
            msgIdsPreds,
            sapply(X = vc$credentialSubject$datasets, FUN = function(d) d$id_msg_drdlr))
        }
      }
      if (length(nmsUrlIpfsPred) > 0) {
        ipfsUrlsPreds <- c(
          unlist(vc$credentialSubject$headers[nmsUrlIpfsPred]),
          unlist(vc$credentialSubject[nmsUrlIpfsPred]))
        if ("uri_ipfs_drdlr" %in% nmsUrlIpfsPred) {
          ipfsUrlsPreds <- c(
            ipfsUrlsPreds,
            sapply(X = vc$credentialSubject$datasets, FUN = function(d) d$uri_ipfs_drdlr))
        }
      }
      
      df <- data.frame(
        id_message_h = msgId,
        url_ipfs = ipfsUrl,
        date_issuance = ifelse(
          "issuanceDate" %in% names(vc), 
          vc$issuanceDate,
          NA_character_),
        did_issuer = ifelse(
          "issuer" %in% names(vc),
          vc$issuer,
          NA_character_),
        uuid = ifelse(
          "id" %in% names(vc$credentialSubject), 
          vc$credentialSubject$id, 
          NA_character_),
        iri_schema = ifelse(
          "type" %in% names(vc$credentialSubject),
          vc$credentialSubject$type,
          NA_character_),
        id_msg_pred = msgIdsPreds,
        url_ipfs_pred = ipfsUrlsPreds)

      # message(sprintf("nrow(df) = %d", nrow(df))) # TODO: Remove after debugging.
      
      # message(sprintf("nrow(df) = %d", nrow(df))) # TODO: Remove after debugging.
      
      return(df)
      
    })
    dfTrCh <- do.call("rbind.fill", lsDfFirstLinks); rm(lsDfFirstLinks)
    
    if (length(dfTrCh) == 0) {
      # message("length(dfTrCh) = 0") # TODO. Remove after debugging.
      return(NULL)
    }
    if (nrow(dfTrCh) == 0) {
      # message("nrow(dfTrCh) = 0") # TODO. Remove after debugging.
      return(NULL)
    }
  }
  
  # Sanitise.
  {
    dfTrCh$id_msg_pred <- gsub(
      pattern = "[[:blank:]]", 
      replacement = "", 
      x = dfTrCh$id_msg_pred)
    idxx <- which(nchar(dfTrCh$id_msg_pred) == 0)
    dfTrCh$id_msg_pred[idxx] <- NA_character_
    
    dfTrCh$url_ipfs_pred <- gsub(
      pattern = "[[:blank:]]", 
      replacement = "", 
      x = dfTrCh$url_ipfs_pred)
    idxx <- which(nchar(dfTrCh$url_ipfs_pred) == 0)
    dfTrCh$url_ipfs_pred[idxx] <- NA_character_
  }
  
  # Retrieve the rest of the trust chain of each VC, if any.
  idxx <- which(!is.na(dfTrCh$id_msg_pred) | !is.na(dfTrCh$url_ipfs_pred))
  if (length(idxx) > 0) {
    
    # message(sprintf("length(idxx) = %d", length(idxx))) # TODO. Remove after debugging.
    
    lsDfTrChs <- lapply(X = idxx, FUN = function(idx) {
      df <- getRealDocChain(
        msgId = dfTrCh$id_msg_pred[idx],
        ipfsUrl = dfTrCh$url_ipfs_pred[idx])
      return(df)
    })
    
    dfTrCh <- rbind(dfTrCh,
                    do.call("rbind.fill", lsDfTrChs))
    rm(lsDfTrChs)
  }
  
  # Remove duplicates.
  dfTrCh <- dfTrCh[which(!duplicated(dfTrCh)),]
  
  #  Get the IPFS URLs in var 'url_ipfs_pred' in the correct format.
  idxx <- match(x = dfTrCh$id_msg_pred, table = dfTrCh$id_message)
  dfTrCh$url_ipfs_pred[which(!is.na(idxx))] <- dfTrCh$url_ipfs[idxx[!is.na(idxx)]]
  
  # Finishing touches.
  {
    dfTrCh <- dfTrCh[order(as.numeric(dfTrCh$id_message_h), 
                           decreasing = TRUE),]
    rownames(dfTrCh) <- 1:nrow(dfTrCh)
  }
  
  # Done.
  return(dfTrCh)
  
}





