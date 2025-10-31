# Aux function to find the message that marked the publication of a specific
# VC and return the contents and timestamp of that message.
#' @param datePublished Character. 
findVCpubMsg <- function(docUuid = NULL, 
                         datePublished = NULL, 
                         topicIds = NULL, 
                         messageId = NULL) {

  # Input checking.
  {
    if (length(messageId) > 0) {
      if (is.na(messageId)) {
        messageId <- NULL
      } else {
        if (nchar(gsub(pattern = "[[:blank:]]", 
                       replacement = "", 
                       x = messageId)) == 0) {
          messageId <- NULL
        }
      }
    }
    
    if (all(sapply(X = list(docUuid, datePublished, topicIds, messageId), FUN = function(x) length(x) == 0))) {
      stop("Must supply either 'messageId' or all three of 'docUuid', 'datePublished' and 'topicIds'.")
    }
    
    if (length(messageId) == 0) {
      if (any(sapply(X = list(docUuid, datePublished, topicIds), FUN = function(x) length(x) == 0))) {
        stop("Must supply 'docUuid', 'datePublished' and 'topicIds' when 'messageId' is not supplied.")
      }
    }
  }
  
  # If we already know the message ID, just retrieve the message contents and 
  # return.
  if (length(messageId) == 1) {
    tryCatch({
      res <- hedera::getMessage(
        messageId = messageId,
        network = hederaNetwork, 
        decode = TRUE)[["message"]]
      return(res)
    }, error = function(e) {
      warning(sprintf("Failed to retrieve message contents. Error: %s", e))
      return(NULL)
    })
  }
  
  # >> Reaching this point means we do not even know the message ID itself. <<
  
  # Get all messages submitted to the topic between just before our 
  # document would have been processed and about 10 seconds after it would have
  # been processed.
  {
    t1 <- datePublished
    t1 <- lubridate::ymd_hms(t1, tz = "UTC")
    t1 <- as.numeric(t1) - 10
    t2 <- t1 + 20
    msgs <- list()
    
    for (topicId in topicIds) {
      
      xURL <- sprintf("https://testnet.mirrornode.hedera.com/api/v1/topics/%s/messages?timestamp=gt:%s&timestamp=lt:%s",
                      topicId, t1, t2)
      bCont <- TRUE
      while (bCont) {
        
        tryCatch({
          res <- httr::GET(xURL)
        }, error = function(e) {
          warning(sprintf("GET query failed for %s.\nError: %s", xURL, e))
          break
        })
        
        nmStatCode <- intersect(c("code", "statusCode", "status_code"), names(res))
        if (length(nmStatCode) == 0) {
          warning("Failed to determine status code field in response.")
          break
        }
        if (res[[nmStatCode]] != 200) {
          warning(sprintf("Failed to retrieve message ID for submitted document. Status code: %s", 
                          res[[nmStatCode]]))
          break
        }
        
        res <- content(x = res, as = "parsed")
        
        if ("messages" %in% names(res)) {
          if (length(res$messages) > 0) {
            nAdd <- length(res$messages)
            msgs[length(msgs) + 1:nAdd] <- res$messages
          }
        }
        
        bCont <- FALSE
        xURL <- NULL
        
        if ("links" %in% names(res)) {
          if ("next" %in% names(res$links)) {
            if (length(res$links[["next"]]) == 1) {
              if (nchar(res$links[["next"]]) > 0) {
                xURL <- sprintf("https://testnet.mirrornode.hedera.com%s", 
                                res$links[["next"]])
                bCont <- TRUE
              }
            }
          }
        }
      }
      
    }
    
    if (length(msgs) == 0) {
      return(NULL)
    }
  }
  
  # Decode the messages' contents.
  {
    msgConts <- lapply(X = msgs, FUN = function(x) {
      return(jsonlite::fromJSON(
        rawToChar(openssl::base64_decode(text = x$message))))
    })
    names(msgConts) <- sapply(X = msgs, FUN = function(x) x$consensus_timestamp)
  }
  
  # Go through all the collected messages and try to find the one that published
  # our document.
  {
    msg <- NULL
    for (k in 1:length(msgConts)) {

      x <- msgConts[[k]]
      if (!("cid" %in% names(x))) { next }
      if (length(x$cid) == 0) { next }
      
      if (exists("z")) { rm(z) }
      
      tryCatch({
        zUrl <- sprintf("https://%s.ipfs.w3s.link/", x$cid)
        z <- httr::GET(url = zUrl)
      }, error = function(e) {
        warning(sprintf("GET query failed for %s.\nError: %s", zUrl, as.character(e)))
      })
      
      if (!exists("z")) { next }
      
      nmStatCode <- intersect(c("code", "statusCode", "status_code"), names(z))
      if (length(nmStatCode) == 0) {
        warning("ERROR: Failed to determine status code field in response.")
        next
      }
      if (z[[nmStatCode]] != 200) { next }
      
      if (z$headers$`content-type` == "application/zip") { next }
      
      if (exists("bSucc")) { rm(bSucc) }
      
      tryCatch({
        z <- content(x = z, as = "parsed")
        bSucc <- TRUE
      }, error = function(e) {
        message(sprintf("ERROR: %s", as.character(e)))
      })
      if (!exists("bSucc")) { next }
      #z <- jsonlite::fromJSON(z)
      if (!("id" %in% names(z))) { next }
      if (z$id == docUuid) {
        msg <- x
        msg$messageId <- names(msgConts)[k]
        break
      }
    }
    
    return(msg)
  }
  
}
