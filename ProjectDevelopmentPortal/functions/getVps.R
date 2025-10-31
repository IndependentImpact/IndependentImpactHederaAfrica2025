
getVps <- function(dbCon,
                   tknIds = NULL,
                   tknNms = NULL, 
                   tknSymbols = NULL, 
                   idAgent, 
                   idProject = NULL,
                   mostRecent = FALSE) {
  
  # Input checking.
  if (all(sapply(list(tknIds, tknNms, tknSymbols), is.null))) {
    stop("No token specified.")
  }
  
  # Get the db metadata of the relevant tokens.
  if (length(tknIds) > 0) {
    q <- sprintf("SELECT * FROM tbl_tokens WHERE id IN(%s);", 
                 paste(sprintf("'%s'", tknIds), collapse = ","))
  } else {
    if (length(tknNms) > 0) {
      q <- sprintf("SELECT * FROM tbl_tokens WHERE name IN(%s);", 
                   paste(sprintf("'%s'", tknNms), collapse = ","))
    } else {
      if (length(idProject) == 0) {
        q <- sprintf("SELECT * FROM tbl_tokens WHERE symbol IN(%s);", 
                     paste(sprintf("'%s'", tknSymbols), collapse = ","))
      } else {
        
        # Reaching this point means length(idProject) >= 1 and only tknSymbols 
        # has been provided.
        
        # 1. Get the message IDs of the policies associated with the project.
        q <- sprintf("SELECT * FROM tbl_link_projects_x_agents WHERE id_project = '%s' AND id_agent = '%s';", 
                     idProject, idAgent)
        dfRes <- dbGetQuery(dbCon, q)
        
        # 2. Get the IDs of the tokens associated with the policies of step 1.
        q <- sprintf("SELECT * FROM tbl_link_tokens_x_policies WHERE id_message_policy IN(%s);",
                     paste(sprintf("'%s'", dfRes$id_msg_policy), 
                           collapse = ","))
        dfRes <- dbGetQuery(dbCon, q)
        
        # 3. Build q to retrieve the metadata of the tokens of step 2 who have the specified symbol.  
        q <- sprintf("SELECT * FROM tbl_tokens WHERE id IN(%s) AND symbol IN(%s);",
                     paste(sprintf("'%s'", dfRes$id_token), 
                           collapse = ","),
                     paste(sprintf("'%s'", tknSymbols), 
                           collapse = ","))
        
      }
    }
  }
  dfTkns <- dbGetQuery(dbCon, q)

  # Get the agent's Hedera account IDs.
  {
    q <- sprintf("SELECT * FROM tbl_link_agents_x_hedera_accounts WHERE id_agent = '%s';",
                 idAgent)
    dfHaccs <- dbGetQuery(dbCon, q)
  }
  
  # Get all instances of the token owned by this agent.
  {
    dfNfts <- do.call(
      "rbind.fill", 
      lapply(X = dfTkns$id_token_h, FUN = function(idTkn) {
        return(
          do.call(
            "rbind.fill", 
            lapply(X = dfHaccs$id_acc_h, FUN = function(idAccH) {
              return(
                hedera::getNfts(
                  tokenId = idTkn, 
                  accountId = idAccH, 
                  network = hederaNetwork, 
                  asDf = TRUE))
            })))
      }))
    if (length(dfNfts) == 0) { return(NULL) }
  
    dfNfts <- dfNfts[which(!dfNfts$deleted),]
    if (nrow(dfNfts) == 0) {
      return(NULL)
    }
  }

  # Retrieve some more information about each of the token instances.
  {
    dfNfts[c("id_message", "url_ipfs")] <- NA_character_
    for (r in 1:nrow(dfNfts)) {
      memo <- rawToChar(
        openssl::base64_decode(
          text = dfNfts$metadata[r]), 
        multiple = FALSE)
      msg <- hedera::getMessage(
        messageId = memo, 
        network = hederaNetwork, 
        decode = TRUE)[["message"]]
      dfNfts$id_message[r] <- msg$messageId
      dfNfts$url_ipfs[r] <- sprintf("https://%s.ipfs.w3s.link", msg$cid)
    }
    dfNfts$metadata <- NULL
  }

  # Convert the message ID to a human-friendly timestamp.
  {
    dfNfts$ts_issued <- as.POSIXct.numeric(
      x = as.numeric(dfNfts$id_message), 
      origin = "1970-01-01 00:00:00.0", 
      tz = "UTC")
  }

  # Order in reverse chronological order.
  dfNfts <- dfNfts[order(as.numeric(dfNfts$id_message), decreasing = TRUE),]
  
  # If mostRecent is TRUE, keep only the most recent instance of each token.
  if (mostRecent) {
    dfNfts <- dfNfts[which(!duplicated(dfNfts$token_id)),]
  }
  
  # Enforce proper row names.
  rownames(dfNfts) <- 1:nrow(dfNfts)
  
  # Done.
  return(dfNfts)
}