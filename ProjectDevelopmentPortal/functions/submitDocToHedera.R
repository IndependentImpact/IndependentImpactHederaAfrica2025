
#'@param doc A named list. One list item per document field. A list item can
#'  be a list in itself.
#'@param idAgent Character. Our db ID for the agent submitting (i.e., publishing 
#'  or issuing) the document - regardless of who or what the document is about. 
#'@param idEntity Character. Our db ID for the agent or project that this
#'  document is about or to whom the document will ultimately belong - regardless 
#'  of who is currently submitting the document.
# 
submitDocToHedera <- function(doc, 
                              idDoc = NULL,
                              dbCon,
                              idSchema,
                              idWorkflow,
                              stepWorkflow,
                              idAgent, 
                              idEntity, 
                              hederaClient,
                              docTypeAs = c("regular", "vc")[1]) {
  
  message("cp1")
  
  # Some input checking on argument 'idDoc'.
  if (length(idDoc) == 1) {
    
    message("cp2")
    
    if (is.na(idDoc)) {
      idDoc <- NULL
    } else {
      if (nchar(idDoc) == 0) {
        idDoc <- NULL
      }
    }
    
    # Check if the idDoc provided actually exists.
    if (length(idDoc) == 1) {
      
      message("cp3")
      
      res <- dbFetch(
        dbSendQuery(
          conn = dbCon, 
          statement = sprintf("SELECT id FROM tbl_document_metadata WHERE id = '%s';",
                              idDoc)))
      if (nrow(res) == 0) {
        warning(sprintf("No entry found for document %s in tbl_document_metadata. Creating new document with different ID.",
                        idDoc))
        idDoc <- NULL
      }
    }
    
  }
  
  message("cp4")
  
  # First make sure that this document has not already been submitted.
  if (length(idDoc) == 1) {
    
    message("cp5")
    
    dfDoc <- dbFetch(
      dbSendQuery(
        conn = dbCon, 
        statement = sprintf(
          "SELECT * FROM tbl_document_metadata WHERE id_entity = '%s' AND id = '%s' AND id_workflow = '%s' AND id_schema = '%s' AND step_workflow = '%s';",
          idEntity, idDoc, idWorkflow, idSchema, stepWorkflow)))
    if (nrow(dfDoc) > 0) {
      if (any(dfDoc$status == 'PUBLISHED')) {
        message(sprintf("Document already submitted. Hedera message ID: %s", 
                        dfDoc$id_message_h))
        return(list(status_code = 200,
                    doc_md = dfDoc,
                    err_msg = NULL))
      }
    }
  }
  
  message("cp6")
  
  # Get some more schema info.
  {
    q <- sprintf("SELECT title, iri, uri_ipfs, b_encrypt FROM tbl_schemas WHERE id = '%s';",
                 idSchema)
    dfSchMd <- dbGetQuery(conn = dbCon, statement = q)
  }
  
  # Format the document correctly according to the schema.
  {
    if (docTypeAs == "regular") {
      
      # 1. Get the schema details.
      {
        sch <- jsonlite::read_json(
          path = sprintf("%s%s.json", schemadir, dfSchMd$iri),
          simplifyVector = TRUE,
          simplifyDataFrame = FALSE,
          flatten = FALSE)
        
        if (length(sch) == 0) {
          return(list(status_code = -2,
                      doc_md = NULL,
                      err_msg = "Failed to retrieve schema definition."))
        }
        
        dfSchema <- schemaToDf(sch)
      }
      
      # 2. Format the document according to the schema.
      bDocPrepSucc <- tryCatch({
        doc <- formatDocToSchema(
          doc = doc, 
          dfSchema = dfSchema, 
          iriSchema = dfSchMd$iri, 
          ipfsUriSchema = dfSchMd$uri_ipfs)
        TRUE
      }, error = function(e) {
        message(as.character(e))
        FALSE
      })
      
      if (!bDocPrepSucc) {
        return(list(status_code = -3,
                    doc_md = NULL,
                    err_msg = "Document preparation failed."))
      }
      
    }
  }
  
  message("cp7")
  
  # Extract 'id_msg_pred' if such exists in the document.
  idMsgPred <- NULL
  if ("id_msg_pred" %in% names(doc)) {
    idMsgPred <- doc$id_msg_pred
  } else {
    if ("headers" %in% names(doc)) {
      if ("id_msg_pred" %in% names(doc$headers)) {
        idMsgPred <- doc$headers$id_msg_pred
      }
    } 
  }
  
  # Extract the document's identifying content.
  {
    idCont <- NULL
    idContAsJson <- NULL
    tryCatch({
      idCont <- extractIdentifyingContent(
        doc = doc, 
        schemaId = idSchema,
        dbCon = dbCon)
    }, error = function(e) {
      warning("Failed to extract identifying content from document.")
    })
    
    if (length(idCont) > 0) {
      idContAsJson <- jsonlite::toJSON(idCont, auto_unbox = TRUE)
    }
  }
  
  # Sign the doc or convert it to a VC.
  if (docTypeAs == "regular") {
    
    doc <- list(
      document = doc,
      signature = signRegularDoc(
        doc = doc, 
        idAgent = idAgent, 
        dbCon = dbCon))
    
  } else {
    #if (docTypeAs == "vc") {
    
    doc <- createVerifCred(
      content = doc, 
      idIssuer = idAgent, 
      idSubject = idEntity, 
      dbCon = dbCon, 
      vcType = "jws")
    
    #}
  }
  
  # Upload the document to IPFS.
  {
    # Determine if the document should be encrypted before upload to IPFS.
    {
      # If this document is based on the Generic Document Review Schema, we 
      # should encrypt this document if it's predecessor (the document that was 
      # reviewed) had to be encrypted, otherwise we can use the db value for 
      # b_encrypt for this schema itself.
      
      if (dfSchMd$title == "Generic Document Review") {
        
        if (length(idMsgPred) != 1) {
          return(list(status_code = -3,
                      doc_md = NULL,
                      err_msg = "Failed to determine value for bEncrypt."))
        }
        
        q <- sprintf("SELECT id_schema FROM tbl_document_metadata WHERE id_message_h = '%s';",
                     idMsgPred)
        idSchPred <- dbGetQuery(conn = dbCon, statement = q)[["id_schema"]][1]
        
        # TODO: What if this is a review of a review, so the predecessor was
        # also based on schema "Generic Document Review"?
        
        q <- sprintf("SELECT b_encrypt FROM tbl_schemas WHERE id = '%s';",
                     idSchPred)
        bEncrypt <- dbGetQuery(conn = dbCon, statement = q)[["b_encrypt"]]
        
      } else {
        bEncrypt <- dfSchMd$b_encrypt
      }
      
    }
    
    fp <- tempfile(fileext = ".json")
    cat(jsonlite::toJSON(x = doc, auto_unbox = TRUE), file = fp)
    
    ipfsCid <- uploadToIpfs(
      df = data.frame(
        datapath = fp, 
        name = "doc.json",
        ext = ".json"), 
      encrypt = bEncrypt, 
      zip = FALSE, 
      cidOnly = TRUE, 
      wrap = FALSE)
    
    file.remove(fp)
  }
  
  #bafkreiabn3rhfvwni4j4swxorrqb6ajy3lufligen2lb4virxeqw2nfm3u # not encrypted
  #bafkreigz5yi5au5rcmtsnmgfpidzwxmguvw5rvcbjoistiw5r24hwo46h4 # encrypted
  message("cp8")
  
  # Submit the IPFS URI of the document to the entity's Hedera topic.
  {
    # Determine the topic ID.
    {
      q <- sprintf("SELECT * FROM tbl_link_entities_x_hedera_topics WHERE id_entity = '%s';",
                   idEntity)
      topicId <- dbGetQuery(conn = dbCon, statement = q)[["id_topic_h"]]
    }
    
    # Get the agent's DID.
    q <- sprintf("SELECT did FROM tbl_link_agents_x_dids WHERE id_agent = '%s';",
                 idAgent)
    didAgent <- dbGetQuery(conn = dbCon, statement = q)[["did"]]
    
    # Get some workflow metadata.
    dfWrkflwMd <- dbGetQuery(
      conn = dbCon, 
      statement = sprintf("SELECT title, tag_version FROM tbl_workflows WHERE id = '%s';",
                          idWorkflow))
    
    # Compile the message.
    msg <- list(
      documentIpfsUri = paste0("ipfs://", ipfsCid),
      submittedByDid = didAgent,
      workflowTitle = dfWrkflwMd$title,
      workflowVersion = dfWrkflwMd$tag_version,
      workflowId = idWorkflow,
      workflowStep = stepWorkflow)
    msg <- jsonlite::toJSON(x = msg, auto_unbox = TRUE)
    
    # Fetch the submit key for the topic.
    {
      q <- sprintf("SELECT * FROM tbl_link_hedera_topics_x_key_pairs WHERE id_topic_h = '%s' AND (label_key_pair = 'SUBMIT' OR label_key_pair = 'ADMIN');",
                   topicId)
      dfTopicKeys <- dbGetQuery(conn = dbCon, statement = q)
      
      if (length(dfTopicKeys) == 0) {
        return(list(status_code = -4,
                    doc_md = NULL,
                    err_msg = "Failed to retrieve topic key pair."))
      }
      if (nrow(dfTopicKeys) == 0) {
        return(list(status_code = -4,
                    doc_md = NULL,
                    err_msg = "Failed to retrieve topic key pair."))
      }
      
      if (nrow(dfTopicKeys) > 1) {
        # Subset to the most recent pair for each label.
        dfTopicKeys <- dfTopicKeys[order(dfTopicKeys$oidx, decreasing = TRUE),]
        dfTopicKeys <- dfTopicKeys[which(!duplicated(dfTopicKeys$label_key_pair)),]
      }
      
      idx <- which(dfTopicKeys$label_key_pair == "SUBMIT")
      if (length(idx) == 0) {
        idx <- which(dfTopicKeys$label_key_pair == "ADMIN")
      }
      topicSubmitKeyPairId <- dfTopicKeys$id_key_pair[idx]
      
      q <- sprintf("SELECT * FROM tbl_key_pairs WHERE id = '%s';", topicSubmitKeyPairId)
      dfKeyPair <- dbGetQuery(conn = dbCon, statement = q)
      
      decrRaw <- openssl::base64_decode(dfKeyPair$private_key_encr)
      decrChar <- cyphr::decrypt_string(
        data = decrRaw,
        key = cyphr::keypair_openssl(
          pub = iwefdj$KEYPTH_CYPHR,
          key = iwefdj$KEYPTH_CYPHR,
          envelope = TRUE,
          password = iwefdj$PW_CYPHR))
      
      topicSubmitPrivKey <- hiero$PrivateKey$from_string(decrChar)
    }
    
    # Submit the message to the topic.
    {
      tx <- hiero$TopicMessageSubmitTransaction(
        topic_id = hiero$TopicId$from_string(topicId), 
        message = msg)
      tx <- tx$freeze_with(hederaClient)
      tx <- tx$sign(topicSubmitPrivKey) 
      tx <- tx$sign(hederaClient$operator_private_key)
      resp <- tx$execute(hederaClient)
      respProto <- resp$to_proto()
      
      if (resp$status != 22) {
        return(
          list(
            status_code = -5,
            doc_md = NULL,
            err_msg = sprintf("Failed to submit message to topic %s. Hedera status code: %s", 
                              topicId, resp$status)))
      }
    }
    
    # Try to get the message ID of the topic submission.
    {
      # TODO. Store the topic sequence number of the message in the db so that
      # we can try to retrieve the message again later if we don't succeed now.
      
      nTries <- 0
      retrMsg <- NULL
      while (nTries < 3 & (length(retrMsg) == 0)) {
        
        retrMsg <- tryCatch({
          hedera::getMessageFromTopic(
            topicId = topicId, 
            msgSeqNo = respProto$topicSequenceNumber, 
            network = hederaClient$network$network, 
            decode = FALSE)
        }, error = function(e) {
          Sys.sleep(time = nTries+1) # The network likely just needs some time.
          NULL
        })
        
        nTries <- nTries + 1
      }
      
      if (length(retrMsg) == 0) {
        return(
          list(
            status_code = -5,
            doc_md = NULL,
            err_msg = sprintf(
              "Failed to retrieve message #%d from topic %s.", 
              respProto$topicSequenceNumber, topicId)))
      }
    }
  }
  
  message("cp13")
  
  # Delete the draft version of this document, if any.
  if (length(idDoc) == 1) {
    fpDraft <- sprintf("%s%s_%s.rds", draftdocdir, idEntity, idDoc)
    if (file.exists(fpDraft)) { file.remove(fpDraft) }
  }
  
  message("cp14")
  
  # Update/insert the document's details in tbl_document_metadata.
  {
    # Add to / update db.
    if (length(idDoc) == 1) { # Update existing record in tbl_document_metadata.
      
      res <- dbSendStatement(
        conn = dbCon, 
        statement = sprintf(
          "UPDATE tbl_document_metadata SET id_workflow = '%s', step_workflow = '%s', status = 'PUBLISHED', outcome = 'PENDING', uri_ipfs = '%s', id_message_h = '%s', type_doc = '%s', encrypted = %s WHERE id = '%s';",
          idWorkflow, 
          stepWorkflow, 
          paste0("ipfs://", ipfsCid),
          retrMsg$consensus_timestamp,
          switch(docTypeAs,
                 regular = "REGULAR_SIGNED",
                 vc = "VC"),
          tolower(as.character(bEncrypt)),
          idDoc))
      dbClearResult(res)
      
      if (length(idCont) > 0) {
        q <- sprintf("UPDATE tbl_document_metadata SET identifying_content = '%s' WHERE id = '%s';", 
                     idContAsJson, idDoc)
        dbExecute(conn = dbCon, statement = q)
      }
      
      if (length(idMsgPred) == 1) {
        q <- sprintf("UPDATE tbl_document_metadata SET id_msg_pred = '%s' WHERE id = '%s';", 
                     idMsgPred, idDoc)
        dbExecute(conn = dbCon, statement = q)
      } 
      
    } else { # Add new record to tbl_document_metadata.
      
      # Build dfAdd.
      tsDoc <- as.character(lubridate::now(tzone = "UTC"))
      dfAdd <- data.frame(
        id_entity = idEntity,
        id_schema = idSchema,
        date_created = tsDoc,
        date_modified = tsDoc,
        did_author = didAgent,
        id_workflow = idWorkflow,
        step_workflow = stepWorkflow,
        uri_ipfs = paste0("ipfs://", ipfsCid),
        id_message_h = retrMsg$consensus_timestamp,
        type_doc = switch(
          docTypeAs,
          regular = "REGULAR_SIGNED",
          vc = "VC"),
        status = "PUBLISHED",
        outcome = "PENDING",
        encrypted = tolower(as.character(bEncrypt)))
      
      if (length(idCont) > 0) {
        dfAdd$identifying_content <- idContAsJson
      }
      
      if (length(idMsgPred) == 1) {
        dfAdd$id_msg_pred <- idMsgPred
      } 
      
      idDoc <- addToDb(
        dfAdd = dfAdd, 
        tblNm = "tbl_document_metadata", 
        vnmsChckEx = c("id_schema", 
                       "date_created", 
                       "date_modified", 
                       "did_author"), 
        dbCon = dbCon, 
        calcIds = TRUE,
        returnIds = TRUE)
    }
  }
  
  message("cp15")
  
  # Retrieve the updated details.
  dfDoc <- dbFetch(
    dbSendQuery(conn = dbCon,
                statement = sprintf("SELECT * FROM tbl_document_metadata WHERE id = '%s';",
                                    idDoc)))
  
  # Done.
  return(list(status_code = 200,
              doc_md = dfDoc,
              err_msg = NULL))
}


# Aux functions to submitDocToHedera() ----------------------------------------------

formatDocToSchema <- function(doc, dfSchema, iriSchema, ipfsUriSchema) {
  
  # 1. Remove any fields that are NULL, NA, or of length = 0.
  {
    doc <- lapply(X = doc, FUN = function(x) {
      if (is.null(x)) { return(NULL) }
      if (length(x) == 0) { return(NULL) }
      if (length(x) == 1) { 
        if (is.na(x)) { return(NULL) }
      }
      return(x)
    })
    
    idxx <- which(sapply(X = doc, FUN = is.null))
    if (length(idxx) > 0) {
      doc <- doc[-idxx]
    }
  }
  
  # 2. If there are any required fields that are missing, abort.
  {
    vnms <- setdiff(dfSchema$key[which(dfSchema$required)],
                    names(doc))
    if (length(vnms) > 0) {
      stop(sprintf("The following required fields are missing: %s", 
                   paste(sprintf("'%s'", vnms), 
                         collapse = ",")))
    }
  }
  
  # 3. Remove any fields not in schema.
  {
    vnms <- setdiff(names(doc), dfSchema$key)
    if (length(vnms) > 0) {
      message(sprintf("WARNING: Removing the following unmatched fields from the document: %s", 
                      paste(sprintf("'%s'", vnms), 
                            collapse = ",")))
      doc[vnms] <- NULL
    }
  }
  
  # 4. Format the fields correctly according to type; also add the 'type' and
  # '@context' variables required by the Guardian.
  for (vnm in names(doc)) {
    
    #message(vnm)
    
    # Note: We have to do this field by field, because any field can be a 
    # composite field based on another schema.
    
    x <- doc[[vnm]]
    idx <- which(dfSchema$key == vnm)
    
    if (!dfSchema$repeated[idx]) {
      # Let's quickly make x a list, so that we can treat x as an 
      # unrepeated field the same as when x is a repeated field. We'll 
      # undo this later.
      x <- list(x)
    } else {
      # Remove indexes as list item names.
      names(x) <- NULL
    }
    
    if (dfSchema$type[idx] == "complex") {
      
      x <- lapply(X = x, FUN = function(y) {
        y <- formatDocToSchema(
          doc = y, 
          dfSchema = dfSchema$format[[idx]],
          iriSchema = dfSchema$iri_schema[idx], 
          ipfsUriSchema = dfSchema$url_ipfs[idx])
        return(y)
      })
      
    } else { # i.e., dfSchema$type[idx] != "complex"
      
      x <- lapply(X = x, FUN = function(y) {
        y <- .formatFld(x = y, 
                        dfFieldInfo = dfSchema[idx,])
        return(y)
      })
      
    }
    
    if (!dfSchema$repeated[idx]) {
      # Undoing, as promised...
      x <- x[[1]]
    }
    
    doc[[vnm]] <- x 
  }
  
  # 5. Add the 'type' and '@context' variables (required by the Guardian).
  {
    # Make sure the IPFS URI is in the correct format.
    {
      for (m in c("https://", "http://", ".ipfs.w3s.link")) {
        ipfsUriSchema <- gsub(pattern = m, 
                              replacement = "", 
                              x = ipfsUriSchema, 
                              fixed = TRUE)
      }
      if (length(grep(pattern = "ipfs://", x = ipfsUriSchema, fixed = TRUE)) == 0) {
        ipfsUriSchema <- sprintf("ipfs://%s", ipfsUriSchema)
      }
    }
    
    doc$type <- gsub(pattern = "^#{1}", 
                     replacement = "", 
                     x = iriSchema)
    doc[["@context"]] <- ipfsUriSchema
  }
  
  # Done.
  return(doc)
  
}


.formatFld <- function(x, dfFieldInfo) {
  
  cTypeMap <- c(
    number = "numeric",
    integer = "integer",
    string = "character",
    'string:date' = "character",
    boolean = "logical")
  
  if (length(x) == 0) { return("") } 
  if (is.na(x)) { return("") }
  
  # Deal with date-time fields.
  if (dfFieldInfo$type == "string:date-time") {
    
    x <- as.character(x)
    
    # If it's already in the correct format, just return it as is.
    if (length(grep(pattern = "^[[:digit:]]{4}-[[:digit:]]{1,2}-[[:digit:]]{1,2}T[[:digit:]]{1,2}:[[:digit:]]{1,2}:[[:digit:]]{1,2}.000Z$", x = x)) == 1) {
      return(x)
    }
    
    # If it's currently just the date component that exists, do:
    if (length(grep(pattern = "^[[:digit:]]{4}-[[:digit:]]{1,2}-[[:digit:]]{1,2}$", x = x)) == 1) {
      x <- sprintf("%sT00:00:00.000Z", x)
      return(x)
    }
    
    # If it's currently only 'YMD HMS', do:
    if (length(grep(pattern = "^[[:digit:]]{4}-[[:digit:]]{1,2}-[[:digit:]]{1,2}[[:blank:]]{1}[[:digit:]]{1,2}:[[:digit:]]{1,2}:[[:digit:]]{1,2}$", x = x)) == 1) {
      x <- gsub(pattern = "[[:blank:]]", replacement = "T", x = x)
      x <- sprintf("%s.000Z", x)
      return(x)
    }
    
    # Reaching this point means it's something quite unexpected.    
    stop(sprintf("Field '%s' contains an invalid date-time value ('%s').", 
                 dfFieldInfo$key, x))
  }
  
  # All field types other than date-time fields.
  x <- as(x, cTypeMap[[dfFieldInfo$type]])
  
  # If the field is not a string field, there's no further formatting we can do,
  # so just return.
  if (dfFieldInfo$type != "string") { return(x) }
  
  # If the field is not an enum, there's no further formatting we can do, so 
  # just return.
  if (!("enum" %in% names(dfFieldInfo))) { return(x) }
  
  # Deal with enum fields.
  {
    if (is.na(dfFieldInfo$enum)) { return(x) }
    validVals <- strsplit(x = dfFieldInfo$enum, 
                          split = ",", 
                          fixed = TRUE)[[1]]
    if (!(x %in% validVals)) {
      stop(sprintf("Field '%s' contains an invalid value ('%s'). Valid values are: %s.", 
                   dfFieldInfo$key, x, 
                   paste(sprintf("'%s'", validVals), 
                         collapse = ",")))
    }
  }
  
  return(x)
  
  # --- NOTE: IT IS SUPER NB TO REPLACE ANY NULLS WITH ''! ---------
  # If we don't replace them, the document gets quietly "discarded"/ignored by 
  # the Guardian, even though the POST command below will return 200. Or maybe
  # it is httr::POST which does not treat NULLs correctly when converting the 
  # document to JSON... Hm.
  # DON'T REMOVE THE NULL FIELDS COMPLETELY! If they are required fields 
  # according to the schema, any document submitted without them will also be 
  # quietly discarded/ignored by the Guardian.
  
}






