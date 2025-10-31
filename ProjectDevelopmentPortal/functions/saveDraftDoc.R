
#'@param idAgent Character. Our db ID for the agent creating (saving) the 
#'  document - regardless of who or what the document is about. 
#'@param idEntity Character. Our db ID for the agent or project that this
#'  document is about - regardless of who is currently creating (saving) the 
#'  document.
#
saveDraftDoc <- function(x,
                         didAgent,
                         idEntity, 
                         docId = NULL,
                         idSchema,
                         idWorkflow,
                         stepWorkflow,
                         dbCon) {
  
  # TODO: Remove after debugging.
  # lsX_saveDraftToDb <- list(
  #   x = x,
  #   didAgent = didAgent,
  #   idEntity = idEntity, 
  #   docId = docId,
  #   idSchema = idSchema,
  #   idWorkflow = idWorkflow,
  #   stepWorkflow = stepWorkflow)
  # save(lsX_saveDraftToDb, file = sprintf("%slsX_saveDraftToDb.Rda", tmpdir))
  
  tblNmMD <- "tbl_document_metadata"
  
  # Extract the document's identifying content.
  {
    idCont <- NULL
    tryCatch({
      idCont <- extractIdentifyingContent(
        doc = x, 
        schemaId = idSchema,
        dbCon = dbCon)
    }, error = function(e) {
      warning("Failed to extract identifying content from document: ", as.character(e))
    })
    
    if (length(idCont) > 0) {
      idContAsJson <- jsonlite::toJSON(idCont, auto_unbox = TRUE)
    }
  }
  
  # Extract 'id_msg_pred' if such exists in the document.
  idMsgPred <- NULL
  if ("id_msg_pred" %in% names(x)) {
    idMsgPred <- x$id_msg_pred
  } else {
    if ("headers" %in% names(x)) {
      if ("id_msg_pred" %in% names(x$headers))
        idMsgPred <- x$headers$id_msg_pred
    }
  }
  
  # Write to tbl_document_metadata.
  if (length(docId) == 0) {
    
    dfAdd <- data.frame(
      id_entity = idEntity,
      id_schema = idSchema,
      id_workflow = idWorkflow,
      step_workflow = stepWorkflow,
      date_created = lubridate::now(tzone = "UTC"),
      date_modified = lubridate::now(tzone = "UTC"),
      status = "DRAFT",
      did_author = didAgent)
    
    if (length(idCont) > 0) {
      dfAdd$identifying_content <- idContAsJson
    }
    
    if (length(idMsgPred) == 1) {
      dfAdd$id_msg_pred <- idMsgPred
    } 
    
    docId <- addToDb(
      dfAdd = dfAdd, 
      tblNm = tblNmMD, 
      vnmsChckEx = c("id_entity", "id_schema", "id_workflow", "step_workflow", 
                     "did_author", "status"), 
      dbCon = dbCon, 
      calcIds = TRUE,
      returnIds = TRUE)
    
  } else {
    # Update existing record.
    
    q <- sprintf("UPDATE %s SET date_modified = '%s' WHERE id = '%s';",
                 tblNmMD, lubridate::now(tzone = "UTC"), docId)
    dbSendStatement(conn = dbCon, statement = q)
    
    if (length(idCont) > 0) {
      q <- sprintf("UPDATE %s SET identifying_content = '%s' WHERE id = '%s';", 
                   tblNmMD, idContAsJson, docId)
      dbExecute(conn = dbCon, statement = q)
    }
    
    if (length(idMsgPred) == 1) {
      q <- sprintf("UPDATE %s SET id_msg_pred = '%s' WHERE id = '%s';", 
                   tblNmMD, idMsgPred, docId)
      dbExecute(conn = dbCon, statement = q)
    } 
  }
  
  
  # TODO. Remove after debugging.
  message("draftdocdir = ", draftdocdir)
  message("idEntity = ", idEntity)
  message("docId = ", docId)
  
  
  
  # Save the draft document as a .rds file.
  saveRDS(
    object = x, 
    file = sprintf("%s%s_%s.rds", draftdocdir, idEntity, docId))
  
  return(docId)
  
}
