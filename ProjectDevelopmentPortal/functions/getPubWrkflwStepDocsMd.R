
# Retrieves, form our db, the metadata of all PUBLISHED documents submitted to a 
# specific step of a specific workflow and for a specific entity. 
#
getPubWrkflwStepDocsMd <- function(workflowId = NULL,
                                   nmWrkflwHndlr = NULL,
                                   workflowStep,
                                   entityId, 
                                   dbCon, 
                                   lsFilters = NULL) {
  
  # Input checking.
  if (length(workflowId) == 0 & length(nmWrkflwHndlr) == 0) {
    stop("At least one of 'workflowId' and 'nmWrkflwHndlr' must be specified.")
  }
  
  # Determine the workflow IDs, if not provided.
  if (length(workflowId) > 0) {
    workflowIds <- workflowId
  } else {
    q <- sprintf("SELECT id FROM tbl_workflows WHERE handler_r = '%s';", 
                 nmWrkflwHndlr)
    workflowIds <- dbGetQuery(dbCon, q)$id
  }
  
  if (length(workflowIds) == 0) {
    # Should really only happen during testing.
    return(NULL)
  }
  
  # Get the metadata of the relevant documents.
  dfDocMd <- dbFetch(
    dbSendQuery(
      conn = dbCon, 
      statement = sprintf("SELECT * FROM tbl_document_metadata WHERE id_entity = '%s' AND id_workflow IN(%s) AND step_workflow = '%s' AND status = 'PUBLISHED';", 
                          entityId,
                          paste(sprintf("'%s'", workflowIds), collapse = ","),
                          workflowStep)))
  
  if (nrow(dfDocMd) == 0) {
    return(dfDocMd)
  }
  
  # # Add a 'handler_r_wrkflw' field to dfDocMd.
  # {
  #   if (length(workflowIds) == 1 & !is.null(nmWrkflwHndlr)) {
  #     dfDocMd$handler_r_wrkflw <- nmWrkflwHndlr
  #   } else {
  #     q <- sprintf("SELECT id, handler_r FROM tbl_workflows WHERE id IN(%s);", 
  #                  paste(sprintf("'%s'", workflowIds), collapse = ","))
  #     res <- dbGetQuery(dbCon, q)
  #     idxx <- match(x = dfDocMd$id_workflow, table = res$id)
  #     dfDocMd$handler_r_wrkflw <- res$handler_r[idxx]
  #   }
  # }
  
  # Fix the dates' timezones.
  dfDocMd$date_created <- lubridate::force_tz(
    time = dfDocMd$date_created, 
    tzone = "UTC")
  dfDocMd$date_modified <- lubridate::force_tz(
    time = dfDocMd$date_modified, 
    tzone = "UTC")
  
  # Apply additional filters, if specified.
  if (length(lsFilters) > 0) {
    
    # Some filter variables may already be in dfDocMd, other may need to be
    # retrieved from the document contents.
    vnmsAdd <- setdiff(names(lsFilters), names(dfDocMd))
    if (length(vnmsAdd) > 0) {
      
      # Add placeholders to dfDocMd for the filter values.
      dfDocMd[,vnmsAdd] <- NA_character_
      
      # Extract the filter fields from the candidate VCs.
      for (r in 1:nrow(dfDocMd)) {
        tryCatch({
          doc <- getPubDoc(
            docId = dfDocMd$id[r],
            dbCon = dbCon,
            contentOnly = FALSE,
            decrypt = TRUE)[[1]]
          for (nm in vnmsAdd) {
            dfDocMd[r,nm] <- doc[[nm]]
          }
        }, error = function(e) {
          #save(dfDocMd, file = sprintf("%sdfDocMd.Rda", tmpdir))
          message(sprintf("WARNING: Failed to extract filter fields from VC (r = %d). Error: %s", 
                          r, e))
        })
      }
    }
    
    # Apply the filters.
    idxxOK <- 1:nrow(dfDocMd)
    for (nm in names(lsFilters)) { 
      idxx <- which(dfDocMd[[nm]] == lsFilters[[nm]])
      idxxOK <- intersect(idxxOK, idxx)
    }
    dfDocMd <- dfDocMd[idxxOK,]
    
    if (nrow(dfDocMd) == 0) {
      return(dfDocMd)
    } 
    
  } 
  
  # # If there are any of the retrieved documents that do not have message
  # # IDs and/or IPFS URLs yet, try to retrieve those details for them.
  # dfDocMd <- updateVcPublicationInfo(
  #   df = dfDocMd, 
  #   dbCon = dbCon, 
  #   bUpdateDbTbl = TRUE, 
  #   tblNm = sprintf("tbl_document_metadata_%s", entityId))
  # 
  # # If there are documents for which we were not able to retrieve the
  # # message ID and IPFS URL, remove them from the options list.
  # dfDocMd <- dfDocMd[which(!is.na(dfDocMd$id_message)),]                  
  
  # Try to retrieve human-friendly names of the issuers of each document.
  {
    q <- sprintf("SELECT * FROM tbl_link_agents_x_dids WHERE did IN(%s);",
                 paste(sprintf("'%s'", unique(dfDocMd$did_author)), 
                       collapse = ","))
    dfAgDids <- dbGetQuery(conn = dbCon, statement = q)
    dfAgDids$oidx <- NULL
    
    q <- sprintf("SELECT * FROM tbl_link_agents_x_email_addresses WHERE id_agent IN(%s);",
                 paste(sprintf("'%s'", unique(dfAgDids$id_agent)), collapse = ","))
    dfAgEmails <- dbGetQuery(conn = dbCon, statement = q)
    dfAgEmails$oidx <- NULL
    
    dfEms <- merge(x = dfAgDids, 
                   y = dfAgEmails, 
                   by = "id_agent", 
                   all.x = TRUE); rm(dfAgDids, dfAgEmails)
    
    dfDocMd$email_addr_issuer <- NA_character_
    idxx <- match(x = dfDocMd$did_issuer, table = dfEms$did)
    dfDocMd$email_addr_issuer[which(!is.na(idxx))] <- dfEms$email_address[idxx[!is.na(idxx)]]
  }
  
  # Finishing touches.
  dfDocMd <- dfDocMd[order(dfDocMd$date_modified, decreasing = TRUE),]
  rownames(dfDocMd) <- 1:nrow(dfDocMd)
  
  # Done.
  return(dfDocMd)  
  
}
