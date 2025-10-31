
# Note 1: A workflow is really just about who must submit which documents when.
#
# Note 2: The state of a workflow is defined by the actions of the primary agent, 
# i.e., which steps have they already completed, which steps should they 
# complete next, etc.
#
# The 'state' variable in the returned data frame can only assume the following
# values:
#  - START
#  - IN_PROGRESS
#  - PENDING_REVIEW
#  - RETRY
#  - DONE
#  - NOT_ALLOWED_YET
#  - READY_FOR_REPEAT
# 
#'@param idEntity Character. The ID of the project in question.
#'@return A data frame containing the current state of the workflow for the specified agent. The
#'  data frame contains at least the following variables:
#'  - oidx
#'  - id_step
#'  - type_step
#'  - step_workflow
#'  - permissions_step
#'  - descr_step
#'  - title_step
#'  - descr_step
#'  - state
#'  - nm_module
#'  - id_message_policy
#'  - id_message_schema
#'  - iri_schema_step
#'  - url_ipfs_schema
#'  
getState.indImpMainWrkflwHndlr1 <- function(
    idPrimAgents, # The IDs of the primary agent(s) of the workflow version for this idEntity.
    idEntity,
    dbCon,
    idWorkflow) {
  
  nmWrkflwHndlr <- "indImpMainWrkflwHndlr1"
  
  # Initialise the overall workflow state variable.
  dfState <- initWorkflowState(
    nmWrkflwHndlr = nmWrkflwHndlr, 
    idWorkflow = idWorkflow, 
    dbCon = dbCon)
  
  # Get the relevant documents' metadata.
  dfDocMD <- getWorkflowStateDocs(
    idPrimAgents = idPrimAgents,
    idEntity = idEntity, 
    nmWrkflwHndlr = nmWrkflwHndlr, 
    dbCon = dbCon)
  
  # If no document metadata table for this idEntity exists yet, it means the
  # idEntity is completely at the start of this workflow (and all other workflows), 
  # so just return the state variable with all its defaults.
  if (is.null(dfDocMD)) { return(dfState) }
  
  # Now determine the state of each step in the workflow.
  {
    # rvcdb_projectDeveloper_PLA 
    {
      wfStep <- "rvcdb_projectDeveloper_PLA"
      
      # This step must be executed successfully before at least once before the 
      # agent can continue to the next steps in the workflow. 
      # Cannot be repeated once executed successfully.
      # Successful execution = a submitted version of the required document was 
      # APPROVED by the SB.
      
      dfStepDocs <- dfDocMD[
        which(dfDocMD$step_workflow == wfStep),]
      state <- getWorkflowStepState(dfStepDocs = dfStepDocs)
      dfState$state[which(dfState$step_workflow == wfStep)] <- state
      
      if (state != 'DONE') {
        return(dfState)
      }
    }
    
    # rvcdb_projectDeveloper_PRR 
    {
      wfStep <- "rvcdb_projectDeveloper_PRR"
      
      # This step must be executed successfully before at least once before the 
      # agent can continue to the next steps in the workflow. 
      # Cannot be repeated once executed successfully.
      # Successful execution = a submitted version of the required document was 
      # APPROVED by the SB.
      
      dfStepDocs <- dfDocMD[
        which(dfDocMD$step_workflow == wfStep),]
      state <- getWorkflowStepState(dfStepDocs = dfStepDocs)
      dfState$state[which(dfState$step_workflow == wfStep)] <- state
      
      if (state != 'DONE') {
        return(dfState)
      }
    }
    
    # rvcdb_projectDeveloper_VIC_IR 
    {
      wfStep <- "rvcdb_projectDeveloper_VIC_IR"
      
      # This step can be executed multiple times once the 
      # rvcdb_projectDeveloper_PLA and rvcdb_projectDeveloper_PRR steps have 
      # been executed successfully.
      # Successful execution = submitted version had been approved by a reviewer 
      # and VICs had subsequently been issued.
      
      dfStepDocs <- dfDocMD[
        which(dfDocMD$step_workflow == wfStep),]
      state <- getWorkflowStepState(dfStepDocs = dfStepDocs)
      dfState$state[which(dfState$step_workflow == wfStep)] <- state
      
      if (state != 'DONE') {
        return(dfState)
      }
      
      dfState$state[which(dfState$step_workflow == wfStep)] <- "READY_FOR_REPEAT"
    }
    
    return(dfState)
    
  }  
}



getWorkflowStepMap.indImpMainWrkflwHndlr1 <- function() {
  
  return(
    data.frame(
      step_workflow = c("rvcdb_projectDeveloper_PLA", 
                        "rvcdb_projectDeveloper_PRR", 
                        "rvcdb_projectDeveloper_VIC_IR", 
                        "rvcdb_standardBody_DR_PLA", 
                        "rvcdb_standardBody_DR_PRR", 
                        "rvcdb_standardBody_DR_VIC_IR"),
      nm_module = c("PLAschemaV2", 
                    "PRRschemaV2",
                    "VICIRschema",
                    "genericDocumentReviewSchema", 
                    "genericDocumentReviewSchema", 
                    "DRVICIRschema"),
      review_with = c("rvcdb_standardBody_DR_PLA", 
                      "rvcdb_standardBody_DR_PRR",
                      "rvcdb_standardBody_DR_VIC_IR",
                      NA_character_,
                      NA_character_,
                      NA_character_)))
  
}



getDefaultStepStates.indImpMainWrkflwHndlr1 <- function() {
  return(
    c(rvcdb_projectDeveloper_PLA = 'START', 
      rvcdb_projectDeveloper_PRR = 'NOT_ALLOWED_YET', 
      rvcdb_projectDeveloper_VIC_IR = 'NOT_ALLOWED_YET'))
}



checkWorkflowTriggers.indImpMainWrkflwHndlr1 <- function(dbCon, hederaClient, idDoc) {

  q <- sprintf("SELECT * FROM tbl_document_metadata WHERE id = '%s';", idDoc)
  dfDocMd <- dbGetQuery(conn = dbCon, statement = q)
  
  if (!(dfDocMd$step_workflow %in% c(
    #"rvcdb_standardBody_DR_PLA", 
    "rvcdb_standardBody_DR_PRR", 
    "rvcdb_standardBody_DR_VIC_IR"))) {
    # Nothing for us to do.
    return(invisible(0))
  }
  
  # Get the outcome of this review.
  {
    q <- sprintf(
      "SELECT * FROM tbl_link_originals_x_reviews WHERE id_review = '%s';", 
      idDoc)
    dfLinkOgXrev <- dbGetQuery(conn = dbCon, statement = q)
  }
  
  # Update the reputation of the agents involved.
  {
    lsReputAgentIds <- list(
      PROJECT_DEVELOPER = c(),
      PDD_VALIDATOR = c(),
      MR_VERIFIER = c())
    
    docRev <- getPubDoc(
      docId = idDoc, 
      dbCon = dbCon, 
      contentOnly = TRUE, 
      decrypt = TRUE)
    
    # Get the project developer's ID from the database as the PRIMARY_AGENT of
    # the project.
    {
      # Note: There can be multiple PRIMARY_AGENTs per project - the reputation
      # for all of them should be adjusted.
      lsReputAgentIds$PROJECT_DEVELOPER <- dbGetQuery(
        conn = dbCon, 
        statement = sprintf(
          "SELECT DISTINCT(id_agent) AS id_agent FROM tbl_link_projects_x_agents WHERE id_project = '%s' AND role = 'PRIMARY_AGENT';",
          dfDocMd$id_entity))[["id_agent"]]
    }
    
    # Get the IDs of the PDD validators and the MR verifiers.
    {
      if (dfDocMd$step_workflow == "rvcdb_standardBody_DR_PRR") {
        
        # Get the PDD validator's ID via the author of the DR-PDDCIR.
        
        idx <- which(
          sapply(
            X = docRev$review, 
            FUN = function(x) { return(x$field_key == "id_msg_pred_drcir") }))
        msgId <- docRev$review[[idx]]$original_response
        splts <- strsplit(x = msgId, split = ".", fixed = TRUE)[[1]]
        splts <- sapply(X = splts, FUN = function(x) {
          gsub(pattern = "[^[:digit:]]", replacement = "", x = x)
        })
        msgId <- paste0(splts[1], ".", splts[2]); rm(splts)
        
        didVal <- dbGetQuery(
          conn = dbCon, 
          statement = sprintf(
            "SELECT did_author FROM tbl_document_metadata WHERE id_message_h = '%s';",
            msgId))[["did_author"]]
        
        lsReputAgentIds$PDD_VALIDATOR <- dbGetQuery(
          conn = dbCon, 
          statement = sprintf(
            "SELECT id_agent FROM tbl_link_agents_x_dids WHERE did = '%s';",
            didVal))[["id_agent"]]
        
      } 
      
      #if (dfDocMd$step_workflow == "rvcdb_standardBody_DR_PLA") {}
      
      if (dfDocMd$step_workflow == "rvcdb_standardBody_DR_VIC_IR") {
        
        # Get the MR verifier's ID via the author of the DR-MR.
        
        idx <- which(
          sapply(
            X = docRev$review, 
            FUN = function(x) { return(x$field_key == "id_msg_pred_drmr") }))
        msgId <- docRev$review[[idx]]$original_response
        splts <- strsplit(x = msgId, split = ".", fixed = TRUE)[[1]]
        splts <- sapply(X = splts, FUN = function(x) {
          gsub(pattern = "[^[:digit:]]", replacement = "", x = x)
        })
        msgId <- paste0(splts[1], ".", splts[2]); rm(splts)
        
        didVal <- dbGetQuery(
          conn = dbCon, 
          statement = sprintf(
            "SELECT did_author FROM tbl_document_metadata WHERE id_message_h = '%s';",
            msgId))[["did_author"]]
        
        lsReputAgentIds$MR_VERIFIER <- dbGetQuery(
          conn = dbCon, 
          statement = sprintf(
            "SELECT id_agent FROM tbl_link_agents_x_dids WHERE did = '%s';",
            didVal))[["id_agent"]]
      }
    }
    
    # Do the actual reputation score updates.
    {
      repEventType <- paste0(
        switch(
          dfDocMd$step_workflow,
          rvcdb_standardBody_DR_PRR = "project_registration_",
          rvcdb_standardBody_DR_VIC_IR = "vic_issuance_"),
        tolower(dfLinkOgXrev$outcome))
      
      adjustAgentReputation(
        lsAgents = lsReputAgentIds, 
        idDocTrigger = dfDocMd$id, 
        eventType = repEventType, 
        dbCon = dbCon)
    }
  }
  
  # If this review was a rejection of a PRR or a VIC_IR, there's nothing left
  # for us to do here.
  if (dfLinkOgXrev$outcome == "REJECTED" & 
      dfDocMd$step_workflow %in% c(
        #"rvcdb_standardBody_DR_PLA",
        "rvcdb_standardBody_DR_PRR", 
        "rvcdb_standardBody_DR_VIC_IR")) {
    return(invisible(0))
  }

  # Add the metadata of the original document to Fluree.
  {
    lsQ <- workflowDocumentMetadata_toFluree(
      docId = dfLinkOgXrev$id_original, 
      dbCon = dbCon)
    
    res <- tryCatch({
      novaRush::flureeInsert(
        data = lsQ, 
        config = defaultFlureeConf, 
        signTransaction = FALSE, 
        apiKey = Sys.getenv("API_KEY_FLUREE"))
    }, error = function(e) {
      message(sprintf("ERROR: %s", as.character(e)))
      NULL
    })
  }
  
  # Add the metadata for the review document to Fluree.
  {
    lsQ <- workflowDocumentMetadata_toFluree(
      dfDocMd = dfDocMd,
      dbCon = dbCon)
    
    res <- tryCatch({
      novaRush::flureeInsert(
        data = lsQ, 
        config = defaultFlureeConf, 
        signTransaction = FALSE, 
        apiKey = Sys.getenv("API_KEY_FLUREE"))
    }, error = function(e) {
      message(sprintf("ERROR: %s", as.character(e)))
      NULL
    })
  }
  
  # Link the review and the original in Fluree.
  {
    lsQ <- list(
      '@context' = list(
        'indimp' = "https://independentimpact.org/ns/"),
      '@id' = paste0("indimp:documents/", dfLinkOgXrev$id_review),
      'indimp:isReviewOf' = list(
        '@id' = paste0("indimp:documents/", dfLinkOgXrev$id_original)))
    
    res <- tryCatch({
      novaRush::flureeInsert(
        data = lsQ, 
        config = defaultFlureeConf, 
        signTransaction = FALSE, 
        apiKey = Sys.getenv("API_KEY_FLUREE"))
    }, error = function(e) {
      message(sprintf("ERROR: %s", as.character(e)))
      NULL
    })
  }

  # if (dfDocMd$step_workflow == "rvcdb_standardBody_DR_PLA") {
  #   # TODO.
  #   return(invisible(0))
  # }
  
  if (dfDocMd$step_workflow == "rvcdb_standardBody_DR_PRR") {
    # Nothing left to do.
    return(invisible(0))
  }
  
  if (dfDocMd$step_workflow == "rvcdb_standardBody_DR_VIC_IR") {
    
    # Make sure we have not issued a certificate based on this application yet.
    {
      # TODO.
    }
    
    dateIssued <- lubridate::now(tzone = "UTC")
    
    # Retrieve the original VICIR.
    docVICIR <- getPubDoc(
      docId = dfLinkOgXrev$id_original, 
      dbCon = dbCon, 
      contentOnly = TRUE, 
      decrypt = TRUE)
    
    # Build the content to be wrapped in a VC.
    {
      projectTopic <- dbGetQuery(
        conn = dbCon, 
        statement = sprintf(
          "SELECT id_topic_h FROM tbl_link_entities_x_hedera_topics WHERE id_entity = '%s';", 
          dfDocMd$id_entity))[["id_topic_h"]]
      splts <- as.integer(strsplit(x = projectTopic, split = ".", fixed = TRUE)[[1]])
      certNoSub <- sprintf("%02dX%02dX%011d", splts[1], splts[2], splts[3])
      
      certId <- sprintf(
        "II-VIC-%s-%s-%s",
        certNoSub,
        gsub(
          pattern = ".", 
          replacement = "", 
          x = docVICIR$id_msg_pred_drmr, 
          fixed = TRUE),
        substr(
          x = gsub(
            pattern = "[^[:digit:]]", 
            replacement = "", 
            x = dateIssued), 
          start = 1, 
          stop = 8))
      
      vcCont <- docVICIR$summary_impact
      vcCont[c("type", "@context")] <- NULL
      vcCont$period_impact[c("type", "@context")] <- NULL
      names(vcCont)[names(vcCont) == "description_impact"] <- "indicator_impact"
      vcCont$id_msg_drmr <- docVICIR$id_msg_pred_drmr
      vcCont$uri_ipfs_drmr <- docVICIR$url_ipfs_pred_drmr
      vcCont$certificate_number <- certId
    }
    
    # Publish the impact certificate as a VC.
    {
      idAgentSb <- dbGetQuery(
        conn = dbCon, 
        statement = sprintf(
          "SELECT id_agent FROM tbl_link_agents_x_email_addresses WHERE email_address = '%s';",
          iwefdj$EMAIL_ADDRESS_NovaInstitute))[["id_agent"]]
      
      dfSchemaMd <- dbGetQuery(
        conn = dbCon, 
        statement = "SELECT * FROM tbl_schemas WHERE title = 'Independent Impact Verified Impact Certificate';")
      if (nrow(dfSchemaMd) > 1) {
        # Find the latest version
        dfSchemaMd <- dfSchemaMd[
          dfSchemaMd$tag_version == orderVersionTags(
            x = dfSchemaMd$tag_version, 
            asc = FALSE)[1],]
      }
      
      res <- submitDocToHedera(
        doc = vcCont, 
        idDoc = NULL, 
        dbCon = dbCon, 
        idSchema = dfSchemaMd$id, 
        idWorkflow = dfDocMd$id_workflow, 
        stepWorkflow = "rvcdb_standardBody_DR_VIC_IR", 
        idAgent = idAgentSb, 
        idEntity = dfDocMd$id_entity, 
        hederaClient = hederaClient, 
        docTypeAs = "vc")
    }
    
    # Create a new certificate entry in tbl_impact_certificates.
    {
      addToDb(
        dfAdd = data.frame(
          id = certId,
          id_entity = dfDocMd$id_entity,
          date_issued = dateIssued,
          id_message_h = res$doc_md$id_message_h,
          uri_ipfs = res$doc_md$uri_ipfs,
          status = "ACTIVE"), 
        tblNm = "tbl_impact_certificates", 
        vnmsChckEx = "id", 
        dbCon = dbCon, 
        calcIds = FALSE)
    }
    
    # Add the certificate's details to Fluree.
    {
      # Get the project ID.
      projectId <- dfDocMd$id_entity
      
      # Retrieve the certificate from IPFS.
      {
        ipfsUri <- dbGetQuery(
          conn = dbCon, 
          statement = sprintf("SELECT uri_ipfs FROM tbl_impact_certificates WHERE id = '%s';", certId))[["uri_ipfs"]]
        
        docId <- dbGetQuery(
          conn = dbCon, 
          statement = sprintf("SELECT id FROM tbl_document_metadata WHERE uri_ipfs = '%s';", 
                              ipfsUri))[["id"]]
        
        doc <- getPubDoc(docId = docId, dbCon = dbCon, contentOnly = FALSE)
      }
      
      # Send to Fluree.
      {
        lsQ <- list(
          '@context' = list(
            'indimp' = "https://independentimpact.org/ns/"),
          '@id' = paste0("indimp:activities/", projectId),
          'indimp:hasVerifiableCredential' = list(doc))
        
        res <- tryCatch({
          novaRush::flureeInsert(
            data = lsQ, 
            config = defaultFlureeConf, 
            signTransaction = FALSE, 
            apiKey = Sys.getenv("API_KEY_FLUREE"))
        }, error = function(e) {
          message(sprintf("ERROR: %s", as.character(e)))
          NULL
        })
      }
    }
    
    return(invisible(0))
    
  }
  
  warning("You should never have reached this point...")
  return(invisible(0))
  
}

