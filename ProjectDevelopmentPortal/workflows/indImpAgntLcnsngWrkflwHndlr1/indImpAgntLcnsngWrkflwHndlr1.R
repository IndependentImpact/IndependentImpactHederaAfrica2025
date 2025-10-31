
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
#'@param idEntity Character. The ID of the agent in question. Will also
#'  always be equal to idPrimAgent.
#'@return A data frame containing the current state of the workflow for the 
#'  specified agent. The data frame contains at least the following variables:
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
getState.indImpAgntLcnsngWrkflwHndlr1 <- function(
    idPrimAgents, # The IDs of the primary agent(s) of the workflow version for this idEntity.
    idEntity,
    dbCon,
    idWorkflow) {
  
  nmWrkflwHndlr <- "indImpAgntLcnsngWrkflwHndlr1"
  
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
    # rvcdb_agent_AD  
    {
      wfStep <- 'rvcdb_agent_AD'
      
      # This step must be executed successfully before at least once before the 
      # agent can continue to the next steps in the workflow. 
      # Can be repeated once executed successfully.
      # Successful execution = a submitted version was reviewed by the SB.
      
      # SUBMITTED
      # REVIEWED
      
      dfStepDocs <- dfDocMD[
        which(dfDocMD$step_workflow == wfStep),]
      state <- getWorkflowStepState(dfStepDocs = dfStepDocs)
      dfState$state[which(dfState$step_workflow == wfStep)] <- state
      
      if (state != 'DONE') {
        return(dfState)
      }
      
      dfState$state[which(dfState$step_workflow == wfStep)] <- "READY_FOR_REPEAT"
      
    }
    
    # rvcdb_agent_LA  
    {
      wfStep <- 'rvcdb_agent_LA'
      
      # This step can be executed multiple times once the rvcdb_agent_AD step
      # has been executed successfully at least once.
      # Successful execution = submitted version had been approved by a reviewer and
      # a license had subsequently been issued.
      
      # SUBMITTED
      # APPROVED
      # REJECTED
      # REVOKED
      # ISSUED
      
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



getWorkflowStepMap.indImpAgntLcnsngWrkflwHndlr1 <- function() {
  
  return(
    data.frame(
      step_workflow = c("rvcdb_agent_AD", 
                        "rvcdb_agent_LA", 
                        "rvcdb_standardBody_DR_AD", 
                        "rvcdb_standardBody_DR_LA"),
      nm_module = c("ADschemaV2", 
                    "LAschema",
                    "genericDocumentReviewSchema", 
                    "DRLAschema"),
      review_with = c("rvcdb_standardBody_DR_AD", 
                      "rvcdb_standardBody_DR_LA",
                      NA_character_,
                      NA_character_)))
  
}



getDefaultStepStates.indImpAgntLcnsngWrkflwHndlr1 <- function() {
  return(
    c(rvcdb_agent_AD = 'START', 
      rvcdb_agent_LA = 'NOT_ALLOWED_YET'))
}



checkWorkflowTriggers.indImpAgntLcnsngWrkflwHndlr1 <- function(dbCon, hederaClient, idDoc) {
  
  # TODO: Replace reputation logic in this function with calls to adjustAgentReputation().
  
  q <- sprintf("SELECT * FROM tbl_document_metadata WHERE id = '%s';", idDoc)
  dfDocMdRev <- dbGetQuery(conn = dbCon, statement = q)
  
  if (!(dfDocMdRev$step_workflow %in% c(
    "rvcdb_standardBody_DR_AD", 
    "rvcdb_standardBody_DR_LA"))) {
    # Nothing for us to do.
    return(invisible(0))
  }
  
  # Check if this review was an approval.
  {
    q <- sprintf(
      "SELECT * FROM tbl_link_originals_x_reviews WHERE id_review = '%s';", 
      idDoc)
    dfLinkOgXrev <- dbGetQuery(conn = dbCon, statement = q)
    if (dfLinkOgXrev$outcome != "APPROVED") {
      # Nothing for us to do.
      return(invisible(0))
    }
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
      dfDocMd = dfDocMdRev,
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
  
  if (dfDocMdRev$step_workflow == "rvcdb_standardBody_DR_AD") {

    # Add the approved agent details to Fluree.
    {
      # Get the original AD document.
      doc <- getPubDoc(
        docId = dfLinkOgXrev$id_original, 
        dbCon = dbCon, 
        contentOnly = TRUE, 
        decrypt = TRUE)
      
      lsQ <- sAgentDetails_1x0x0_toFluree(
        agentId = doc$headers$id_subject, 
        lsData = doc)
      
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
    
    # Issue the agent with reputation, if this is their first AD. 
    {
      dfEx <- dbGetQuery(
        conn = dbCon, 
        statement = sprintf(
          "SELECT * FROM tbl_agent_reputation WHERE id_agent = '%s' AND domain = 'GENERAL';",
          dfDocMdRev$id_entity))
      
      if (nrow(dfEx) > 0) { 
        # TODO: An agent can submit new details or new evidence at any later point
        # in time, at which their reputation will have to be _adjusted_ through a
        # much more elaborate process.
        return(invisible(0))
      }
      
      docRev <- getPubDoc(
        docId = idDoc, 
        dbCon = dbCon, 
        contentOnly = TRUE, 
        decrypt = TRUE)
      
      rep <- length(
        which(
          sapply(
            X = docRev$review, FUN = function(x) {
              x$reviewer_decision == "APPROVE"
            }))) # TODO. This is not fair at all; just intended for PoC.
      
      # TODO. Wrap the statements below into a single Postgres transaction so 
      # that either all table updates succeed or all table updates fail.
      
      addToDb(
        dfAdd = data.frame(
          id_agent = dfDocMdRev$id_entity,
          domain = "GENERAL",
          score = rep), 
        tblNm = "tbl_agent_reputation", 
        vnmsChckEx = c("id_agent", "domain"), 
        dbCon = dbCon, 
        calcIds = FALSE)
      
      addToDb(
        dfAdd = data.frame(
          id_agent = dfDocMdRev$id_entity,
          event_type = "agent_details_verification",
          occurred_at = dfDocMdRev$date_modified,
          trigger = dfDocMdRev$id,
          reputation_domain = "GENERAL",
          reputation_adjustment = rep), 
        tblNm = "tbl_agent_reputation_event_log", 
        vnmsChckEx = c(
          "id_agent", "event_type", "occurred_at", 
          "trigger", "reputation_domain"), 
        dbCon = dbCon, 
        calcIds = FALSE)
      
    }
    
    return(invisible(0))
  }
  
  if (dfDocMdRev$step_workflow == "rvcdb_standardBody_DR_LA") {
    
    # Determine the license to be issued.
    {
      dfDocMdOrig <- dbGetQuery(
        conn = dbCon, 
        statement = sprintf(
          "SELECT * FROM tbl_document_metadata WHERE id = '%s';", 
          dfLinkOgXrev$id_original))
      lic <- tryCatch({
        jsonlite::fromJSON(dfDocMdOrig$identifying_content)[["license"]]
      }, error = function(e) {
        docOg <- getPubDoc(
          docId = dfDocMdOrig$id, 
          dbCon = dbCon, 
          contentOnly = TRUE, 
          decrypt = TRUE)
        docOg$license
      })
    }
    
    # Initialise the agent's reputation score for the domain in question, if 
    # this is their first LA for the domain.
    {
      repDomain <- switch(
        lic,
        PROJECT_DEVELOPER = "PROJECT_DEVELOPMENT",
        PDD_VALIDATOR = "PDD_VALIDATION",
        MR_VERIFIER = "MR_VERIFICATION")
      
      dfEx <- dbGetQuery(
        conn = dbCon, 
        statement = sprintf(
          "SELECT * FROM tbl_agent_reputation WHERE id_agent = '%s' AND domain = '%s';",
          dfDocMdRev$id_entity, repDomain))
      
      if (nrow(dfEx) > 0) { 
        # TODO: An agent can reapply for a license of this kind at any point
        # in time, at which their reputation will have to be _adjusted_ through a
        # much more elaborate process.
        return(invisible(0))
      }

      # TODO. Wrap the statements below into a single Postgres transaction so 
      # that either all table updates succeed or all table updates fail.
      
      addToDb(
        dfAdd = data.frame(
          id_agent = dfDocMdRev$id_entity,
          domain = repDomain,
          score = 0), 
        tblNm = "tbl_agent_reputation", 
        vnmsChckEx = c("id_agent", "domain"), 
        dbCon = dbCon, 
        calcIds = FALSE)
      
      addToDb(
        dfAdd = data.frame(
          id_agent = dfDocMdRev$id_entity,
          event_type = "license_application",
          occurred_at = dfDocMdRev$date_modified,
          trigger = dfDocMdRev$id,
          reputation_domain = repDomain,
          reputation_adjustment = 0), 
        tblNm = "tbl_agent_reputation_event_log", 
        vnmsChckEx = c(
          "id_agent", "event_type", "occurred_at", 
          "trigger", "reputation_domain"), 
        dbCon = dbCon, 
        calcIds = FALSE)
      
    }
    

    
    # Make sure we have not issued a license yet based on this application.
    {
      # TODO.
    }
    

    
    dateIssued <- lubridate::now(tzone = "UTC")
    
    # Build the content to be wrapped in a VC.
    {
      q <- sprintf(
        "SELECT did FROM tbl_link_agents_x_dids WHERE id_agent = '%s';",
        dfDocMdOrig$id_entity)
      didLicensee <- dbGetQuery(conn = dbCon, statement = q)[["did"]]
      
      didTopic <- strsplit(x = didLicensee, split = "_", fixed = TRUE)[[1]][2]
      
      # q <- sprintf(
      #   "SELECT email_address FROM tbl_link_agents_x_email_addresses WHERE id_agent = '%s';",
      #   dfDocMdOrig$id_entity)
      # emailAddr <- dbGetQuery(conn = dbCon, statement = q)[["email_address"]]
      
      splts <- as.integer(strsplit(x = didTopic, split = ".", fixed = TRUE)[[1]])
      licNoSub <- sprintf("%02dX%02dX%011d", splts[1], splts[2], splts[3])
      
      licId <- sprintf(
        "II-%s-%s-%s",
        switch(lic,
               PROJECT_DEVELOPER = "PD",
               PDD_VALIDATOR = "PV",
               MR_VERIFIER = "MV"),
        licNoSub,
        substr(
          x = gsub(
            pattern = "[^[:digit:]]", 
            replacement = "", 
            x = dateIssued), 
          start = 1, 
          stop = 8))
      
      vcCont <- list(
        scope = list(lic),
        licenseNumber = licId,
        applicationApprovalDocument = dfDocMdRev$uri_ipfs)
    }
    
    # Publish the license as a VC.
    {
      idAgentSb <- dbGetQuery(
        conn = dbCon, 
        statement = sprintf(
          "SELECT id_agent FROM tbl_link_agents_x_email_addresses WHERE email_address = '%s';",
          iwefdj$EMAIL_ADDRESS_NovaInstitute))[["id_agent"]]
      
      dfSchemaMd <- dbGetQuery(
        conn = dbCon, 
        statement = "SELECT * FROM tbl_schemas WHERE title = 'Independent Impact Agent License';")
      if (nrow(dfSchemaMd) > 1) {
        # Find the latest version
        dfSchemaMd <- dfSchemaMd[
          dfSchemaMd$tag_version == orderVersionTags(
            x = dfSchemaMd$tag_version, 
            asc = FALSE)[1],]
      }
      
      # TODO: Add 'AgentLicense' to the 'type' property of the VC.
      
      res <- submitDocToHedera(
        doc = vcCont, 
        idDoc = NULL, 
        dbCon = dbCon, 
        idSchema = dfSchemaMd$id, 
        idWorkflow = dfDocMdOrig$id_workflow, 
        stepWorkflow = "rvcdb_standardBody_DR_LA", 
        idAgent = idAgentSb, 
        idEntity = dfDocMdOrig$id_entity, 
        hederaClient = hederaClient, 
        docTypeAs = "vc")
    }
    
    # Create a new license entry in the tbl_agent_licenses.
    {
      addToDb(
        dfAdd = data.frame(
          id = licId,
          did_holder = didLicensee,
          scope = lic,
          date_issued = dateIssued,
          id_message_h = res$doc_md$id_message_h,
          uri_ipfs = res$doc_md$uri_ipfs,
          status = "ACTIVE"), 
        tblNm = "tbl_agent_licenses", 
        vnmsChckEx = "id", 
        dbCon = dbCon, 
        calcIds = FALSE)
    }
    
    # Add these license details to Fluree.
    {
      # Get the agent ID.
      agentId <- dbGetQuery(
        conn = dbCon, 
        statement = sprintf("SELECT id_agent FROM tbl_link_agents_x_dids WHERE did = '%s';", 
                            didLicensee))[["id_agent"]]
      
      # Retrieve the license from IPFS.
      {
        ipfsUri <- dbGetQuery(
          conn = dbCon, 
          statement = sprintf("SELECT uri_ipfs FROM tbl_agent_licenses WHERE id = '%s';", licId))[["uri_ipfs"]]
        
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
            'indimp' = "https://independentimpact.org/ns/",
            'aiao' = "http://w3id.org/aiao#"),
          '@id' = paste0("indimp:agents/", agentId),
          '@type' = "aiao:Agent",
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
  
  message(sprintf("WARNING: No known triggers for %s.", dfDocMdRev$step_workflow))
  return(invisible(0))
  
}
  
  
  
  