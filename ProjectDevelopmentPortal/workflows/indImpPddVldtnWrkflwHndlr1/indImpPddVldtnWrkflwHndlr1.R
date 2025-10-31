
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
getState.indImpPddVldtnWrkflwHndlr1 <- function(
    idPrimAgents, # The IDs of the primary agent(s) of the workflow version for this idEntity.
    idEntity,
    dbCon,
    idWorkflow) {
  
  nmWrkflwHndlr <- "indImpPddVldtnWrkflwHndlr1"
  
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
    # rvcdb_projDev_PDDxA 
    {
      wfStep <- "rvcdb_projDev_PDDxA"
      
      # This step must be executed successfully at least once before the 
      # 'rvcdb_projDev_CIR' step can be executed. All other steps in the proj
      # dev workflow, however, can be executed regardless of the state of this 
      # step.
      # Cannot be repeated once executed successfully.
      # Successful execution = a submitted version of the required document was 
      # APPROVED by the SB.
      
      dfStepDocs <- dfDocMD[
        which(dfDocMD$step_workflow == wfStep),]
      state <- getWorkflowStepState(dfStepDocs = dfStepDocs)
      dfState$state[which(dfState$step_workflow == wfStep)] <- state
      
    }
    
    # rvcdb_projDev_PDDxB 
    {
      wfStep <- "rvcdb_projDev_PDDxB"
      
      # This step must be executed successfully at least once before the 
      # 'rvcdb_projDev_CIR' step can be executed. All other steps in the proj
      # dev workflow, however, can be executed regardless of the state of this 
      # step.
      # Cannot be repeated once executed successfully.
      # Successful execution = a submitted version of the required document was 
      # APPROVED by the SB.
      
      dfStepDocs <- dfDocMD[
        which(dfDocMD$step_workflow == wfStep),]
      state <- getWorkflowStepState(dfStepDocs = dfStepDocs)
      dfState$state[which(dfState$step_workflow == wfStep)] <- state
      
    }
    
    # rvcdb_projDev_PDDxC 
    {
      wfStep <- "rvcdb_projDev_PDDxC"
      
      # This step must be executed successfully at least once before the 
      # 'rvcdb_projDev_CIR' step can be executed. All other steps in the proj
      # dev workflow, however, can be executed regardless of the state of this 
      # step.
      # Cannot be repeated once executed successfully.
      # Successful execution = a submitted version of the required document was 
      # APPROVED by the SB.
      
      dfStepDocs <- dfDocMD[
        which(dfDocMD$step_workflow == wfStep),]
      state <- getWorkflowStepState(dfStepDocs = dfStepDocs)
      dfState$state[which(dfState$step_workflow == wfStep)] <- state
      
    }
    
    if (!all(dfState$state[which(dfState$step_workflow %in% c(
      "rvcdb_projDev_PDDxA", 
      "rvcdb_projDev_PDDxB",
      "rvcdb_projDev_PDDxC"))] == "DONE")) {
      return(dfState)
    }
    
    # rvcdb_projDev_CIR 
    {
      wfStep <- "rvcdb_projDev_CIR"
      
      # This step can only be executed once all the preceding steps in the
      # proj dev workflow have each been executed successfully.
      # This step cannot be repeated once executed successfully.
      # Successful execution = submitted version had been approved by a reviewer 
      # and a PDD Certificate had subsequently been issued.
      
      dfStepDocs <- dfDocMD[
        which(dfDocMD$step_workflow == wfStep),]
      state <- getWorkflowStepState(dfStepDocs = dfStepDocs)
      dfState$state[which(dfState$step_workflow == wfStep)] <- state
      
      if (state != 'DONE') {
        return(dfState)
      }
      
    }
    
    return(dfState)
    
  }  
}



getWorkflowStepMap.indImpPddVldtnWrkflwHndlr1 <- function() {
  
  return(
    data.frame(
      step_workflow = c("rvcdb_projDev_PDDxA", 
                        "rvcdb_projDev_PDDxB", 
                        "rvcdb_projDev_PDDxC", 
                        "rvcdb_projDev_CIR", 
                        "rvcdb_vldtr_DR_PDDxA", 
                        "rvcdb_vldtr_DR_PDDxB", 
                        "rvcdb_vldtr_DR_PDDxC", 
                        "rvcdb_vldtr_DR_CIR"),
      nm_module = c("PDDXAschemaV2", 
                    "PDDXBschemaV2",
                    "PDDXCschemaV2",
                    "PDDCIRschemaV2",
                    "genericDocumentReviewSchema", 
                    "DRPDDXBschemaV2",
                    "genericDocumentReviewSchema",
                    "genericDocumentReviewSchema"),
      review_with = c("rvcdb_vldtr_DR_PDDxA", 
                      "rvcdb_vldtr_DR_PDDxB", 
                      "rvcdb_vldtr_DR_PDDxC", 
                      "rvcdb_vldtr_DR_CIR",
                      NA_character_,
                      NA_character_,
                      NA_character_,
                      NA_character_)))
  
}



getDefaultStepStates.indImpPddVldtnWrkflwHndlr1 <- function() {
  return(
    c(rvcdb_projDev_PDDxA = 'START', 
      rvcdb_projDev_PDDxB = 'START', 
      rvcdb_projDev_PDDxC = 'START', 
      rvcdb_projDev_CIR = 'NOT_ALLOWED_YET'))
}



checkWorkflowTriggers.indImpPddVldtnWrkflwHndlr1 <- function(dbCon, hederaClient, idDoc) {
  
  q <- sprintf("SELECT * FROM tbl_document_metadata WHERE id = '%s';", idDoc)
  dfDocMdRev <- dbGetQuery(conn = dbCon, statement = q)

  if (dfDocMdRev$step_workflow != "rvcdb_vldtr_DR_CIR") {
    # Nothing for us to do.
    return(invisible(0))
  }
  
  # Check if this DR-CIR approved the certificate issuance.
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
  
  # Extract information from the PDD into Fluree.
  {
    docCir <- getPubDoc(
      docId = dfLinkOgXrev$id_original, 
      dbCon = dbCon, 
      contentOnly = TRUE, 
      decrypt = TRUE)
    
    # PDDxA
    {
      msgIdA <- dbGetQuery(
        conn = dbCon, 
        statement = sprintf(
          "SELECT id_msg_pred FROM tbl_document_metadata WHERE uri_ipfs = '%s' AND id_message_h = '%s';",
          docCir$url_ipfs_pred_drxa, docCir$id_msg_pred_drxa))[["id_msg_pred"]]
      
      docIdA <- dbGetQuery(
        conn = dbCon, 
        statement = sprintf(
          "SELECT id FROM tbl_document_metadata WHERE id_message_h = '%s';",
          msgIdA))[["id"]]
      
      docA <- getPubDoc(
        docId = docIdA, 
        dbCon = dbCon, 
        contentOnly = TRUE, 
        decrypt = TRUE)
      docA[c("type", "@context")] <- NULL
      
      lsQ <- sPDDxA_1x0x0_toFluree(lsData = docA)
      
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
    
    # PDDxB
    {
      msgIdB <- dbGetQuery(
        conn = dbCon, 
        statement = sprintf(
          "SELECT id_msg_pred FROM tbl_document_metadata WHERE uri_ipfs = '%s' AND id_message_h = '%s';",
          docCir$url_ipfs_pred_drxb, docCir$id_msg_pred_drxb))[["id_msg_pred"]]
      
      docIdB <- dbGetQuery(
        conn = dbCon, 
        statement = sprintf(
          "SELECT id FROM tbl_document_metadata WHERE id_message_h = '%s';",
          msgIdB))[["id"]]
      
      docB <- getPubDoc(
        docId = docIdB, 
        dbCon = dbCon, 
        contentOnly = TRUE, 
        decrypt = TRUE)
      docB[c("type", "@context")] <- NULL
      
      lsQ <- sPDDxB_9x0x0_toFluree(
        lsData = docB, 
        projectId = docB$headers$id_subject)
      
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
    
    # PDDxC
    {
      # TODO.
    }
  }
  
  
  
  
  
  
  
  
  
  # # Make sure we have not issued a certificate based on this application yet.
  # {
  #   # TODO.
  # }
  # 
  # dateIssued <- lubridate::now(tzone = "UTC")
  # 
  # # Build the content to be wrapped in a VC.
  # {
  #   q <- sprintf(
  #     "SELECT id_topic_h FROM tbl_link_entities_x_hedera_topics WHERE id_entity = '%s';",
  #     dfDocMdRev$id_entity)
  #   projHtopic <- dbGetQuery(conn = dbCon, statement = q)[["id_topic_h"]]
  #   
  #   splts <- as.integer(strsplit(x = projHtopic, split = ".", fixed = TRUE)[[1]])
  #   certNoSub <- sprintf("%02dX%02dX%011d", splts[1], splts[2], splts[3])
  #   
  #   certId <- sprintf(
  #     "II-%s-%s-%s",
  #     "PRJREG",
  #     certNoSub,
  #     substr(
  #       x = gsub(
  #         pattern = "[^[:digit:]]", 
  #         replacement = "", 
  #         x = dateIssued), 
  #       start = 1, 
  #       stop = 8))
  #   
  #   vcCont <- list(
  #     certificateNumber = certId,
  #     registeredOn = lubridate::format_ISO8601(dateIssued, usetz = "Z"),
  #     applicationApprovalDocument = dfDocMdRev$uri_ipfs)
  # }
  # 
  # # Publish the certificate as a VC.
  # {
  #   idAgentSb <- dbGetQuery(
  #     conn = dbCon,
  #     statement = sprintf(
  #       "SELECT id_agent FROM tbl_link_agents_x_email_addresses WHERE email_address = '%s';",
  #       iwefdj$EMAIL_ADDRESS_NovaInstitute))[["id_agent"]]
  # 
  #   dfSchemaMd <- dbGetQuery(
  #     conn = dbCon,
  #     statement = "SELECT * FROM tbl_schemas WHERE title = 'Independent Impact Agent License';")
  #   if (nrow(dfSchemaMd) > 1) {
  #     # Find the latest version
  #     dfSchemaMd <- dfSchemaMd[
  #       dfSchemaMd$tag_version == orderVersionTags(
  #         x = dfSchemaMd$tag_version,
  #         asc = FALSE)[1],]
  #   }
  #   
  #   # TODO: Add 'ProjectRegistrationCertificate' to the 'type' property of the VC.
  # 
  #   res <- submitDocToHedera(
  #     doc = vcCont,
  #     idDoc = NULL,
  #     dbCon = dbCon,
  #     idSchema = dfSchemaMd$id,
  #     idWorkflow = dfDocMdOrig$id_workflow,
  #     stepWorkflow = "rvcdb_standardBody_DR_LA",
  #     idAgent = idAgentSb,
  #     idEntity = dfDocMdOrig$id_entity,
  #     hederaClient = hederaClient,
  #     docTypeAs = "vc")
  # }
  # 
  # # Create a new certificate entry in the tbl_project_registration_certificates.
  # {
  #   addToDb(
  #     dfAdd = data.frame(
  #       id = certId,
  #       did_holder = didLicensee,
  #       scope = lic,
  #       date_issued = dateIssued,
  #       id_message_h = res$doc_md$id_message_h,
  #       uri_ipfs = res$doc_md$uri_ipfs,
  #       status = "ACTIVE"),
  #     tblNm = "tbl_agent_licenses",
  #     vnmsChckEx = "id",
  #     dbCon = dbCon,
  #     calcIds = FALSE)
  # }
  # 
  # # Add these certificate details to Fluree.
  # {
  #   # Get the agent ID.
  #   agentId <- dbGetQuery(
  #     conn = dbCon, 
  #     statement = sprintf("SELECT id_agent FROM tbl_link_agents_x_dids WHERE did = '%s';", 
  #                         didLicensee))[["id_agent"]]
  #   
  #   # Retrieve the license from IPFS.
  #   {
  #     ipfsUri <- dbGetQuery(
  #       conn = dbCon, 
  #       statement = sprintf("SELECT uri_ipfs FROM tbl_agent_licenses WHERE id = '%s';", licId))[["uri_ipfs"]]
  #     
  #     docId <- dbGetQuery(
  #       conn = dbCon, 
  #       statement = sprintf("SELECT id FROM tbl_document_metadata WHERE uri_ipfs = '%s';", 
  #                           ipfsUri))[["id"]]
  #     
  #     doc <- getPubDoc(docId = docId, dbCon = dbCon, contentOnly = FALSE)
  #   }
  #   
  #   # Send to Fluree.
  #   {
  #     lsQ <- list(
  #       '@context' = list(
  #         'indimp' = "https://independentimpact.org/ns/"),
  #       '@id' = paste0("indimp:agents/", agentId),
  #       'indimp:hasVerifiableCredential' = list(doc))
  #     
  #     res <- tryCatch({
  #       novaRush::flureeInsert(
  #         data = lsQ, 
  #         config = defaultFlureeConf, 
  #         signTransaction = FALSE, 
  #         apiKey = Sys.getenv("API_KEY_FLUREE"))
  #     }, error = function(e) {
  #       message(sprintf("ERROR: %s", as.character(e)))
  #       NULL
  #     })
  #   }
  # }
  
  return(invisible(0))
  
}
