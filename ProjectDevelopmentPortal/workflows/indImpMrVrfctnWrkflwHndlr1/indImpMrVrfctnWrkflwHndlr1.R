
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
getState.indImpMrVrfctnWrkflwHndlr1 <- function(
    idPrimAgents, # The IDs of the primary agent(s) of the workflow version for this idEntity.
    idEntity,
    dbCon,
    idWorkflow) {
  
  nmWrkflwHndlr <- "indImpMrVrfctnWrkflwHndlr1"
  
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
    # rvcdb_projectDeveloper_DLR
    {
      wfStep <- "rvcdb_projectDeveloper_DLR"
      
      # Can be repeated without limit.
      
      dfStepDocs <- dfDocMD[
        which(dfDocMD$step_workflow == wfStep),]
      state <- getWorkflowStepState(dfStepDocs = dfStepDocs)
      dfState$state[which(dfState$step_workflow == wfStep)] <- state
      
      if (state == "START") {
        return(dfState)
      }
      
      if (state != 'IN_PROGRESS') {
        dfState$state[which(dfState$step_workflow == wfStep)] <- "READY_FOR_REPEAT"  
      }
    }
    
    # rvcdb_projectDeveloper_MR 
    {
      wfStep <- "rvcdb_projectDeveloper_MR"
      
      # Can only be executed once the rvcd_projectDeveloper_DLR step has been 
      # executed at least once.
      # Can be repeated, once executed successfully.
      # Successful execution = a submitted version of the required document was 
      # REVIEWED by an MR verifier.
      
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



getWorkflowStepMap.indImpMrVrfctnWrkflwHndlr1 <- function() {
  
  return(
    data.frame(
      step_workflow = c("rvcdb_projectDeveloper_DLR", 
                        "rvcdb_projectDeveloper_MR", 
                        "rvcdb_verifier_DR_DLR", 
                        "rvcdb_verifier_DR_MR"),
      nm_module = c("DLRschemaV2", 
                    "MRschemaV2", 
                    "genericDocumentReviewSchema",
                    "genericDocumentReviewSchema"),
      review_with = c("rvcdb_verifier_DR_DLR",
                      "rvcdb_verifier_DR_MR",
                      NA_character_,
                      NA_character_)))
  
}




getDefaultStepStates.indImpMrVrfctnWrkflwHndlr1 <- function() {
  return(
    c(rvcdb_projectDeveloper_DLR = 'START',
      rvcdb_projectDeveloper_MR = 'NOT_ALLOWED_YET'))
}




checkWorkflowTriggers.indImpMrVrfctnWrkflwHndlr1 <- function(dbCon, hederaClient, idDoc) {
 
  q <- sprintf("SELECT * FROM tbl_document_metadata WHERE id = '%s';", idDoc)
  dfDocMdRev <- dbGetQuery(conn = dbCon, statement = q)
  
  if (!(dfDocMdRev$step_workflow %in% c(
    "rvcdb_verifier_DR_DLR", 
    "rvcdb_verifier_DR_MR"))) {
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
  
  if (dfDocMdRev$step_workflow == "rvcdb_verifier_DR_DLR") {
    
    docDlr <- getPubDoc(
      docId = dfLinkOgXrev$id_original, 
      dbCon = dbCon, 
      contentOnly = TRUE, 
      decrypt = TRUE)
    
    # Create a Fluree node for the dataset.
    {
      lsQ <- list(
        
        '@context' = list(
          'data' = "http://jellyfiiish.xyz/ns/",
          'dcterms' = "http://purl.org/dc/terms/",
          'schema' = "https://schema.org/"),
        
        '@type' = "data:Dataset",
        
        'dcterms:title' = docDlr$name_dataset)
        
      if ("data_actions" %in% names(docDlr)) {
        
        # TODO. The DLR was created using a newer version of the jellyfishR 
        # module.
        stop("NYI.")
        
      } else {
        
        lsQ <- c(
          lsQ, 
          list('data:lineage' = list()))
        
        if ("uri_ipfs_rmd_transfer" %in% names(docDlr)) {
          
          lsQ[["data:lineage"]][[length(lsQ[["data:lineage"]]) + 1]] <- list(
            '@type' = "data:DataAction",
            'schema:description' = "transfer",
            'data:instrument' = list(
              '@type' = "dcterms:Resource",
              'data:resourceContentLocation' = docDlr$uri_ipfs_rmd_transfer),
            'data:report' = list(
              '@type' = "dcterms:Resource",
              'data:resourceContentLocation' = docDlr$uri_ipfs_report_transfer))
        }
        
        if ("uri_ipfs_rmd_cleaning" %in% names(docDlr)) {
          
          lsQ[["data:lineage"]][[length(lsQ[["data:lineage"]]) + 1]] <- list(
            '@type' = "data:DataAction",
            'schema:description' = "cleaning",
            'data:instrument' = list(
              '@type' = "dcterms:Resource",
              'data:resourceContentLocation' = docDlr$uri_ipfs_rmd_cleaning),
            'data:report' = list(
              '@type' = "dcterms:Resource",
              'data:resourceContentLocation' = docDlr$uri_ipfs_report_cleaning))
        }
        
        if ("uri_ipfs_rmd_transformation" %in% names(docDlr)) {
          
          lsQ[["data:lineage"]][[length(lsQ[["data:lineage"]]) + 1]] <- list(
            '@type' = "data:DataAction",
            'schema:description' = "transformation",
            'data:instrument' = list(
              '@type' = "dcterms:Resource",
              'data:resourceContentLocation' = docDlr$uri_ipfs_rmd_transformation),
            'data:report' = list(
              '@type' = "dcterms:Resource",
              'data:resourceContentLocation' = docDlr$uri_ipfs_report_transformation))
        }
        
        # Add 'uri_ipfs_data_raw' as the input to the first data action and
        # 'uri_ipfs_data_final' as the output of the last data action.
        
        lsQ[["data:lineage"]][[1]][["data:input"]] <- list(
            '@type' = "dcterms:Resource",
            'data:resourceContentLocation' = docDlr$uri_ipfs_data_raw)
        
        lsQ[["data:lineage"]][[length(lsQ[["data:lineage"]])]][["data:output"]] <- list(
          '@type' = "dcterms:Resource",
          'data:resourceContentLocation' = docDlr$uri_ipfs_data_final)
        
        # 'uri_ipfs_data_final' should also be set as the resource content 
        # location of the dataset itself.
        lsQ[['data:resourceContentLocation']] <- docDlr$uri_ipfs_data_final
      }
      
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
    
    # Retrieve the ID that Fluree created for the dataset node.
    {
      res <- jsonlite::fromJSON(res)
      
      lsQ <- list(
        "@context" = list(
          "dcterms" = "http://purl.org/dc/terms/",
          "rdf" = "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
          "data" = "http://jellyfiiish.xyz/ns/",
          "f" = "https://ns.flur.ee/ledger#"),
        "from" = defaultFlureeConf$ledger,
        "history" = c(NA_character_, "rdf:type", "data:Dataset"),
        "t" = list("from" = res$t, "to" = res$t))
      
      qry <- novaRush::history(
        config = defaultFlureeConf, 
        query = lsQ, 
        signQuery = FALSE, 
        apiKey = Sys.getenv("API_KEY_FLUREE"))
      
      res <- novaRush::sendHistoryQuery(qry)
      res <- jsonlite::fromJSON(txt = res, simplifyDataFrame = TRUE)
      res <- res[["f:assert"]][[1]]
      dsId <- res[which(res[["@type"]] == "data:Dataset"), "@id"]
    }
    
    # Insert an arc to link the DLR to the dataset.
    {
      lsQ <- list(
        "@context" = list(
          "dcterms" = "http://purl.org/dc/terms/",
          "indimp" = "https://independentimpact.org/ns/"),
        "@id" = paste0("indimp:documents/", dfLinkOgXrev$id_original),
        "dcterms:subject" = dsId)
      
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
    
    return(invisible(0))
  }
  
  if (dfDocMdRev$step_workflow == "rvcdb_verifier_DR_MR") {
    
    docMr <- getPubDoc(
      docId = dfLinkOgXrev$id_original,
      dbCon = dbCon,
      contentOnly = TRUE,
      decrypt = TRUE)
    
    # Insert arcs to link the MR to its datasets.
    {
      # We'll find the datasets by finding the "subject" of the DLRs reference 
      # by the MR.
      
      for (dsk in length(docMr$datasets)) {
        
        # Get the ID of the data lineage report.
        {
          msgIdDlr <- dbGetQuery(
            conn = dbCon, 
            statement = sprintf(
              "SELECT id_msg_pred FROM tbl_document_metadata WHERE id_message_h = '%s';",
              docMr$datasets[[dsk]]$id_msg_drdlr))[["id_msg_pred"]]
          if (length(msgIdDlr) == 0 || nchar(msgIdDlr) == 0) {
            stop("Failed to determine message ID of DLR.")
          }
          docIdDlr <- dbGetQuery(
            conn = dbCon, 
            statement = sprintf(
              "SELECT id FROM tbl_document_metadata WHERE id_message_h = '%s';",
              msgIdDlr))[["id"]]
        }
        
        # Get the ID of the dataset.
        {
          lsQ <- list(
            "@context" = list(
              "dcterms" = "http://purl.org/dc/terms/"),
            "from" = defaultFlureeConf$ledger,
            "where" = list(
              list("@id" = "?s"),
              list("dcterms:subject" = "?p"),
              c('filter', 
                paste0('(= (str ?s) "https://independentimpact.org/ns/documents/', 
                       docIdDlr,
                       '")'))),
            "select" = c("?s", "?p"))
          
          qry <- novaRush::query(
            config = defaultFlureeConf, 
            query = lsQ, 
            signQuery = FALSE, 
            apiKey = Sys.getenv("API_KEY_FLUREE"))
          
          res <- novaRush::sendQuery(qry)
          res <- jsonlite::fromJSON(txt = res, simplifyMatrix = FALSE)
          
          dsId <- res[[1]][2]
        }
        
        # Insert an arc to link the MR to the dataset.
        {
          lsQ <- list(
            "@context" = list(
              "dcterms" = "http://purl.org/dc/terms/",
              "indimp" = "https://independentimpact.org/ns/"),
            "@id" = paste0("indimp:documents/", dfLinkOgXrev$id_original),
            "dcterms:references" = dsId)
          
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
    }
    
    return(invisible(0))
  }
  
  warning("You should never have reached this point...")
  return(invisible(0))
  
}






