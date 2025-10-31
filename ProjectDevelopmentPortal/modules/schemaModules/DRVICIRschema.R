
DRVICIRschemaUI <- function(
    id, 
    lsPreset = NULL, 
    hL = 4, 
    colWidth = 12, 
    inpWidth = DEFAULT_INP_WIDTH,
    idSchemaV = NULL) { 
  
  ns <- NS(id)
  
  tagList(
    
    #useShinyjs(),

    chooseLicenseInput(
      id = ns('chLicRev'),
      title = "Reviewer License",
      helpTxt = "Select your license to be used for this review."),
    
    uiOutput(outputId = ns("uioReviewFields")),
    
    textOutput(outputId = ns("toInvalidInput")),
    
    br())
}

#'@param idSchemaOrig The ID of the schema for the document under review.
#'@param lsPreset A list of lists like these: list(
#'  field_key = fieldKey,
#'  field_title = fieldTitle,
#'  field_prompt = fieldPrompt,
#'  original_response = fieldVal,
#'  reviewer_decision = lsPreset$reviewer_decision, 
#'  reviewer_feedback = lsPreset$reviewer_feedback)
#'  
DRVICIRschemaServer <- function(
    id,  
    dbCon,
    loginInfoUsr,
    idEntity,
    licenseReq,
    idWorkflow = NULL, # Ignored.
    idSchema = NULL, # Ignored.
    idSchemaOrig,
    lsOriginal,
    lsPreset = NULL) {
  
  moduleServer(
    id = id, 
    function(input, output, session) {
      
      ns <- session$ns
      
      rvToReturn <- reactiveValues()
      rvToReturn$allResultsGood <- FALSE
      rvToReturn$results <- reactiveValues(
        headers = list(
          id_msg_lic = NULL,
          url_ipfs_lic = NULL,
          id_msg_pred = lsOriginal$md$id_message,
          url_ipfs_pred = lsOriginal$md$url_ipfs,
          id_subject = lsOriginal$cont$headers$id_subject,
          type_subject = lsOriginal$cont$headers$type_subject),
        review = list(),
        final_rd = 'REJECT',
        id_acc_h = lsOriginal$cont$id_acc_h) 
      
      resultsGood <- reactiveValues()
      resultsGood$id_msg_lic <- FALSE
      resultsGood$url_ipfs_lic <- FALSE
      resultsGood$final_rd <- FALSE
      
      reviewerDecisions <- reactiveValues()
      
      #outputMsgs <- reactiveValues()
      # None.
      
      modSrvrs <- reactiveValues()
      
      modSrvrs$mRevwrLic <- chooseLicenseServer(
        id = "chLicRev",
        dbCon = dbCon,
        scopes = licenseReq, 
        statuses = "ACTIVE",
        idAgent = loginInfoUsr$id_agent,
        lsPreset = NULL) # TODO: implement lsPreset
      
      # Fetch the original schema. ---------------------------------------------
      {
        schIri <- dbGetQuery(
          conn = dbCon, 
          statement = sprintf("SELECT iri FROM tbl_schemas WHERE id = '%s';",
                              idSchemaOrig))
        
        sch <- jsonlite::read_json(
          path = sprintf("%s%s.json", schemadir, schIri),
          simplifyVector = TRUE,
          simplifyDataFrame = FALSE,
          flatten = FALSE)
        
        dfSchema <- schemaToDf(sch)
        
        # Exclude fields that should not be reviewed.
        dfSchema <- dfSchema[which(dfSchema$key != "id_acc_h"),]
        dfSchema <- dfSchema[which(dfSchema$key != "headers"),]
      }
      
      # Build an index of lsPreset$review, if it is not NULL. -------------------------
      {
        keysLsPresetReview <- c()
        if (length(lsPreset) > 0) {
          if (length(lsPreset$review) > 0) {
            keysLsPresetReview <- sapply(X = lsPreset$review, FUN = function(x) x$field_key)
          }
        }
      }
      
      # module servers ---------------------------------------------------------
      
      # Spin up a reviewFieldV2Server for each field to be reviewed, and 
      # connect its return values to 
      # rvToReturn$results$review,  
      # resultsGood, and
      # reviewerDecisions
      {
        for (r in 1:nrow(dfSchema)) {
          
          # Extract the previously saved reviewer response to this field, if any.
          revDecPreset <- "REJECT"
          revFdbckPreset <- ""
          if (length(keysLsPresetReview) > 0) {
            lsPresetIdx <- which(keysLsPresetReview == dfSchema$key[r])  
            if (length(lsPresetIdx) == 1) {
              revDecPreset <- lsPreset$review[[lsPresetIdx]]$reviewer_decision
              revFdbckPreset <- lsPreset$review[[lsPresetIdx]]$reviewer_feedback
            }
          }
          
          # Spin up the module server.
          modSrvrs[[dfSchema$key[r]]] <- reviewFieldV2Server(
            id = dfSchema$key[r],
            fieldKey = dfSchema$key[r],
            fieldTitle = dfSchema$title[r],
            fieldPrompt = dfSchema$description[r],
            fieldVal = lsOriginal$cont[[dfSchema$key[r]]],
            lsPreset = list(
              reviewer_decision = revDecPreset,
              reviewer_feedback = revFdbckPreset))
        }
      }
      
      # Render a reviewFieldV2UI for each field to be reviewed.
      output$uioReviewFields <- renderUI({
      
        tl <- tagList()
        
        for (r in 1:nrow(dfSchema)) {
          
          # Extract the previously saved reviewer response to this field, if any.
          revDecPreset <- "REJECT"
          revFdbckPreset <- ""
          if (length(keysLsPresetReview) > 0) {
            lsPresetIdx <- which(keysLsPresetReview == dfSchema$key[r])  
            if (length(lsPresetIdx) == 1) {
              revDecPreset <- lsPreset$review[[lsPresetIdx]]$reviewer_decision
              revFdbckPreset <- lsPreset$review[[lsPresetIdx]]$reviewer_feedback
            }
          }
          
          # Add a UI element to tl for this field.
          tl[[r]] <- reviewFieldV2UI(
            id = ns(dfSchema$key[r]), 
            fieldTitle = dfSchema$title[r], 
            fieldPrompt = dfSchema$description[r], 
            lsPreset = list(
              reviewer_decision = revDecPreset,
              reviewer_feedback = revFdbckPreset))
        }
        
        return(tl)
        
      })

      observe({
        
        for (r in 1:nrow(dfSchema)) {
          # Connect the module server's 'results' to rvToReturn$results$review.
          rvToReturn$results$review[[dfSchema$key[r]]] <- modSrvrs[[dfSchema$key[r]]]$results
          
          # Connect the module server's 'allResultsGood' to resultsGood.
          resultsGood[[dfSchema$key[r]]] <-  modSrvrs[[dfSchema$key[r]]]$allResultsGood
          
          # Connect the module server's 'results$reviewer_decision' to reviewerDecisions.
          reviewerDecisions[[dfSchema$key[r]]] <-  modSrvrs[[dfSchema$key[r]]]$results$reviewer_decision
        }
      })
      
      # inputs -----------------------------------------------------------------
      
      observe({
        
        resultsGood$final_rd <- FALSE
        rvToReturn$results$final_rd <- 'REJECT'
        
        revDecs <- reactiveValuesToList(reviewerDecisions)
        idxx <- which(sapply(X = revDecs, FUN = function(x) length(x) == 0))
        if (length(idxx) > 0) {
          return(invisible(0))
        }
        
        revDecs <- unlist(revDecs)
        revDecs <- unique(revDecs)
        if (length(setdiff(revDecs, c('APPROVE', 'FORWARD_ACTION_REQUEST'))) == 0) {
          rvToReturn$results$final_rd <- 'APPROVE'
        }

        resultsGood$final_rd <- TRUE
        
      })
      
      observe({
        
        resultsGood$id_msg_lic <- FALSE
        resultsGood$url_ipfs_lic <- FALSE
        
        rvToReturn$results$headers$id_msg_lic <- modSrvrs$mRevwrLic$results$id_msg
        rvToReturn$results$headers$url_ipfs_lic <- modSrvrs$mRevwrLic$results$url_ipfs
        
        if (modSrvrs$mRevwrLic$allResultsGood) {
          resultsGood$id_msg_lic <- TRUE
          resultsGood$url_ipfs_lic <- TRUE
        }
        
      })
      
      # outputs ----------------------------------------------------------------
      
      output$toInvalidInput <- renderText({
        
        if (rvToReturn$allResultsGood) { return(NULL) }
        
        return(
          sprintf("Your inputs to the following fields are not valid: %s",
                  paste(names(resultsGood)[!unlist(reactiveValuesToList(resultsGood))], sep = "", collapse = ", ")))
        
      })
      
      # return logic -----------------------------------------------------------
      
      observe({
        rvToReturn$allResultsGood <- all(unlist(reactiveValuesToList(resultsGood)))
      })
      
      return(rvToReturn)
      
    })
}
