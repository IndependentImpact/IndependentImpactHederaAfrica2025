
# This is an adaptation of the genericDocumentReview module. Its output is
# identical to that of genericDocumentReview, because the policy block for
# DRPDDXB expects input that conforms to genericDocumentReview.

DRPDDXBschemaV2UI <- function(
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
      id = ns('chLicPddVal'),
      title = "PDD Validator License",
      helpTxt = "Select your Independent Impact PDD Validator License (II-L-PV) to use for this review."),
    
    hx("Review each of the impacts listed below:", lvl = hL),
    DT::dataTableOutput(outputId = ns("dtoImpacts")),
    
    br(),
    
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
DRPDDXBschemaV2Server <- function(
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
  
  # # TODO. Remove after debugging.
  # lsArgs <- list(id = id,
  #                idEntity = idEntity,
  #                licenseReq = licenseReq,
  #                idSchemaOrig  = idSchemaOrig,
  #                lsOriginal = lsOriginal,
  #                lsPreset = lsPreset)
  # save(lsArgs, file = sprintf("%slsArgs_DRPDDXBschemaV2Server.Rda", tmpdir))

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
        final_rd = 'REJECT') 
      
      resultsGood <- reactiveValues()
      resultsGood$id_msg_lic <- FALSE
      resultsGood$url_ipfs_lic <- FALSE
      resultsGood$final_rd <- FALSE
      
      rvOther <- reactiveValues()
      rvOther$currImpactIdx <- NULL
      
      reviewerDecisions <- reactiveValues()
      
      #outputMsgs <- reactiveValues()
      # None.
      
      modSrvrs <- reactiveValues()
      
      modSrvrs$mPddValLic <- chooseLicenseServer(
        id = "chLicPddVal",
        dbCon = dbCon,
        scopes = "PDD_VALIDATOR", 
        statuses = "ACTIVE",
        idAgent = loginInfoUsr$id_agent,
        lsPreset = NULL) # TODO: implement lsPreset
      
      # Fetch the original schema. ---------------------------------------------
      {
        # 1. Fetch the original PDDxB schema.
        {
          schIri <- dbGetQuery(
            conn = dbCon, 
            statement = sprintf("SELECT iri FROM tbl_schemas WHERE id = '%s';",
                                idSchemaOrig))[["iri"]]
          
          sch <- jsonlite::read_json(
            path = sprintf("%s%s.json", schemadir, schIri),
            simplifyVector = TRUE,
            simplifyDataFrame = FALSE,
            flatten = FALSE)
          
          dfSchema <- schemaToDf(sch); rm(sch)
        }
        
        # 2. Fetch the Impact schema, because that's what we're really after.
        {
          sch <- jsonlite::read_json(
            path = sprintf(
              "%s%s.json", 
              schemadir, 
              dfSchema$iri_schema[which(dfSchema$key == "impacts")]),
            simplifyVector = TRUE,
            simplifyDataFrame = FALSE,
            flatten = FALSE)
          
          dfSchema <- schemaToDf(sch); rm(sch)
        }
        
      }
      
      # Build an index of lsPreset$review, if it is not NULL. -------------------------
      {
        # Note: x$field_key will actually be [indicator_label]_schemaFieldKey, e.g., LEAK_additionality.
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
        for (indIdx in 1:length(lsOriginal$cont$impacts)) {
          for (r in 1:nrow(dfSchema)) {

            extendedKey <- sprintf(
              "%s_%s", 
              lsOriginal$cont$impacts[[indIdx]]$indicator_label,
              dfSchema$key[r])
                        
            # Extract the previously saved reviewer response to this field, if any.
            revDecPreset <- "REJECT"
            revFdbckPreset <- ""
            if (length(keysLsPresetReview) > 0) {
              lsPresetIdx <- which(keysLsPresetReview == extendedKey)  
              if (length(lsPresetIdx) == 1) {
                revDecPreset <- lsPreset$review[[lsPresetIdx]]$reviewer_decision
                revFdbckPreset <- lsPreset$review[[lsPresetIdx]]$reviewer_feedback
              }
            }
            
            # Spin up the module server.
            modSrvrs[[extendedKey]] <- reviewFieldV2Server(
              id = extendedKey,
              fieldKey = extendedKey,
              fieldTitle = dfSchema$title[r],
              fieldPrompt = dfSchema$description[r],
              fieldVal = lsOriginal$cont$impacts[[indIdx]][[dfSchema$key[r]]],
              lsPreset = list(
                reviewer_decision = revDecPreset,
                reviewer_feedback = revFdbckPreset))
          }
        }
      }
      
      # Render a reviewFieldV2UI for each field to be reviewed.
      output$uioReviewFields <- renderUI({
        
        if (length(rvOther$currImpactIdx) == 0) {
          return(NULL)
        }
        if (rvOther$currImpactIdx == 0) {
          return(NULL)
        }

        tl <- tagList()
        
        for (r in 1:nrow(dfSchema)) {
          
          extendedKey <- sprintf(
            "%s_%s", 
            lsOriginal$cont$impacts[[rvOther$currImpactIdx]]$indicator_label,
            dfSchema$key[r])
          
          # Extract the previously saved reviewer response to this field, if any.
          revDecPreset <- "REJECT"
          revFdbckPreset <- ""
          if (length(keysLsPresetReview) > 0) {
            lsPresetIdx <- which(keysLsPresetReview == extendedKey)  
            if (length(lsPresetIdx) == 1) {
              revDecPreset <- lsPreset$review[[lsPresetIdx]]$reviewer_decision
              revFdbckPreset <- lsPreset$review[[lsPresetIdx]]$reviewer_feedback
            }
          }
          
          # Add a UI element to tl for this field.
          tl[[r]] <- reviewFieldV2UI(
            id = ns(extendedKey), 
            fieldTitle = dfSchema$title[r], 
            fieldPrompt = dfSchema$description[r], 
            lsPreset = list(
              reviewer_decision = revDecPreset,
              reviewer_feedback = revFdbckPreset))
        }
        
        tl <- c(
          tl, 
          tagList(
            br(),
            fluidRow(
              column(width = 5),
              column(
                width = 2,
                actionButton(inputId = ns("abCurrImpReviewOk"), 
                             label = "OK", 
                             width = "100%")),
              column(width = 5)),
            br()
              ))
        
        return(tl)
        
      })      
      
      observe({
        
        for (indIdx in 1:length(lsOriginal$cont$impacts)) {
          for (r in 1:nrow(dfSchema)) {
            
            extendedKey <- sprintf(
              "%s_%s", 
              lsOriginal$cont$impacts[[indIdx]]$indicator_label,
              dfSchema$key[r])
            
            # Connect the module server's 'results' to rvToReturn$results$review.
            rvToReturn$results$review[[extendedKey]] <- modSrvrs[[extendedKey]]$results
            
            # Connect the module server's 'allResultsGood' to resultsGood.
            resultsGood[[extendedKey]] <-  modSrvrs[[extendedKey]]$allResultsGood
            
            # Connect the module server's 'results$reviewer_decision' to reviewerDecisions.
            reviewerDecisions[[extendedKey]] <-  modSrvrs[[extendedKey]]$results$reviewer_decision
          }
        }
        
      })
      
      # outputs ----------------------------------------------------------------
      output$dtoImpacts <- DT::renderDataTable({
        
        lsdf <- lapply(X = 1:length(lsOriginal$cont$impacts), FUN = function(idx) {
          impact <- lsOriginal$cont$impacts[[idx]]
          return(
            data.frame(
              oidx = idx,
              label = impact$indicator_label,
              type = sprintf("%s, %s", impact$intentionality, impact$beneficial_or_adverse),
              uom = impact$indicator_unit_of_measure))
        })
        dfImpacts <- do.call("rbind", lsdf); rm(lsdf)
        
        # Add "Review" buttons to each row.
        dfImpacts$btn_review <- makeButtonHtmlForDocumentsGrid(
          documentIds = dfImpacts$oidx, buttonType = 'review')
        
        dfImpacts$oidx <- NULL
        names(dfImpacts) <- c("Label", "Type", "Unit of Measure", "Review")
        return(dfImpacts)
      }, 
      escape = F, rownames = FALSE, selection = c('none', 'single')[1], 
      options = list(
        processing = FALSE, 
        dom = "t", 
        scrollX = TRUE))
      
      # inputs -----------------------------------------------------------------
      
      observe({
        
        resultsGood$id_msg_lic <- FALSE
        resultsGood$url_ipfs_lic <- FALSE
        
        rvToReturn$results$headers$id_msg_lic <- modSrvrs$mPddValLic$results$id_msg
        rvToReturn$results$headers$url_ipfs_lic <- modSrvrs$mPddValLic$results$url_ipfs
        
        if (modSrvrs$mPddValLic$allResultsGood) {
          resultsGood$id_msg_lic <- TRUE
          resultsGood$url_ipfs_lic <- TRUE
        }
        
      })
      
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
      
      observeEvent(input$dtoImpacts_cell_clicked, handlerExpr = {
      
        res <- input$dtoImpacts_cell_clicked
        if (length(res) == 0) { return() }
        
        if (length(grep(pattern = "<button id=", x = res$value, fixed = TRUE)) == 0) {
          return()
        }
        
        oidx <- getDocumentIdFromButtonHtml(
          htmlTxt = res$value, buttonType = "review")
        rvOther$currImpactIdx <- as.integer(oidx)
          
      })

      observeEvent(input$abCurrImpReviewOk, handlerExpr = {
        rvOther$currImpactIdx <- NULL
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
