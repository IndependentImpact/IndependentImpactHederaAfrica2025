
schemaUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    ##useShinyjs(),
    
    uiOutput(outputId = ns("uioSchTitle")),
    
    uiOutput(outputId = ns("uioSchContent")),
    
    textOutput(outputId = ns("toMain")),
    
    fluidRow(
      column(width = 4),
      column(
        width = 2,
        actionButton(inputId = ns('abSave'), 
                     label = 'Save', 
                     width = '100%')),
      column(
        width = 2,
        actionButton(inputId = ns('abSubmit'), 
                     label = 'Submit', 
                     width = '100%')),
      column(width = 4))  #  ,
    # actionButton(inputId = ns("abClose"), 
    #              label = "Close")
  )
}

schemaServer <- function(id,  
                         nmSchemaModSrvr, 
                         nmSchemaModUI,
                         schemaUIargs = NULL,
                         dbCon,
                         loginInfoUsr,
                         hederaClient,
                         idEntity, # Who (idAgent) or what (idProject) is the schema about?
                         idWorkflow, 
                         idSchema, 
                         stepWorkflow,
                         idDoc = NULL,
                         lsPreset = NULL,
                         lsOriginal = NULL,
                         bReturnRes = FALSE) {
  
  # # TODO. Remove after debugging.
  # lsArgsSchemaServer <- list(
  #   nmSchemaModSrvr = nmSchemaModSrvr, 
  #   nmSchemaModUI = nmSchemaModUI,
  #   schemaUIargs = schemaUIargs,
  #   loginInfoUsr = loginInfoUsr,
  #   idEntity = idEntity,
  #   idWorkflow = idWorkflow, 
  #   idSchema = idSchema, 
  #   stepWorkflow = stepWorkflow,
  #   idDoc = idDoc,
  #   lsPreset = lsPreset,
  #   lsOriginal = lsOriginal,
  #   bReturnRes = bReturnRes)
  # save(lsArgsSchemaServer, file = sprintf("%slsArgsSchemaServer.Rda", tmpdir))
  
  moduleServer(
    id = id, 
    function(input, output, session) {
      
      ns <- session$ns
      
      rvToReturn <- reactiveValues()
      rvToReturn$done <- FALSE
      rvToReturn$idDoc <- idDoc
      rvToReturn$results <- reactiveValues()
      
      outputMsgs <- reactiveValues()
      outputMsgs$main <- ""
      
      rvOther <- reactiveValues()
      
      # module servers ---------------------------------------------------------
      
      modSrvrs <- reactiveValues()
      modSrvrs$mSchema <- NULL
      
      observe({
        if (bReturnRes) {
          if (length(modSrvrs$mSchema) > 0) {
            rvToReturn$results <- modSrvrs$mSchema$results # Needed when we (as module 'schema') were called by module 'review'.
          }
        }
      })
      
      # ui rendering -----------------------------------------------------------
      
      observe({
        if (length(modSrvrs$mSchema) > 0) {
          shinyjs::toggleState(id = 'abSubmit',
                               condition = modSrvrs$mSchema$allResultsGood, 
                               asis = FALSE) 
        }
      })
      
      # inputs -----------------------------------------------------------------
      
      # observeEvent(input$abClose, handlerExpr = {
      #   rvToReturn$done <- TRUE
      # })
      
      observeEvent(input$abSave, handlerExpr = {
        
        showModal(
          modalDialog(
            title = "Saving document...", 
            footer = NULL, 
            size = "s", 
            easyClose = FALSE))
        
        tryCatch({
          rvToReturn$idDoc <- saveDraftDoc(
            x = recursiveReactiveValuesToList(modSrvrs$mSchema$results),
            didAgent = loginInfoUsr$did,
            idEntity = idEntity,
            docId = rvToReturn$idDoc,
            idSchema = idSchema, 
            idWorkflow = idWorkflow, 
            stepWorkflow = stepWorkflow,
            dbCon = dbCon)
          bSaveSucc <- TRUE
        }, error = function(e) {
          warning(as.character(e))
        })
        
        removeModal()
        
        if (!exists("bSaveSucc")) {
          showModal(
            modalDialog(
              title = "Error",
              "An error occurred while saving the document. Please try again. If the problem persists, please contact the system administrators.",
              footer = NULL, 
              size = "s", 
              easyClose = T))
        } else {
          showModal(
            modalDialog(
              title = "Document saved.",
              footer = NULL, 
              size = "s", 
              easyClose = T))
        }
        
      })
      
      observeEvent(input$abSubmit, handlerExpr = {
        
        if (!modSrvrs$mSchema$allResultsGood) {
          outputMsgs$main <- "Some required fields are either missing or contain invalid responses. Please review your answers."
          return(NULL)
        } else {
          outputMsgs$main <- ""
        }
        
        showModal(
          modalDialog(
            title = "Submitting document...", 
            "This may take a few moments. Please be patient.",
            footer = NULL, 
            size = "s", 
            easyClose = FALSE))
        
        # Make sure all reactive values have been transformed to non-reactive 
        # values.
        doc <- recursiveReactiveValuesToList(modSrvrs$mSchema$results)

        # Submit the document.
        res <- submitDocToHedera(
          doc = doc,
          idDoc = rvToReturn$idDoc,
          dbCon = dbCon,
          idSchema = idSchema, 
          idWorkflow = idWorkflow, 
          stepWorkflow = stepWorkflow, 
          idAgent = loginInfoUsr$id_agent,
          idEntity = idEntity,
          hederaClient = hederaClient)

        removeModal()
        
        if (res$status_code == 200) {
          
          # TODO: Move the logic below out to the workflow triggers.
          # Interlude
          # If this is a PDD-A schema, extract the project location for display in our UI.
          # If this is a PDD-B schema, extract the impact information for display in our UI.
          # If this is ...
          {
            {
              # # TODO. Remove after debugging.
              # save(doc, file = sprintf("%sschema_doc.Rda", tmpdir))
              # save(res, file = sprintf("%sschema_res.Rda", tmpdir))
            }
            
            if (nmSchemaModSrvr == "PDDXAschemaV2Server") {
              
              locations <- doc$location_project
              
              for (k in 1:length(locations)) {
                addToDb(
                  dfAdd = data.frame(
                    id_project = idEntity,
                    url_ipfs_location = locations[[k]]$location,
                    submitted = res$doc_md$id), 
                  tblNm = "tbl_project_locations", 
                  vnmsChckEx = c("id_project", "url_ipfs_location"), 
                  dbCon = dbCon, 
                  calcIds = FALSE)
              }
            }
            
            if (nmSchemaModSrvr == "PDDXBschemaV2Server") {
              
              for (k in 1:length(doc$impacts)) {
                
                dfAdd = data.frame(
                  id_project = idEntity,
                  label_indicator = doc$impacts[[k]]$indicator_label,
                  obj_indicator = jsonlite::toJSON(
                    x = doc$impacts[[k]],
                    dataframe = "rows", 
                    matrix = "rowmajor", 
                    factor = "string",
                    auto_unbox = TRUE),
                  submitted = res$doc_md$id)
                
                addToDb(dfAdd = dfAdd, 
                        tblNm = "tbl_project_impact_indicators", 
                        vnmsChckEx = c("id_project", "label_indicator"), 
                        dbCon = dbCon, 
                        calcIds = FALSE)
              }
            }
            
            if (res$doc_md$step_workflow == "rvcdb_vldtr_DR_PDDxA") {
              if (doc$final_rd == "APPROVE") {
                
                # Mark the locations in our db for this project as approved.
                
                # 1. Get the ID of the document that was reviewed.
                q <- sprintf("SELECT id FROM tbl_document_metadata WHERE id_entity = '%s' AND id_message_h = '%s';", 
                             idEntity, res$doc_md$id_msg_pred)
                origDocId <- dbGetQuery(conn = dbCon, statement = q)$id
                
                # 2. Update tbl_project_locations.
                df <- data.frame(
                  id_project = idEntity, 
                  approved = res$doc_md$id,
                  submitted = origDocId)
                updateDb(df = df, 
                         tblNm = "tbl_project_locations", 
                         idVars = c("id_project", "submitted"), 
                         dbCon = dbCon)
              }
            }
            
            if (res$doc_md$step_workflow == "rvcdb_vldtr_DR_PDDxB") {
              if (doc$final_rd == "APPROVE") {
                
                # Mark the indicators in our db for this project as approved.
                
                # 1. Get the ID of the document that was reviewed.
                q <- sprintf("SELECT id FROM tbl_document_metadata WHERE id_entity = '%s' AND id_message_h = '%s';", 
                             idEntity, res$doc_md$id_msg_pred)
                origDocId <- dbGetQuery(conn = dbCon, statement = q)$id
                
                # 2. Update tbl_project_impact_indicators.
                df <- data.frame(
                  id_project = idEntity, 
                  approved = res$doc_md$id,
                  submitted = origDocId)
                updateDb(df = df, 
                         tblNm = "tbl_project_impact_indicators", 
                         idVars = c("id_project", "submitted"), 
                         dbCon = dbCon)
              }
            }
            
          }
          
          rvToReturn$idDoc <- res$doc_md$id
          
          tl <- tagList(
            h5(sprintf("Hedera Message ID: %s",
                       ifelse(!is.na(res$doc_md$id_message_h),
                              ifelse(nchar(res$doc_md$id_message_h) > 0,
                                     res$doc_md$id_message_h,
                                     "(none yet)"),
                              "(none yet)"))),
            h5(sprintf("IPFS URI: %s",
                       ifelse(!is.na(res$doc_md$uri_ipfs),
                              ifelse(nchar(res$doc_md$uri_ipfs) > 0,
                                     res$doc_md$uri_ipfs,
                                     "(none yet)"),
                              "(none yet)"))))
          
          # Share the details with the user.
          showModal(
            modalDialog(
              title = "Document submitted",
              tl,
              footer = modalButton("OK"),
              size = "m",
              easyClose = F))
          
          rvToReturn$done <- TRUE # Necessary, otherwise module 'review' will not
          # know to use the result of this module to perform updates on documents'
          # outcomes.
          
        } else {
          
          # Show error message with status code.
          showModal(
            modalDialog(
              title = "Error",
              sprintf("Document could not be submitted. Error code: %s. %s",
                      res$status_code,
                      ifelse(length(res$err_msg) == 0,
                             "",
                             sprintf("Error message: %s", res$err_msg))),
              footer = modalButton("OK"),
              size = "s",
              easyClose = F))
        }
        
      })
      
      # outputs ----------------------------------------------------------------
      
      output$uioSchTitle <- renderUI({
        
        q <- sprintf("SELECT title, tag_version FROM tbl_schemas WHERE id = '%s';",
                     idSchema)
        df <- dbGetQuery(conn = dbCon, statement = q) 
        
        return(
          wellPanel(
            h4(sprintf("Document: %s (version %s)", df$title, df$tag_version))))
        
      })
      
      output$uioSchContent <- renderUI({
        
        # One of the tricky things with modules is that the UI function of the
        # module should ALWAYS be called BEFORE the server function of the module,
        # otherwise the UI function's default input/output settings will override 
        # any input/output settings that the server function may have set via
        # updateNumericInput or update<whatever>Input.
        
        # 1. Call the UI function of the module, but don't render its results yet.
        {
          if (length(schemaUIargs) == 0) {
            schemaUIargs <- list()
          }
          schemaUIargs$id <- ns("mSchema")
          schemaUIargs$inpWidth <- DEFAULT_INP_WIDTH
          
          res <- do.call(
            what = nmSchemaModUI, 
            args = schemaUIargs)
        }
        
        # 2. Call the server function of the module.
        {
          lsArgs <- list(id = "mSchema",  
                         dbCon = dbCon,
                         idEntity = idEntity,
                         lsPreset = lsPreset,
                         loginInfoUsr = loginInfoUsr)
          
          if (length(grep(pattern = "DocumentReview|^DR", 
                          x = nmSchemaModSrvr, 
                          ignore.case = FALSE)) == 1) {
            
            lsArgs$lsOriginal <- lsOriginal
          }
          
          if (nmSchemaModSrvr %in% c("genericDocumentReviewSchemaServer",
                                     "DRVICIRschemaServer",
                                     "DRPDDXBschemaV2Server")) {
            
            lsArgs$lsOriginal <- lsOriginal
            
            # TODO: This is super hacky. Improve this when we have more time.
            licenseReq <- NULL
            if (length(grep(pattern = "^rvcdb_standardBody", x = stepWorkflow) == 1)) {
              licenseReq <- "STANDARDS_BODY"
            } else {
              if (length(grep(pattern = "^rvcdb_verifier", x = stepWorkflow) == 1)) {
                licenseReq <- "MR_VERIFIER"
              } else {
                if (length(grep(pattern = "^rvcdb_vldtr", x = stepWorkflow) == 1)) {
                  licenseReq <- "PDD_VALIDATOR"
                }
              }
              
            }
            
            if (length(licenseReq) == 0) {
              stop("Failed to determine license requirements for reviewer role.")
            }
            lsArgs$licenseReq <- licenseReq
            
            # The genericDocumentReviewSchemaServer (and DRVICIRschemaServer, DRPDDXBschemaV2Server) has 
            # an idSchemaOrig param instead of an idSchema param, so we need 
            # to determine that.
            {
              q <- sprintf("SELECT id_schema FROM tbl_document_metadata WHERE id = '%s';",
                           lsOriginal$md$id)
              idSchemaOrig <- dbGetQuery(conn = dbCon, statement = q)[["id_schema"]][1]
              lsArgs$idSchemaOrig <- idSchemaOrig
            }
            
          }

          modSrvrs$mSchema <- do.call(
            what = nmSchemaModSrvr, 
            args = lsArgs)
        }
        
        # 3. Render the results of the call to the UI function of the module.
        return(res)
        
      })
      
      output$toMain <- renderText({
        outputMsgs$main
      })
      
      # return logic -----------------------------------------------------------
      
      return(rvToReturn)
      
    })
}
