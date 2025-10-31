
# UI
tabStandardsBdUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    #useShinyjs(),
    
    uiOutput(outputId = ns("uioMain")))
}

# Server
tabStandardsBdServer <- function(id, 
                                 dbCon, 
                                 hederaClient,
                                 loginInfoUsr) {
  
  moduleServer(
    id = id, 
    function(input, output, session) {
      
      ns <- session$ns
      
      rvToReturn <- reactiveValues()
      rvToReturn$goToModule <- NULL 
      
      resultsGood <- reactiveValues()
      
      outputMsgs <- reactiveValues()
      
      rvOther <- reactiveValues()
      rvOther$uiMode <- c("showDocs", "doSchemaWork")[1]
      rvOther$refresh <- NULL
      rvOther$refresh_dfDocs <- NULL
      rvOther$nonce <- 0
      rvOther$dfSubset <- NULL
      rvOther$dfDocs <- NULL
      rvOther$vnmsDtoDocs <- NULL
      rvOther$dfWorkflows <- NULL
      rvOther$dfWorkflowSteps <- NULL
      
      # module servers ---------------------------------------------------------
      
      modSrvrs <- reactiveValues()
      modSrvrs$mReview <- NULL
      
      # ui rendering -----------------------------------------------------------
      
      observeEvent(rvOther$uiMode, handlerExpr = {
        
        validate(need(rvOther$uiMode, message = FALSE))
        
        if (rvOther$uiMode == "showDocs") {
          
          modSrvrs$mReview <- NULL
          rvOther$nonce <- rvOther$nonce + 1
          rvOther$refresh_dfDocs <- Sys.time() # It's important that this call precedes rvOther$refresh.
          rvOther$refresh <- Sys.time()
          
          output$uioMain <- renderUI({
            
            validate(need(rvOther$dfWorkflows, message = FALSE))
            
            wfOpts <- rvOther$dfWorkflows$id
            names(wfOpts) <- rvOther$dfWorkflows$lbl_chc
            
            return(
              tagList(
                wellPanel(
                  selectInput(inputId = ns("siWorkflow"), 
                              label = "Select workflow:", 
                              choices = wfOpts, 
                              selected = NULL, 
                              multiple = FALSE),
                  selectInput(inputId = ns("siEntity"), 
                              label = "Select entity:", 
                              choices = c(), 
                              selected = NULL, 
                              multiple = FALSE),
                  actionButton(inputId = ns("abGetDocList"), 
                               label = "OK"),
                  br(),
                  DT::dataTableOutput(outputId = ns("dtoDocs")))))
            
          })
          
          return(NULL)
        }
        
        if (rvOther$uiMode == "doSchemaWork") {
          
          output$uioMain <- renderUI({
            return(
              tagList(
                reviewUI(id = ns(sprintf("mReview%s", rvOther$nonce))),
                br(),
                br(),
                actionButton(inputId = ns("abBack"), 
                             label = "<< Back to documents")))
          })
          
          return(NULL)
        }
        
        warning(sprintf("Unknown value encountered for rvOther$uiMode."))
        
      })
      
      observe({
        validate(need(modSrvrs$mReview, message = FALSE))
        if (modSrvrs$mReview$done) {
          rvOther$uiMode <- "showDocs"
        }
      })
      
      observeEvent(input$abBack, handlerExpr = {
        rvOther$uiMode <- "showDocs"
      })
      
      # inputs -----------------------------------------------------------------
      
      observeEvent(rvOther$refresh, handlerExpr = {
        
        # Populate rvOther$dfWorkflows and rvOther$dfWorkflowSteps.

        dfWorkflows <- dbGetQuery(conn = dbCon, statement = "SELECT * FROM tbl_workflows;")
        dfWorkflows$lbl_chc <- sprintf(
          "%s (%s) [%s]", 
          gsub(pattern = "Independent Impact", replacement = "II", x = dfWorkflows$title, fixed = TRUE), 
          dfWorkflows$tag_version, 
          sprintf("%s...", substr(x = dfWorkflows$id, start = 1, stop = 4)))
        rvOther$dfWorkflows <- dfWorkflows
        
        lsdfMaps <- lapply(X = 1:nrow(dfWorkflows), FUN = function(r) {
          return(
            getWorkflowStepMap(
              nmWrkflwHndlr = dfWorkflows$handler_r[r], 
              idWorkflow = dfWorkflows$id[r], 
              dbCon = dbCon))
        })
        
        df <- do.call("rbind.fill", lsdfMaps); rm(lsdfMaps)
        rvOther$dfWorkflowSteps <- df
        
      })
      
      observeEvent(input$siWorkflow, handlerExpr = {
        
        validate(need(input$siWorkflow, message = FALSE))

        q <- sprintf(
          "SELECT DISTINCT(id_entity) AS id_entity FROM tbl_document_metadata WHERE id_workflow = '%s';",
          input$siWorkflow)
        cEntities <- dbGetQuery(conn = dbCon, statement = q)[["id_entity"]]
        
        updateSelectInput(
          session = session, 
          inputId = "siEntity", 
          choices = cEntities, 
          selected = NULL)
        
      })
      
      observeEvent(input$abGetDocList, handlerExpr = {
        validate(need(input$siEntity, message = FALSE))
        rvOther$refresh_dfDocs <- Sys.time()
      })
      
      observeEvent(rvOther$refresh_dfDocs, handlerExpr = {
        
        validate(need(input$siEntity, message = FALSE))
        
        q <- sprintf("SELECT * FROM tbl_document_metadata WHERE id_workflow = '%s' AND id_entity = '%s';",
                     input$siWorkflow, input$siEntity)
        dfDocs <- dbGetQuery(conn = dbCon, statement = q)
        
        if (length(dfDocs) == 0) {
          rvOther$dfDocs <- NULL
          return(NULL)
        }
        if (nrow(dfDocs) == 0) {
          rvOther$dfDocs <- NULL
          return(NULL)
        }
        
        # Order chronologically.
        dfDocs <- dfDocs[order(dfDocs$date_created, decreasing = TRUE),]
        rownames(dfDocs) <- 1:nrow(dfDocs)
        
        # Done.
        rvOther$dfDocs <- dfDocs
        
      })

      observeEvent(input$dtoDocs_cell_clicked, handlerExpr = {
        
        res <- input$dtoDocs_cell_clicked
        if (length(res) == 0) { return() }
        
        idxRow <- res$row
        idxCol <- res$col
        val <- res$value
        
        # If the user didn't click one of the buttons, simply ignore the click.
        if (!(names(rvOther$vnmsDtoDocs)[idxCol+1] %in% c("btn_view", "btn_review", "btn_revoke")) | 
            (length(val) == 0)) {
          return(NULL)
        }
        if (is.na(val)) { return(NULL) }
        if (nchar(val) == 0) { return(NULL) }

        # If the user clicked a 'View' button, show the document in a modal.
        if (length(grep(pattern = 'id="abView[[:digit:]]{1,}"', x = val)) == 1) {
          
          showModal(loadingDocumentModal())
          
          # Get the IPFS URL of the schema that defined the document format.
          {
            q <- sprintf("SELECT uri_ipfs FROM tbl_schemas WHERE id = '%s';",
                         rvOther$dfDocs$id_schema[idxRow])
            ipfsUriSch <- dbGetQuery(conn = dbCon, statement = q)[["uri_ipfs"]]

            if (length(ipfsUriSch) == 0) {
              removeModal()
              showModal(
                modalDialog("Failed to retrieve schema IPFS URI.", 
                            title = "Error", 
                            footer = NULL, 
                            size = "s", 
                            easyClose = TRUE))
              return(NULL)
            }
            
            #message("ipfsUriSch: ", ipfsUriSch)
          }
          
          docModal <- viewDocumentModal3(
            idDoc = rvOther$dfDocs$id[idxRow], 
            dbCon = dbCon, 
            contentOnly = FALSE)
          
          removeModal()
          
          showModal(docModal)
          
          return(NULL)
        }
        
        # If the user clicked a 'Review' button, serve the review module.
        if (length(grep(pattern = 'id="abReview[[:digit:]]{1,}"', x = val)) == 1) {
          
          modSrvrs$mReview <- reviewServer(
            id = sprintf("mReview%s", rvOther$nonce), 
            dbCon = dbCon, 
            loginInfoUsr = loginInfoUsr, 
            idDocOrig = rvOther$dfDocs$id[idxRow],
            hederaClient = hederaClient) 
          
          rvOther$uiMode <- c("showDocs", "doSchemaWork")[2]
          
          return(NULL)
        }
        
        # If the user clicked a 'Revoke' button, serve a modal.
        {
          # TODO.
          return(NULL)
        }
        
      })
      
      # outputs ----------------------------------------------------------------
      
      output$dtoDocs <- DT::renderDataTable({
        
        validate(need(rvOther$dfDocs, message = FALSE))
        
        df <- rvOther$dfDocs
        
        # Add buttons.
        {
          # View buttons for all.
          df$btn_view <- sprintf('<button id="abView%d" type="button" class="btn btn-default action-button">View</button>',
                                 1:nrow(df))
          
          # Review buttons for VCs with status2 == PUBLISHED/SUBMITTED and 
          # did_creator != us and outcome != APPROVED.
          {
            df$btn_review <- NA_character_
            idxx <- which(df$status %in% c("PUBLISHED", "SUBMITTED") &
                            df$did_author != loginInfoUsr$did &
                            df$outcome != "APPROVED")
            df$btn_review[idxx] <- sprintf('<button id="abReview%d" type="button" class="btn btn-default action-button">Review</button>',
                                           idxx)
          }
          
          # # Revoke buttons for VPs that were issued by the current, acting SB.
          # {
          # TODO.
          #   df$btn_revoke <- NA_character_
          #   idxx <- which(df$type == "VerifiablePresentation" &
          #                   df$did_issuer == loginInfoSb$did &
          #                   df$status != "Revoked")
          #   df$btn_revoke[idxx] <- sprintf('<button id="abRevoke%d" type="button" class="btn btn-default action-button">Revoke</button>',
          #                                  idxx)
          # }
        }
        
        # Translate did_creator and did_owner to human-friendly names, where
        # possible. If not possible, abbreviate.
        {
          idxx <- grep(pattern = "^did:hedera:", x = df$did_author)
          df$did_author[idxx] <- substr(x = df$did_author[idxx], 
                                       start = 1, 
                                       stop = 25)
        }
        
        # # Abbreviate the 'type' variable.
        # df$type[which(df$type == "VerifiablePresentation")] <- "VP"
        # df$type[which(df$type == "VerifiableCredential")] <- "VC"
        
        # Subset to display variables.
        {
          vnmsDtoDocs <- c("type" = "Type",
                           "doc_type_abbr" = "Form", 
                           "date_created" = "Date Created", 
                           "status" = "Status", 
                           "outcome" = "Outcome", 
                           "did_author" = "Author", 
                           "btn_view" = "View", 
                           "btn_review" = "Review", 
                           "btn_revoke" = "Revoke")
          vnmsDtoDocs <- vnmsDtoDocs[intersect(names(vnmsDtoDocs), names(df))]
          
          df <- df[names(vnmsDtoDocs)]
          names(df) <- vnmsDtoDocs
          rvOther$vnmsDtoDocs <- vnmsDtoDocs
        }
        
        return(df)
      }, 
      escape = F, rownames = FALSE, selection = c('none', 'single')[1], 
      options = list(processing = FALSE, dom = "tp", scrollX = TRUE, pageLength = 10))
      
      # ------------------------------------------------------------------------
      
      #rvOther$uiMode <- "showDocs"
      
      #rvOther$refresh <- Sys.time()
      
    })
  
}
