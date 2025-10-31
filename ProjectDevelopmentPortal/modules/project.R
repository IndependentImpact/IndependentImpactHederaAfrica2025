
projectUI <- function(id) {

  ns <- NS(id)

  tagList(

    ##useShinyjs(),

    uiOutput(outputId = ns("uioMain")))
}

projectServer <- function(id,
                          idProject,
                          dbCon, 
                          loginInfoUsr,
                          hederaClient) {
  
  moduleServer(
    id = id,
    function(input, output, session) {

      ns <- session$ns

      rvToReturn <- reactiveValues()
      rvToReturn$goToModule <- NULL
      rvToReturn$argsForGoToModule <- NULL

      rvOther <- reactiveValues()
      rvOther$dfProject <- NULL
      rvOther$dfProjAgents <- NULL
      rvOther$dfProjWorkflows <- NULL
      #rvOther$dfProjTokens <- NULL
      rvOther$dfProjVICs <- NULL
      rvOther$dfProjLocations <- NULL
      rvOther$dfProjImpacts <- NULL
      rvOther$dfDocs <- NULL
      rvOther$initialise <- NULL
      rvOther$refresh_dfProject <- NULL
      rvOther$refresh_dfProjLocations <- NULL
      rvOther$refresh_dfProjImpacts <- NULL
      rvOther$refresh_dfDocs <- NULL
      rvOther$vnmsDtoProjWrkflws <- NULL
      rvOther$vnmsDtoDocs <- NULL
      rvOther$vnmsDtoProjVICs <- NULL
      rvOther$dfTknIssuances <- NULL
      #rvOther$nmSchemaModUI <- NULL
      #rvOther$schemaUIargs <- NULL
      rvOther$dfWorkflowDets <- NULL
      rvOther$nonce <- 0
      rvOther$nonce2 <- 0
      rvOther$uiMode <- NULL
      rvOther$wrkflwTitles_dtoProjWrkflws <- NULL
      rvOther$documentGridView <- c("grouped", "ungrouped")[1]
      rvOther$projLocCurrIdx <- NULL
      rvOther$dtoDocsShowAdditionalInfo <- FALSE
      #rvOther$agentDid <- NULL

      outputMsgs <- reactiveValues()
      outputMsgs$projectLocation <- "Loading..."
      outputMsgs$quantifiedImpacts <- "Loading..."

      # module servers ---------------------------------------------------------

      modSrvrs <- reactiveValues()
      modSrvrs$mWrkflwState <- NULL
      modSrvrs$mReview <- NULL
      modSrvrs$mWrkflwStep <- NULL
      modSrvrs$mMonPers <- NULL
      modSrvrs$mMonParams <- NULL
      
      # modSrvrs$mWrkflwStep$done
      
      observe({
        validate(need(modSrvrs$mWrkflwStep, message = FALSE))
        if (length(modSrvrs$mWrkflwStep$done) == 0) { return(invisible(NULL)) }
        if (modSrvrs$mWrkflwStep$done) {
          modSrvrs$mWrkflwStep <- NULL
          rvOther$nonce <- rvOther$nonce + 1
          rvOther$refresh_dfDocs <- Sys.time()
          rvOther$uiMode <- 1
        }
      })

      # modSrvrs$mReview$done
      observe({
        validate(need(modSrvrs$mReview, message = FALSE))
        if (length(modSrvrs$mReview$done) == 0) { return(invisible(NULL)) }
        if (modSrvrs$mReview$done) {
          modSrvrs$mReview <- NULL
          rvOther$nonce <- rvOther$nonce + 1
          rvOther$refresh_dfDocs <- Sys.time()
          rvOther$uiMode <- 1
        }
      })

      # ui rendering -----------------------------------------------------------

      output$uioMain <- renderUI({

        validate(need(rvOther$uiMode, message = FALSE))
        
        modSrvrs$mWrkflwState <- NULL
        modSrvrs$mReview <- NULL
        modSrvrs$mWrkflwStep <- NULL
        modSrvrs$mMonPers <- NULL
        modSrvrs$mMonParams <- NULL

        hL <- 4
        colWidth <- 12
        inpWidth <- DEFAULT_INP_WIDTH

        if (rvOther$uiMode == 1) { # Project overview and documents.

          # modSrvrs$mMonPers <- monitoringPeriodsServer(
          #   id = sprintf("mMonPers%d", rvOther$nonce),
          #   idEntity = idProject,
          #   dbCon = dbCon,
          #   lsPreset = NULL)
          #
          # modSrvrs$mMonParams <- monitoredParametersServer(
          #   id = sprintf("mMonParams%d", rvOther$nonce),
          #   dbCon = dbCon,
          #   idEntity = idProject,
          #   lsPreset = NULL)

          tl <- tagList(

            shinyjs::useShinyjs(),

            fluidRow(
              column(
                width = 3,
                GtextOutput(id = ns("toTtlProj"),
                            title = "Project Title",
                            hL = hL,
                            colWidth = colWidth)),
              column(
                width = 3,
                GtextOutput(id = ns("toDateCreated"),
                            title = "Date Created",
                            hL = hL,
                            colWidth = colWidth)),
              column(
                width = 3,
                GtextOutput(id = ns("toProjectHederaTopicId"),
                            title = "Hedera Topic ID",
                            hL = hL,
                            colWidth = colWidth)),
              column(
                width = 3,
                GtextOutput(id = ns("toCreatedBy"),
                            title = "Created By",
                            hL = hL,
                            colWidth = colWidth))),

            # bsTooltip(id = ns("toCreatedBy"),
            #           title = "DID",
            #           placement = "bottom",
            #           trigger = "hover"),

            fluidRow(

              column(
                width = 6,
                wellPanel(
                  hx(x = "Project Location", lvl = hL),
                  textOutput(outputId = ns("toProjectLocation")),
                  br(),
                  leaflet::leafletOutput(outputId = ns("pltoProjectLocation")),
                  br(),
                  fluidRow(
                    column(width = 2),
                    column(
                      width = 4,
                      actionButton(
                        inputId = ns("abProjLocPrev"),
                        label = "Prev",
                        width = "100%")),
                    column(
                      width = 4,
                      actionButton(
                        inputId = ns("abProjLocNext"),
                        label = "Next",
                        width = "100%")),
                    column(width = 2)),
                  br())),

              column(
                width = 6,
                wellPanel(
                  hx(x = "Quantified Impacts", lvl = hL),
                  textOutput(outputId = ns("toQuantifiedImpacts")),
                  br(),
                  DT::dataTableOutput(outputId = ns("dtoQuantifiedImpacts")),
                  br()))),


            # # Monitored Parameters.
            # fluidRow(
            #   column(
            #     width = colWidth,
            #     wellPanel(
            #       HTML(sprintf("<h%d>Monitored Parameters</h%d>", hL, hL)),
            #       monitoredParametersUI(
            #         id = ns(sprintf("mMonParams%d", rvOther$nonce)),
            #         viewOnly = (rvOther$dfProject$created_by != loginInfoUsr$id_agent))))),

            # # Monitoring Periods.
            # fluidRow(
            #   column(
            #     width = colWidth,
            #     wellPanel(
            #       HTML(sprintf("<h%d>Monitoring Periods</h%d>", hL, hL)),
            #       monitoringPeriodsUI(
            #         id = ns(sprintf("mMonPers%d", rvOther$nonce)),
            #         viewOnly = (rvOther$dfProject$created_by != loginInfoUsr$id_agent))))),

            # TODO. Data Sources. (These are activities, such as "continuous
            # monitoring" [using autonomous sensors] or the KPT.)
            # When you click on a data source you can view the different data
            # sets [and their DLRs] produced by the data source.

            # fluidRow(
            #   column(
            #     width = colWidth,
            #     wellPanel(
            #       HTML(sprintf("<h%d>Associated Tokens</h%d>", hL, hL)),
            #       textOutput(outputId = ns("toDtoProjTokens")),
            #       DT::dataTableOutput(ns("dtoProjTokens"))))),
            
            fluidRow(
              column(
                width = colWidth,
                wellPanel(
                  HTML(sprintf("<h%d>Verified Impact Certificates</h%d>", hL, hL)),
                  textOutput(outputId = ns("toDtoProjVICs")),
                  DT::dataTableOutput(ns("dtoProjVICs"))))),
            
            fluidRow(
              column(
                width = colWidth,
                wellPanel(
                  HTML(sprintf("<h%d>Associated Workflows</h%d>", hL, hL)),
                  textOutput(outputId = ns("toDtoProjWrkflws")),
                  DT::dataTableOutput(ns("dtoProjWrkflws"))))),
            
            fluidRow(
              column(
                width = colWidth,
                wellPanel(
                  HTML(sprintf("<h%d>Associated Agents</h%d>", hL, hL)),
                  textOutput(outputId = ns("toDtoProjAgents")),
                  DT::dataTableOutput(ns("dtoProjAgents"))))),

            fluidRow(
              column(
                width = colWidth,
                wellPanel(
                  HTML(sprintf("<h%d>Documents</h%d>", hL, hL)),
                  textOutput(outputId = ns("toDtoDocs")),
                  br(),
                  fluidRow(
                    column(
                      width = 2,
                      actionButton(inputId = ns("abSwitchDocumentGridView"),
                                   label = "Switch View",
                                   width = "100%")),
                    column(
                      width = 3,
                      checkboxInput(inputId = ns("cbiShowAdditionalInfo"),
                                    label = "Show Additional Info",
                                    value = isolate(rvOther$dtoDocsShowAdditionalInfo))),
                    column(width = 7)),

                  DT::dataTableOutput(ns("dtoDocs")),

                  tags$script(HTML("
    function toggleAdditionalInfoVisibility(columnName, tableId, setTo) {
      var table = $('#' + tableId + ' table').DataTable();

      var columnIndex = table.columns().header().toArray().findIndex(header =>
        $(header).text().trim() === columnName
      );

      var column = table.column(columnIndex);
      column.visible(setTo);
    }
  "))))))

          return(tl)
        }
        
        if (rvOther$uiMode == 2) { # Workflow state.
          return(
            tagList(

              fluidRow(
                column(
                  width = 1),
                column(
                  width = 10,
                  workflowStateUI(id = ns(sprintf("mWrkflwState%d", 
                                                isolate(rvOther$nonce))))),
                column(
                  width = 1)),

              fluidRow(
                column(width = 1),
                column(
                  width = 2,
                  actionButton(inputId = ns("abBack"),
                               label = "<< Back to project",
                               width = "100%")),
                column(width = 9))))
        }

        if (rvOther$uiMode == 3) { # Review a document.
          return(
            tagList(
              reviewUI(id = ns(sprintf("mReview%s", isolate(rvOther$nonce)))),
              br(),
              br(),
              fluidRow(
                column(
                  width = 2,
                  actionButton(inputId = ns("abBack"),
                               label = "<< Back to project",
                               width = "100%")),
                column(width = 10))))
        }

        # if (rvOther$uiMode == 4) { # Edit a draft document.
        #
        #   return(
        #     tagList(
        #       wellPanel(
        #         h3(rvOther$dfWorkflowDets$name),
        #         h4(sprintf("Version %s", 
        #                    rvOther$dfWorkflowDets$version))),
        #       # schemaUI(id = ns(sprintf("mSchema%d", isolate(rvOther$nonce))),
        #       #          nmSchemaModUI = rvOther$nmSchemaModUI,
        #       #          schemaUIargs = rvOther$schemaUIargs),
        #       schemaUI(id = ns(sprintf("mSchema%d", isolate(rvOther$nonce)))),
        #       br(),
        #       br(),
        # fluidRow(
        #   column(
        #     width = 2,
        #     actionButton(inputId = ns("abBack"),
        #                  label = "<< Back to project",
        #                  width = "100%")),
        #   column(width = 10)))
        #
        # }

        warning("Unknown value encountered for rvOther$uiMode.")
      })

      observeEvent(input$abBack, handlerExpr = {
        modSrvrs$mWrkflwState <- NULL
        modSrvrs$mReview <- NULL
        modSrvrs$mWrkflwStep <- NULL
        modSrvrs$mMonPers <- NULL
        modSrvrs$mMonParams <- NULL
        rvOther$nonce <- rvOther$nonce + 1
        rvOther$refresh_dfDocs <- Sys.time()
        rvOther$uiMode <- 1
      })

      # initialisation----------------------------------------------------------

      observeEvent(rvOther$initialise, handlerExpr = {
        rvOther$refresh_dfProject <- Sys.time()
        rvOther$refresh_dfProjAgents <- Sys.time()
        rvOther$refresh_dfProjWrkflws <- Sys.time()
        #rvOther$refresh_dfProjTokens <- Sys.time()
        rvOther$refresh_dfProjVICs <- Sys.time()
        rvOther$refresh_dfDocs <- Sys.time()
        rvOther$refresh_dfProjLocations <- Sys.time()
        rvOther$refresh_dfProjImpacts <- Sys.time()

        rvOther$uiMode <- 1
      })

      observeEvent(rvOther$refresh_dfProject, handlerExpr = {

        # Refresh rvOther$dfProject.
        q <- sprintf("SELECT * FROM tbl_projects WHERE id = '%s';", idProject)
        rvOther$dfProject <- dbGetQuery(conn = dbCon, statement = q)

      })

      observeEvent(rvOther$refresh_dfProjAgents, handlerExpr = {

        rvOther$dfProjAgents <- NULL
        
        q <- sprintf("SELECT * FROM tbl_link_projects_x_agents WHERE id_project = '%s';",
                     idProject)
        dfAgents <- dbGetQuery(conn = dbCon, statement = q)
        dfAgents[c("oidx", "id_project")] <- NULL
        
        if (nrow(dfAgents) == 0) {
          return(invisible(NULL))
        }
        
        rvOther$dfProjAgents <- dfAgents; rm(dfAgents)
        
      })
      
      observeEvent(rvOther$refresh_dfProjWrkflws, handlerExpr = {
        
        rvOther$dfProjWorkflows <- NULL
        validate(need(rvOther$dfProjAgents, message = FALSE))
        
        q <- sprintf("SELECT * FROM tbl_workflows WHERE id IN(%s);",
                     paste(sprintf("'%s'", unique(rvOther$dfProjAgents$id_workflow)), 
                           collapse = ","))
        df <- dbGetQuery(conn = dbCon, statement = q)
        
        rvOther$dfProjWorkflows <- df; rm(df)
        
      })
      
      observeEvent(rvOther$refresh_dfProjVICs, handlerExpr = {
        
        rvOther$dfProjVICs <- NULL
        
        dfVICs <- dbGetQuery(
          conn = dbCon, 
          statement = sprintf(
            "SELECT * FROM tbl_impact_certificates WHERE id_entity = '%s';",
            idProject))
        if (length(dfVICs) == 0) { return(NULL) }
        if (nrow(dfVICs) == 0) { return(NULL) }

        names(dfVICs)[names(dfVICs) == "id"] <- "id_cert"
        
        # Add the document ID to the df.
        dfDocIds <- dbGetQuery(
          conn = dbCon, 
          statement = sprintf(
            "SELECT id, id_message_h FROM tbl_document_metadata WHERE id_message_h IN(%s);",
            paste(sprintf("'%s'", dfVICs$id_message_h), 
                  sep = "", collapse = ",")))
        idxx <- match(x = dfVICs$id_message_h, table = dfDocIds$id_message_h)
        dfVICs$id_doc <- dfDocIds$id[idxx]

        rvOther$dfProjVICs <- dfVICs
        
      })

      observeEvent(rvOther$refresh_dfDocs, handlerExpr = {
        
        df <- dbFetch(
          dbSendQuery(
            conn = dbCon, 
            statement = sprintf(
              "SELECT * FROM tbl_document_metadata WHERE id_entity = '%s';", 
              idProject)))
        
        if (nrow(df) > 0) {
          df <- df[order(df$date_modified, decreasing = TRUE),]
          rownames(df) <- 1:nrow(df)
        }

        rvOther$dfDocs <- df
      })

      observeEvent(rvOther$refresh_dfProjLocations, handlerExpr = {

        rvOther$projLocCurrIdx <- NULL
        rvOther$dfProjLocations <- NULL

        q <- sprintf("SELECT * FROM tbl_project_locations WHERE id_project = '%s';",
                     idProject)
        dfProjLocs <- dbGetQuery(conn = dbCon, statement = q)

        if (length(dfProjLocs) == 0) {
          return(invisible(0))
        }
        if (nrow(dfProjLocs) == 0) {
          return(invisible(0))
        }

        rvOther$dfProjLocations <- dfProjLocs
        rvOther$projLocCurrIdx <- 1
      })

      observeEvent(rvOther$refresh_dfProjImpacts, handlerExpr = {

        rvOther$dfProjImpacts <- NULL

        q <- sprintf("SELECT * FROM tbl_project_impact_indicators WHERE id_project = '%s';",
                     idProject)
        dfImpInds <- dbGetQuery(conn = dbCon, statement = q)

        if (length(dfImpInds) == 0) { return(NULL) }
        if (nrow(dfImpInds) == 0) { return(NULL) }

        rvOther$dfProjImpacts <- dfImpInds

      })

      # inputs -----------------------------------------------------------------
      
      observeEvent(input$dtoProjWrkflws_cell_clicked, handlerExpr = {
        
        # validate(need(rvOther$dfProjWorkflows, message = FALSE))
        # validate(need(rvOther$vnmsDtoProjWrkflws, message = FALSE))
        
        res <- input$dtoProjWrkflws_cell_clicked
        if (length(res) == 0) { return() }

        idxRow <- res$row
        idxCol <- res$col +1 # +1 because column 1 has value = 0
        val <- res$value

        # If the user didn't click one of the buttons, simply ignore the click.
        if (!(rvOther$vnmsDtoProjWrkflws[idxCol] %in% c("btn_view")) | 
            (length(val) == 0)) {
          return(NULL)
        }

        # If a 'View' button were clicked, render the policyState module from
        # where the agent will be able to interact with the chosen policy.
        if (rvOther$vnmsDtoProjWrkflws[idxCol] == "btn_view") {
          
          rvOther$nonce <- rvOther$nonce + 1
          
          modSrvrs$mWrkflwState <- workflowStateServer(
            id = sprintf("mWrkflwState%d", rvOther$nonce),
            idProject = idProject,
            dbCon = dbCon, 
            loginInfoUsr = loginInfoUsr,
            idWorkflow = rvOther$dfProjWorkflows$id[idxRow],
            hederaClient = hederaClient)
          
          rvOther$uiMode <- 2
          return(NULL)

        }

      })

      observeEvent(input$dtoDocs_cell_clicked, handlerExpr = {

        validate(need(rvOther$dfDocs, message = FALSE))
        validate(need(rvOther$vnmsDtoDocs, message = FALSE))

        res <- input$dtoDocs_cell_clicked
        if (length(res) == 0) { return() }

        #idxRow <- res$row
        idxCol <- res$col + 1 # +1 because column 1 has value = 0
        val <- res$value

        # If the user didn't click one of the view, edit, or review buttons/links,
        # simply ignore the click.
        if (!(rvOther$vnmsDtoDocs[idxCol] %in% c(
          "btn_view", "btn_action", "btn_view_history",
          "latest_version_timestamp", "latest_review_timestamp")) |
          (length(val) == 0)) {
          return(NULL)
        }
        if (is.na(val)) { return(NULL) }
        if (nchar(val) == 0) { return(NULL) }

        # If the user clicked a 'View' button, show the document in a modal.
        if (rvOther$vnmsDtoDocs[idxCol] %in% c("btn_view", "latest_version_timestamp", "latest_review_timestamp")) {

          showModal(loadingDocumentModal())

          # Determine the corresponding row idx in rvOther$dfDocs.
          if (rvOther$documentGridView == "ungrouped") {
            oidx <- getDocumentIdFromButtonHtml(htmlTxt = val, buttonType = "view")
          } else {
            oidx <- getDocumentIdFromLinkHtml(htmlTxt = val, linkType = "view")
          }
          oidx <- as.integer(oidx)
          idxRow <- which(rvOther$dfDocs$oidx == oidx)
        
          docModal <- viewDocumentModal3(
            idDoc = rvOther$dfDocs$id[idxRow], 
            dbCon = dbCon, 
            contentOnly = FALSE)

          # docModal <- viewDocumentModal2(
          #     ipfsUrlDoc = ipfsUrlDoc, 
          #     idMsgSch = rvOther$dfDocs$id_msg_schema[idxRow],
          #     idDocGuardian = rvOther$dfDocs$id_g[idxRow], 
          #     tagBlockPost = rvOther$dfDocs$step_workflow[idxRow], 
          #     idMsgPolicy = rvOther$dfDocs$id_msg_policy[idxRow], 
          #     dbCon = dbCon)
          
          removeModal()

          showModal(docModal)

          return(NULL)
        }

        # # If the user clicked an 'Edit' button, open the document in the schema
        # # editor.
        # if (length(grep(pattern = 'id="abEdit[[:digit:]]{1,}"', x = val)) == 1) {
        #
        #   # Determine the corresponding row idx in rvOther$dfDocs.
        #   oidx <- getDocumentIdFromButtonHtml(htmlTxt = val, buttonType = "edit")
        #   oidx <- as.integer(oidx)
        #   idxRow <- which(rvOther$dfDocs$oidx == oidx)
        #
        #   # Bump up nonce.
        #   rvOther$nonce <- rvOther$nonce +1
        #
        #   # Retrieve the contents of the draft document to be edited.
        #   lsPreset <- retrieveDraftDoc(
        #     idEntity = idProject,
        #     docId = rvOther$dfDocs$id[idxRow])
        #
        #   # Serve the module corresponding to this document's block in its policy.
        #   {
        #     # Get metadata about the policy to which this document belongs.
        #     q <- sprintf("SELECT * FROM tbl_policies WHERE id_message = '%s';",
        #                  rvOther$dfDocs$id_msg_policy[idxRow])
        #     rvOther$dfWorkflowDets <- dbGetQuery(conn = dbCon, statement = q)
        #     
        #     # Get the name of the handler for the policy to which the schema belongs.
        #     nmPolHndlr <- rvOther$dfWorkflowDets$handler_r
        #     
        #     # Get the name of the module that must be used for this schema.
        #     dfPolBlocks <- do.call(what = sprintf("getWorkflowStepMap.%s", nmPolHndlr), 
        #                            args = list())
        #     nmModule <- dfPolBlocks$nm_module[
        #       which(dfPolBlocks$step_workflow == rvOther$dfDocs$step_workflow[idxRow])]
        #     
        #     # # Render the schema module's UI.
        #     # rvOther$nmSchemaModUI <- sprintf("%sUI", nmModule)
        #     # rvOther$schemaUIargs <- NULL
        #
        #     # Call the schema module's server.
        #     modSrvrs$mWrkflwStep <- schemaServer(
        #       id = sprintf("mSchema%d", rvOther$nonce), 
        #       nmSchemaModSrvr = sprintf("%sServer", nmModule), 
        #       nmSchemaModUI = sprintf("%sUI", nmModule),
        #       schemaUIargs = NULL,
        #       dbCon = dbCon, 
        #       loginInfoUsr = loginInfoUsr, 
        #       idMsgSchema = rvOther$dfDocs$id_msg_schema[idxRow], 
        #       idMsgPolicy = rvOther$dfDocs$id_msg_policy[idxRow], 
        #       idEntity = loginInfoUsr$id_agent,
        #       idDoc = rvOther$dfDocs$id[idxRow], 
        #       tagBlockPost = rvOther$dfDocs$step_workflow[idxRow],
        #       lsPreset = lsPreset)
        #
        #     rvOther$uiMode <- 4
        #     return(NULL)
        #   }
        # }

        # If the user clicked a 'Review' button, serve the review module for
        # the schema.
        if (length(grep(pattern = 'id="abReview[[:digit:]]{1,}"', x = val)) == 1) {

          # Determine the corresponding row idx in rvOther$dfDocs.
          oidx <- getDocumentIdFromButtonHtml(htmlTxt = val, buttonType = "review")
          oidx <- as.integer(oidx)
          idxRow <- which(rvOther$dfDocs$oidx == oidx)
          
          # Link the current agent as REVIEWER to the project and workflow of the 
          # chosen document.
          bAssocSucc <- tryCatch({
            
            linkAgentXproject(
              idAgent = loginInfoUsr$id_agent, 
              idProject = idProject, 
              idWorkflow = rvOther$dfDocs$id_workflow[idxRow], 
              role = "REVIEWER", 
              dbCon = dbCon)
             
            TRUE
             
          }, error = function(e) {
            warning(as.character(e))
            FALSE
          })
          
          if (!bAssocSucc) {
            showModal(
              modalDialog(
                title = "Error", 
                "An error occurred while trying to register you as a reviewer of this document. Please try again later.", 
                footer = NULL, 
                size = "s", 
                easyClose = TRUE))
            return(NULL)
          }

          # Now serve the actual review module.
          modSrvrs$mReview <- reviewServer(
            id = sprintf("mReview%s", rvOther$nonce), 
            dbCon = dbCon, 
            loginInfoUsr = loginInfoUsr, 
            idDocOrig = rvOther$dfDocs$id[idxRow],
            hederaClient = hederaClient) 
          
          rvOther$uiMode <- 3

          return(NULL)
        }

      })
      
      observeEvent(input$dtoProjVICs_cell_clicked, handlerExpr = {
        
        validate(need(rvOther$dfProjVICs, message = FALSE))
        validate(need(rvOther$vnmsDtoProjVICs, message = FALSE))
        
        res <- input$dtoProjVICs_cell_clicked
        if (length(res) == 0) { return() }
        
        #idxRow <- res$row
        idxCol <- res$col + 1 # +1 because column 1 has value = 0
        val <- res$value
        
        # If the user didn't click one of the links or buttons simply ignore the 
        # click.
        if (!(names(rvOther$vnmsDtoProjVICs)[idxCol] %in% c("link_view", "btn_trustchain")) | 
            (length(val) == 0)) {
          return(NULL)
        }
        if (is.na(val)) { return(NULL) }
        if (nchar(val) == 0) { return(NULL) }
        
        # If the user clicked a 'View' link, show the document in a modal.
        if (names(rvOther$vnmsDtoProjVICs)[idxCol] == "link_view") {
          
          showModal(loadingDocumentModal())
          
          # Determine the corresponding row idx in rvOther$dfProjVICs.
          oidx <- getDocumentIdFromLinkHtml(htmlTxt = val, linkType = "view")
          oidx <- as.integer(oidx)
          idxRow <- which(rvOther$dfProjVICs$oidx == oidx)

          docModal <- viewDocumentModal3(
            idDoc = rvOther$dfProjVICs$id_doc[idxRow], 
            dbCon = dbCon, 
            contentOnly = FALSE)
          
          removeModal()
          
          showModal(docModal)
          
          return(NULL)
        }
        
        # If the user clicked a Trustchain button, show the trust chain.
        {
          rvOther$nonce2 <- rvOther$nonce2 +1
          modNm <- sprintf("vtcTrustChain%d", rvOther$nonce2)
          
          # Determine the corresponding row idx in rvOther$dfProjVICs.
          oidx <- getDocumentIdFromButtonHtml(htmlTxt = val, buttonType = "trustchain")
          oidx <- as.integer(oidx)
          idxRow <- which(rvOther$dfProjVICs$oidx == oidx)
          
          showModal(
            modalDialog(
              viewTrustChain2UI(id = ns(modNm)),
              footer = NULL,
              size = "l",
              easyClose = T))
          
          viewTrustChain2Server(
            id = modNm,
            certId = rvOther$dfProjVICs$id_cert[idxRow],
            msgId = rvOther$dfProjVICs$id_message_h[idxRow], 
            ipfsUri = rvOther$dfProjVICs$uri_ipfs[idxRow], 
            dbCon = dbCon,
            loginInfoUsr = loginInfoUsr)

        }

      })

      observeEvent(input$abSwitchDocumentGridView, handlerExpr = {
        rvOther$documentGridView <- setdiff(c("grouped", "ungrouped"), rvOther$documentGridView)
      })

      observeEvent(input$cbiShowAdditionalInfo, handlerExpr = {
        rvOther$dtoDocsShowAdditionalInfo <- input$cbiShowAdditionalInfo
        shinyjs::runjs(sprintf("toggleAdditionalInfoVisibility('Additional info','%s',%s);",
                      ns("dtoDocs"),
                      ifelse(input$cbiShowAdditionalInfo, 'true', 'false')))
      })

      observeEvent(input$abProjLocNext, handlerExpr = {

        if (length(rvOther$projLocCurrIdx) == 0) {
          return(invisible(NULL))
        }

        nxt <- rvOther$projLocCurrIdx + 1
        if (nxt > nrow(rvOther$dfProjLocations)) {
          return(invisible(NULL))
        }

        rvOther$projLocCurrIdx <- nxt

      })

      observeEvent(input$abProjLocPrev, handlerExpr = {

        if (length(rvOther$projLocCurrIdx) == 0) {
          return(invisible(NULL))
        }

        prev <- rvOther$projLocCurrIdx - 1
        if (prev < 1) {
          return(invisible(NULL))
        }

        rvOther$projLocCurrIdx <- prev

      })

      # outputs ----------------------------------------------------------------

      output$toTtlProj <- renderText({
        rvOther$dfProject$title
      })

      output$toCreatedBy <- renderText({
        idAgentCr <- rvOther$dfProject$created_by
        q <- sprintf("SELECT did from tbl_link_agents_x_dids WHERE id_agent = '%s';",
                     idAgentCr)
        did <- dbGetQuery(conn = dbCon, statement = q)[["did"]]
        return(did)
      })
      
      output$toProjectHederaTopicId <- renderText({
        
        dbGetQuery(
          conn = dbCon, 
          statement = sprintf(
            "SELECT id_topic_h FROM tbl_link_entities_x_hedera_topics WHERE id_entity = '%s' AND label_topic_h = 'PRIMARY';",
            idProject))[["id_topic_h"]]
        
      })
      
      output$toDateCreated <- renderText({
        as.character(rvOther$dfProject$date_created)
      })

      output$dtoQuantifiedImpacts <- DT::renderDataTable({

        outputMsgs$quantifiedImpacts <- "Loading..."

        if (length(rvOther$dfProjImpacts) == 0) {
          outputMsgs$quantifiedImpacts <- "No impact indicators are available for this project yet."
          return(NULL)
        }
        if (nrow(rvOther$dfProjImpacts) == 0) {
          outputMsgs$quantifiedImpacts <- "No impact indicators are available for this project yet."
          return(NULL)
        }

        lsdfImpInds <- lapply(X = 1:nrow(rvOther$dfProjImpacts), FUN = function(r) {
          lsInd <- jsonlite::fromJSON(txt = rvOther$dfProjImpacts$obj_indicator[r])
          return(
            data.frame(label = rvOther$dfProjImpacts$label_indicator[r],
                       type = sprintf("%s, %s", lsInd$intentionality, lsInd$beneficial_or_adverse),
                       monitored = lsInd$monitored,
                       ex_ante_est = abs(lsInd$impact_estimation_ex_ante),
                       uom = lsInd$indicator_unit_of_measure,
                       approved = !is.na(rvOther$dfProjImpacts$approved[r])))
        })

        dfImpInds <- do.call("rbind", lsdfImpInds); rm(lsdfImpInds)

        dfImpInds <- gdata::rename.vars(
          data = dfImpInds,
          from = c("label", "type", "monitored", "ex_ante_est", "uom", "approved"),
          to = c("Label", "Type", "Monitored", "Ex Ante Estimation", "Unit of Measure", "Approved"),
          info = FALSE)

        outputMsgs$quantifiedImpacts <- NULL

        return(dfImpInds)
      },
      escape = F, rownames = FALSE, selection = c('none', 'single')[1],
      options = list(processing = FALSE, dom = "t", scrollX = TRUE))

      output$toQuantifiedImpacts <- renderText({
        outputMsgs$quantifiedImpacts
      })

      output$pltoProjectLocation <- leaflet::renderLeaflet({

        if (length(rvOther$projLocCurrIdx) == 0) {
          outputMsgs$projectLocation <- "No project locations are available for this project yet."
          return(NULL)
        }

        outputMsgs$projectLocation <- "Loading..."

        # Get the file from IPFS.
        {
          # TODO. Have a cache for this.
          
          ipfsUrl <- rvOther$dfProjLocations$url_ipfs_location[rvOther$projLocCurrIdx]

          fpDest <- tempfile()
          download.file(url = ipfsUrl, destfile = fpDest, mode = "wb")

          if (!file.exists(fpDest)) {
            outputMsgs$projectLocation <- "Failed to download file."
            return(NULL)
          }
        }
        
        # Unzip.
        {
          exDir <- paste0(fpDest, "_unzipped")
          if (!dir.exists(exDir)) { dir.create(exDir) }
          unzip(zipfile = fpDest, overwrite = TRUE, exdir = exDir)
          if (length(dir(exDir)) == 0) {
           outputMsgs$projectLocation <- "Failed to extract contents of downloaded file."
           return(NULL)
          }
          #exDir <- "data/"
          dfFiles <- data.frame(
            datapath = dir(exDir, full.names = TRUE),
            name_file = dir(exDir, full.names = FALSE),
            name_loc = NA_character_,
            ext = NA_character_)
        }

        # Plot.
        {
          lsEls <- strsplit(x = dfFiles$name_file, split = ".", fixed = TRUE)
          dfFiles$name_loc <- sapply(X = lsEls, FUN = function(x) {
            paste(x[-length(x)], collapse = "")
          })
          dfFiles$ext <- sapply(X = lsEls, FUN = function(x) {
            x[length(x)]
          })
          
          # Plot .shp or .kml files.
          {
            idx <- which(dfFiles$ext %in% c("shp", "kml"))
            if (length(idx) == 0) {
              outputMsgs$projectLocation <- "No .shp or .kml files found."
              return(NULL)
            }
            
            if (length(idx) > 1) {
              # Plot only the first one.
              idx <- idx[1]
            }

            sfProjLoc <- sf::st_read(dfFiles$datapath[idx])

            plt <- leaflet() %>%
              addProviderTiles(providers$Esri.WorldImagery) %>%
              addPolygons(data = sfProjLoc, color = "red", weight = 2, opacity = 1) #%>%
              # addLabelOnlyMarkers(data = sfProjLoc %>% distinct() %>% st_centroid(),
              #           label = ~MP_NAME,
              #           labelOptions = leaflet::labelOptions(noHide = TRUE, direction = "auto"))
          }
          
        }

        outputMsgs$projectLocation <- sprintf(
          "Approved: %s | Verified: %s",
          !is.na(rvOther$dfProjLocations$approved[rvOther$projLocCurrIdx]),
          !is.na(rvOther$dfProjLocations$verified[rvOther$projLocCurrIdx]))

        return(plt)

      })

      output$toProjectLocation <- renderText({
        outputMsgs$projectLocation
      })
      
      output$dtoProjVICs <- DT::renderDataTable({
        
        validate(need(rvOther$dfProjVICs, message = FALSE))
        
        df <- rvOther$dfProjVICs
        
        df$link_view <- makeLinkHtmlForDocumentsGrid(
          documentIds = df$oidx, 
          linkText = df$id_cert, 
          linkType = "view")
        
        df$btn_trustchain <- makeButtonHtmlForDocumentsGrid(
          documentIds = df$oidx, 
          buttonType = 'trustchain')
        
        vnms <- c(
          link_view = "Certificate ID", 
          date_issued = "Date Issued", 
          status = "Status", 
          btn_trustchain = "Trust Chain")
        
        rvOther$vnmsDtoProjVICs <- vnms
        
        df <- df[,names(vnms)]
        names(df) <- vnms

        return(df)
        
      }, 
      escape = F, 
      rownames = FALSE, selection = c('none', 'single')[1], 
      options = list(
        processing = FALSE, 
        dom = "tp", 
        scrollX = TRUE,
        pageLength = 10))
      
      
      output$toDtoProjVICs <- renderText({
        
        if (length(rvOther$dfProjVICs) > 0) {
          if (nrow(rvOther$dfProjVICs) > 0) { return(NULL) }
        }
        
        return("This project doesn't have any Verified Impact Certificates (VICs) yet.")
        
      })

      
      output$dtoProjWrkflws <- DT::renderDataTable({
        
        # validate(need(rvOther$dfProjWorkflows, message = FALSE))
        # if (nrow(rvOther$dfProjWorkflows) == 0) { return(NULL) }
        
        df <- rvOther$dfProjWorkflows
        
        if (rvOther$dfProject$created_by != loginInfoUsr$id_agent) {
          if (length(df) == 0) { return(NULL) }
          if (nrow(df) == 0) { return(NULL) }
        }

        # Add 'View' buttons.
        df$btn_view <- sprintf('<button id="abView%d" type="button" class="btn btn-default action-button">View</button>',
                               1:nrow(df))
        
        # Update rvOther$wrkflwTitles_dtoProjWrkflws.
        rvOther$wrkflwTitles_dtoProjWrkflws <- df$title
        
        # Subset to vars to be displayed; rename.
        {
          vnms <- c(title = "Title", tag_version = "Version", btn_view = "View")
          vnms <- vnms[intersect(names(vnms), names(df))]
          df <- df[,names(vnms)]
          rvOther$vnmsDtoProjWrkflws <- names(vnms)
          names(df) <- vnms
        }

        # Done.
        return(df)

      },
      escape = F, rownames = FALSE, selection = c('none', 'single')[1],
      options = list(
        processing = FALSE,
        dom = "t",
        scrollX = TRUE))
      
      output$toDtoProjWrkflws <- renderText({
        
        # if (length(rvOther$dfProjWorkflows) > 0) {
        #   if (nrow(rvOther$dfProjWorkflows) > 0) { return(NULL) }
        # }
        # 
        # if (rvOther$dfProject$created_by == loginInfoUsr$id_agent) {
        #   return("This project doesn't have any workflows associated with it yet. Click on one of the 'Add' buttons below to get started.")
        # } else {
        #   return("This project doesn't have any workflows associated with it yet.")
        # }
      })


      output$dtoProjAgents <- DT::renderDataTable({

        if (length(rvOther$dfProjAgents) == 0) {
          return(NULL)
        }
        if (nrow(rvOther$dfProjAgents) == 0) {
          return(NULL)
        }

        df <- rvOther$dfProjAgents
        
        # Translate id_workflow to human-friendly info.
        {
          df$title_workflow <- NA_character_
          df$version_workflow <- NA_character_
          idxx <- match(x = df$id_workflow, table = rvOther$dfProjWorkflows$id)
          df$title_workflow <- rvOther$dfProjWorkflows$title[idxx]
          df$version_workflow <- rvOther$dfProjWorkflows$tag_version[idxx]
        }
      
        # Retrieve agent DIDs.
        {
          dfDids <- dbGetQuery(
            conn = dbCon, 
            statement = sprintf(
              "SELECT id_agent, did FROM tbl_link_agents_x_dids WHERE id_agent IN(%s);",
              paste(sprintf("'%s'", df$id_agent), sep = "", collapse = ",")))
          
          df$did <- NA_character_
          idxx <- match(x = df$id_agent, table = dfDids$id_agent)
          df$did[which(!is.na(idxx))] <- dfDids$did[idxx[!is.na(idxx)]]
        }
        
        df <- df[c("title_workflow", "version_workflow", "did", "role")]
        df <- df[order(df$title_workflow, df$version_workflow, df$did, df$role),]
        rownames(df) <- 1:nrow(df)
        names(df) <- c("Workflow", "Version", "DID", "Role")
        
        return(df)
      },
      escape = F, rownames = FALSE, selection = c('none', 'single')[1],
      options = list(
        processing = FALSE,
        dom = "tp",
        scrollX = TRUE,
        pageLength = 5))

      output$toDtoProjAgents <- renderText({

        if (length(rvOther$dfProjAgents) > 0) {
          if (nrow(rvOther$dfProjAgents) > 0) { return(NULL) }
        }

        return("This project doesn't have any agents associated with it yet.")

      })


      output$dtoDocs <- DT::renderDataTable({

        validate(need(rvOther$dfDocs, message = FALSE))
        if (nrow(rvOther$dfDocs) == 0) { return(NULL) }

        funcNm <- ifelse(rvOther$documentGridView == "ungrouped",
                         buildUngroupedDocumentsGridViewDf,
                         buildGroupedDocumentsGridViewDf)

        res <- do.call(
          what = funcNm, 
          args = list(df = rvOther$dfDocs, 
                      dbCon = dbCon, 
                      loginInfoUsr = loginInfoUsr, 
                      idProject = idProject))

        if (length(res) == 0) { return(NULL) }

        rvOther$vnmsDtoDocs <- names(res$vnms)
        names(res$vnms) <- NULL
        names(res$df) <- res$vnms
        return(res$df)

      },
      escape = F, rownames = FALSE, selection = c('none', 'single')[1],
      options = list(
        processing = FALSE,
        dom = "tp",
        scrollX = TRUE,
        pageLength = 10,
        columnDefs = list(
          list(visible = isolate(rvOther$dtoDocsShowAdditionalInfo), targets = 1))))

      output$toDtoDocs <- renderText({

        if (length(rvOther$dfDocs) > 0) {
          if (nrow(rvOther$dfDocs) > 0) { return(NULL) }
        }

        return("This project doesn't have any documents associated with it yet.")

      })
      
      # # (transient - output$dtoTknIssuances lives inside a modal)
      # output$dtoTknIssuances <- DT::renderDataTable({
      #   validate(need(rvOther$dfTknIssuances, message = FALSE))
      #   
      #   df <- rvOther$dfTknIssuances
      #   
      #   # Add "view trust chain" buttons.
      #   df$btn_view_tc <- sprintf('<button id="abViewTC%d" type="button" class="btn btn-default action-button">View Trust Chain</button>',
      #                             1:nrow(df))
      #   
      #   # Subset to variables of interest and rename.
      #   {
      #     vnms <- c(issuance = 'Issuance', timestamp = "Timestamp", 
      #               serials = "Serials", n_deleted = "N Deleted", 
      #               btn_view_tc = "View Trust Chain")
      #     df <- df[,names(vnms)]
      #     rvOther$vnmsDtoTknIssuances <- names(df)
      #     names(df) <- vnms
      #   }
      #   
      #   return(df)
      # }, 
      # escape = F, rownames = FALSE, selection = c('none', 'single')[1], 
      # options = list(processing = FALSE, dom = "t", 
      #                scrollX = TRUE, scrollY = TRUE))

      # return logic -----------------------------------------------------------

      rvOther$initialise <- Sys.time()

      return(rvToReturn)
    })
}




processIdentifyingContentForDisplay <- function(df) {

  # Parse 'identifying_content' into meaningful variables, where possible.
  df$monitoring_period <- NA_character_
  df$additional_info <- NA_character_

  idxx <- which(!is.na(df$identifying_content))
  for (r in idxx) {

    monPer <- NA_character_
    x <- jsonlite::fromJSON(df$identifying_content[r])
    if ('date_start_monper' %in% names(x)) {
      monPer <- x$date_start_monper
      if ('date_end_monper' %in% names(x)) {
        monPer <- sprintf("%s - %s", monPer, x$date_end_monper)
      }
      df$monitoring_period[r] <- monPer
    }

    vnms <- setdiff(names(x), c("id_project", "date_start_monper", "date_end_monper"))
    if (length(vnms) == 0) { next }

    x <- x[vnms]
    df$additional_info[r] <- paste(
      sapply(X = vnms, FUN = function(vnm) {
        sprintf("%s: %s",
                stringr::str_to_title(
                  gsub(pattern = "_", replacement = " ", x = vnm, fixed = TRUE)),
                x[[vnm]])
      }), sep = "", collapse =  " | ")
  }

  return(df)
}
