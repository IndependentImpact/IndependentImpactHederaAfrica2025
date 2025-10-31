# This module embodies the Independent Impact Agent Application Workflow.

agentUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    ##useShinyjs(),
    
    uiOutput(outputId = ns("uioMain")))
}

agentServer <- function(id, 
                        dbCon, 
                        hederaClient,
                        loginInfoUsr) {
  
  moduleServer(
    id = id, 
    function(input, output, session) {
      
      ns <- session$ns
      
      rvToReturn <- reactiveValues()
      rvToReturn$goToModule <- NULL
      rvToReturn$argsForGoToModule <- NULL
      
      rvOther <- reactiveValues()
      rvOther$dfAgent <- NULL
      rvOther$dfMapAllCredWflwsToTkns <- NULL # TODO. Remove.
      rvOther$dfAllCredWflws <- NULL
      rvOther$dfAgentCredWflws <- NULL
      rvOther$dfAgentCreds <- NULL
      rvOther$dfRep <- NULL
      rvOther$dfDocs <- NULL
      rvOther$refresh <- NULL
      rvOther$refresh_dfAgent <- NULL
      rvOther$refresh_dfMapAllCredWflwsToTkns <- NULL
      rvOther$refresh_dfAllCredWflws <- NULL
      rvOther$refresh_dfAgentCredWflws <- NULL
      rvOther$refresh_dfAgentCreds <- NULL
      rvOther$refresh_dfDocs <- NULL
      rvOther$dfChosenCred <- NULL
      rvOther$dfChosenWflw <- NULL
      rvOther$vnmsDtoAgentCredWflws <- NULL
      rvOther$vnmsDtoCredWflwDocs <- NULL
      #rvOther$nmSchemaModUI <- NULL
      rvOther$schemaUIargs <- NULL
      rvOther$dfWflwDets <- NULL
      rvOther$nonce <- 0
      rvOther$nonce2 <- 0
      rvOther$uiMode <- NULL
      
      # module servers ---------------------------------------------------------
      
      modSrvrs <- reactiveValues()
      modSrvrs$mWflwState <- NULL
      modSrvrs$mChooseCred <- NULL
      modSrvrs$mChooseWflw <- NULL
      modSrvrs$mChooseWflwV <- NULL
      modSrvrs$mWflwStep <- NULL
      
      # modSrvrs$mWflwStep$done
      observe({
        validate(need(modSrvrs$mWflwStep, message = FALSE))
        if (length(modSrvrs$mWflwStep$done) > 0) {
          if (modSrvrs$mWflwStep$done) {
            modSrvrs$mWflwStep <- NULL
            rvOther$nonce <- rvOther$nonce + 1
            rvOther$refresh_dfDocs <- Sys.time()
            rvOther$refresh_dfAgentCreds <- Sys.time()
            rvOther$uiMode <- 1
          }
        }
      })
      
      # ui rendering -----------------------------------------------------------
      
      output$uioMain <- renderUI({
        
        validate(need(rvOther$uiMode, message = FALSE))
        
        if (rvOther$uiMode == 1) {
          
          modSrvrs$mWflwState <- NULL
          modSrvrs$mWflwStep <- NULL
          
          return(
            tagList(
              
              fluidRow(
                
                column(
                  width = 4,
                  GtextOutput(id = ns("toEmailAddr"), 
                              title = "Email Address", 
                              hL = 4),
                  GtextOutput(id = ns("toHederaAccId"), 
                              title = "Hedera Account ID", 
                              hL = 4)),
                
                column(
                  width = 4,
                  GtextOutput(id = ns("toDateReg"), 
                              title = "Date Registered",
                              hL = 4),
                  uiOutput(outputId = ns("uioDID"))),
                
                column(
                  width = 4,
                  wellPanel(
                    h4("Reputation"),
                    DT::dataTableOutput(ns("dtoAgentRep")),
                    br()))),
              
              # Credentials
              wellPanel(
                h4("Issued Credentials"),
                DT::dataTableOutput(ns("dtoAgentCreds")),
                textOutput(outputId = ns("toDtoAgentCreds")),
                br()),
              
              wellPanel(
                h4("Associated Credential Workflows"),
                DT::dataTableOutput(outputId = ns("dtoAgentCredWflws")) #,
                # br(),
                # actionButton(inputId = ns("abObtainCreds"), 
                #              label = "Obtain Credentials")
              ),
              
              wellPanel(
                h4("Credential Workflow Documents"),
                DT::dataTableOutput(outputId = ns("dtoCredWflwDocs")))))
        }
        
        if (rvOther$uiMode == 2) {
          
          modSrvrs$mWflwStep <- NULL
          
          return(
            tagList(
              fluidRow(
                column(
                  width = 1),
                column(
                  width = 10,
                  workflowStateUI(
                    id = ns(sprintf("mWflwState%d", 
                                    isolate(rvOther$nonce)))),
                  br(),
                  br(),
                  br(),
                  actionButton(inputId = ns("abBack"),
                               label = "<< Back to all credentials")),
                column(
                  width = 1))))
        }
        
        if (rvOther$uiMode == 3) {
          
          return(
            tagList(
              wellPanel(
                h3(rvOther$dfWflwDets$name),
                h4(sprintf("Version %s", 
                           rvOther$dfWflwDets$version))),
              # schemaUI(id = ns(sprintf("mSchema%d", rvOther$nonce)),
              #          nmSchemaModUI = rvOther$nmSchemaModUI, 
              #          schemaUIargs = rvOther$schemaUIargs),
              schemaUI(id = ns(sprintf("mSchema%d", rvOther$nonce))),
              br(),
              br(),
              br(),
              actionButton(inputId = ns("abBack"),
                           label = "<< Back to all credentials")))
          
        }
        
        warning("Unknown value encountered for rvOther$uiMode.")
      })
      
      observeEvent(input$abBack, handlerExpr = {
        modSrvrs$mWflwState <- NULL
        modSrvrs$mWflwStep <- NULL
        rvOther$nonce <- rvOther$nonce + 1
        rvOther$refresh_dfDocs <- Sys.time()
        rvOther$refresh_dfAgentCreds <- Sys.time()
        rvOther$uiMode <- 1
      })
      
      # inputs -----------------------------------------------------------------
      
      observeEvent(rvOther$refresh, handlerExpr = {
        rvOther$refresh_dfAgent <- Sys.time()
        rvOther$refresh_dfRep <- Sys.time()
        rvOther$refresh_dfMapAllCredWflwsToTkns <- Sys.time()
        rvOther$refresh_dfAllCredWflws <- Sys.time()
        rvOther$refresh_dfAllCreds <- Sys.time()
        rvOther$refresh_dfAgentCredWflws <- Sys.time()
        rvOther$refresh_dfAgentCreds <- Sys.time()
        rvOther$refresh_dfDocs <- Sys.time()
      })
      
      observeEvent(rvOther$refresh_dfAgent, handlerExpr = {
        
        q <- sprintf("SELECT * FROM tbl_agents WHERE id = '%s';", 
                     loginInfoUsr$id_agent)
        dfAgent <- dbGetQuery(conn = dbCon, statement = q)
        dfAgent$oidx <- NULL
        
        q <- sprintf("SELECT * FROM tbl_link_agents_x_dids WHERE id_agent = '%s';", 
                     loginInfoUsr$id_agent)
        dfDids <- dbGetQuery(conn = dbCon, statement = q)
        dfDids$oidx <- NULL
        
        q <- sprintf("SELECT * FROM tbl_link_agents_x_hedera_accounts WHERE id_agent = '%s';", 
                     loginInfoUsr$id_agent)
        dfHaccs <- dbGetQuery(conn = dbCon, statement = q)
        dfHaccs$oidx <- NULL
        
        q <- sprintf("SELECT * FROM tbl_link_agents_x_email_addresses WHERE id_agent = '%s';",
                     loginInfoUsr$id_agent)
        dfEmAddr <- dbGetQuery(conn = dbCon, statement = q)
        dfEmAddr$oidx <- NULL
        
        df <- merge.data.frame(
          y = dfAgent, 
          x = dfDids, 
          by.y = "id", 
          by.x = "id_agent", 
          all = TRUE); rm(dfAgent, dfDids)
        
        df <- merge.data.frame(
          x = df, 
          y = dfHaccs, 
          by = "id_agent", 
          all = TRUE); rm(dfHaccs)
        
        df <- merge.data.frame(
          x = df, 
          y = dfEmAddr, 
          by = "id_agent", 
          all = TRUE)
        
        rvOther$dfAgent <- df
      })

      observeEvent(rvOther$refresh_dfRep, handlerExpr = {
        rvOther$dfRep <- dbGetQuery(
          conn = dbCon, 
          statement = sprintf(
            "SELECT * FROM tbl_agent_reputation WHERE id_agent = '%s';",
            loginInfoUsr$id_agent))
      })
      
      observeEvent(rvOther$refresh_dfAllCredWflws, handlerExpr = {
        
        rvOther$dfAllCredWflws <- NULL
        
        q <- sprintf("SELECT * FROM tbl_workflows WHERE subject = 'AGENT';")
        dfCredWflws <- dbGetQuery(conn = dbCon, statement = q)
        dfCredWflws$oidx <- NULL
        
        rvOther$dfAllCredWflws <- dfCredWflws
        
      })
      
      observeEvent(rvOther$refresh_dfAgentCredWflws, handlerExpr = {
        
        rvOther$dfAgentCredWflws <- NULL
        
        validate(need(rvOther$dfAllCredWflws, message = FALSE))
        
        q <- sprintf(
          "SELECT DISTINCT(id_workflow) FROM tbl_document_metadata WHERE id_entity = '%s';",
          loginInfoUsr$id_agent)
        allWflwIdsWithDocs <- dbGetQuery(conn = dbCon, statement = q)[["id_workflow"]]
        credWflwIds <- intersect(rvOther$dfAllCredWflws$id, allWflwIdsWithDocs)
        if (length(credWflwIds) == 0) {
          # Automatically "associate" this agent with the latest version of our credential 
          # workflow.
          credWflwIds <- rvOther$dfAllCredWflws$id[
            which(rvOther$dfAllCredWflws$tag_version == orderVersionTags(
              x = rvOther$dfAllCredWflws$tag_version, 
              asc = FALSE)[1])]
        }
        
        rvOther$dfAgentCredWflws <- rvOther$dfAllCredWflws[
          which(rvOther$dfAllCredWflws$id %in% credWflwIds),]
      })
      
      observeEvent(rvOther$refresh_dfAgentCreds, handlerExpr = {
        
        rvOther$dfAgentCreds <- NULL
        
        # Retrieve all licenses of this agent.
        q <- sprintf("SELECT * FROM tbl_agent_licenses WHERE did_holder = '%s';", 
                     loginInfoUsr$did)
        dfLics <- dbGetQuery(conn = dbCon, statement = q)
        if (nrow(dfLics) == 0) { return(NULL) }
        
        # Retrieve the document IDs of those licenses.
        q <- sprintf(
          "SELECT id, id_message_h FROM tbl_document_metadata WHERE id_entity = '%s' AND id_message_h IN(%s);",
          loginInfoUsr$id_agent,
          paste(sprintf("'%s'", dfLics$id_message_h), sep = "", collapse = ","))
        dfDocMd <- dbGetQuery(conn = dbCon, statement = q)
        
        idxx <- match(x = dfLics$id_message_h, table = dfDocMd$id_message_h)
        dfLics$id_doc <- dfDocMd$id[idxx]
        
        rvOther$dfAgentCreds <- dfLics
        
      })
      
      observeEvent(rvOther$refresh_dfDocs, handlerExpr = {

        q <- sprintf(
          "SELECT * FROM tbl_document_metadata WHERE id_entity = '%s';", 
          loginInfoUsr$id_agent)
        df <- dbGetQuery(conn = dbCon, statement = q)
        if (nrow(df) > 0) {
          df <- df[order(df$date_modified, decreasing = TRUE),]
          rownames(df) <- 1:nrow(df)
        }
        
        rvOther$dfDocs <- df
        
      })

      observeEvent(input$abCancelChMdl, handlerExpr = {
        removeModal()
        modSrvrs$mChooseCred <- NULL
        modSrvrs$mChooseWflw <- NULL
        rvOther$dfChosenCred <- NULL
        rvOther$dfChosenWflw <- NULL
      })
      
      observeEvent(input$dtoAgentCredWflws_cell_clicked, handlerExpr = {
        
        res <- input$dtoAgentCredWflws_cell_clicked
        if (length(res) == 0) { return() }
        
        idxRow <- res$row
        idxCol <- res$col
        val <- res$value
        
        # If the user didn't click one of the view buttons, simply ignore the
        # click.
        if ((rvOther$vnmsDtoAgentCredWflws[idxCol+1] != "btn_view") | 
            (length(val) == 0)) {
          return(NULL)
        }
        
        rvOther$nonce <- rvOther$nonce + 1
        
        # Render the workflowState module from where the agent will be able to 
        # interact with the chosen workflow.
        {
          modSrvrs$mWflwState <- workflowStateServer(
            id = sprintf("mWflwState%d", rvOther$nonce),
            dbCon = dbCon, 
            loginInfoUsr = loginInfoUsr,
            idWorkflow = rvOther$dfAgentCredWflws$id[idxRow],
            idProject = NULL,
            hederaClient = hederaClient)
          
          rvOther$uiMode <- 2
        }
        
      })
      
      observeEvent(input$dtoCredWflwDocs_cell_clicked, handlerExpr = {
        
        res <- input$dtoCredWflwDocs_cell_clicked
        if (length(res) == 0) { return() }
        
        idxRow <- res$row
        idxCol <- res$col
        val <- res$value
        
        # If the user didn't click one of the buttons, simply ignore the click.
        if (!("btn" %in% rvOther$vnmsDtoCredWflwDocs)) {
          return(NULL)
        }
        if ((rvOther$vnmsDtoCredWflwDocs[idxCol+1] != "btn") | 
            (length(val) == 0)) {
          return(NULL)
        }
        
        # If the user clicked a 'View' button, show the document in a modal.
        if (length(grep(pattern = 'id="abView[[:digit:]]{1,}"', x = val)) == 1) {
          
          showModal(loadingDocumentModal())
          
          docModal <- viewDocumentModal3(
            idDoc = rvOther$dfDocs$id[idxRow], 
            dbCon = dbCon, 
            contentOnly = FALSE)
          
          removeModal()
          
          showModal(docModal)
          
          return(NULL)
        }
        
        # If the user clicked an 'Edit' button, render the document's schema
        # module.
        if (length(grep(pattern = 'id="abEdit[[:digit:]]{1,}"', x = val)) == 1) {
          
          rvOther$nonce <- rvOther$nonce +1
          
          # Retrieve the contents of the draft document to be edited.
          lsPreset <- retrieveDraftDoc(
            idEntity = loginInfoUsr$id_agent, 
            docId = rvOther$dfDocs$id[idxRow])
          
          # Serve the module corresponding to this document's step in its workflow.
          {
            # Get metadata about the workflow to which this document belongs.
            q <- sprintf("SELECT * FROM tbl_workflows WHERE id = '%s';",
                         rvOther$dfDocs$id_workflow[idxRow])
            rvOther$dfWflwDets <- dbGetQuery(conn = dbCon, statement = q)
            
            # Get the name of the handler for the workflow to which the schema belongs.
            nmWrkflwHndlr <- rvOther$dfWflwDets$handler_r
            
            # Get the name of the module that must be used for this schema.
            dfWflwSteps <- do.call(
              what = sprintf("getWorkflowStepMap.%s", nmWrkflwHndlr), 
              args = list())
            nmModule <- dfWflwSteps$nm_module[
              which(dfWflwSteps$step_workflow == rvOther$dfDocs$step_workflow[idxRow])]
            
            # message("nmWrkflwHndlr: ", nmWrkflwHndlr)
            # message("nmModule: ", nmModule)
            
            # Call the schema module's server.
            modSrvrs$mWflwStep <- schemaServer(
              id = sprintf("mSchema%d", rvOther$nonce), 
              nmSchemaModSrvr = sprintf("%sServer", nmModule), 
              nmSchemaModUI = sprintf("%sUI", nmModule),
              schemaUIargs = NULL,
              dbCon = dbCon, 
              loginInfoUsr = loginInfoUsr, 
              hederaClient = hederaClient,
              idSchema = rvOther$dfDocs$id_schema[idxRow], 
              idWorkflow = rvOther$dfDocs$id_workflow[idxRow], 
              idEntity = loginInfoUsr$id_agent,
              idDoc = rvOther$dfDocs$id[idxRow], 
              stepWorkflow = rvOther$dfDocs$step_workflow[idxRow],
              lsPreset = lsPreset)
            
            rvOther$uiMode <- 3
            return(NULL)
          }
        }
        
      })
      
      observeEvent(input$dtoAgentCreds_cell_clicked, handlerExpr = {
        
        res <- input$dtoAgentCreds_cell_clicked
        if (length(res) == 0) { return() }
        
        idxRow <- res$row
        idxCol <- res$col
        val <- res$value
        
        # If the user didn't click one of the buttons, simply ignore the click.
        if (!("btn_view" %in% rvOther$vnmsDtoAgentCreds)) {
          return(NULL)
        }
        if ((rvOther$vnmsDtoAgentCreds[idxCol+1] != "btn_view") | 
            (length(val) == 0)) {
          return(NULL)
        }
        
        # If the user clicked a 'View' button, show the document in a modal.
        if (length(grep(pattern = 'id="abView[[:digit:]]{1,}"', x = val)) == 1) {

          showModal(loadingDocumentModal())
          
          docModal <- viewDocumentModal3(
            idDoc = rvOther$dfAgentCreds$id_doc[idxRow], 
            dbCon = dbCon, 
            contentOnly = FALSE)
          
          removeModal()
          
          showModal(docModal)
          
        }
      })
      
      observeEvent(input$alDID, handlerExpr = {
        showModal(
          modalDialog(
            title = "Hedera DID", 
            rvOther$dfAgent$did, 
            footer = NULL, 
            easyClose = TRUE, 
            size = "s", 
            fade = FALSE))
      })
      
      # outputs ----------------------------------------------------------------
      
      output$toEmailAddr <- renderText({
        validate(need(rvOther$dfAgent, message = FALSE))
        rvOther$dfAgent$email_address
      })
      
      output$uioDID <- renderUI({
        
        validate(need(rvOther$dfAgent$did, message = FALSE))
        
        nCh <- nchar(rvOther$dfAgent$did)
        
        tagList(
          wellPanel(
            hx(x = "Hedera DID", lvl = 4),
            actionLink(
              inputId = ns("alDID"), 
              label = sprintf(
                "%s...%s",
                substr(rvOther$dfAgent$did, start = 12, stop = 25),
                substr(rvOther$dfAgent$did, start = nCh-10, stop = nCh)))))
      })
      
      output$toUserType <- renderText({
        validate(need(rvOther$dfAgent, message = FALSE))
        rvOther$dfAgent$type_user
      })
      
      output$toHederaAccId <- renderText({
        validate(need(rvOther$dfAgent, message = FALSE))
        rvOther$dfAgent$id_acc_h
      })
      
      output$toDateReg <- renderText({
        validate(need(rvOther$dfAgent, message = FALSE))
        as.character(rvOther$dfAgent$date_registered)
      })
      
      
      output$dtoAgentRep <- DT::renderDataTable({
        
        validate(need(rvOther$dfRep, message = FALSE))
        
        df <- rvOther$dfRep[c("domain", "score")]
        names(df) <- c("Domain", "Score")
        
        return(df)
      }, 
      escape = F, rownames = FALSE, selection = c('none', 'single')[1], 
      options = list(processing = FALSE, dom = "t", scrollX = FALSE))
      
      
      output$dtoAgentCreds <- DT::renderDataTable({
        
        validate(need(rvOther$dfAgentCreds, message = FALSE))
        
        # Get data.
        df <- rvOther$dfAgentCreds
        
        # Add 'View' buttons.
        df$btn_view <- sprintf(
          '<button id="abView%d" type="button" class="btn btn-default action-button">View</button>',
          1:nrow(df))
        
        # Subset to display vars.
        vnms <- c("id", "scope", "date_issued", "status", "btn_view")
        rvOther$vnmsDtoAgentCreds <- vnms
        df <- df[vnms]
        names(df) <- c("License No.", "Scope", "Date Issued", "Status", "Action")
        return(df)
        
      }, 
      escape = F, rownames = FALSE, selection = c('none', 'single')[1], 
      options = list(processing = FALSE, dom = "t", scrollX = TRUE))
      
      
      output$toDtoAgentCreds <- renderText({
        
        if (length(rvOther$dfAgentCreds) == 0) {
          return("You don't have any issued verifiable credentials yet. Use one of your associated credential workflows below to obtain credentials.")
        }
        
        return("")
        
      })
      
      
      output$dtoAgentCredWflws <- DT::renderDataTable({
        
        validate(need(rvOther$dfAgentCredWflws, message = FALSE))
        if (nrow(rvOther$dfAgentCredWflws) == 0) {
          return(NULL)
        }
        
        df <- rvOther$dfAgentCredWflws
        
        # Add 'View' buttons.
        df$btn_view <- sprintf(
          '<button id="abView%d" type="button" class="btn btn-default action-button">View</button>',
          1:nrow(df))
        
        vnms <- c('Title' = "title", 'Version' = "tag_version", 'View' = "btn_view")
        rvOther$vnmsDtoAgentCredWflws <- vnms
        df <- df[vnms]
        names(df) <- names(vnms)
        return(df)
        
      }, 
      escape = F, rownames = FALSE, selection = c('none', 'single')[1], 
      options = list(processing = FALSE, dom = "t", scrollX = TRUE))
      
      
      output$dtoCredWflwDocs <- DT::renderDataTable({
        
        validate(need(rvOther$dfDocs, message = FALSE))
        
        df <- rvOther$dfDocs
        if (nrow(df) == 0) { return(NULL) }
        
        # Get schema titles.
        {
          dfSchemas <- dbFetch(
            dbSendQuery(
              conn = dbCon, 
              statement = sprintf(
                "SELECT id, title, tag_version FROM tbl_schemas WHERE id IN(%s);",
                paste(sprintf("'%s'", unique(df$id_schema)), 
                      collapse = ","))))
          idxx <- match(x = df$id_schema, table = dfSchemas$id)
          df$title <- dfSchemas$title[idxx]
        }
        
        # Add buttons.
        {
          df$btn <- NA_character_
          
          idxx <- which(df$status == "DRAFT" & df$did_author == loginInfoUsr$did)
          df$btn[idxx] <- sprintf(
            '<button id="abEdit%d" type="button" class="btn btn-default action-button">Edit</button>',
            idxx)
          
          idxx <- which(df$status == "PUBLISHED")
          df$btn[idxx] <- sprintf(
            '<button id="abView%d" type="button" class="btn btn-default action-button">View</button>',
            idxx)
        }
        
        # Hide the 'Outcome' values for documents of which the current user is 
        # not the author. TODO: This is only a temporary measure until we've
        # implemented the functionality for users to accept, contest or reject
        # reviews.
        {
          idxx <- which(df$did_author != loginInfoUsr$did)
          df$outcome[idxx] <- NA_character_
        }
        
        vnms <- c("title", "date_created", "date_modified", "status", "outcome", "btn")
        df <- df[vnms]
        rvOther$vnmsDtoCredWflwDocs <- vnms
        names(df) <- c("Title", "Date created", "Date modified", 
                       "Status", "Outcome", "Action")
        return(df)
        
      }, 
      escape = F, rownames = FALSE, selection = c('none', 'single')[1], 
      options = list(processing = FALSE, dom = "tp", scrollX = TRUE, pageLength = 10))
      
      # return logic -----------------------------------------------------------
      
      rvOther$uiMode <- 1
      
      rvOther$refresh <- Sys.time()
      
      return(rvToReturn)
    })
}



