
# Workflow state module for viewing workflow state (by anyone).
# Shows which steps of the PRIMARY_AGENT role have already been completed and 
# which not.
# Tells the caller which module(s) to call next and with which arguments.
#
workflowStateUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    ##useShinyjs(),
    
    uiOutput(outputId = ns("uioMain")))
}


workflowStateServer <- function(id, 
                                dbCon, 
                                loginInfoUsr,
                                idWorkflow,
                                idProject = NULL,
                                hederaClient) {
  
  moduleServer(
    id = id, 
    function(input, output, session) {
      
      ns <- session$ns
      
      rvToReturn <- reactiveValues()
      rvToReturn$goToModule <- NULL 
      rvToReturn$argsForGoToMod <- NULL
      # rvToReturn$save <- NULL
      # rvToReturn$submit <- NULL
      # rvToReturn$allResultsGood <- FALSE
      # rvToReturn$results <- reactiveValues()
      
      # resultsGood <- reactiveValues()
      
      # outputMsgs <- reactiveValues()
      
      rvOther <- reactiveValues()
      rvOther$agentRole <- NULL
      rvOther$dfWorkflowDets <- NULL
      rvOther$dfWorkflowState <- NULL
      rvOther$vnmsDtoState <- NULL
      rvOther$refresh <- NULL
      rvOther$uiMode <- c("showState", "doSchemaWork")[1]
      rvOther$nonce <- 0
      #rvOther$nmSchemaModUI <- NULL
      #rvOther$schemaUIargs <- NULL
      
      # ui rendering -----------------------------------------------------------
      
      observeEvent(rvOther$uiMode, handlerExpr = {
        
        if (rvOther$uiMode == "showState") {
          
          #rvOther$nmSchemaModUI <- NULL
          #rvOther$schemaUIargs <- NULL
          modSrvrs$mWrkflwStep <- NULL
          rvOther$nonce <- rvOther$nonce + 1
          rvOther$refresh <- Sys.time()
          
          output$uioMain <- renderUI({
            return(
              tagList(
                wellPanel(
                  h3(rvOther$dfWorkflowDets$title),
                  h4(sprintf("Version %s", 
                             rvOther$dfWorkflowDets$tag_version)),
                  br(),
                  #h4("Workflow State"),
                  DT::dataTableOutput(outputId = ns("dtoState")))))
            
          }) 
          
          return(NULL)
        }
        
        if (rvOther$uiMode == "doSchemaWork") {
          
          output$uioMain <- renderUI({
            return(
              tagList(
                fluidRow(
                  column(
                    width = 12,
                    wellPanel(
                      h3(rvOther$dfWorkflowDets$title),
                      h4(sprintf("Version %s", 
                                 rvOther$dfWorkflowDets$tag_version))))),
                # schemaUI(id = ns(sprintf("mSchema%d", rvOther$nonce)),
                #          nmSchemaModUI = rvOther$nmSchemaModUI, 
                #          schemaUIargs = rvOther$schemaUIargs))
                fluidRow(
                  #column(width = 2),
                  column(
                    width = 12,
                    schemaUI(id = ns(sprintf("mSchema%d", rvOther$nonce)))) #,
                  #column(width = 2)
                )))
          })
          
          return(NULL)
        }
        
        warning("Invalid value encountered for rvOther$uiMode.")   
        
      })
      
      # modSrvrs$mWrkflwStep$done
      observe({
        validate(need(modSrvrs$mWrkflwStep, message = FALSE))
        if (length(modSrvrs$mWrkflwStep$done) == 0) { return(invisible(NULL)) }
        if (modSrvrs$mWrkflwStep$done) {
          rvOther$uiMode <- "showState"
        }
      })
      
      # module servers ---------------------------------------------------------
      
      modSrvrs <- reactiveValues()
      modSrvrs$mWrkflwStep <- NULL
      
      # inputs -----------------------------------------------------------------
      
      observeEvent(rvOther$refresh, handlerExpr = {
        
        # Retrieve details of the workflow.
        {
          q <- sprintf("SELECT * FROM tbl_workflows WHERE id = '%s';", idWorkflow)
          rvOther$dfWorkflowDets <- dbGetQuery(conn = dbCon, statement = q)
        }
        
        subjWrkflw <- rvOther$dfWorkflowDets$subject
        
        # Get the agent's role in the workflow.
        {
          agentRole <- NULL
          
          if (subjWrkflw == 'AGENT') {
            # TODO. This is dangerous.
            agentRole <- "PRIMARY_AGENT"
            
          } else {
            
            q <- sprintf(
              "SELECT * FROM tbl_link_projects_x_agents WHERE id_project = '%s' AND id_workflow = '%s' AND id_agent = '%s';",
              idProject, idWorkflow, loginInfoUsr$id_agent)
            df <- dbGetQuery(conn = dbCon, statement = q)
            if (nrow(df) == 1) {
              if (!is.na(df$role)) {
                if (nchar(gsub(pattern = "[[:blank:]]", 
                               replacement = "", 
                               x = df$role)) > 0) {
                  agentRole <- df$role
                }
              }
            }
          }
          
          if (length(agentRole) == 0) {
            agentRole <- "VIEWER"
          }
          
          rvOther$agentRole <- agentRole
        }
        
        # Update the current state of the workflow.
        {
          if (!exists(sprintf("getState.%s", rvOther$dfWorkflowDets$handler_r))) {
            showModal(
              modalDialog(
                title = "Coming soon!", 
                sprintf("Function 'getState.%s' has not been defined yet.", 
                        rvOther$dfWorkflowDets$handler_r), 
                footer = NULL, 
                size = "m", 
                easyClose = TRUE))
            return(NULL)
          }
          
          # Determine who are the primary agents of this workflow.
          {
            if (subjWrkflw == "AGENT") {
              idPrimAgents <- loginInfoUsr$id_agent   
            } else {
              q <- sprintf(
                "SELECT id_agent FROM tbl_link_projects_x_agents WHERE id_project = '%s' AND id_workflow = '%s' AND role = 'PRIMARY_AGENT';",
                idProject, idWorkflow)
              idPrimAgents <- dbGetQuery(conn = dbCon, statement = q)
            }
          }
          
          # Determine the ID of the entity subject of this workflow.
          idEntity <- ifelse(subjWrkflw == "AGENT", 
                             loginInfoUsr$id_agent,
                             idProject)
          
          # Retrieve the current state of the policy.
          df <- do.call(
            what = sprintf("getState.%s", rvOther$dfWorkflowDets$handler_r), 
            args = list(idPrimAgents = idPrimAgents,
                        idEntity = idEntity,
                        dbCon = dbCon,
                        idWorkflow = idWorkflow))
          
          df$oidx <- 1:nrow(df)
          rvOther$dfWorkflowState <- df
        }
        
      })
      
      observeEvent(input$dtoState_cell_clicked, handlerExpr = {
        
        validate(need(rvOther$dfWorkflowState, message = FALSE))
        
        res <- input$dtoState_cell_clicked
        if (length(res) == 0) { return() }
        
        idxRow <- res$row
        idxCol <- res$col
        val <- res$value
        
        # If the user didn't click one of the action buttons, simply ignore the
        # click.
        if ((rvOther$vnmsDtoState[idxCol+1] != "action") | (length(val) == 0)) {
          return(NULL)
        }
        if (is.na(val)) { return(NULL) }
        if (nchar(val) == 0) { return(NULL) }
        
        # Retrieve the 'oidx' of the row in question, because idxRow here 
        # is NOT the same as idx in rvOther$dfWorkflowState, because we removed
        # rows from rvOther$dfWorkflowState before we rendered it to the DTO.
        # Use the value of oidx to determine the correct value for idxRow.
        {
          oidx <- strsplit(x = val, split = " ", fixed = TRUE)[[1]][2]
          oidx <- gsub(pattern = "[^[:digit:]]", replacement = "", x = oidx)
          oidx <- as.integer(oidx)
          idxRow <- which(rvOther$dfWorkflowState$oidx == oidx); rm(oidx)
        }
        
        # If the workflow step in question is a requestVcDocumentBlock, serve
        # the relevant schema module for the step
        if (rvOther$dfWorkflowState$type_step[idxRow] == "requestVcDocumentBlock") {
          
          # Initialise some variables for use below.
          subjWrkflw <- rvOther$dfWorkflowDets$subject
          
          docId <- NULL
          lsPreset <- NULL
          
          # If a 'Continue' button were clicked, populate lsPreset with the 
          # draft document that the current agent (or one of their fellow 
          # primary agents) has saved.
          if (length(grep(pattern = "abDtoContn", x = val, fixed = TRUE)) > 0) {
            
            # Retrieve the metadata of all the draft documents for this 
            # particular workflow step.
            q <- sprintf("SELECT * FROM tbl_document_metadata WHERE id_entity = '%s' AND status = 'DRAFT' AND id_workflow = '%s' AND id_schema = '%s' AND step_workflow = '%s';", 
                         ifelse(subjWrkflw == "AGENT", loginInfoUsr$id_agent, idProject),
                         rvOther$dfWorkflowState$id_workflow[idxRow],
                         rvOther$dfWorkflowState$id_schema[idxRow],
                         rvOther$dfWorkflowState$step_workflow[idxRow])
            dfDrafts <- dbGetQuery(conn = dbCon, statement = q)
            
            if (nrow(dfDrafts) > 0) {
              
              # Order by date_modified descending.
              dfDrafts <- dfDrafts[order(dfDrafts$date_modified, decreasing = TRUE),]
              rownames(dfDrafts) <- 1:nrow(dfDrafts)
              
              # If there are multiple drafts, retrieve the most recent one 
              # created by the current agent.
              if (nrow(dfDrafts) > 1) {
                idxx <- which(dfDrafts$did_author == loginInfoUsr$did)
                if (length(idxx) > 0) {
                  docId <- dfDrafts$id[idxx[1]]
                }
              }
              
              # If none of the existing drafts were created by the current 
              # agent, return the draft most recently modified by one of the 
              # agent's fellow primary agents.
              if (length(docId) == 0) {
                docId <- dfDrafts$id[1]
              }
              
              # Retrieve the actual contents of the draft.
              if (length(docId) == 1) {
                lsPreset <- retrieveDraftDoc(
                  idEntity = ifelse(subjWrkflw == "AGENT", loginInfoUsr$id_agent, idProject),
                  docId = docId)
              }
            } 
          }
          
          # If a 'Retry' button were clicked, retrieve the most-recently rejected
          # document for this workflow step as lsPreset.
          if (length(grep(pattern = "abDtoRetry", x = val, fixed = TRUE)) > 0) {
            
            q <- sprintf(
              "SELECT * FROM tbl_document_metadata WHERE id_entity = '%s' AND id_workflow = '%s' AND step_workflow = '%s' AND outcome = 'REJECTED';",
              ifelse(subjWrkflw == 'AGENT', loginInfoUsr$id_agent, idProject),
              idWorkflow,
              rvOther$dfWorkflowState$step_workflow[idxRow])
            dfRejs <- dbGetQuery(conn = dbCon, statement = q)
            
            if (nrow(dfRejs) == 0) {
              message("WARNING: Failed to retrieve rejected document.")
            } else {
              
              # Select the most recent one.
              dfRejs <- dfRejs[order(dfRejs$date_modified, decreasing = TRUE),]
              dfRejs <- dfRejs[1,]
              
              # Get the actual contents of the document.
              lsPreset <- getPubDoc(
                contentOnly = TRUE, 
                docId = dfRejs$id, 
                dbCon = dbCon,
                decrypt = TRUE)
              
              # Extract if nested one level too deep.
              if (length(lsPreset) == 1) {
                if (is.list(lsPreset[[1]])) {
                  if (length(lsPreset[[1]]) > 1 & length(names(lsPreset[[1]])) > 1) {
                    lsPreset <- lsPreset[[1]]
                  }
                }
              }
              
            }
          }
          
          # If a 'Repeat' button were clicked, retrieve the most-recently approved
          # document for this block as lsPreset.
          if (length(grep(pattern = "abDtoRepeat", x = val, fixed = TRUE)) > 0) { 
            
            q <- sprintf(
              "SELECT * FROM tbl_document_metadata WHERE id_entity = '%s' AND id_workflow = '%s' AND step_workflow = '%s' AND outcome IN('APPROVED','REVIEWED');",
              ifelse(subjWrkflw == 'AGENT', loginInfoUsr$id_agent, idProject),
              idWorkflow,
              rvOther$dfWorkflowState$step_workflow[idxRow])
            dfApprs <- dbGetQuery(conn = dbCon, statement = q)
            
            if (nrow(dfApprs) == 0) {
              message("WARNING: Failed to retrieve most recently approved document for this policy and block.")
            } else {
              
              # Select the most recent one.
              dfApprs <- dfApprs[order(dfApprs$date_modified, decreasing = TRUE),]
              dfApprs <- dfApprs[1,]
              
              # Get the actual contents of the document.
              lsPreset <- getPubDoc(
                contentOnly = TRUE, 
                docId = dfApprs$id, 
                decrypt = TRUE,
                dbCon = dbCon)
              
              # Extract if nested one level too deep.
              if (length(lsPreset) == 1) {
                if (is.list(lsPreset[[1]])) {
                  if (length(lsPreset[[1]]) > 1 & length(names(lsPreset[[1]])) > 1) {
                    lsPreset <- lsPreset[[1]]
                  }
                }
              }
              
            }
          }
          
          # Serve the module corresponding to this block in the policy.
          {
            nmSchModUI <- sprintf("%sUI", rvOther$dfWorkflowState$nm_module[idxRow])
            nmSchModSrvr <- sprintf("%sServer", rvOther$dfWorkflowState$nm_module[idxRow])
            
            #rvOther$nmSchemaModUI <- nmSchModUI
            
            modSrvrs$mWrkflwStep <- schemaServer(
              id = sprintf("mSchema%d", rvOther$nonce), 
              nmSchemaModSrvr = nmSchModSrvr, 
              nmSchemaModUI = nmSchModUI,
              schemaUIargs = list(lsPreset = lsPreset),
              dbCon = dbCon, 
              loginInfoUsr = loginInfoUsr, 
              hederaClient = hederaClient,
              idSchema = rvOther$dfWorkflowState$id_schema[idxRow], 
              idWorkflow = idWorkflow, 
              idEntity = ifelse(subjWrkflw == 'AGENT', loginInfoUsr$id_agent, idProject),
              idDoc = docId, 
              stepWorkflow = rvOther$dfWorkflowState$step_workflow[idxRow], 
              lsPreset = lsPreset)

            rvOther$uiMode <- c("showState", "doSchemaWork")[2]
            return(NULL)
          }
        }
        
      })
      
      # outputs ----------------------------------------------------------------
      
      output$dtoState <- DT::renderDataTable({
        
        validate(need(rvOther$dfWorkflowState, message = FALSE))
        validate(need(rvOther$agentRole, message = FALSE))
        
        # Note: The state of a workflow is defined solely by the state of the 
        # blocks that require action from the primary agent. We are therefore
        # ignoring all other blocks here.
        
        dfState <- rvOther$dfWorkflowState
        idxx <- grep(pattern = "PRIMARY_AGENT", 
                     x = dfState$permissions_step, 
                     fixed = TRUE)
        dfState <- dfState[idxx,]
        
        if (nrow(dfState) == 0) {
          warning("Something went wrong. Workflow has no steps for role 'PRIMARY_AGENT'.")
          return(NULL)
        }
        
        if (rvOther$agentRole != "PRIMARY_AGENT") {
          
          # Only the primary agent of the workflow should be able to take action
          # from within this module; everyone else is just a viewer, so return
          # without adding buttons.
          
          vnms <- c("title_step", "descr_step", "state")
          dfState <- dfState[vnms]
          names(dfState) <- c("Step", "Description", "State")
          rvOther$vnmsDtoState <- vnms
          return(dfState)
          
        }
        
        # Add buttons according to each step's state.
        {
          # Note: We use 'oidx' here to label the buttons, instead of just
          # 1:nrow(dfState), because dfState here no longer contains all the
          # rows in rvOther$dfWorkflowState, so when we check for cell clicks on
          # this DTO, we'll need oidx instead of just idxRow.
          
          dfState$action <- NA_character_
          
          idxx <- which(dfState$state == "START")
          dfState$action[idxx] <- sprintf(
            '<button id="abDtoStart%d" type="button" class="btn btn-default action-button">Start</button>',
            dfState$oidx[idxx])
          
          idxx <- which(dfState$state == "IN_PROGRESS")
          dfState$action[idxx] <- sprintf(
            '<button id="abDtoContn%d" type="button" class="btn btn-default action-button">Continue</button>',
            dfState$oidx[idxx])
          
          idxx <- which(dfState$state == "RETRY")
          dfState$action[idxx] <- sprintf(
            '<button id="abDtoRetry%d" type="button" class="btn btn-default action-button">Retry</button>',
            dfState$oidx[idxx])
          
          idxx <- which(dfState$state == "READY_FOR_REPEAT")
          dfState$action[idxx] <- sprintf(
            '<button id="abDtoRepeat%d" type="button" class="btn btn-default action-button">Repeat</button>',
            dfState$oidx[idxx])
        }
        
        vnms <- c("title_step", "descr_step", "state", "action")
        dfState <- dfState[vnms]
        names(dfState) <- c("Step", "Description", "State", "Action")
        rvOther$vnmsDtoState <- vnms
        return(dfState)
        
      }, 
      escape = F, rownames = FALSE, selection = c('none', 'single')[1], 
      options = list(processing = FALSE, dom = "t", ordering = FALSE))
      
      # return logic -----------------------------------------------------------
      
      # observe({
      #   rvToReturn$allResultsGood <- all(unlist(reactiveValuesToList(resultsGood)))
      # })
      
      rvOther$refresh <- Sys.time()
      
      return(rvToReturn)
      
    })
}

