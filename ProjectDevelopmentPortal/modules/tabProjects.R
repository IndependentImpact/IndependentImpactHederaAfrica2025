
# UI
tabProjectsUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    #useShinyjs(),
    
    uiOutput(outputId = ns("uioMain")),
    
    textOutput(outputId = ns("toDebug")))
}

# Server
tabProjectsServer <- function(id, 
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
      outputMsgs$ttlProj <- ""
      outputMsgs$toMyProjs <- ""
      outputMsgs$debug <- ""
      
      rvOther <- reactiveValues()
      rvOther$uioMain <- NULL
      rvOther$dfMyProjs <- NULL
      rvOther$dfOtherProjs <- NULL
      rvOther$refresh <- NULL
      rvOther$vnmsDtoMyProjs <- NULL
      rvOther$vnmsDtoOtherProjs <- NULL
      rvOther$nonce <- 0
      
      modSrvrs <- reactiveValues()
      modSrvrs$mViewProj <- NULL
      
      # ui rendering -----------------------------------------------------------
      
      output$uioMain <- renderUI({
        
        validate(need(rvOther$uioMain, message = FALSE))
        
        tryCatch({
          
          if (rvOther$uioMain == "allProjs") {
            return(
              tagList(
                wellPanel(
                  h3("My Projects"),
                  textOutput(outputId = ns("toMyProjs")),
                  br(),
                  DT::dataTableOutput(ns("dtoMyProjs")),
                  br(),
                  actionButton(ns("abNewProject"), 
                               label = "Create new project")),
                wellPanel(
                  h3("Other Projects"),
                  textOutput(outputId = ns("toOtherProjs")),
                  br(),
                  DT::dataTableOutput(ns("dtoOtherProjs")))))
          }
          
          if (rvOther$uioMain == "singleProj") {
            return(
              tagList(
                projectUI(id = ns(sprintf("mViewProj%s", 
                                          isolate(rvOther$nonce)))),
                br(),
                fluidRow(
                  column(
                    width = 2,
                    actionButton(inputId = ns("abBack"), 
                                 label = "<< Back to projects", 
                                 width = "100%")),
                  column(width = 10)),
                br()))
          }
          
          warning(sprintf("Unknown value encountered for rvOther$uioMain."))     
          
        }, error = function(e) {
          outputMsgs$debug <- paste(e, collapse = "\n")
        })
        
      })
      
      # inputs -----------------------------------------------------------------
      
      observeEvent(rvOther$refresh, handlerExpr = {
        
        rvOther$dfMyProjs <- NULL
        rvOther$dfOtherProjs <- NULL
        
        tryCatch({
          
          outputMsgs$toMyProjs <- "You have no current projects. Click on the 'Create new project' button below to start a project."
          
          # Get all projects.
          dfProjs <- dbFetch(
            dbSendQuery(conn = dbCon, 
                        statement = "SELECT * FROM tbl_projects;"))
          
          if (nrow(dfProjs) == 0) {
            return(NULL)
          }
          
          # Add username of project 'creator'.
          {
            q <- sprintf("SELECT * FROM tbl_link_agents_x_email_addresses WHERE id_agent IN(%s);",
                         paste(sprintf("'%s'", unique(dfProjs$created_by)), 
                               sep = "", collapse = ","))
            dfEms <- dbGetQuery(conn = dbCon, statement = q)
            #alert(paste("tabProjects: ", q))
            dfProjs$created_by_email <- NA_character_
            idxx <- match(x = dfProjs$created_by, table = dfEms$id_agent)
            dfProjs$created_by_email[which(!is.na(idxx))] <- dfEms$email_address[idxx[!is.na(idxx)]]
          }
          
          # Order by date_created, descending.
          dfProjs <- dfProjs[order(dfProjs$date_created, decreasing = TRUE),]
          rownames(dfProjs) <- 1:nrow(dfProjs)
          
          rvOther$dfMyProjs <- dfProjs[which(dfProjs$created_by == loginInfoUsr$id_agent),]
          rvOther$dfOtherProjs <- dfProjs[which(dfProjs$created_by != loginInfoUsr$id_agent),]
          
          nMyProjs <- nrow(rvOther$dfMyProjs)
          if (nMyProjs == 0) {
            outputMsgs$toMyProjs <- "You have no current projects. Click on the 'Create new project' button below to start a project."
          } else {
            outputMsgs$toMyProjs <- ""
          }
          
        }, error = function(e) {
          outputMsgs$debug <- paste(e, collapse = "\n")
        })
        
      })
      
      observeEvent(input$abNewProject, handlerExpr = {
        
        tryCatch({
          
          showModal(
            modalDialog(
              GtextInput(id = ns("tiTtlProj"), 
                         title = "Project Title", 
                         helpTxt = "Provide a title for your project.", 
                         hL = 5),
              title = "New Project",
              footer = tagList(
                actionButton(inputId = ns("abCreate"), 
                             label = "OK"),
                modalButton("Cancel")),
              size = c("m", "s", "l", "xl")[1],
              easyClose = FALSE,
              fade = FALSE))
          
        }, error = function(e) {
          outputMsgs$debug <- paste(e, collapse = "\n")
        })
        
      })
      
      observeEvent(input$tiTtlProj, handlerExpr = {
        
        tryCatch({
          
          nChar <- nchar(input$tiTtlProj)
          outputMsgs$ttlProj <- sprintf("%d chars. remaining", 100 - nChar)
          
        }, error = function(e) {
          outputMsgs$debug <- paste(e, collapse = "\n")
        })
        
      })
      
      observeEvent(input$abCreate, handlerExpr = {
        
        validate(need(input$tiTtlProj, message = FALSE))
        
        tryCatch({
          
          projTitle <- input$tiTtlProj
          
          # Check length requirements.
          if (!isValidInput.text(x = projTitle, 
                                 bRequired = TRUE, 
                                 nCharMin = 3, 
                                 nCharMax = 100)) {
            outputMsgs$ttlProj <- "Project title must be between 3 and 100 characters in length."
            return()
          }
          
          # Check that no other project has already been registered with this title.
          res <- dbFetch(
            dbSendQuery(conn = dbCon, 
                        statement = sprintf("SELECT id FROM tbl_projects WHERE title='%s';",
                                            projTitle)))
          if (length(res$id) > 0) {
            outputMsgs$ttlProj <- "Project title not available. Please choose a different title."
            return(NULL)
          }
          
          # Remove the "create new project" modal.
          removeModal()
          
          # Create the project.
          currStep <- 0
          nTotSteps <- 5+1
          withProgress(min = 0, max = nTotSteps, value = 0, expr = {
            
            # 1. Add project to database.
            {
              currStep <- currStep + 1
              incProgress(
                amount = 0,
                message = "Adding project to database...", 
                detail = sprintf("Step %s/%s", currStep, nTotSteps))
              
              dfAdd <- data.frame(
                id = NA_character_, 
                title = projTitle, 
                created_by = loginInfoUsr$id_agent,
                date_created = lubridate::now(tzone = "UTC"))
              
              idProj <- addToDb(dfAdd = dfAdd, 
                                tblNm = "tbl_projects", 
                                vnmsChckEx = "title", 
                                dbCon = dbCon, 
                                returnIds = TRUE)
              
              incProgress(amount = 1)
            }
            
            # 2. Create and store Hedera topic admin and submission key pairs for 
            # this project.
            {
              currStep <- currStep + 1
              incProgress(
                amount = 0,
                message = "Creating Hedera topic...",
                detail = sprintf("Step %s/%s", currStep, nTotSteps))
              
              # Admin key pair.
              {
                topicAdminPrivKey <- hederaClient$operator_private_key
                topicAdminPubKey <- topicAdminPrivKey$public_key()
                
                topicAdminKeyPairId <- dbGetQuery(
                  conn = dbCon, 
                  statement = sprintf(
                    "SELECT id FROM tbl_key_pairs WHERE public_key = '%s';",
                    topicAdminPubKey$to_string()))[["id"]]
              }
              
              # Submit key pair.
              {
                topicSubmitPrivKey <- hiero$PrivateKey$generate_ed25519()
                topicSubmitPubKey <- topicSubmitPrivKey$public_key()
                
                # Add this key pair to tbl_key_pairs.
                {
                  encrPrivKey <- cyphr::encrypt_string(
                    string = topicSubmitPrivKey$to_string_der(), 
                    key = cyphr::keypair_openssl(
                      pub = iwefdj$KEYPTH_CYPHR, 
                      key = iwefdj$KEYPTH_CYPHR, 
                      envelope = TRUE,
                      password = iwefdj$PW_CYPHR))
                  encrPrivKey <- openssl::base64_encode(encrPrivKey)
                  
                  # decrRaw <- openssl::base64_decode(encrPrivKey)
                  # decrChar <- cyphr::decrypt_string(
                  #   data = decrRaw, 
                  #   key = cyphr::keypair_openssl(
                  #     pub = iwefdj$KEYPTH_CYPHR, 
                  #     key = iwefdj$KEYPTH_CYPHR, 
                  #     envelope = TRUE,
                  #     password = iwefdj$PW_CYPHR))
                  
                  topicSubmitKeyPairId <- addToDb(
                    dfAdd = data.frame(
                      public_key = topicSubmitPubKey$to_string(),
                      private_key_encr = encrPrivKey), 
                    tblNm = "tbl_key_pairs", 
                    vnmsChckEx = "public_key", 
                    dbCon = dbCon, 
                    calcIds = TRUE, 
                    returnIds = TRUE)
                }
              }
              
              incProgress(amount = 1)
            }
            
            # 3. Create a Hedera topic for this project and add to db.
            {
              currStep <- currStep + 1
              incProgress(
                amount = 0,
                message = "Creating Hedera topic...",
                detail = sprintf("Step %s/%s", currStep, nTotSteps))

              tx <- hiero$TopicCreateTransaction(
                memo = idProj,
                admin_key = topicAdminPubKey,
                submit_key = topicSubmitPubKey)
              tx <- tx$freeze_with(hederaClient)
              tx <- tx$sign(topicAdminPrivKey)
              tx <- tx$sign(topicSubmitPrivKey)
              #tx <- tx$sign(hederaClient$operator_private_key) # Is the same as topicAdminPrivKey.
              resp <- tx$execute(hederaClient)
              projTopicId <- paste(
                resp$topicId$shard, 
                resp$topicId$realm, 
                resp$topicId$num, 
                sep = ".", collapse = "")
              
              # Link the two key pairs to this topic in the db.
              {
                addToDb(
                  dfAdd = data.frame(
                    id_topic_h = projTopicId, 
                    id_key_pair = c(
                      topicAdminKeyPairId,
                      topicSubmitKeyPairId),
                    label_key_pair = c("ADMIN", "SUBMIT")), 
                  tblNm = "tbl_link_hedera_topics_x_key_pairs", 
                  vnmsChckEx = c("id_topic_h", "id_key_pair", "label_key_pair"), 
                  dbCon = dbCon, 
                  calcIds = FALSE)
              }
              
              # Link the project to this topic in the db.
              {
                addToDb(
                  dfAdd = data.frame(
                    id_topic_h = projTopicId, 
                    id_entity = idProj, 
                    label_topic_h = "PRIMARY"), 
                  tblNm = "tbl_link_entities_x_hedera_topics", 
                  vnmsChckEx = c("id_topic_h", "id_entity", "label_topic_h"), 
                  dbCon = dbCon, 
                  calcIds = FALSE)
              }
              
              incProgress(amount = 1)
            }

            # 4. Set this agent as the PRIMARY_AGENT on all the II activity
            # workflows.
            {
              currStep <- currStep + 1
              incProgress(
                amount = 0,
                message = "Linking workflows...",
                detail = sprintf("Step %s/%s", currStep, nTotSteps))
              
              # Get all activity workflows.
              dfWrkflws <- dbGetQuery(
                conn = dbCon, 
                statement = "SELECT * FROM tbl_workflows WHERE subject = 'ACTIVITY';")
              
              # Subset to the latest version of each.
              dfWrkflws$version <- gsub(
                pattern = ".", 
                replacement = "", 
                x = dfWrkflws$tag_version, 
                fixed = TRUE)
              dfWrkflws$version <- as.numeric(dfWrkflws$version)
              dfWrkflws <- dfWrkflws[order(dfWrkflws$version, decreasing = TRUE),]
              dfWrkflws <- dfWrkflws[which(!duplicated(dfWrkflws$title)),]
              
              # Link agent.
              dfAdd <- data.frame(
                id_project = idProj,
                id_workflow = dfWrkflws$id,
                id_agent = loginInfoUsr$id_agent,
                role = "PRIMARY_AGENT")
              addToDb(
                dfAdd = dfAdd, 
                tblNm = "tbl_link_projects_x_agents", 
                vnmsChckEx = c("id_project", "id_workflow", "id_agent"), 
                dbCon = dbCon, 
                calcIds = FALSE)
              
              incProgress(amount = 1)
            }
            
            # 5. Add to rvOther$dfMyProjs
            {
              currStep <- currStep + 1
              incProgress(
                amount = 0,
                message = "Finishing up...",
                detail = sprintf("Step %s/%s", currStep, nTotSteps))
              
              dfAdd <- dbFetch(
                dbSendQuery(
                  conn = dbCon, 
                  statement = sprintf("SELECT * FROM tbl_projects WHERE id = '%s';",
                                      idProj)))
              
              outputMsgs$toMyProjs <- ""
              
              incProgress(amount = 1)
              
              # Add it to the top of rvOther$dfMyProjs (because rvOther$dfMyProjs is
              # ordered according to date_created, descending).
              rvOther$dfMyProjs <- rbind(dfAdd, rvOther$dfMyProjs)
            }
            
          })
          
        }, error = function(e) {
          outputMsgs$debug <- paste(e, collapse = "\n")
        })
        
      })
      
      observeEvent(input$dtoMyProjs_cell_clicked, handlerExpr = {
        
        validate(need(rvOther$dfMyProjs, message = FALSE))
        
        tryCatch({
          
          res <- input$dtoMyProjs_cell_clicked
          if (length(res) == 0) { return() }
          
          idxCol <- res$col
          val <- res$value
          idxRow <- res$row
          
          # Ignore the click if it weren't in the 'btn_view' column.
          if (rvOther$vnmsDtoMyProjs[idxCol+1] != "btn_view") {
            return(NULL)
          }
          
          rvOther$nonce <- rvOther$nonce + 1
          
          # Render the 'project' module for this project.
          modSrvrs$mViewProj <- projectServer(
            id = sprintf("mViewProj%s", rvOther$nonce),
            idProject = rvOther$dfMyProjs$id[idxRow], 
            dbCon = dbCon, 
            loginInfoUsr = loginInfoUsr, 
            hederaClient = hederaClient)
          rvOther$uioMain <- "singleProj"
          
        }, error = function(e) {
          outputMsgs$debug <- paste(e, collapse = "\n")
        })
        
      })
      
      observeEvent(input$dtoOtherProjs_cell_clicked, handlerExpr = {
        
        validate(need(rvOther$dfOtherProjs, message = FALSE))
        
        tryCatch({
          
          res <- input$dtoOtherProjs_cell_clicked
          if (length(res) == 0) { return() }
          
          idxCol <- res$col
          val <- res$value
          idxRow <- res$row
          
          # Ignore the click if it weren't in the 'btn_view' column.
          if (rvOther$vnmsDtoOtherProjs[idxCol+1] != "btn_view") {
            return(NULL)
          }
          
          rvOther$nonce <- rvOther$nonce + 1
          
          # Render the 'project' module for this project.
          modSrvrs$mViewProj <- projectServer(
            id = sprintf("mViewProj%s", rvOther$nonce),
            idProject = rvOther$dfOtherProjs$id[idxRow], 
            dbCon = dbCon, 
            loginInfoUsr = loginInfoUsr,
            hederaClient = hederaClient)
          rvOther$uioMain <- "singleProj"
          
        }, error = function(e) {
          outputMsgs$debug <- paste(e, collapse = "\n")
        })
        
      })
      
      observeEvent(input$abBack, handlerExpr = {
        
        tryCatch({
          
          modSrvrs$mViewProj <- NULL
          rvOther$nonce <- rvOther$nonce + 1
          rvOther$uioMain <- "allProjs"
          
        }, error = function(e) {
          outputMsgs$debug <- paste(e, collapse = "\n")
        })
        
      })
      
      # outputs ----------------------------------------------------------------
      
      output$toDebug <- renderText({
        outputMsgs$debug
      })
      
      output$toMyProjs <- renderText({
        outputMsgs$toMyProjs
      })
      
      output$toOtherProjs <- renderText({
        # nProjs <- 0
        # if (length(rvOther$dfOtherProjs) > 0) {
        #   nProjs <- nrow(rvOther$dfOtherProjs)
        # }
        # return(sprintf("Total: %d", nProjs))
      })
      
      output$tiTtlProj_msg <- renderText({
        outputMsgs$ttlProj
      })
      
      output$dtoMyProjs <- DT::renderDataTable({
        
        tryCatch({
          
          if (length(rvOther$dfMyProjs) == 0) {
            return(NULL)
          }
          if (nrow(rvOther$dfMyProjs) == 0) {
            return(NULL)
          }
          
          df <- rvOther$dfMyProjs
          
          # Add 'View' buttons.
          {
            df$btn_view <- sprintf('<button id="abView%d" type="button" class="btn btn-default action-button">View</button>',
                                   1:nrow(df))
          }
          
          # Drop and rename some variables.
          {
            df[c("oidx", "id", "created_by", "un_creator", "created_by_email")] <- NULL
            rvOther$vnmsDtoMyProjs <- names(df)
            names(df) <- c("Title", "Created On", "View")
          }
          
          return(df)
          
        }, error = function(e) {
          outputMsgs$debug <- paste(e, collapse = "\n")
        })
        
      }, 
      escape = F, rownames = FALSE, 
      selection = c('none', 'single')[1], 
      options = list(
        processing = FALSE, 
        dom = "tp"))
      
      output$dtoOtherProjs <- DT::renderDataTable({
        
        tryCatch({
          
          if (length(rvOther$dfOtherProjs) == 0) {
            return(NULL)
          }
          if (nrow(rvOther$dfOtherProjs) == 0) {
            return(NULL)
          }
          
          df <- rvOther$dfOtherProjs
          
          # Add 'View' buttons.
          {
            df$btn_view <- sprintf('<button id="abView%d" type="button" class="btn btn-default action-button">View</button>',
                                   1:nrow(df))
          }
          
          # Rename and drop some variables.
          df[c("oidx", "id", "created_by")] <- NULL
          rvOther$vnmsDtoOtherProjs <- names(df)
          names(df) <- c("Title", "Created On", "Created By", "View")
          
          return(df)
          
        }, error = function(e) {
          outputMsgs$debug <- paste(e, collapse = "\n")
        })
        
      }, 
      escape = F, rownames = FALSE, 
      selection = c('none', 'single')[1], 
      options = list(
        processing = FALSE, 
        dom = "tp"))
      
      output$toNoPolHndlr <- renderText({
        "No handler specified for chosen workflow. Please contact Independent Impact admin."
      })
      
      # ------------------------------------------------------------------------
      
      rvOther$uioMain <- "allProjs"
      
      rvOther$refresh <- Sys.time()
      
    })
  
}
