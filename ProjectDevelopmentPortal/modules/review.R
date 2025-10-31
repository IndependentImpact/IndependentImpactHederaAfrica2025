
reviewUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    ##useShinyjs(),
    
    fluidRow(
      column(width = 1),
    
      column(
        width = 10,
        wellPanel(
          h4("Details of document under review"),
          textOutput(outputId = ns("toOrigDocDate")),
          textOutput(outputId = ns("toOrigDocAuthor")),
          textOutput(outputId = ns("toOrigDocMsgId")),
          textOutput(outputId = ns("toOrigDocIpfsUri")))),
      
      column(width = 1)),
    
    fluidRow(
      column(width = 1),
      column(width = 10,
             uiOutput(outputId = ns("uioMain"))),
      column(width = 1)))
}

#'@param idDocOrig Character. Our db ID for the document to be reviewed.
#'
reviewServer <- function(id,  
                         dbCon,
                         loginInfoUsr,
                         idDocOrig,
                         hederaClient) {

  moduleServer(
    id = id, 
    function(input, output, session) {
      
      ns <- session$ns
      
      rvToReturn <- reactiveValues()
      rvToReturn$done <- FALSE
      #rvToReturn$allResultsGood <- FALSE
      #rvToReturn$results <- reactiveValues()
      
      #resultsGood <- reactiveValues()
      
      #outputMsgs <- reactiveValues()
      
      rvOther <- reactiveValues()
      rvOther$refresh <- NULL
      rvOther$dfMdOrig <- NULL
      #rvOther$nmSchemaModUI <- NULL
      rvOther$nonce <- 0
      
      # module servers ---------------------------------------------------------
      
      modSrvrs <- reactiveValues()
      modSrvrs$mSchema <- NULL
      
      # inputs -----------------------------------------------------------------
      
      # The once-off setup.
      observeEvent(rvOther$refresh, handlerExpr = {
        
        validate(need(rvOther$refresh, message = FALSE))

        # Fetch the metadata of the original document from the db.
        dfDocOrig <- dbGetQuery(
          conn = dbCon, 
          statement = sprintf(
            "SELECT * FROM tbl_document_metadata WHERE id = '%s';", idDocOrig))
        
        # Assign dfDocOrig to rvOther$dfMdOrig so that the UI output components
        # can start displaying things while we finish up a few things here.
        rvOther$dfMdOrig <- dfDocOrig

        # Find out which module and schema we need to serve to the user to 
        # review this document.
        {
          dfRevwStep <- getRevwStepInfo(
            stepWorkflow = dfDocOrig$step_workflow,
            idWorkflow = dfDocOrig$id_workflow,
            dbCon = dbCon)

          if (length(dfRevwStep) == 0) {
            showModal(
              modalDialog(
                title = "Failed to retrieve 'dfRevwStep'.", 
                footer = NULL, 
                size = "s", 
                easyClose = TRUE))
            return(NULL)
          }
          
          if (nrow(dfRevwStep) == 0) {
            showModal(
              modalDialog(
                title = "Failed to retrieve 'dfRevwStep'.", 
                footer = NULL, 
                size = "s", 
                easyClose = TRUE))
            return(NULL)
          }
        }
        
        # Check if there is a draft document that the current reviewer has
        # been working on.
        {
          # Scour for drafts by this agent.
          q <- sprintf("SELECT * FROM tbl_document_metadata WHERE id_entity = '%s' AND status = 'DRAFT' AND id_workflow = '%s' AND id_schema = '%s' AND step_workflow = '%s' AND did_author = '%s';", 
                       dfDocOrig$id_entity,
                       dfRevwStep$id_workflow,
                       dfRevwStep$id_schema,
                       dfRevwStep$step_workflow,
                       loginInfoUsr$did)
          dfDrafts <- dbGetQuery(conn = dbCon, statement = q)
          
          # If there are multiple drafts, get the ID of the most recent one 
          # created by the current agent.
          idDocDraft <- NULL
          if (nrow(dfDrafts) > 1) {
            dfDrafts <- dfDrafts[order(dfDrafts$date_modified, decreasing = TRUE),]
            rownames(dfDrafts) <- 1:nrow(dfDrafts)
            idDocDraft <- dfDrafts$id[1]
          } else {
            idDocDraft <- dfDrafts$id
          }
          
          # Retrieve the actual contents of the draft.
          lsPreset <- NULL
          if (length(idDocDraft) == 1) {
            lsPreset <- retrieveDraftDoc(
              idEntity = dfDocOrig$id_entity, 
              docId = idDocDraft)
          }
        }
        
        # Get the contents of the original document (i.e., the one about to be 
        # reviewed).
        {
          lsOriginal <- list()
          
          lsOriginal$cont <- getPubDoc(
            docId = dfDocOrig$id,
            dbCon = dbCon,
            contentOnly = TRUE,
            decrypt = TRUE)

          lsOriginal$md <- rvOther$dfMdOrig
        }
        
        # Now serve the module corresponding to this block in the policy.
        {
          nmSchModUI <- sprintf("%sUI", dfRevwStep$nm_module)
          nmSchModSrvr <- sprintf("%sServer", dfRevwStep$nm_module)
          
          #rvOther$nmSchemaModUI <- nmSchModUI

          modSrvrs$mSchema <- schemaServer(
            id = "mSchema", 
            nmSchemaModSrvr = nmSchModSrvr, 
            nmSchemaModUI = nmSchModUI,
            schemaUIargs = NULL,
            dbCon = dbCon, 
            loginInfoUsr = loginInfoUsr, 
            hederaClient = hederaClient,
            idEntity = dfDocOrig$id_entity, 
            idWorkflow = dfDocOrig$id_workflow, 
            idSchema = dfRevwStep$id_schema, 
            stepWorkflow = dfRevwStep$step_workflow, 
            idDoc = idDocDraft, 
            lsPreset = lsPreset, 
            lsOriginal = lsOriginal, 
            bReturnRes = TRUE)

          return(NULL)
        }
        
      })
      
      observeEvent(modSrvrs$mSchema$done, handlerExpr = {
        
        validate(need(modSrvrs$mSchema$done, message = FALSE))
        
        if (!modSrvrs$mSchema$done) { return(NULL) }
        
        if (length(modSrvrs$mSchema$idDoc) != 1) { 
          rvToReturn$done <- TRUE
          return(NULL) 
        }
        
        revwStatus <- dbFetch(
          dbSendQuery(
            conn = dbCon, 
            statement = sprintf("SELECT status FROM tbl_document_metadata WHERE id = '%s';",
                                modSrvrs$mSchema$idDoc)))[["status"]]
        
        if (revwStatus != "PUBLISHED") { 
          rvToReturn$done <- TRUE
          return(NULL) 
        }
        
        # Update the 'outcome' variable of the original document (ONLY the 
        # original doc's).
        outcome <- "REVIEWED"
        if ("final_rd" %in% names(modSrvrs$mSchema$results)) {
          outcome <- switch(modSrvrs$mSchema$results$final_rd,
                            APPROVE = "APPROVED",
                            APPROVED = "APPROVED",
                            YES = "APPROVED",
                            REJECT = "REJECTED",
                            REJECTED = "REJECTED",
                            NO = "REJECTED") 
        }
        
        # Update 'outcome' in tbl_document_metadata.
        res <- dbSendStatement(
          conn = dbCon,
          statement = sprintf("UPDATE tbl_document_metadata SET outcome = '%s' WHERE id = '%s';",
                              outcome,
                              rvOther$dfMdOrig$id))
        dbClearResult(res)
        
        # Add entry into tbl_link_originals_x_reviews
        dfAdd <- data.frame(id_original = rvOther$dfMdOrig$id,
                            id_review = modSrvrs$mSchema$idDoc,
                            outcome = outcome)
        addToDb(dfAdd = dfAdd, 
                tblNm = "tbl_link_originals_x_reviews", 
                vnmsChckEx = names(dfAdd), 
                dbCon = dbCon, 
                calcIds = FALSE, 
                returnIds = FALSE)

        # Let the workflow handler check if any VCs or tokens should now be 
        # issued or minted or whatever.
        {
          showModal(
            modalDialog(
              title = "Updating Workflow State...", 
              "This may take a moment or two. Please be patient.", 
              footer = NULL, 
              size = "s", 
              easyClose = FALSE))
          
          q <- sprintf("SELECT handler_r FROM tbl_workflows WHERE id = '%s';",
                       rvOther$dfMdOrig$id_workflow)
          wrkflwHndlr <- dbGetQuery(conn = dbCon, statement = q)[["handler_r"]]
          
          do.call(what = paste0("checkWorkflowTriggers.", wrkflwHndlr), 
                  args = list(dbCon = dbCon,
                              hederaClient = hederaClient,
                              idDoc = modSrvrs$mSchema$idDoc))
          
          removeModal()
        }

        rvToReturn$done <- TRUE

      })
      
      # outputs ----------------------------------------------------------------

      output$uioMain <- renderUI({
        
        # validate(need(rvOther$nmSchemaModUI, message = FALSE))
        # 
        # return(schemaUI(id = ns("mSchema"), 
        #                 nmSchemaModUI = rvOther$nmSchemaModUI, 
        #                 schemaUIargs = NULL))
        
        return(schemaUI(id = ns("mSchema")))
        
      })
      
      output$toOrigDocDate <- renderText({
        validate(need(rvOther$dfMdOrig, message = FALSE))
        return(sprintf("Date last modified: %s", 
                       rvOther$dfMdOrig$date_modified))
      })
      
      output$toOrigDocAuthor <- renderText({
        validate(need(rvOther$dfMdOrig, message = FALSE))
        return(sprintf("Author: %s", 
                       rvOther$dfMdOrig$did_author))
      }) 
      
      output$toOrigDocMsgId <- renderText({
        validate(need(rvOther$dfMdOrig, message = FALSE))
        return(sprintf("Message ID: %s", 
                       rvOther$dfMdOrig$id_message_h))
      }) 
      
      output$toOrigDocIpfsUri <- renderText({
        validate(need(rvOther$dfMdOrig, message = FALSE))
        return(sprintf("IPFS URI: %s", 
                       rvOther$dfMdOrig$uri_ipfs))
      }) 
      
      # return logic -----------------------------------------------------------
      
      # observe({
      #   rvToReturn$allResultsGood <- all(unlist(reactiveValuesToList(resultsGood)))
      # })
      
      rvOther$refresh <- Sys.time()
      
      return(rvToReturn)
      
    })
}
