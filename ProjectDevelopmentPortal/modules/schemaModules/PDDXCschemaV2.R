
PDDXCschemaV2UI <- function(id, 
                            lsPreset = NULL, 
                            hL = 4, 
                            colWidth = 12, 
                            inpWidth = DEFAULT_INP_WIDTH,
                            idSchemaV = NULL) {
  
  ns <- NS(id)
  
  tagList(
    
    #useShinyjs(),
    
    GtextOutput(id = ns("id_project"), 
                title = "Project ID", 
                hL = hL, 
                colWidth = colWidth),
    
    GtextOutput(id = ns("toDRPLA"), 
                title = "Approving DR-PLA of Project", 
                hL = hL, 
                colWidth = colWidth),
    
    chooseLicenseInput(
      id = ns('chLicPDL'),
      title = "Project Developer License",
      helpTxt = "Select your Independent Impact Project Developer License (II-L-PD) to use for this project."),
    
    GtextAreaInput(id = ns('modalities'),
                   title = 'Modalities',
                   helpTxt = "Modalities for local stakeholder consultation: (100 chars. min.)",
                   value = lsPreset$modalities,
                   resize = 'vertical',
                   hL = hL,
                   colWidth = colWidth,
                   inpWidth = inpWidth),
    
    GtextAreaInput(id = ns('summary_comments'),
                   title = 'Comments Summary',
                   helpTxt = "Summary of comments received:",
                   value = lsPreset$summary_comments,
                   resize = 'vertical',
                   hL = hL,
                   colWidth = colWidth,
                   inpWidth = inpWidth),
    
    GtextAreaInput(id = ns('consideration_comments'),
                   title = 'Comments Consideration',
                   helpTxt = "Consideration of comments received:",
                   value = lsPreset$consideration_comments,
                   resize = 'vertical',
                   hL = hL,
                   colWidth = colWidth,
                   inpWidth = inpWidth),
    
    textOutput(outputId = ns("toInvalidInput")),
    
    br())
}

PDDXCschemaV2Server <- function(id,  
                                dbCon,
                                loginInfoUsr,
                                idEntity,
                                idWorkflow = NULL,
                                idSchema = NULL,
                                lsPreset = NULL) {
  
  moduleServer(
    id = id, 
    function(input, output, session) {
      
      ns <- session$ns
      
      rvToReturn <- reactiveValues()
      rvToReturn$allResultsGood <- FALSE
      rvToReturn$results <- reactiveValues(
        headers = list(
          id_msg_pred = NULL, 
          url_ipfs_pred = NULL, 
          id_msg_lic = NULL,
          url_ipfs_lic = NULL,
          id_subject = idEntity,
          type_subject = "PROJECT"),
        modalities = NULL,
        summary_comments = NULL,
        consideration_comments = NULL)
      
      resultsGood <- reactiveValues()
      resultsGood$headers <- FALSE
      resultsGood$modalities <- FALSE
      resultsGood$summary_comments <- FALSE
      resultsGood$consideration_comments <- FALSE
      
      outputMsgs <- reactiveValues()
      outputMsgs$modalities <- ''
      outputMsgs$summary_comments <- ''
      outputMsgs$consideration_comments <- ''
      
      rvOther <- reactiveValues()
      
      # preset inputs ----------------------------------------------------------
      
      if ('modalities' %in% names(lsPreset)) {
        updateTextAreaInput(session = session, 
                            inputId = 'modalities', 
                            value = lsPreset$modalities)
      }
      
      if ('summary_comments' %in% names(lsPreset)) {
        updateTextAreaInput(session = session, 
                            inputId = 'summary_comments', 
                            value = lsPreset$summary_comments)
      }
      
      if ('consideration_comments' %in% names(lsPreset)) {
        updateTextAreaInput(session = session, 
                            inputId = 'consideration_comments', 
                            value = lsPreset$consideration_comments)
      }
      
      # module servers ---------------------------------------------------------
      
      modSrvrs <- reactiveValues()
      
      modSrvrs$mProjDevLic <- chooseLicenseServer(
        id = "chLicPDL",
        dbCon = dbCon,
        scopes = "PROJECT_DEVELOPER", 
        statuses = "ACTIVE",
        idAgent = loginInfoUsr$id_agent,
        lsPreset = NULL) # TODO: implement lsPreset
      
      # inputs -----------------------------------------------------------------
      
      observe({
        rvToReturn$results$headers$id_msg_lic <- modSrvrs$mProjDevLic$results$id_msg
        rvToReturn$results$headers$url_ipfs_lic <- modSrvrs$mProjDevLic$results$url_ipfs
      })
      
      observeEvent(input$modalities, handlerExpr = {
        
        resultsGood$modalities <- FALSE
        rvToReturn$results$modalities <- input$modalities
        
        nChar <- nchar(input$modalities)
        outputMsgs$modalities <- sprintf('%d chars. remaining', 1000 - nChar)
        
        resultsGood$modalities <- isValidInput.text(
          x = input$modalities, 
          bRequired = TRUE, 
          nCharMin = 100, 
          nCharMax = 1000) 
        
      })
      
      observeEvent(input$summary_comments, handlerExpr = {
        
        resultsGood$summary_comments <- FALSE
        rvToReturn$results$summary_comments <- input$summary_comments
        
        nChar <- nchar(input$summary_comments)
        outputMsgs$summary_comments <- sprintf('%d chars. remaining', 1000 - nChar)
        
        resultsGood$summary_comments <- isValidInput.text(
          x = input$summary_comments, 
          bRequired = FALSE, 
          nCharMin = 0, 
          nCharMax = 1000) 
        
      })
      
      observeEvent(input$consideration_comments, handlerExpr = {
        
        resultsGood$consideration_comments <- FALSE
        rvToReturn$results$consideration_comments <- input$consideration_comments
        
        nChar <- nchar(input$consideration_comments)
        outputMsgs$consideration_comments <- sprintf('%d chars. remaining', 1000 - nChar)
        
        resultsGood$consideration_comments <- isValidInput.text(
          x = input$consideration_comments, 
          bRequired = FALSE, 
          nCharMin = 0, 
          nCharMax = 1000) 
        
      })
      
      observe({
        resultsGood$headers <- FALSE
        
        if (!isValidInput.text(
          x = rvToReturn$results$headers$id_msg_pred, 
          bRequired = TRUE, 
          nCharMin = 5)) {
          return(invisible(1))
        }
        
        if (!isValidInput.text(
          x = rvToReturn$results$headers$url_ipfs_pred, 
          bRequired = TRUE, 
          nCharMin = 5)) {
          return(invisible(2))
        }
        
        if (!modSrvrs$mProjDevLic$allResultsGood) {
          return(invisible(3))
        }
        
        resultsGood$headers <- TRUE
      })
      
      # outputs ----------------------------------------------------------------
      
      output$id_project <- renderText({
        rvToReturn$results$headers$id_subject
      })
      
      output$toDRPLA <- renderText({
        
        noDocMsg <- "You do not seem to have an approved Project Listing Application (PLA) for this project yet. Please use the 'Independent Impact - Main Workflow' to apply for project listing."
        
        dfDocMd <- getApprovingDRPLAs(
          dbCon = dbCon,
          idProject = idEntity)
        if (length(dfDocMd) == 0) {
          return(noDocMsg)
        }
        
        rvToReturn$results$headers$id_msg_pred <- dfDocMd$id_message_h
        rvToReturn$results$headers$url_ipfs_pred <- dfDocMd$uri_ipfs
        
        return(dfDocMd$uri_ipfs)
        
      })
      
      output$modalities_msg <- renderText({
        outputMsgs$modalities
      })
      
      output$summary_comments_msg <- renderText({
        outputMsgs$summary_comments
      })
      
      output$consideration_comments_msg <- renderText({
        outputMsgs$consideration_comments
      })
      
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
