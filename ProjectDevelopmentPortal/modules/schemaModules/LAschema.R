
LAschemaUI <- function(id, 
                       lsPreset = NULL, 
                       hL = 4, 
                       colWidth = 12, 
                       inpWidth = DEFAULT_INP_WIDTH) {
  
  ns <- NS(id)
  
  tagList(
    
    #useShinyjs(),

    # GselectInput(id = ns("siDRAD"), 
    #              title = "Select AD VC", 
    #              choices = c(), 
    #              helpTxt = "Select which one of your reviewed Agent Details (AD) VCs you would like to base this license application on.", 
    #              selected = c(), 
    #              multiple = FALSE, 
    #              hL = hL, 
    #              colWidth = colWidth, 
    #              inpWidth = inpWidth),
    
    fluidRow(
      column(
        width = colWidth,
        wellPanel(
          hx(x = "Select AD document", lvl = hL),
          helpText("Select which one of your approved Agent Details (AD) documents you would like to base this license application on."),
          selectInput(
            inputId = ns('siDRAD'), 
            label = NULL, 
            choices = c(), 
            selected = c(), 
            multiple = FALSE, 
            selectize = TRUE, 
            width = inpWidth, 
            size = NULL),
          textOutput(outputId = ns("siDRAD_msg"))))),

    # GselectInput(id = ns('license'),
    #              title = 'License',
    #              helpTxt = 'Which license do you wish to apply for?', 
    #              choices = c('PROJECT_DEVELOPER', 'PDD_VALIDATOR', 'MR_VERIFIER'),
    #              selected = lsPreset$license,
    #              hL = hL,
    #              colWidth = colWidth,
    #              inpWidth = inpWidth)
    
    fluidRow(
      column(
        width = colWidth,
        wellPanel(
          hx(x = "License", lvl = hL),
          helpText('Which license do you wish to apply for?'),
          selectInput(
            inputId = ns('license'), 
            label = NULL, 
            choices = c('PROJECT_DEVELOPER', 'PDD_VALIDATOR', 'MR_VERIFIER'), 
            selected = lsPreset$license, 
            multiple = FALSE, 
            selectize = TRUE, 
            width = inpWidth, 
            size = NULL),
          textOutput(outputId = ns("license_msg"))))))
}

LAschemaServer <- function(id,  
                           dbCon,
                           loginInfoUsr,
                           idEntity = NULL,
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
          id_msg_pred = lsPreset$headers$id_msg_pred, # DR-AD
          url_ipfs_pred = lsPreset$headers$url_ipfs_pred, # DR-AD
          id_subject = loginInfoUsr$id_agent,
          type_subject = "AGENT"),
        license = NULL,
        id_acc_h = dbFetch(
          dbSendQuery(
            conn = dbCon, 
            statement = sprintf("SELECT id_acc_h FROM tbl_link_agents_x_hedera_accounts WHERE id_agent = '%s';",
                                loginInfoUsr$id_agent)))[["id_acc_h"]])
      
      resultsGood <- reactiveValues()
      resultsGood$id_msg_pred <- FALSE
      resultsGood$url_ipfs_pred <- FALSE
      resultsGood$license <- FALSE
      
      outputMsgs <- reactiveValues()
      outputMsgs$siDRAD <- ''
      outputMsgs$license <- ''
      
      rvOther <- reactiveValues()
      rvOther$dfDRADs <- NULL
      
      # preset inputs ----------------------------------------------------------
      
      # input$siDRAD
      {
        # Get all approved ADs for this agent.
        dfADs <- getPubWrkflwStepDocsMd(
          nmWrkflwHndlr = "indImpAgntLcnsngWrkflwHndlr1", # TODO. What if we eventually have a ...2 and ...3, etc.?
          workflowStep = "rvcdb_agent_AD",
          entityId = loginInfoUsr$id_agent, 
          dbCon = dbCon, 
          lsFilters = NULL # TODO. Filter out those that have been archived or whatever by the agent.
        )
        dfADs <- dfADs[which(dfADs$outcome == "APPROVED"),]
        
        # Get all the corresponding DR-ADs for this agent.
        dfDRADs <- NULL
        if (nrow(dfADs) > 0) {
          dfDRADs <- getPubWrkflwStepDocsMd(
            nmWrkflwHndlr = "indImpAgntLcnsngWrkflwHndlr1", # TODO. What if we eventually have a ...2 and ...3, etc.?
            workflowStep = "rvcdb_standardBody_DR_AD",
            entityId = loginInfoUsr$id_agent, 
            dbCon = dbCon, 
            lsFilters = NULL # TODO. Filter out those that have been archived or whatever by the agent.
          )
          dfDRADs <- dfDRADs[which(dfDRADs$id_msg_pred %in% dfADs$id_message_h),]
        }

        bNoAd <- FALSE
        if (length(dfDRADs) == 0) {
          bNoAd <- TRUE
        } else {
          if (nrow(dfDRADs) == 0) {
            bNoAd <- TRUE
          }
        }
        
        if (bNoAd) {
          
          rvOther$dfDRADs <- NULL; rm(dfDRADs)
          
          updateSelectInput(session = session, 
                            inputId = "siDRAD", 
                            choices = c(), 
                            selected = NULL)
          
          outputMsgs$siDRAD <- 'You do not seem to have any approved Agent Details (AD) documents yet. If you have recently submitted an AD document for review, please wait for the review to be completed; otherwise go back to the previous step of the workflow and fill one in.'
          
        } else {

          chcs <- 1:nrow(dfDRADs)
          names(chcs) <- sprintf("%s (%s)", 1:nrow(dfDRADs), dfDRADs$date_modified)
          
          idxSlctd <- 1
          if (length(lsPreset$headers$id_msg_pred) == 1) {
            if (!is.na(lsPreset$headers$id_msg_pred)) {
              if (nchar(lsPreset$headers$id_msg_pred) > 0) {
                idxSlctd <- which(dfDRADs$id_message_h == lsPreset$headers$id_msg_pred)
                if (length(idxSlctd) != 1) {
                  idxSlctd <- 1
                }
              }
            }
          }
          
          rvOther$dfDRADs <- dfDRADs; rm(dfDRADs)
          
          updateSelectInput(session = session, 
                            inputId = "siDRAD", 
                            choices = chcs, 
                            selected = chcs[idxSlctd])
        }
        
      }
    
      if ('license' %in% names(lsPreset)) {
        updateSelectInput(session = session, 
                          inputId = 'license', 
                          selected = lsPreset$license)
      }
      
      # module servers ---------------------------------------------------------
      
      modSrvrs <- reactiveValues()
    
      # inputs -----------------------------------------------------------------
      
      observeEvent(input$siDRAD, handlerExpr = {
        
        resultsGood$id_msg_pred <- FALSE
        resultsGood$url_ipfs_pred <- FALSE
        
        validate(need(input$siDRAD, message = FALSE))
        
        idx <- as.integer(input$siDRAD)
        
        rvToReturn$results$headers$id_msg_pred <- rvOther$dfDRADs$id_message_h[idx] 
        rvToReturn$results$headers$url_ipfs_pred <- rvOther$dfDRADs$uri_ipfs[idx] 
        resultsGood$id_msg_pred <- TRUE
        resultsGood$url_ipfs_pred <- TRUE
        
      })
      
      observeEvent(input$license, handlerExpr = {
        
        resultsGood$license <- FALSE
        outputMsgs$license <- ''
        rvToReturn$results$license <- input$license
        validate(need(input$license, message = FALSE))
        resultsGood$license <- TRUE
        
      })
      
      
      # outputs ----------------------------------------------------------------
      
      output$siDRAD_msg <- renderText({
        outputMsgs$siDRAD
      })
      
      output$license_msg <- renderText({
        outputMsgs$license
      })
      
      # return logic -----------------------------------------------------------
      
      observe({
        rvToReturn$allResultsGood <- all(unlist(reactiveValuesToList(resultsGood)))
      })
      
      return(rvToReturn)
      
    })
}
