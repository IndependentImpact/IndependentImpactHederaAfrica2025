
datasetInput <- function(id, 
                         lsPreset = NULL, 
                         hL = 5,
                         colWidth = 12, 
                         inpWidth = DEFAULT_INP_WIDTH) {
  
  ns <- NS(id)
  
  tagList(
    
    ##useShinyjs(),
    
    GtextInput(id = ns("name_dataset"), 
               title = "Dataset Name", 
               helpTxt = "Name of dataset (2 chars. min., 60 chars. max.).", 
               value = lsPreset$name_dataset, 
               hL = hL, 
               colWidth = colWidth, 
               inpWidth = inpWidth),
    
    # GselectInput(id = ns("siDRDLR"), 
    #              title = "Select DR-DLR", 
    #              choices = c(), 
    #              helpTxt = "Select the Document Review: Data Lineage Report that approved the final version of this dataset.", 
    #              selected = c(), 
    #              multiple = FALSE, 
    #              hL = hL, 
    #              colWidth = colWidth, 
    #              inpWidth = inpWidth),
    
    fluidRow(
      column(
        width = colWidth,
        wellPanel(
          hx(x = 'Select DR-DLR', lvl = hL),
          helpText("Select the Document Review: Data Lineage Report that approved the final version of this dataset."),
          selectInput(
            inputId = ns('siDRDLR'), 
            label = NULL, 
            choices = c(), 
            selected = c(), 
            multiple = FALSE, 
            selectize = TRUE, 
            width = inpWidth, 
            size = NULL)))),

    GtextInput(id = ns("uri_ipfs_dataset_final"), 
               title = "IPFS URI of Final Dataset", 
               helpTxt = "IPFS URI of the final, approved version of the processed dataset:", 
               value = lsPreset$uri_ipfs_dataset_final, 
               hL = hL, 
               colWidth = colWidth, 
               inpWidth = inpWidth),
    
    textOutput(outputId = ns("toInvalidInput")),
    
    br())
}

datasetServer <- function(id, 
                          dbCon, 
                          loginInfoSb = NULL, 
                          idEntity, 
                          lsPreset = NULL) {
  
  moduleServer(
    id = id, 
    function(input, output, session) {
      
      ns <- session$ns
      
      rvToReturn <- reactiveValues()
      rvToReturn$goToModule <- NULL
      rvToReturn$allResultsGood <- FALSE
      rvToReturn$results <- reactiveValues(
        name_dataset = NULL,
        id_msg_drdlr = NULL,
        uri_ipfs_drdlr = NULL,
        uri_ipfs_dataset_final = NULL)
      
      resultsGood <- reactiveValues()
      resultsGood$name_dataset <- FALSE
      resultsGood$id_msg_drdlr <- FALSE
      resultsGood$uri_ipfs_drdlr <- FALSE
      resultsGood$uri_ipfs_dataset_final <- FALSE
      
      rvOther <- reactiveValues()
      rvOther$dfDRDLRs <- NULL
      
      outputMsgs <- reactiveValues()
      outputMsgs$name_dataset <- ""
      outputMsgs$siDRDLR <- ""
      outputMsgs$uri_ipfs_dataset_final <- ""
      
      # preset inputs ----------------------------------------------------------
      
      if ("name_dataset" %in% names(lsPreset)) {
        updateTextInput(session = session, 
                        inputId = "name_dataset", 
                        value = lsPreset$name_dataset)
      }
      
      # input$siDRDLR
      {
        # Get all DR-DLRs for this project.
        
        dfDRDLRs <- getApprovingDRs(
          dbCon = dbCon, 
          idProject = idEntity, 
          stepWorkflow = "rvcdb_verifier_DR_DLR",
          all = TRUE)
        
        if (length(dfDRDLRs) == 0) {
          
          rvOther$dfDRDLRs <- NULL
          rm(dfDRDLRs)
          
          updateSelectInput(session = session, 
                            inputId = "siDRDLR", 
                            choices = c(), 
                            selected = NULL)
          
          outputMsgs$siDRDLR <- 'You have no reviewed data lineage reports yet. Please wait for your submitted reports to be reviewed, or go back to the previous step of the policy and submit a DLR for each dataset.'
          
        } else {
          
          if (nrow(dfDRDLRs) == 0) {
            
            rvOther$dfDRDLRs <- NULL
            rm(dfDRDLRs)
            
            updateSelectInput(session = session, 
                              inputId = "siDRDLR", 
                              choices = c(), 
                              selected = NULL)
            
            outputMsgs$siDRDLR <- 'You have no reviewed data lineage reports yet. Please wait for your submitted reports to be reviewed, or go back to the previous step of the policy and submit a DLR for each dataset.'
            
          } else {
            
            #message(sprintf("nrow(dfDRDLRs) = %d", nrow(dfDRDLRs)))
            #save(dfDRDLRs, file = sprintf("%sdfDRDLRs.Rda", tmpdir)) # TODO: Remove after debugging.
            
            chcs <- 1:nrow(dfDRDLRs)
            names(chcs) <- sprintf("%s (%s)", 1:nrow(dfDRDLRs), dfDRDLRs$date_modified)
            
            idxSlctd <- 1
            if (length(lsPreset$id_msg_drdlr) == 1) {
              if (!is.na(lsPreset$id_msg_drdlr)) {
                if (nchar(lsPreset$id_msg_drdlr) > 0) {
                  idxSlctd <- which(dfDRDLRs$id_message_h == lsPreset$id_msg_drdlr)
                  if (length(idxSlctd) != 1) {
                    idxSlctd <- 1
                  }
                }
              }
            }
            
            rvOther$dfDRDLRs <- dfDRDLRs; rm(dfDRDLRs)
            
            updateSelectInput(session = session, 
                              inputId = "siDRDLR", 
                              choices = chcs, 
                              selected = chcs[idxSlctd])
          }
          
        }
        
        
        
      }
      
      if ("uri_ipfs_dataset_final" %in% names(lsPreset)) {
        updateTextInput(session = session, 
                        inputId = "uri_ipfs_dataset_final", 
                        value = lsPreset$uri_ipfs_dataset_final)
      }
      
      # inputs -----------------------------------------------------------------
      
      observeEvent(input$name_dataset, handlerExpr = {
        
        resultsGood$name_dataset <- FALSE
        rvToReturn$results$name_dataset <- input$name_dataset
        
        nChar <- nchar(input$name_dataset)
        outputMsgs$name_dataset <- sprintf('%d chars. remaining', 60 - nChar)
        
        resultsGood$name_dataset <- isValidInput.text(
          x = input$name_dataset, 
          bRequired = TRUE, 
          nCharMin = 2, 
          nCharMax = 60) 
        
      })
      
      observeEvent(input$siDRDLR, handlerExpr = {

        resultsGood$id_msg_drdlr <- FALSE
        resultsGood$uri_ipfs_drdlr <- FALSE
        
        if (length(rvOther$dfDRDLRs) == 0) { return(invisible(0)) }
        if (nrow(rvOther$dfDRDLRs) == 0) { return(invisible(0)) }
        if (length(input$siDRDLR) == 0) { return(invisible(0)) }
        
        if (input$siDRDLR < 1) { return(invisible(0)) }
        
        rvToReturn$results$id_msg_drdlr <- rvOther$dfDRDLRs$id_message_h[as.integer(input$siDRDLR)]
        rvToReturn$results$uri_ipfs_drdlr <- rvOther$dfDRDLRs$uri_ipfs[as.integer(input$siDRDLR)]
        
        resultsGood$id_msg_drdlr <- isValidInput.text(
          x = rvToReturn$results$id_msg_drdlr, 
          bRequired = TRUE, 
          nCharMin = 5)
        resultsGood$uri_ipfs_drdlr <- isValidInput.text(
          x = rvToReturn$results$uri_ipfs_drdlr, 
          bRequired = TRUE, 
          nCharMin = 5)
        
      })
      
      observeEvent(input$id_msg_drdlr, handlerExpr = {
        
        resultsGood$id_msg_drdlr <- FALSE
        rvToReturn$results$id_msg_drdlr <- input$id_msg_drdlr
        
        nChar <- nchar(input$id_msg_drdlr)
        outputMsgs$id_msg_drdlr <- sprintf("%d chars. remaining", 100 - nChar)
        
        resultsGood$id_msg_drdlr <- isValidInput.text(
          x = input$id_msg_drdlr, 
          bRequired = TRUE, 
          nCharMin = 1, 
          nCharMax = 100)
        
      })
      
      observeEvent(input$uri_ipfs_drdlr, handlerExpr = {
        
        resultsGood$uri_ipfs_drdlr <- FALSE
        rvToReturn$results$uri_ipfs_drdlr <- input$uri_ipfs_drdlr
        
        nChar <- nchar(input$uri_ipfs_drdlr)
        outputMsgs$uri_ipfs_drdlr <- sprintf("%d chars. remaining", 100 - nChar)
        
        resultsGood$uri_ipfs_drdlr <- isValidInput.text(
          x = input$uri_ipfs_drdlr, 
          bRequired = TRUE, 
          nCharMin = 1, 
          nCharMax = 100)
        
      })
      
      observeEvent(input$uri_ipfs_dataset_final, handlerExpr = {
        
        resultsGood$uri_ipfs_dataset_final <- FALSE
        rvToReturn$results$uri_ipfs_dataset_final <- input$uri_ipfs_dataset_final
        
        nChar <- nchar(input$uri_ipfs_dataset_final)
        outputMsgs$uri_ipfs_dataset_final <- sprintf("%d chars. remaining", 100 - nChar)
        
        resultsGood$uri_ipfs_dataset_final <- isValidInput.text(
          x = input$uri_ipfs_dataset_final, 
          bRequired = TRUE, 
          nCharMin = 1, 
          nCharMax = 100)
        
      })
      
      # outputs-----------------------------------------------------------------
      
      output$name_msg <- renderText({
        outputMsgs$name
      })
      
      output$is_host_msg <- renderText({
        outputMsgs$is_host
      })
      
      output$public_private_msg <- renderText({
        outputMsgs$public_private
      })
      
      output$is_participant_msg <- renderText({
        outputMsgs$is_participant
      })
      
      output$info_additional_msg <- renderText({
        outputMsgs$info_additional
      })
      
      output$toInvalidInput <- renderText({
        
        if (rvToReturn$allResultsGood) { return(NULL) }
        
        return(
          sprintf("Your inputs to the following fields are not valid: %s",
                  paste(names(resultsGood)[!unlist(reactiveValuesToList(resultsGood))], sep = "", collapse = ", ")))
        
      })
      
      # module return logic ----------------------------------------------------
      
      observe({
        rvToReturn$allResultsGood <- all(unlist(reactiveValuesToList(resultsGood)))
      })
      
      return(rvToReturn)
      
    })
}


# name_dataset
# id_msg_drdlr
# uri_ipfs_drdlr
# uri_ipfs_dataset_final
