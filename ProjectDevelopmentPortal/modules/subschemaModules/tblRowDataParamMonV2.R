
# Module for schema 'Table Row: Data/Parameter Monitoring [v3.0.1]'.

tblRowDataParamMonV2UI <- function(id, 
                                   lsPreset = NULL, 
                                   hL = 4, 
                                   colWidth = 12, 
                                   inpWidth = DEFAULT_INP_WIDTH) {
  
  ns <- NS(id)
  
  tagList(
    
    #useShinyjs(),
    
    GtextInput(id = ns('label'),
               title = 'Label',
               helpTxt = 'Please provide a unique label for this parameter or data point (4 chars. min.):', 
               value = lsPreset$label,
               hL = hL,
               colWidth = colWidth,
               inpWidth = inpWidth),
    
    GtextInput(id = ns('unit_of_measure'),
               title = 'Data Unit',
               helpTxt = 'Unit of measure.',
               value = lsPreset$unit_of_measure,
               hL = hL,
               colWidth = colWidth,
               inpWidth = inpWidth),
    
    GtextAreaInput(id = ns('description'),
                   title = 'Description',
                   helpTxt = 'Description. (10 chars. min.)', 
                   value = lsPreset$description,
                   resize = 'vertical',
                   hL = hL,
                   colWidth = colWidth,
                   inpWidth = inpWidth),
    
    GtextAreaInput(id = ns('purpose'),
                   title = 'Purpose',
                   helpTxt = 'Purpose of data. (50 chars. min.)',
                   value = lsPreset$purpose,
                   resize = 'vertical',
                   hL = hL,
                   colWidth = colWidth,
                   inpWidth = inpWidth),
    
    fluidRow(
      column(
        width = colWidth,
        wellPanel(
          hx(x = "Monitored or Fixed", lvl = hL),
          helpText("Will this parameter / data point be monitored or will it be fixed ex-ante?"),
          selectInput(
            inputId = ns("monitored_or_fixed"), 
            label = NULL, 
            choices = c("MONITORED", "FIXED_EXANTE"), 
            selected = NULL,
            multiple = FALSE, 
            width = inpWidth, 
            selectize = TRUE)
        ))),
    # GselectInput(id = ns("monitored_or_fixed"), 
    #              title = "Monitored or Fixed", 
    #              helpTxt = "Will this parameter / data point be monitored or will it be fixed ex-ante?",
    #              choices = c("MONITORED", "FIXED_EXANTE"), 
    #              multiple = FALSE, 
    #              selectize = TRUE, 
    #              #size = , 
    #              hL = hL, 
    #              colWidth = colWidth, 
    #              inpWidth = inpWidth),
    
    uiOutput(outputId = ns("uioMain")),
    
    textOutput(outputId = ns("toInvalidInput")),
    
    br())
}

tblRowDataParamMonV2Server <- function(id,  
                                       lsPreset = NULL, 
                                       hL = 4, 
                                       colWidth = 12, 
                                       inpWidth = DEFAULT_INP_WIDTH) {
  
  moduleServer(
    id = id, 
    function(input, output, session) {
      
      ns <- session$ns
      
      rvToReturn <- reactiveValues()
      rvToReturn$allResultsGood <- FALSE
      rvToReturn$results <- reactiveValues(
        label = NULL,
        unit_of_measure = NULL,
        description = NULL,
        purpose = NULL,
        monitored_or_fixed = NULL,
        
        value_applied = lsPreset$value_applied,
        data_source = lsPreset$data_source,
        
        measurement_methods_and_procedures = lsPreset$measurement_methods_and_procedures,
        sampling_plan = lsPreset$sampling_plan,
        qa_qc_procedures = lsPreset$qa_qc_procedures,
        monitoring_frequency = lsPreset$monitoring_frequency)
      
      resultsGood <- reactiveValues()
      resultsGood$label <- FALSE
      resultsGood$unit_of_measure <- FALSE
      resultsGood$description <- FALSE
      resultsGood$purpose <- FALSE
      resultsGood$monitored_or_fixed <- FALSE
      
      resultsGoodF <- reactiveValues()
      resultsGoodF$value_applied <- FALSE
      resultsGoodF$data_source <- FALSE
      
      resultsGoodM <- reactiveValues()
      resultsGoodM$measurement_methods_and_procedures <- FALSE
      resultsGoodM$sampling_plan <- FALSE
      resultsGoodM$monitoring_frequency <- FALSE
      resultsGoodM$qa_qc_procedures <- FALSE
      
      outputMsgs <- reactiveValues()
      outputMsgs$label <- ''
      outputMsgs$unit_of_measure <- ''
      outputMsgs$description <- ''
      outputMsgs$purpose <- ''
      outputMsgs$monitored_or_fixed <- ''
      
      outputMsgs$value_applied <- ''
      outputMsgs$data_source <- ''
      
      outputMsgs$measurement_methods_and_procedures <- ''
      outputMsgs$sampling_plan <- ''
      outputMsgs$qa_qc_procedures <- ''
      outputMsgs$monitoring_frequency <- ''
      
      rvOther <- reactiveValues()
      
      # UI rendering -----------------------------------------------------------
      output$uioMain <- renderUI({
        
        if (length(rvToReturn$results$monitored_or_fixed) == 0) {
          return(NULL)
        }
        
        if (rvToReturn$results$monitored_or_fixed == "FIXED_EXANTE") {
          return(
            tagList(
              # Making use of a textInput instead of a numericInput, because the stepping
              # business and the max and min params of the numericInput don't provide what
              # we need for this field.
              GtextInput(id = ns('value_applied'),
                         title = 'Value',
                         helpTxt = 'Value(s) applied.', 
                         value = isolate(rvToReturn$results$value_applied), # TODO
                         hL = hL,
                         colWidth = colWidth,
                         inpWidth = inpWidth),
              
              GtextAreaInput(id = ns('data_source'),
                             title = 'Source',
                             helpTxt = 'Source of data. (10 chars. min.)', 
                             value = isolate(rvToReturn$results$data_source), # TODO.
                             resize = 'vertical',
                             hL = hL,
                             colWidth = colWidth,
                             inpWidth = inpWidth)))
        }
        
        return(
          tagList(
            
            GtextAreaInput(id = ns('measurement_methods_and_procedures'),
                           title = 'Measurement Methods and Procedures',
                           helpTxt = 'Describe the methods and procedures that you will use or follow to monitor this parameter / obtain this data (50 chars. min):',
                           value = isolate(rvToReturn$results$measurement_methods_and_procedures), # TODO.
                           resize = 'vertical',
                           hL = hL,
                           colWidth = colWidth,
                           inpWidth = inpWidth),
            
            GtextAreaInput(id = ns('sampling_plan'),
                           title = 'Sampling Plan',
                           helpTxt = 'Sampling plan (10 chars. min.):',
                           value = isolate(rvToReturn$results$sampling_plan), # TODO.
                           resize = 'vertical',
                           hL = hL,
                           colWidth = colWidth,
                           inpWidth = inpWidth),
            
            GtextAreaInput(id = ns('qa_qc_procedures'),
                           title = 'Quality Control and Assurance',
                           helpTxt = 'QA/QC procedures. (4 chars. min.)', 
                           value = isolate(rvToReturn$results$qa_qc_procedures), # TODO.
                           resize = 'vertical',
                           hL = hL,
                           colWidth = colWidth,
                           inpWidth = inpWidth),
            
            GtextAreaInput(id = ns('monitoring_frequency'),
                           title = 'Monitoring Frequency',
                           helpTxt = 'Monitoring frequency. (4 chars. min.)', 
                           value = isolate(rvToReturn$results$monitoring_frequency), # TODO.
                           resize = 'vertical',
                           hL = hL,
                           colWidth = colWidth,
                           inpWidth = inpWidth)))
        
      })
      
      # preset inputs ----------------------------------------------------------
      
      if ('label' %in% names(lsPreset)) {
        updateTextInput(session = session, 
                        inputId = 'label', 
                        value = lsPreset$label)
      }
      
      if ('unit_of_measure' %in% names(lsPreset)) {
        updateTextInput(session = session, 
                        inputId = 'unit_of_measure', 
                        value = lsPreset$unit_of_measure)
      }
      
      if ('description' %in% names(lsPreset)) {
        updateTextAreaInput(session = session, 
                            inputId = 'description', 
                            value = lsPreset$description)
      }
      
      if ('purpose' %in% names(lsPreset)) {
        updateTextAreaInput(session = session, 
                            inputId = 'purpose', 
                            value = lsPreset$purpose)
      }
      
      if ('monitored_or_fixed' %in% names(lsPreset)) {
        updateSelectInput(session = session, 
                          inputId = "monitored_or_fixed", 
                          selected = lsPreset$monitored_or_fixed)
      }
      
      # module servers ---------------------------------------------------------
      # modSrvrs <- reactiveValues()
      
      # inputs -----------------------------------------------------------------
      
      observeEvent(input$label, handlerExpr = {
        
        resultsGood$label <- FALSE
        rvToReturn$results$label <- input$label
        
        nChar <- nchar(input$label)
        outputMsgs$label <- sprintf('%d chars. remaining', 50 - nChar)
        
        resultsGood$label <- isValidInput.text(
          x = input$label, 
          bRequired = TRUE, 
          nCharMin = 4, 
          nCharMax = 50) 
        
      })
      
      observeEvent(input$unit_of_measure, handlerExpr = {
        
        resultsGood$unit_of_measure <- FALSE
        rvToReturn$results$unit_of_measure <- input$unit_of_measure
        
        nChar <- nchar(input$unit_of_measure)
        outputMsgs$unit_of_measure <- sprintf('%d chars. remaining', 10 - nChar)
        
        resultsGood$unit_of_measure <- isValidInput.text(
          x = input$unit_of_measure, 
          bRequired = TRUE, 
          nCharMin = 1, 
          nCharMax = 10) 
        
      })
      
      observeEvent(input$description, handlerExpr = {
        
        resultsGood$description <- FALSE
        rvToReturn$results$description <- input$description
        
        nChar <- nchar(input$description)
        outputMsgs$description <- sprintf('%d chars. remaining', 300 - nChar)
        
        resultsGood$description <- isValidInput.text(
          x = input$description, 
          bRequired = TRUE, 
          nCharMin = 10, 
          nCharMax = 300) 
        
      })
      
      observeEvent(input$purpose, handlerExpr = {
        
        resultsGood$purpose <- FALSE
        rvToReturn$results$purpose <- input$purpose
        
        nChar <- nchar(input$purpose)
        outputMsgs$purpose <- sprintf('%d chars. remaining', 1000 - nChar)
        
        resultsGood$purpose <- isValidInput.text(
          x = input$purpose, 
          bRequired = TRUE, 
          nCharMin = 50, 
          nCharMax = 1000) 
        
      })
      
      observeEvent(input$monitored_or_fixed, handlerExpr = {
        
        resultsGood$monitored_or_fixed <- FALSE
        outputMsgs$monitored_or_fixed <- ''
        rvToReturn$results$monitored_or_fixed <- input$monitored_or_fixed
        validate(need(input$monitored_or_fixed, message = FALSE))
        if (length(input$monitored_or_fixed) == 0) { return(invisible(NULL)) }
        resultsGood$monitored_or_fixed <- TRUE

        if (rvToReturn$results$monitored_or_fixed == "MONITORED") {
          rvToReturn$results$value_applied <- NULL
          rvToReturn$results$data_source <- NULL
        } else {
          rvToReturn$results$measurement_methods_and_procedures <- NULL
          rvToReturn$results$sampling_plan <- NULL
          rvToReturn$results$monitoring_frequency <- NULL
          rvToReturn$results$qa_qc_procedures <- NULL
        }
      })
      
      
      observeEvent(input$value_applied, handlerExpr = {
        
        resultsGoodF$value_applied <- FALSE
        outputMsgs$value_applied <- ''
        
        res <- validateNumeric(input$value_applied)
        if (!res$is_valid) {
          outputMsgs$value_applied <- res$msg
          return(NULL)
        }
        
        rvToReturn$results$value_applied <- res$val
        resultsGoodF$value_applied <- TRUE
        
      })
      
      observeEvent(input$data_source, handlerExpr = {
        
        resultsGoodF$data_source <- FALSE
        rvToReturn$results$data_source <- input$data_source
        
        nChar <- nchar(input$data_source)
        outputMsgs$data_source <- sprintf('%d chars. remaining', 1000 - nChar)
        
        resultsGoodF$data_source <- isValidInput.text(
          x = input$data_source, 
          bRequired = TRUE, 
          nCharMin = 10,
          nCharMax = 1000) 
        
      })
      
      
      observeEvent(input$measurement_methods_and_procedures, handlerExpr = {
        
        resultsGoodM$measurement_methods_and_procedures <- FALSE
        rvToReturn$results$measurement_methods_and_procedures <- input$measurement_methods_and_procedures
        
        nChar <- nchar(input$measurement_methods_and_procedures)
        outputMsgs$measurement_methods_and_procedures <- sprintf('%d chars. remaining', 1000 - nChar)
        
        resultsGoodM$measurement_methods_and_procedures <- isValidInput.text(
          x = input$measurement_methods_and_procedures, 
          bRequired = TRUE, 
          nCharMin = 50,
          nCharMax = 1000) 
        
      })
      
      observeEvent(input$sampling_plan, handlerExpr = {
        
        resultsGoodM$sampling_plan <- FALSE
        rvToReturn$results$sampling_plan <- input$sampling_plan
        
        nChar <- nchar(input$sampling_plan)
        outputMsgs$sampling_plan <- sprintf('%d chars. remaining', 1000 - nChar)
        
        resultsGoodM$sampling_plan <- isValidInput.text(
          x = input$sampling_plan, 
          bRequired = FALSE, 
          nCharMin = 10, 
          nCharMax = 1000) 
        
      })
      
      observeEvent(input$qa_qc_procedures, handlerExpr = {
        
        resultsGoodM$qa_qc_procedures <- FALSE
        rvToReturn$results$qa_qc_procedures <- input$qa_qc_procedures
        
        nChar <- nchar(input$qa_qc_procedures)
        outputMsgs$qa_qc_procedures <- sprintf('%d chars. remaining', 1000 - nChar)
        
        resultsGoodM$qa_qc_procedures <- isValidInput.text(
          x = input$qa_qc_procedures, 
          bRequired = TRUE, 
          nCharMin = 4, 
          nCharMax = 1000) 
        
      })
      
      observeEvent(input$monitoring_frequency, handlerExpr = {
        
        resultsGoodM$monitoring_frequency <- FALSE
        rvToReturn$results$monitoring_frequency <- input$monitoring_frequency
        
        nChar <- nchar(input$monitoring_frequency)
        outputMsgs$monitoring_frequency <- sprintf('%d chars. remaining', 500 - nChar)
        
        resultsGoodM$monitoring_frequency <- isValidInput.text(
          x = input$monitoring_frequency, 
          bRequired = TRUE, 
          nCharMin = 4, 
          nCharMax = 500) 
        
      })
      
      
      # outputs ----------------------------------------------------------------
      
      output$label_msg <- renderText({
        outputMsgs$label
      })
      
      output$unit_of_measure_msg <- renderText({
        outputMsgs$unit_of_measure
      })
      
      output$description_msg <- renderText({
        outputMsgs$description
      })
      
      output$purpose_msg <- renderText({
        outputMsgs$purpose
      })
      
      output$monitored_or_fixed_msg <- renderText({
        outputMsgs$monitored_or_fixed
      })
      
      
      output$value_applied_msg <- renderText({
        outputMsgs$value_applied
      })
      
      output$data_source_msg <- renderText({
        outputMsgs$data_source
      })
      
      
      output$measurement_methods_and_procedures_msg <- renderText({
        outputMsgs$measurement_methods_and_procedures
      })
      
      output$sampling_plan_msg <- renderText({
        outputMsgs$sampling_plan
      })
      
      output$qa_qc_procedures_msg <- renderText({
        outputMsgs$qa_qc_procedures
      })
      
      output$monitoring_frequency_msg <- renderText({
        outputMsgs$monitoring_frequency
      })
      
      
      output$toInvalidInput <- renderText({
        
        nms <- names(resultsGood)[!unlist(reactiveValuesToList(resultsGood))]
        
        if (resultsGood$monitored_or_fixed) {
          if (rvToReturn$results$monitored_or_fixed == "FIXED_EXANTE") {
            nms <- c(nms, names(resultsGoodF)[!unlist(reactiveValuesToList(resultsGoodF))])
          } else {
            nms <- c(nms, names(resultsGoodM)[!unlist(reactiveValuesToList(resultsGoodM))])
          }
        }
        
        if (length(nms) == 0) {
          return (NULL)
        }
        
        return(
          sprintf("Your inputs to the following fields are not valid: %s",
                  paste(nms, sep = "", collapse = ", ")))
      })
      
      # return logic -----------------------------------------------------------
      
      observe({
        
        if (!resultsGood$monitored_or_fixed) {
          rvToReturn$allResultsGood <- FALSE
          return(invisible(0))
        }
        
        if (rvToReturn$results$monitored_or_fixed == "FIXED_EXANTE") {
          rvToReturn$allResultsGood <- all(unlist(reactiveValuesToList(resultsGood))) & 
            all(unlist(reactiveValuesToList(resultsGoodF)))
          return(invisible(0))
        }
        
        if (rvToReturn$results$monitored_or_fixed == "MONITORED") {
          rvToReturn$allResultsGood <- all(unlist(reactiveValuesToList(resultsGood))) & 
            all(unlist(reactiveValuesToList(resultsGoodM)))
          return(invisible(0))
        }
        
      })
      
      return(rvToReturn)
      
    })
}
