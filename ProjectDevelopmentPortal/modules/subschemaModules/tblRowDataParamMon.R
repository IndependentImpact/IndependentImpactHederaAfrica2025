
# Module for schema 'Table Row: Data/Parameter Monitoring [v1.0.0]'.

tblRowDataParamMonUI <- function(id, 
                                 lsPreset = NULL, 
                                 hL = 4, 
                                 colWidth = 12, 
                                 inpWidth = DEFAULT_INP_WIDTH) {
  
  ns <- NS(id)
  
  tagList(
    
    #useShinyjs(),
    
    GtextInput(id = ns('data_parameter'),
               title = 'Data/Parameter',
               helpTxt = 'Data/Parameter. (1 char. min.)', 
               value = lsPreset$data_parameter,
               hL = hL,
               colWidth = colWidth,
               inpWidth = inpWidth),
    
    GtextInput(id = ns('data_unit'),
               title = 'Data Unit',
               helpTxt = 'Unit of measure.',
               value = lsPreset$data_unit,
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
    
    GtextAreaInput(id = ns('source_of_data'),
                   title = 'Source',
                   helpTxt = 'Source of data. (10 chars. min.)', 
                   value = lsPreset$source_of_data,
                   resize = 'vertical',
                   hL = hL,
                   colWidth = colWidth,
                   inpWidth = inpWidth),
    
    # Making use of a textInput instead of a numericInput, because the stepping
    # business and the max and min params of the numericInput don't provide what
    # we need for this field.
    GtextInput(id = ns('values_applied'),
               title = 'Value',
               helpTxt = 'Value(s) applied.', 
               value = lsPreset$values_applied,
               hL = hL,
               colWidth = colWidth,
               inpWidth = inpWidth),
    
    GtextAreaInput(id = ns('methods_and_procedures'),
                   title = 'Measurement Method',
                   helpTxt = 'Measurement methods and procedures. (50 chars. min.)',
                   value = lsPreset$methods_and_procedures,
                   resize = 'vertical',
                   hL = hL,
                   colWidth = colWidth,
                   inpWidth = inpWidth),
    
    GtextAreaInput(id = ns('monitoring_frequency'),
                   title = 'Monitoring Frequency',
                   helpTxt = 'Monitoring frequency. (4 chars. min.)', 
                   value = lsPreset$monitoring_frequency,
                   resize = 'vertical',
                   hL = hL,
                   colWidth = colWidth,
                   inpWidth = inpWidth),
    
    GtextAreaInput(id = ns('qa_qc_procedures'),
                   title = 'Quality Control and Assurance',
                   helpTxt = 'QA/QC procedures. (4 chars. min.)', 
                   value = lsPreset$qa_qc_procedures,
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
    
    GtextAreaInput(id = ns('comment'),
                   title = 'Comment',
                   helpTxt = 'Additional comment.',
                   value = lsPreset$comment,
                   resize = 'vertical',
                   hL = hL,
                   colWidth = colWidth,
                   inpWidth = inpWidth))
}

tblRowDataParamMonServer <- function(id,  
                                     lsPreset = NULL) {
  
  moduleServer(
    id = id, 
    function(input, output, session) {
      
      ns <- session$ns
      
      rvToReturn <- reactiveValues()
      rvToReturn$allResultsGood <- FALSE
      rvToReturn$results <- reactiveValues(
        data_parameter = NULL,
        data_unit = NULL,
        description = NULL,
        source_of_data = NULL,
        values_applied = NULL,
        methods_and_procedures = NULL,
        monitoring_frequency = NULL,
        qa_qc_procedures = NULL,
        purpose = NULL,
        comment = NULL)
      
      resultsGood <- reactiveValues()
      resultsGood$data_parameter <- FALSE
      resultsGood$data_unit <- FALSE
      resultsGood$description <- FALSE
      resultsGood$source_of_data <- FALSE
      resultsGood$values_applied <- FALSE
      resultsGood$methods_and_procedures <- FALSE
      resultsGood$monitoring_frequency <- FALSE
      resultsGood$qa_qc_procedures <- FALSE
      resultsGood$purpose <- FALSE
      resultsGood$comment <- FALSE
      
      outputMsgs <- reactiveValues()
      outputMsgs$data_parameter <- ''
      outputMsgs$data_unit <- ''
      outputMsgs$description <- ''
      outputMsgs$source_of_data <- ''
      outputMsgs$values_applied <- ''
      outputMsgs$methods_and_procedures <- ''
      outputMsgs$monitoring_frequency <- ''
      outputMsgs$qa_qc_procedures <- ''
      outputMsgs$purpose <- ''
      outputMsgs$comment <- ''
      
      rvOther <- reactiveValues()
      
      # preset inputs ----------------------------------------------------------
      
      if ('data_parameter' %in% names(lsPreset)) {
        updateTextInput(session = session, 
                        inputId = 'data_parameter', 
                        value = lsPreset$data_parameter)
      }
      
      if ('data_unit' %in% names(lsPreset)) {
        updateTextInput(session = session, 
                        inputId = 'data_unit', 
                        value = lsPreset$data_unit)
      }
      
      if ('description' %in% names(lsPreset)) {
        updateTextAreaInput(session = session, 
                            inputId = 'description', 
                            value = lsPreset$description)
      }
      
      if ('source_of_data' %in% names(lsPreset)) {
        updateTextAreaInput(session = session, 
                            inputId = 'source_of_data', 
                            value = lsPreset$source_of_data)
      }
      
      if ('values_applied' %in% names(lsPreset)) {
        updateTextInput(session = session, 
                           inputId = 'values_applied', 
                           value = lsPreset$values_applied)
      }
      
      if ('methods_and_procedures' %in% names(lsPreset)) {
        updateTextAreaInput(session = session, 
                            inputId = 'methods_and_procedures', 
                            value = lsPreset$methods_and_procedures)
      }
      
      if ('monitoring_frequency' %in% names(lsPreset)) {
        updateTextAreaInput(session = session, 
                            inputId = 'monitoring_frequency', 
                            value = lsPreset$monitoring_frequency)
      }
      
      if ('qa_qc_procedures' %in% names(lsPreset)) {
        updateTextAreaInput(session = session, 
                            inputId = 'qa_qc_procedures', 
                            value = lsPreset$qa_qc_procedures)
      }
      
      if ('purpose' %in% names(lsPreset)) {
        updateTextAreaInput(session = session, 
                            inputId = 'purpose', 
                            value = lsPreset$purpose)
      }
      
      if ('comment' %in% names(lsPreset)) {
        updateTextAreaInput(session = session, 
                            inputId = 'comment', 
                            value = lsPreset$comment)
      }
      
      # module servers ---------------------------------------------------------
      # modSrvrs <- reactiveValues()
      
      # inputs -----------------------------------------------------------------
      
      observeEvent(input$data_parameter, handlerExpr = {
        
        resultsGood$data_parameter <- FALSE
        rvToReturn$results$data_parameter <- input$data_parameter
        
        nChar <- nchar(input$data_parameter)
        outputMsgs$data_parameter <- sprintf('%d chars. remaining', 50 - nChar)
        
        resultsGood$data_parameter <- isValidInput.text(
          x = input$data_parameter, 
          bRequired = TRUE, 
          nCharMin = 1, 
          nCharMax = 50) 
        
      })
      
      observeEvent(input$data_unit, handlerExpr = {
        
        resultsGood$data_unit <- FALSE
        rvToReturn$results$data_unit <- input$data_unit
        
        nChar <- nchar(input$data_unit)
        outputMsgs$data_unit <- sprintf('%d chars. remaining', 10 - nChar)
        
        resultsGood$data_unit <- isValidInput.text(
          x = input$data_unit, 
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
      
      observeEvent(input$source_of_data, handlerExpr = {
        
        resultsGood$source_of_data <- FALSE
        rvToReturn$results$source_of_data <- input$source_of_data
        
        nChar <- nchar(input$source_of_data)
        outputMsgs$source_of_data <- sprintf('%d chars. remaining', 1000 - nChar)
        
        resultsGood$source_of_data <- isValidInput.text(
          x = input$source_of_data, 
          bRequired = TRUE, 
          nCharMin = 10,
          nCharMax = 1000) 
        
      })
      
      observeEvent(input$values_applied, handlerExpr = {
        
        resultsGood$values_applied <- FALSE
        outputMsgs$values_applied <- ''
        
        res <- validateNumeric(input$values_applied)
        if (!res$is_valid) {
          outputMsgs$values_applied <- res$msg
          return(NULL)
        }
        
        rvToReturn$results$values_applied <- res$val
        resultsGood$values_applied <- TRUE
        
      })
      
      observeEvent(input$methods_and_procedures, handlerExpr = {
        
        resultsGood$methods_and_procedures <- FALSE
        rvToReturn$results$methods_and_procedures <- input$methods_and_procedures
        
        nChar <- nchar(input$methods_and_procedures)
        outputMsgs$methods_and_procedures <- sprintf('%d chars. remaining', 1000 - nChar)
        
        resultsGood$methods_and_procedures <- isValidInput.text(
          x = input$methods_and_procedures, 
          bRequired = TRUE, 
          nCharMin = 50,
          nCharMax = 1000) 
        
      })
      
      observeEvent(input$monitoring_frequency, handlerExpr = {
        
        resultsGood$monitoring_frequency <- FALSE
        rvToReturn$results$monitoring_frequency <- input$monitoring_frequency
        
        nChar <- nchar(input$monitoring_frequency)
        outputMsgs$monitoring_frequency <- sprintf('%d chars. remaining', 500 - nChar)
        
        resultsGood$monitoring_frequency <- isValidInput.text(
          x = input$monitoring_frequency, 
          bRequired = TRUE, 
          nCharMin = 4, 
          nCharMax = 500) 
        
      })
      
      observeEvent(input$qa_qc_procedures, handlerExpr = {
        
        resultsGood$qa_qc_procedures <- FALSE
        rvToReturn$results$qa_qc_procedures <- input$qa_qc_procedures
        
        nChar <- nchar(input$qa_qc_procedures)
        outputMsgs$qa_qc_procedures <- sprintf('%d chars. remaining', 1000 - nChar)
        
        resultsGood$qa_qc_procedures <- isValidInput.text(
          x = input$qa_qc_procedures, 
          bRequired = TRUE, 
          nCharMin = 4, 
          nCharMax = 1000) 
        
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
      
      observeEvent(input$comment, handlerExpr = {
        
        resultsGood$comment <- FALSE
        rvToReturn$results$comment <- input$comment
        
        nChar <- nchar(input$comment)
        outputMsgs$comment <- sprintf('%d chars. remaining', 1000 - nChar)
        
        resultsGood$comment <- isValidInput.text(
          x = input$comment, 
          bRequired = FALSE, 
          nCharMin = 0, 
          nCharMax = 1000) 
        
      })
      
      # outputs ----------------------------------------------------------------
      
      output$data_parameter_msg <- renderText({
        outputMsgs$data_parameter
      })
      
      output$data_unit_msg <- renderText({
        outputMsgs$data_unit
      })
      
      output$description_msg <- renderText({
        outputMsgs$description
      })
      
      output$source_of_data_msg <- renderText({
        outputMsgs$source_of_data
      })
      
      output$values_applied_msg <- renderText({
        outputMsgs$values_applied
      })
      
      output$methods_and_procedures_msg <- renderText({
        outputMsgs$methods_and_procedures
      })
      
      output$monitoring_frequency_msg <- renderText({
        outputMsgs$monitoring_frequency
      })
      
      output$qa_qc_procedures_msg <- renderText({
        outputMsgs$qa_qc_procedures
      })
      
      output$purpose_msg <- renderText({
        outputMsgs$purpose
      })
      
      output$comment_msg <- renderText({
        outputMsgs$comment
      })
      
      # return logic -----------------------------------------------------------
      
      observe({
        rvToReturn$allResultsGood <- all(unlist(reactiveValuesToList(resultsGood)))
      })
      
      return(rvToReturn)
      
    })
}
