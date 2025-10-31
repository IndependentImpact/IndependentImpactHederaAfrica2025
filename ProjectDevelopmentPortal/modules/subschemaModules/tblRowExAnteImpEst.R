
# Module for schema 'Table Row: Ex Ante Impact Estimations [v2.0.0]'.

tblRowExAnteImpEstUI <- function(id, 
                                lsPreset = NULL, 
                                hL = 4, 
                                colWidth = 12, 
                                inpWidth = DEFAULT_INP_WIDTH) {
  
  ns <- NS(id)
  
  tagList(
    
    #useShinyjs(),
    
    GtextAreaInput(
      id = ns("description_parameter_impact"), 
      title = "Impact Parameter Description", 
      helpTxt = "Description of the parameter that defines the impact to be estimated (10 chars. min.):", 
      value = lsPreset$description_parameter_impact, 
      hL = hL, 
      colWidth = colWidth #, 
      #inpWidth = inpWidth
      ),
    
    GtextInput(
      id = ns("unit_parameter_impact"), 
      title = "Unit of Measure", 
      helpTxt = "Unit of measure for the parameter that defines the impact to be estimated:", 
      value = lsPreset$unit_parameter_impact, 
      hL = hL, 
      colWidth = colWidth #, 
      #inpWidth = inpWidth
      ),
    
    fluidRow(
      column(
        width = colWidth,
        wellPanel(
          hx(x = "Monitoring Period", lvl = hL),
          helpText("Monitoring period for which the impact is estimated:"),
          dateTimePeriodInput(
            id = ns("period_monitoring"), 
            lsPreset = lsPreset$period_monitoring, 
            hL = hL+1, 
            colWidth = colWidth #, 
            #inpWidth = inpWidth
            )))),
    
    # Making use of a textInput instead of a numericInput, because the stepping
    # business and the max and min params of the numericInput don't provide what
    # we need for this field.
    GtextInput(id = ns('state_baseline'),
               title = 'Baseline State',
               helpTxt = "Estimated baseline state in the parameter's unit of measure indicated above:", 
               value = lsPreset$state_baseline,
               hL = hL,
               colWidth = colWidth #,
               #inpWidth = inpWidth
               ),
    
    # Making use of a textInput instead of a numericInput, because the stepping
    # business and the max and min params of the numericInput don't provide what
    # we need for this field.
    GtextInput(id = ns('state_project'),
               title = 'Project State',
               helpTxt = "Estimated state of the parameter due to the project activity, in the parameter's unit of measure indicated above:", 
               value = lsPreset$state_project,
               hL = hL,
               colWidth = colWidth #,
               #inpWidth = inpWidth
               ),
    
    # Making use of a textInput instead of a numericInput, because the stepping
    # business and the max and min params of the numericInput don't provide what
    # we need for this field.
    GtextInput(id = ns('leakage'),
               title = 'Leakage',
               helpTxt = "Estimated leakage, expressed in the parameter's unit of measure indicated above:",
               value = lsPreset$leakage,
               hL = hL,
               colWidth = colWidth #,
               #inpWidth = inpWidth
               ),
    
    GtextOutput(id = ns('estimate_impact'), 
                title = 'Impact Estimate', 
                hL = hL, 
                colWidth = colWidth),
    
    textOutput(outputId = ns("toInvalidInput")),
    
    br())
  
}

tblRowExAnteImpEstServer <- function(id,  
                                    lsPreset = NULL) {
  
  moduleServer(
    id = id, 
    function(input, output, session) {
      
      ns <- session$ns
      
      rvToReturn <- reactiveValues()
      rvToReturn$allResultsGood <- FALSE
      rvToReturn$results <- reactiveValues(
        description_parameter_impact = NULL,
        unit_parameter_impact = NULL,
        period_monitoring = NULL,
        state_baseline = NULL,
        state_project = NULL,
        leakage = NULL,
        estimate_impact = NULL)
      
      resultsGood <- reactiveValues()
      resultsGood$description_parameter_impact <- FALSE
      resultsGood$unit_parameter_impact <- FALSE
      resultsGood$period_monitoring <- FALSE
      resultsGood$state_baseline <- FALSE
      resultsGood$state_project <- FALSE
      resultsGood$leakage <- FALSE
      resultsGood$estimate_impact <- FALSE

      
      outputMsgs <- reactiveValues()
      outputMsgs$description_parameter_impact <- ''
      outputMsgs$unit_parameter_impact <- ''
      outputMsgs$state_baseline <- ''
      outputMsgs$state_project <- ''
      outputMsgs$leakage <- ''

      rvOther <- reactiveValues()
      
      # preset inputs ----------------------------------------------------------
      
      if ('description_parameter_impact' %in% names(lsPreset)) {
        updateTextAreaInput(session = session, 
                            inputId = 'description_parameter_impact', 
                            value = lsPreset$description_parameter_impact)
      }
      
      if ('unit_parameter_impact' %in% names(lsPreset)) {
        updateTextInput(session = session, 
                        inputId = 'unit_parameter_impact', 
                        value = lsPreset$unit_parameter_impact)
      }
      
      if ('state_baseline' %in% names(lsPreset)) {
        updateTextInput(session = session, 
                        inputId = 'state_baseline', 
                        value = lsPreset$state_baseline)
      }
      
      if ('state_project' %in% names(lsPreset)) {
        updateTextInput(session = session, 
                        inputId = 'state_project', 
                        value = lsPreset$state_project)
      }
      
      if ('leakage' %in% names(lsPreset)) {
        updateTextInput(session = session, 
                        inputId = 'leakage', 
                        value = lsPreset$leakage)
      }
      
      # module servers ---------------------------------------------------------
      modSrvrs <- reactiveValues()
      modSrvrs$mMonPer <- dateTimePeriodServer(
        id = "period_monitoring", 
        lsPreset = lsPreset$period_monitoring)
      
      # inputs -----------------------------------------------------------------
      
      observeEvent(input$description_parameter_impact, handlerExpr = {
        
        resultsGood$description_parameter_impact <- FALSE
        rvToReturn$results$description_parameter_impact <- input$description_parameter_impact
        
        nChar <- nchar(input$description_parameter_impact)
        outputMsgs$description_parameter_impact <- sprintf('%d chars. remaining', 300 - nChar)
        
        resultsGood$description_parameter_impact <- isValidInput.text(
          x = input$description_parameter_impact, 
          bRequired = TRUE, 
          nCharMin = 10, 
          nCharMax = 300) 
        
      })

      observeEvent(input$unit_parameter_impact, handlerExpr = {
        
        resultsGood$unit_parameter_impact <- FALSE
        rvToReturn$results$unit_parameter_impact <- input$unit_parameter_impact
        
        nChar <- nchar(input$unit_parameter_impact)
        outputMsgs$unit_parameter_impact <- sprintf('%d chars. remaining', 10 - nChar)
        
        resultsGood$unit_parameter_impact <- isValidInput.text(
          x = input$unit_parameter_impact, 
          bRequired = TRUE, 
          nCharMin = 1, 
          nCharMax = 10) 
        
      })
      
      # modSrvrs$mMonPer$results
      observe({
        validate(need(modSrvrs$mMonPer, message = FALSE))
        resultsGood$period_monitoring <- FALSE
        rvToReturn$results$period_monitoring <- reactiveValuesToList(modSrvrs$mMonPer$results)
        resultsGood$period_monitoring <- modSrvrs$mMonPer$allResultsGood
      })
      
      observeEvent(input$state_baseline, handlerExpr = {
        
        resultsGood$state_baseline <- FALSE
        outputMsgs$state_baseline <- ''
        
        res <- validateNumeric(input$state_baseline, min = 0)
        if (!res$is_valid) {
          outputMsgs$state_baseline <- res$msg
          return(NULL)
        }
        
        rvToReturn$results$state_baseline <- res$val
        resultsGood$state_baseline <- TRUE
        
      })

      observeEvent(input$state_project, handlerExpr = {
        
        resultsGood$state_project <- FALSE
        outputMsgs$state_project <- ''
        
        res <- validateNumeric(input$state_project, min = 0)
        if (!res$is_valid) {
          outputMsgs$state_project <- res$msg
          return(NULL)
        }
        
        rvToReturn$results$state_project <- res$val
        resultsGood$state_project <- TRUE
        
      })
      
      observeEvent(input$leakage, handlerExpr = {
        
        resultsGood$leakage <- FALSE
        outputMsgs$leakage <- ''
        
        res <- validateNumeric(input$leakage)
        if (!res$is_valid) {
          outputMsgs$leakage <- res$msg
          return(NULL)
        }
        
        rvToReturn$results$leakage <- res$val
        resultsGood$leakage <- TRUE
        
      })

      observe({
        resultsGood$estimate_impact <- FALSE
        rvToReturn$results$estimate_impact <- NULL
        
        validate(need(rvToReturn$results$state_baseline, message = FALSE))
        validate(need(rvToReturn$results$state_project, message = FALSE))
        validate(need(rvToReturn$results$leakage, message = FALSE))
        
        impact <- rvToReturn$results$state_baseline - 
          rvToReturn$results$state_project - 
            rvToReturn$results$leakage
        
        rvToReturn$results$estimate_impact <- impact
        resultsGood$estimate_impact <- TRUE
      })
      
      # outputs ----------------------------------------------------------------
      
      output$description_parameter_impact_msg <- renderText({
        outputMsgs$description_parameter_impact
      })
      
      output$unit_parameter_impact_msg <- renderText({
        outputMsgs$unit_parameter_impact
      })
      
      output$state_baseline_msg <- renderText({
        outputMsgs$state_baseline
      })
      
      output$state_project_msg <- renderText({
        outputMsgs$state_project
      })
      
      output$leakage_msg <- renderText({
        outputMsgs$leakage
      })
      
      output$estimate_impact <- renderText({
        
        validate(need(rvToReturn$results$estimate_impact, message = FALSE))
        validate(need(rvToReturn$results$unit_parameter_impact, message = FALSE))
        
        sprintf("%.3f %s", 
                rvToReturn$results$estimate_impact, 
                rvToReturn$results$unit_parameter_impact)
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
