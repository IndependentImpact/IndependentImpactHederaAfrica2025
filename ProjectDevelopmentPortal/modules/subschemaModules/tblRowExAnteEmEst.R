
# Module for schema 'Table Row: Ex Ante Emissions Estimations [v1.0.0]'.

tblRowExAnteEmEstUI <- function(id, 
                                lsPreset = NULL, 
                                hL = 4, 
                                colWidth = 12, 
                                inpWidth = DEFAULT_INP_WIDTH) {
  
  ns <- NS(id)
  
  tagList(
    
    #useShinyjs(),
    
    GnumericInput(id = ns('year'),
                  title = 'Year',
                  helpTxt = 'Year.',
                  value = lsPreset$year,
                  step = 1,
                  hL = hL,
                  colWidth = colWidth,
                  inpWidth = inpWidth),
    
    # Making use of a textInput instead of a numericInput, because the stepping
    # business and the max and min params of the numericInput don't provide what
    # we need for this field.
    GtextInput(id = ns('emissions_baseline'),
               title = 'Baseline Emissions',
               helpTxt = 'Estimated baseline emissions (tCO2e).', 
               value = lsPreset$emissions_baseline,
               hL = hL,
               colWidth = colWidth,
               inpWidth = inpWidth),
    
    # Making use of a textInput instead of a numericInput, because the stepping
    # business and the max and min params of the numericInput don't provide what
    # we need for this field.
    GtextInput(id = ns('emissions_project'),
               title = 'Project Emissions',
               helpTxt = 'Estimated project emissions (tCO2e).', 
               value = lsPreset$emissions_project,
               hL = hL,
               colWidth = colWidth,
               inpWidth = inpWidth),
    
    # Making use of a textInput instead of a numericInput, because the stepping
    # business and the max and min params of the numericInput don't provide what
    # we need for this field.
    GtextInput(id = ns('emissions_leakage'),
               title = 'Leakage',
               helpTxt = 'Estimated leakage emissions (tCO2e).',
               value = lsPreset$emissions_leakage,
               hL = hL,
               colWidth = colWidth,
               inpWidth = inpWidth),
    
    GtextOutput(id = ns('emissions_reductions'), 
                title = 'Estimated emission reductions', 
                hL = hL, 
                colWidth = colWidth))
  
}

tblRowExAnteEmEstServer <- function(id,  
                                    lsPreset = NULL) {
  
  moduleServer(
    id = id, 
    function(input, output, session) {
      
      ns <- session$ns
      
      rvToReturn <- reactiveValues()
      rvToReturn$allResultsGood <- FALSE
      rvToReturn$results <- reactiveValues(
        year = NULL,
        emissions_baseline = NULL,
        emissions_project = NULL,
        emissions_leakage = NULL,
        emissions_reductions = NULL)
      
      resultsGood <- reactiveValues()
      resultsGood$year <- FALSE
      resultsGood$emissions_baseline <- FALSE
      resultsGood$emissions_project <- FALSE
      resultsGood$emissions_leakage <- FALSE
      resultsGood$emissions_reductions <- FALSE
      
      outputMsgs <- reactiveValues()
      outputMsgs$year <- ''
      outputMsgs$emissions_baseline <- ''
      outputMsgs$emissions_project <- ''
      outputMsgs$emissions_leakage <- ''
      
      rvOther <- reactiveValues()
      
      # preset inputs ----------------------------------------------------------
      
      if ('year' %in% names(lsPreset)) {
        updateNumericInput(session = session, 
                           inputId = 'year', 
                           value = lsPreset$year)
      }
      
      if ('emissions_baseline' %in% names(lsPreset)) {
        updateTextInput(session = session, 
                        inputId = 'emissions_baseline', 
                        value = lsPreset$emissions_baseline)
      }
      
      if ('emissions_project' %in% names(lsPreset)) {
        updateTextInput(session = session, 
                        inputId = 'emissions_project', 
                        value = lsPreset$emissions_project)
      }
      
      if ('emissions_leakage' %in% names(lsPreset)) {
        updateTextInput(session = session, 
                        inputId = 'emissions_leakage', 
                        value = lsPreset$emissions_leakage)
      }
      
      # module servers ---------------------------------------------------------
      #modSrvrs <- reactiveValues()
      
      # inputs -----------------------------------------------------------------
      
      observeEvent(input$year, handlerExpr = {
        
        resultsGood$year <- FALSE
        rvToReturn$results$year <- input$year
        outputMsgs$year <- ''
        
        validate(need(input$year, message = FALSE))
        
        if (input$year < lubridate::year(Sys.time())) {
          outputMsgs$year <- "Warning: The value you've entered refers to a year in the past. Are you sure the value is correct? If you are, you can safely ignore this message."
        }
        
        if (input$year > (lubridate::year(Sys.time()) + 10)) {
          outputMsgs$year <- "Warning: The value you've entered refers to a year well off into the future. Are you sure the value is correct? If you are, you can safely ignore this message."
        }
        
        resultsGood$year <- TRUE
        
      })

      observeEvent(input$emissions_baseline, handlerExpr = {
        
        resultsGood$emissions_baseline <- FALSE
        outputMsgs$emissions_baseline <- ''
        
        res <- validateNumeric(input$emissions_baseline, min = 0)
        if (!res$is_valid) {
          outputMsgs$emissions_baseline <- res$msg
          return(NULL)
        }
        
        rvToReturn$results$emissions_baseline <- res$val
        resultsGood$emissions_baseline <- TRUE
        
      })

      observeEvent(input$emissions_project, handlerExpr = {
        
        resultsGood$emissions_project <- FALSE
        outputMsgs$emissions_project <- ''
        
        res <- validateNumeric(input$emissions_project, min = 0)
        if (!res$is_valid) {
          outputMsgs$emissions_project <- res$msg
          return(NULL)
        }
        
        rvToReturn$results$emissions_project <- res$val
        resultsGood$emissions_project <- TRUE
        
      })
      
      observeEvent(input$emissions_leakage, handlerExpr = {
        
        resultsGood$emissions_leakage <- FALSE
        outputMsgs$emissions_leakage <- ''
        
        res <- validateNumeric(input$emissions_leakage)
        if (!res$is_valid) {
          outputMsgs$emissions_leakage <- res$msg
          return(NULL)
        }
        
        rvToReturn$results$emissions_leakage <- res$val
        resultsGood$emissions_leakage <- TRUE
        
      })

      observe({
        resultsGood$emissions_reductions <- FALSE
        rvToReturn$results$emissions_reductions <- NULL
        
        validate(need(rvToReturn$results$emissions_baseline, message = FALSE))
        validate(need(rvToReturn$results$emissions_project, message = FALSE))
        validate(need(rvToReturn$results$emissions_leakage, message = FALSE))
        
        ERs <- rvToReturn$results$emissions_baseline - 
          rvToReturn$results$emissions_project - 
            rvToReturn$results$emissions_leakage
        
        rvToReturn$results$emissions_reductions <- ERs
        resultsGood$emissions_reductions <- TRUE
      })
      
      # outputs ----------------------------------------------------------------
      
      output$year_msg <- renderText({
        outputMsgs$year
      })
      
      output$emissions_baseline_msg <- renderText({
        outputMsgs$emissions_baseline
      })
      
      output$emissions_project_msg <- renderText({
        outputMsgs$emissions_project
      })
      
      output$emissions_leakage_msg <- renderText({
        outputMsgs$emissions_leakage
      })
      
      output$emissions_reductions <- renderText({
        validate(need(rvToReturn$results$emissions_reductions, message = FALSE))
        sprintf("%.3f tCO2e", rvToReturn$results$emissions_reductions)
      })
      
      # return logic -----------------------------------------------------------
      
      observe({
        rvToReturn$allResultsGood <- all(unlist(reactiveValuesToList(resultsGood)))
      })
      
      return(rvToReturn)
      
    })
}
