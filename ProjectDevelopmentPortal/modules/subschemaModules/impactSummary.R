# Impact Summary schema [v1.0.0]


# Module for schema 'Impact Summary' (IS) [v1.0.0].

impactSummaryInput <- function(id, 
                               lsPreset = NULL, 
                               hL = 4, 
                               colWidth = 12, 
                               inpWidth = DEFAULT_INP_WIDTH) {
  
  ns <- NS(id)
  
  tagList(
    
    #useShinyjs(),
    
    GtextInput(
      id = ns("description_impact"), 
      title = "Impact Description", 
      helpTxt = "Description of impact (15 chars. min.):", 
      value = lsPreset$description_impact, 
      hL = hL, 
      colWidth = colWidth, 
      inpWidth = inpWidth),
    
    wellPanel(
      dateTimePeriodInput(
        id = ns("period_impact"), 
        title = "Impact Period",
        helpTxt = "Period during which the impact occurred:",
        lsPreset = lsPreset$period_impact, 
        hL = hL, 
        colWidth = colWidth, 
        inpWidth = inpWidth)),
    
    GtextInput(
      id = ns("unit_impact"), 
      title = "Impact Unit", 
      helpTxt = "Unit of measure for impact (e.g., 'kilogramme' or 'metric tonne'):", 
      value = lsPreset$unit_impact, 
      hL = hL, 
      colWidth = colWidth, 
      inpWidth = inpWidth),
    
    GtextInput(
      id = ns("extent_impact"), # TODO: This should really be "magnitude", not "extent".
      title = "Impact Extent", 
      helpTxt = "Extent of impact, as a quantity in the unit of impact:", 
      value = lsPreset$extent_impact, 
      hL = hL, 
      colWidth = colWidth, 
      inpWidth = inpWidth),
    
    textOutput(outputId = ns("toInvalidInput")),
    
    br()
  )
}

impactSummaryServer <- function(id, lsPreset = NULL) {
  
  moduleServer(
    id = id, 
    function(input, output, session) {
      
      ns <- session$ns
      
      rvToReturn <- reactiveValues()
      rvToReturn$goToModule <- NULL
      rvToReturn$allResultsGood <- FALSE
      rvToReturn$results <- reactiveValues(
        description_impact = NULL,
        period_impact = NULL,
        unit_impact = NULL,
        extent_impact = NULL)
      
      resultsGood <- reactiveValues()
      resultsGood$description_impact <- FALSE
      resultsGood$period_impact <- FALSE
      resultsGood$unit_impact <- FALSE
      resultsGood$extent_impact <- FALSE
      
      outputMsgs <- reactiveValues()
      outputMsgs$description_impact <- ""
      outputMsgs$period_impact <- ""
      outputMsgs$unit_impact <- ""
      outputMsgs$extent_impact <- ""
      
      # preset inputs ----------------------------------------------------------
      
      if ("description_impact" %in% names(lsPreset)) {
        updateTextInput(session = session, 
                        inputId = "description_impact", 
                        value = lsPreset$description_impact)
      }
      
      if ("period_impact" %in% names(lsPreset)) {
        updateTextInput(session = session, 
                        inputId = "period_impact", 
                        value = lsPreset$period_impact)
      }
      
      if ("unit_impact" %in% names(lsPreset)) {
        updateTextInput(session = session, 
                        inputId = "unit_impact", 
                        value = lsPreset$unit_impact)
      }
      
      if ("extent_impact" %in% names(lsPreset)) {
        updateTextInput(session = session, 
                        inputId = "extent_impact", 
                        value = lsPreset$extent_impact)
      }
      
      # module servers ---------------------------------------------------------
      
      modSrvrs <- reactiveValues()
      modSrvrs$period_impact <- dateTimePeriodServer(
        id = "period_impact", 
        lsPreset = lsPreset$period_impact)
      
      # inputs -----------------------------------------------------------------
      
      observeEvent(input$description_impact, handlerExpr = {
        
        resultsGood$description_impact <- FALSE
        rvToReturn$results$description_impact <- input$description_impact
        
        nChar <- nchar(input$description_impact)
        outputMsgs$description_impact <- sprintf("%d chars. remaining", 100 - nChar)
        
        resultsGood$description_impact <- isValidInput.text(
          x = input$description_impact, 
          bRequired = TRUE, 
          nCharMin = 15, 
          nCharMax = 100)
        
      })
      
      observe({
        rvToReturn$results$period_impact <- modSrvrs$period_impact$results
        resultsGood$period_impact <- modSrvrs$period_impact$allResultsGood
      })
      
      observeEvent(input$unit_impact, handlerExpr = {
        
        resultsGood$unit_impact <- FALSE
        rvToReturn$results$unit_impact <- input$unit_impact
        
        nChar <- nchar(input$unit_impact)
        outputMsgs$unit_impact <- sprintf("%d chars. remaining", 20 - nChar)
        
        resultsGood$unit_impact <- isValidInput.text(
          x = input$unit_impact, 
          bRequired = TRUE, 
          nCharMin = 1, 
          nCharMax = 20)
        
      })
      
      observeEvent(input$extent_impact, handlerExpr = {
        
        resultsGood$extent_impact <- FALSE
        outputMsgs$extent_impact <- ""
        rvToReturn$results$extent_impact <- input$extent_impact
        
        validationRes <- validateNumeric(val = input$extent_impact)
        if (!validationRes$is_valid) {
          outputMsgs$extent_impact <- validationRes$msg
        } else {
          rvToReturn$results$extent_impact <- validationRes$val
          resultsGood$extent_impact <- TRUE
        }
        
      })
      
      
      
      # outputs-----------------------------------------------------------------
      
      output$description_impact_msg <- renderText({
        outputMsgs$description_impact
      })
      
      output$period_impact_msg <- renderText({
        outputMsgs$period_impact
      })
      
      output$unit_impact_msg <- renderText({
        outputMsgs$unit_impact
      })
      
      output$extent_impact_msg <- renderText({
        outputMsgs$extent_impact
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

