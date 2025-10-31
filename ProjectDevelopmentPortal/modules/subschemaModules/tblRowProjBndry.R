
tblRowProjBndryUI <- function(id, 
                              lsPreset = NULL, 
                              hL = 4, 
                              colWidth = 12, 
                              inpWidth = DEFAULT_INP_WIDTH) {
  
  ns <- NS(id)
  
  tagList(
    
    #useShinyjs(),
    
    GtextInput(id = ns('id_source'),
               title = 'Source ID',
               helpTxt = 'Source ID (5 chars. min.):', 
               value = lsPreset$id_source,
               hL = hL,
               colWidth = colWidth,
               inpWidth = inpWidth),
    
    # GselectInput(id = ns('ghg'),
    #              title = 'Greenhouse Gas',
    #              helpTxt = 'Greenhouse gas.',
    #              choices = c('CO2', 'CH4', 'NO2'),
    #              selected = lsPreset$ghg,
    #              hL = hL,
    #              colWidth = colWidth,
    #              inpWidth = inpWidth),
    
    # fluidRow(
    #   column(
    #     width = colWidth,
    #     wellPanel(
    #       hx(x = 'Greenhouse Gas', lvl = hL),
    #       helpText('Greenhouse gas.'),
    #       selectInput(
    #         inputId = ns('ghg'), 
    #         label = NULL, 
    #         choices = c('CO2', 'CH4', 'NO2'), 
    #         selected = lsPreset$ghg, 
    #         multiple = FALSE, 
    #         selectize = TRUE, 
    #         width = inpWidth, 
    #         size = NULL)))),
    
    GtextInput(id = ns("description_source"), 
               title = "Description", 
               helpTxt = "A description of the source of impact (10 chars. min.):", 
               value = lsPreset$description, 
               hL = hL, 
               colWidth = colWidth, 
               inpWidth = inpWidth),
    
    # GselectInput(id = ns('included'),
    #              title = 'Included',
    #              helpTxt = 'Included?',
    #              choices = c('INCLUDED', 'EXCLUDED'),
    #              selected = lsPreset$included,
    #              hL = hL,
    #              colWidth = colWidth,
    #              inpWidth = inpWidth),
    
    fluidRow(
      column(
        width = colWidth,
        wellPanel(
          hx(x = 'Beneficial or Adverse', lvl = hL),
          helpText('Does the source have a beneficial or an adverse impact on the environment within the project boundary?'),
          selectInput(
            inputId = ns('beneficial_or_adverse'), 
            label = NULL, 
            choices = c('BENEFICIAL', 'ADVERSE'), 
            selected = lsPreset$beneficial_or_adverse, 
            multiple = FALSE, 
            selectize = TRUE, 
            width = inpWidth, 
            size = NULL)))),    
    
    fluidRow(
      column(
        width = colWidth,
        wellPanel(
          hx(x = 'Included', lvl = hL),
          helpText('Is the source included in or excluded from the project boundary?'),
          selectInput(
            inputId = ns('included'), 
            label = NULL, 
            choices = c('INCLUDED', 'EXCLUDED'), 
            selected = lsPreset$included, 
            multiple = FALSE, 
            selectize = TRUE, 
            width = inpWidth, 
            size = NULL)))),
    
    GtextAreaInput(id = ns('justification_explanation'),
                   title = 'Justification/Explanation',
                   helpTxt = 'Justification or explanation. (50 chars. min.)',
                   value = lsPreset$justification_explanation,
                   resize = 'vertical',
                   hL = hL,
                   colWidth = colWidth,
                   inpWidth = inpWidth),
    
    textOutput(outputId = ns("toInvalidInput")),
    
    br())
}

tblRowProjBndryServer <- function(id,  
                                  lsPreset = NULL) {
  
  moduleServer(
    id = id, 
    function(input, output, session) {
      
      ns <- session$ns
      
      rvToReturn <- reactiveValues()
      rvToReturn$allResultsGood <- FALSE
      rvToReturn$results <- reactiveValues(
        id_source = NULL,
        description_source = NULL,
        beneficial_or_adverse = NULL,
        #ghg = NULL,
        included = NULL,
        justification_explanation = NULL)
      
      resultsGood <- reactiveValues()
      resultsGood$id_source <- FALSE
      #resultsGood$ghg <- FALSE
      resultsGood$description_source <- FALSE
      resultsGood$beneficial_or_adverse <- FALSE
      resultsGood$included <- FALSE
      resultsGood$justification_explanation <- FALSE
      
      outputMsgs <- reactiveValues()
      outputMsgs$id_source <- ''
      outputMsgs$description_source <- ''
      outputMsgs$beneficial_or_adverse <- ''
      #outputMsgs$ghg <- ''
      outputMsgs$included <- ''
      outputMsgs$justification_explanation <- ''
      
      rvOther <- reactiveValues()
      
      # preset inputs ----------------------------------------------------------
      
      if ('id_source' %in% names(lsPreset)) {
        updateTextInput(session = session, 
                        inputId = 'id_source', 
                        value = lsPreset$id_source)
      }
      
      if ('description_source' %in% names(lsPreset)) {
        updateTextInput(session = session, 
                        inputId = 'description_source', 
                        value = lsPreset$description_source)
      }
      
      if ('beneficial_or_adverse' %in% names(lsPreset)) {
        updateTextInput(session = session, 
                        inputId = 'beneficial_or_adverse', 
                        value = lsPreset$beneficial_or_adverse)
      }
      
      # if ('ghg' %in% names(lsPreset)) {
      #   updateSelectInput(session = session, 
      #                     inputId = 'ghg', 
      #                     value = lsPreset$ghg)
      # }
      
      if ('included' %in% names(lsPreset)) {
        updateSelectInput(session = session, 
                          inputId = 'included', 
                          value = lsPreset$included)
      }
      
      if ('justification_explanation' %in% names(lsPreset)) {
        updateTextAreaInput(session = session, 
                            inputId = 'justification_explanation', 
                            value = lsPreset$justification_explanation)
      }
      
      # module servers ---------------------------------------------------------
      
      # None.
      
      # inputs -----------------------------------------------------------------
      
      observeEvent(input$id_source, handlerExpr = {
        
        resultsGood$id_source <- FALSE
        rvToReturn$results$id_source <- input$id_source
        
        nChar <- nchar(input$id_source)
        outputMsgs$id_source <- sprintf('%d chars. remaining', 50 - nChar)
        
        resultsGood$id_source <- isValidInput.text(
          x = input$id_source, 
          bRequired = TRUE, 
          nCharMin = 5, 
          nCharMax = 50) 
        
      })

      observeEvent(input$description_source, handlerExpr = {
        
        resultsGood$description_source <- FALSE
        rvToReturn$results$description_source <- input$description_source
        
        nChar <- nchar(input$description_source)
        outputMsgs$description_source <- sprintf('%d chars. remaining', 300 - nChar)
        
        resultsGood$description_source <- isValidInput.text(
          x = input$description_source, 
          bRequired = TRUE, 
          nCharMin = 10, 
          nCharMax = 300) 
        
      })
      
      # observeEvent(input$ghg, handlerExpr = {
      #   
      #   resultsGood$ghg <- FALSE
      #   outputMsgs$ghg <- ''
      #   rvToReturn$results$ghg <- input$ghg
      #   validate(need(input$ghg, message = FALSE))
      #   resultsGood$ghg <- TRUE
      #   
      # })
 
      observeEvent(input$beneficial_or_adverse, handlerExpr = {
        
        resultsGood$beneficial_or_adverse <- FALSE
        outputMsgs$beneficial_or_adverse <- ''
        rvToReturn$results$beneficial_or_adverse <- input$beneficial_or_adverse
        validate(need(input$beneficial_or_adverse, message = FALSE))
        resultsGood$beneficial_or_adverse <- TRUE
        
      })
      
      observeEvent(input$included, handlerExpr = {
        
        resultsGood$included <- FALSE
        outputMsgs$included <- ''
        rvToReturn$results$included <- input$included
        validate(need(input$included, message = FALSE))
        resultsGood$included <- TRUE
        
      })
      
      observeEvent(input$justification_explanation, handlerExpr = {
        
        resultsGood$justification_explanation <- FALSE
        rvToReturn$results$justification_explanation <- input$justification_explanation
        
        nChar <- nchar(input$justification_explanation)
        outputMsgs$justification_explanation <- sprintf('%d chars. remaining', 1000 - nChar)
        
        resultsGood$justification_explanation <- isValidInput.text(
          x = input$justification_explanation, 
          bRequired = TRUE, 
          nCharMin = 50, 
          nCharMax = 1000) 
        
      })
      
      # outputs ----------------------------------------------------------------
      
      output$id_source_msg <- renderText({
        outputMsgs$id_source
      })
      
      output$description_source_msg <- renderText({
        outputMsgs$description_source
      })
      
      output$beneficial_or_adverse_msg <- renderText({
        outputMsgs$beneficial_or_adverse
      })
      
      # output$ghg_msg <- renderText({
      #   outputMsgs$ghg
      # })
      
      output$included_msg <- renderText({
        outputMsgs$included
      })
      
      output$justification_explanation_msg <- renderText({
        outputMsgs$justification_explanation
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
