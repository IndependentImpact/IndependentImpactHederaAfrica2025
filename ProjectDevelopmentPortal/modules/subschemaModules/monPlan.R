
# Module for schema 'Monitoring Plan [v1.0.0]'.

monPlanUI <- function(id, 
                      lsPreset = NULL, 
                      hL = 4, 
                      colWidth = 12, 
                      inpWidth = DEFAULT_INP_WIDTH) {
  
  ns <- NS(id)
  
  tagList(
    
    #useShinyjs(),

    fluidRow(
      column(
        width = colWidth,
        wellPanel(
          hx("Monitored Data and Parameters", lvl = hL),
          helpText('Data and parameters to be monitored.'),
          dynamicMultiInput(id = ns('datapar_monitored')),
          textOutput(outputId = ns("datapar_monitored_msg"))))),
    
    GtextAreaInput(id = ns('sampling_plan'),
                   title = 'Sampling plan',
                   helpTxt = 'Sampling plan. (50 chars. min.)',
                   value = lsPreset$sampling_plan,
                   resize = 'vertical',
                   hL = hL,
                   colWidth = colWidth,
                   inpWidth = inpWidth),
    
    GtextAreaInput(id = ns('other_elements'),
                   title = 'Other elements of monitoring plan',
                   helpTxt = 'Other elements of monitoring plan.',
                   value = lsPreset$other_elements,
                   resize = 'vertical',
                   hL = hL,
                   colWidth = colWidth,
                   inpWidth = inpWidth))
}

monPlanServer <- function(id,  
                          lsPreset = NULL) {
  
  moduleServer(
    id = id, 
    function(input, output, session) {
      
      ns <- session$ns
      
      rvToReturn <- reactiveValues()
      rvToReturn$allResultsGood <- FALSE
      rvToReturn$results <- reactiveValues(
        datapar_monitored = NULL,
        sampling_plan = NULL,
        other_elements = NULL)
      
      resultsGood <- reactiveValues()
      resultsGood$datapar_monitored <- FALSE
      resultsGood$sampling_plan <- FALSE
      resultsGood$other_elements <- FALSE
      
      outputMsgs <- reactiveValues()
      outputMsgs$datapar_monitored <- ''
      outputMsgs$sampling_plan <- ''
      outputMsgs$other_elements <- ''
      
      rvOther <- reactiveValues()
      
      # preset inputs ----------------------------------------------------------

      if ('sampling_plan' %in% names(lsPreset)) {
        updateTextAreaInput(session = session, 
                            inputId = 'sampling_plan', 
                            value = lsPreset$sampling_plan)
      }
      
      if ('other_elements' %in% names(lsPreset)) {
        updateTextAreaInput(session = session, 
                            inputId = 'other_elements', 
                            value = lsPreset$other_elements)
      }
      
      # module servers ---------------------------------------------------------
      
      modSrvrs <- reactiveValues()
      
      modSrvrs$datapar_monitored <- dynamicMultiServer(
        id = 'datapar_monitored', 
        nmUImod = 'tblRowDataParamMonUI', 
        nmSrvrMod = 'tblRowDataParamMonServer', 
        btns = c('edit', 'remove'), 
        lsArgsModUI = list(hL = 5, colWidth = 12, inpWidth = DEFAULT_INP_WIDTH),
        lsArgsModSrvr = list(lsPreset = lsPreset$datapar_monitored))
      
      # inputs -----------------------------------------------------------------
      
      observeEvent(modSrvrs$datapar_monitored$items, handlerExpr = {
        
        resultsGood$datapar_monitored <- FALSE
        outputMsgs$datapar_monitored <- ''
        rvToReturn$results$datapar_monitored <- modSrvrs$datapar_monitored$items
        resultsGood$datapar_monitored <- length(modSrvrs$datapar_monitored$items) > 0
        if (!resultsGood$datapar_monitored) {
          outputMsgs$datapar_monitored <- '*Required.'
        }
        
      })
      
      observeEvent(input$sampling_plan, handlerExpr = {
        
        resultsGood$sampling_plan <- FALSE
        rvToReturn$results$sampling_plan <- input$sampling_plan
        
        nChar <- nchar(input$sampling_plan)
        outputMsgs$sampling_plan <- sprintf('%d chars. remaining', 1000 - nChar)
        
        resultsGood$sampling_plan <- isValidInput.text(
          x = input$sampling_plan, 
          bRequired = TRUE, 
          nCharMin = 50, 
          nCharMax = 1000) 
        
      })
      
      observeEvent(input$other_elements, handlerExpr = {
        
        resultsGood$other_elements <- FALSE
        rvToReturn$results$other_elements <- input$other_elements
        
        nChar <- nchar(input$other_elements)
        outputMsgs$other_elements <- sprintf('%d chars. remaining', 1000 - nChar)
        
        resultsGood$other_elements <- isValidInput.text(
          x = input$other_elements, 
          bRequired = FALSE, 
          nCharMin = 0, 
          nCharMax = 1000) 
        
      })
      
      # outputs ----------------------------------------------------------------
      
      output$datapar_monitored_msg <- renderText({
        outputMsgs$datapar_monitored
      })
      
      output$sampling_plan_msg <- renderText({
        outputMsgs$sampling_plan
      })
      
      output$other_elements_msg <- renderText({
        outputMsgs$other_elements
      })
      
      # return logic -----------------------------------------------------------
      
      observe({
        rvToReturn$allResultsGood <- all(unlist(reactiveValuesToList(resultsGood)))
      })
      
      return(rvToReturn)
      
    })
}
