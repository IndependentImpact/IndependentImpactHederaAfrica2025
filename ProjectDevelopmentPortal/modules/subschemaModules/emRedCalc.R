
# Module for schema 'Emission Reduction Calculation [v1.0.0]'.

emRedCalcUI <- function(id, 
                        lsPreset = NULL, 
                        hL = 4, 
                        colWidth = 12, 
                        inpWidth = DEFAULT_INP_WIDTH) {
  
  ns <- NS(id)
  
  tagList(
    
    #useShinyjs(),
    
    GtextAreaInput(id = ns('explanation_methchoices'),
                   title = 'Explanation of Methodological Choices',
                   helpTxt = 'Explanation of methodological choices. (30 chars. min.)',
                   value = lsPreset$explanation_methchoices,
                   resize = 'vertical',
                   hL = hL,
                   colWidth = colWidth,
                   inpWidth = inpWidth),
    
    fluidRow(
      column(
        width = colWidth, 
        wellPanel(
          hx(x = 'Data and Parameters Fixed Ex Ante', lvl = hL),
          helpText('Data and parameters fixed ex ante.'),
          dynamicMultiInput(id = ns('dataparams_exante'))))),
    
    fluidRow(
      column(
        width = colWidth, 
        wellPanel(
          hx(x = 'Ex Ante Calculation of ERs', lvl = hL),
          helpText('Ex ante calculation of emission reductions by year.'),
          dynamicMultiInput(id = ns('calculation_exante'))))),
    
    GtextAreaInput(id = ns('summary_exante'),
                   title = 'Summary of Ex Ante Estimates of ERs',
                   helpTxt = 'Summary of ex ante estimates of emission reductions. (50 chars. min.)',
                   value = lsPreset$summary_exante,
                   resize = 'vertical',
                   hL = hL,
                   colWidth = colWidth,
                   inpWidth = inpWidth))
}

emRedCalcServer <- function(id,  
                            lsPreset = NULL) {
  
  moduleServer(
    id = id, 
    function(input, output, session) {
      
      ns <- session$ns
      
      rvToReturn <- reactiveValues()
      rvToReturn$allResultsGood <- FALSE
      rvToReturn$results <- reactiveValues(
        explanation_methchoices = NULL,
        dataparams_exante = NULL,
        calculation_exante = NULL,
        summary_exante = NULL)
      
      resultsGood <- reactiveValues()
      resultsGood$explanation_methchoices <- FALSE
      resultsGood$dataparams_exante <- FALSE
      resultsGood$calculation_exante <- FALSE
      resultsGood$summary_exante <- FALSE
      
      outputMsgs <- reactiveValues()
      outputMsgs$explanation_methchoices <- ''
      outputMsgs$dataparams_exante <- ''
      outputMsgs$calculation_exante <- ''
      outputMsgs$summary_exante <- ''
      
      rvOther <- reactiveValues()
      
      # preset inputs ----------------------------------------------------------
      
      if ('explanation_methchoices' %in% names(lsPreset)) {
        updateTextAreaInput(session = session, 
                            inputId = 'explanation_methchoices', 
                            value = lsPreset$explanation_methchoices)
      }
      
      if ('summary_exante' %in% names(lsPreset)) {
        updateTextAreaInput(session = session, 
                            inputId = 'summary_exante', 
                            value = lsPreset$summary_exante)
      }
      
      # module servers ---------------------------------------------------------
      
      modSrvrs <- reactiveValues()
      
      modSrvrs$dataparams_exante <- dynamicMultiServer(
        id = 'dataparams_exante', 
        nmUImod = 'tblRowDataParamMonUI', 
        nmSrvrMod = 'tblRowDataParamMonServer', 
        btns = c('edit', 'remove'), 
        lsArgsModUI = list(hL = 5, colWidth = 12, inpWidth = DEFAULT_INP_WIDTH),
        lsArgsModSrvr = list(lsPreset = lsPreset$dataparams_exante))
      
      modSrvrs$calculation_exante <- dynamicMultiServer(
        id = 'calculation_exante', 
        nmUImod = 'tblRowExAnteEmEstUI', 
        nmSrvrMod = 'tblRowExAnteEmEstServer', 
        btns = c('edit', 'remove'), 
        lsArgsModUI = list(hL = 5, colWidth = 12, inpWidth = DEFAULT_INP_WIDTH),
        lsArgsModSrvr = list(lsPreset = lsPreset$calculation_exante))
      
      # inputs -----------------------------------------------------------------
      
      observeEvent(input$explanation_methchoices, handlerExpr = {
        
        resultsGood$explanation_methchoices <- FALSE
        rvToReturn$results$explanation_methchoices <- input$explanation_methchoices
        
        nChar <- nchar(input$explanation_methchoices)
        outputMsgs$explanation_methchoices <- sprintf('%d chars. remaining', 1000 - nChar)
        
        resultsGood$explanation_methchoices <- isValidInput.text(
          x = input$explanation_methchoices, 
          bRequired = TRUE, 
          nCharMin = 30, 
          nCharMax = 1000) 
        
      })
      
      observeEvent(modSrvrs$dataparams_exante$items, handlerExpr = {
        
        resultsGood$dataparams_exante <- FALSE
        outputMsgs$dataparams_exante <- ''
        rvToReturn$results$dataparams_exante <- modSrvrs$dataparams_exante$items
        resultsGood$dataparams_exante <- length(modSrvrs$dataparams_exante$items) > 0
        if (!resultsGood$dataparams_exante) {
          outputMsgs$dataparams_exante <- '*Required.'
        }
        
      })
      
      observeEvent(modSrvrs$calculation_exante$items, handlerExpr = {
        
        resultsGood$calculation_exante <- FALSE
        outputMsgs$calculation_exante <- ''
        rvToReturn$results$calculation_exante <- modSrvrs$calculation_exante$items
        resultsGood$calculation_exante <- length(modSrvrs$calculation_exante$items) > 0
        if (!resultsGood$calculation_exante) {
          outputMsgs$calculation_exante <- '*Required.'
        }
        
      })
      
      observeEvent(input$summary_exante, handlerExpr = {
        
        resultsGood$summary_exante <- FALSE
        rvToReturn$results$summary_exante <- input$summary_exante
        
        nChar <- nchar(input$summary_exante)
        outputMsgs$summary_exante <- sprintf('%d chars. remaining', 1000 - nChar)
        
        resultsGood$summary_exante <- isValidInput.text(
          x = input$summary_exante, 
          bRequired = TRUE, 
          nCharMin = 50, 
          nCharMax = 1000) 
        
      })
      
      # outputs ----------------------------------------------------------------
      
      output$explanation_methchoices_msg <- renderText({
        outputMsgs$explanation_methchoices
      })
      
      output$dataparams_exante_msg <- renderText({
        outputMsgs$dataparams_exante
      })
      
      output$calculation_exante_msg <- renderText({
        outputMsgs$calculation_exante
      })
      
      output$summary_exante_msg <- renderText({
        outputMsgs$summary_exante
      })
      
      # return logic -----------------------------------------------------------
      
      observe({
        rvToReturn$allResultsGood <- all(unlist(reactiveValuesToList(resultsGood)))
      })
      
      return(rvToReturn)
      
    })
}
