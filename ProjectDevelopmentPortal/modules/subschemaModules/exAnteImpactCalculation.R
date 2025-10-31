
exAnteImpactCalculationUI <- function(id,
                                      hL = 4,
                                      colWidth = 12,
                                      inpWidth = DEFAULT_INP_WIDTH,
                                      lsPreset = NULL) {
  
  ns <- NS(id)
  
  tagList(
    
    #useShinyjs(),
    
    GtextAreaInput(id = ns('explanation_methchoices'),
                   title = 'Explanation of Methodological Choices',
                   helpTxt = "Explanation of methodological choices (50 chars. min.):",
                   value = lsPreset$explanation_methchoices,
                   resize = 'vertical',
                   hL = hL,
                   colWidth = colWidth,
                   inpWidth = inpWidth),
    
    fluidRow(
      column(
        width = colWidth,
        wellPanel(
          hx(x = "Data and Parameters Fixed Ex-Ante", lvl = hL),
          helpText("Data and parameters fixed ex-ante:"),
          dynamicMultiInput(id = ns("dataparams_exante"))))),
    
    fluidRow(
      column(
        width = colWidth,
        wellPanel(
          hx(x = "Ex-Ante Estimation of Impacts", lvl = hL),
          helpText("Ex-ante estimation for each impact parameter and monitoring period:"),
          dynamicMultiInput(id = ns("calculation_exante"))))),
    
    GtextOutput(id = ns("summary_exante"), 
                title = 'Summary of Ex-Ante Impact Estimates', 
                subTxt =  "Summary of ex-ante estimates of impacts:", 
                hL = hL, 
                colWidth = colWidth),
    
    textOutput(outputId = ns("toInvalidInput")),
    
    br())
}

exAnteImpactCalculationServer <- function(id,
                                          hL = 4,
                                          colWidth = 12,
                                          inpWidth = DEFAULT_INP_WIDTH, 
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
      
      # preset inputs ----------------------------------------------------------
      
      if ('explanation_methchoices' %in% names(lsPreset)) {
        updateTextInput(session = session, 
                        inputId = 'explanation_methchoices', 
                        value = lsPreset$explanation_methchoices)
      }
      
      # module servers ---------------------------------------------------------
      
      modSrvrs <- reactiveValues()
      
      modSrvrs$dataparams_exante <- dynamicMultiServer(
        id = "dataparams_exante", 
        nmUImod = "tblRowDataParamMonUI", 
        nmSrvrMod = "tblRowDataParamMonServer", 
        lsArgsModUI = list(hL = hL, colWidth = colWidth, inpWidth = inpWidth), 
        lsArgsModSrvr = list(lsPreset = lsPreset$dataparams_exante))
      
      modSrvrs$calculation_exante <- dynamicMultiServer(
        id = "calculation_exante", 
        nmUImod = "tblRowExAnteImpEstUI", 
        nmSrvrMod = "tblRowExAnteImpEstServer", 
        lsArgsModUI = list(hL = hL, colWidth = colWidth, inpWidth = inpWidth), 
        lsArgsModSrvr = list(lsPreset = lsPreset$calculation_exante))
      
      # inputs -----------------------------------------------------------------
      
      observeEvent(input$explanation_methchoices, handlerExpr = {
        
        resultsGood$explanation_methchoices <- FALSE
        rvToReturn$results$explanation_methchoices <- input$explanation_methchoices
        
        nChar <- nchar(input$explanation_methchoices)
        outputMsgs$explanation_methchoices <- sprintf('%d chars. remaining', 300 - nChar)
        
        resultsGood$explanation_methchoices <- isValidInput.text(
          x = input$explanation_methchoices, 
          bRequired = TRUE, 
          nCharMin = 50, 
          nCharMax = 300) 
        
      })
      
      observe({
        resultsGood$dataparams_exante <- FALSE
        rvToReturn$results$dataparams_exante <- modSrvrs$dataparams_exante$items
        resultsGood$dataparams_exante <- length(rvToReturn$results$dataparams_exante) > 0
      })
      
      observe({
        resultsGood$calculation_exante <- FALSE
        resultsGood$summary_exante <- FALSE
        rvToReturn$results$summary_exante <- ''
        
        rvToReturn$results$calculation_exante <- modSrvrs$calculation_exante$items
        resultsGood$calculation_exante <- length(rvToReturn$results$calculation_exante) > 0
        
        if (resultsGood$calculation_exante) {
          rvToReturn$results$summary_exante <- paste(
            sprintf("%s %s", 
                    resultsGood$calculation_exante$estimate_impact,
                    resultsGood$calculation_exante$unit_parameter_impact), 
            sep = "", collapse = "\n")
          resultsGood$summary_exante <- nchar(rvToReturn$results$summary_exante) > 0
        }
      })
      
      # outputs ----------------------------------------------------------------
    
      output$explanation_methchoices_msg <- renderText({
        outputMsgs$explanation_methchoices
      })
      
      output$summary_exante <- renderText({
        rvToReturn$results$summary_exante
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
