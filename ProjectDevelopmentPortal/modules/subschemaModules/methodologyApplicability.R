
methodologyApplicabilityInput <- function(id, 
                                          lsPreset = NULL, 
                                          hL = 5,
                                          colWidth = 12, 
                                          inpWidth = DEFAULT_INP_WIDTH) {
  
  ns <- NS(id)
  
  tagList(
    
    #useShinyjs(),
    
    fluidRow(
      column(
        width = colWidth,
        wellPanel(
          
          hx(x = "Methodology", lvl = hL),
          
          methodologyInput(
            id = ns("methodology"), 
            lsPreset = lsPreset$methodology, 
            hL = hL, 
            colWidth = colWidth, 
            inpWidth = inpWidth)))),
    
    GtextAreaInput(
      id = ns("applicability"), 
      title = "Applicability", 
      helpTxt = "Why is the indicated methodology applicable to the use case? (50 chars. min.)", 
      value = lsPreset$applicability, 
      hL = hL, 
      colWidth = colWidth, 
      inpWidth = inpWidth, 
      resize = "vertical"),
    
    textOutput(outputId = ns("toInvalidInput")),
    
    br())
}

methodologyApplicabilityServer <- function(id, lsPreset = NULL) {
  
  moduleServer(
    id = id, 
    function(input, output, session) {
      
      ns <- session$ns
      
      rvToReturn <- reactiveValues()
      rvToReturn$goToModule <- NULL
      rvToReturn$allResultsGood <- FALSE
      rvToReturn$results <- reactiveValues(
        methodology = NULL,
        applicability = NULL)
      
      resultsGood <- reactiveValues()
      resultsGood$methodology <- FALSE
      resultsGood$applicability <- FALSE
      
      outputMsgs <- reactiveValues()
      outputMsgs$applicability <- ""
      
      # module servers ---------------------------------------------------------
      modSrvrs <- reactiveValues()
      modSrvrs$methodology <- methodologyServer(
        id = "methodology", 
        lsPreset = lsPreset$methodology)
      
      # preset inputs ----------------------------------------------------------
      
      if ("applicability" %in% names(lsPreset)) {
        updateSelectInput(session = session, 
                          inputId = "applicability", 
                          selected = lsPreset$applicability)
      }
      
      # inputs -----------------------------------------------------------------
      
      observe({
        resultsGood$methodology <- FALSE
        rvToReturn$results$methodology <- modSrvrs$methodology$results
        resultsGood$methodology <- modSrvrs$methodology$allResultsGood
      })
      
      observeEvent(input$applicability, handlerExpr = {
        
        resultsGood$applicability <- FALSE
        rvToReturn$results$applicability <- input$applicability
        
        nChar <- nchar(input$applicability)
        outputMsgs$applicability <- sprintf("%d chars. remaining", 300 - nChar)
        
        resultsGood$applicability <- isValidInput.text(x = input$applicability, 
                                                       bRequired = TRUE, 
                                                       nCharMin = 50, 
                                                       nCharMax = 300)
        
      })
      
      # outputs-----------------------------------------------------------------
      
      output$applicability_msg <- renderText({
        outputMsgs$applicability
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

