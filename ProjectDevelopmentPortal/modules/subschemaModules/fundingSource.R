
fundingSourceInput <- function(id, 
                               lsPreset = NULL, 
                               hL = 5,
                               colWidth = 12, 
                               inpWidth = DEFAULT_INP_WIDTH) {
  
  ns <- NS(id)

  # TODO: Expand this to name, description, conditions.
  tagList(
    
    #useShinyjs(),
    
    GtextAreaInput(id = ns("details"), 
                   title = "Details of funding source", 
                   helpTxt = "Include the name of the funding entity and the conditions under which the funds are provided to the project. (10 chars. min.)", 
                   value = lsPreset$details,
                   resize = "vertical",
                   hL = hL, 
                   colWidth = colWidth, 
                   inpWidth = inpWidth))

}

fundingSourceServer <- function(id, lsPreset = NULL) {
  
  moduleServer(
    id = id, 
    function(input, output, session) {
      
      ns <- session$ns
      
      rvToReturn <- reactiveValues()
      rvToReturn$goToModule <- NULL
      rvToReturn$allResultsGood <- FALSE
      rvToReturn$results <- reactiveValues(
        details = NULL)
      
      resultsGood <- reactiveValues()
      resultsGood$details <- FALSE
      
      outputMsgs <- reactiveValues()
      outputMsgs$details <- ""
      
      # preset inputs ----------------------------------------------------------

      if ("details" %in% names(lsPreset)) {
        updateTextInput(session = session, 
                        inputId = "details", 
                        value = lsPreset$details)
      }
      
      # inputs -----------------------------------------------------------------
      
      observeEvent(input$details, handlerExpr = {
        
        resultsGood$details <- FALSE
        rvToReturn$results$details <- input$details
        
        nChar <- nchar(input$details)
        outputMsgs$details <- sprintf("%d chars. remaining", 1000 - nChar)
        
        resultsGood$details <- isValidInput.text(
          x = input$details, 
          bRequired = TRUE, 
          nCharMin = 10, 
          nCharMax = 1000)
        
      })
      
      # outputs-----------------------------------------------------------------
      
      output$details_msg <- renderText({
        outputMsgs$details
      })
      
      # module return logic ----------------------------------------------------
      
      observe({
        rvToReturn$allResultsGood <- all(unlist(reactiveValuesToList(resultsGood)))
      })
      
      return(rvToReturn)
      
    })
}












