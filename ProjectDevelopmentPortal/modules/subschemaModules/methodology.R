
methodologyInput <- function(id, 
                             lsPreset = NULL, 
                             hL = 5,
                             colWidth = 12, 
                             inpWidth = DEFAULT_INP_WIDTH) {
  
  ns <- NS(id)
  
  tagList(
    
    #useShinyjs(),
    
    GtextInput(id = ns("title"), 
               title = "Title", 
               helpTxt = "Title of the methodology (10 chars. min.):", 
               value = lsPreset$title, 
               hL = hL, 
               colWidth = colWidth, 
               inpWidth = inpWidth),
    
    GtextInput(id = ns("label"), 
               title = "Label", 
               helpTxt = "Label for the methodology (e.g., ACM0002; 3 chars. min.):", 
               value = lsPreset$label, 
               hL = hL, 
               colWidth = colWidth, 
               inpWidth = inpWidth),
    
    GtextInput(id = ns("version"), 
               title = "Version", 
               helpTxt = "Version of the methodology used:", 
               value = lsPreset$version, 
               hL = hL, 
               colWidth = colWidth, 
               inpWidth = inpWidth),
    
    textOutput(outputId = ns("toInvalidInput")),
    
    br())
}

methodologyServer <- function(id, lsPreset = NULL) {
  
  moduleServer(
    id = id, 
    function(input, output, session) {
      
      ns <- session$ns
      
      rvToReturn <- reactiveValues()
      rvToReturn$goToModule <- NULL
      rvToReturn$allResultsGood <- FALSE
      rvToReturn$results <- reactiveValues(
        title = NULL,
        label = NULL,
        version = NULL)
      
      resultsGood <- reactiveValues()
      resultsGood$title <- FALSE
      resultsGood$label <- FALSE
      resultsGood$version <- FALSE
      
      outputMsgs <- reactiveValues()
      outputMsgs$title <- ""
      outputMsgs$label <- ""
      outputMsgs$version <- ""
      
      # preset inputs ----------------------------------------------------------
      
      if ("title" %in% names(lsPreset)) {
        updateTextInput(session = session, 
                        inputId = "title", 
                        value = lsPreset$title)
      }
      
      if ("label" %in% names(lsPreset)) {
        updateTextInput(session = session, 
                        inputId = "label", 
                        value = lsPreset$label)
      }
      
      if ("version" %in% names(lsPreset)) {
        updateTextInput(session = session, 
                        inputId = "version", 
                        value = lsPreset$version)
      }
      
      # inputs -----------------------------------------------------------------
      
      observeEvent(input$title, handlerExpr = {
        
        resultsGood$title <- FALSE
        rvToReturn$results$title <- input$title
        
        nChar <- nchar(input$title)
        outputMsgs$title <- sprintf("%d chars. remaining", 200 - nChar)
        
        resultsGood$title <- isValidInput.text(x = input$title, 
                                               bRequired = TRUE, 
                                               nCharMin = 10, 
                                               nCharMax = 200)
        
      })
      
      observeEvent(input$label, handlerExpr = {
        
        resultsGood$label <- FALSE
        rvToReturn$results$label <- input$label
        
        nChar <- nchar(input$label)
        outputMsgs$label <- sprintf("%d chars. remaining", 40 - nChar)
        
        resultsGood$label <- isValidInput.text(x = input$label, 
                                               bRequired = TRUE, 
                                               nCharMin = 3, 
                                               nCharMax = 40)
        
      })
      
      observeEvent(input$version, handlerExpr = {
        
        resultsGood$version <- FALSE
        rvToReturn$results$version <- input$version
        
        nChar <- nchar(input$version)
        outputMsgs$version <- sprintf("%d chars. remaining", 10 - nChar)
        
        resultsGood$version <- isValidInput.text(x = input$version, 
                                                 bRequired = TRUE, 
                                                 nCharMin = 1, 
                                                 nCharMax = 10)
        
      })
      
      # outputs-----------------------------------------------------------------
      
      output$title_msg <- renderText({
        outputMsgs$title
      })
      
      output$label_msg <- renderText({
        outputMsgs$label
      })
      
      output$version_msg <- renderText({
        outputMsgs$version
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

