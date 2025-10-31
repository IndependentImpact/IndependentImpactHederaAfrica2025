
projectPartyInput <- function(id, 
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
          
          GtextInput(id = ns("name"), 
                     title = "Name of party", 
                     helpTxt = "", # TODO: Add help text here.
                     value = lsPreset$name, 
                     hL = hL, 
                     colWidth = colWidth, 
                     inpWidth = inpWidth),
          
          GselectInput(id = ns("is_host"), 
                       title = "Host status", 
                       choices = c("YES", "NO"), 
                       helpTxt = "Is this party a host party?", 
                       selected = lsPreset$is_host, 
                       multiple = FALSE, 
                       hL = hL, 
                       colWidth = colWidth, 
                       inpWidth = inpWidth),
          
          GselectInput(id = ns("public_private"), 
                       title = "Type", 
                       choices = c("PUBLIC", "PRIVATE"), 
                       helpTxt = "Is this a public or a private party?", 
                       selected = lsPreset$public_private, 
                       multiple = FALSE, 
                       hL = hL, 
                       colWidth = colWidth, 
                       inpWidth = inpWidth),
          
          GselectInput(id = ns("is_participant"), 
                       title = "Participant status", 
                       choices = c("YES", "NO"), 
                       helpTxt = "Should this party be considered a project participant?", 
                       selected = lsPreset$is_participant, 
                       multiple = FALSE, 
                       hL = hL, 
                       colWidth = colWidth, 
                       inpWidth = inpWidth),

          GtextAreaInput(id = ns("info_additional"), 
                         title = "Additional information", 
                         helpTxt = "As relevant.", 
                         value = lsPreset$info_additional, 
                         resize = "vertical",
                         hL = hL, 
                         colWidth = colWidth, 
                         inpWidth = inpWidth)))))
}

projectPartyServer <- function(id, lsPreset = NULL) {
  
  moduleServer(
    id = id, 
    function(input, output, session) {
      
      ns <- session$ns
      
      rvToReturn <- reactiveValues()
      rvToReturn$goToModule <- NULL
      rvToReturn$allResultsGood <- FALSE
      rvToReturn$results <- reactiveValues(
        name = NULL,
        is_host = NULL,
        public_private = NULL,
        is_participant = NULL,
        info_additional = NULL)
      
      resultsGood <- reactiveValues()
      resultsGood$name <- FALSE
      resultsGood$is_host <- FALSE
      resultsGood$public_private <- FALSE
      resultsGood$is_participant <- FALSE
      resultsGood$info_additional <- FALSE
      
      outputMsgs <- reactiveValues()
      outputMsgs$name <- ""
      outputMsgs$is_host <- ""
      outputMsgs$public_private <- ""
      outputMsgs$is_participant <- ""
      outputMsgs$info_additional <- ""
      
      # preset inputs ----------------------------------------------------------
      
      if ("name" %in% names(lsPreset)) {
        updateTextInput(session = session, 
                        inputId = "name", 
                        value = lsPreset$name)
      }
      
      if ("is_host" %in% names(lsPreset)) {
        updateSelectInput(session = session, 
                          inputId = "is_host", 
                          selected = lsPreset$is_host)
      }
      
      if ("public_private" %in% names(lsPreset)) {
        updateSelectInput(session = session, 
                          inputId = "public_private", 
                          selected = lsPreset$public_private)
      }
      
      if ("is_participant" %in% names(lsPreset)) {
        updateSelectInput(session = session, 
                          inputId = "is_participant", 
                          selected = lsPreset$is_participant)
      }
      
      if ("info_additional" %in% names(lsPreset)) {
        updateTextInput(session = session, 
                        inputId = "info_additional", 
                        value = lsPreset$info_additional)
      }
      
      # inputs -----------------------------------------------------------------
      
      observeEvent(input$name, handlerExpr = {
        
        resultsGood$name <- FALSE
        rvToReturn$results$name <- input$name
        
        nChar <- nchar(input$name)
        outputMsgs$name <- sprintf("%d chars. remaining", 100 - nChar)
        
        resultsGood$name <- isValidInput.text(x = input$name, 
                                              bRequired = TRUE, 
                                              nCharMin = 1, 
                                              nCharMax = 100)
        
      })
      
      observeEvent(input$is_host, handlerExpr = {
        
        resultsGood$is_host <- FALSE
        rvToReturn$results$is_host <- input$is_host
        outputMsgs$is_host <- ""
        
        validate(need(input$is_host, message = FALSE))
        
        resultsGood$is_host <- TRUE
      })
      
      observeEvent(input$public_private, handlerExpr = {
        
        resultsGood$public_private <- FALSE
        rvToReturn$results$public_private <- input$public_private
        outputMsgs$public_private <- ""
        
        validate(need(input$public_private, message = FALSE))
        
        resultsGood$public_private <- TRUE
      })
      
      observeEvent(input$is_participant, handlerExpr = {
        
        resultsGood$is_participant <- FALSE
        rvToReturn$results$is_participant <- input$is_participant
        outputMsgs$is_participant <- ""
        
        validate(need(input$is_participant, message = FALSE))
        
        resultsGood$is_participant <- TRUE
      })
      
      observeEvent(input$info_additional, handlerExpr = {
        
        resultsGood$info_additional <- FALSE
        rvToReturn$results$info_additional <- input$info_additional
        
        nChar <- nchar(input$info_additional)
        outputMsgs$info_additional <- sprintf("%d chars. remaining", 500 - nChar)
        
        resultsGood$info_additional <- isValidInput.text(
          x = input$info_additional, 
          bRequired = FALSE, 
          nCharMin = 0, 
          nCharMax = 500)
      })
      
      # outputs-----------------------------------------------------------------
      
      output$name_msg <- renderText({
        outputMsgs$name
      })
      
      output$is_host_msg <- renderText({
        outputMsgs$is_host
      })
      
      output$public_private_msg <- renderText({
        outputMsgs$public_private
      })
      
      output$is_participant_msg <- renderText({
        outputMsgs$is_participant
      })
      
      output$info_additional_msg <- renderText({
        outputMsgs$info_additional
      })
      
      # module return logic ----------------------------------------------------
      
      observe({
        rvToReturn$allResultsGood <- all(unlist(reactiveValuesToList(resultsGood)))
      })

      return(rvToReturn)
      
    })
}

