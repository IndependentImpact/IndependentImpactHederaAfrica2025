# Impact Summary schema [v1.0.0]


# Module for schema 'Date-Time Period' [v1.0.0].
# Can also be used for schema 'Monitoring Period' [v2.0.0].

dateTimePeriodInput <- function(id, 
                                title,
                                helpTxt = NULL,
                                lsPreset = NULL, 
                                hL = 4, 
                                colWidth = 12, 
                                inpWidth = DEFAULT_INP_WIDTH) {
  
  ns <- NS(id)
  
  tagList(
    
    ##useShinyjs(),
    
    wellPanel(
      
      fluidRow(
        column(
          width = 12,
          hx(x = title, lvl = hL),
          helpText(helpTxt))),
      
      fluidRow(
        column(
          width = 6,
          GtextInput(
            id = ns("datetime_start"), 
            title = "Start Date and Time", 
            helpTxt = "(YYYY-MM-DD hh:mm:ss)", 
            value = lsPreset$datetime_start, 
            hL = max(hL+1, 1), 
            colWidth = colWidth, 
            inpWidth = inpWidth,
            wellP = FALSE)),
        column(
          width = 6, 
          GtextInput(
            id = ns("datetime_end"),
            title = "End Date and Time", 
            helpTxt = "(YYYY-MM-DD hh:mm:ss)", 
            value = lsPreset$datetime_end, 
            hL = max(hL+1, 1), 
            colWidth = colWidth, 
            inpWidth = inpWidth,
            wellP = FALSE))),
      
      br(),
      
      fluidRow(
        column(
          width = 12,
          textOutput(outputId = ns("toRelation"))))))
}

dateTimePeriodServer <- function(id, lsPreset = NULL) {
  
  moduleServer(
    id = id, 
    function(input, output, session) {
      
      ns <- session$ns
      
      rvToReturn <- reactiveValues()
      rvToReturn$goToModule <- NULL
      rvToReturn$allResultsGood <- FALSE
      rvToReturn$results <- reactiveValues(
        datetime_start = NULL,
        datetime_end = NULL)
      
      resultsGood <- reactiveValues()
      resultsGood$datetime_start <- FALSE
      resultsGood$datetime_end <- FALSE
      resultsGood$relation <- FALSE
      
      outputMsgs <- reactiveValues()
      outputMsgs$datetime_start <- ""
      outputMsgs$datetime_end <- ""
      outputMsgs$relation <- ""
      
      # preset inputs ----------------------------------------------------------
      
      if ("datetime_start" %in% names(lsPreset)) {
        updateTextInput(session = session, 
                        inputId = "datetime_start", 
                        value = lsPreset$datetime_start)
      }
      
      if ("datetime_end" %in% names(lsPreset)) {
        updateTextInput(session = session, 
                        inputId = "datetime_end", 
                        value = lsPreset$datetime_end)
      }
      
      # inputs -----------------------------------------------------------------
      
      observeEvent(input$datetime_start, handlerExpr = {
        
        resultsGood$datetime_start <- FALSE
        outputMsgs$datetime_start <- "Invalid date format."
        
        if (nchar(input$datetime_start) != 19) {
          rvToReturn$results$datetime_start <- input$datetime_start
          return(invisible(0))
        }
        
        dtStart <- lubridate::ymd_hms(input$datetime_start, tz = "UTC")
        rvToReturn$results$datetime_start <- dtStart
        if (is.na(dtStart)) {
          return(invisible(0))
        }
        
        outputMsgs$datetime_start <- ""
        resultsGood$datetime_start <- TRUE
        
      })
      
      observeEvent(input$datetime_end, handlerExpr = {
        
        resultsGood$datetime_end <- FALSE
        outputMsgs$datetime_end <- "Invalid date format."
        
        if (nchar(input$datetime_end) != 19) {
          rvToReturn$results$datetime_end <- input$datetime_end
          return(invisible(0))
        }
        
        dtEnd <- lubridate::ymd_hms(input$datetime_end, tz = "UTC")
        rvToReturn$results$datetime_end <- dtEnd
        if (is.na(dtEnd)) {
          return(invisible(0))
        }
        
        outputMsgs$datetime_end <- ""
        resultsGood$datetime_end <- TRUE
        
      })
      
      observe({
        
        resultsGood$relation <- FALSE
        outputMsgs$relation <- ""
        
        if (!resultsGood$datetime_start) { return(invisible(0)) }
        if (!resultsGood$datetime_end) { return(invisible(0)) }
        
        if (isolate(rvToReturn$results$datetime_end) <= isolate(rvToReturn$results$datetime_start)) {
          outputMsgs$relation <- "End date cannot be earlier than or the same as the start date."
          return(invisible(0))
        }
        
        resultsGood$relation <- TRUE
      })
      
      # outputs-----------------------------------------------------------------
      
      output$datetime_start_msg <- renderText({
        outputMsgs$datetime_start
      })
      
      output$datetime_end_msg <- renderText({
        outputMsgs$datetime_end
      })
      
      output$toRelation <- renderText({
        outputMsgs$relation
      })
      
      # module return logic ----------------------------------------------------
      
      observe({
        rvToReturn$allResultsGood <- all(unlist(reactiveValuesToList(resultsGood)))
      })
      
      return(rvToReturn)
      
    })
}

