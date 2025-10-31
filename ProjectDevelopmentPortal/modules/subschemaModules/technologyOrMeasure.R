
technologyOrMeasureInput <- function(id, 
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

          # GselectInput(id = ns("type_techmeas"),
          #              title = "Type of technology or measure",
          #              choices = c("FACILITY",
          #                          "SYSTEM",
          #                          "EQUIPMENT",
          #                          "OTHER"),
          #              helpTxt = "", # TODO.
          #              selected = lsPreset$type_techmeas,
          #              multiple = FALSE,
          #              hL = hL,
          #              colWidth = colWidth,
          #              inpWidth = inpWidth),
          
          fluidRow(
            column(
              width = colWidth,
              wellPanel(
                h5("Type of technology or measure"),
                helpText(""),
                selectInput(inputId = ns("type_techmeas"),
                            label = NULL,
                            choices = c("FACILITY",
                                        "SYSTEM",
                                        "EQUIPMENT",
                                        "OTHER"),
                            multiple = FALSE,
                            selected = lsPreset$type_techmeas,
                            selectize = TRUE,
                            width = inpWidth),
                textOutput(outputId = ns("type_techmeas_msg"))))),
          
          GtextInput(id = ns("type_techmeas_otherexplain"),
                     title = "Explanation of type of technology or measure",
                     helpTxt = "If you have selected 'OTHER' above, please explain what type of technology or measure this project will be implementing (5 chars. min.).",
                     value = lsPreset$type_techmeas_otherexplain,
                     hL = hL,
                     colWidth = colWidth,
                     inpWidth = inpWidth),
          
          GtextInput(id = ns("description"),
                     title = "Description",
                     helpTxt = "Please describe the technology or measure, e.g., 'wind turbine' or 'cookstove' (5 chars. min.)",
                     value = lsPreset$description,
                     hL = hL,
                     colWidth = colWidth,
                     inpWidth = inpWidth),
          
          GnumericInput(id = ns("age_current"), 
                        title = "Current age (in years)", 
                        helpTxt = "", # TODO.
                        value = lsPreset$age_current, 
                        min = 0, 
                        max = 100, 
                        step = 1, 
                        hL = hL, 
                        colWidth = colWidth, 
                        inpWidth = inpWidth),
          
          GnumericInput(id = ns("lifespan_estimated"), 
                        title = "Lifespan", 
                        helpTxt = "Estimated lifespan (in years) according to manufacturer's specifications and industry standards.",
                        value = lsPreset$lifespan_estimated, 
                        min = 0, 
                        max = 100, 
                        step = 1, 
                        hL = hL, 
                        colWidth = colWidth, 
                        inpWidth = inpWidth),
          
          textOutput(outputId = ns("toAgeVsLifespan")),
          
          GtextAreaInput(id = ns("info_additional"), 
                         title = "Additional information", 
                         helpTxt = "As relevant.", 
                         value = lsPreset$info_additional, 
                         resize = "vertical",
                         hL = hL, 
                         colWidth = colWidth, 
                         inpWidth = inpWidth)))))
}

technologyOrMeasureServer <- function(id, lsPreset = NULL) {
  
  moduleServer(
    id = id, 
    function(input, output, session) {
      
      ns <- session$ns
      
      rvToReturn <- reactiveValues()
      rvToReturn$goToModule <- NULL
      rvToReturn$allResultsGood <- FALSE
      rvToReturn$results <- reactiveValues(
        type_techmeas = NULL,
        type_techmeas_otherexplain = NULL,
        description = NULL,
        age_current = NULL,
        lifespan_estimated = NULL,
        info_additional = NULL)
      
      resultsGood <- reactiveValues()
      resultsGood$type_techmeas <- FALSE
      resultsGood$type_techmeas_otherexplain <- FALSE
      resultsGood$description <- FALSE
      resultsGood$age_current <- FALSE
      resultsGood$lifespan_estimated <- FALSE
      resultsGood$info_additional <- FALSE 
      
      outputMsgs <- reactiveValues()
      outputMsgs$type_techmeas <- ""
      outputMsgs$type_techmeas_otherexplain <- ""
      outputMsgs$description <- ""
      outputMsgs$age_current <- ""
      outputMsgs$toAgeVsLifespan <- ""
      outputMsgs$lifespan_estimated <- ""
      outputMsgs$info_additional <- ""
      
      # preset inputs ----------------------------------------------------------
      
      if ("type_techmeas" %in% names(lsPreset)) {
        updateSelectInput(session = session, 
                          inputId = "type_techmeas", 
                          selected = lsPreset$type_techmeas)
      }
      
      if ("type_techmeas_otherexplain" %in% names(lsPreset)) {
        updateTextInput(session = session, 
                        inputId = "type_techmeas_otherexplain", 
                        value = lsPreset$type_techmeas_otherexplain)
      }
      
      if ("description" %in% names(lsPreset)) {
        updateTextInput(session = session, 
                        inputId = "description", 
                        value = lsPreset$description)
      }
      
      if ("age_current" %in% names(lsPreset)) {
        updateNumericInput(session = session, 
                           inputId = "age_current", 
                           value = lsPreset$age_current)
      }
      
      if ("lifespan_estimated" %in% names(lsPreset)) {
        updateNumericInput(session = session, 
                           inputId = "lifespan_estimated", 
                           value = lsPreset$lifespan_estimated)
      }
      
      if ("info_additional" %in% names(lsPreset)) {
        updateTextAreaInput(session = session, 
                            inputId = "info_additional", 
                            value = lsPreset$info_additional)
      }
      
      # inputs -----------------------------------------------------------------
      
      observe({
        
        resultsGood$type_techmeas <- FALSE
        rvToReturn$results$type_techmeas <- NULL
        outputMsgs$type_techmeas <- ""
        resultsGood$type_techmeas_otherexplain <- FALSE
        rvToReturn$results$type_techmeas_otherexplain <- NULL
        outputMsgs$type_techmeas_otherexplain <- ""
        
        #validate(need(input$type_techmeas, message = FALSE)) # Didn't do what we wanted it to.
        if (length(input$type_techmeas) == 0) { return(invisible(NULL)) }
        
        rvToReturn$results$type_techmeas <- input$type_techmeas
        resultsGood$type_techmeas <- TRUE
        
        if (input$type_techmeas == "OTHER") {
          
          outputMsgs$type_techmeas <- "Please explain below what type of technology or measure this project will be implementing."
          
          rvToReturn$results$type_techmeas_otherexplain <- input$type_techmeas_otherexplain
          
          nChar <- nchar(input$type_techmeas_otherexplain)
          outputMsgs$type_techmeas_otherexplain <- sprintf("%d chars. remaining", 100 - nChar)
          
          resultsGood$type_techmeas_otherexplain <- isValidInput.text(
            x = input$type_techmeas_otherexplain, 
            bRequired = TRUE, 
            nCharMin = 5, 
            nCharMax = 100)
          
          if (resultsGood$type_techmeas_otherexplain) {
            outputMsgs$type_techmeas <- ""
          }
        } else {
          rvToReturn$results$type_techmeas_otherexplain <- NA_character_
          resultsGood$type_techmeas_otherexplain <- TRUE
        }
      })
      
      
      observeEvent(input$description, handlerExpr = {
        
        resultsGood$description <- FALSE
        rvToReturn$results$description <- input$description
        
        nChar <- nchar(input$description)
        outputMsgs$description <- sprintf("%d chars. remaining", 500 - nChar)
        
        resultsGood$description <- isValidInput.text(
          x = input$description, 
          bRequired = TRUE, 
          nCharMin = 5, 
          nCharMax = 500)
      })
      
      
      
      
      observeEvent(input$age_current, handlerExpr = {
        
        rvToReturn$results$age_current <- input$age_current
        outputMsgs$age_current <- ""
        # resultsGood$age_current <- FALSE # Not doing this here because its 
        # final status depends on its relationship to input$lifespan_estimated.
        
        validate(need(input$age_current, message = FALSE))
        
        if (input$age_current > 10) {
          outputMsgs$age_current <- paste(
            "You have entered a high value for the age of the technology/measure.",
            "Are you sure the value is correct?",
            "If you are sure, you can ignore this message.", 
            sep = "\n")
        }
        
        # resultsGood$age_current <- TRUE # Not doing this here because its 
        # final status depends on its relationship to input$lifespan_estimated.
        
      })
      
      observeEvent(input$lifespan_estimated, handlerExpr = {
        
        rvToReturn$results$lifespan_estimated <- input$lifespan_estimated
        outputMsgs$lifespan_estimated <- ""
        # resultsGood$lifespan_estimated <- FALSE # Not doing this here because its 
        # final status depends on its relationship to input$age_current.
        
        validate(need(input$lifespan_estimated, message = FALSE))
        
        if (input$lifespan_estimated > 10) {
          outputMsgs$lifespan_estimated <- paste(
            "You have entered a high value for the lifespan of the technology/measure.",
            "Are you sure the value is correct?",
            "If you are sure, you can ignore this message.", 
            sep = "\n")
        }
        
        # resultsGood$lifespan_estimated <- TRUE # Not doing this here because its 
        # final status depends on its relationship to input$age_current.
        
      })
      
      observe({
        
        resultsGood$age_current <- FALSE
        resultsGood$lifespan_estimated <- FALSE
        outputMsgs$toAgeVsLifespan <- ""
        
        validate(need(input$lifespan_estimated, message = FALSE))
        validate(need(input$age_current, message = FALSE))
        if (length(input$lifespan_estimated) == 0) { return(invisible(NULL)) }
        if (length(input$age_current) == 0) { return(invisible(NULL)) }
        
        if (input$lifespan_estimated < input$age_current) {
          outputMsgs$toAgeVsLifespan <- "Estimated lifespan cannot be less than current age."
        } else {
          resultsGood$age_current <- TRUE
          resultsGood$lifespan_estimated <- TRUE
        }
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
      
      # outputs ----------------------------------------------------------------
      
      output$type_techmeas_msg <- renderText({
        outputMsgs$type_techmeas
      })
      
      output$type_techmeas_otherexplain_msg <- renderText({
        outputMsgs$type_techmeas_otherexplain
      })
      
      output$description_msg <- renderText({
        outputMsgs$description
      })
      
      output$age_current_msg <- renderText({
        outputMsgs$age_current
      })
      
      output$lifespan_estimated_msg <- renderText({
        outputMsgs$lifespan_estimated
      })
      
      output$toAgeVsLifespan <- renderText({
        outputMsgs$toAgeVsLifespan
      })
      
      output$info_additional_msg <- renderText({
        outputMsgs$info_additional
      })
      
      # return logic -----------------------------------------------------------
      
      observe({
        rvToReturn$allResultsGood <- all(unlist(reactiveValuesToList(resultsGood)))
      })
      
      return(rvToReturn)
      
    })
}
