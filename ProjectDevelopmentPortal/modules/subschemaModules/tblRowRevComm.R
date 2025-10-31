
# Module for schema 'Table Row: Reviewer Commentary [v1.0.0]'.

tblRowRevCommUI <- function(id, 
                            lsPreset = NULL, 
                            hL = 4, 
                            colWidth = 12, 
                            inpWidth = DEFAULT_INP_WIDTH) {
  
  ns <- NS(id)
  
  tagList(
    
    #useShinyjs(),
    
    GtextInput(id = ns('id_section'),
               title = 'Section ID',
               helpTxt = 'Section identifier.', 
               value = lsPreset$id_section,
               hL = hL,
               colWidth = colWidth,
               inpWidth = inpWidth),
    
    GtextInput(id = ns("number_paragraph"), 
               title = "Paragraph Number",
               helpTxt = "Number of the paragraph (restart counting from 1 for each section).", 
               value = lsPreset$number_paragraph, 
               hL = hL, 
               colWidth = colWidth, 
               inpWidth = inpWidth),
    
    GselectInput(id = ns("reviewer_evaluation"), 
                 title = "Reviewer Evaluation", 
                 choices = c("APPROVE",
                             "FORWARD_ACTION_REQUEST",
                             "CORRECTIVE_ACTION_REQUEST",
                             "REJECT"), 
                 helpTxt = "Evaluation of reviewer.", 
                 selected = lsPreset$reviewer_evaluation, 
                 multiple = FALSE, 
                 hL = hL, 
                 colWidth = colWidth, 
                 inpWidth = inpWidth),

    GtextAreaInput(id = ns('reviewer_feedback'),
                   title = 'Reviewer Feedback',
                   helpTxt = 'Feedback from reviewer (5 chars. min., 1000 chars. max.).',
                   value = lsPreset$reviewer_feedback,
                   resize = 'vertical',
                   hL = hL,
                   colWidth = colWidth,
                   inpWidth = inpWidth))
}

tblRowRevCommServer <- function(id,  
                                lsPreset = NULL) {
  
  moduleServer(
    id = id, 
    function(input, output, session) {
      
      ns <- session$ns
      
      rvToReturn <- reactiveValues()
      rvToReturn$allResultsGood <- FALSE
      rvToReturn$results <- reactiveValues(
        id_section = NULL,
        number_paragraph = NULL,
        reviewer_evaluation = NULL,
        reviewer_feedback = NULL)
      
      resultsGood <- reactiveValues()
      resultsGood$id_section <- FALSE
      resultsGood$number_paragraph <- FALSE
      resultsGood$reviewer_evaluation <- FALSE
      resultsGood$reviewer_feedback <- FALSE

      outputMsgs <- reactiveValues()
      outputMsgs$id_section <- ''
      outputMsgs$number_paragraph <- ''
      outputMsgs$reviewer_evaluation <- ''
      outputMsgs$reviewer_feedback <- ''

      rvOther <- reactiveValues()
      
      # preset inputs ----------------------------------------------------------
      
      if ('id_section' %in% names(lsPreset)) {
        updateTextInput(session = session, 
                        inputId = 'id_section', 
                        value = lsPreset$id_section)
      }
      
      if ('number_paragraph' %in% names(lsPreset)) {
        updateTextInput(session = session, 
                        inputId = 'number_paragraph', 
                        value = lsPreset$number_paragraph)
      }
      
      if ('reviewer_evaluation' %in% names(lsPreset)) {
        updateSelectInput(session = session, 
                          inputId = 'reviewer_evaluation', 
                          selected = lsPreset$reviewer_evaluation)
      }
      
      if ('reviewer_feedback' %in% names(lsPreset)) {
        updateTextAreaInput(session = session, 
                            inputId = 'reviewer_feedback', 
                            value = lsPreset$reviewer_feedback)
      }
    
      # module servers ---------------------------------------------------------
      # modSrvrs <- reactiveValues()
      
      # inputs -----------------------------------------------------------------
  
      observeEvent(input$id_section, handlerExpr = {
        
        resultsGood$id_section <- FALSE
        rvToReturn$results$id_section <- input$id_section
        
        nChar <- nchar(input$id_section)
        outputMsgs$id_section <- sprintf('%d chars.', nChar)
        
        resultsGood$id_section <- isValidInput.text(
          x = input$id_section, 
          bRequired = TRUE, 
          nCharMin = 1, 
          nCharMax = 1000) 
        
      })
      
      observeEvent(input$number_paragraph, handlerExpr = {
        
        resultsGood$number_paragraph <- FALSE
        outputMsgs$number_paragraph <- ''
        x <- as.integer(input$number_paragraph)
        rvToReturn$results$number_paragraph <- x
        resultsGood$number_paragraph <- (!is.na(x) & !is.nan(x) & (x > 0))
        
        if (!resultsGood$number_paragraph) {
          outputMsgs$number_paragraph <- "Must be an integer greater than 0."
        }
  
      })
      
      observeEvent(input$reviewer_evaluation, handlerExpr = {
        
        resultsGood$reviewer_evaluation <- FALSE
        outputMsgs$reviewer_evaluation <- ''
        rvToReturn$results$reviewer_evaluation <- input$reviewer_evaluation
        validate(need(input$reviewer_evaluation, message = FALSE))
        resultsGood$reviewer_evaluation <- TRUE
        
        if (!resultsGood$reviewer_evaluation) {
          outputMsgs$reviewer_evaluation <- 'Required.'
        }
      })
      
      observeEvent(input$reviewer_feedback, handlerExpr = {
        
        resultsGood$reviewer_feedback <- FALSE
        rvToReturn$results$reviewer_feedback <- input$reviewer_feedback
        
        nChar <- nchar(input$reviewer_feedback)
        if (nChar < 5) {
          msg <- sprintf("%d chars. remaining (5 chars. min.)", 1000 - nChar)
        } else {
          if (nChar > 1000) {
            msg <- sprintf("%d chars. remaining (1000 chars. max.)", 1000 - nChar)
          } else {
            msg <- sprintf("%d chars. remaining.", 1000 - nChar)
          } 
        }
        outputMsgs$reviewer_feedback <- msg
    
        resultsGood$reviewer_feedback <- isValidInput.text(
          x = input$reviewer_feedback, 
          bRequired = TRUE, 
          nCharMin = 5, 
          nCharMax = 1000) 
        
      })
      
      # outputs ----------------------------------------------------------------

      output$id_section_msg <- renderText({
        outputMsgs$id_section
      })
      
      output$number_paragraph_msg <- renderText({
        outputMsgs$number_paragraph
      })
      
      output$reviewer_evaluation_msg <- renderText({
        outputMsgs$reviewer_evaluation
      })
      
      output$reviewer_feedback_msg <- renderText({
        outputMsgs$reviewer_feedback
      })
      
      
      # return logic -----------------------------------------------------------
      
      observe({
        rvToReturn$allResultsGood <- all(unlist(reactiveValuesToList(resultsGood)))
      })
      
      return(rvToReturn)
      
    })
}
