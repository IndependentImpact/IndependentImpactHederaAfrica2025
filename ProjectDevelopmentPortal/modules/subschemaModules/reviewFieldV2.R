
reviewFieldV2UI <- function(id, 
                            fieldTitle,
                            fieldPrompt, 
                            decOpts = c('REJECT',
                                        'CORRECTIVE_ACTION_REQUEST', 
                                        'FORWARD_ACTION_REQUEST',
                                        'APPROVE'),
                            lblRevDecInpt = "Reviewer decision:",
                            lblRevFdbckInpt = "Reviewer feedback:",
                            useModal = FALSE, # Ignored
                            lsPreset = NULL, 
                            hL = 4, 
                            colWidth = 7,
                            inpWidth = DEFAULT_INP_WIDTH) {
  
  ns <- NS(id)
  
  tl <- tagList(
    
    #useShinyjs(),
    
    wellPanel(
      
      eval(parse(text = sprintf("h%d('%s')", max(1, hL-1), fieldTitle))),
      helpText(fieldPrompt),
      uiOutput(outputId = ns("uioOrigAnsw")),
      br(),
      actionButton(inputId = ns("abReview"), 
                   label = "Review"),
      br(),
      br(),
      textOutput(outputId = ns("toRevDec")),
      textOutput(outputId = ns("toRevFdbck")),
      br()))
  
  return(tl)
  
}

reviewFieldV2Server <- function(id, 
                                fieldKey, 
                                fieldTitle,
                                fieldPrompt,
                                fieldVal,
                                lsPreset = NULL,
                                useModal = FALSE, # Ignored.
                                decOpts = c('REJECT',
                                            'CORRECTIVE_ACTION_REQUEST', 
                                            'FORWARD_ACTION_REQUEST',
                                            'APPROVE'),
                                lblRevDecInpt = "Reviewer decision:",
                                lblRevFdbckInpt = "Reviewer feedback:") {
  
  rbind.fill <- plyr::rbind.fill
  
  moduleServer(
    id = id, 
    function(input, output, session) {
      
      ns <- session$ns
      
      rvToReturn <- reactiveValues()
      rvToReturn$goToModule <- NULL
      rvToReturn$allResultsGood <- FALSE
      rvToReturn$results <- reactiveValues(
        field_key = fieldKey,
        field_title = fieldTitle,
        field_prompt = fieldPrompt,
        original_response = fieldVal,
        reviewer_decision = lsPreset$reviewer_decision, 
        reviewer_feedback = lsPreset$reviewer_feedback)
      
      resultsGood <- reactiveValues()
      resultsGood$reviewer_decision <- .isReviewerDecisionGood(lsPreset$reviewer_decision)
      resultsGood$reviewer_feedback <- .isReviewerFeedbackGood(lsPreset$reviewer_feedback)
      
      outputMsgs <- reactiveValues()
      outputMsgs$reviewer_decision <- ""
      outputMsgs$reviewer_feedback <- ""
      
      rvOther <- reactiveValues()
      rvOther$dtoNms <- NULL
      rvOther$idxView <- NULL
      rvOther$nmFieldValOutput <- NULL
      
      # preset inputs ----------------------------------------------------------
      # None.
      
      # inputs -----------------------------------------------------------------
      
      hL <- 4
      tlRevFlds <- tagList(
        wellPanel(
          fluidRow(
            column(
              width = 12,
              wellPanel(
                hx(x = lblRevDecInpt, lvl = hL),
                helpText("Please choose an option:"),
                selectInput(
                  inputId = ns('reviewer_decision'), 
                  label = NULL, 
                  choices = decOpts, 
                  selected = lsPreset$reviewer_decision, 
                  multiple = FALSE, 
                  selectize = TRUE, 
                  #width = inpWidth, 
                  size = NULL)))),
          
          GtextAreaInput(id = ns("reviewer_feedback"), 
                         title = lblRevFdbckInpt, 
                         helpTxt = "Motivate your choice (5 chars. min.):",  
                         value = lsPreset$reviewer_feedback, 
                         resize = "vertical",
                         hL = hL, 
                         inpWidth = DEFAULT_INP_WIDTH,
                         colWidth = 12),
          
          textOutput(outputId = ns("toInvalidInput")),
          
          br(),
          fluidRow(
            column(width = 2),
            column(
              width = 4,
              actionButton(
                inputId = ns("abReviewModalOK"), 
                label = "OK", 
                width = "100%")),
            column(
              width = 4,
              actionButton(
                inputId = ns("abReviewModalCancel"), 
                label = "Cancel", 
                width = "100%")),
            column(width = 2))))
      
      observeEvent(input$abReview, handlerExpr = {

        showModal(
          modalDialog(
            title = sprintf("Review '%s'", fieldTitle), 
            tlRevFlds,
            footer = NULL, 
            size = "m", 
            easyClose = FALSE))
      })
      
      observeEvent(input$abReviewModalOK, handlerExpr = {
        if (!rvToReturn$allResultsGood) {
          return(invisible(0))
        }
        removeModal()
      })
      
      observeEvent(input$abReviewModalCancel, handlerExpr = {
        removeModal()
      })
      
      observeEvent(input$reviewer_decision, handlerExpr = {
        resultsGood$reviewer_decision <- FALSE
        rvToReturn$results$reviewer_decision <- input$reviewer_decision
        validate(need(input$reviewer_decision, message = FALSE))
        resultsGood$reviewer_decision <- .isReviewerDecisionGood(
          input$reviewer_decision)
      })
      
      observeEvent(input$reviewer_feedback, handlerExpr = {
        
        resultsGood$reviewer_feedback <- FALSE
        rvToReturn$results$reviewer_feedback <- input$reviewer_feedback
        
        nChar <- nchar(input$reviewer_feedback)
        outputMsgs$reviewer_feedback <- sprintf('%d chars. remaining', 1000 - nChar)
        
        resultsGood$reviewer_feedback <- .isReviewerFeedbackGood(
          input$reviewer_feedback)
        
      })
      
      observeEvent(input$dtoFieldVal_cell_clicked, handlerExpr = {
        
        res <- input$dtoFieldVal_cell_clicked
        if (length(res) == 0) { return() }
        
        idxCol <- res$col +1
        val <- res$value
        
        # If a 'View' button were clicked do:
        if (isolate(rvOther$dtoNms)[idxCol] == "btn_view") {
          
          val <- strsplit(x = val, split = "[[:blank:]]")[[1]]
          val <- grep(pattern = "abView[[:digit:]]{1,}", x = val, value = TRUE)
          idx <- gsub(pattern = "[^[:digit:]]", replacement = "", x = val)
          rvOther$idxView <- as.integer(idx)
          
          # Show modal rendering the item in a vto.
          showModal(
            modalDialog(
              verbatimTextOutput(outputId = ns("vtoView")),
              title = NULL,
              easyClose = TRUE,
              footer = NULL
            ))
        }
        
      })
      
      # outputs-----------------------------------------------------------------
      
      output$uioOrigAnsw <- renderUI({
        
        if (is.list(fieldVal)) {
          rvOther$nmFieldValOutput <- "dtoFieldVal"
          return(
            tagList(
              DT::dataTableOutput(outputId = ns("dtoFieldVal"))))
        }
        
        if (length(fieldVal) == 1 & ((
          is.character(fieldVal) | 
          is.numeric(fieldVal) | 
          is.logical(fieldVal) | 
          is.integer(fieldVal)))) {
          
          rvOther$nmFieldValOutput <- "toFieldVal"
          return(
            tagList(
              textOutput(outputId = ns("toFieldVal"))))
          
        }
        
        rvOther$nmFieldValOutput <- "vtoFieldVal"
        return(
          tagList(
            verbatimTextOutput(outputId = ns("vtoFieldVal"))))
        
      })
      
      output$toFieldVal <- renderText({
        
        if (rvOther$nmFieldValOutput != "toFieldVal") {
          return(NULL)
        }
        
        return(fieldVal)
        
      })
      
      output$vtoFieldVal <- renderPrint({
        
        if (rvOther$nmFieldValOutput != "vtoFieldVal") {
          return(NULL)
        }
        
        print(fieldVal)
        
      })
      
      output$dtoFieldVal <- DT::renderDataTable({
        
        if (rvOther$nmFieldValOutput != "dtoFieldVal") {
          return(NULL)
        }
        
        if (length(fieldVal) == 0) { # Can happen to optional fields.
          rvOther$dtoNms <- NULL
          return(NULL)
        }
        
        bOpt2 <- FALSE
        
        # Convert fieldVal to data frame.
        if (!is.data.frame(fieldVal)) {
          
          # First check if we could possibly convert it to a very nice, neat,
          # informative df. If not, just make a data frame with "(nested object)" entries.
          
          # To make a nice, neat, informative df, all items in fieldVal's list 
          # must be the same length.
          if (length(unique(sapply(X = fieldVal, FUN = length))) == 1) {
            
            # Then all items in fieldVal's list must have names for the items inside of them.
            if (all(sapply(X = fieldVal, FUN = function(xx) length(names(xx))) > 0)) {
              
              # Those names should furthermore all be the same across all the items in fieldVal.
              if (all(sapply(X = fieldVal[-1], FUN = function(xx) {
                names(xx) == names(fieldVal[[1]])
              }))) {
                # Yay! We can make a nice, neat, informative df :)
                fieldVal <- lapply(X = fieldVal, FUN = data.frame)
                fieldVal <- do.call("rbind.fill", fieldVal)
                df <- fieldVal
                df$idx <- 1:nrow(df)
              }
              
            } else {
              bOpt2 <- TRUE
            }
            
          } else { # Oh well, at least we tried.
            bOpt2 <- TRUE
          }
          
          if (bOpt2) {
            
            fieldVal <- lapply(X = fieldVal, FUN = function(x) {
              if (!is.list(x)) { return(x) }
              return("(nested object)")
            })
            
            if (length(names(fieldVal)) > 0) {
              
              df <- data.frame(field = names(fieldVal),
                               value = sapply(X = fieldVal, FUN = function(x) {
                                 if (length(x) == 1) { return(x) }
                                 if (length(x) == 0) { return("(empty)") }
                                 return(paste(x, collapse = " "))
                               }))
              df$idx <- 1:nrow(df)
              
            } else {
              df <- data.frame(value = paste(unlist(fieldVal), collapse = " "),
                               idx = 1)
            }
            
          }
          
        } else {
          df <- fieldVal
          df$idx <- 1:nrow(df)
        }
        
        rownames(df) <- NULL
        
        # If df has more than 7 columns (other than the "idx" column), just 
        # display the first 6 and hide the rest.
        if (ncol(df) > 8) {
          df <- df[,c("idx", setdiff(names(df), "idx"))]
          nCol <- ncol(df) -1 # -1 for 'idx' col that will be removed soon
          df <- df[,1:7] # 7 = 6 + 1(idx)
          df[[sprintf("(%d more columns)", nCol - 6)]] <- "..." 
        }
        
        # Shorten long text fields (for display purposes only).
        for (k in 1:ncol(df)) {
          if (!(class(df[[k]])[1] == "character")) { next }
          idxx <- which(nchar(df[[k]]) > 47)
          df[[k]][idxx] <- substr(x = df[[k]][idxx], 
                                  start = 1, 
                                  stop = 47)
          df[[k]][idxx] <- sprintf("%s...", df[[k]][idxx])
        }
        
        # Add 'View' buttons.
        df$btn_view <- sprintf('<button id="abView%d" type="button" class="btn btn-default action-button">View</button>',
                               df$idx)
        # << Rows with the same idx val must have the same actionButton ID.
        
        df <- df[,setdiff(names(df), "idx")]
        rvOther$dtoNms <- names(df)
        
        return(df)
      }, 
      escape = F, rownames = FALSE, selection = c("none", "single")[1],
      options = list(processing = FALSE, dom = "t", scrollX = TRUE))
      
      output$vtoView <- renderPrint({
        validate(need(rvOther$idxView, message = FALSE))
        fieldVal[rvOther$idxView]
      })
      
      output$reviewer_decision_msg <- renderText({
        outputMsgs$reviewer_decision
      })
      
      output$reviewer_feedback_msg <- renderText({
        outputMsgs$reviewer_feedback
      })
      
      output$toRevDec <- renderText({
        sprintf("Reviewer decision: %s",
                rvToReturn$results$reviewer_decision)
      })
      
      output$toRevFdbck <- renderText({
        txt <- rvToReturn$results$reviewer_feedback
        if (length(txt) == 0) { 
          txt <- "" 
        } else {
          txt <- shortText(x = txt, lim = 200)
        }
        sprintf("Reviewer feedback: %s", txt)
      })
      
      output$toInvalidInput <- renderText({
        
        if (rvToReturn$allResultsGood) { return(NULL) }
        
        return(
          sprintf("Your inputs to the following fields are not valid: %s",
                  paste(names(resultsGood)[!unlist(reactiveValuesToList(resultsGood))], sep = "", collapse = ", ")))
        
      })
      
      # module return logic ----------------------------------------------------
      
      observe({
        allResGood <- all(unlist(reactiveValuesToList(resultsGood)))
        if (allResGood) {
          
          # This is a quick way to make sure "original_response" has a value even when
          # no response was originally provided by the user.
          if (is.null(emptyOrMissingAsNull(isolate(rvToReturn$results$original_response)))) {
            rvToReturn$results$original_response <- "(none)"
          }
          
          # Always store the original response as JSON.
          rvToReturn$results$original_response <- jsonlite::toJSON(
            x = isolate(rvToReturn$results$original_response),
            dataframe = "rows", 
            Date = "ISO8601", 
            factor = "string", 
            null = "list", 
            na = "string", 
            auto_unbox = TRUE, 
            pretty = FALSE)
        }
        rvToReturn$allResultsGood <- allResGood
      })
      
      return(rvToReturn)
      
    })
}

.isReviewerDecisionGood <- function(x) {
  return(!is.null(emptyOrMissingAsNull(x)))
}

.isReviewerFeedbackGood <- function(x) {
  return(
    isValidInput.text(
      x = x, 
      bRequired = TRUE, 
      nCharMin = 5, 
      nCharMax = 1000))
}  
