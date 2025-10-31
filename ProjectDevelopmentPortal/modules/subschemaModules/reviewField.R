
reviewFieldUI <- function(id, 
                          origFldTitle,
                          origFldHelpTxt = "",
                          lsPreset = NULL, 
                          hL = 4, 
                          colWidth = 7,
                          inpWidth = DEFAULT_INP_WIDTH) {
  
  ns <- NS(id)
  
  tagList(
    
    #useShinyjs(),
    
    wellPanel(
      eval(parse(text = sprintf("h%d('%s')", max(1, hL-1), origFldTitle))),
      helpText(origFldHelpTxt),
      uiOutput(outputId = ns("uioOrigAnsw")),
      br(),
      actionButton(inputId = ns("abReview"), 
                   label = "Review"),
      textOutput(outputId = ns("rev_decision")),
      textOutput(outputId = ns("rev_feedback"))))
  
}

reviewFieldServer <- function(id, 
                              fieldKey, 
                              fieldVal, 
                              decOpts = c('APPROVE', 
                                          'FORWARD_ACTION_REQUEST',
                                          'CORRECTIVE_ACTION_REQUEST', 
                                          'REJECT'),
                              lblRevDecInpt = "Reviewer decision:",
                              lblRevFdbckInpt = "Reviewer feedback:",
                              lsPreset = NULL) {
  
  rbind.fill <- plyr::rbind.fill
  
  moduleServer(
    id = id, 
    function(input, output, session) {
      
      ns <- session$ns
      
      rvToReturn <- reactiveValues()
      rvToReturn$goToModule <- NULL
      rvToReturn$allResultsGood <- FALSE
      rvToReturn$results <- reactiveValues(
        fieldKey = fieldVal,
        rev_decision = lsPreset$rev_decision, 
        rev_feedback = lsPreset$rev_feedback)
      
      resultsGood <- reactiveValues()
      resultsGood$rev_decision <- FALSE
      resultsGood$rev_feedback <- FALSE
      
      outputMsgs <- reactiveValues()
      outputMsgs$siRevDec <- ""
      outputMsgs$taiRevFdbck <- ""
      
      rvOther <- reactiveValues()
      rvOther$dtoNms <- NULL
      rvOther$idxView <- NULL
      rvOther$nmFieldValOutput <- NULL
      
      # preset inputs ----------------------------------------------------------
      # None.
      
      # inputs -----------------------------------------------------------------
      
      observeEvent(input$siRevDec, handlerExpr = {
        resultsGood$rev_decision <- FALSE
        rvToReturn$results$rev_decision <- input$siRevDec
        validate(need(input$siRevDec, message = FALSE))
        resultsGood$rev_decision <- TRUE
      })
      
      observeEvent(input$taiRevFdbck, handlerExpr = {
        
        resultsGood$rev_feedback <- FALSE
        rvToReturn$results$rev_feedback <- input$taiRevFdbck
        
        nChar <- nchar(input$taiRevFdbck)
        if (nChar == 0) {
          msg <- "5 chars. min., 1000 chars. max."
        } else {
          if (nChar < 5) {
            msg <- "5 chars. min."
          } else {
            if (nChar > 1000) {
              msg <- sprintf("%d chars. remaining (1000 chars. max.)", 1000 - nChar)
            } else {
              msg <- sprintf("%d chars. remaining.", 1000 - nChar)
            } 
          }
        }
        outputMsgs$taiRevFdbck <- msg
        
        resultsGood$rev_feedback <- isValidInput.text(
          x = input$taiRevFdbck, 
          bRequired = TRUE, 
          nCharMin = 5, 
          nCharMax = 1000)
        
      })
      
      observeEvent(input$abReview, handlerExpr = {
   
        hL <- 5
        colWidth <- 12
        inpWidth <- DEFAULT_INP_WIDTH
        
        showModal(
          modalDialog(
            title = ,
            tagList(
              
              # GselectInput(id = ns("siRevDec"),
              #              title = lblRevDecInpt,
              #              choices = decOpts,
              #              helpTxt = NULL, # TODO.
              #              selected = rvToReturn$results$rev_decision,
              #              multiple = FALSE,
              #              hL = hL,
              #              colWidth = colWidth,
              #              inpWidth = inpWidth),
              
              fluidRow(
                column(
                  width = colWidth,
                  wellPanel(
                    hx(x = lblRevDecInpt, lvl = hL),
                    helpText("Please choose an option."),
                    selectInput(
                      inputId = ns('siRevDec'), 
                      label = NULL, 
                      choices = decOpts, 
                      selected = rvToReturn$results$rev_decision, 
                      multiple = FALSE, 
                      selectize = TRUE, 
                      width = inpWidth, 
                      size = NULL)))),
              
              
              GtextAreaInput(id = ns("taiRevFdbck"), 
                             title = lblRevFdbckInpt, 
                             helpTxt = NULL, # TODO.  
                             value = rvToReturn$results$rev_feedback, 
                             resize = "vertical",
                             hL = hL, 
                             colWidth = colWidth,
                             inpWidth = inpWidth)),
            footer = modalButton(label = "OK"), 
            size = "m", 
            easyClose = FALSE))

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
                fieldVal <- do.call("rbind", fieldVal)
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
      
      output$siRevDec_msg <- renderText({
        outputMsgs$siRevDec
      })
      
      output$taiRevFdbck_msg <- renderText({
        outputMsgs$taiRevFdbck
      })
      
      output$rev_decision <- renderText({
        sprintf("Reviewer decision: %s",
                rvToReturn$results$rev_decision)
      })
      
      output$rev_feedback <- renderText({
        sprintf("Reviewer feedback: %s",
                rvToReturn$results$rev_feedback)
      })
      
      # module return logic ----------------------------------------------------
      
      observe({
        rvToReturn$allResultsGood <- all(unlist(reactiveValuesToList(resultsGood)))
      })
      
      return(rvToReturn)
      
    })
}

