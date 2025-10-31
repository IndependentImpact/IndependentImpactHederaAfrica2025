
dynamicMultiInput <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    ##useShinyjs(),
    
    actionButton(inputId = ns("abAdd"), label = "Add"),
    DT::dataTableOutput(outputId = ns("dtoItems")))
  
}

dynamicMultiServer <- function(id, 
                               nmUImod, nmSrvrMod,
                               btns = c("edit", "remove"),
                               lsArgsModUI = NULL,
                               lsArgsModSrvr = NULL) {
  
  # import plyr's rbind.fill
  rbind.fill <- plyr::rbind.fill
  
  moduleServer(
    id = id, 
    function(input, output, session) {
      
      ns <- session$ns
      
      rvToReturn <- reactiveValues()
      rvToReturn$goToModule <- NULL
      rvToReturn$items <- lsArgsModSrvr$lsPreset
      
      outputMsgs <- reactiveValues()
      outputMsgs$msg1 <- ""
      
      rvOther <- reactiveValues()
      rvOther$dtoNms <- NULL
      rvOther$idxEdit <- NA_integer_
      
      # TODO: Nonce-ing.
      
      # submodule servers ------------------------------------------------------
      
      if (is.null(lsArgsModSrvr)) {
        lsArgsModSrvr <- list()
      }
      lsArgsModSrvr$id <- "iMain" 
      subModRes <- do.call(what = nmSrvrMod, 
                           args = lsArgsModSrvr)
      
      # ui rendering -----------------------------------------------------------
      
      itemModal <- function(nmUImod, lsArgsModUI) {
        
        modalDialog(title = "Add item", 
                    do.call(what = nmUImod, 
                            args = lsArgsModUI),
                    footer = tagList(
                      actionButton(inputId = ns("abOK"), 
                                   label = "OK"),
                      actionButton(inputId = ns("abCancel"), 
                                   label = "Cancel")), 
                    size = "l", 
                    easyClose = FALSE)
        
      }
      
      # inputs -----------------------------------------------------------------
      
      observeEvent(input$abAdd, handlerExpr = {
        
        # Set up lsArgsModUI
        if (is.null(lsArgsModUI)) { lsArgsModUI <- list() }
        lsArgsModUI$id <- ns('iMain')
        lsArgsModUI$lsPreset <- NULL
        
        # Serve the subschema module's UI inside a modal
        showModal(
          itemModal(nmUImod = nmUImod, 
                    lsArgsModUI = lsArgsModUI))
        
      })
      
      observeEvent(input$abOK, handlerExpr = {
        
        if (!subModRes$allResultsGood) {
          outputMsgs$msg1 <- "Some required fields are missing or contain invalid values. Please review your response."  
          return(NULL)
        }
        
        outputMsgs$msg1 <- ""
        
        # Check against duplicates.
        if (length(rvToReturn$items) > 0) {
          # TODO.
        }
        
        # Update rvToReturn$items.
        if (!is.na(rvOther$idxEdit)) {
          rvToReturn$items[[rvOther$idxEdit]] <- recursiveReactiveValuesToList(subModRes$results)
          rvOther$idxEdit <- NA_integer_
        } else {
          rvToReturn$items[[length(rvToReturn$items) + 1]] <- recursiveReactiveValuesToList(subModRes$results)
        }
        
        removeModal()
        
      })
      
      observeEvent(input$abCancel, handlerExpr = {
        rvOther$idxEdit <- NA_integer_
        removeModal()
      })
      
      observeEvent(input$dtoItems_cell_clicked, handlerExpr = {
        
        res <- input$dtoItems_cell_clicked
        if (length(res) == 0) { return() }
        
        idxRow <- res$row
        idxCol <- res$col
        val <- res$value
        
        # If an 'Edit' button were clicked do:
        if (isolate(rvOther$dtoNms)[idxCol+1] == "btn_edit") {
          
          val <- strsplit(x = val, split = "[[:blank:]]")[[1]]
          val <- grep(pattern = "abEdit[[:digit:]]{1,}", x = val, value = TRUE)
          idx <- gsub(pattern = "[^[:digit:]]", replacement = "", x = val)
          idx <- as.integer(idx)
          
          rvOther$idxEdit <- idx
          
          # Serve this row for editing inside a modal.
          {
            # Set up lsArgsModUI
            if (is.null(lsArgsModUI)) { lsArgsModUI <- list() }
            lsArgsModUI$id <- ns('iMain')
            lsArgsModUI$lsPreset <- rvToReturn$items[[rvOther$idxEdit]]
            
            # Serve the subschema module's UI inside a modal.
            showModal(
              itemModal(nmUImod = nmUImod, 
                        lsArgsModUI = lsArgsModUI))
          }
          
          return()
        }
        
        # If a 'Remove' button were clicked do:
        if (isolate(rvOther$dtoNms)[idxCol+1] == "btn_rm") {

          val <- strsplit(x = val, split = "[[:blank:]]")[[1]]
          val <- grep(pattern = "abRm[[:digit:]]{1,}", x = val, value = TRUE)
          idx <- gsub(pattern = "[^[:digit:]]", replacement = "", x = val)
          idx <- as.integer(idx)
          
          rvToReturn$items <- rvToReturn$items[-idx]
          if (length(rvToReturn$items) == 0) { # Sometimes rvToReturn$items end up being stuff like 'character(0)'.
            rvToReturn$items <- NULL
          }
          
          return()
        }
      })
      
      # outputs ----------------------------------------------------------------
      
      output$dtoItems <- DT::renderDataTable(
        expr = {
          
          # TODO: Improve this by rather using dataTableProxy().
          
          if (length(rvToReturn$items) == 0) { return(NULL) }

          items <- rvToReturn$items
          
          #save(items, file = sprintf("%sitems.Rda", tmpdir)) # TODO. Remove after debugging.
          
          # Convert items to data frame.
          df <- do.call("rbind.fill", lapply(
            X = 1:length(items), FUN = function(idx) {
              idxxNull <- sapply(X = items[[idx]], FUN = function(y) length(y) == 0)
              x <- items[[idx]][!idxxNull]
              x <- lapply(X = x, FUN = function(z) {
                if (is.list(z)) { return("(nested list)") }
                return(z)
              })
              x <- as.data.frame(x)
              x$idx <- idx # Must assign the same idx val to each row of x, if x is a multi-row df.
              return(x)
            }))
          
          # Shorten long field text.
          for (k in 1:ncol(df)) {
            if (!(class(df[[k]])[1] == "character")) { next }
            idxx <- which(nchar(df[[k]]) > 47)
            df[[k]][idxx] <- substr(x = df[[k]][idxx], 
                                    start = 1, 
                                    stop = 47)
            df[[k]][idxx] <- sprintf("%s...", df[[k]][idxx])
          }

          # Add 'Edit' buttons.
          if ("edit" %in% btns) {
            df$btn_edit <- sprintf('<button id="abEdit%d" type="button" class="btn btn-default action-button">Edit</button>',
                                   df$idx)
            # << Rows with the same idx val must have the same actionButton ID.
          }
          
          # Add 'Remove' buttons
          if ("remove" %in% btns) {
            df$btn_rm <- sprintf('<button id="abRm%d" type="button" class="btn btn-default action-button">Remove</button>',
                                 df$idx)
            # << Rows with the same idx val must have the same actionButton ID.
          }
          
          rvOther$dtoNms <- names(df)
          
          return(df[,c("idx", setdiff(names(df), "idx"))])
        }, 
        escape = F, rownames = FALSE, selection = c('none', 'single')[1], 
        options = list(processing = FALSE, dom = "t", scrollX = TRUE))
      
      # return logic -----------------------------------------------------------
      
      return(rvToReturn)
      
    })
}




