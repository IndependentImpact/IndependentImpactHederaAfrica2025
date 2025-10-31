
#'@param modalSize Ignored if 'useModal' = FALSE.
#'@param nmUImod Ignored when 'useModal' = TRUE.
#'@param lsArgsModUI Ignored when 'useModal' = TRUE.
#'
dynamicMultiV2Input <- function(id, 
                                nmUImod = NULL, 
                                lsArgsModUI = NULL,
                                useModal = FALSE,
                                modalSize = c("small", "large")[2]) {
  
  ns <- NS(id)

  tl <- tagList(
    
    ##useShinyjs(),
    textOutput(outputId = ns("toMain")),
    br(),
    actionButton(inputId = ns("abAdd"), label = "Add"),
    br(),
    DT::dataTableOutput(outputId = ns("dtoItems")),
    br())
  
  if (useModal) {
    
    if (is.null(lsArgsModUI)) { lsArgsModUI <- list() }
    lsArgsModUI$lsPreset <- NULL
    lsArgsModUI$id <- ns(sprintf("mSubModuleX%d", rvOther$nonce)) # ns("mSubModuleX")
    
    tl <- c(tl, tagList(
      shinyBS::bsModal(id = ns("itemModal"), 
                       title = "Add/Edit Item", 
                       trigger = NULL, 
                       size = modalSize,
                       do.call(what = nmUImod, 
                               args = lsArgsModUI),
                       actionButton(inputId = ns("abAddItemModalOk"), 
                                    label = "OK"),
                       actionButton(inputId = ns("abAddItemModalCancel"), 
                                    label = "Cancel"))))
    
  } else {
    
    tl <- c(tl, tagList(
    
      uiOutput(outputId = ns("uioItem"))))
    
  }
  
  return(tl)
}

#'@param lsPreset This is assigned to rvToReturn$items. To send preset values to 
#'  the submodule's server, use lsArgsModSrvr$lsPreset.
#'@param nmUImod Ignored when 'useModal' = TRUE.
#'@param lsArgsModUI Ignored when 'useModal' = TRUE.
dynamicMultiV2Server <- function(id, 
                                 nmSrvrMod,
                                 btns = c("edit", "remove"),
                                 lsPreset = NULL,
                                 lsArgsModSrvr = NULL,
                                 useModal = FALSE,
                                 nmUImod = NULL,
                                 lsArgsModUI = NULL) {
  
  # import plyr's rbind.fill
  rbind.fill <- plyr::rbind.fill
  
  moduleServer(
    id = id, 
    function(input, output, session) {
      
      ns <- session$ns

      rvToReturn <- reactiveValues()
      rvToReturn$goToModule <- NULL
      rvToReturn$items <- lsPreset
      
      outputMsgs <- reactiveValues()
      outputMsgs$msg1 <- ""
      
      rvOther <- reactiveValues()
      rvOther$dtoNms <- NULL
      rvOther$idxEdit <- NA_integer_
      rvOther$nonce <- 0
      rvOther$initId1 <- Sys.time()
      rvOther$initId2 <- 0
      rvOther$idSrvrMod <- sprintf("mSubModuleX%d", isolate(rvOther$nonce))
      rvOther$operationInProgress <- TRUE
      
      # module servers ------------------------------------------------------
      
      modSrvrs <- reactiveValues()
      #modSrvrs$mSubModuleX <- NULL

      # inputs -----------------------------------------------------------------
      
      observe({
        rvOther$idSrvrMod <- sprintf("mSubModuleX%d", rvOther$nonce)
      })
      
      observeEvent(input$abAdd, handlerExpr = {
        
        if (rvOther$initId2 < rvOther$initId1) { return(invisible(0)) }
        
        if (rvOther$operationInProgress) {
          return(invisible(0))
        }
        
        rvOther$operationInProgress <- TRUE
        
        lsArgs <- lsArgsModSrvr
        if (is.null(lsArgs)) { lsArgs <- list() }
        lsArgs$id <- sprintf("mSubModuleX%d", rvOther$nonce) # "mSubModuleX"
        lsArgs$lsPreset <- NULL
        
        modSrvrs[[rvOther$idSrvrMod]] <- do.call(
          what = nmSrvrMod, 
          args = lsArgs)
        
        
        if (useModal) {
          
          message("using modal")
          
          shinyBS::toggleModal(
            session = session, 
            modalId = "itemModal", 
            toggle = "open")
          
        } else {
          
          message("not using modal")
          
          lsArgs <- lsArgsModUI
          if (is.null(lsArgs)) { lsArgs <- list() }
          lsArgs$lsPreset <- NULL
          lsArgs$id <- ns(sprintf("mSubModuleX%d", rvOther$nonce)) # ns("mSubModuleX")

          output$uioItem <- renderUI({
            tagList(
              fluidRow(
                column(width = 1),
                column(
                  width = 10,
                  wellPanel(hx("Add Item", lvl = 4)),
                  do.call(what = nmUImod, args = lsArgs),
                  actionButton(inputId = ns("abAddItemModalOk"), 
                               label = "OK"),
                  actionButton(inputId = ns("abAddItemModalCancel"), 
                               label = "Cancel")),
                column(width = 1)))
          })
        }
        
      })
      
      observeEvent(input$abAddItemModalOk, handlerExpr = {
        
        if (!modSrvrs[[rvOther$idSrvrMod]]$allResultsGood) {
          outputMsgs$msg1 <- "Some required fields are missing or contain invalid values. Please review your response."  
          return(NULL)
        }
        
        outputMsgs$msg1 <- ""
        
        # Check against duplicates.
        if (length(rvToReturn$items) > 0) {
          # TODO.
        }
        
        # Update rvToReturn$items.
        itemToAdd <- recursiveReactiveValuesToList(modSrvrs[[rvOther$idSrvrMod]]$results)
        if (!is.na(rvOther$idxEdit)) {
          rvToReturn$items[[rvOther$idxEdit]] <- itemToAdd
          rvOther$idxEdit <- NA_integer_
        } else {
          rvToReturn$items[[length(rvToReturn$items) + 1]] <- itemToAdd
        }
        
        #removeModal()
        
        if (useModal) {
          shinyBS::toggleModal(
            session = session, 
            modalId = "itemModal", 
            toggle = "close")
        } else {
          output$uioItem <- renderUI({ NULL })
        }

        modSrvrs[[rvOther$idSrvrMod]] <- NULL
        rvOther$nonce <- rvOther$nonce +1
        rvOther$operationInProgress <- FALSE
      })
      
      observeEvent(input$abAddItemModalCancel, handlerExpr = {
        
        rvOther$idxEdit <- NA_integer_
        
        if (useModal) {
          shinyBS::toggleModal(
            session = session, 
            modalId = "itemModal", 
            toggle = "close")
        } else {
          output$uioItem <- renderUI({ NULL })
        }
        
        modSrvrs[[rvOther$idSrvrMod]] <- NULL
        rvOther$nonce <- rvOther$nonce + 1
        rvOther$operationInProgress <- FALSE
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
            lsArgs <- lsArgsModSrvr
            if (is.null(lsArgs)) { lsArgs <- list() }
            lsArgs$id <- sprintf("mSubModuleX%d", rvOther$nonce) # "mSubModuleX"
            lsArgs$lsPreset <- rvToReturn$items[[rvOther$idxEdit]]

            modSrvrs[[rvOther$idSrvrMod]] <- do.call(
              what = nmSrvrMod, 
              args = lsArgs)
            
            if (useModal) {
              
              shinyBS::toggleModal(
                session = session, 
                modalId = "itemModal", 
                toggle = "open")
              
            } else {
              
              lsArgs <- lsArgsModUI
              if (is.null(lsArgs)) { lsArgs <- list() }
              lsArgs$lsPreset <- NULL
              lsArgs$id <- ns(sprintf("mSubModuleX%d", rvOther$nonce)) # ns("mSubModuleX")
              
              output$uioItem <- renderUI({
                tagList(
                  fluidRow(
                    column(width = 1),
                    column(
                      width = 10,
                      wellPanel(hx("Edit Item", lvl = 4)),
                      do.call(what = nmUImod, args = lsArgs),
                      actionButton(inputId = ns("abAddItemModalOk"), 
                                   label = "OK"),
                      actionButton(inputId = ns("abAddItemModalCancel"), 
                                   label = "Cancel")),
                    column(width = 1)))
              })
            }
            
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
      
      output$toMain <- renderText({
        if (length(rvToReturn$items) == 0) { return("No items have been added yet.") }
        return(NULL)
      })
      
      output$dtoItems <- DT::renderDataTable(
        expr = {
          
          # TODO: Improve this by rather using dataTableProxy().
          
          if (length(rvToReturn$items) == 0) { return(NULL) }
          
          items <- rvToReturn$items

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
      
      # UI init ----------------------------------------------------------------
      
      rvOther$operationInProgress <- FALSE
      rvOther$initId2 <- Sys.time()
      
      # return logic -----------------------------------------------------------
      
      return(rvToReturn)
      
    })
}




