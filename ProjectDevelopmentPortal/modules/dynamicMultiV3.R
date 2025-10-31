
# The reactivity of this one does not work so well.

#' 
#' dynamicMultiV3Input <- function(id,
#'                                 modalSize = c("small", "large")[2]) {
#' 
#'   ns <- NS(id)
#'  
#'   tagList(
#'     
#'     #useShinyjs(),
#'     textOutput(outputId = ns("toMain")),
#'     br(),
#'     actionButton(inputId = ns("abAdd"), label = "Add"),
#'     br(),
#'     DT::dataTableOutput(outputId = ns("dtoItems")),
#'     
#'     shinyBS::bsModal(id = ns("itemModal"), 
#'                      title = "Add/Edit Item", 
#'                      trigger = NULL, 
#'                      size = modalSize,
#'                      uiOutput(outputId = ns("uioSubModuleUI")),
#'                      actionButton(inputId = ns("abItemModalOk"), 
#'                                   label = "OK"),
#'                      actionButton(inputId = ns("abItemModalCancel"), 
#'                                   label = "Cancel")))
#'   
#' }
#' 
#' #'@param lsPreset This is assigned to rvToReturn$items. To send preset values to
#' #' the submodule's UI, use lsArgsModUI$lsPreset. To send preset values to the 
#' #' submodule's server, use lsArgsModSrvr$lsPreset.
#' dynamicMultiV3Server <- function(id, 
#'                                  nmUImod, 
#'                                  nmSrvrMod,
#'                                  btns = c("edit", "remove"),
#'                                  lsPreset = NULL,
#'                                  lsArgsModUI = NULL, 
#'                                  lsArgsModSrvr = NULL) {
#'   
#'   # import plyr's rbind.fill
#'   rbind.fill <- plyr::rbind.fill
#'   
#'   moduleServer(
#'     id = id, 
#'     function(input, output, session) {
#'       
#'       ns <- session$ns
#'       
#'       rvToReturn <- reactiveValues()
#'       rvToReturn$goToModule <- NULL
#'       rvToReturn$items <- lsPreset
#'       
#'       outputMsgs <- reactiveValues()
#'       
#'       rvOther <- reactiveValues()
#'       rvOther$dtoNms <- NULL
#'       rvOther$idxEdit <- NA_integer_
#'       rvOther$nonce <- 0
#'       rvOther$idSubModule <- sprintf("mSubModuleX%d", isolate(rvOther$nonce))
#'       rvOther$lsPresetSrvr <- lsArgsModSrvr$lsPreset
#'       rvOther$showModal <- FALSE
#'       rvOther$initialised <- FALSE
#'       
#'       # module servers ---------------------------------------------------------
#'       
#'       modSrvrs <- reactiveValues()
#'       #modSrvrs$mSubModuleX <- NULL
#' 
#'       # inputs -----------------------------------------------------------------
#'       
#'       observe({
#'         rvOther$idSubModule <- sprintf("mSubModuleX%d", rvOther$nonce)
#'       })
#'       
#'       observe({
#'       
#'         if (rvOther$showModal) {
#'           
#'           lsArgs <- lsArgsModSrvr
#'           if (is.null(lsArgs)) { lsArgs <- list() }
#'           lsArgs$id <- isolate(rvOther$idSubModule)
#'           lsArgs$lsPreset <- isolate(rvOther$lsPresetSrvr)
#'           
#'           message("Server ID: ", isolate(rvOther$idSubModule))
#'           
#'           modSrvrs[[isolate(rvOther$idSubModule)]] <- do.call(
#'             what = nmSrvrMod, 
#'             args = lsArgs)
#'           
#'           output$uioSubModuleUI <- renderUI({
#'             
#'             message("UI ID: ", isolate(rvOther$idSubModule))
#'             
#'             lsArgs <- lsArgsModUI
#'             if (is.null(lsArgs)) { lsArgs <- list() }
#'             lsArgs$id <- ns(isolate(rvOther$idSubModule))
#'             lsArgs$lsPreset <- NULL
#'             
#'             do.call(what = nmUImod, 
#'                     args = lsArgs)
#'             
#'           })
#'           
#'           shinyBS::toggleModal(
#'             session = session,
#'             modalId = "itemModal",
#'             toggle = "open")
#'           
#'           return(invisible(0))
#'           
#'         }
#'         
#'         shinyBS::toggleModal(
#'           session = session, 
#'           modalId = "itemModal", 
#'           toggle = "close")
#'         output$uioSubModuleUI <- renderUI({ NULL })
#'         modSrvrs[[isolate(rvOther$idSubModule)]] <- NULL
#'         rvOther$nonce <- isolate(rvOther$nonce +1)
#'         
#'       })
#'       
#'       observeEvent(input$abAdd, handlerExpr = {
#'         if (!rvOther$initialised) { return(invisible(0)) }
#'         rvOther$lsPresetSrvr <- NULL
#'         rvOther$showModal <- TRUE
#'       })
#'       
#'       observeEvent(input$abItemModalOk, handlerExpr = {
#'         
#'         if (!modSrvrs[[rvOther$idSubModule]]$allResultsGood) {
#'           return(NULL)
#'         }
#'         
#'         # Check against duplicates.
#'         if (length(rvToReturn$items) > 0) {
#'           # TODO.
#'         }
#'         
#'         # Update rvToReturn$items.
#'         if (!is.na(rvOther$idxEdit)) {
#'           rvToReturn$items[[rvOther$idxEdit]] <- recursiveReactiveValuesToList(
#'             modSrvrs[[rvOther$idSubModule]]$results)
#'           rvOther$idxEdit <- NA_integer_
#'         } else {
#'           rvToReturn$items[[length(rvToReturn$items) + 1]] <- recursiveReactiveValuesToList(
#'             modSrvrs[[rvOther$idSubModule]]$results)
#'         }
#'         
#'         rvOther$showModal <- FALSE
#'         
#'       })
#'       
#'       observeEvent(input$abItemModalCancel, handlerExpr = {
#'         rvOther$idxEdit <- NA_integer_
#'         rvOther$showModal <- FALSE
#'       })
#'       
#'       observeEvent(input$dtoItems_cell_clicked, handlerExpr = {
#'         
#'         res <- input$dtoItems_cell_clicked
#'         if (length(res) == 0) { return() }
#'         
#'         idxRow <- res$row
#'         idxCol <- res$col
#'         val <- res$value
#'         
#'         # If an 'Edit' button were clicked do:
#'         if (isolate(rvOther$dtoNms)[idxCol+1] == "btn_edit") {
#'           
#'           val <- strsplit(x = val, split = "[[:blank:]]")[[1]]
#'           val <- grep(pattern = "abEdit[[:digit:]]{1,}", x = val, value = TRUE)
#'           idx <- gsub(pattern = "[^[:digit:]]", replacement = "", x = val)
#'           idx <- as.integer(idx)
#'           
#'           rvOther$idxEdit <- idx
#'           
#'           # Serve this row for editing inside a modal.
#'           rvOther$lsPresetSrvr <- rvToReturn$items[[rvOther$idxEdit]]
#'           rvOther$showModal <- TRUE
#' 
#'           return()
#'         }
#'         
#'         # If a 'Remove' button were clicked do:
#'         if (isolate(rvOther$dtoNms)[idxCol+1] == "btn_rm") {
#'           
#'           val <- strsplit(x = val, split = "[[:blank:]]")[[1]]
#'           val <- grep(pattern = "abRm[[:digit:]]{1,}", x = val, value = TRUE)
#'           idx <- gsub(pattern = "[^[:digit:]]", replacement = "", x = val)
#'           idx <- as.integer(idx)
#'           
#'           rvToReturn$items <- rvToReturn$items[-idx]
#'           if (length(rvToReturn$items) == 0) { # Sometimes rvToReturn$items end up being stuff like 'character(0)'.
#'             rvToReturn$items <- NULL
#'           }
#'           
#'           return()
#'         }
#'       })
#'       
#'       # outputs ----------------------------------------------------------------
#'       
#'       output$toMain <- renderText({
#'         if (length(rvToReturn$items) == 0) { return("No items have been added yet.") }
#'         return(NULL)
#'       })
#'       
#'       output$dtoItems <- DT::renderDataTable(
#'         expr = {
#'           
#'           # TODO: Improve this by rather using dataTableProxy().
#'           
#'           if (length(rvToReturn$items) == 0) { return(NULL) }
#'           
#'           items <- rvToReturn$items
#'           
#'           # Convert items to data frame.
#'           df <- do.call("rbind.fill", lapply(
#'             X = 1:length(items), FUN = function(idx) {
#'               idxxNull <- sapply(X = items[[idx]], FUN = function(y) length(y) == 0)
#'               x <- items[[idx]][!idxxNull]
#'               x <- lapply(X = x, FUN = function(z) {
#'                 if (is.list(z)) { return("(nested list)") }
#'                 return(z)
#'               })
#'               x <- as.data.frame(x)
#'               x$idx <- idx # Must assign the same idx val to each row of x, if x is a multi-row df.
#'               return(x)
#'             }))
#'           
#'           # Shorten long field text.
#'           for (k in 1:ncol(df)) {
#'             if (!(class(df[[k]])[1] == "character")) { next }
#'             idxx <- which(nchar(df[[k]]) > 47)
#'             df[[k]][idxx] <- substr(x = df[[k]][idxx], 
#'                                     start = 1, 
#'                                     stop = 47)
#'             df[[k]][idxx] <- sprintf("%s...", df[[k]][idxx])
#'           }
#'           
#'           # Add 'Edit' buttons.
#'           if ("edit" %in% btns) {
#'             df$btn_edit <- sprintf('<button id="abEdit%d" type="button" class="btn btn-default action-button">Edit</button>',
#'                                    df$idx)
#'             # << Rows with the same idx val must have the same actionButton ID.
#'           }
#'           
#'           # Add 'Remove' buttons
#'           if ("remove" %in% btns) {
#'             df$btn_rm <- sprintf('<button id="abRm%d" type="button" class="btn btn-default action-button">Remove</button>',
#'                                  df$idx)
#'             # << Rows with the same idx val must have the same actionButton ID.
#'           }
#'           
#'           rvOther$dtoNms <- names(df)
#'           
#'           return(df[,c("idx", setdiff(names(df), "idx"))])
#'         }, 
#'         escape = F, rownames = FALSE, selection = c('none', 'single')[1], 
#'         options = list(processing = FALSE, dom = "t", scrollX = TRUE))
#'       
#'       # return logic -----------------------------------------------------------
#'       
#'       rvOther$initialised <- TRUE
#'       
#'       return(rvToReturn)
#'       
#'     })
#' }
#' 
#' 
#' 
#' 
