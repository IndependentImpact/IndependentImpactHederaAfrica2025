
#'@description Generates the code for a Shiny module based on schema information
#'  retrieved via the Guardian API.
#'@param schema One of the items in the list returned by Guardener::GgetSchemas()
#'@param subject The subject of the schema. One of c('agent','activity').
makeSchemaModule <- function(schemaAbbr, schema, savedir, subject) {
  
  dfSchema <- schemaToDf(schema)
  
  dfSchema$input_tp_shiny <- sapply(X = dfSchema$type, FUN = function(x) {
    switch(EXPR = x,
           string = "GtextAreaInput",
           number = "GnumericInput",
           boolean = "GselectInput",
           integer = "GnumericInput",
           NA_character_)
  })
  
  idxx <- which(dfSchema$repeated & dfSchema$type == "complex")
  dfSchema$input_tp_shiny[idxx] <- "GdynamicMultiInput"
  
  dfSchema$input_tp_shiny[which(!is.na(dfSchema$enum))] <- "GselectInput"
  
  cat(file = sprintf("%s/%sschema.R", savedir, schemaAbbr),
  
sprintf(  
"
# Module for schema '%s [v%s]'.

%sschemaUI <- function(id, 
                        lsPreset = NULL, 
                        hL = 4, 
                        colWidth = 12, 
                        inpWidth = DEFAULT_INP_WIDTH,
                        idSchemaV) {
  
  ns <- NS(id)

  tagList(
    
    ##useShinyjs(),
    
    %s)
}

%sschemaServer <- function(id,  
                           dbCon,
                           idAgent,
                           idEntity = NULL,
                           idPolicyV, 
                           idSchemaV,
                           GaccTknSb,
                           lsPreset = NULL) {
  
  moduleServer(
    id = id, 
    function(input, output, session) {
      
      ns <- session$ns
      
      rvToReturn <- reactiveValues()
      rvToReturn$allResultsGood <- FALSE
      rvToReturn$results <- reactiveValues(
        %s)
      
      resultsGood <- reactiveValues()
      %s
      
      outputMsgs <- reactiveValues()
      %s
      
      rvOther <- reactiveValues()

      # preset inputs ----------------------------------------------------------
      
      %s
      
      # module servers ---------------------------------------------------------
      
      modSrvrs <- reactiveValues()
      
      %s
      
      # inputs -----------------------------------------------------------------

      %s
      
      # outputs ----------------------------------------------------------------

      %s
      
      # return logic -----------------------------------------------------------
      
      observe({
        rvToReturn$allResultsGood <- all(unlist(reactiveValuesToList(resultsGood)))
      })
      
      return(rvToReturn)
      
    })
}
", 

schema$name,
schema$version,

schemaAbbr, 

paste(
  sapply(X = 1:nrow(dfSchema), FUN = function(r) {
    return(.makeUIinputTxt(dfSchema[r,]))
  }), 
  collapse = ",\n\n"),

schemaAbbr,
paste(sprintf("%s = NULL", dfSchema$key), collapse = ",\n"),
paste(sprintf("resultsGood$%s <- FALSE", dfSchema$key), collapse = "\n"),
paste(sprintf("outputMsgs$%s <- ''", dfSchema$key), collapse = "\n"),

paste(
  sapply(X = 1:nrow(dfSchema), FUN = function(r) {
    return(.makeSrvrPresetInptTxt(dfSchema[r,]))
  }), 
  collapse = "\n\n"),

paste(
  unlist(sapply(X = 1:nrow(dfSchema), FUN = function(r) {
    return(.makeModSrvrTxt(dfSchema[r,]))
  })), 
  collapse = "\n\n"),

paste(
  unlist(sapply(X = 1:nrow(dfSchema), FUN = function(r) {
    return(.makeInptObsrvr(dfSchema[r,]))
  })), 
  collapse = "\n\n"),

paste(
  unlist(sapply(X = 1:nrow(dfSchema), FUN = function(r) {
    return(.makeOutptRndr(dfSchema[r,]))
  })), 
  collapse = "\n\n")
))
}

# Auxiliary functions ----------------------------------------------------------

.makeUIinputTxt <- function(x) {
  
  idStr <- sprintf("id = ns('%s')", x$key)
  
  titleStr <- sprintf("title = '%s'", x$title)
  
  helpTxtStr <- sprintf("helpTxt = '%s'", x$description)
  
  chcsStr <- ""
  if (!is.na(x$input_tp_shiny)) {
    if (x$input_tp_shiny == "GselectInput") {
      chcsStr <-  sprintf(
        "choices = c(%s)", 
        paste(sprintf("'%s'", strsplit(x = x$enum, split = ",")[[1]]), 
              collapse = ", "))
    }
  }
  
  valStr <- ""
  if (!is.na(x$input_tp_shiny)) {
    if (x$input_tp_shiny != "GdynamicMultiInput") {
      valStr <- sprintf("%s = lsPreset$%s", 
                        ifelse(x$input_tp_shiny == "GselectInput", 
                               "selected", 
                               "value"),
                        x$key)
    }
  }
  
  resizeStr <- ""
  if (!is.na(x$input_tp_shiny)) {
    if (x$input_tp_shiny == "GtextAreaInput") {
      resizeStr <- "resize = 'vertical'"
    }
  }
  
  otherStr <- "hL = hL,\ncolWidth = colWidth,\ninpWidth = inpWidth"
  
  argStrs <- c(idStr, titleStr, helpTxtStr, chcsStr, valStr, resizeStr, otherStr)
  argStrs <- argStrs[which(nchar(argStrs) > 0)]  
  
  return(sprintf("%s(%s)", 
                 x$input_tp_shiny, 
                 paste(argStrs, collapse = ",\n")))
}

.makeSrvrPresetInptTxt <- function(x) {
  
  nmUpdateFunc <- switch(x$input_tp_shiny,
                         GcheckboxInput = "updateCheckboxInput",
                         GdateInput = "updateDateInput",
                         GdateRangeInput = "updateDateRangeInput",
                         GfileInput = "restoreInput",
                         GnumericInput = "updateNumericInput",
                         GradioButtons = "updateRadioButtons",
                         GselectInput = "updateSelectInput",
                         GselectizeInput = "updateSelectizeInput",
                         GsliderInput = "updateSliderInput",
                         GtextAreaInput = "updateTextAreaInput",
                         GtextInput = "updateTextInput",
                         GvarSelectInput = "updateVarSelectInput",
                         GvarSelectizeInput = "updateVarSelectizeInput",
                         NA_character_)
  
  if (is.na(nmUpdateFunc)) {
    return(sprintf("# preset '%s'\n# TODO.", x$key))
  }
  
  return(
    sprintf("if ('%s' %%in%% names(lsPreset)) {
  %s(session = session, 
                  inputId = '%s', 
                  value = lsPreset$%s)
}", 
x$key,
nmUpdateFunc,
x$key,
x$key))
  
}

.makeModSrvrTxt <- function(x) {
  
  if (length(x$format[[1]][[1]]) == 1) {
    if (is.na(x$format[[1]][[1]])) { return(NULL) }
  }
  
  if (!is.na(x$input_tp_shiny)) {
    if (x$input_tp_shiny == "GdynamicMultiInput") {
      return(sprintf("modSrvrs$%s <- dynamicMultiServer(
  id = '%s', 
  nmUImod = '', # TODO.  
  nmSrvrMod = '', # TODO.
  btns = c('edit', 'remove'), 
  lsArgsModUI = list(hL = 5, colWidth = 12, inpWidth = DEFAULT_INP_WIDTH),
  lsArgsModSrvr = list(lsPreset = lsPreset$%s))",
                     x$key,
                     x$key,
                     x$key))
    }
  }
  
  return(sprintf("modSrvrs$%s <- ...Server(
id = '%s', 
lsPreset = lsPreset$%s)",
                 x$key, 
                 x$key,
                 x$key))
  
}

.makeInptObsrvr <- function(x) {
  
  if (is.na(x$input_tp_shiny)) {
    return(sprintf("observe({
        '%s' # TODO. 
    })", x$key))
  }
  
  if (x$input_tp_shiny == "GdynamicMultiInput") {
    
    if (x$required) {
    
    return(sprintf("observeEvent(modSrvrs$%s$items, handlerExpr = {
        
        resultsGood$%s <- FALSE
        outputMsgs$%s <- ''
        rvToReturn$results$%s <- modSrvrs$%s$items
        resultsGood$%s <- length(modSrvrs$%s$items) > 0
        if (!resultsGood$%s) {
          outputMsgs$%s <- '*Required.'
        }
        
      })", 
                   x$key, x$key, x$key, x$key, x$key, x$key, x$key, x$key, x$key))
    }
    
    return(sprintf("observeEvent(modSrvrs$%s$items, handlerExpr = {
        
        resultsGood$%s <- TRUE
        outputMsgs$%s <- ''
        rvToReturn$results$%s <- modSrvrs$%s$items
        
      })", 
                   x$key, x$key, x$key, x$key, x$key))
    
  }
  
  if (x$input_tp_shiny == "GtextAreaInput") {
    
    return(
      sprintf("observeEvent(input$%s, handlerExpr = {

        resultsGood$%s <- FALSE
        rvToReturn$results$%s <- input$%s
        
        nChar <- nchar(input$%s)
        outputMsgs$%s <- sprintf('%%d chars. remaining', 1000 - nChar)
        
        resultsGood$%s <- isValidInput.text(
          x = input$%s, 
          bRequired = %s, 
          nCharMin = 100, # TODO.
          nCharMax = 1000) # TODO.
        
      })",
              x$key, x$key, x$key, x$key, x$key, x$key, x$key, x$key, x$required))
    
  }
  
  if (x$input_tp_shiny == "GselectInput") {
    
    if (x$required) {
      
      return(sprintf("
      observeEvent(input$%s, handlerExpr = {
        
        resultsGood$%s <- FALSE
        outputMsgs$%s <- ''
        rvToReturn$results$%s <- input$%s
        validate(need(input$%s, message = FALSE))
        resultsGood$%s <- TRUE

      })
    ", x$key, x$key, x$key, x$key, x$key, x$key, x$key))
    }
    
    return(sprintf("
      observeEvent(input$%s, handlerExpr = {
        
        resultsGood$%s <- TRUE
        outputMsgs$%s <- ''
        rvToReturn$results$%s <- input$%s

      })
    ", x$key, x$key, x$key, x$key, x$key))
  }

  if (x$input_tp_shiny == "GnumericInput") {
    
    if (x$required) {
      return(
        sprintf("
      observeEvent(input$%s, handlerExpr = {
        
        resultsGood$%s <- FALSE
        rvToReturn$results$%s <- input$%s
        outputMsgs$%s <- ''
        
        validate(need(input$%s, message = FALSE))
        
        if (input$%s > 10) {
          outputMsgs$%s <- paste(
            'You have entered a high value for ...', # TODO: Complete this sentence.
            'Are you sure the value is correct?',
            'If you are sure, you can ignore this message.', 
            sep = '\n')
        }
        
        resultsGood$%s <- TRUE
        
      })
    ", x$key, x$key, x$key, x$key, x$key, x$key, x$key, x$key, x$key))
    }
    
    return(
      sprintf("
      observeEvent(input$%s, handlerExpr = {
        
        resultsGood$%s <- TRUE
        rvToReturn$results$%s <- input$%s
        outputMsgs$%s <- ''
        
        if (length(input$%s) == 1) {
        if (!is.na(input$%s)) {
        if (input$%s > 10) {
          outputMsgs$%s <- paste(
            'You have entered a high value for ...', # TODO: Complete this sentence.
            'Are you sure the value is correct?',
            'If you are sure, you can ignore this message.', 
            sep = '\n')
        }
        }
        }
        
      })
    ", x$key, x$key, x$key, x$key, x$key, x$key, x$key, x$key, x$key))
    
  }

  return(sprintf("observe({
        '%s' # TODO. 
    })", x$key))    

  # observe({
  #   rvToReturn$results$location_project <- modSrvrs$location_project$results$location
  #   resultsGood$location_project <- modSrvrs$location_project$allResultsGood
  # })
    
}

.makeOutptRndr <- function(x) {
  return(sprintf("output$%s_msg <- renderText({
        outputMsgs$%s
      })", x$key, x$key))
}



# Example:
# resLogin <- Guardener::Glogin(un = "NovaInstitute", 
#                               pw = Sys.getenv("GUARDIAN_PW_NovaInstitute"))
# 
# schemas <- Guardener::GgetSchemas(refreshToken = resLogin$refreshToken,
#                                   topicId = "0.0.3881359")
# 
# status <- sapply(X = schemas, FUN = function(x) x$status)
# idxxPublished <- which(status == "PUBLISHED")
# schemas <- schemas[idxxPublished]
# nms <- sapply(X = schemas, FUN = function(x) sprintf("%s (%s)", x$name, x$version))
# names(nms) <- NULL
# grep(pattern = "TIR", x = nms, value = TRUE)
# idx <- which(nms == "HGICV Token Issuance Request (TIR) (2.1.0)")
# 
# makeSchemaModule(schemaAbbr = "TIR",
#                  schema = schemas[[idx]],
#                  savedir = tmpdir,
#                  subject = "activity")
# 
# # makeDRschemaModule(schemaAbbr = "RA",
# #                    schema = schemas[[idx]],
# #                    savedir = tmpdir)
# 
# #dfSch <- schemaToDf(schemas[[idx]])
# 
# accTkn <- Guardener::GgetAccessToken(
#   refreshToken = GaccTknSb,
#   baseurl = "http://localhost:3000/")
# schemas <- GET(url = sprintf("http://localhost:3000/api/v1/schemas/%s", topicId),
#                pageIndex = 0, pageSize = 10,
#                add_headers(Authorization = sprintf("Bearer %s", accTkn)))
