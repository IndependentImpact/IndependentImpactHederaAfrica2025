
#'@description Generates the code for a Shiny document review (DR) module based 
#' on original schema information retrieved via the Guardian API.
#'@param schema One of the items in the list returned by Guardener::GgetSchemas().
#'
makeDRschemaModule <- function(schemaAbbr, schema, savedir) {
  
  dfSchema <- schemaToDf(schema)
  
  dfSchema$input_tp_shiny <- sapply(X = dfSchema$type, FUN = function(x) {
    switch(EXPR = x,
           string = "GtextAreaInput",
           array = "GdynamicMultiInput",
           number = "GnumericInput",
           boolean = "GselectInput",
           integer = "GnumericInput",
           NA_character_)
  })
  
  dfSchema$input_tp_shiny[which(!is.na(dfSchema$enum))] <- "GselectInput"
  
  cat(file = sprintf("%s/DR%sschema.R", savedir, schemaAbbr),
  
sprintf(  
"

# Module for schema '%s [v%s]'.

DR%sschemaUI <- function(id, 
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

DR%sschemaServer <- function(id,  
                             dbCon,
                             idAgent,
                             idEntity,
                             idPolicyV, 
                             idSchemaV,
                             lsOriginal,
                             GaccTknSb,
                             lsPreset = NULL) {
  
  moduleServer(
    id = id, 
    function(input, output, session) {
      
      ns <- session$ns
      
      rvToReturn <- reactiveValues()
      # rvToReturn$goToModule <- NULL 
      # rvToReturn$save <- NULL
      # rvToReturn$submit <- NULL
      rvToReturn$allResultsGood <- FALSE
      rvToReturn$results <- reactiveValues(
        final_rd = NULL, # Not necessarily in the schema, but required by module 'review'.
        id_msg_%s = lsOriginal$md$id_message,
        id_vc_%s = '(Deprecated.)', # (root.document.id)  
        url_ipfs_%s = lsOriginal$md$url_ipfs,
        %s)
      
      resultsGood <- reactiveValues()
      resultsGood$final_rd <- FALSE
      %s
      
      outputMsgs <- reactiveValues()
      # None.

      # preset inputs ----------------------------------------------------------
      
      %s
      
      # module servers ---------------------------------------------------------
      
      modSrvrs <- reactiveValues()
      
      %s
      
      # inputs -----------------------------------------------------------------
      
      %s
      
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

tolower(schemaAbbr),
tolower(schemaAbbr),
tolower(schemaAbbr),

paste(sprintf("%s = lsOriginal$cont$%s,\n%s_rd = NULL,\n%s_rf = NULL", 
              dfSchema$key,
              dfSchema$key,
              dfSchema$key,
              dfSchema$key), 
      collapse = ",\n"),

paste(sprintf("resultsGood$%s_r <- FALSE", 
              dfSchema$key), 
      collapse = "\n"),

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

.makeFinalRDobsrvr(dfSchema),

paste(
  unlist(sapply(X = 1:nrow(dfSchema), FUN = function(r) {
    return(.makeOutptRndr(dfSchema[r,]))
  })), 
  collapse = "\n\n")
))
}

# Auxiliary functions ----------------------------------------------------------

.makeUIinputTxt <- function(x) {

  return(  
    sprintf("reviewFieldUI(id = ns('%s_r'),
                  origFldTitle = '%s',
                  origFldHelpTxt = '%s',
                  lsPreset = lsPreset$%s_r, 
                  hL = hL+1, 
                  colWidth = colWidth, 
                  inpWidth = inpWidth)",
            x$key, x$title, x$description, x$key))
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
  
  return(
    sprintf("modSrvrs$%s_r <- reviewFieldServer(
        id = '%s_r', 
        fieldKey = '%s', 
        fieldVal = lsOriginal$cont$%s, 
        lsPreset = lsPreset$%s_r)", 
            x$key, x$key, x$key, x$key, x$key))
}

.makeInptObsrvr <- function(x) {
  
  return(
    sprintf("observe({
        rvToReturn$results$%s_rd <- modSrvrs$%s_r$results$rev_decision
        rvToReturn$results$%s_rf <- modSrvrs$%s_r$results$rev_feedback
        resultsGood$%s_r <- modSrvrs$%s_r$allResultsGood
      })", 
            x$key, x$key, x$key, x$key, x$key, x$key))
  
}

.makeFinalRDobsrvr <- function(dfSchema) {
  
  return(
    sprintf("observe({
  
  rvToReturn$results$final_rd <- 'REJECT'
  resultsGood$final_rd <- FALSE
  
  %s
  
  optsPass <- c('APPROVE', 'FORWARD_ACTION_REQUEST')
  bAllPass <- (%s)
  rvToReturn$results$final_rd <- c('REJECT', 'APPROVE')[1 + bAllPass]
  resultsGood$final_rd <- TRUE

})",
            paste(
              sprintf("validate(need(rvToReturn$results$%s_rd, message = FALSE))", 
                      dfSchema$key), 
              collapse = "\n"),
            
            paste(
              sprintf("(rvToReturn$results$%s_rd %%in%% optsPass)",
                      dfSchema$key), 
              collapse = " &\n")))
  
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
# grep(pattern = "(TIR)", x = nms, value = TRUE, fixed = TRUE)
# idx <- which(nms == "HGICV Token Issuance Request (TIR) (2.1.0)")
# 
# makeDRschemaModule(schemaAbbr = "TIR",
#                    schema = schemas[[idx]],
#                    savedir = tmpdir)






