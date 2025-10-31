
# Module that allows a user to choose an agent license from their list of 
# existing agent licenses, if any. 
# Returns the message ID and IPFS URL (URI) of the selected VP/VC.

chooseLicenseInput <- function(id, 
                               title,
                               helpTxt,
                               lsPreset = NULL, 
                               hL = 4,
                               colWidth = 12, 
                               inpWidth = DEFAULT_INP_WIDTH) {
  
  ns <- NS(id)
  
  tagList(
    
    ##useShinyjs(),
    
    # fluidRow(
    #   column(
    #     width = colWidth,
    #     GselectInput(id = ns("siLicense"),
    #                  title = title,
    #                  choices = c(),
    #                  helpTxt = helpTxt,
    #                  selected = NULL,
    #                  multiple = FALSE,
    #                  hL = hL,
    #                  colWidth = colWidth,
    #                  inpWidth = inpWidth)))
    
    fluidRow(
      column(
        width = colWidth,
        wellPanel(
          hx(x = title, lvl = hL),
          helpText(helpTxt),
          selectInput(inputId = ns("siLicense"),
                      label = NULL,
                      choices = c(),
                      multiple = FALSE,
                      selected = NULL,
                      selectize = TRUE,
                      width = inpWidth),
          textOutput(outputId = ns("siLicense_msg"))))))
}

#' @param idAgent Character. Agent ID of the holder of the license.
#' 
chooseLicenseServer <- function(id, 
                                dbCon,
                                idAgent, 
                                scopes, # PROJECT_DEVELOPER, PDD_VALIDATOR, MR_VERIFIER, STANDARDS_BODY
                                statuses = c("ACTIVE", "INACTIVE")[1],
                                lsPreset = NULL) {
  
  moduleServer(
    id = id, 
    function(input, output, session) {
      
      ns <- session$ns
      
      rvToReturn <- reactiveValues()
      rvToReturn$goToModule <- NULL
      rvToReturn$allResultsGood <- FALSE
      rvToReturn$results <- reactiveValues(
        id_msg = NULL,
        url_ipfs = NULL)
      
      resultsGood <- reactiveValues()
      resultsGood$id_msg <- FALSE
      resultsGood$url_ipfs <- FALSE
      
      rvOther <- reactiveValues()
      rvOther$init <- NULL
      rvOther$dfLics <- NULL
      
      outputMsgs <- reactiveValues()
      outputMsgs$siLicense <- ""
      
      # preset inputs ----------------------------------------------------------
      
      # siLicense
      observeEvent(rvOther$init, handlerExpr = {
        
        validate(need(rvOther$init, message = FALSE))
        
        rvOther$dfLics <- NULL
        outputMsgs$siLicense <- ""
        dfLics <- NULL
        
        if ("STANDARDS_BODY" %in% scopes) {
          
          userType <- dbGetQuery(
            conn = dbCon, 
            statement = sprintf(
              "SELECT type_user FROM tbl_agents WHERE id = '%s';", 
              idAgent))[["type_user"]]
          
          if (userType == "STANDARD_REGISTRY") {
            rvOther$dfLics <- data.frame(
              date_issued = "(STANDARDS_BODY)",
              id_message_h = "(STANDARDS_BODY)",
              uri_ipfs = "(STANDARDS_BODY)")
            return(NULL)
          }
        }
 
        didAgent <- dbGetQuery(
          conn = dbCon, 
          statement = sprintf(
            "SELECT did FROM tbl_link_agents_x_dids WHERE id_agent = '%s';",
            idAgent))[["did"]]
        
        q <- sprintf(
          "SELECT * FROM tbl_agent_licenses WHERE did_holder = '%s' AND scope IN(%s) AND status IN(%s);",
          didAgent,
          paste(sprintf("'%s'", scopes), sep = "", collapse = ","),
          paste(sprintf("'%s'", statuses), sep = "", collapse = ","))
        dfLics <- dbGetQuery(conn = dbCon, statement = q)

        if (nrow(dfLics) == 0) {
          outputMsgs$siLicense <- sprintf(
            "You don't seem to have any active %s licenses. Please apply for one or wait for your application to be processed.",
            paste(scopes, sep = "", collapse = " / "))
          return(NULL)
        }
        
        # Done.
        rvOther$dfLics <- dfLics

      })
      
      observeEvent(rvOther$dfLics, handlerExpr = {
        
        if (length(rvOther$dfLics) == 0) { return(NULL) }
        if (nrow(rvOther$dfLics) == 0) { return(NULL) }
        
        chcs <- 1:nrow(rvOther$dfLics)
        names(chcs) <- sprintf("%s (%s)", 
                               1:nrow(rvOther$dfLics), 
                               rvOther$dfLics$date_issued)
        
        # updateSelectizeInput(session = session,
        #                      inputId = "siLicense",
        #                      choices = chcs,
        #                      selected = 1)
        updateSelectInput(session = session,
                          inputId = "siLicense",
                          choices = chcs,
                          selected = 1)
        
        # TODO: lsPreset, but make sure that the license is still valid.
        
        # if (length(chcs) == 1) {
        #   message("Setting rvToReturn$results$id_msg and ...$url_ipfs.")
        #   rvToReturn$results$id_msg <- rvOther$dfLics$id_message[1]
        #   rvToReturn$results$url_ipfs <- rvOther$dfLics$url_ipfs[1]
        #   resultsGood$id_msg <- TRUE
        #   resultsGood$url_ipfs <- TRUE
        #   outputMsgs$siLicense <- rvToReturn$results$id_msg 
        # } 
        
      })
      
      # inputs -----------------------------------------------------------------
      
      observeEvent(input$siLicense, handlerExpr = {
        
        resultsGood$id_msg <- FALSE
        resultsGood$url_ipfs <- FALSE
        rvToReturn$results$id_msg <- NULL
        rvToReturn$results$url_ipfs <- NULL
        
        idx <- as.integer(input$siLicense) 
        if (length(idx) == 0) { return(NULL) }
        if (is.na(idx)) { return(NULL) }
        
        rvToReturn$results$id_msg <- rvOther$dfLics$id_message_h[idx]
        rvToReturn$results$url_ipfs <- rvOther$dfLics$uri_ipfs[idx]
        resultsGood$id_msg <- TRUE
        resultsGood$url_ipfs <- TRUE
        
      })
      
      # outputs-----------------------------------------------------------------
      
      output$siLicense_msg <- renderText({
        outputMsgs$siLicense
      })
      
      # module return logic ----------------------------------------------------
      
      observe({
        # message("all(unlist(reactiveValuesToList(resultsGood))) = ", 
        #         all(unlist(reactiveValuesToList(resultsGood))))
        rvToReturn$allResultsGood <- all(unlist(reactiveValuesToList(resultsGood)))
      })
      
      rvOther$init <- Sys.time()
      
      return(rvToReturn)
      
    })
}


