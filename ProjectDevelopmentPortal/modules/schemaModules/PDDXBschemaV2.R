
PDDXBschemaV2UI <- function(id,
                            lsPreset = NULL,
                            hL = 4,
                            colWidth = 12,
                            inpWidth = DEFAULT_INP_WIDTH,
                            idSchemaV = NULL) {
  
  ns <- NS(id)
  
  tagList(
    
    #useShinyjs(),
    
    GtextOutput(id = ns("title_project"),
                title = "Project Title",
                hL = hL,
                colWidth = colWidth),
    
    GtextOutput(id = ns("id_project"),
                title = "Project ID",
                hL = hL,
                colWidth = colWidth),
    
    GtextOutput(id = ns("toDRPLA"),
                title = "Approving DR-PLA of Project",
                hL = hL,
                colWidth = colWidth),
    
    chooseLicenseInput(
      id = ns('chLicPDL'),
      title = "Project Developer License",
      helpTxt = "Select your Independent Impact Project Developer License (II-L-PD) to use for this project."),
    
    fluidRow(
      column(
        width = colWidth,
        wellPanel(
          hx(x = "Impacts", lvl = hL),
          helpText("Impact Evaluation Plan"),
          dynamicMultiV2Input(
            id = ns("impacts"), 
            useModal = FALSE
            #nmUImod = "impactUI", 
            # lsArgsModUI = list(
            #   hL = hL, 
            #   colWidth = colWidth, 
            #   inpWidth = inpWidth)
            )))), 
    
    textOutput(outputId = ns("toInvalidInput")),
    
    br())
  
  
}

PDDXBschemaV2Server <- function(id,
                                dbCon,
                                loginInfoUsr,
                                idEntity,
                                idWorkflow = NULL,
                                idSchema = NULL,
                                lsPreset = NULL,
                                hL = 4,
                                colWidth = 12,
                                inpWidth = DEFAULT_INP_WIDTH) {
  
  moduleServer(
    id = id,
    function(input, output, session) {
      
      ns <- session$ns
      
      rvToReturn <- reactiveValues()
      rvToReturn$allResultsGood <- FALSE
      rvToReturn$results <- reactiveValues(
        headers = list(
          id_msg_lic = NULL,
          url_ipfs_lic = NULL,
          id_msg_pred = NULL,
          url_ipfs_pred = NULL,
          id_subject = idEntity,
          type_subject = "PROJECT"),
        impacts = NULL)
      
      resultsGood <- reactiveValues()
      resultsGood$headers <- FALSE      
      resultsGood$impacts <- FALSE
      
      outputMsgs <- reactiveValues()
      
      rvOther <- reactiveValues()
      
      # preset inputs ----------------------------------------------------------
      
      # module servers ---------------------------------------------------------
      
      modSrvrs <- reactiveValues()
      
      modSrvrs$mProjDevLic <- chooseLicenseServer(
        id = "chLicPDL",
        dbCon = dbCon,
        scopes = "PROJECT_DEVELOPER", 
        statuses = "ACTIVE",
        idAgent = loginInfoUsr$id_agent,
        lsPreset = NULL) # TODO: implement lsPreset
      
      modSrvrs$impacts <- dynamicMultiV2Server(
        id = "impacts", 
        nmSrvrMod = "impactServer", 
        lsPreset = lsPreset$impacts,
        lsArgsModSrvr = list(hL = hL, colWidth = colWidth, inpWidth = inpWidth),
        useModal = FALSE,
        nmUImod = "impactUI",
        lsArgsModUI = list(hL = hL, colWidth = colWidth, inpWidth = inpWidth))

      # inputs -----------------------------------------------------------------
      
      observe({
        rvToReturn$results$headers$id_msg_lic <- modSrvrs$mProjDevLic$results$id_msg
        rvToReturn$results$headers$url_ipfs_lic <- modSrvrs$mProjDevLic$results$url_ipfs
      })
      
      observe({
        resultsGood$headers <- FALSE
        
        if (!isValidInput.text(
          x = rvToReturn$results$headers$id_msg_pred, 
          bRequired = TRUE, 
          nCharMin = 5)) {
          return(invisible(1))
        }
        
        if (!isValidInput.text(
          x = rvToReturn$results$headers$url_ipfs_pred, 
          bRequired = TRUE, 
          nCharMin = 5)) {
          return(invisible(2))
        }
        
        if (!modSrvrs$mProjDevLic$allResultsGood) {
          return(invisible(3))
        }
        
        resultsGood$headers <- TRUE
      })
      
      observe({
        resultsGood$impacts <- FALSE
        rvToReturn$results$impacts <- modSrvrs$impacts$items
        # TODO: Rethink this one below.
        resultsGood$impacts <- length(rvToReturn$results$impacts) > 0
      })
      
      # outputs ----------------------------------------------------------------
      
      output$title_project <- renderText({
        
        tryCatch({
          
          projTitle <- dbFetch(
            dbSendQuery(
              conn = dbCon,
              statement = sprintf("SELECT title FROM tbl_projects WHERE id = '%s';",
                                  idEntity)))[["title"]]
          
        }, error = function(e) {
          warning(e)
        })
        
        return(projTitle)
        
      })
      
      output$id_project <- renderText({
        rvToReturn$results$headers$id_subject
      })
      
      output$toDRPLA <- renderText({
        
        noDocMsg <- "You do not seem to have an approved Project Listing Application (PLA) for this project yet. Please use the 'Independent Impact - Main Workflow' to apply for project listing."
        
        dfDocMd <- getApprovingDRPLAs(
          dbCon = dbCon,
          idProject = idEntity)
        if (length(dfDocMd) == 0) {
          return(noDocMsg)
        }
        
        rvToReturn$results$headers$id_msg_pred <- dfDocMd$id_message_h
        rvToReturn$results$headers$url_ipfs_pred <- dfDocMd$uri_ipfs
        resultsGood$id_msg_pred <- TRUE
        resultsGood$url_ipfs_pred <- TRUE
        
        return(dfDocMd$uri_ipfs)
        
      })
      
      output$toInvalidInput <- renderText({
        
        if (rvToReturn$allResultsGood) { return(NULL) }
        
        return(
          sprintf("Your inputs to the following fields are not valid: %s",
                  paste(names(resultsGood)[!unlist(reactiveValuesToList(resultsGood))], sep = "", collapse = ", ")))
        
      })
      
      # return logic -----------------------------------------------------------
      
      observe({
        rvToReturn$allResultsGood <- all(unlist(reactiveValuesToList(resultsGood)))
      })
      
      return(rvToReturn)
      
    })
}
