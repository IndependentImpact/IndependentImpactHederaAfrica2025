
PLAschemaV2UI <- function(id, 
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
    
    chooseLicenseInput(
      id = ns('chLicPDL'),
      title = "Project Developer License",
      helpTxt = "Select your Independent Impact Project Developer License (II-L-PD) to use for this project."),
    
    GtextAreaInput(id = ns("purpose_project"), 
                   title = "Project Purpose", 
                   helpTxt = "What is the purpose of the project? (100 chars. min.)", 
                   value = lsPreset$purpose_project, 
                   resize = "vertical",
                   hL = hL, 
                   colWidth = colWidth, 
                   inpWidth = inpWidth),
    
    GtextInput(id = ns("location_project"), 
               helpTxt = "Where will the project take place? (Country, state or province, and city or town or village. 3 chars. min, 100 chars. max.)", 
               value = lsPreset$location_project, 
               title = "Project Location", 
               hL = hL, 
               colWidth = colWidth, 
               inpWidth = inpWidth),
    
    textOutput(outputId = ns("toInvalidInput")),
    
    br())
}

PLAschemaV2Server <- function(id,  
                              dbCon,
                              loginInfoUsr,
                              idEntity = NULL,
                              idWorkflow = NULL,
                              idSchema = NULL,
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
        headers = list(
          id_msg_lic = NULL, 
          url_ipfs_lic = NULL, 
          id_subject = idEntity,
          type_subject = "PROJECT"),
        title_project = NULL,
        location_project = NULL, 
        purpose_project = NULL)
      
      resultsGood <- reactiveValues()
      resultsGood$location_project <- FALSE
      resultsGood$purpose_project <- FALSE 
      resultsGood$id_msg_lic <- FALSE 
      resultsGood$url_ipfs_lic <- FALSE 
      resultsGood$title_project <- FALSE
      
      outputMsgs <- reactiveValues()
      outputMsgs$purpose_project <- ""
      outputMsgs$location_project <- ""
      
      rvOther <- reactiveValues()
      
      # preset inputs ----------------------------------------------------------
      
      if ("purpose_project" %in% names(lsPreset)) {
        updateTextAreaInput(session = session, 
                            inputId = "purpose_project", 
                            value = lsPreset$purpose_project)
      }
      
      if ("location_project" %in% names(lsPreset)) {
        updateTextInput(session = session, 
                        inputId = "location_project", 
                        value = lsPreset$location_project)
      }
      
      # module servers ---------------------------------------------------------
      
      modSrvrs <- reactiveValues()
      
      modSrvrs$mProjDevLic <- chooseLicenseServer(
        id = "chLicPDL", 
        dbCon = dbCon,
        scopes = "PROJECT_DEVELOPER", 
        statuses = "ACTIVE",
        idAgent = loginInfoUsr$id_agent, 
        lsPreset = NULL) # TODO: implement lsPreset
      
      # inputs -----------------------------------------------------------------
      
      observe({
        
        validate(need(modSrvrs$mProjDevLic$results, message = FALSE))
        validate(need(modSrvrs$mProjDevLic$allResultsGood, message = FALSE))
        
        resultsGood$id_msg_lic <- FALSE
        resultsGood$url_ipfs_lic <- FALSE
        rvToReturn$results$headers$id_msg_lic <- modSrvrs$mProjDevLic$results$id_msg
        rvToReturn$results$headers$url_ipfs_lic <- modSrvrs$mProjDevLic$results$url_ipfs
        resultsGood$id_msg_lic <- modSrvrs$mProjDevLic$allResultsGood
        resultsGood$url_ipfs_lic <- modSrvrs$mProjDevLic$allResultsGood
        
      })
      
      observeEvent(input$purpose_project, handlerExpr = {
        
        resultsGood$purpose_project <- FALSE
        rvToReturn$results$purpose_project <- input$purpose_project
        
        nChar <- nchar(input$purpose_project)
        outputMsgs$purpose_project <- sprintf("%d chars. remaining", 1000 - nChar)
        
        resultsGood$purpose_project <- isValidInput.text(
          x = input$purpose_project, 
          bRequired = TRUE, 
          nCharMin = 100, 
          nCharMax = 1000)
        
      })
      
      observeEvent(input$location_project, handlerExpr = {
        
        resultsGood$location_project <- FALSE
        rvToReturn$results$location_project <- input$location_project
        
        nChar <- nchar(input$location_project)
        outputMsgs$location_project <- sprintf("%d chars. remaining", 100 - nChar)
        
        resultsGood$location_project <- isValidInput.text(
          x = input$location_project, 
          bRequired = TRUE, 
          nCharMin = 3, 
          nCharMax = 100)
        
      })
      
      
      # outputs ----------------------------------------------------------------
      
      output$title_project <- renderText({
        
        tryCatch({
          
          q <- sprintf("SELECT title FROM tbl_projects WHERE id = '%s';",
                       idEntity)
          res <- dbGetQuery(conn = dbCon, statement = q)
          rvToReturn$results$title_project <- res$title
          resultsGood$title_project <- TRUE
          
        }, error = function(e) {
          message("WARNING: Failed to retrieve project title. ", e)
          rvToReturn$results$title_project <- NULL
          resultsGood$title_project <- FALSE
        })
        
        rvToReturn$results$title_project
        
      })
      
      output$purpose_project_msg <- renderText({
        outputMsgs$purpose_project
      })
      
      output$location_project_msg <- renderText({
        outputMsgs$location_project
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

