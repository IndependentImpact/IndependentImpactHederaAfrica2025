
# Module for schema 'Project Registration Request (PRR) [v2.1.0]'.

PRRschemaV2UI <- function(id, 
                          lsPreset = NULL, 
                          hL = 4, 
                          colWidth = 12, 
                          inpWidth = DEFAULT_INP_WIDTH) {
  
  ns <- NS(id)
  
  tagList(
    
    #useShinyjs(),
    
    GtextOutput(id = ns("id_project"), 
                title = "Project ID", 
                hL = hL, 
                colWidth = colWidth),
    
    chooseLicenseInput(
      id = ns('chLicPDL'),
      title = "Project Developer License",
      helpTxt = "Select your Independent Impact Project Developer License (II-L-PD) to use for this project."),
    
    GtextOutput(id = ns("toDRPLA"), 
                title = "Approving DR-PLA of Project", 
                hL = hL, 
                colWidth = colWidth),
    
    GtextOutput(id = ns("toDRPDDCIR"), 
                title = "Approving DR-PDDCIR of Project", 
                hL = hL, 
                colWidth = colWidth))
}

PRRschemaV2Server <- function(id,  
                              dbCon,
                              loginInfoUsr,
                              idEntity,
                              idWorkflow = NULL,
                              idSchema = NULL,
                              lsPreset = NULL) {
  
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
          id_msg_pred = NULL, # DR-PLA
          url_ipfs_pred = NULL,
          id_subject = idEntity,
          type_subject = "PROJECT"),
        id_msg_pred_drcir = NULL,
        url_ipfs_pred_drcir = NULL)
      
      resultsGood <- reactiveValues()
      resultsGood$id_msg_lic <- FALSE
      resultsGood$url_ipfs_lic <- FALSE
      resultsGood$id_msg_pred <- FALSE
      resultsGood$url_ipfs_pred <- FALSE
      resultsGood$id_msg_pred_drcir <- FALSE
      resultsGood$url_ipfs_pred_drcir <- FALSE
      
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
      
      # inputs -----------------------------------------------------------------
      
      observe({
        
        resultsGood$id_msg_lic <- FALSE
        resultsGood$url_ipfs_lic <- FALSE
        
        rvToReturn$results$headers$id_msg_lic <- modSrvrs$mProjDevLic$results$id_msg
        rvToReturn$results$headers$url_ipfs_lic <- modSrvrs$mProjDevLic$results$url_ipfs
        
        if (modSrvrs$mProjDevLic$allResultsGood) {
          resultsGood$id_msg_lic <- TRUE
          resultsGood$url_ipfs_lic <- TRUE
        }
        
      })
      
      # outputs ----------------------------------------------------------------
      
      output$id_project <- renderText({
        rvToReturn$results$headers$id_subject
      })
      
      output$toDRPLA <- renderText({
        
        noDocMsg <- "You do not seem to have an approved Project Listing Application (PLA) for this project yet. Please use the 'Independent Impact - Main Worfklow' to apply for project listing."
        
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
      
      output$toDRPDDCIR <- renderText({
        
        noDocMsg <- "You do not seem to have an approved PDD CIR for this project yet. Please use the 'Independent Impact - PDD Validation Workflow' to apply for project listing."
        
        dfDocMd <- getApprovingDRPDDCIRs(
          dbCon = dbCon,
          idProject = idEntity)
        if (length(dfDocMd) == 0) {
          return(noDocMsg)
        }
        
        rvToReturn$results$id_msg_pred_drcir <- dfDocMd$id_message_h
        rvToReturn$results$url_ipfs_pred_drcir <- dfDocMd$uri_ipfs
        resultsGood$id_msg_pred_drcir <- TRUE
        resultsGood$url_ipfs_pred_drcir <- TRUE
        
        return(dfDocMd$uri_ipfs)
        
      })
      
      # return logic -----------------------------------------------------------
      
      observe({
        rvToReturn$allResultsGood <- all(unlist(reactiveValuesToList(resultsGood)))
      })
      
      return(rvToReturn)
      
    })
}
