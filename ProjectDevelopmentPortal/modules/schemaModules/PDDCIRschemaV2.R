
# Module for schema 'PDD Certificate Issuance Request (CIR)'.

PDDCIRschemaV2UI <- function(id, 
                             lsPreset = NULL, 
                             hL = 4, 
                             colWidth = 12, 
                             inpWidth = DEFAULT_INP_WIDTH) {
  
  ns <- NS(id)
  
  tagList(
    
    GtextOutput(id = ns("id_project"), 
                title = "Project ID", 
                hL = hL, 
                colWidth = colWidth),
    
    GtextOutput(id = ns("toDRPLA"), 
                title = "Approving DR-PLA of Project", 
                hL = hL, 
                colWidth = colWidth),
    
    GtextOutput(id = ns("toDRPDDXA"), 
                title = "Document Review: PDD Section A (DR-PDDXA)", 
                hL = hL, 
                colWidth = colWidth),
    
    GtextOutput(id = ns("toDRPDDXB"), 
                title = "Document Review: PDD Section B (DR-PDDXB)", 
                hL = hL, 
                colWidth = colWidth),
    
    GtextOutput(id = ns("toDRPDDXC"), 
                title = "Document Review: PDD Section C (DR-PDDXC)", 
                hL = hL, 
                colWidth = colWidth),
    
    textOutput(outputId = ns("toInvalidInput")),
    
    br())
}

PDDCIRschemaV2Server <- function(id,  
                                 dbCon,
                                 loginInfoUsr,
                                 loginInfoSb = NULL,
                                 idEntity,
                                 idWorkflow,
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
          id_msg_pred = NULL,
          url_ipfs_pred = NULL,
          id_subject = idEntity,
          type_subject = "PROJECT"),
        id_msg_pred_drxa = NULL,
        url_ipfs_pred_drxa = NULL,
        id_msg_pred_drxb = NULL,
        url_ipfs_pred_drxb = NULL,
        id_msg_pred_drxc = NULL,
        url_ipfs_pred_drxc = NULL,
        id_acc_h = NULL)
      
      resultsGood <- reactiveValues()
      resultsGood$id_msg_pred <- FALSE
      resultsGood$url_ipfs_pred <- FALSE
      resultsGood$id_msg_pred_drxa <- FALSE
      resultsGood$url_ipfs_pred_drxa <- FALSE
      resultsGood$id_msg_pred_drxb <- FALSE
      resultsGood$url_ipfs_pred_drxb <- FALSE
      resultsGood$id_msg_pred_drxc <- FALSE
      resultsGood$url_ipfs_pred_drxc <- FALSE
      resultsGood$id_acc_h <- FALSE
      
      # outputMsgs <- reactiveValues()
      # None.
      
      # rvOther <- reactiveValues()
      # None.
      
      # id_acc_h
      observe({
        
        resultsGood$id_acc_h <- FALSE
        
        accId <- dbFetch(
          dbSendQuery(
            conn = dbCon, 
            statement = sprintf("SELECT id_acc_h FROM tbl_link_agents_x_hedera_accounts WHERE id_agent = '%s';", 
                                loginInfoUsr$id_agent)))[["id_acc_h"]]
        if (length(accId) == 1) {
          if (!is.na(accId)) {
            if (nchar(accId) >= 5) {
              rvToReturn$results$id_acc_h <- accId
              resultsGood$id_acc_h <- TRUE
            }
          }
        }
        
      })
      
      # preset inputs ----------------------------------------------------------
      # None.

      # module servers ---------------------------------------------------------
      # None.
      
      # inputs -----------------------------------------------------------------
      # None.
      
      # outputs ----------------------------------------------------------------
      
      output$id_project <- renderText({
        rvToReturn$results$headers$id_subject
      })
      
      output$toDRPLA <- renderText({
        
        resultsGood$id_msg_pred <- FALSE
        resultsGood$url_ipfs_pred <- FALSE
        
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
      
      # toDRPDDXA
      output$toDRPDDXA <- renderText({
        
        resultsGood$id_msg_pred_drxa <- FALSE
        resultsGood$url_ipfs_pred_drxa <- FALSE
        
        dfRvMd <- getApprovingDRs(
          dbCon = dbCon, 
          idProject = idEntity, 
          stepWorkflow = "rvcdb_vldtr_DR_PDDxA", 
          all = FALSE)
        
        if (length(dfRvMd) == 0) {
          return("Could not find any approving DR-PDDXAs.")
        }
        if (nrow(dfRvMd) == 0) {
          return("Could not find any approving DR-PDDXAs.")
        }
        
        # Automatically choose the most recent one.
        dfRvMd <- dfRvMd[1,]
        
        # Assign to results.
        rvToReturn$results$id_msg_pred_drxa <- dfRvMd$id_message_h
        rvToReturn$results$url_ipfs_pred_drxa <- dfRvMd$uri_ipfs
        resultsGood$id_msg_pred_drxa <- TRUE
        resultsGood$url_ipfs_pred_drxa <- TRUE
        
        # Render output.
        return(sprintf("The following DR-PDDXA will be used for this Certificate Issuance Request: (%s) %s", 
                       dfRvMd$date_modified, 
                       dfRvMd$uri_ipfs))
        
      })
      
      # toDRPDDXB
      output$toDRPDDXB <- renderText({
        
        resultsGood$id_msg_pred_drxb <- FALSE
        resultsGood$url_ipfs_pred_drxb <- FALSE
        
        dfRvMd <- getApprovingDRs(
          dbCon = dbCon, 
          idProject = idEntity, 
          stepWorkflow = "rvcdb_vldtr_DR_PDDxB", 
          all = FALSE)
        
        if (length(dfRvMd) == 0) {
          return("Could not find any approving DR-PDDXBs.")
        }
        if (nrow(dfRvMd) == 0) {
          return("Could not find any approving DR-PDDXBs.")
        }
        
        # Automatically choose the most recent one.
        dfRvMd <- dfRvMd[1,]
        
        # Assign to results.
        rvToReturn$results$id_msg_pred_drxb <- dfRvMd$id_message_h
        rvToReturn$results$url_ipfs_pred_drxb <- dfRvMd$uri_ipfs
        resultsGood$id_msg_pred_drxb <- TRUE
        resultsGood$url_ipfs_pred_drxb <- TRUE
        
        # Render output.
        return(sprintf("The following DR-PDDXB will be used for this Certificate Issuance Request: (%s) %s", 
                       dfRvMd$date_modified, 
                       dfRvMd$uri_ipfs))
        
      })
      
      # toDRPDDXC
      output$toDRPDDXC <- renderText({
        
        resultsGood$id_msg_pred_drxc <- FALSE
        resultsGood$url_ipfs_pred_drxc <- FALSE
        
        dfRvMd <- getApprovingDRs(
          dbCon = dbCon, 
          idProject = idEntity, 
          stepWorkflow = "rvcdb_vldtr_DR_PDDxC", 
          all = FALSE)
        
        if (length(dfRvMd) == 0) {
          return("Could not find any approving DR-PDDXCs.")
        }
        if (nrow(dfRvMd) == 0) {
          return("Could not find any approving DR-PDDXCs.")
        }
        
        # Automatically choose the most recent one.
        dfRvMd <- dfRvMd[1,]
        
        # Assign to results.
        rvToReturn$results$id_msg_pred_drxc <- dfRvMd$id_message_h
        rvToReturn$results$url_ipfs_pred_drxc <- dfRvMd$uri_ipfs
        resultsGood$id_msg_pred_drxc <- TRUE
        resultsGood$url_ipfs_pred_drxc <- TRUE
        
        # Render output.
        return(sprintf("The following DR-PDDXC will be used for this Certificate Issuance Request: (%s) %s", 
                       dfRvMd$date_modified, 
                       dfRvMd$uri_ipfs))
        
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
