
DRLAschemaUI <- function(id, 
                         lsPreset = NULL, 
                         hL = 3, 
                         colWidth = 12, 
                         inpWidth = DEFAULT_INP_WIDTH,
                         idSchemaV = NULL) { 
  
  ns <- NS(id)
  
  tagList(
    
    #useShinyjs(),
    
    reviewFieldUI(id = ns('license_r'),
                  origFldTitle = 'License',
                  origFldHelpTxt = '',
                  lsPreset = lsPreset$license_r, 
                  hL = hL+1, 
                  colWidth = colWidth, 
                  inpWidth = inpWidth),
    
    fluidRow(
      column(
        width = colWidth,
        wellPanel(
          h4("Accompanying Document Review: Agent Details"),
          # actionLink(inputId = ns("alAD"), 
          #            label = ""),
          textOutput(outputId = ns("toDRADipfs")),
          br(),
          br(),
          actionButton(inputId = ns("abViewDRAD"), 
                       label = "View")))),
    
    textOutput(outputId = ns("toInvalidInput")),
    
    br())
}

DRLAschemaServer <- function(id,  
                             dbCon,
                             loginInfoUsr,
                             idEntity = NULL,
                             idWorkflow = NULL,
                             idSchema = NULL,
                             lsOriginal,
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
          id_msg_pred = lsOriginal$md$id_message_h,
          url_ipfs_pred = lsOriginal$md$uri_ipfs,
          id_subject = lsOriginal$cont$headers$id_subject,
          type_subject = "AGENT"),
        final_rd = NULL, 
        id_msg_drad = lsOriginal$cont$headers$id_msg_pred,
        url_ipfs_drad = lsOriginal$cont$headers$url_ipfs_pred,
        license = lsOriginal$cont$license,
        id_acc_h = lsOriginal$cont$id_acc_h,
        license_application_rd = NULL, # TODO: Remove.
        commentary_r = NULL)
      
      resultsGood <- reactiveValues()
      resultsGood$license_r <- FALSE
      resultsGood$final_rd <- FALSE

      rvOther <- reactiveValues()
      rvOther$ipfsDRAD <- sprintf("https://%s.ipfs.w3s.link",
                                gsub(pattern = "ipfs://", 
                                     replacement = "", 
                                     x = lsOriginal$cont$headers$url_ipfs_pred, 
                                     fixed = TRUE))
      
      outputMsgs <- reactiveValues()
      # None.
      
      # preset inputs ----------------------------------------------------------
      
      # observe({
      #   updateActionLink(session = session, 
      #                    inputId = ns("alAD"), 
      #                    label = rvOther$ipfsDRAD)
      # })
      
      # module servers ---------------------------------------------------------
      
      modSrvrs <- reactiveValues()
      
      modSrvrs$license_r <- reviewFieldServer(
        id = 'license_r', 
        fieldKey = 'license', 
        decOpts = c('YES','NO'),
        fieldVal = lsOriginal$cont$license, 
        lblRevDecInpt = "Do you approve the issuance of the requested license to this agent?",
        lsPreset = list(rev_decision = lsPreset$license_application_rd, 
                        rev_feedback = lsPreset$commentary_r))
      
      # inputs -----------------------------------------------------------------
    
      observe({
        rvToReturn$results$license_application_rd <- modSrvrs$license_r$results$rev_decision
        rvToReturn$results$commentary_r <- modSrvrs$license_r$results$rev_feedback
        resultsGood$license_r <- modSrvrs$license_r$allResultsGood
      })
      
      observe({
        
        rvToReturn$results$final_rd <- 'REJECT'
        resultsGood$final_rd <- FALSE
        
        validate(need(rvToReturn$results$license_application_rd, message = FALSE))
        
        optsPass <- c('YES')
        bAllPass <- (rvToReturn$results$license_application_rd %in% optsPass)
        rvToReturn$results$final_rd <- c('REJECT', 'APPROVE')[1 + bAllPass]
        resultsGood$final_rd <- TRUE

      })
      
      # observeEvent(input$alAD, handlerExpr = {
      #   
      #   tryCatch({
      #     showModal(
      #       viewDocumentModal(uriIPFS = rvOther$ipfsDRAD, 
      #                         contentOnly = TRUE))
      #   }, error = function(e) {
      #     showModal(
      #       modalDialog(title = "Oops! Something went wrong.", 
      #                   sprintf("Please check your Internet connection. (Error: %s)",
      #                           e),
      #                   footer = NULL, 
      #                   size = "m", 
      #                   easyClose = TRUE))
      #   })
      #   
      # })
      
      observeEvent(input$abViewDRAD, handlerExpr = {
        
        showModal(loadingDocumentModal())
        
        q <- sprintf(
          "SELECT id FROM tbl_document_metadata WHERE id_message_h = '%s';",
          lsOriginal$cont$headers$id_msg_pred)
        idDocDRAD <- dbGetQuery(conn = dbCon, statement = q)[["id"]]
        
        docModal <- viewDocumentModal3(
          idDoc = idDocDRAD, 
          dbCon = dbCon, 
          contentOnly = FALSE)
        
        removeModal()
        
        showModal(docModal)
        
      })
      
      # outputs ----------------------------------------------------------------
      
      output$toDRADipfs <- renderText({
        rvOther$ipfsDRAD
      })
      
      output$toInvalidInput <- renderText({
        
        if (rvToReturn$allResultsGood) { return(NULL) }
        
        return(
          sprintf("Your inputs to the following fields are not valid: %s",
                  paste(names(resultsGood)[!unlist(reactiveValuesToList(resultsGood))],
                        sep = "", collapse = ", ")))
        
      })
      
      # return logic -----------------------------------------------------------
      
      observe({
        rvToReturn$allResultsGood <- all(unlist(reactiveValuesToList(resultsGood)))
      })
      
      return(rvToReturn)
      
    })
}
