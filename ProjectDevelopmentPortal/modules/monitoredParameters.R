# Module for adding or editing a monitored parameter for an activity (sometimes 
# called a "project").

monitoredParametersUI <- function(id, 
                                  lsPreset = NULL, 
                                  hL = 4, 
                                  colWidth = 12, 
                                  inpWidth = DEFAULT_INP_WIDTH,
                                  viewOnly = TRUE) {
  
  ns <- NS(id)
  
  if (viewOnly) {
    return(
      tagList(
        ##useShinyjs(),
        DT::dataTableOutput(outputId = ns("dtoAllMonParams")),
        #tableOutput(outputId = ns("tbloAllMonParams")),
        textOutput(outputId = ns("toMain")),
        br()))
  }
  
  tagList(
    
    ##useShinyjs(),
    
    DT::dataTableOutput(outputId = ns("dtoAllMonParams")),
    #tableOutput(outputId = ns("tbloAllMonParams")),
    
    textOutput(outputId = ns("toMain")),
    br(),
    actionButton(inputId = ns("abAddMonParam"), label = "Add"),
    actionButton(inputId = ns("abEditMonParam"), label = "Edit"),
    actionButton(inputId = ns("abDeleteMonParam"), label = "Delete")
    
  )
  
}


monitoredParametersServer <- function(id,  
                                      dbCon,
                                      idEntity,
                                      lsPreset = NULL) {
  
  moduleServer(
    id = id, 
    function(input, output, session) {
      
      ns <- session$ns
      
      rvToReturn <- reactiveValues()
      rvToReturn$allResultsGood <- FALSE
      rvToReturn$results <- reactiveValues()
      
      rvOther <- reactiveValues()
      rvOther$refresh <- NULL
      rvOther$nonce <- 0
      rvOther$dfMonParams <- NULL
      rvOther$currentOperation <- c(NA_character_, "add", "edit", "delete")[1]
      rvOther$idxRowSelected <- 0
      
      outputMsgs <- reactiveValues()
      outputMsgs$main <- NULL
      outputMsgs$toAddOrEditModal <- NULL
      
      # aux --------------------------------------------------------------------
      
      addOrEditModal <- function(dfEx = NULL) {
        modalDialog(
          title = ifelse(length(dfEx) == 0, 
                         "Add new parameter",
                         "Edit parameter"), 
          impactParameterUI(
            id = ns(sprintf("mImpactParam%s", rvOther$nonce)), 
            paramType = "monitored"), 
          size = "l", 
          footer = tagList(
            textOutput(outputId = ns("toAddOrEditModal")),
            actionButton(
              inputId = ns("abAddOrEditModalOk"), label = "OK"),
            actionButton(
              inputId = ns("abModalCancel"), label = "Cancel")), 
          easyClose = FALSE)
      }
      
      # init -------------------------------------------------------------------
      
      # module servers----------------------------------------------------------
      modSrvrs <- reactiveValues()
      modSrvrs$mImpactParam <- NULL
      
      observeEvent(rvOther$refresh, handlerExpr = {
        
        # Fetch all monitored parameters for this activity.
        df <- dbGetQuery(
          conn = dbCon, 
          statement = sprintf("SELECT * FROM tbl_monitored_parameters WHERE id_project = '%s';", idEntity))
        
        if (nrow(df) == 0) {
          rvOther$dfMonParams <- NULL
          outputMsgs$main <- "No monitored parameters have been added for this project yet."
          return(invisible(0))
        }
        
        rownames(df) <- 1:nrow(df)
        
        rvOther$dfMonParams <- df
        
        # Reset other vars
        rvOther$currentOperation <- NA_character_
        outputMsgs$main <- NULL
        
      })
      
      # inputs -----------------------------------------------------------------
      
      observeEvent(input$abAddMonParam, handlerExpr = {
        
        outputMsgs$main <- NULL
        
        if (!is.na(rvOther$currentOperation)) {
          outputMsgs$main <- "Please wait for the current operation to complete."
          return(invisible(0))
        }
        
        rvOther$currentOperation <- "add"
        rvOther$nonce <- rvOther$nonce +1
        modSrvrs$mImpactParam <- impactParameterServer(
          id = sprintf("mImpactParam%s", rvOther$nonce),
          paramType = "monitored")
        showModal(addOrEditModal())
        
      })
      
      observeEvent(input$abEditMonParam, handlerExpr = {
        
        outputMsgs$main <- NULL
        
        if (!is.na(rvOther$currentOperation)) {
          outputMsgs$main <- "Please wait for the current operation to complete."
          return(invisible(0))
        }
        
        selection <- input$dtoAllMonParams_cell_clicked
        if (length(selection) == 0) { 
          outputMsgs$main <- "No parameter selected."
          return()
        }
        
        rvOther$currentOperation <- "edit"
        rvOther$idxRowSelected <- selection$row
        
        dfEdit <- rvOther$dfMonParams[selection$row, ]
        dfEdit <- gdata::rename.vars(
          data = dfEdit, 
          from = c("label", "unit_of_measure", "data_source", "monitoring_procedures"), 
          to = c("data_parameter", "data_unit", "source_of_data", "methods_and_procedures"), 
          info = FALSE)
        
        
        rvOther$nonce <- rvOther$nonce +1
        modSrvrs$mImpactParam <- impactParameterServer(
          id = sprintf("mImpactParam%s", rvOther$nonce),
          paramType = "monitored", 
          lsPreset = dfEdit)
        showModal(addOrEditModal(dfEdit))

      })
      
      observeEvent(input$abDeleteMonParam, handlerExpr = {
        
        outputMsgs$main <- NULL
        
        if (!is.na(rvOther$currentOperation)) {
          outputMsgs$main <- "Please wait for the current operation to complete."
          return(invisible(0))
        }
        
        selection <- input$dtoAllMonParams_cell_clicked
        if (length(selection) == 0) { 
          outputMsgs$main <- "No parameter selected."
          return()
        }
        
        rvOther$currentOperation <- "delete"
        rvOther$idxRowSelected <- selection$row
        
        showModal(
          modalDialog(
            title = "Confirm Deletion", 
            sprintf("Are you sure you want to delete parameter '%s'?",
                    rvOther$dfMonParams$label[selection$row]),
            footer = tagList(
              actionButton(inputId = ns("abConfirmDelete"), label = "Yes"),
              actionButton(inputId = ns("abModalCancel"), label = "Cancel")), 
            easyClose = FALSE, 
            size = "s"))
      })
      
      observeEvent(input$abAddOrEditModalOk, handlerExpr = {
        
        if (!modSrvrs$mImpactParam$allResultsGood) {
          outputMsgs$toAddOrEditModal <- "Some inputs are not valid. Please review your inputs."
          return(invisible(NULL))
        }
        
        df <- as.data.frame(reactiveValuesToList(modSrvrs$mImpactParam$results))
        df$id_project <- idEntity
        df <- gdata::rename.vars(
          data = df, 
          from = c("data_parameter", "data_unit", "source_of_data", "methods_and_procedures"), 
          to = c("label", "unit_of_measure", "data_source", "monitoring_procedures"),
          info = FALSE)
        
        if (rvOther$currentOperation == "add") {
          addToDb(
            dfAdd = df, 
            tblNm = "tbl_monitored_parameters", 
            vnmsChckEx = names(df), 
            dbCon = dbCon, 
            calcIds = FALSE)
        } 
        
        if (rvOther$currentOperation == "edit") {
          df$oidx <- rvOther$dfMonParams$oidx[rvOther$idxRowSelected]
          updateDb(
            df = df, 
            tblNm = "tbl_monitored_parameters", 
            idVars = c("oidx", "id_project"), 
            dbCon = dbCon)
        }
        
        # Refresh data table output and reset UI.
        rvOther$refresh <- Sys.time()
        modSrvrs$mImpactParam <- NULL
        removeModal()
        
        
      })
      
      observeEvent(input$abModalCancel, handlerExpr = {
        modSrvrs$mImpactParam <- NULL
        removeModal()
        rvOther$currentOperation <- NA_character_
        rvOther$refresh <- Sys.time()
      })
      
      observeEvent(input$abConfirmDelete, handlerExpr = {
        
        dbSendStatement(
          conn = dbCon, 
          statement = sprintf("DELETE FROM tbl_monitored_parameters WHERE id_project = '%s' AND oidx = %d;",
                              idEntity,
                              rvOther$dfMonParams$oidx[rvOther$idxRowSelected]))
        
        # Refresh data table output.
        rvOther$refresh <- Sys.time()
        rvOther$currentOperation <- NA_character_
        removeModal()
        
      })
      
      # outputs ----------------------------------------------------------------
      
      output$toMain <- renderText({
        outputMsgs$main
      })
      
      output$dtoAllMonParams <- DT::renderDataTable({
        df <- rvOther$dfMonParams
        if (is.null(df)) {
          return(NULL)
        }
        df <- df[setdiff(names(df), c("oidx", "id_project"))]
        return(df)
      },
      escape = F, rownames = TRUE, selection = c('none', 'single')[2],
      options = list(
        processing = FALSE,
        dom = "t",
        scrollX = TRUE,
        scrollY = TRUE))
      
      # output$tbloAllMonParams <- renderTable({
      #   df <- rvOther$dfMonParams
      #   df <- df[c("date_start", "date_end")]
      #   return(df)
      # })
      
      output$toAddOrEditModal <- renderText({ # Lives inside the modal.
        outputMsgs$toAddOrEditModal
      })
      
      # return logic -----------------------------------------------------------
      
      rvOther$refresh <- Sys.time()
      
      return(rvToReturn)
    })
}
