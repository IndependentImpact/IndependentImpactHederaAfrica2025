# Module for adding or editing a monitoring period for an activity (sometimes 
# called a "project").

monitoringPeriodsUI <- function(id, 
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
      DT::dataTableOutput(outputId = ns("dtoAllMonPers")),
      #tableOutput(outputId = ns("tbloAllMonPers")),
      textOutput(outputId = ns("toMain")),
      br()))
  }
  
  tagList(
    
    ##useShinyjs(),
    
    DT::dataTableOutput(outputId = ns("dtoAllMonPers")),
    #tableOutput(outputId = ns("tbloAllMonPers")),
    
    textOutput(outputId = ns("toMain")),
    br(),
    actionButton(inputId = ns("abAddMonPer"), label = "Add"),
    actionButton(inputId = ns("abEditMonPer"), label = "Edit"),
    actionButton(inputId = ns("abDeleteMonPer"), label = "Delete")
    
  )

}


monitoringPeriodsServer <- function(id,  
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
      rvOther$dfMonPers <- NULL
      rvOther$currentOperation <- c(NA_character_, "add", "edit", "delete")[1]
      rvOther$idxRowSelected <- 0
      
      outputMsgs <- reactiveValues()
      outputMsgs$main <- NULL
      outputMsgs$toAddOrEditModal <- NULL
      
      # aux --------------------------------------------------------------------
      
      addOrEditModal <- function(dtStart = NULL, dtEnd = NULL) {
        modalDialog(
          helpText("All dates must be entered in the YYYY-MM-DD hh:mm:ss format."),
          fluidRow(
            column(
              width = 6,
              textInput(
                inputId = ns("tiStartDate"), 
                label = "Start date:", 
                value = dtStart,
                placeholder = "YYYY-MM-DD hh:mm:ss")),
            column(
              width = 6,
              textInput(
                inputId = ns("tiEndDate"), 
                label = "End date:", 
                value = dtEnd,
                placeholder = "YYYY-MM-DD hh:mm:ss"))), 
          textOutput(outputId = ns("toAddOrEditModal")),
          title = ifelse(is.null(dtStart) & is.null(dtEnd), 
                         "Add new monitoring period",
                         "Edit monitoring period"), 
          size = "m", 
          footer = tagList(
            actionButton(
              inputId = ns("abAddOrEditModalOk"), label = "OK"),
            actionButton(
              inputId = ns("abModalCancel"), label = "Cancel")), 
          easyClose = FALSE)
      }
      
      # init -------------------------------------------------------------------
      
      observeEvent(rvOther$refresh, handlerExpr = {
        
        # Fetch all monitoring periods for this activity.
        df <- dbGetQuery(
          conn = dbCon, 
          statement = sprintf("SELECT * FROM tbl_monitoring_periods WHERE id_project = '%s';", idEntity))
        
        if (nrow(df) == 0) {
          rvOther$dfMonPers <- NULL
          outputMsgs$main <- "No monitoring periods have been added for this project yet."
          return(invisible(0))
        }
        
        df$date_start <- lubridate::force_tz(df$date_start, tzone = "UTC")
        df$date_end <- lubridate::force_tz(df$date_end, tzone = "UTC")
        
        df <- df[order(df$date_start, decreasing = TRUE),]
        rownames(df) <- 1:nrow(df)
        
        rvOther$dfMonPers <- df
        
        # Reset other vars
        rvOther$currentOperation <- NA_character_
        outputMsgs$main <- NULL
        
      })
      
      # inputs -----------------------------------------------------------------
      
      observeEvent(input$abAddMonPer, handlerExpr = {
        
        outputMsgs$main <- NULL
        
        if (!is.na(rvOther$currentOperation)) {
          outputMsgs$main <- "Please wait for the current operation to complete."
          return(invisible(0))
        }
        
        rvOther$currentOperation <- "add"
        showModal(addOrEditModal())
        
      })
      
      observeEvent(input$abEditMonPer, handlerExpr = {
        
        outputMsgs$main <- NULL
        
        if (!is.na(rvOther$currentOperation)) {
          outputMsgs$main <- "Please wait for the current operation to complete."
          return(invisible(0))
        }
        
        selection <- input$dtoAllMonPers_cell_clicked
        if (length(selection) == 0) { 
          outputMsgs$main <- "No monitoring period selected."
          return()
        }
        
        rvOther$currentOperation <- "edit"
        rvOther$idxRowSelected <- selection$row
        
        # Add "00:00:00" time where appropriate.
        dtStart <- as.character(rvOther$dfMonPers$date_start[selection$row])
        if (nchar(dtStart) == 10) {
          dtStart <- sprintf("%s 00:00:00", dtStart)
        }
        dtEnd <- as.character(rvOther$dfMonPers$date_end[selection$row])
        if (nchar(dtEnd) == 10) {
          dtEnd <- sprintf("%s 00:00:00", dtEnd)
        }
        
        showModal(
          addOrEditModal(dtStart = dtStart, dtEnd = dtEnd))
        
      })
      
      observeEvent(input$abDeleteMonPer, handlerExpr = {
        
        outputMsgs$main <- NULL
        
        if (!is.na(rvOther$currentOperation)) {
          outputMsgs$main <- "Please wait for the current operation to complete."
          return(invisible(0))
        }
        
        selection <- input$dtoAllMonPers_cell_clicked
        if (length(selection) == 0) { 
          outputMsgs$main <- "No monitoring period selected."
          return()
        }
        
        rvOther$currentOperation <- "delete"
        rvOther$idxRowSelected <- selection$row
        
        showModal(
          modalDialog(
            title = "Confirm Deletion", 
            "Are you sure you want to delete this monitoring period?",
            br(),
            sprintf("%s to %s", 
                    rvOther$dfMonPers$date_start[selection$row],
                    rvOther$dfMonPers$date_end[selection$row]),
            footer = tagList(
              actionButton(inputId = ns("abConfirmDelete"), label = "Yes"),
              actionButton(inputId = ns("abModalCancel"), label = "Cancel")), 
            easyClose = FALSE, 
            size = "s"))
      })
      
      observeEvent(input$abAddOrEditModalOk, handlerExpr = {
        
        # Input validation.
        {
          outputMsgs$toAddOrEditModal <- NULL
          
          if (is.null(emptyOrMissingAsNull(input$tiStartDate, ignoreSpaces = TRUE))) {
            outputMsgs$toAddOrEditModal <- "Please provide a valid start date."
            return(invisible(0))
          }
          
          if (is.null(emptyOrMissingAsNull(input$tiEndDate, ignoreSpaces = TRUE))) {
            outputMsgs$toAddOrEditModal <- "Please provide a valid end date."
            return(invisible(0))
          }
          
          dtStart <- lubridate::ymd_hms(input$tiStartDate, tz = "UTC")
          dtEnd  <- lubridate::ymd_hms(input$tiEndDate, tz = "UTC")
          
          if (is.na(dtStart) | is.na(dtEnd)) {
            outputMsgs$toAddOrEditModal <- "Invalid date format."
            return(invisible(0))
          }
          
          if (dtEnd <= dtStart) {
            outputMsgs$toAddOrEditModal <- "End date cannot be earlier than or the same as the start date."
            return(invisible(0))
          }
        }
        
        # Check that this new monitoring period does not overlap with any existing monitoring periods.
        if (!is.null(rvOther$dfMonPers)) {
          if (nrow(rvOther$dfMonPers) > 0) {
            
            idxx <- 1:nrow(rvOther$dfMonPers)
            if (rvOther$idxRowSelected > 0 & rvOther$currentOperation == "edit") {
              idxx <- idxx[-rvOther$idxRowSelected]
            }
            for (r in idxx) {
              
              if ((dtEnd >= rvOther$dfMonPers$date_start[r] & 
                   dtEnd <= rvOther$dfMonPers$date_end[r]) |
                  (dtStart >= rvOther$dfMonPers$date_start[r] &
                   dtStart <= rvOther$dfMonPers$date_end[r]) |
                  (dtStart <= rvOther$dfMonPers$date_start[r]) &
                  (dtEnd >= rvOther$dfMonPers$date_end[r])) {
                
                outputMsgs$toAddOrEditModal <- "Error: This monitoring period will overlap with existing monitoring periods."
                return(invisible(0))
                
              }
              
            }
            
          }
        }
        
        # Add "00:00:00" time where appropriate.
        dtStart <- as.character(dtStart)
        if (nchar(dtStart) == 10) {
          dtStart <- sprintf("%s 00:00:00", dtStart)
        }
        dtEnd <- as.character(dtEnd)
        if (nchar(dtEnd) == 10) {
          dtEnd <- sprintf("%s 00:00:00", dtEnd)
        }
        
        if (rvOther$currentOperation == "add") {
          
          dfAdd <- data.frame(
            id_project = idEntity,
            date_start = dtStart,
            date_end = dtEnd)
          addToDb(
            dfAdd = dfAdd, 
            tblNm = "tbl_monitoring_periods", 
            vnmsChckEx = names(dfAdd), 
            dbCon = dbCon, 
            calcIds = FALSE)
          
        } 
        
        if (rvOther$currentOperation == "edit") {
          
          oidx <- rvOther$dfMonPers$oidx[rvOther$idxRowSelected]
          q <- sprintf("UPDATE tbl_monitoring_periods SET date_start = '%s', date_end = '%s' WHERE id_project = '%s' AND oidx = %d;",
                       dtStart,
                       dtEnd,
                       idEntity,
                       oidx)
          res <- dbSendStatement(conn = dbCon, statement = q)
          dbClearResult(res)
          
        }
        
        # Refresh data table output.
        rvOther$refresh <- Sys.time()
        removeModal()
        
      })
      
      observeEvent(input$abModalCancel, handlerExpr = {
        removeModal()
        outputMsgs$toAddOrEditModal <- NULL
        rvOther$currentOperation <- NA_character_
        rvOther$refresh <- Sys.time()
      })
      
      observeEvent(input$abConfirmDelete, handlerExpr = {
        
        dbSendStatement(
          conn = dbCon, 
          statement = sprintf("DELETE FROM tbl_monitoring_periods WHERE id_project = '%s' AND oidx = %d;",
                              idEntity,
                              rvOther$dfMonPers$oidx[rvOther$idxRowSelected]))
        
        # Refresh data table output.
        rvOther$refresh <- Sys.time()
        rvOther$currentOperation <- NA_character_
        removeModal()
        
      })
      
      # outputs ----------------------------------------------------------------
      
      output$toMain <- renderText({
        outputMsgs$main
      })
      
      output$toAddOrEditModal <- renderText({
        outputMsgs$toAddOrEditModal
      })
      
      output$dtoAllMonPers <- DT::renderDataTable({
        df <- rvOther$dfMonPers
        if (is.null(df)) {
          return(NULL)
        }
        df <- df[c("date_start", "date_end")]
        #df <- DT::datatable(df) %>% DT::formatDate(columns = c(1,2), method = "toUTCString")
        for (vnm in c("date_start", "date_end")) {
          df[[vnm]] <- as.character(df[[vnm]])
          # Add "00:00:00" time where appropriate.
          idxx <- which(nchar(df[[vnm]]) == 10)
          df[idxx,vnm] <- sprintf("%s 00:00:00", df[idxx,vnm])
        }
        return(df)
      },
      escape = F, rownames = TRUE, selection = c('none', 'single')[2],
      options = list(
        processing = FALSE,
        dom = "t",
        scrollY = TRUE))
      
      # output$tbloAllMonPers <- renderTable({
      #   df <- rvOther$dfMonPers
      #   df <- df[c("date_start", "date_end")]
      #   return(df)
      # })
      
      # return logic -----------------------------------------------------------
      
      rvOther$refresh <- Sys.time()
      
      return(rvToReturn)
    })
}
