
chooseFromTableUI <- function(id, hlpTxt) {
  
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 7,
        helpText(hlpTxt),
        DT::dataTableOutput(outputId = ns("dtoOptions"))),
      column(
        width = 5,
        wellPanel(
          HTML(sprintf('<h4>Description</h4>
                 <div id="%s" class="shiny-text-output"></div>',
                       ns("toDescr"))),   
          br(),
          actionButton(inputId = ns("abSelectOpt"), 
                       label = "Select")))))
  
}

chooseFromTableServer <- function(id, dfOpts, vnmsShow, vnmDescr) {
  
  moduleServer(
    id = id, 
    function(input, output, session) {
      
      ns <- session$ns
    
      rvToReturn <- reactiveValues()
      rvToReturn$goToModule <- NULL
      rvToReturn$allResultsGood <- FALSE
      rvToReturn$results <- reactiveValues(
        choice = NULL)
      
      rvOther <- reactiveValues()
      rvOther$dfOpts <- dfOpts
      rvOther$idxCurr <- NULL
      
      # inputs -----------------------------------------------------------------
      
      observeEvent(input$dtoOptions_cell_clicked, handlerExpr = {
        res <- input$dtoOptions_cell_clicked
        if (length(res) == 0) { 
          rvToReturn$allResultsGood <- FALSE
          rvOther$idxCurr <- NULL
          return() 
        }
        rvOther$idxCurr <- res$row
      })
      
      observe({
        shinyjs::toggle(id = "abSelectOpt", 
                        condition = (length(rvOther$idxCurr) == 1))
      })
      
      observeEvent(input$abSelectOpt, handlerExpr = {
        rvToReturn$results$choice <- rvOther$dfOpts[rvOther$idxCurr,]
        rvToReturn$allResultsGood <- TRUE
      })
      
      # outputs ----------------------------------------------------------------
      
      output$dtoOptions <- DT::renderDataTable({
        df <- rvOther$dfOpts[names(vnmsShow)]
        names(df) <- vnmsShow
        return(df)
      }, 
      selection = c('none', 'single')[2], 
      options = list(processing = FALSE, dom = "tp", scrollX = TRUE))
    
      output$toDescr <- renderText({
        validate(need(rvOther$idxCurr, message = FALSE))
        rvOther$dfOpts[[vnmDescr]][rvOther$idxCurr]
      })
      
      # return logic -----------------------------------------------------------
      
      return(rvToReturn)
      
    })
  
}