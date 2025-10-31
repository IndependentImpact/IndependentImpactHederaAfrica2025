
# Module for schema 'Impact [v2.0.0]'.

impactUI <- function(id, 
                     lsPreset = NULL, 
                     hL = 4, 
                     colWidth = 12, 
                     inpWidth = DEFAULT_INP_WIDTH) {
  
  ns <- NS(id)
  
  tl <- tagList(
    
    #useShinyjs(),
    
    fluidRow(
      column(
        width = colWidth,
        wellPanel(
          hx(x = "Intentionality", lvl = hL),
          helpText("Is this an intentional impact or an unintentional impact (e.g., is it a goal or is it leakage)?"),
          selectInput(
            inputId = ns("intentionality"), 
            label = NULL, 
            choices = c("INTENTIONAL", "UNINTENTIONAL"), 
            selected = lsPreset$intentionality,
            multiple = FALSE, 
            width = inpWidth, 
            selectize = TRUE)))),
    
    fluidRow(
      column(
        width = colWidth,
        wellPanel(
          hx(x = "Beneficial or Adverse", lvl = hL),
          helpText("Is this impact an environmentally beneficial impact or an environmentally adverse impact?"),
          selectInput(
            inputId = ns("beneficial_or_adverse"), 
            label = NULL, 
            choices = c("BENEFICIAL", "ADVERSE"), 
            selected = lsPreset$beneficial_or_adverse,
            multiple = FALSE, 
            width = inpWidth, 
            selectize = TRUE)))),
    
    GtextAreaInput(id = ns('description'),
                   title = 'Description',
                   helpTxt = 'Description. (10 chars. min.)', 
                   value = lsPreset$description,
                   resize = 'vertical',
                   hL = hL,
                   colWidth = colWidth,
                   inpWidth = inpWidth),
    
    fluidRow(
      column(
        width = colWidth,
        wellPanel(
          hx(x = "Monitored", lvl = hL),
          helpText("Will this impact be monitored (quantified) or not?"),
          selectInput(
            inputId = ns("monitored"), 
            label = NULL, 
            choices = c("YES", "NO"), 
            selected = lsPreset$monitored,
            multiple = FALSE, 
            width = inpWidth, 
            selectize = TRUE)))),
    
    uiOutput(outputId = ns("uioMain")), 
    
    textOutput(outputId = ns("toInvalidInput")),
    
    br())
}

impactServer <- function(id,  
                         lsPreset = NULL, 
                         hL = 4, 
                         colWidth = 12, 
                         inpWidth = DEFAULT_INP_WIDTH) {
  
  if (length(lsPreset) == 0) {
    lsPreset <- list()
  }
  lsPreset2 <- lsPreset
  
  moduleServer(
    id = id, 
    function(input, output, session) {
      
      ns <- session$ns
      
      rvToReturn <- reactiveValues()
      rvToReturn$allResultsGood <- FALSE
      
      rvToReturn$results <- reactiveValues(
        intentionality = NULL,
        beneficial_or_adverse = NULL,
        description = NULL,
        monitored = NULL,
        
        not_monitored_justification = NULL,
        
        indicator = NULL,
        indicator_label = NULL,
        indicator_unit_of_measure = NULL,
        methodologies = NULL,
        additionality = NULL,
        data_and_parameters = NULL,
        crediting_period = NULL,
        crediting_period_type = NULL,
        monitoring_periods = NULL,
        state_baseline = NULL,
        state_project = NULL,
        impact_estimation_ex_ante = NULL)
      
      resultsGood <- reactiveValues()
      resultsGood$intentionality <- FALSE
      resultsGood$beneficial_or_adverse <- FALSE
      resultsGood$description <- FALSE
      resultsGood$monitored <- FALSE
      
      resultsGoodF <- reactiveValues()
      resultsGoodF$not_monitored_justification <- FALSE
      
      resultsGoodM <- reactiveValues()
      resultsGoodM$indicator <- FALSE
      resultsGoodM$indicator_label <- FALSE
      resultsGoodM$indicator_unit_of_measure <- FALSE
      resultsGoodM$methodologies <- FALSE
      resultsGoodM$additionality <- FALSE
      resultsGoodM$data_and_parameters <- FALSE
      resultsGoodM$crediting_period <- FALSE
      resultsGoodM$crediting_period_type <- FALSE
      resultsGoodM$monitoring_periods <- FALSE
      resultsGoodM$state_baseline <- FALSE
      resultsGoodM$state_project <- FALSE
      resultsGoodM$impact_estimation_ex_ante <- FALSE

      outputMsgs <- reactiveValues()
      outputMsgs$intentionality <- ''
      outputMsgs$beneficial_or_adverse <- ''
      outputMsgs$description <- ''
      outputMsgs$monitored <- ''
      outputMsgs$not_monitored_justification <- ''
      outputMsgs$indicator <- ''
      outputMsgs$indicator_label <- ''
      outputMsgs$indicator_unit_of_measure <- ''
      outputMsgs$methodologies <- ''
      outputMsgs$additionality <- ''
      outputMsgs$data_and_parameters <- ''
      outputMsgs$crediting_period <- ''
      outputMsgs$crediting_period_type <- ''
      outputMsgs$monitoring_periods <- ''
      outputMsgs$state_baseline <- ''
      outputMsgs$state_project <- ''
      outputMsgs$impact_estimation_ex_ante <- ''
      
      rvOther <- reactiveValues()
      rvOther$nonce <- 0
      
      # UI rendering -----------------------------------------------------------
      
      output$uioMain <- renderUI({
        
        if (!resultsGood$monitored) {
          return(NULL)
        }
        
        if (rvToReturn$results$monitored == "NO") {
          return(
            GtextAreaInput(id = ns("not_monitored_justification"), 
                           title = "Justification", 
                           helpTxt = "Why will this impact not be monitored? (10 chars. min.)", 
                           value = lsPreset2$not_monitored_justification,
                           hL = hL, 
                           colWidth = colWidth, 
                           inpWidth = inpWidth))
        }
        
        tagList(
          GtextAreaInput(id = ns('indicator'),
                         title = 'Indicator',
                         helpTxt = 'Describe the state indicator that will be used to measure the impact as a state difference (20 chars. min.):', 
                         value = lsPreset2$indicator,
                         resize = 'vertical',
                         hL = hL,
                         colWidth = colWidth,
                         inpWidth = inpWidth),
          
          GtextInput(id = ns('indicator_label'),
                         title = 'Label',
                         helpTxt = 'Provide a unique, short label for the indicator (3 chars. min.):', 
                         value = lsPreset2$indicator_label,
                         hL = hL,
                         colWidth = colWidth,
                         inpWidth = inpWidth),
          
          GtextInput(id = ns('indicator_unit_of_measure'),
                     title = 'Unit of Measure',
                     helpTxt = 'What is the unit of measure for the indicator?', 
                     value = lsPreset2$indicator_unit_of_measure,
                     hL = hL,
                     colWidth = colWidth,
                     inpWidth = inpWidth),
          
          wellPanel(
            h4('Methodologies'),
            helpText("Which methodologies will you follow to monitor and/or quantify this impact?"),
            dynamicMultiV2Input(
              id = ns(sprintf('methodologies%d', isolate(rvOther$nonce))), 
              useModal = FALSE,
              nmUImod = "methodologyApplicabilityInput", 
              lsArgsModUI = list(hL = hL, colWidth = colWidth, inpWidth = inpWidth)),
            br(),
            textOutput(outputId = ns("methodologies_msg"))),
          
          GtextAreaInput(id = ns('additionality'),
                         title = 'Additionality',
                         helpTxt = 'Justify why you consider this impact to be additional (10 chars. min.):', 
                         value = lsPreset2$additionality,
                         resize = 'vertical',
                         hL = hL,
                         colWidth = colWidth,
                         inpWidth = inpWidth),
          
          wellPanel(
            h4('Supporting Data and Parameters'),
            helpText("What parameters will be monitored, or what data needs to be obtained, to enable the quantification of this impact?"),
            dynamicMultiV2Input(
              id = ns(sprintf('data_and_parameters%d', isolate(rvOther$nonce))),
              useModal = FALSE,
              nmUImod = "tblRowDataParamMonV2UI", 
              lsArgsModUI = list(hL = hL, colWidth = colWidth, inpWidth = inpWidth)),
            br(),
            textOutput(outputId = ns("data_and_parameters_msg"))),
      
          
          dateTimePeriodInput(
            id = ns(sprintf("crediting_period%d", isolate(rvOther$nonce))), 
            title = "Crediting Period", 
            helpTxt = "When is the (first) crediting period for this impact?",
            #lsPreset = isolate(rvToReturn$results$crediting_period), # isolate() did not work 
            lsPreset = lsPreset2$crediting_period,
            hL = hL, 
            colWidth = colWidth, 
            inpWidth = inpWidth),
          
          fluidRow(
            column(
              width = colWidth,
              wellPanel(
                hx(x = "Crediting Period Type", lvl = hL),
                helpText("Will this crediting period be renewable or non-renewable?"),
                selectInput(
                  inputId = ns("crediting_period_type"), 
                  label = NULL, 
                  choices = c("RENEWABLE", "NON_RENEWABLE"), 
                  selected = lsPreset2$crediting_period_type,
                  multiple = FALSE, 
                  width = inpWidth, 
                  selectize = TRUE)))),

          wellPanel(
            h4('Monitoring Periods'),
            helpText("What will the monitoring periods be for the (first) crediting period of this impact?"),
            dynamicMultiV2Input(
              id = ns(sprintf('monitoring_periods%d', isolate(rvOther$nonce))),
              useModal = FALSE,
              nmUImod = "dateTimePeriodInput", 
              lsArgsModUI = list(title = NULL, helpTxt = NULL, 
                hL = hL, colWidth = colWidth, inpWidth = inpWidth)),
            br(),
            textOutput(outputId = ns("monitoring_periods_msg"))),
          
          # Making use of a textInput instead of a numericInput, because the stepping
          # business and the max and min params of the numericInput don't provide what
          # we need for this field.
          GtextInput(id = ns('state_baseline'),
                     title = 'Baseline State',
                     helpTxt = 'What is the value of this indicator in the baseline state? Use the unit of measure identified earlier.', 
                     value = lsPreset2$state_baseline,
                     hL = hL,
                     colWidth = colWidth,
                     inpWidth = inpWidth),
          
          # Making use of a textInput instead of a numericInput, because the stepping
          # business and the max and min params of the numericInput don't provide what
          # we need for this field.
          GtextInput(id = ns('state_project'),
                     title = 'Project State',
                     helpTxt = 'What is the expected value for this indicator in the project state? Provide it for the (first) crediting period and in the unit of measure identified earlier.', 
                     value = lsPreset2$state_project,
                     hL = hL,
                     colWidth = colWidth,
                     inpWidth = inpWidth),
          
          GtextOutput(id = ns("impact_estimation_ex_ante"), 
                      title = "Ex Ante Impact Estimation", 
                      subTxt = "Ex ante estimated impact for first crediting period (in the indicator's unit of measure identified earlier):", 
                      hL = hL, 
                      colWidth = colWidth))
      })
      
      # preset inputs ----------------------------------------------------------
      
      if ('intentionality' %in% names(lsPreset2)) {
        updateSelectInput(session = session, 
                        inputId = 'intentionality',
                        selected = lsPreset2$intentionality)
      }
      
      if ('beneficial_or_adverse' %in% names(lsPreset2)) {
        updateSelectInput(session = session, 
                          inputId = 'beneficial_or_adverse',
                          selected = lsPreset2$beneficial_or_adverse)
      }
      
      if ('description' %in% names(lsPreset2)) {
        updateTextAreaInput(session = session, 
                            inputId = 'description', 
                            value = lsPreset2$description)
      }
      
      # module servers ---------------------------------------------------------
      modSrvrs <- reactiveValues()
      modSrvrs$methodologies <- NULL
      modSrvrs$data_and_parameters <- NULL
      modSrvrs$crediting_period <- NULL
      modSrvrs$monitoring_periods <- NULL
      
      observe({
        
        if (!resultsGood$monitored) {
          modSrvrs$methodologies <- NULL
          modSrvrs$data_and_parameters <- NULL
          modSrvrs$crediting_period <- NULL
          modSrvrs$monitoring_periods <- NULL
          return(invisible(0))
        }
        
        if (rvToReturn$results$monitored == "NO") {
          modSrvrs$methodologies <- NULL
          modSrvrs$data_and_parameters <- NULL
          modSrvrs$crediting_period <- NULL
          modSrvrs$monitoring_periods <- NULL
          return(invisible(0))
        }
        
        modSrvrs$methodologies <- dynamicMultiV2Server(
          id = sprintf("methodologies%d", isolate(rvOther$nonce)), 
          lsPreset = lsPreset2$methodologies,
          nmSrvrMod = "methodologyApplicabilityServer", 
          nmUImod = "methodologyApplicabilityInput", 
          lsArgsModSrvr = NULL,
          useModal = FALSE)
        
        modSrvrs$data_and_parameters <- dynamicMultiV2Server(
          id = sprintf("data_and_parameters%d", isolate(rvOther$nonce)),
          lsPreset = lsPreset2$data_and_parameters,
          nmSrvrMod = "tblRowDataParamMonV2Server", 
          nmUImod = "tblRowDataParamMonV2UI",
          lsArgsModSrvr = list(hL = hL, colWidth = colWidth, inpWidth = inpWidth),
          useModal = FALSE)
        
        modSrvrs$crediting_period <- dateTimePeriodServer(
          id = sprintf("crediting_period%d", isolate(rvOther$nonce)),
          lsPreset = lsPreset2$crediting_period)
        
        modSrvrs$monitoring_periods <- dynamicMultiV2Server(
          id = sprintf("monitoring_periods%d", isolate(rvOther$nonce)), 
          lsPreset = lsPreset2$monitoring_periods,
          nmSrvrMod = "dateTimePeriodServer", 
          nmUImod = "dateTimePeriodInput",
          lsArgsModSrvr = NULL,
          lsArgsModUI = list(title = "Monitoring Period"),
          useModal = FALSE)
      })

      # inputs -----------------------------------------------------------------
      
      observeEvent(input$intentionality, handlerExpr = {
        
        resultsGood$intentionality <- FALSE
        outputMsgs$intentionality <- ''
        rvToReturn$results$intentionality <- input$intentionality
        validate(need(input$intentionality, message = FALSE))
        if (length(input$intentionality) == 0) { return(invisible(NULL)) }
        resultsGood$intentionality <- TRUE
        
      })
      
      observeEvent(input$beneficial_or_adverse, handlerExpr = {
        
        resultsGood$beneficial_or_adverse <- FALSE
        outputMsgs$beneficial_or_adverse <- ''
        rvToReturn$results$beneficial_or_adverse <- input$beneficial_or_adverse
        validate(need(input$beneficial_or_adverse, message = FALSE))
        if (length(input$beneficial_or_adverse) == 0) { return(invisible(NULL)) }
        resultsGood$beneficial_or_adverse <- TRUE
        
      })
      
      observeEvent(input$description, handlerExpr = {
        
        resultsGood$description <- FALSE
        rvToReturn$results$description <- input$description
        
        nChar <- nchar(input$description)
        outputMsgs$description <- sprintf('%d chars. remaining', 300 - nChar)
        
        resultsGood$description <- isValidInput.text(
          x = input$description, 
          bRequired = TRUE, 
          nCharMin = 10, 
          nCharMax = 300) 
        
      })
      
      observeEvent(input$monitored, handlerExpr = {
        
        resultsGood$monitored <- FALSE
        outputMsgs$monitored <- ''
        rvToReturn$results$monitored <- input$monitored
        validate(need(input$monitored, message = FALSE))
        
        rvOther$nonce <- rvOther$nonce + 1
        
        if (length(input$monitored) == 0) { return(invisible(NULL)) }
        resultsGood$monitored <- TRUE

        if (rvToReturn$results$monitored == "YES") {
          rvToReturn$results$not_monitored_justification <- NULL
        } else {
          rvToReturn$results$indicator <- NULL
          rvToReturn$results$indicator_label <- NULL
          rvToReturn$results$indicator_unit_of_measure <- NULL
          rvToReturn$results$methodologies <- NULL
          rvToReturn$results$additionality <- NULL
          rvToReturn$results$data_and_parameters <- NULL
          rvToReturn$results$crediting_period <- NULL
          rvToReturn$results$crediting_period_type <- NULL
          rvToReturn$results$monitoring_periods <- NULL
          rvToReturn$results$state_baseline <- NULL
          rvToReturn$results$state_project <- NULL
          rvToReturn$results$impact_estimation_ex_ante <- NULL
        }
        # Note: Doing it the long way, because "Can't index reactivevalues with `[`."
      })

      # FIXED
      observeEvent(input$not_monitored_justification, handlerExpr = {
        
        resultsGoodF$not_monitored_justification <- FALSE
        rvToReturn$results$not_monitored_justification <- input$not_monitored_justification
        
        lsPreset2$not_monitored_justification <- input$not_monitored_justification 
        # Updating lsPreset2 here so that we can restore the user's response in case they 
        # accidentally switches to "Monitored" = "YES" and then want to switch back.
        
        nChar <- nchar(input$not_monitored_justification)
        outputMsgs$not_monitored_justification <- sprintf('%d chars. remaining', 200 - nChar)
        
        resultsGoodF$not_monitored_justification <- isValidInput.text(
          x = input$not_monitored_justification, 
          bRequired = TRUE, 
          nCharMin = 10, 
          nCharMax = 200) 
        
      })
      
      # MONITORED
      observeEvent(input$indicator, handlerExpr = {
        
        resultsGoodM$indicator <- FALSE
        rvToReturn$results$indicator <- input$indicator
        
        nChar <- nchar(input$indicator)
        outputMsgs$indicator <- sprintf('%d chars. remaining', 200 - nChar)
        
        lsPreset2$indicator <- input$indicator
        # Updating lsPreset2 here so that we can restore the user's response in case they 
        # accidentally switches to "Monitored" = "NO" and then want to switch back.
        
        resultsGoodM$indicator <- isValidInput.text(
          x = input$indicator, 
          bRequired = TRUE, 
          nCharMin = 20, 
          nCharMax = 200) 
        
      })

      observeEvent(input$indicator_label, handlerExpr = {
        
        resultsGoodM$indicator_label <- FALSE
        rvToReturn$results$indicator_label <- input$indicator_label
        
        nChar <- nchar(input$indicator_label)
        outputMsgs$indicator_label <- sprintf('%d chars. remaining', 20 - nChar)
        
        lsPreset2$indicator_label <- input$indicator_label
        # Updating lsPreset2 here so that we can restore the user's response in case they 
        # accidentally switches to "Monitored" = "NO" and then want to switch back.
        
        resultsGoodM$indicator_label <- isValidInput.text(
          x = input$indicator_label, 
          bRequired = TRUE, 
          nCharMin = 3,
          nCharMax = 20) 
        
      })
      
      observeEvent(input$indicator_unit_of_measure, handlerExpr = {
        
        resultsGoodM$indicator_unit_of_measure <- FALSE
        rvToReturn$results$indicator_unit_of_measure <- input$indicator_unit_of_measure
      
        nChar <- nchar(input$indicator_unit_of_measure)
        outputMsgs$indicator_unit_of_measure <- sprintf('%d chars. remaining', 10 - nChar)
      
        lsPreset2$indicator_unit_of_measure <- input$indicator_unit_of_measure
        # Updating lsPreset2 here so that we can restore the user's response in case they 
        # accidentally switches to "Monitored" = "NO" and then want to switch back.
          
        resultsGoodM$indicator_unit_of_measure <- isValidInput.text(
          x = input$indicator_unit_of_measure, 
          bRequired = TRUE, 
          nCharMin = 1,
          nCharMax = 10) 
        
      })
      
      observe({
        
        resultsGoodM$methodologies <- FALSE
        outputMsgs$methodologies <- ''
        
        if (length(modSrvrs$methodologies) == 0) {
          return(invisible(0))
        }
        
        rvToReturn$results$methodologies <- modSrvrs$methodologies$items
        
        lsPreset2$methodologies <- modSrvrs$methodologies$items
        # Updating lsPreset2 here so that we can restore the user's response in case they 
        # accidentally switches to "Monitored" = "NO" and then want to switch back.
        
        resultsGoodM$methodologies <- length(rvToReturn$results$methodologies) > 0
        
        if (!resultsGoodM$methodologies) {
          outputMsgs$methodologies <- '*Required.'
        }
        
      })
      
      observeEvent(input$additionality, handlerExpr = {
        
        resultsGoodM$additionality <- FALSE
        rvToReturn$results$additionality <- input$additionality
        
        nChar <- nchar(input$additionality)
        outputMsgs$additionality <- sprintf('%d chars. remaining', 300 - nChar)
        
        lsPreset2$additionality <- input$additionality
        # Updating lsPreset2 here so that we can restore the user's response in case they 
        # accidentally switches to "Monitored" = "NO" and then want to switch back.
        
        resultsGoodM$additionality <- isValidInput.text(
          x = input$additionality, 
          bRequired = TRUE, 
          nCharMin = 10, 
          nCharMax = 300) 
        
      })
      
      observe({
        
        resultsGoodM$data_and_parameters <- FALSE
        outputMsgs$data_and_parameters <- ''
        
        if (length(modSrvrs$data_and_parameters) == 0) {
          return(invisible(0))
        }
        
        rvToReturn$results$data_and_parameters <- modSrvrs$data_and_parameters$items
        
        lsPreset2$data_and_parameters <- modSrvrs$data_and_parameters$items
        
        # Updating lsPreset2 here so that we can restore the user's response in case they 
        # accidentally switches to "Monitored" = "NO" and then want to switch back.
        
        resultsGoodM$data_and_parameters <- length(rvToReturn$results$data_and_parameters) > 0
        
        if (!resultsGoodM$data_and_parameters) {
          outputMsgs$data_and_parameters <- '*Required.'
        }
        
      })
      
      observe({
        
        resultsGoodM$crediting_period <- FALSE
        
        if (length(modSrvrs$crediting_period) == 0) {
          return(invisible(0))
        }
        
        rvToReturn$results$crediting_period <- modSrvrs$crediting_period$results
        
        lsPreset2$crediting_period <- modSrvrs$crediting_period$results
        # Updating lsPreset2 here so that we can restore the user's response in case they 
        # accidentally switches to "Monitored" = "NO" and then want to switch back.
        
        resultsGoodM$crediting_period <- modSrvrs$crediting_period$allResultsGood
        
      })
      
      observeEvent(input$crediting_period_type, handlerExpr = {
        
        resultsGoodM$crediting_period_type <- FALSE
        outputMsgs$crediting_period_type <- ''
        rvToReturn$results$crediting_period_type <- input$crediting_period_type
        
        lsPreset2$crediting_period_type <- input$crediting_period_type
        # Updating lsPreset2 here so that we can restore the user's response in case they 
        # accidentally switches to "Monitored" = "NO" and then want to switch back.
        
        validate(need(input$crediting_period_type, message = FALSE))
        if (length(input$crediting_period_type) == 0) { return(invisible(NULL)) }
        resultsGoodM$crediting_period_type <- TRUE
        
      })
      
      observe({
        
        resultsGoodM$monitoring_periods <- FALSE
        outputMsgs$monitoring_periods <- ''
        
        if (length(modSrvrs$monitoring_periods) == 0) {
          return(invisible(0))
        }
        
        rvToReturn$results$monitoring_periods <- modSrvrs$monitoring_periods$items
        
        lsPreset2$monitoring_periods <- modSrvrs$monitoring_periods$items
        # Updating lsPreset2 here so that we can restore the user's response in case they 
        # accidentally switches to "Monitored" = "NO" and then want to switch back.
        
        resultsGoodM$monitoring_periods <- length(rvToReturn$results$monitoring_periods) > 0
        
        if (!resultsGoodM$monitoring_periods) {
          outputMsgs$monitoring_periods <- '*Required.'
        }
        
      })

      observeEvent(input$state_baseline, handlerExpr = {
        
        resultsGoodM$state_baseline <- FALSE
        outputMsgs$state_baseline <- ''
        
        res <- validateNumeric(input$state_baseline)
        if (!res$is_valid) {
          outputMsgs$state_baseline <- res$msg
          return(NULL)
        }
        
        rvToReturn$results$state_baseline <- res$val
        
        lsPreset2$state_baseline <- res$val
        # Updating lsPreset2 here so that we can restore the user's response in case they 
        # accidentally switches to "Monitored" = "NO" and then want to switch back.
        
        resultsGoodM$state_baseline <- TRUE
        
      })
      
      observeEvent(input$state_project, handlerExpr = {
        
        resultsGoodM$state_project <- FALSE
        outputMsgs$state_project <- ''
        
        res <- validateNumeric(input$state_project)
        if (!res$is_valid) {
          outputMsgs$state_project <- res$msg
          return(NULL)
        }
        
        rvToReturn$results$state_project <- res$val
        
        lsPreset2$state_project <- res$val
        # Updating lsPreset2 here so that we can restore the user's response in case they 
        # accidentally switches to "Monitored" = "NO" and then want to switch back.
        
        resultsGoodM$state_project <- TRUE
        
      })
      
      observe({
        
        resultsGoodM$impact_estimation_ex_ante <- FALSE
        rvToReturn$results$impact_estimation_ex_ante <- NULL
    
        if (!resultsGoodM$state_baseline || !resultsGoodM$state_project) {
          return(invisible(0))
        }
        
        rvToReturn$results$impact_estimation_ex_ante <- 
          rvToReturn$results$state_baseline - rvToReturn$results$state_project
        
        resultsGoodM$impact_estimation_ex_ante <- TRUE
        
      })
      
      # outputs ----------------------------------------------------------------
      
      output$intentionality_msg <- renderText({
        outputMsgs$intentionality
      })
      
      output$beneficial_or_adverse_msg <- renderText({
        outputMsgs$beneficial_or_adverse
      })
      
      output$description_msg <- renderText({
        outputMsgs$description
      })
      
      output$monitored_msg <- renderText({
        outputMsgs$monitored
      })

      # FIXED 
      output$not_monitored_justification_msg <- renderText({
        outputMsgs$not_monitored_justification
      })
      
      # MONITORED
      output$indicator_msg <- renderText({
        outputMsgs$indicator
      })
      
      output$indicator_label_msg <- renderText({
        outputMsgs$indicator_label
      })
      
      output$indicator_unit_of_measure_msg <- renderText({
        outputMsgs$indicator_unit_of_measure
      })
      
      output$methodologies_msg <- renderText({
        outputMsgs$methodologies
      })
      
      output$additionality_msg <- renderText({
        outputMsgs$additionality
      })
      
      output$data_and_parameters_msg <- renderText({
        outputMsgs$data_and_parameters
      })
      
      output$crediting_period_msg <- renderText({
        outputMsgs$crediting_period
      })
      
      output$crediting_period_type_msg <- renderText({
        outputMsgs$crediting_period_type
      })
      
      output$monitoring_periods_msg <- renderText({
        outputMsgs$monitoring_periods
      })
      
      output$state_baseline_msg <- renderText({
        outputMsgs$state_baseline
      })
      
      output$state_project_msg <- renderText({
        outputMsgs$state_project
      })
      
      output$impact_estimation_ex_ante <- renderText({
        
        if (!resultsGoodM$impact_estimation_ex_ante) {
          return(NULL)
        }
        
        txt <- rvToReturn$results$impact_estimation_ex_ante
        if (resultsGoodM$indicator_unit_of_measure) {
          txt <- sprintf("%s %s", txt, rvToReturn$results$indicator_unit_of_measure)
        }
        
        return(txt)
      })

      output$toInvalidInput <- renderText({
        
        nms <- names(resultsGood)[!unlist(reactiveValuesToList(resultsGood))]
        
        if (resultsGood$monitored) {
          if (rvToReturn$results$monitored == "NO") {
            nms <- c(nms, names(resultsGoodF)[!unlist(reactiveValuesToList(resultsGoodF))])
          } else {
            nms <- c(nms, names(resultsGoodM)[!unlist(reactiveValuesToList(resultsGoodM))])
          }
        }
        
        if (length(nms) == 0) {
          return (NULL)
        }
        
        return(
          sprintf("Your inputs to the following fields are not valid: %s",
                  paste(nms, sep = "", collapse = ", ")))
        
      })
      
      # return logic -----------------------------------------------------------
      
      observe({
        
        if (!resultsGood$monitored) {
          rvToReturn$allResultsGood <- FALSE
          return(invisible(0))
        }
        
        if (rvToReturn$results$monitored == "NO") {
          rvToReturn$allResultsGood <- all(unlist(reactiveValuesToList(resultsGood))) & 
            all(unlist(reactiveValuesToList(resultsGoodF)))
          return(invisible(0))
        }
        
        if (rvToReturn$results$monitored == "YES") {
          rvToReturn$allResultsGood <- all(unlist(reactiveValuesToList(resultsGood))) & 
            all(unlist(reactiveValuesToList(resultsGoodM)))
          return(invisible(0))
        }
      })
      
      return(rvToReturn)
      
    })
}
