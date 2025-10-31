
# Module for schema 'Monitoring Report (MR) [v4.0.0]'.

MRschemaV2UI <- function(id,
                         lsPreset = NULL,
                         hL = 4,
                         colWidth = 12,
                         inpWidth = DEFAULT_INP_WIDTH,
                         idSchemaV = NULL) {

  ns <- NS(id)

  if (length(lsPreset) == 0) {
    # Quick hack to change the default values of the date-time pickers. We have
    # to change this to an unlikely value to force the user to change both,
    # because if they just keep the default values and never change it, the
    # values won't be updated in rvToReturn$results.
    lsPreset <- list()
    lsPreset$date_start_monper <- lubridate::now(tzone = "UTC") - lubridate::hours(2)
    lsPreset$date_end_monper <- lubridate::now(tzone = "UTC") - lubridate::hours(1)
  }

  tagList(

    ##useShinyjs(),

    GtextOutput(id = ns("id_project"),
                title = "Project ID",
                hL = hL,
                colWidth = colWidth),

    GtextOutput(id = ns("toDRPRR"),
                title = "Approving DR-PRR of Project",
                hL = hL,
                colWidth = colWidth),

    chooseLicenseInput(
      id = ns('chLicPDL'),
      title = "Project Developer License",
      helpTxt = "Select your Independent Impact Project Developer License (II-L-PD) to use for this submission."),
    
    GtextInput( # TODO. Make this a selectInput once we can retrieve the approved indicators from the db.
      id = ns("indicator_label"),
      title = "Indicator",
      helpTxt = 'The label of the impact indicator in question (3 chars. min.):',
      value = lsPreset$indicator_label,
      hL = hL,
      colWidth = colWidth,
      inpWidth = inpWidth,
      wellP = TRUE),

    dateTimePeriodInput( # TODO, Make this a selectInput once we can retrieve the approved monitoring periods per indicator from the db.
      id = ns("monitoring_period"),
      title = "Monitoring Period",
      helpTxt = "Provide the start and end dates and times that demarcate the monitoring period in question:",
      lsPreset = lsPreset$monitoring_period,
      hL = hL,
      colWidth = colWidth,
      inpWidth = inpWidth),

    wellPanel(
      h4("Datasets"),
      helpText("The datasets used in the calculation of the project's impact during this monitoring period and in terms of this indicator:"),
      dynamicMultiV2Input(id = ns('datasets'),
                          #useModal = TRUE,
                          useModal = FALSE,
                          # nmUImod = 'datasetInput',
                          # lsArgsModUI = list(hL = 5, colWidth = 12, inpWidth = DEFAULT_INP_WIDTH)
                          ),
      textOutput(outputId = ns("datasets_msg"))),

    wellPanel(
      h4("Generate Report"),
      helpText("Use the Jellyfish module below to generate a monitoring report for the monitoring period in question. IMPORTANT: The file you upload to the 'Code' input must be a .Rmd file."),
      jellyfi3shR::jellyfi3shInput(id = ns("jfiMR"))),

    GtextOutput(id = ns("ipfs_uri_rmd_calculation"),
                title = "Calculation RMD IPFS URI",
                subTxt = "IPFS URI of the RMD that performed the impact calculation:",
                hL = hL,
                colWidth = colWidth),

    GtextOutput(id = ns("ipfs_uri_impact"),
                title = "Impact Answer IPFS URI",
                subTxt = "IPFS URI of the result of the impact calculation:",
                hL = hL,
                colWidth = colWidth),

    GtextOutput(id = ns("value_impact"),
                title = "Impact Answer",
                subTxt = "Numeric value of the calculated impact:",
                hL = hL,
                colWidth = colWidth),

    GtextOutput(id = ns("unit_impact"),
                title = "Unit of Measure",
                subTxt = "The unit of measure for the calculated impact:",
                hL = hL,
                colWidth = colWidth),

    GtextOutput(id = ns("ipfs_uri_report_calculation"),
                title = "Calculation Report IPFS URI",
                subTxt = "IPFS URI of the report produced by the RMD that performed the impact calculation:",
                hL = hL,
                colWidth = colWidth),

    GtextOutput(id = ns("id_acc_h"),
                title = "Hedera Account ID to issue II-VCERT-MV to",
                hL = hL,
                colWidth = colWidth),

    textOutput(outputId = ns("toInvalidInput")),

    br())
}

MRschemaV2Server <- function(id,
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

      idAccH <- dbFetch(
        dbSendQuery(
          conn = dbCon,
          statement = sprintf("SELECT id_acc_h FROM tbl_link_agents_x_hedera_accounts WHERE id_agent = '%s';",
                              loginInfoUsr$id_agent)))[["id_acc_h"]]

      # TODO. The structure of $results below allows for only one impact per 
      # monitoring report. Is that what we want? Reconsider this.
      
      rvToReturn <- reactiveValues()
      rvToReturn$allResultsGood <- FALSE
      rvToReturn$results <- reactiveValues(
        headers = list(
          id_msg_pred = NULL,
          url_ipfs_pred = NULL,
          id_msg_lic = NULL,
          url_ipfs_lic = NULL,
          id_subject = idEntity,
          type_subject = "PROJECT"),
        indicator_label = NULL,
        monitoring_period = NULL,
        datasets = NULL,
        ipfs_uri_rmd_calculation = lsPreset$ipfs_uri_rmd_calculation,
        ipfs_uri_impact = lsPreset$ipfs_uri_impact,
        ipfs_uri_report_calculation = lsPreset$ipfs_uri_report_calculation,
        value_impact = lsPreset$value_impact,
        unit_impact = lsPreset$unit_impact,
        # id_msg_pred_drdlr = NULL,
        # url_ipfs_pred_drdlr = NULL,
        id_acc_h = idAccH)

      resultsGood <- reactiveValues()
      resultsGood$id_msg_lic <- FALSE
      resultsGood$url_ipfs_lic <- FALSE
      resultsGood$id_msg_pred <- FALSE
      resultsGood$url_ipfs_pred <- FALSE
      resultsGood$indicator_label <- FALSE
      resultsGood$monitoring_period <- FALSE
      resultsGood$datasets <- FALSE
      resultsGood$ipfs_uri_rmd_calculation <- FALSE
      resultsGood$ipfs_uri_impact <- FALSE
      resultsGood$value_impact <- FALSE
      resultsGood$unit_impact <- FALSE
      resultsGood$ipfs_uri_report_calculation <- FALSE
      resultsGood$id_acc_h <- isValidInput.text(x = idAccH, bRequired = TRUE, nCharMin = 5)

      outputMsgs <- reactiveValues()
      outputMsgs$indicator_label <- ''
      outputMsgs$datasets <- ''

      rvOther <- reactiveValues()
      rvOther$dfDRDLRs <- NULL

      # preset inputs ----------------------------------------------------------

      if ("indicator_label" %in% names(lsPreset)) {
        updateTextInput(session = session,
                        inputId = 'indicator_label',
                        value = lsPreset$indicator_label)
      }

      # if ('ipfs_uri_rmd_calculation' %in% names(lsPreset)) {
      #   updateTextInput(session = session,
      #                   inputId = 'ipfs_uri_rmd_calculation',
      #                   value = lsPreset$ipfs_uri_rmd_calculation)
      # }

      # if ('ipfs_uri_impact' %in% names(lsPreset)) {
      #   updateTextInput(session = session,
      #                   inputId = 'ipfs_uri_impact',
      #                   value = lsPreset$ipfs_uri_impact)
      # }

      # if ('value_impact' %in% names(lsPreset)) {
      #   updateNumericInput(session = session,
      #                      inputId = 'value_impact',
      #                      value = lsPreset$value_impact)
      # }

      # if ('ipfs_uri_report_calculation' %in% names(lsPreset)) {
      #   updateTextInput(session = session,
      #                   inputId = 'ipfs_uri_report_calculation',
      #                   value = lsPreset$ipfs_uri_report_calculation)
      # }

      # module servers ---------------------------------------------------------

      modSrvrs <- reactiveValues()

      modSrvrs$monitoring_period <- dateTimePeriodServer(
        id = "monitoring_period",
        lsPreset = lsPreset$monitoring_period)

      modSrvrs$datasets <- dynamicMultiV2Server(
        id = "datasets",
        nmSrvrMod = "datasetServer",
        lsPreset = lsPreset$datasets,
        lsArgsModSrvr = list(dbCon = dbCon, idEntity = idEntity),
        #useModal = TRUE
        useModal = FALSE,
        nmUImod = 'datasetInput',
        lsArgsModUI = list(hL = 5, colWidth = 12, inpWidth = DEFAULT_INP_WIDTH))

      modSrvrs$mProjDevLic <- chooseLicenseServer(
        id = "chLicPDL",
        dbCon = dbCon,
        scopes = "PROJECT_DEVELOPER", 
        statuses = "ACTIVE",
        idAgent = loginInfoUsr$id_agent,
        lsPreset = NULL) # TODO: implement lsPreset

      modSrvrs$jfiMR <- jellyfi3shServer(
        id = "jfiMR", 
        exeLoc = TRUE)

      # inputs -----------------------------------------------------------------

      # modSrvrs$mProjDevLic
      observe({

        resultsGood$id_msg_lic <- FALSE
        resultsGood$url_ipfs_lic <- FALSE
        rvToReturn$results$headers$id_msg_lic <- modSrvrs$mProjDevLic$results$id_msg
        rvToReturn$results$headers$url_ipfs_lic <- modSrvrs$mProjDevLic$results$url_ipfs
        resultsGood$id_msg_lic <- modSrvrs$mProjDevLic$allResultsGood
        resultsGood$url_ipfs_lic <- modSrvrs$mProjDevLic$allResultsGood

      })

      observeEvent(input$indicator_label, handlerExpr = {

        resultsGood$indicator_label <- FALSE
        rvToReturn$results$indicator_label <- input$indicator_label

        nChar <- nchar(input$indicator_label)
        outputMsgs$indicator_label <- sprintf('%d chars. remaining', 20 - nChar)

        resultsGood$indicator_label <- isValidInput.text(
          x = input$indicator_label,
          bRequired = TRUE,
          nCharMin = 3,
          nCharMax = 20)

      })

      # modSrvrs$monitoring_period
      observe({

        resultsGood$monitoring_period <- FALSE
        rvToReturn$results$monitoring_period <- modSrvrs$monitoring_period$results
        resultsGood$monitoring_period <- modSrvrs$monitoring_period$allResultsGood

      })

      # modSrvrs$datasets
      observe({

        resultsGood$datasets <- FALSE
        rvToReturn$results$datasets <- modSrvrs$datasets$items
        resultsGood$datasets <- length(modSrvrs$datasets$items) > 0

      })

      # modSrvrs$jfiMR
      observe({

        resultsGood$ipfs_uri_rmd_calculation <- FALSE
        resultsGood$ipfs_uri_impact <- FALSE
        resultsGood$value_impact <- FALSE
        resultsGood$unit_impact <- FALSE
        resultsGood$ipfs_uri_report_calculation <- FALSE

        validate(need(modSrvrs$jfiMR, message = FALSE))
        validate(need(modSrvrs$jfiMR$done, message = FALSE))
        validate(need(modSrvrs$jfiMR$results, message = FALSE))
        if (!modSrvrs$jfiMR$done) { return(invisible(NULL)) }

        res <- recursiveReactiveValuesToList(modSrvrs$jfiMR$results)
        saveRDS(res, file = "~/MRschemaV2_results.rds") # For debugging purposes.
        # ipfs_uri_rmd_calculation
        rvToReturn$results$ipfs_uri_rmd_calculation <- res$fCar
        resultsGood$ipfs_uri_rmd_calculation <- TRUE

        # ipfs_uri_report_calculation
        if ("rtCar" %in% names(res)) { # This should always be TRUE, because users are supposed to always
          # only upload RMDs to this module in this context.
          if (length(res$rtCar) == 1) {
            rvToReturn$results$ipfs_uri_report_calculation <- res$rtCar
            resultsGood$ipfs_uri_report_calculation <- TRUE
          }
        }

        # ipfs_uri_impact
        if (length(res$rCar) == 1) {
          rvToReturn$results$ipfs_uri_impact <- res$rCar
          resultsGood$ipfs_uri_impact <- TRUE
        }

        # Fetch rCar from IPFS so that we can extract impact and report details.
        tryCatch({

          # 'rCar' is JSON, so using simple httr here.
          resX <- httr::GET(url = res$rCar)
          resX <- httr::content(resX, as = "parsed")

          # value
          rvToReturn$results$value_impact <- resX$value[[1]]
          resultsGood$value_impact <- TRUE

          # unit
          rvToReturn$results$unit_impact <- resX$unit[[1]]
          resultsGood$unit_impact <- TRUE

        }, error = function(e) {
          warning(as.character(e))
        })

        if (!resultsGood$value_impact | !resultsGood$unit_impact) {
          showModal(
            modalDialog(
              title = "Error",
              "Failed to obtain calculation results.",
              footer = NULL,
              size = "s",
              easyClose = TRUE))
          return(invisible(NULL))
        }

      })

      # outputs ----------------------------------------------------------------

      output$id_project <- renderText({
        rvToReturn$results$headers$id_subject
      })

      output$toDRPRR <- renderText({

        noDocMsg <- "You do not seem to have an approved Project Registration Request (PRR) for this project yet. Please use the 'Independent Impact - Main Workflow' to apply for project registration."

        dfDocMd <- getApprovingDRPRRs(
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

      output$indicator_label_msg <- renderText({
        outputMsgs$indicator_label
      })

      output$ipfs_uri_rmd_calculation <- renderText({
        rvToReturn$results$ipfs_uri_rmd_calculation
      })

      output$ipfs_uri_impact <- renderText({
        rvToReturn$results$ipfs_uri_impact
      })

      output$value_impact <- renderText({
        rvToReturn$results$value_impact
      })

      output$unit_impact <- renderText({
        rvToReturn$results$unit_impact
      })

      output$ipfs_uri_report_calculation <- renderText({
        rvToReturn$results$ipfs_uri_report_calculation
      })

      output$id_acc_h <- renderText({
        rvToReturn$results$id_acc_h
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
