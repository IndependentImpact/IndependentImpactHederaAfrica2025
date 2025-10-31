
# Module for schema 'Data Lineage Report (DLR) [v1.0.0]'.

DLRschemaV2UI <- function(id,
                          lsPreset = NULL,
                          hL = 4,
                          colWidth = 12,
                          inpWidth = DEFAULT_INP_WIDTH,
                          idSchemaV) {

  ns <- NS(id)

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
    
    # GselectInput(id = ns("name_dataset"),
    #              title = "Dataset Name",
    #              choices = c(), # TODO.
    #              helpTxt = "Name of the dataset.",
    #              selected = lsPreset$name_dataset,
    #              multiple = FALSE,
    #              hL = hL,
    #              colWidth = colWidth,
    #              inpWidth = inpWidth),

    GtextInput(id = ns("name_dataset"),
               title = "Dataset Name",
               helpTxt = "Name of the dataset (3 chars. min., 40 chars. max.).",
               value = lsPreset$name_dataset,
               hL = hL,
               colWidth = colWidth,
               inpWidth = inpWidth),

    dateTimePeriodInput(
      id = ns('monitoring_period'),
      title = 'Monitoring Period',
      helpTxt = 'The monitoring period that this dataset belongs to:',
      lsPreset = lsPreset$monitoring_period,
      hL = hL,
      colWidth = colWidth,
      inpWidth = inpWidth),

    fluidRow(
      column(
        width = colWidth,
        wellPanel(
          h4("Aspects of Data Lineage Covered"),
          helpText("Which aspects of the dataset's lineage will be covered by this report?"),
          br(),
          checkboxGroupInput(inputId = ns("data_lineage_aspects"),
                             label = NULL,
                             choices = c("Transferral of raw data from source to IPFS" = "transferral",
                                         "Anonymisation of raw data" = "anonymisation",
                                         "Quality control and cleaning of the raw data" = "cleaning",
                                         "Transformation of the cleaned data into useful variables and formats" = "transformation"))))),

    wellPanel(
      h4("Generate Report"),
      helpText("Use the Jellyfish module below to generate a data lineage report for this dataset and monitoring period."),
      jellyfi3shR::jellyfi3shInput(id = ns("j3i_dlr"))),

    textOutput(outputId = ns("toInvalidInput")),

    br())

}

DLRschemaV2Server <- function(id,
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
          id_msg_pred = NULL,
          url_ipfs_pred = NULL,
          id_msg_lic = NULL,
          url_ipfs_lic = NULL,
          id_subject = idEntity,
          type_subject = "PROJECT"),
        name_dataset = NULL,
        monitoring_period = NULL,
        uri_ipfs_data_raw = NULL,
        uri_ipfs_rmd_transfer = NULL,
        uri_ipfs_report_transfer = NULL,
        uri_ipfs_rmd_cleaning = NULL,
        uri_ipfs_report_cleaning = NULL,
        uri_ipfs_rmd_transformation = NULL,
        uri_ipfs_report_transformation = NULL,
        uri_ipfs_data_final = NULL)

      resultsGood <- reactiveValues()
      resultsGood$id_msg_lic <- FALSE
      resultsGood$url_ipfs_lic <- FALSE
      resultsGood$id_msg_pred <- FALSE
      resultsGood$url_ipfs_pred <- FALSE
      resultsGood$name_dataset <- FALSE
      resultsGood$monitoring_period <- FALSE
      resultsGood$uri_ipfs_data_raw <- FALSE
      resultsGood$uri_ipfs_rmd_transfer <- FALSE
      resultsGood$uri_ipfs_report_transfer <- FALSE
      resultsGood$uri_ipfs_rmd_cleaning <- FALSE
      resultsGood$uri_ipfs_report_cleaning <- FALSE
      resultsGood$uri_ipfs_rmd_transformation <- FALSE
      resultsGood$uri_ipfs_report_transformation <- FALSE
      resultsGood$uri_ipfs_data_final <- FALSE

      outputMsgs <- reactiveValues()

      rvOther <- reactiveValues()

      # preset inputs ----------------------------------------------------------
      #None.

      # module servers ---------------------------------------------------------

      modSrvrs <- reactiveValues()

      modSrvrs$mProjDevLic <- chooseLicenseServer(
        id = "chLicPDL",
        dbCon = dbCon,
        scopes = "PROJECT_DEVELOPER", 
        statuses = "ACTIVE",
        idAgent = loginInfoUsr$id_agent,
        lsPreset = NULL) # TODO: implement lsPreset

      modSrvrs$monitoring_period <- dateTimePeriodServer(
        id = "monitoring_period",
        lsPreset = lsPreset$monitoring_period)

      modSrvrs$j3iDlr <- jellyfi3shR::jellyfi3shServer(
        id = "j3i_dlr")

      # inputs -----------------------------------------------------------------

      observe({

        resultsGood$id_msg_lic <- FALSE
        resultsGood$url_ipfs_lic <- FALSE
        rvToReturn$results$headers$id_msg_lic <- modSrvrs$mProjDevLic$results$id_msg
        rvToReturn$results$headers$url_ipfs_lic <- modSrvrs$mProjDevLic$results$url_ipfs
        resultsGood$id_msg_lic <- modSrvrs$mProjDevLic$allResultsGood
        resultsGood$url_ipfs_lic <- modSrvrs$mProjDevLic$allResultsGood

      })

      observeEvent(input$name_dataset, handlerExpr = {

        resultsGood$name_dataset <- FALSE
        rvToReturn$results$name_dataset <- input$name_dataset

        nChar <- nchar(input$name_dataset)
        outputMsgs$name_dataset <- sprintf("%d chars. remaining", 40 - nChar)

        resultsGood$name_dataset <- isValidInput.text(
          x = input$name_dataset,
          bRequired = TRUE,
          nCharMin = 3,
          nCharMax = 40)

      })

      observe({

        resultsGood$monitoring_period <- FALSE
        rvToReturn$results$monitoring_period <- modSrvrs$monitoring_period$results
        resultsGood$monitoring_period <- modSrvrs$monitoring_period$allResultsGood

      })

      observe({

        tryCatch({

        resultsGood$uri_ipfs_data_raw <- FALSE
        resultsGood$uri_ipfs_report_transfer <- FALSE
        resultsGood$uri_ipfs_rmd_transfer <- FALSE
        resultsGood$uri_ipfs_rmd_cleaning <- FALSE
        resultsGood$uri_ipfs_report_cleaning <- FALSE
        resultsGood$uri_ipfs_rmd_transformation <- FALSE
        resultsGood$uri_ipfs_report_transformation <- FALSE
        resultsGood$uri_ipfs_data_final <- FALSE


        validate(need(modSrvrs$j3iDlr, message = FALSE))
        if (!("done" %in% names(modSrvrs$j3iDlr))) { return(invisible(NULL)) }

        if (!modSrvrs$j3iDlr$done) { return(invisible(NULL)) }
        dataLineageCoverage <- input$data_lineage_aspects
        res <- reactiveValuesToList(modSrvrs$j3iDlr$results)
        # saveRDS(res, file = "tmp/res.Rda")
        # the raw data
        # The raw data could have been in aCar, eCar or not explicitly provided
        # (i.e., it was obtained by the function/RMD itself). We'll improve this
        # module soon, but in the meantime we don't want to do guesswork.
        rvToReturn$results$uri_ipfs_data_raw <- "Please refer to fCar."
        resultsGood$uri_ipfs_data_raw <- TRUE

        # transferral and anonymisation
        if ("transferral" %in% dataLineageCoverage) {
          if (!is.null(jellyfi3shR::emptyOrMissingAsNull(res$fCar))) {
            rvToReturn$results$uri_ipfs_rmd_transfer <- res$fCar
            resultsGood$uri_ipfs_rmd_transfer <- TRUE
          }
          if (!is.null(jellyfi3shR::emptyOrMissingAsNull(res$rCar))) {
            rvToReturn$results$uri_ipfs_report_transfer <- res$rCar
            resultsGood$uri_ipfs_report_transfer <- TRUE
          }
        } else {
          rvToReturn$results$uri_ipfs_rmd_transfer <- "Not covered by this report."
          resultsGood$uri_ipfs_rmd_transfer <- TRUE
          rvToReturn$results$uri_ipfs_report_transfer <- "Not covered by this report."
          resultsGood$uri_ipfs_report_transfer <- TRUE
        }

        # quality control and cleaning
        if ("cleaning" %in% dataLineageCoverage) {
          if (!is.null(jellyfi3shR::emptyOrMissingAsNull(res$fCar))) {
            rvToReturn$results$uri_ipfs_rmd_cleaning <- res$fCar
            resultsGood$uri_ipfs_rmd_cleaning <- TRUE
          }
          if (!is.null(jellyfi3shR::emptyOrMissingAsNull(res$rCar))) {
            rvToReturn$results$uri_ipfs_report_cleaning <- res$rCar
            resultsGood$uri_ipfs_report_cleaning <- TRUE
          }
        } else {
          rvToReturn$results$uri_ipfs_rmd_cleaning <- "Not covered by this report."
          resultsGood$uri_ipfs_rmd_cleaning <- TRUE
          rvToReturn$results$uri_ipfs_report_cleaning <- "Not covered by this report."
          resultsGood$uri_ipfs_report_cleaning <- TRUE
        }

        # transformation of the cleaned data
        if ("transformation" %in% dataLineageCoverage) {
          if (!is.null(jellyfi3shR::emptyOrMissingAsNull(res$fCar))) {
            rvToReturn$results$uri_ipfs_rmd_transformation <- res$fCar
            resultsGood$uri_ipfs_rmd_transformation <- TRUE
          }
          if (!is.null(jellyfi3shR::emptyOrMissingAsNull(res$rCar))) {
            rvToReturn$results$uri_ipfs_report_transformation <- res$rCar
            resultsGood$uri_ipfs_report_transformation <- TRUE
          }
        } else {
          rvToReturn$results$uri_ipfs_rmd_transformation <- "Not covered by this report."
          resultsGood$uri_ipfs_rmd_transformation <- TRUE
          rvToReturn$results$uri_ipfs_report_transformation <- "Not covered by this report."
          resultsGood$uri_ipfs_report_transformation <- TRUE
        }

        # the prepared data
        if (!is.null(jellyfi3shR::emptyOrMissingAsNull(res$rCar))) {
          if (res$outform %in% c("json", "rds")) {
            # This means we know the user supplied a function to fCar, so the
            # prepared data must be rCar.
            rvToReturn$results$uri_ipfs_data_final <- res$rCar
            resultsGood$uri_ipfs_data_final <- TRUE
          } else {
            # This means we know the user supplied an RMD to fCar, so the IPFS URI
            # of the prepared data will be inside the report pointed to by rCar.
            rvToReturn$results$uri_ipfs_data_final <- "See report."
            resultsGood$uri_ipfs_data_final <- TRUE
          }
        }

        }, error = function(e) {
          message("Error in DLRschemaV2Server observeEvent: ", e$message)
          alert(paste("Error in DLRschemaV2Server observeEvent: \n", e$message, p = ""))
        })

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

      output$name_dataset_msg <- renderText({
        outputMsgs$name_dataset
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
