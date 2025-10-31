
# Module for schema 'Verified Impact Certificate Issuance Request' (VICIR) [v1.0.0].

VICIRschemaUI <- function(id,
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

    GtextOutput(id = ns("toDRPRR"),
                title = "Approving DR-PRR of Project",
                hL = hL,
                colWidth = colWidth),

    fluidRow(
      column(
        width = colWidth,
        wellPanel(
          hx(x = 'Approved Monitoring Report (DR-MR)', lvl = hL),
          helpText("Select the project's approving 'Document Review: Monitoring Report' to be used for this specific issuance request."),
          selectInput(
            inputId = ns('siDRMR'),
            label = NULL,
            choices = c(),
            selected = NULL,
            multiple = FALSE,
            selectize = TRUE,
            width = inpWidth,
            size = NULL),
          textOutput(outputId = ns("siDRMR_msg"))))),

    wellPanel(
      hx(x = 'Impact Summary', lvl = hL),
      helpText("The impact, as per the verified monitoring report:"),
      impactSummaryInput(
        id = ns("summary_impact"),
        lsPreset = lsPreset$summary_impact,
        hL = hL,
        colWidth = colWidth,
        inpWidth = inpWidth)),

    GtextOutput(id = ns("id_acc_h"),
                title = "Hedera account to issue the certificate to:",
                hL = hL,
                colWidth = colWidth),

    textOutput(outputId = ns("toInvalidInput")),

    br())
}

VICIRschemaServer <- function(id,
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
          id_msg_pred = NULL,
          url_ipfs_pred = NULL,
          id_subject = idEntity,
          type_subject = "PROJECT"),
        id_msg_pred_drmr = NULL,
        url_ipfs_pred_drmr = NULL,
        summary_impact = lsPreset$summary_impact, # Doing this here because the summary_impact UI element should be read-only.
        id_acc_h = NULL)

      resultsGood <- reactiveValues()
      resultsGood$id_msg_lic <- FALSE
      resultsGood$url_ipfs_lic <- FALSE
      resultsGood$id_msg_pred <- FALSE
      resultsGood$url_ipfs_pred <- FALSE
      resultsGood$id_msg_pred_drmr <- FALSE
      resultsGood$url_ipfs_pred_drmr <- FALSE
      resultsGood$summary_impact <- FALSE
      resultsGood$id_acc_h <- FALSE

      outputMsgs <- reactiveValues()
      outputMsgs$siDRMR <- ""

      rvOther <- reactiveValues()
      rvOther$init <- NULL
      rvOther$dfDRMRs <- NULL

      # preset inputs ----------------------------------------------------------
      # None.

      # module servers ---------------------------------------------------------

      modSrvrs <- reactiveValues()
      modSrvrs$summary_impact <- NULL

      modSrvrs$mProjDevLic <- chooseLicenseServer(
        id = "chLicPDL",
        dbCon = dbCon,
        scopes = "PROJECT_DEVELOPER", 
        statuses = "ACTIVE",
        idAgent = loginInfoUsr$id_agent,
        lsPreset = NULL) # TODO: implement lsPreset
      
      # inputs -----------------------------------------------------------------

      # Initial setup (populate rvOther$dfDRMRs).
      observeEvent(rvOther$init, handlerExpr = {

        # Get the basic metadata of each mon cert, if any.
        {
          dfDRMRs <- getApprovingDRMRs(
            dbCon = dbCon,
            idProject = idEntity,
            all = TRUE)
          
          if (nrow(dfDRMRs) == 0) {
            outputMsgs$siDRMR <- "You do not seem to have any verified monitoring reports for this project yet. Use the 'Independent Impact - Monitoring Verification Workflow' to obtain one, or wait for your submitted reports to be reviewed."
            return(NULL)
          }
         
          q <- sprintf(
            "SELECT id, id_message_h, uri_ipfs FROM tbl_document_metadata WHERE id_message_h IN(%s);",
            paste(sprintf("'%s'", dfDRMRs$id_msg_pred), sep = "", collapse = ","))
          dfMRs <- dbGetQuery(conn = dbCon, statement = q)
           
        }
        

        warning("TODO: Make this much more efficient. Try to make use of identifying_content instead of fetching the docs from IPFS.")
        
        
        # For each MR, get the dates of the monitoring period as well as the details about the impact.
        {
          # Retrieve the approved MRs (the MRs themselves, not the DRMRs).
          lsDocs <- lapply(X = 1:nrow(dfMRs), FUN = function(r) {

            # Get the IPFS URL in the correct format.
            urlIpfs <- ipfsUriToUrl(dfMRs$uri_ipfs[r])

            # Retrieve the document from IPFS.
            {
              doc <- httr::GET(url = urlIpfs)
              if (doc$status_code != 200) {
                stCode <- doc$status_code
                msg <- content(x = doc, as = "parsed")
                message(sprintf("WARNING: Failed to retrieve document from IPFS. Error: %s (code %s)",
                                msg, stCode))
                return(NULL)
              }
              doc <- content(x = doc, as = "parsed")
            }

            # Subset to what is of interest.
            {
              doc <- doc$document

              doc <- doc[intersect(
                names(doc),
                c("indicator_label",
                  "monitoring_period",
                  "value_impact",
                  "unit_impact"))]
              doc$monitoring_period$type <- NULL
              doc$monitoring_period$`@context` <- NULL

              doc <- data.frame(doc)
              doc$id_message_h <- dfMRs$id_message_h[r]
            }

            # Done.
            return(doc)

          })
          dfDets <- do.call("rbind.fill", lsDocs); rm(lsDocs)

          # Merge with dfDRMRs.
          dfDRMRs <- merge.data.frame(
            x = dfDRMRs,
            y = dfDets,
            by.x = "id_msg_pred", 
            by.y = "id_message_h", 
            all.x = TRUE); rm(dfDets)
        }

        dfDRMRs <- gdata::rename.vars(
          data = dfDRMRs,
          from = c("monitoring_period.datetime_start", "monitoring_period.datetime_end"),
          to = c("date_start_monper", "date_end_monper"),
          info = FALSE)

        # Ignore ones for which we could not retrieve the details.
        dfDRMRs <- dfDRMRs[which(!is.na(dfDRMRs$date_start_monper)),]
        if (nrow(dfDRMRs) == 0) {
          outputMsgs$siDRMR <- "You do not seem to have any verified monitoring reports for this project yet. Use the 'Independent Impact - Monitoring Verification Workflow'  to obtain one, or wait for your submitted reports to be reviewed."
          return(NULL)
        }

        # Convert dates to more human-friendly formats.
        for (vnm in c("date_start_monper", "date_end_monper")) {
          for (r in 1:nrow(dfDRMRs)) {
            dt <- strptime(x = dfDRMRs[[vnm]][r],
                           format = "%Y-%m-%dT%H:%M:%S.000Z",
                           tz = "UTC")
            if (is.na(dt)) {
              dt <- strptime(x = dfDRMRs[[vnm]][r],
                             format = "%Y-%m-%d",
                             tz = "UTC")
            }
            if (is.na(dt)) {
              warning(sprintf("Failed to parse date (%s).", dfDRMRs[[vnm]][r]))
            } else {
              dfDRMRs[[vnm]][r] <- as.character(dt)
            }
          }
        }

        # Order in reversed chronological order.
        dfDRMRs <- dfDRMRs[order(dfDRMRs$date_created, decreasing = TRUE),]
        rownames(dfDRMRs) <- 1:nrow(dfDRMRs)

        # Done.
        rvOther$dfDRMRs <- dfDRMRs
      })

      # Populate choices for siDRMR.
      observeEvent(rvOther$dfDRMRs, handlerExpr = {

        validate(need(rvOther$dfDRMRs, message = FALSE))

        chcs <- rvOther$dfDRMRs$id_message_h
        names(chcs) <- sprintf("%d (%s; %s - %s)",
                               1:length(chcs),
                               rvOther$dfDRMRs$indicator_label,
                               rvOther$dfDRMRs$date_start_monper,
                               rvOther$dfDRMRs$date_end_monper)

        updateSelectInput(
          session = session,
          inputId = "siDRMR",
          choices = chcs,
          selected = chcs[1])
      })

      # React to user selection of mon cert.
      observeEvent(input$siDRMR, handlerExpr = {

        resultsGood$id_msg_pred_drmr <- FALSE
        resultsGood$url_ipfs_pred_drmr <- FALSE
        resultsGood$summary_impact <- FALSE

        validate(need(input$siDRMR, message = FALSE))

        idx <- which(rvOther$dfDRMRs$id_message_h == input$siDRMR)
        rvToReturn$results$id_msg_pred_drmr <- rvOther$dfDRMRs$id_message_h[idx]
        rvToReturn$results$url_ipfs_pred_drmr <- rvOther$dfDRMRs$uri_ipfs[idx]

        rvToReturn$results$summary_impact <- list(
          description_impact = rvOther$dfDRMRs$indicator_label[idx],
          period_impact = list(
            datetime_start = rvOther$dfDRMRs$date_start_monper[idx],
            datetime_end = rvOther$dfDRMRs$date_end_monper[idx]),
          unit_impact = rvOther$dfDRMRs$unit_impact[idx],
          extent_impact = rvOther$dfDRMRs$value_impact[idx])

        # Doing this a little bit differently because the summary_impact UI element should be read-only,
        # with rvToReturn$results$summary_impact populated by us based on the DR-MR selected by the user.
        modSrvrs$summary_impact <- impactSummaryServer(
          id = "summary_impact",
          lsPreset = rvToReturn$results$summary_impact)

        resultsGood$id_msg_pred_drmr <- TRUE
        resultsGood$url_ipfs_pred_drmr <- TRUE
      })

      observe({
        resultsGood$summary_impact <- modSrvrs$summary_impact$allResultsGood
      })

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
      
      # Populate rvToReturn$results$id_acc_h.
      observe({

        accId <- dbFetch(
          dbSendQuery(
            conn = dbCon,
            statement = sprintf(
              "SELECT id_acc_h FROM tbl_link_agents_x_hedera_accounts WHERE id_agent = '%s';",
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

      output$siDRMR_msg <- renderText({
        outputMsgs$siDRMR
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

      rvOther$init <- Sys.time()

      return(rvToReturn)

    })
}
