
ADschemaV2UI <- function(id, 
                         lsPreset = NULL, 
                         hL = 4, 
                         colWidth = 12, 
                         inpWidth = DEFAULT_INP_WIDTH) {
  
  ns <- NS(id)
  
  tagList(
    
    ##useShinyjs(),
    
    GtextInput(id = ns('initials'),
               title = 'Initials',
               helpTxt = 'What are your initials?', 
               value = lsPreset$initials,
               hL = hL,
               colWidth = colWidth,
               inpWidth = inpWidth),
    
    GtextInput(id = ns('name_first'),
               title = 'First Name',
               helpTxt = 'What is your first name?', 
               value = lsPreset$name_first,
               hL = hL,
               colWidth = colWidth,
               inpWidth = inpWidth),
    
    GtextInput(id = ns('name_last'),
               title = 'Last Name',
               helpTxt = 'What is your last name?', 
               value = lsPreset$name_last,
               hL = hL,
               colWidth = colWidth,
               inpWidth = inpWidth),
    
    fluidRow(
      column(
        width = colWidth,
        wellPanel(
          h4("Date of Birth"),
          helpText("What is your date of birth?"),
          dateInput(inputId = ns('date_of_birth'), 
                    label = NULL, 
                    min = "1900-01-01", 
                    max = Sys.Date() - lubridate::years(16), 
                    width = inpWidth, 
                    value = Sys.Date() - lubridate::years(16)),
          textOutput(outputId = ns('date_of_birth_msg'))))),
    
    GtextInput(id = ns('country'),
               title = 'Country',
               helpTxt = 'Which country is your home country?', 
               value = lsPreset$country,
               hL = hL,
               colWidth = colWidth,
               inpWidth = inpWidth),
    
    GfileInput(id = ns("supporting_evidence_pp1"), 
               title = "Supporting Evidence - PP 1", 
               helpTxt = "(Optional, but highly recommended) Upload evidence of your personal particulars. For example, upload a photo of your passport or driver's license next to your face. Make sure the details on the passport/license are clear and legible. File formats accepted: .png, .PNG, .jpeg and .pdf.", 
               hL = hL, 
               colWidth = colWidth, 
               inpWidth = inpWidth, 
               buttonLabel = "Browse", 
               accept = c(".png", ".PNG", ".jpeg", ".pdf"), 
               multiple = FALSE),
    
    GfileInput(id = ns("supporting_evidence_pp2"), 
               title = "Supporting Evidence - PP 2", 
               helpTxt = "(Optional) Upload additional evidence of your personal particulars. For example, upload a photo of your passport or driver's license next to your face. Make sure the details on the passport/license are clear and legible. File formats accepted: .png, .PNG, .jpeg and .pdf.", 
               hL = hL, 
               colWidth = colWidth, 
               inpWidth = inpWidth, 
               buttonLabel = "Browse", 
               accept = c(".png", ".PNG", ".jpeg", ".pdf"), 
               multiple = FALSE),
    
    GtextOutput(id = ns("email_address"), 
                title = "Email Address", 
                hL = hL, 
                colWidth = colWidth),
    
    # emailInputUI(id = ns("email_address"),
    #              hL = hL,
    #              presetVal = lsPreset$email_address,
    #              colWidth = colWidth,
    #              inpWidth = inpWidth),
    
    GtextInput(id = ns('contact_number'),
               title = 'Contact Number',
               helpTxt = 'What is your contact number? Please provide the country code (e.g., +27) too.',
               value = lsPreset$contact_number,
               hL = hL,
               colWidth = colWidth,
               inpWidth = inpWidth),
    
    GfileInput(id = ns("supporting_evidence_contact_no"), 
               title = "Supporting Evidence - Contact Number", 
               helpTxt = "(Optional, but highly recommended) Upload proof of ownership of your contact number. For example, upload an account statement that connects your name and surname to the contact number. File formats accepted: .png, .PNG, .jpeg and .pdf.", 
               hL = hL, 
               colWidth = colWidth, 
               inpWidth = inpWidth, 
               buttonLabel = "Browse", 
               placeholder = lsPreset$supporting_evidence_contact_no,
               accept = c(".png", ".PNG", ".jpeg", ".pdf"), 
               multiple = FALSE),
    
    textOutput(outputId = ns("toInvalidInput")),
    
    br())
  
}



ADschemaV2Server <- function(id,  
                             dbCon,
                             loginInfoUsr,
                             idEntity = NULL,
                             idWorkflow = NULL,
                             idSchema = NULL,
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
          id_subject = loginInfoUsr$id_agent,
          type_subject = "AGENT"),
        id_acc_h = NULL, 
        initials = NULL,
        name_first = NULL,
        name_last = NULL,
        date_of_birth = NULL,
        country = NULL,
        supporting_evidence_pp1 = lsPreset$supporting_evidence_pp1, 
        supporting_evidence_pp2 = lsPreset$supporting_evidence_pp2, 
        email_address = loginInfoUsr$email_address,
        supporting_evidence_email = "Verified upon sign-up.",
        contact_number = NULL,
        supporting_evidence_contact_no = lsPreset$supporting_evidence_contact_no)
      
      resultsGood <- reactiveValues()
      resultsGood$id_acc_h <- FALSE 
      resultsGood$initials <- FALSE
      resultsGood$name_first <- FALSE
      resultsGood$name_last <- FALSE
      resultsGood$date_of_birth <- FALSE
      resultsGood$country <- FALSE
      #resultsGood$email_address <- FALSE 
      resultsGood$contact_number <- FALSE
      
      # resultsGood$supporting_evidence_pp1 <- FALSE # TODO.
      # resultsGood$supporting_evidence_pp2 <- FALSE # TODO.
      # resultsGood$supporting_evidence_contact_no <- FALSE # TODO.
      # resultsGood$supporting_evidence_email <- FALSE
      
      
      outputMsgs <- reactiveValues()
      outputMsgs$initials <- ''
      outputMsgs$name_first <- ''
      outputMsgs$name_last <- ''
      outputMsgs$date_of_birth <- ''
      outputMsgs$country <- ''
      outputMsgs$contact_number <- ''
      outputMsgs$supporting_evidence_pp1 <- ''
      outputMsgs$supporting_evidence_pp2 <- ''
      outputMsgs$supporting_evidence_contact_no <- ''
      
      rvOther <- reactiveValues()
      
      # preset inputs/results---------------------------------------------------
      
      # id_acc_h
      observe({
        q <- sprintf("")
        agentHaccId <- dbFetch(
          dbSendQuery(
            conn = dbCon, 
            statement = sprintf("SELECT id_acc_h FROM tbl_link_agents_x_hedera_accounts WHERE id_agent = '%s';",
                                loginInfoUsr$id_agent)))
        rvToReturn$results$id_acc_h <- agentHaccId$id_acc_h
        resultsGood$id_acc_h <- TRUE
      })
      
      if ('initials' %in% names(lsPreset)) {
        updateTextInput(session = session, 
                        inputId = 'initials', 
                        value = lsPreset$initials)
      }
      
      if ('name_first' %in% names(lsPreset)) {
        updateTextInput(session = session, 
                        inputId = 'name_first', 
                        value = lsPreset$name_first)
      }
      
      if ('name_last' %in% names(lsPreset)) {
        updateTextInput(session = session, 
                        inputId = 'name_last', 
                        value = lsPreset$name_last)
      }
      
      if ('contact_number' %in% names(lsPreset)) {
        updateTextInput(session = session, 
                        inputId = 'contact_number', 
                        value = lsPreset$contact_number)
      }
      
      if ('date_of_birth' %in% names(lsPreset)) {
        updateDateInput(session = session, 
                        inputId = 'date_of_birth', 
                        value = lsPreset$date_of_birth)
      }
      
      if ('country' %in% names(lsPreset)) {
        updateTextInput(session = session, 
                        inputId = 'country', 
                        value = lsPreset$country)
      }
      
      # TODO: Find a way to pass lsPreset$supporting_evidence_pp1,
      # lsPreset$supporting_evidence_pp2 and
      # lsPreset$supporting_evidence_contact_no 
      # to their respective GfileInputs.
      
      # module servers ---------------------------------------------------------
      
      modSrvrs <- reactiveValues()
      
      # modSrvrs$email_address <- emailInputServer(
      #   id = "email_address",
      #   presetVal = lsPreset$email_address)
      
      # inputs -----------------------------------------------------------------
      
      observeEvent(input$initials, handlerExpr = {
        
        resultsGood$initials <- FALSE
        rvToReturn$results$initials <- input$initials
        
        nChar <- nchar(input$initials)
        outputMsgs$initials <- sprintf('%d chars. remaining', 10 - nChar)
        
        resultsGood$initials <- isValidInput.text(
          x = input$initials, 
          bRequired = TRUE, 
          nCharMin = 1, 
          nCharMax = 10) 
        
      })
      
      observeEvent(input$name_first, handlerExpr = {
        
        resultsGood$name_first <- FALSE
        rvToReturn$results$name_first <- input$name_first
        
        nChar <- nchar(input$name_first)
        outputMsgs$name_first <- sprintf('%d chars. remaining', 30 - nChar)
        
        resultsGood$name_first <- isValidInput.text(
          x = input$name_first, 
          bRequired = FALSE, 
          nCharMin = 2, 
          nCharMax = 30) 
        
      })
      
      observeEvent(input$name_last, handlerExpr = {
        
        resultsGood$name_last <- FALSE
        rvToReturn$results$name_last <- input$name_last
        
        nChar <- nchar(input$name_last)
        outputMsgs$name_last <- sprintf('%d chars. remaining', 30 - nChar)
        
        resultsGood$name_last <- isValidInput.text(
          x = input$name_last, 
          bRequired = TRUE, 
          nCharMin = 2, 
          nCharMax = 30) 
        
      })
      
      observeEvent(input$date_of_birth, handlerExpr = {
        
        resultsGood$date_of_birth <- FALSE
        rvToReturn$results$date_of_birth <- input$date_of_birth
        resultsGood$date_of_birth <- TRUE
        
      })
      
      observeEvent(input$country, handlerExpr = {
        
        resultsGood$country <- FALSE
        rvToReturn$results$country <- input$country
        
        nChar <- nchar(input$country)
        outputMsgs$country <- sprintf('%d chars. remaining', 60 - nChar)
        
        resultsGood$country <- isValidInput.text(
          x = input$country, 
          bRequired = TRUE, 
          nCharMin = 2, 
          nCharMax = 60) 
        
      })
      
      observeEvent(input$supporting_evidence_pp1, handlerExpr = {
        
        rvToReturn$results$supporting_evidence_pp1 <- NULL
        outputMsgs$supporting_evidence_pp1 <- ""
        
        validate(need(input$supporting_evidence_pp1, message = FALSE))
        
        showModal(
          modalDialog(
            title = "Uploading file...", 
            "Please be patient.", 
            footer = NULL, 
            size = "s", 
            easyClose = FALSE))
        
        df <- input$supporting_evidence_pp1
        bErr <- TRUE
        tryCatch({
          ipfsURI <- uploadToIpfs(
            df = df, 
            encrypt = TRUE, 
            wrap = FALSE, 
            cidOnly = TRUE)
          rvToReturn$results$supporting_evidence_pp1 <- ipfsURI
          bErr <- FALSE
        }, error = function(e) { 
          cat(sprintf("%s [ERROR]: %s\n", Sys.time(), e), 
              file = sprintf("%slog.txt", logdir), 
              append = TRUE)
        })
        
        removeModal()
        
        if (bErr) {
          showModal(
            modalDialog(
              title = "Upload failed.",
              sprintf("Failed to upload the file (%s) to IPFS.",
                      df$name),
              footer = NULL,
              size = "s",
              easyClose = TRUE))
          return(NULL)
        }
        
      })
      
      observeEvent(input$supporting_evidence_pp2, handlerExpr = {
        
        rvToReturn$results$supporting_evidence_pp2 <- NULL
        outputMsgs$supporting_evidence_pp2 <- ""
        
        validate(need(input$supporting_evidence_pp2, message = FALSE))
        
        showModal(
          modalDialog(
            title = "Uploading file...", 
            "Please be patient.", 
            footer = NULL, 
            size = "s", 
            easyClose = FALSE))
        
        df <- input$supporting_evidence_pp2
        bErr <- TRUE
        tryCatch({
          ipfsURI <- uploadToIpfs(
            df = df, 
            encrypt = TRUE, 
            wrap = FALSE,
            cidOnly = TRUE)
          rvToReturn$results$supporting_evidence_pp2 <- ipfsURI
          bErr <- FALSE
        }, error = function(e) { 
          cat(sprintf("%s [ERROR]: %s\n", Sys.time(), e), 
              file = sprintf("%slog.txt", logdir), 
              append = TRUE)
        })
        
        removeModal()
        
        if (bErr) {
          showModal(
            modalDialog(
              title = "Upload failed.",
              sprintf("Failed to upload the file (%s) to IPFS.",
                      df$name),
              footer = NULL,
              size = "s",
              easyClose = TRUE))
          return(NULL)
        }
        
      })
      
      # observe({
      #   resultsGood$email_address <- modSrvrs$email_address$allResultsGood
      # 
      #   rvToReturn$results$email_address <-
      #     modSrvrs$email_address$results$email_address
      # 
      #   rvToReturn$results$supporting_evidence_email <-
      #     modSrvrs$email_address$results$verification_res
      # })
      
      observeEvent(input$contact_number, handlerExpr = {
        
        resultsGood$contact_number <- FALSE
        rvToReturn$results$contact_number <- input$contact_number
        
        # TODO. Improve validation here to check for nchars according to your
        # country (derived from your dialing code).
        
        nChar <- nchar(input$contact_number)
        outputMsgs$contact_number <- sprintf('%d chars. remaining', 14 - nChar)
        
        if (!isValidInput.text(x = input$contact_number, 
                               bRequired = FALSE, 
                               nCharMin = 12,
                               nCharMax = 14)) {
          return(NULL)
        }
        
        if (length(grep(pattern = "^+", x = input$contact_number)) != 1) {
          return(NULL)
        }
        
        if (length(grep(pattern = "[^[:digit:]]", 
                        x = substr(x = input$contact_number, 
                                   start = 2, 
                                   stop = nchar(input$contact_number)))) == 1) {
          return(NULL)
        }
        
        # TODO. Improve by actually VERIFYING the contact number via some 
        # send-message test.
        
        resultsGood$contact_number <- TRUE
      })
      
      observeEvent(input$supporting_evidence_contact_no, handlerExpr = {
        
        rvToReturn$results$supporting_evidence_contact_no <- NULL
        outputMsgs$supporting_evidence_contact_no <- ""
        
        validate(need(input$supporting_evidence_contact_no, message = FALSE))
        
        showModal(
          modalDialog(
            title = "Uploading file...", 
            "Please be patient.", 
            footer = NULL, 
            size = "s", 
            easyClose = FALSE))
        
        df <- input$supporting_evidence_contact_no
        bErr <- TRUE
        tryCatch({
          ipfsURI <- uploadToIpfs(
            df = df, 
            encrypt = TRUE, 
            wrap = FALSE,
            cidOnly = TRUE)
          rvToReturn$results$supporting_evidence_contact_no <- ipfsURI
          bErr <- FALSE
        }, error = function(e) { 
          cat(sprintf("%s [ERROR]: %s\n", Sys.time(), e), 
              file = sprintf("%slog.txt", logdir), 
              append = TRUE)
        })
        
        removeModal()
        
        if (bErr) {
          showModal(
            modalDialog(
              title = "Upload failed.",
              sprintf("Failed to upload the file (%s) to IPFS.",
                      df$name),
              footer = NULL,
              size = "s",
              easyClose = TRUE))
          return(NULL)
        }
        
      })
      
      
      # outputs ----------------------------------------------------------------
      
      output$initials_msg <- renderText({
        outputMsgs$initials
      })
      
      output$name_first_msg <- renderText({
        outputMsgs$name_first
      })
      
      output$name_last_msg <- renderText({
        outputMsgs$name_last
      })
      
      output$contact_number_msg <- renderText({
        outputMsgs$contact_number
      })
      
      output$date_of_birth_msg <- renderText({
        outputMsgs$date_of_birth
      })
      
      output$country_msg <- renderText({
        outputMsgs$country
      })
      
      output$supporting_evidence_pp1_msg <- renderText({
        
        if (nchar(outputMsgs$supporting_evidence_pp1) == 0) {
          if (length(rvToReturn$results$supporting_evidence_pp1) == 1) {
            if (nchar(rvToReturn$results$supporting_evidence_pp1) > 0) {
              return(rvToReturn$results$supporting_evidence_pp1)
            }
          }
        }
        
        outputMsgs$supporting_evidence_pp1
      })
      
      output$supporting_evidence_pp2_msg <- renderText({
        
        if (nchar(outputMsgs$supporting_evidence_pp2) == 0) {
          if (length(rvToReturn$results$supporting_evidence_pp2) == 1) {
            if (nchar(rvToReturn$results$supporting_evidence_pp2) > 0) {
              return(rvToReturn$results$supporting_evidence_pp2)
            }
          }
        }
        
        outputMsgs$supporting_evidence_pp2
      })
      
      output$supporting_evidence_contact_no_msg <- renderText({
        
        if (nchar(outputMsgs$supporting_evidence_contact_no) == 0) {
          if (length(rvToReturn$results$supporting_evidence_contact_no) == 1) {
            if (nchar(rvToReturn$results$supporting_evidence_contact_no) > 0) {
              return(rvToReturn$results$supporting_evidence_contact_no)
            }
          }
        }
        
        outputMsgs$supporting_evidence_contact_no
      })
      
      output$email_address <- renderText({
        rvToReturn$results$email_address
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
