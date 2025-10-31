
emailInputUI <- function(id, 
                         presetVal = "",
                         hL = 4, 
                         colWidth = 12, 
                         inpWidth = DEFAULT_INP_WIDTH) {
  
  ns <- NS(id)
  
  if (length(presetVal) == 0) {
    presetVal <- ""
  }
  
  tagList(
    
    ##useShinyjs(),
    
    # GtextInput(id = ns('email_address'),
    #            title = 'Email Address',
    #            helpTxt = 'What is your email address?', 
    #            value = presetVal,
    #            hL = hL,
    #            colWidth = colWidth,
    #            inpWidth = inpWidth),
    
    fluidRow(
      column(
        width = colWidth,
        wellPanel(
          h4("Email Address"),
          helpText("What is your email address?"),
          textInput(
            inputId = ns('email_address'), 
            label = NULL, 
            value = presetVal, 
            width = inpWidth),
          textOutput(outputId = ns("email_address_msg")),
          br(),
          fluidRow(
            column(
              width = 4,
              br(),
              actionButton(inputId = ns("abVerifyEmail"), 
                           label = "Send Verification Code")),
            column(
              width = 8,
              textInput(inputId = ns("tiVerifCode"), 
                        label = "Verification code:"))),
          textOutput(outputId = ns("toVerifCode"))))))
  
}

emailInputServer <- function(id, 
                             presetVal = NULL) {
  
  
  moduleServer(
    id = id, 
    function(input, output, session) {
      
      ns <- session$ns
      
      rvToReturn <- reactiveValues()
      rvToReturn$allResultsGood <- FALSE
      rvToReturn$results <- reactiveValues(
        email_address = NULL,
        verification_res = NULL # verification result
      )
      
      resultsGood <- reactiveValues()
      resultsGood$email_address <- FALSE # Will only be set to TRUE if email
      # address were properly verified.
      
      outputMsgs <- reactiveValues()
      outputMsgs$email_address <- ""
      outputMsgs$verifCode <- ""
      
      rvOther <- reactiveValues()
      rvOther$uiMode <- 1
      rvOther$randmCode <- NULL
      rvOther$tEmailSent <- NULL
      rvOther$btnLbl <- "Verify"
      rvOther$emailAddr <- NULL
      rvOther$nAttempts <- 0
      rvOther$verifInProgress <- FALSE
      rvOther$isValidAddr <- FALSE
      
      # preset inputs ----------------------------------------------------------
      
      if (length(presetVal) == 1) {
        if (!is.na(presetVal)) {
          updateTextInput(session = session, 
                          inputId = "email_address", 
                          value = presetVal)
        }
      }
      
      # ui rendering -----------------------------------------------------------
      
      observeEvent(rvOther$btnLbl, handlerExpr = {
        updateActionButton(session = session,
                           inputId = "abVerifyEmail", 
                           label = rvOther$btnLbl)
      })
      
      # inputs -----------------------------------------------------------------
      
      observeEvent(input$email_address, handlerExpr = {
        
        resultsGood$email_address <- FALSE
        rvOther$isValidAddr <- FALSE
        rvToReturn$results$verification_res <- NULL
        outputMsgs$verifCode <- ""
        rvToReturn$results$email_address <- input$email_address
        rvOther$verifInProgress <- FALSE
        rvOther$btnLbl <- "Verify"
        rvOther$uiMode <- 1
        
        nChar <- nchar(input$email_address)
        outputMsgs$email_address <- sprintf('%d chars. remaining', 50 - nChar)
        
        rvOther$isValidAddr <- validateEmail(input$email_address)
        # Note: 'TRUE' here means formally valid, but not yet verified.
        
      })
      
      observeEvent(input$abVerifyEmail, handlerExpr = {
        
        if (!rvOther$isValidAddr) { 
          outputMsgs$email_address <- "Invalid email address."
          return(NULL) 
        }
        
        if (rvOther$nAttempts >= 3) {
          outputMsgs$verifCode <- "Maximum number of attempts reached. Please try again later."
          return(NULL)
        }
        
        if (rvOther$verifInProgress) {
          # Ignore the click.
          return(NULL)
        }
        
        rvOther$verifInProgress <- TRUE
        rvOther$emailAddr <- input$email_address
        rvOther$randmCode <- sample(x = 100000:999999, size = 1)

        showModal(
          modalDialog(title = "Sending email...", 
                      footer = NULL, 
                      size = "s", 
                      easyClose = FALSE))
        
        tryCatch({
          
          # NOTE: According to SMTP protocol every \n must be preceded by an \r 
          # and the entire message must end with \r\n.\r\n
          
          msg <- sprintf(
            'From: "Independent Impact" <%s>\r\nTo: "" <%s>\r\nSubject: Independent Impact Standard - Email verification\r\n\r\n%s\r\n\r\n.\r\n',
            iwefdj$EMAIL_CONF,
            rvOther$emailAddr,
            rvOther$randmCode)
          msg <- rvOther$randmCode
          #showNotification(Sys.info()[["user"]], duration = 4)
          #res <- curl::send_mail(mail_from = iwefdj$EMAIL_CONF, 
          #                       mail_rcpt = rvOther$emailAddr, 
          #                       message = msg, 
          #                       smtp_server = "smtpout.secureserver.net:465", #iwefdj$EMAIL_CONF_SRVR, 
          #                       use_ssl = "try", 
          #                       verbose = TRUE,
          #                       username = iwefdj$EMAIL_CONF, 
          #                       password  = iwefdj$EMAIL_CONF_PWD)
          novaComs::novaMailr(
            to = rvOther$emailAddr,
            text = msg, 
            subject = "Independent Impact - OTP", 
            draft = FALSE)
          
        }, error = function(e) {
          
          outputMsgs$verifCode <- sprintf(
            "Failed to send verification email. (Error: %s)", e)
          rvOther$verifInProgress <- FALSE
          return(NULL)
          
        })
        
        removeModal()
        
        rvOther$tEmailSent <- Sys.time()
        rvOther$nAttempts <- rvOther$nAttempts +1
        outputMsgs$verifCode <- sprintf(
          "A six-digit code has just been sent to %s. Please fetch the code and enter it in the box above.",
          rvOther$emailAddr)
        rvOther$uiMode <- 2
        
      })
      
      observeEvent(input$tiVerifCode, handlerExpr = {
        
        validate(need(rvOther$randmCode, message = FALSE))
        validate(need(input$tiVerifCode, message = FALSE))
        
        if (nchar(input$tiVerifCode) < 6) {
          return(NULL)
        }
        
        if (Sys.time() >= (rvOther$tEmailSent + (5*60))) {
          outputMsgs$verifCode <- "Code expired. Please try again."
          rvOther$verifInProgress <- FALSE
          rvOther$btnLbl <- "Resend"
          return(NULL)
        }
        
        if (input$tiVerifCode != rvOther$randmCode) {
          outputMsgs$verifCode <- "Wrong code. Verification failed."
          rvToReturn$results$verification_res <- "Failed challenge-response."
          rvOther$verifInProgress <- FALSE
          rvOther$btnLbl <- "Retry"
          return(NULL)
        }
        
        tNow <- Sys.time()
        
        rvToReturn$results$verification_res <- sprintf(
          "Verified via challenge-response at %s. Challenge code: %s. Signature: %s.", 
          tNow, 
          rvOther$randmCode, 
          paste(
            PKI.encrypt(
              what = charToRaw(sprintf("E:%s T:%s C:%s", 
                                       rvOther$emailAddr, 
                                       tNow, 
                                       rvOther$randmCode)), 
              key = PKI.load.key(
                file = sprintf("%spub.pem", 
                               iwefdj$PATH_RSAKEY))), 
            collapse = ""))
        
        outputMsgs$verifCode <- "Email address verified."
        resultsGood$email_address <- TRUE 
        rvOther$verifInProgress <- FALSE
        rvOther$uiMode <- 3
        
      })
      
      # outputs ----------------------------------------------------------------
      
      output$email_address_msg <- renderText({
        outputMsgs$email_address
      })
      
      output$toVerifCode <- renderText({
        outputMsgs$verifCode 
      })
      
      # return logic -----------------------------------------------------------
      
      observe({
        rvToReturn$allResultsGood <- all(unlist(reactiveValuesToList(resultsGood)))
      })
      
      return(rvToReturn)
    })
}
