
# This module is for creating a new account on the Independent Impact platform. 
# They will first need to accept our Independent Impact terms and conditions, though.

createIndImpAccountUI <- function(id, 
                                  lsPreset = NULL, 
                                  hL = 4, 
                                  colWidth = 12, 
                                  inpWidth = DEFAULT_INP_WIDTH) {
  
  ns <- NS(id)
  
  tagList(
    
    ##useShinyjs(),
    
    fluidRow(
      column(
        width = 3),
      column(
        width = 6,
        
        uiOutput(outputId = ns("uio1")),
        br(),
        br(),
        GtextOutput(id = ns("toMsgMain"), 
                    hL = hL, 
                    colWidth = colWidth)),
      column(
        width = 3)))
}

createIndImpAccountServer <- function(id, 
                                      hL = 4, 
                                      colWidth = 12, 
                                      inpWidth = DEFAULT_INP_WIDTH) {
  
  moduleServer(
    id = id, 
    function(input, output, session) {
      
      require(httr)
      
      ns <- session$ns
      
      rvToReturn <- reactiveValues()
      rvToReturn$goToModule <- NULL
      rvToReturn$allResultsGood <- FALSE
      rvToReturn$results <- reactiveValues(
        email_address = NULL,
        id_agent = NULL, # Our db ID for the user.
        userType = NULL,
        did = NULL #,
        #refreshToken = NULL
        ) 
      
      resultsGood <- reactiveValues()
      resultsGood$email_address <- FALSE
      resultsGood$id_agent <- FALSE
      resultsGood$pwd <- FALSE # Not in rvToReturn$results, but we do need it in this module itself.
      resultsGood$userType <- FALSE
      resultsGood$did <- FALSE
      resultsGood$bAccCreated <- FALSE # Not in rvToReturn$results, but we do need it in this module itself.
      
      outputMsgs <- reactiveValues()
      outputMsgs$piPassword1 <- ""
      outputMsgs$piPassword2 <- ""
      outputMsgs$main <- ""
      
      rvOther <- reactiveValues()
      rvOther$uiMode <- c("emailAndPassw", "showIndImpTaCs", 
                          "indImpTaCsAccepted", "indImpTaCsRejected",
                          "indImpAccCreatSucc")[1]
      rvOther$email_address <- NULL 
      rvOther$pwd <- NULL
      rvOther$id_acc_h <- NULL
      
      # module servers ---------------------------------------------------------
      modSrvrs <- reactiveValues()
      
      modSrvrs$eiEmail <- emailInputServer(
        id = "eiEmail")
      
      # preset inputs ----------------------------------------------------------
      
      
      # inputs -----------------------------------------------------------------
      
      # modSrvrs$eiEmail$...
      observe({
        resultsGood$email_address <- modSrvrs$eiEmail$allResultsGood
        
        rvToReturn$results$email_address <-
          modSrvrs$eiEmail$results$email_address
      })
      
      observeEvent(input$piPassword1, handlerExpr = {
        
        resultsGood$pwd <- FALSE
        outputMsgs$password1 <- ""
        
        validate(need(input$piPassword1, message = FALSE))
        
        res <- validatePassword(
          pwd = input$piPassword1, 
          emailAddr = rvToReturn$results$email_address)
        
        outputMsgs$password1 <- res$msg
        
        if (!res$valid) { return(NULL) }
        
        validate(need(input$piPassword2, message = FALSE))
        
        if (input$piPassword2 != input$piPassword1) {
          outputMsgs$password2 <- "Passwords do not match."
          return(NULL)
        }
        
        outputMsgs$password2 <- "Passwords match."
        resultsGood$pwd <- TRUE
      })
      
      observeEvent(input$piPassword2, handlerExpr = {
        
        resultsGood$pwd <- FALSE
        outputMsgs$password2 <- ""
        
        validate(need(input$piPassword2, message = FALSE))
        validate(need(input$piPassword1, message = FALSE))
        
        if (input$piPassword2 != input$piPassword1) {
          outputMsgs$password2 <- "Passwords do not match."
          return(NULL)
        }
        
        outputMsgs$password2 <- "Passwords match."
        resultsGood$pwd <- TRUE
        
      })
      
      observeEvent(input$abCreateAccount, handlerExpr = {
        
        if (!resultsGood$pwd) {
          return(NULL)
        }
        if (!resultsGood$email_address) {
          return(NULL)
        }
        
        usrPwd <- input$piPassword1
        pwdValidRes <- validatePassword(
          pwd = usrPwd, 
          emailAddr = rvToReturn$results$email_address)
        if (!pwdValidRes$valid) {
          return(NULL)
        }
        
        # Make sure this email address is not already in our database.
        {
          dbCon <-  getDbCon()
          
          q <- sprintf("SELECT * FROM tbl_link_agents_x_email_addresses WHERE email_address = '%s';",
                       modSrvrs$eiEmail$results$email_address)
          dfEx <- dbGetQuery(conn = dbCon, statement = q)
          
          dbDisconnect(dbCon)
          
          if (nrow(dfEx) > 0) {
            outputMsgs$main <- "This email address is already in use. Please choose a different one or return to the login screen to log in with this email address."
            return(NULL)
          }
        }
        
        outputMsgs$main <- ""
        
        # Save the inputs for account creation once the user has accepted the 
        # Independent Impact Terms and Conditions.
        rvOther$email_address <- modSrvrs$eiEmail$results$email_address 
        rvOther$pwd <- usrPwd
        
        rvOther$uiMode <- "showIndImpTaCs"
        
      })
      
      observeEvent(input$abAcceptIndImpTaCs, handlerExpr = {

        rvOther$uiMode <- "indImpTaCsAccepted"
        
        currStep <- 0
        nTotSteps <- 11 
        
        withProgress(
          min = 0, max = nTotSteps, value = 0, message = "Creating your account...",
          expr = {
            
            tryCatch({
              
              # Initialise Hedera client.
              {
                operatorAccId <- hiero$AccountId$from_string(
                  iwefdj[["HEDERA_OPERATOR_ACCOUNT_ID_ED25519"]])
                operatorPrivKey <- hiero$PrivateKey$from_string(
                  iwefdj[["HEDERA_OPERATOR_ACCOUNT_PRIVATE_KEY_ED25519"]])
                
                client <- hiero$Client(network = hiero$Network(hederaNetwork))
                client$set_operator(
                  account_id = operatorAccId, 
                  private_key = operatorPrivKey)
                
                balance_query <- hiero$CryptoGetAccountBalanceQuery(
                  account_id = operatorAccId)
                balance <- balance_query$execute(client)
                if (balance$hbars$to_hbars() < 100) {
                  stop("Not enough HBAR to perform operations. Please contact admin.")
                }
              }
              
              # Initialise dbCon.
              dbCon <-  getDbCon()
              
              # 1. Create an entry into tbl_agents.
              {
                currStep <- currStep + 1
                incProgress(
                  amount = 0,
                  message = sprintf("Step %s/%s", currStep, nTotSteps))
                
                dfAdd <- data.frame(
                  id = NA_character_,
                  date_registered = lubridate::now(tzone = "UTC"),
                  type_user = "USER")
                
                idAgent <- addToDb(
                  dfAdd = dfAdd,
                  tblNm = "tbl_agents",
                  calcIds = TRUE,
                  returnIds = TRUE,
                  dbCon = dbCon,
                  vnmsChckEx = NULL)
                
                rvToReturn$results$userType <- "USER"
                rvToReturn$results$id_agent <- idAgent
                resultsGood$userType <- TRUE
                resultsGood$id_agent <- TRUE
                
                incProgress(amount = 1)
              }
              
              # 2. Create an entry into tbl_link_agents_x_ics_passwords.
              {
                currStep <- currStep + 1
                incProgress(
                  amount = 0,
                  message = sprintf("Step %s/%s", currStep, nTotSteps))
                
                dfAdd <- data.frame(
                  id_agent = idAgent,
                  hash_pw_ics = digest::digest(object = rvOther$pwd, algo = "sha512"))
                
                addToDb(
                  dfAdd = dfAdd,
                  tblNm = "tbl_link_agents_x_ics_passwords",
                  vnmsChckEx = "id_agent",
                  dbCon = dbCon,
                  calcIds = FALSE)
                
                incProgress(amount = 1)
              }
              
              # 3. Create an entry into tbl_link_agents_x_email_addresses.
              {
                currStep <- currStep + 1
                incProgress(
                  amount = 0,
                  message = sprintf("Step %s/%s", currStep, nTotSteps))
                
                dfAdd <- data.frame(
                  id_agent = idAgent,
                  email_address = rvOther$email_address)
                
                addToDb(
                  dfAdd = dfAdd,
                  tblNm = "tbl_link_agents_x_email_addresses",
                  vnmsChckEx = names(dfAdd),
                  dbCon = dbCon,
                  calcIds = FALSE)
                
                incProgress(amount = 1)
              }
              
              # 4. Create a public/private key pair for the user for their Hedera 
              #     account (and their DID + DID topic).
              {
                currStep <- currStep + 1
                incProgress(
                  amount = 0,
                  message = sprintf("Step %s/%s", currStep, nTotSteps))

                userPrivKey <- hiero$PrivateKey$generate_ed25519()
                userPubKey <- userPrivKey$public_key()
                
                # Add this key pair to tbl_key_pairs.
                {
                  encrPrivKey <- cyphr::encrypt_string(
                    string = userPrivKey$to_string_der(), 
                    key = cyphr::keypair_openssl(
                    pub = iwefdj$KEYPTH_CYPHR, 
                    key = iwefdj$KEYPTH_CYPHR, 
                    envelope = TRUE,
                    password = iwefdj$PW_CYPHR))
                  encrPrivKey <- openssl::base64_encode(encrPrivKey)
                  
                  # decrRaw <- openssl::base64_decode(encrPrivKey)
                  # decrChar <- cyphr::decrypt_string(
                  #   data = decrRaw, 
                  #   key = cyphr::keypair_openssl(
                  #     pub = iwefdj$KEYPTH_CYPHR, 
                  #     key = iwefdj$KEYPTH_CYPHR, 
                  #     envelope = TRUE,
                  #     password = iwefdj$PW_CYPHR))
                  
                  idUserKeyPair <- addToDb(
                    dfAdd = data.frame(
                      public_key = userPubKey$to_string(),
                      private_key_encr = encrPrivKey), 
                    tblNm = "tbl_key_pairs", 
                    vnmsChckEx = "public_key", 
                    dbCon = dbCon, 
                    calcIds = TRUE, 
                    returnIds = TRUE)
                }
                
                incProgress(amount = 1)
              }
              
              # 5. Create a Hedera account for the user.
              {
                currStep <- currStep + 1
                incProgress(
                  amount = 0,
                  message = sprintf("Step %s/%s", currStep, nTotSteps))

                tx <- hiero$AccountCreateTransaction(
                  key = userPubKey, 
                  initial_balance = hiero$Hbar(20), 
                  receiver_signature_required = FALSE, 
                  auto_renew_period = as.integer(90*24*60*60), 
                  memo = rvToReturn$results$id_agent)
                tx <- tx$freeze_with(client)
                tx <- tx$sign(operatorPrivKey)
                resp <- tx$execute(client)
                
                rvOther$id_acc_h <- paste(
                  resp$accountId$shard,
                  resp$accountId$realm, 
                  resp$accountId$num, sep = ".")
                
                incProgress(amount = 1)
              }

              # 6. Link the account and the key pair in the db.
              {
                currStep <- currStep + 1
                incProgress(
                  amount = 0,
                  message = sprintf("Step %s/%s", currStep, nTotSteps))
                
                addToDb(
                  dfAdd = data.frame(
                    id_account_h = rvOther$id_acc_h,
                    id_key_pair = idUserKeyPair,
                    label_key_pair = "PRIMARY"), 
                  tblNm = "tbl_link_hedera_accounts_x_key_pairs", 
                  vnmsChckEx = c("id_account_h", "id_key_pair", "label_key_pair"), 
                  dbCon = dbCon, 
                  calcIds = FALSE)
                
                incProgress(amount = 1)
              }
              
              # 7. Create an entry into tbl_link_agents_x_hedera_accounts.
              {
                currStep <- currStep + 1
                incProgress(
                  amount = 0,
                  message = sprintf("Step %s/%s", currStep, nTotSteps))
                
                dfAdd <- data.frame(
                  id_agent = idAgent,
                  id_acc_h = rvOther$id_acc_h,
                  label_acc_h = "PRIMARY")
                
                addToDb(
                  dfAdd = dfAdd,
                  tblNm = "tbl_link_agents_x_hedera_accounts",
                  vnmsChckEx = names(dfAdd),
                  dbCon = dbCon,
                  calcIds = FALSE)
                
                incProgress(amount = 1)
              }
              
              # 8. Create a Hedera DID (and thus a topic) for the user.
              {
                currStep <- currStep + 1
                incProgress(
                  amount = 0,
                  message = sprintf("Step %s/%s", currStep, nTotSteps))
                
                # Register a new identifier.
                {
                  did <- hieroDid$HederaDid(
                    client = client, 
                    private_key_der = userPrivKey$to_string_der())
                  asyncio$run(did$register())
                  #sprintf("New DID created with identifier %s", did$identifier)
                  rvToReturn$results$did <- did$identifier
                  resultsGood$did <- TRUE
                }
                
                # Create the DID document.
                {
                  didDoc <- list(
                    "@context" = list("https://www.w3.org/ns/did/v1"),
                    id = did$identifier,
                    verificationMethod = list(
                      list(
                        id = paste0(did$identifier, "#key-1"),
                        type = "Ed25519VerificationKey2018",
                        controller = did$identifier,
                        publicKeyMultibase = hieroDid$utils$encoding$multibase_encode(
                          value = userPubKey$to_bytes_raw(), 
                          encoding = "base58btc"))),
                    authentication = list(paste0(did$identifier, "#key-1")),
                    assertionMethod = list(paste0(did$identifier, "#key-1")))
                }
                
                # Upload the DID document to IPFS.
                {
                  fp <- tempfile(fileext = ".json")
                  cat(jsonlite::toJSON(x = didDoc, auto_unbox = TRUE), file = fp)
                  
                  ipfsCid <- uploadToIpfs(
                    df = data.frame(
                      datapath = fp, 
                      name = "did-doc.json",
                      ext = ".json"), 
                    encrypt = FALSE, 
                    zip = FALSE, 
                    cidOnly = TRUE, 
                    wrap = FALSE)
                  
                  file.remove(fp)
                }
                
                # Add a service entry to the DID.
                asyncio$run(
                  did$add_service(
                    id_ = paste0(did$identifier, "#service-0"), 
                    service_type = "DIDDocument", 
                    service_endpoint = paste0("ipfs://", ipfsCid)))
                
                # # Test.
                # resolver <- hieroDid$HederaDidResolver(client)
                # result <- asyncio$run(resolver$resolve(did$identifier))
                
                incProgress(amount = 1)
              }
              
              # 9. Link the DID topic and its key pair in the db.
              {
                currStep <- currStep + 1
                incProgress(
                  amount = 0,
                  message = sprintf("Step %s/%s", currStep, nTotSteps))
                
                didTopicId <- strsplit(did$identifier, split = "_", fixed = TRUE)[[1]][2]
                addToDb(
                  dfAdd = data.frame(
                    id_topic_h = didTopicId,
                    id_key_pair = idUserKeyPair,
                    label_key_pair = "ADMIN"), 
                  tblNm = "tbl_link_hedera_topics_x_key_pairs", 
                  vnmsChckEx = c("id_topic_h", "id_key_pair", "label_key_pair"), 
                  dbCon = dbCon, 
                  calcIds = FALSE)
                
                incProgress(amount = 1)
              }
              
              # 10. Create an entry into tbl_link_agents_x_dids.
              {
                currStep <- currStep + 1
                incProgress(
                  amount = 0,
                  message = sprintf("Step %s/%s", currStep, nTotSteps))
                
                dfAdd <- data.frame(
                  id_agent = rvToReturn$results$id_agent,
                  did = did$identifier,
                  uri_ipfs_doc_did = paste0("ipfs://", ipfsCid))
                
                addToDb(
                  dfAdd = dfAdd, 
                  tblNm = "tbl_link_agents_x_dids", 
                  vnmsChckEx = "did", 
                  dbCon = dbCon, 
                  calcIds = FALSE)
                
                incProgress(amount = 1)
              }
              
              # 11. Link the agent to their DID topic in the db. 
              {
                currStep <- currStep + 1
                incProgress(
                  amount = 0,
                  message = sprintf("Step %s/%s", currStep, nTotSteps))
                
                addToDb(
                  dfAdd = data.frame(
                    id_topic_h = didTopicId, 
                    id_entity = rvToReturn$results$id_agent, 
                    label_topic_h = "DID"), 
                  tblNm = "tbl_link_entities_x_hedera_topics", 
                  vnmsChckEx = c("id_topic_h", "id_entity", "label_topic_h"), 
                  dbCon = dbCon, 
                  calcIds = FALSE)
                
                incProgress(amount = 1)
              }
              
              dbDisconnect(dbCon)
              
              outputMsgs$main <- "Your Independent Impact account has been created. Welcome!"
              resultsGood$bAccCreated <- TRUE # This will trigger the calling module to end us.
              rvOther$uiMode <- "indImpAccCreatSucc"
              
            }, error = function(e) {
              dbDisconnect(dbCon)
              outputMsgs$main <- sprintf("Account creation failed. Error: %s", e)
              return(NULL)
            })
            
          })
        
      })
      
      observeEvent(input$abRejectIndImpTaCs, handlerExpr = {
        rvOther$uiMode <- "indImpTaCsRejected"
      })
      
      observeEvent(input$abBackFromIndImpTaCs, handlerExpr = {
        rvOther$uiMode <- "emailAndPassw"
      })
      
      observeEvent(input$abBackFromIndImpAccCreatFailed, handlerExpr = {
        rvOther$uiMode <- "emailAndPassw"
      })
      
      # outputs ----------------------------------------------------------------
      
      output$uio1 <- renderUI({
        
        if (rvOther$uiMode == "emailAndPassw") {
          return(
            tagList(
              emailInputUI(id = ns("eiEmail"), 
                           hL = hL, 
                           colWidth = colWidth, 
                           inpWidth = inpWidth),
              
              GpasswordInput(id = ns("piPassword1"), 
                             title = "Password", 
                             helpTxt = "Must be at least 8 characters in length, must have at least one letter, one digit and one special character (#,@,! or _), and cannot contain any spaces.",
                             hL = hL, 
                             colWidth = colWidth, 
                             inpWidth = inpWidth),
              
              GpasswordInput(id = ns("piPassword2"), 
                             title = "Confirm password", 
                             hL = hL, 
                             colWidth = colWidth, 
                             inpWidth = inpWidth),
              
              actionButton(inputId = ns("abCreateAccount"),
                           label = "Create account")))
        }
        
        if (rvOther$uiMode == "showIndImpTaCs") {
          return(
            tagList(
              htmlOutput(outputId = ns("htmloIndImpTaCs")),
              actionButton(inputId = ns("abAcceptIndImpTaCs"), 
                           label = "I accept"),
              actionButton(inputId = ns("abRejectIndImpTaCs"), 
                           label = "I do not accept"),
              actionButton(inputId = ns("abBackFromIndImpTaCs"), 
                           label = "Back")))
        } 
        
        if (rvOther$uiMode == "indImpTaCsRejected") {
          return(
            tagList(
              helpText("We can unfortunately not continue with the account creation process if you do not accept our Independent Impact Terms and Conditions."),
              actionButton(inputId = ns("abBackFromIndImpTaCs"), 
                           label = "Back")))
        } 
        
        if (rvOther$uiMode == "indImpTaCsAccepted") {
          return(tagList(hx(x = "Creating your account...", lvl = 3))) 
        }
        
        if (rvOther$uiMode == "indImpAccCreatSucc") {
          return(tagList(hx(x = "Account creation successful.", lvl = 3)))
        }
        
        if (rvOther$uiMode == "indImpAccCreatFailed") {
          return(tagList(hx(x = "Account creation failed.", lvl = 3),
                         actionButton(inputId = ns("abBackFromIndImpAccCreatFailed"), 
                                      label = "Back")))
        }
        
        warning(sprintf("Unknown value for rvOther$uiMode encountered: %s.",
                        rvOther$uiMode))
        return(NULL)
      })
      
      output$htmloIndImpTaCs <- renderText({
        txt <- readLines(
          con = sprintf("%sterms-and-conditions/ii-20251028.htm", appdir))
        
        #txt <- "Here is some dummy text."
        
        txt <- paste(txt, sep = "", collapse = "")
        #HTML(txt)
        txt
      })
      
      output$piPassword1_msg <- renderText({ outputMsgs$password1 })
      
      output$piPassword2_msg <- renderText({ outputMsgs$password2 })
      
      output$toMsgMain <- renderText({ outputMsgs$main })
      
      # return logic -----------------------------------------------------------
      
      observe({
        rvToReturn$allResultsGood <- all(unlist(reactiveValuesToList(resultsGood)))
      })
      
      return(rvToReturn)
    })
}
