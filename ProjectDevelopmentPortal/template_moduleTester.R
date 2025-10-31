
# Use this file to create your own moduleTester.R.

source("global.R")

if (FALSE) {
  
  ui <- fluidPage(
    
    tagList(
      uiOutput(outputId = "uioAppMain"),
      verbatimTextOutput(outputId = "vtoTest"))
  )
  
  server <- function(input, output, session) {
    
    rv <- reactiveValues()
    rv$moduleCurr <- "technologyOrMeasure"
    
    moduleResult <- reactiveValues()
    
    output$uioAppMain <- renderUI({
      
      eval(
        parse(text = sprintf("return(%s%s(id = '%s'))", 
                             rv$moduleCurr,
                             c("UI", "Input", "Output")[2],
                             genModuleId(rv$moduleCurr))))
    })
    
    observe({
      if (length(rv$moduleCurr) > 0) {
        eval(
          parse(text = sprintf("moduleResult <<- %sServer(id = '%s')",
                               rv$moduleCurr, genModuleId(rv$moduleCurr))))
      }
    })
    
    output$vtoTest <- renderPrint({
      moduleResult
    })
  }
}


if (TRUE) {
  
  ui <- fluidPage(
    
    includeCSS('www/wrap_long_strings.css'),
    
    tagList(
      
      ##useShinyjs(),

      #ADschemaV2UI(id = "rvui1"),
      #tabStandardsBdUI(id = "rvui1"),
      #reviewUI(id = "rvui1"),
      #workflowStateUI(id = "rvui1"),
      #agentUI(id = "rvui1"),
      #tabProjectsUI(id = "rvui1"),
      PDDXAschemaV2UI("rvui1"),
      
      actionButton(inputId = "abClose", label = "Close"),
      verbatimTextOutput(outputId = "vtoTest")
    )
  )
  
  server <- function(input, output, session) {
    
    if (TRUE) {
      dbCon <- getDbCon(clearExisting = TRUE)
    }

    if (TRUE) {
      
      operatorAccId <- hiero$AccountId$from_string(
        iwefdj[["HEDERA_OPERATOR_ACCOUNT_ID_ED25519"]])
      operatorPrivKey <- hiero$PrivateKey$from_string(
        iwefdj[["HEDERA_OPERATOR_ACCOUNT_PRIVATE_KEY_ED25519"]])
      
      hederaClient <- hiero$Client(network = hiero$Network(hederaNetwork))
      hederaClient$set_operator(
        account_id = operatorAccId, 
        private_key = operatorPrivKey)
      
      balance_query <- hiero$CryptoGetAccountBalanceQuery(
        account_id = operatorAccId)
      balance <- balance_query$execute(hederaClient)
      if (balance$hbars$to_hbars() < 100) {
        stop("Not enough HBAR to perform operations. Please contact admin.")
      }
      
    }
    
    # Log regular user in.
    {
      usrEmail <- c("abcde12345@nomail.com")[1]
      loginInfoUsr <- switch(
        usrEmail, 
        
        "abcde12345@nomail.com" = list(
          id_agent = "REPLACE_ME",
          did = "REPLACE_ME",
          userType = "USER",
          email_address = usrEmail))
    }
    

    # moduleResult <- tabStandardsBdServer(
    #   id = "rvui1",
    #   dbCon = dbCon,
    #   hederaClient = hederaClient,
    #   loginInfoUsr = loginInfoUsr)

    # moduleResult <- reviewServer(
    #   id = "rvui1",
    #   dbCon = dbCon,
    #   loginInfoUsr = loginInfoUsr,
    #   idDocOrig = "3c1d21b3c943477c51fddf4fd80cdb9c",
    #   hederaClient = hederaClient)
    
    # moduleResult <- ADschemaV2Server(
    #   id = "rvui1",
    #   dbCon = dbCon,
    #   loginInfoUsr = loginInfoUsr)
    
    # moduleResult <- workflowStateServer(
    #   id = "rvui1",
    #   dbCon = dbCon,
    #   loginInfoUsr = loginInfoUsr,
    #   idWorkflow = "e3f1f8222b683608a9ad8845a432604b",
    #   idProject = NULL,
    #   hederaClient = hederaClient)
    
    # moduleResult <- agentServer(
    #   id = "rvui1", 
    #   dbCon = dbCon, 
    #   hederaClient = hederaClient, 
    #   loginInfoUsr = loginInfoUsr)
    
    # moduleResult <- tabProjectsServer(
    #   id = "rvui1",
    #   dbCon = dbCon,
    #   hederaClient = hederaClient,
    #   loginInfoUsr = loginInfoUsr)
    
    moduleResult <- PDDXAschemaV2Server(
      id = "rvui1", 
      dbCon = dbCon, 
      loginInfoUsr = loginInfoUsr, 
      idEntity = "827fc4ac122a7b8c47e2e67ab6a6373d")
    
    # if (!gmailr::gm_has_token()) {
    #   gmailr::gm_auth()
    # }
    # moduleResult <- emailInputServer(id = "rvui1")
    
    
    
    output$vtoTest <- renderPrint({
      
      print("moduleTester")
      print("------------------")
      
      print("moduleResult:")
      print(moduleResult)
      
      if("done" %in% names(moduleResult)) {
        print("moduleResult$done")
        print(moduleResult$done)
      }
      
      if ("allResultsGood" %in% names(moduleResult)) {
        print("moduleResult$allResultsGood:")
        print(moduleResult$allResultsGood)
      }
      
      if ("results" %in% names(moduleResult)) {
        print("results:")
        print(recursiveReactiveValuesToList(moduleResult$results))
      }
      
      if ("items" %in% names(moduleResult)) {
        print("items:")
        print(moduleResult$items)
      }
      
      if ("goToModule" %in% names(moduleResult)) {
        print("goToModule:")
        print(moduleResult$goToModule)
      }
      
      if ("argsForGoToMod" %in% names(moduleResult)) {
        print("argsForGoToMod:")
        print(moduleResult$argsForGoToMod)
      }
      
    })
    
    observeEvent(input$abClose, handlerExpr = {
      
      # doc <- recursiveReactiveValuesToList(moduleResult$results)
      # save(doc, file = "tmp/ADdoc.Rda")
      
      message("Closing db connection...")
      if (exists('dbCon')) {
        dbDisconnect(dbCon)
      }
      stopApp(0)
    })
    
  }
}


shinyApp(ui = ui, server = server)
