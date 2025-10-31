generalUI <- function(id, userType = "USER") {
  
  ns <- NS(id)
  
  if (userType == "USER") {
    return(
      tagList(
        
        fluidRow(
          column(
            width = 10),
          column(
            width = 2,
            br(),
            actionButton(inputId = ns("abLogOut"), 
                         label = "Log Out", 
                         width = DEFAULT_INP_WIDTH))),
        
        tabsetPanel(
          id = ns("tspMain"),
          type = "pills",
          tabPanel(title = "Projects",
                   br(),
                   tabProjectsUI(id = ns("mProjects"))),
          tabPanel(title = "Credentials",
                   br(),
                   agentUI(id = ns("mMyCredentials"))))))
  }
  
  if (userType == "STANDARD_REGISTRY") {
    return(
      tagList(
        
        fluidRow(
          column(
            width = 10),
          column(
            width = 2,
            br(),
            actionButton(inputId = ns("abLogOut"), 
                         label = "Log Out", 
                         width = DEFAULT_INP_WIDTH))),
        
        tabsetPanel(
          id = ns("tspMain"),
          type = "pills",
          tabPanel(title = "Standards Body",
                   br(),
                   tabStandardsBdUI(id = ns("mStandardsBd"))))))
  }
  
}

generalServer <- function(id, 
                          loginInfoUsr) {
  
  moduleServer(
    id = id, 
    function(input, output, session) {
      
      ns <- session$ns
      
      rvToReturn <- reactiveValues()
      rvToReturn$goToModule <- NULL 
      rvToReturn$done <- FALSE
      
      resultsGood <- reactiveValues()
      
      outputMsgs <- reactiveValues()
      
      rvOther <- reactiveValues()
      
      # Initialise db connection.
      rvOther$dbCon <- getDbCon()
      
      # Initialise Hedera client.
      {
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
        
        rvOther$hederaClient <- hederaClient
      }
      
      # module servers ---------------------------------------------------------
      
      modSrvrs <- reactiveValues()
      
      modSrvrs$mProjects <- tabProjectsServer(
        id = "mProjects",
        dbCon = rvOther$dbCon,
        hederaClient = rvOther$hederaClient,
        loginInfoUsr = loginInfoUsr)
      
      # modSrvrs$mPolicies <- tabPoliciesServer(
      #   id = "mPolicies")
      # 
      # modSrvrs$mSchemas <- tabSchemasServer(
      #   id = "mSchemas")
      # 
      # modSrvrs$mAgents <- tabAgentsServer(
      #   id = "mAgents")
      
      modSrvrs$mMyCredentials <- agentServer(
        id = "mMyCredentials",
        dbCon = rvOther$dbCon,
        hederaClient = rvOther$hederaClient,
        loginInfoUsr = loginInfoUsr)
      
      if (loginInfoUsr$userType == "STANDARD_REGISTRY") {
        
        modSrvrs$mStandardsBd <- tabStandardsBdServer(
          id = "mStandardsBd",
          dbCon = rvOther$dbCon,
          hederaClient = rvOther$hederaClient,
          loginInfoUsr = loginInfoUsr)
      }
      
      # inputs -----------------------------------------------------------------
      
      observeEvent(input$abLogOut, handlerExpr = {
        
        # Disconnect from data base.
        message("Module 'general': Disconnecting from db...")
        dbDisconnect(rvOther$dbCon)
        
        # Inform parent we are done.
        rvToReturn$done <- TRUE
        
      })
      
      # outputs ----------------------------------------------------------------
      
      # initialisation triggers ------------------------------------------------
      
      # return logic -----------------------------------------------------------
      return(rvToReturn)
      
    })
}
