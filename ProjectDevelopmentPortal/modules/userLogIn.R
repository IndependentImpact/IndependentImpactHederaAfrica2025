
# A user can only log into our app, Independent Impact, with their email address. 
# If a en existing user loses their email address or wish to start using a new 
# one, there are special steps to follow. If they do not follow those special 
# steps, the new email address will be treated as belonging to a new agent.

userLogInUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    #useShinyjs(),
    
    uiOutput(outputId = ns("uio1")),
    uiOutput(outputId = ns("uio2")))
}

# Returns the user name, hash of password, user type and access token.
#
userLogInServer <- function(id, 
                            hL = 4, 
                            colWidth = 12, 
                            inpWidth = DEFAULT_INP_WIDTH) {
  
  moduleServer(
    id = id, 
    function(input, output, session) {
      
      ns <- session$ns
      
      rvToReturn <- reactiveValues()
      rvToReturn$goToModule <- NULL
      rvToReturn$argsForGoToMod <- NULL
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
      resultsGood$userType <- FALSE
      resultsGood$did <- FALSE
      #resultsGood$refreshToken <- FALSE
      
      outputMsgs <- reactiveValues()
      outputMsgs$tiEmailAddr <- ""
      outputMsgs$piPassword <- ""
      outputMsgs$loginErrorMsg <- ""
      
      rvOther <- reactiveValues()
      rvOther$uiMode <- NULL
      rvOther$init <- Sys.time()
      rvOther$nonce <- 0
      
      # module servers ---------------------------------------------------------
      
      modSrvrs <- reactiveValues()
      
      modSrvrs$modCreateAcc <- NULL
      
      # inputs -----------------------------------------------------------------
      
      observeEvent(input$tiEmailAddr, handlerExpr = {
        
        resultsGood$email_address <- FALSE
        resultsGood$id_agent <- FALSE
        resultsGood$userType <- FALSE
        resultsGood$did <- FALSE
        #resultsGood$refreshToken <- FALSE
        outputMsgs$loginErrorMsg <- ""
        
        validate(need(input$tiEmailAddr, message = FALSE))
        
        outputMsgs$tiEmailAddr <- ""
        
        if (!validateEmail(input$tiEmailAddr)) {
          outputMsgs$tiEmailAddr <- "Invalid email address."
        }
        
      })
      
      # modSrvrs$modCreateAcc$...
      observe({
        validate(need(rvOther$uiMode, message = FALSE))
        validate(need(modSrvrs$modCreateAcc$allResultsGood, message = FALSE))
        
        if (rvOther$uiMode == "createAcc") {
          if (modSrvrs$modCreateAcc$allResultsGood) {
            
            rvToReturn$results$email_address <- modSrvrs$modCreateAcc$results$email_address
            rvToReturn$results$id_agent <- modSrvrs$modCreateAcc$results$id_agent
            rvToReturn$results$userType <- modSrvrs$modCreateAcc$results$userType
            rvToReturn$results$did <- modSrvrs$modCreateAcc$results$did
            #rvToReturn$results$refreshToken <- modSrvrs$modCreateAcc$results$refreshToken
            
            resultsGood$email_address <- TRUE
            resultsGood$id_agent <- TRUE
            resultsGood$userType <- TRUE
            resultsGood$did <- TRUE
            #resultsGood$refreshToken <- TRUE
          }
        }
      })
      
      observeEvent(input$abCreateAccount, handlerExpr = {
        rvOther$uiMode <- "createAcc"
      })
      
      observeEvent(eventExpr = input$abCancelAccCreation, handlerExpr = {
        outputMsgs$loginErrorMsg <- ""
        rvOther$uiMode <- "logIn"
      })
      
      observeEvent(input$abLogIn, handlerExpr = {
        tryCatch({
        resultsGood$email_address <- FALSE
        resultsGood$id_agent <- FALSE
        resultsGood$userType <- FALSE
        resultsGood$did <- FALSE
        # resultsGood$refreshToken <- FALSE
        
        validate(need(input$tiEmailAddr, message = FALSE))
        validate(need(input$piPassword, message = FALSE))
        
        outputMsgs$loginErrorMsg <- ""
        
        if (nchar(input$tiEmailAddr) < 1) {
          outputMsgs$loginErrorMsg <- "Please enter your email address."
          return(NULL)
        }
        
        if (nchar(input$piPassword) < 1) {
          outputMsgs$loginErrorMsg <- "Please enter your II password."
          return(NULL)
        }
        
        dbCon <-  getDbCon()
        
        # Check if the email address is in our db.
        {
          q <- sprintf("SELECT * FROM tbl_link_agents_x_email_addresses WHERE email_address = '%s';",
                       input$tiEmailAddr)
          dfEx <- dbGetQuery(conn = dbCon, statement = q)
          
          if (nrow(dfEx) == 0) {
            outputMsgs$loginErrorMsg <- "You do not seem to have an II account yet. Please click on 'Register' to create an II account."
            dbDisconnect(dbCon)
            return(NULL)
          }

          if (nrow(dfEx) > 1) {
            outputMsgs$loginErrorMsg <- "Error: More than one user with this email address in our db. Please contact support."
            dbDisconnect(dbCon)
            return(NULL)
          }

          rvToReturn$results$email_address <- input$tiEmailAddr
          resultsGood$email_address <- TRUE
          rvToReturn$results$id_agent <- dfEx$id_agent
          resultsGood$id_agent <- TRUE
        }

        # Check if the password is correct.
        {
          q <- sprintf("SELECT * FROM tbl_link_agents_x_ics_passwords WHERE id_agent = '%s';",
                       dfEx$id_agent)
          dfEx <- dbGetQuery(conn = dbCon, statement = q)

          if (nrow(dfEx) == 0) {
            outputMsgs$loginErrorMsg <- "Error: No password found for this user in our db. Please contact support."
            dbDisconnect(dbCon)
            return(NULL)
          }
          
          if (digest::digest(object = input$piPassword, algo = "sha512") != dfEx$hash_pw_ics) {
            outputMsgs$loginErrorMsg <- "Incorrect password."
            dbDisconnect(dbCon)
            return(NULL)
          }
        }
        
        # Determine user type..
        {
          res <- dbGetQuery(
            conn = dbCon, 
            statement = sprintf(
              "SELECT type_user FROM tbl_agents WHERE id = '%s';", 
              rvToReturn$results$id_agent))[["type_user"]]
          
          if (length(res) == 0) {
            dbDisconnect(dbCon)
            stop("Failed to determine user type.")
          }
          if (is.na(res)) {
            dbDisconnect(dbCon)
            stop("User type is NA.")
          } 
          if (nchar(res) == 0) {
            dbDisconnect(dbCon)
            stop("Invalid user type.")
          }
          
          rvToReturn$results$userType <- res
          resultsGood$userType <- TRUE
        }
        
        # Retrieve user DID from db.
        {
          res <- dbGetQuery(
            conn = dbCon, 
            statement = sprintf(
              "SELECT did FROM tbl_link_agents_x_dids WHERE id_agent = '%s';", 
              rvToReturn$results$id_agent))[["did"]]
          
          if (length(res) == 0) {
            dbDisconnect(dbCon)
            stop("Failed to retrieve user DID from database.")
          }
          if (is.na(res)) {
            dbDisconnect(dbCon)
            stop("User DID is NA.")
          } 
          if (nchar(res) == 0) {
            dbDisconnect(dbCon)
            stop("Invalid user DID.")
          }
          
          rvToReturn$results$did <- res
          resultsGood$did <- TRUE
        }
        
        dbDisconnect(dbCon)
        
        outputMsgs$loginErrorMsg <- "Welcome."
        
        }, error = function(e) {
          outputMsgs$loginErrorMsg <- as.character(e)
        })
      })
      
      # outputs ----------------------------------------------------------------
      
      output$uio1 <- renderUI({
        
        validate(need(rvOther$uiMode, message = FALSE))
        
        if (rvOther$uiMode == "logIn") {
          
          modSrvrs$modCreateAcc <- NULL
          
          return(
            tagList(
              br(),
              br(),
              fluidRow(
                column(
                  width = 3),
                column(
                  width = 6,
                  wellPanel(
                    hx(x = "Independent Impact", 
                       lvl = 2),
                    GtextInput(id = ns("tiEmailAddr"), 
                               title = "Email Address", 
                               inpWidth = DEFAULT_INP_WIDTH),
                    GpasswordInput(id = ns("piPassword"), 
                                   title = "Password", 
                                   inpWidth = DEFAULT_INP_WIDTH),
                    fluidRow(
                      column(width = 4),
                      column(width = 4,
                             actionButton(ns("abLogIn"), 
                                          label = "Log in", 
                                          width = DEFAULT_INP_WIDTH)),
                      column(width = 4)),
                    br(),
                    textOutput(ns("toLoginErrorMsg")),
                    br(),
                    fluidRow(
                      column(width = 4),
                      column(width = 4,
                             actionButton(inputId = ns("abCreateAccount"), 
                                          label = "Register",
                                          width = DEFAULT_INP_WIDTH)),
                      column(width = 4)))),
                column(
                  width = 3))))
          
        }
        
        if (rvOther$uiMode == "createAcc") {
          
          #rvOther$nonce <- rvOther$nonce + 1
          
          modSrvrs$modCreateAcc <- createIndImpAccountServer(
            id = sprintf("modCreateAcc%s", rvOther$nonce),
            hL = hL, 
            colWidth = colWidth, 
            inpWidth = inpWidth)
          
          return(
            tagList(
              createIndImpAccountUI(id = ns(sprintf("modCreateAcc%s", 
                                                 rvOther$nonce)))))
        }
        
        warning("Invalid value encountered for rvOther$uiMode.")
        
      })
      
      output$uio2 <- renderUI({
        
        if (rvOther$uiMode != "createAcc") { return(NULL) }
        
        return(
          tagList(
            actionButton(inputId = ns("abCancelAccCreation"), 
                         label = "Cancel")))
        
      })
      
      
      output$tiEmailAddr_msg <- renderText({
        outputMsgs$tiEmailAddr
      })
      
      output$piPassword_msg <- renderText({
        outputMsgs$piPassword
      })
      
      output$toLoginErrorMsg <- renderText({
        if (nchar(outputMsgs$loginErrorMsg) == 0) {
          return("Don't have an Independent Impact account yet? Don't worry! Click on 'Register' below to create one now.")
        }
        outputMsgs$loginErrorMsg 
      })
      
      # ------------------------------------------------------------------------
      
      rvOther$init <- Sys.time()
      
      rvOther$uiMode <- "logIn"
      
      observe({
        rvToReturn$allResultsGood <- all(unlist(reactiveValuesToList(resultsGood)))
      })
      
      return(rvToReturn)
    })
}
