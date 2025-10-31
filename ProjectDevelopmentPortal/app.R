
source("global.R")

ui <- fluidPage(

  theme = shinythemes::shinytheme("sandstone"),

  includeCSS(sprintf("%swrap_long_strings.css", cstmcssdir)),

  #shinythemes::themeSelector(),

  useShinyjs(),

  uiOutput(outputId = "uioMain")

)

server <- function(input, output, session) {

  rv <- reactiveValues()
  rv$userCurr <- reactiveValues(
    email_address = NULL, # The email address with which the agent logged in for this session.
    id_agent = NULL, # Our db ID for the user.
    userType = NULL,
    did = NULL #,
    #refreshToken = NULL
    )


  log_appender(appender_file(paste0("logs/independent-impacts-", novaUtils::dateAsName() ,".log")))

  resultsGood <- reactiveValues()
  resultsGood$email_address <- FALSE
  resultsGood$id_agent <- FALSE
  resultsGood$userType <- FALSE
  resultsGood$did <- FALSE
  #resultsGood$refreshToken <- FALSE

  rvOther <- reactiveValues()
  rvOther$uiMode <- NULL
  rvOther$nonce <- 0
  rvOther$start <- NULL

  # module servers -------------------------------------------------------------

  modSrvrs <- reactiveValues()
  modSrvrs$modUserLogIn <- NULL
  modSrvrs$modGeneral <- NULL

  # inputs ---------------------------------------------------------------------

#alert(Sys.info()[["user"]])

  observeEvent(rvOther$uiMode, handlerExpr = {

    validate(need(rvOther$start, message = FALSE))
    validate(need(rvOther$uiMode, message = FALSE))

    if (rvOther$uiMode == "logIn") {

      modSrvrs$modGeneral <- NULL

      # Render the login module for the user.
      modSrvrs$modUserLogIn <- userLogInServer(
        id = sprintf("modUserLogIn%d", rvOther$nonce))

      return(NULL)
    }

    if (rvOther$uiMode == "general") {

      modSrvrs$modUserLogIn <- NULL

      modSrvrs$modGeneral <- generalServer(
        id = sprintf("modGeneral%d", rvOther$nonce),
        loginInfoUsr = reactiveValuesToList(rv$userCurr))

      return(NULL)
    }

    warning("Invalid value encountered for rvOther$uiMode.")
  })

  # modSrvrs$modUserLogIn$allResultsGood
  observe({

    validate(need(isolate(rvOther$start), message = FALSE))
    validate(need(rvOther$uiMode, message = FALSE))
    validate(need(modSrvrs$modUserLogIn$allResultsGood, message = FALSE))

    if (rvOther$uiMode == "logIn") {

      # if (length(modSrvrs$modUserLogIn) > 0) {
      #   if ("allResultsGood" %in% names(modSrvrs$modUserLogIn)){
      if (modSrvrs$modUserLogIn$allResultsGood) {

        rv$userCurr$id_agent <- modSrvrs$modUserLogIn$results$id_agent
        rv$userCurr$email_address <- modSrvrs$modUserLogIn$results$email_address
        rv$userCurr$userType <- modSrvrs$modUserLogIn$results$userType
        rv$userCurr$did <- modSrvrs$modUserLogIn$results$did
        #rv$userCurr$refreshToken <- modSrvrs$modUserLogIn$results$refreshToken

        rvOther$uiMode <- "general"

      }
      #}
      #}
    }
  })

  # modSrvrs$modGeneral$done
  observe({

    validate(need(isolate(rvOther$start), message = FALSE))
    validate(need(rvOther$uiMode, message = FALSE))
    validate(need(modSrvrs$modGeneral$done, message = FALSE))
    if (length(modSrvrs$modGeneral$done) == 0) { return(invisible(NULL)) }

    if (rvOther$uiMode == "general") {
      if (modSrvrs$modGeneral$done) {
        rv$userCurr$id_agent <- NULL
        rv$userCurr$email_address <- NULL
        rv$userCurr$userType <- NULL
        rv$userCurr$did <- NULL
        #rv$userCurr$refreshToken <- NULL
        
        rvOther$nonce <- rvOther$nonce + 1
        rvOther$uiMode <- "logIn"
      }
    }

  })

  # outputs --------------------------------------------------------------------

  output$uioMain <- renderUI({

    validate(need(rvOther$uiMode, message = FALSE))

    if (rvOther$uiMode == "logIn") {
      return(
        userLogInUI(id = sprintf("modUserLogIn%d", rvOther$nonce)))
    }

    if (rvOther$uiMode == "general") {
      validate(need(rv$userCurr$userType, message = FALSE))
      return(
        generalUI(id = sprintf("modGeneral%d", rvOther$nonce),
                  userType = rv$userCurr$userType))
    }

    warning("Invalid value encountered for rvOther$uiMode.")
  })

  # ----------------------------------------------------------------------------
  rvOther$start <- Sys.time()
  rvOther$uiMode <- "logIn"

}

shinyApp(ui = ui, server = server)



