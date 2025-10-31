
verifyContactNumUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    textOutput(ns("toCodeSent")),
    textInput(ns("tiCode"), label = "Verification code:"),
    actionButton(ns("abOK"), "OK"),
    textOutput(ns("toResultVerification")),
    helpText("Didn't receive the message?"),
    actionButton(ns("abResendMsg"), label = "Resend message"),
    actionButton(ns("abSkip"), label = "Skip this step"),
    textOutput(ns("toSkipWarning")),
    actionButton(ns("abConfirmSkip"), label = "Yes, I'm sure.")
  )
}

verifyContactNumServer <- function(id) {
  
  moduleServer(
    id = id, 
    function(input, output, session) {
      
      rvToReturn <- reactiveValues()
      rvToReturn$goToModule <- NULL
      
      return(rvToReturn)
      
    })
}