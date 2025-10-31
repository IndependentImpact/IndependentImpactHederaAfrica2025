
verifyPersonalsUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    helpText("Please upload a photo of your face next to some official documentation that states at least your surname and date of birth."),
    fileInput(ns("fiPersonals"), 
              multiple = FALSE, 
              label = "Upload file:", 
              buttonLabel = "OK", 
              accept = c(".jpeg", ".jpg", ".PNG", ".png")),
    # TODO: use fileInput's 'capture' parameter to use webcam.
    actionButton(ns("abOK"), "OK"),
    textOutput(ns("toResultVerification")),
    actionButton(ns("abSkip"), label = "Skip this step"),
    textOutput(ns("toSkipWarning")),
    actionButton(ns("abConfirmSkip"), label = "Yes, I'm sure.")
  )
}

verifyPersonalsServer <- function(id) {
  
  moduleServer(
    id = id, 
    function(input, output, session) {
      
      rvToReturn <- reactiveValues()
      rvToReturn$goToModule <- NULL
      
      return(rvToReturn)
      
    })
}