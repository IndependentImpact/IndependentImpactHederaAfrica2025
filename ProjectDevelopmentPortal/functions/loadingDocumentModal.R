loadingDocumentModal <- function() {
  return(
    modalDialog(
      title = "Retrieving document...", 
      "Please be patient.",
      footer = NULL, 
      size = "s", 
      easyClose = FALSE))
}