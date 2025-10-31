getDocumentIdFromButtonHtml <- function(
    htmlTxt, 
    buttonType = c('view', 'edit', 'review', 'trustchain')[1]) {
  
  # TODO. Refactor this. We can do this so much better.
  # BUT NB: Remember, we must be able to accommodate any kind of button IDs - integers, characters, alnum, etc.
  
  label <- stringr::str_to_title(buttonType)
  
  buttonId <- gsub(pattern = sprintf('" type="button" class="btn btn-default action-button">%s</button>', label), 
                   replacement = "", 
                   x = htmlTxt, 
                   fixed = TRUE)
  buttonId <- gsub(pattern = sprintf('<button id="ab%s', label), 
                   replacement = "", 
                   x = buttonId, 
                   fixed = TRUE)
  
  return(buttonId)
}