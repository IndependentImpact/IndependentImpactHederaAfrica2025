makeButtonHtmlForDocumentsGrid <- function(
    documentIds, 
    buttonType = c('view', 'edit', 'review', 'trustchain')[1]) {
  
  label <- stringr::str_to_title(buttonType)
  
  return(sprintf('<button id="ab%s%d" type="button" class="btn btn-default action-button">%s</button>',
                 label, documentIds, label))
}