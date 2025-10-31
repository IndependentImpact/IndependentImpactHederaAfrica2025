makeLinkHtmlForDocumentsGrid <- function(documentIds, linkText, linkType = "view") {
  
  if (length(documentIds) > 1) {
    if (length(linkText) > 1) {
      if (length(linkText) != length(documentIds)) {
        stop("length(linkText) != length(documentIds)")
      }
    }
  }
  
  return(
    sprintf('<a id="al%s%s" href="#" class="action-button">%s</a>',
            stringr::str_to_title(linkType),
            documentIds,
            linkText))
}