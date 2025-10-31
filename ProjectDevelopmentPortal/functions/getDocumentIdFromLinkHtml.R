getDocumentIdFromLinkHtml <- function(htmlTxt, linkType = "view") {
  splts <- strsplit(x = htmlTxt, split = "[[:blank:]]")
  splts <- splts[[1]]
  id <- splts[2]
  id <- gsub(
    pattern = sprintf('id="al%s', stringr::str_to_title(linkType)), 
    replacement = "", 
    x = id, 
    fixed = TRUE)
  id <- substr(x = id, start = 1, stop = nchar(id)-1)
  return(id)
}