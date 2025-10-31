
#' @title viewDocumentModal3
#' @param idDoc Character. Our db ID for the document in question. 
#'    Ignored if 'ipfsUrlDoc' is provided; required otherwise.
#' @param dbCon Postgres database connection. 
#' 
viewDocumentModal3 <- function(idDoc, dbCon, contentOnly = FALSE) {
  
  tryCatch({
    
    # Retrieve the document.
    { 
      # Remember: IF contentOnly = TRUE, getPubDoc() returns a LIST of length = the 
      # number of VCs in the document (if the document is a VC).
      doc <- getPubDoc(docId = idDoc,
                       dbCon = dbCon,
                       decrypt = TRUE,
                       contentOnly = contentOnly)
    }
    
    # Get the schema definition and metadata.
    {
      q <- sprintf(
        "SELECT id_schema FROM tbl_document_metadata WHERE id = '%s';",
        idDoc)
      idSchema <- dbGetQuery(conn = dbCon, statement = q)[["id_schema"]]
      
      q <- sprintf(
        "SELECT uri_ipfs FROM tbl_schemas WHERE id = '%s';", 
        idSchema)
      ipfsUrlSch <- dbGetQuery(conn = dbCon, statement = q)[["uri_ipfs"]]

      # Make sure 'ipfsUrlSch' is in the correct format.
      ipfsUrlSch <- ipfsUriToUrl(ipfsUrlSch)
      
      # Get def and md.
      sch <- httr::GET(url = ipfsUrlSch)
      sch <- content(x = sch, as = "parsed")
      sch <- schemaToDf(schema = sch, inclMd = TRUE)
    }
    
    # Construct the HTML.
    {
      txt <- docToHtml(
        docAsList = doc,
        dfSchDef = sch$dfFields,
        schNm = sch$dfMd$title,
        schV = sch$dfMd$version,
        schDescr = sch$dfMd$descr)
    }
    
    # Create and return the doc modal.
    return(
      modalDialog(HTML(txt),
                  footer = NULL, 
                  size = "l", 
                  easyClose = TRUE))
    
  }, error = function(e) {
    message("ERROR: viewDocumentModal3(): ", e)
    return(
      modalDialog("Oops! Something went wrong. Please try again later. If the problem persists, please contact the application administrator.",
                  footer = NULL, 
                  size = "m", 
                  easyClose = TRUE))
  })
}
