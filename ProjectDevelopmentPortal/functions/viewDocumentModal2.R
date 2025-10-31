
#' @title viewDocumentModal2
#' @note Params 'idDoc', 'tagBlockPost', 'idMsgPolicy', 'idBlockRetr', 
#'    'dbCon' and 'accTknGsb' are ignored if 'ipfsUrlDoc' is not NULL.
#' @param ipfsUrlDoc Character. The full IPFS URL of the document to be shown to
#'    the user. Not required if 'idDoc', 'tagBlockPost', 'idMsgPolicy' 
#'    and 'accTknGsb' are provided.
#' @param ipfsUrlSch Character. The full IPFS URL of the Guardian schema 
#'    definition for the document to be shown to the user. Not required if
#'    idMsgSch is provided.
#' @param idMsgSch Character. The message ID of the Guardian schema definition
#'    for the document to be shown to the user. Will be ignored if 'ipfsUrlSch' 
#'    is provided.
#' @param idDoc Character. Our db ID for the document in question. 
#'    Ignored if 'ipfsUrlDoc' is provided; required otherwise.
#' @param tagBlockPost Characer. The Guardian policy block tag for the block to 
#'    which the document in question was submitted to. Ignored when 'ipfsUrlDoc'
#'    is provided; required otherwise.
#' @param idMsgPolicy Character. The message ID of the Guardian policy to which 
#'    the document in question was submitted. Ignored if 'ipfsUrlDoc' is provided;
#'    required otherwise.
#' @param idBlockRetr Character. Optional. The Guardian ID of the policy block 
#'    from which the document must be retrieved. Will be ignored if 'ipfsUrlDoc' 
#'    is provided. 
#' @param dbCon Postgres database connection. 
#' 
viewDocumentModal2 <- function(ipfsUrlDoc = NULL,
                               ipfsUrlSch = NULL,
                               idMsgSch = NULL,
                               idDoc = NULL, # dfDocMd$id
                               tagBlockPost = NULL, # dfDocMd$step_workflow
                               idMsgPolicy = NULL, # dfDocMd$id_msg_pol
                               idBlockRetr = NULL,
                               dbCon) {
  
  # Input checking.
  {
    ipfsUrlDoc <- jellyfi3shR::emptyOrMissingAsNull(ipfsUrlDoc, ignoreSpaces = TRUE)
    idMsgSch <- jellyfi3shR::emptyOrMissingAsNull(idMsgSch, ignoreSpaces = TRUE)
    ipfsUrlSch <- jellyfi3shR::emptyOrMissingAsNull(ipfsUrlSch, ignoreSpaces = TRUE)
    idDoc <- jellyfi3shR::emptyOrMissingAsNull(idDoc, ignoreSpaces = TRUE)
    tagBlockPost <- jellyfi3shR::emptyOrMissingAsNull(tagBlockPost, ignoreSpaces = TRUE)
    idMsgPolicy <- jellyfi3shR::emptyOrMissingAsNull(idMsgPolicy, ignoreSpaces = TRUE)
    idBlockRetr <- jellyfi3shR::emptyOrMissingAsNull(idBlockRetr, ignoreSpaces = TRUE)
    
    if (length(ipfsUrlSch) == 0 & length(idMsgSch) == 0) {
      stop("Must provide either 'ipfsUrlSch' or 'idMsgSch'.")
    }
    
    if ((length(idDoc) == 0 | (length(tagBlockPost) == 0) | (length(idMsgPolicy) == 0) | (length(accTknGsb) == 0) | (length(dbCon) == 0)) & 
        length(ipfsUrlDoc) == 0) {
      stop("Must provide idDoc, tagBlockPost, idMsgPolicy, dbCon and accTknGsb when not providing ipfsUrlDoc.")
    }
    
  }
  
  tryCatch({
    
    # Retrieve the document.
    if (length(ipfsUrlDoc) == 1) { # Retrieve the document directly from IPFS.
      
      # Get the IPFS URL in the correct format.
      ipfsUrlDoc <- ipfsUriToUrl(ipfsUrlDoc)
      
      # Retrieve.
      doc <- httr::GET(url = ipfsUrlDoc)
      if (doc$status_code < 200 | doc$status_code >= 400) {
        stop(sprintf("Failed to retrieve document from IPFS: %s %s",
                     doc$status_code, content(x = doc, as = "parsed")))
      }
      doc <- httr::content(x = doc, as = "parsed")
      
    } else { # Retrieve the document via its containing Guardian block.
      
      # Remember: IF contentOnly = TRUE, getPubDoc() returns a LIST of length = the 
      # number of VCs in the document.
      doc <- getPubDoc(docId = idDoc,
                       dbCon = dbCon,
                       decrypt = TRUE,
                       contentOnly = FALSE)
    }
    
    # Get the schema definition and metadata.
    {
      # Retrieve 'ipfsUrlSch' from the db if only 'idMsgSch' were provided. 
      if (length(ipfsUrlSch) == 0) {
        q <- sprintf("SELECT url_ipfs FROM tbl_schemas WHERE id_message = '%s';",
                     idMsgSch)
        ipfsUrlSch <- dbGetQuery(conn = dbCon, statement = q)[["url_ipfs"]]
      }
      
      # Make sure 'ipfsUrlSch' is in the correct format.
      ipfsUrlSch <- ipfsUriToUrl(ipfsUrlSch)
      
      
      # Get def and md.
      sch <- httr::GET(url = ipfsUrlSch)
      sch <- content(x = sch, as = "parsed")
      sch <- schemaToDf(schema = sch, inclMd = TRUE)
    }
    
    # Construct the HTML.
    {
      txt <- .vcToHtml(
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
    message("ERROR: viewDocumentModal2(): ", e)
    return(
      modalDialog("Oops! Something went wrong. Please try again later. If the problem persists, please contact the application administrator.",
                  footer = NULL, 
                  size = "m", 
                  easyClose = TRUE))
  })
}
