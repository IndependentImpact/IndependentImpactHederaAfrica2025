
#' Params dfDocMD, idBlockRetr, accTknGsb and dbCon are ignored if uriIPFS is
#' not NULL.
#' @param idBlockRetr Can be NULL, even when dfDocMD is not NULL.
#' @param uriIPFS Character. Misnomer. The full IPFS URL of the document, not 
#'    just its IPFS URI.
viewDocumentModal <- function(dfDocMD = NULL, 
                              uriIPFS = NULL,
                              idBlockRetr = NULL,
                              accTknGsb = NULL, 
                              dbCon = NULL, 
                              contentOnly = FALSE) {

  # Input checking.
  if (length(dfDocMD) == 0 & length(uriIPFS) == 0) {
    stop("Must provide either 'dfDocMD' or 'uriIPFS'.")
  }

  # See if we can retrieve uriIPFS from dfDocMD, if uriIPFS is NULL.
  if (length(uriIPFS) == 0) {
    if ("url_ipfs" %in% names(dfDocMD)) {
      uriIPFS <- dfDocMD$url_ipfs
    }
  }
  
  # Get the IPFS URL in the correct format.
  if (length(grep(pattern = "http", x = uriIPFS, value = FALSE)) == 0) {
    uriIPFS <- gsub(pattern = "ipfs://", replacement = "", x = uriIPFS, fixed = TRUE)
    uriIPFS <- sprintf("https://%s.ipfs.w3s.link", uriIPFS)
  }
  
  # Retrieve the document.
  if (length(uriIPFS) == 1) { # Retrieve the document directly from IPFS.
    
    doc <- httr::GET(url = uriIPFS)
    doc <- content(x = doc, as = "parsed")
    if (contentOnly) {
      
      lsVcs <- list()
      
      # Proceed differently depending on whether the document is a VP or a VC.
      if ("verifiableCredential" %in% names(doc)) { # We are dealing with a VP.
        
        for (k in 1:length(doc$verifiableCredential)) {
          for (m in 1:length(doc$verifiableCredential[[k]]$credentialSubject)) {
            lsVcs[length(lsVcs) +1] <- doc$verifiableCredential[[k]]$credentialSubject[m]
          }
        }
        
      } else { # We are dealing with a VC.
        for (m in 1:length(doc$credentialSubject)) {
          lsVcs[length(lsVcs) +1] <- doc$credentialSubject[m]
        }
      }
      
      doc <- lsVcs; rm(lsVcs)
    }
    
  } else { # Retrieve the document via its containing Guardian block.
    
    # Remember: IF contentOnly = TRUE, getPubDoc() returns a LIST of length = the 
    # number of VCs in the document.
    doc <- getPubDoc(docId = dfDocMD$id,
                     dbCon = dbCon,
                     decrypt = TRUE,
                     contentOnly = contentOnly)
    
  }

  # If the user wants the document's metadata too, we are done here.
  if (!contentOnly) {
    return(
      modalDialog(#title = dfDocMD$id_g, 
                  HTML(.genericDocToHtml(x = doc, lv = 1)),
                  footer = NULL, 
                  size = "l", 
                  easyClose = TRUE))
  }
  
  # Reaching this point means the user wants only the document contents, not
  # the metadata, so strip the metadata.
  {
    doc <- lapply(X = doc, FUN = function(vc) {
      
      vc <- lapply(X = vc, FUN = function(x) {
        x <- truncTxt(x = x, nChar = 70)
        return(x)
      })
      
      vc <- sprintf("<b>%s</b><span>: %s</span>", 
                    names(vc), vc)
      vc <- paste(vc, collapse = "<br>")
      return(vc)
      
      # toHTML <- function(x) {
      #   
      #   idxx <- which(sapply(X = x, FUN = is.list))
      #   for (idx in idxx) {
      #     x[[idx]] <- toHTML(x[[idx]])
      #     x[[idx]] <- paste(x[[idx]], collapse = "<br>")
      #   }
      #   
      #   x <- sprintf("<b>%s</b><span>: %s</span>", 
      #                names(x), x)
      #   x <- paste(x, collapse = "<br>")
      #   return(x)
      #   
      # }
      
    })
    
    doc <- paste(sprintf("<h4>Credential Subject %d</h4>%s", 
                         1:length(doc), 
                         unlist(doc)), 
                 collapse = "<br>")
  }
  
  # Create and return the doc modal.
  return(
    modalDialog(title = ifelse(length(dfDocMD) > 0, dfDocMD$id_g, NULL), 
                HTML(doc),
                footer = NULL, 
                size = "l", 
                easyClose = TRUE))
}
