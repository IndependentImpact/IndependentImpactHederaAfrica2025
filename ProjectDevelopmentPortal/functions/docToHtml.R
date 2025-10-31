docToHtml <- function(docAsList, 
                      dfSchDef = NULL,
                      schNm = NULL,
                      schV = NULL,
                      schDescr = NULL) {
  
  # If it is a VC, use our VC helper.
  if (all(c("@context", "credentialSubject", "issuer", "proof") %in% names(docAsList))) {
    return(.vcToHtml(
      docAsList = docAsList, 
      dfSchDef = dfSchDef, 
      schNm = schNm, 
      schV = schV, 
      schDescr = schDescr))
  }
  
  # If it is not a VC or a signed document based on a known schema, use our 
  # generic helper.
  if ((length(dfSchDef) == 0 & 
      length(schNm) == 0 & 
      length(schV) == 0 & 
      length(schDescr) == 0) |
      (length(intersect(c("document", "signature"), names(docAsList))) != 2)) {
    return(.genericDocToHtml(x = docAsList))
  }

  # Reaching this point means it is a regular, signed document, that was based on
  # some schema known to us.
  
  doc <- docAsList$document
  sig <- docAsList$signature
  
  txt <- c("<h2>Document Content</h2>")
  
  if (schNm == "Generic Document Review") {
    
    txt <- c(txt, sprintf("<h3>%s (GDR v%s)</h3>", "Document Review", schV))
    
    txt <- c(txt, 
             .schemaDocToHtml(doc = list(headers = doc$headers), 
                              dfSchDef = dfSchDef[which(dfSchDef$key == "headers"),]))
    
    txt <- c(txt,
             .reviewDocToHtml(doc = doc))
    
    txt <- c(txt, 
             .schemaDocToHtml(doc = list(final_rd = doc$final_rd), 
                              dfSchDef = dfSchDef[which(dfSchDef$key == "final_rd"),]))
  } else {
    
    txt <- c(txt,
             sprintf("<h3>%s (v%s)</h3>", schNm, schV),
             sprintf("%s <br /><br />", schDescr))
    
    txt <- c(txt, 
             .schemaDocToHtml(doc = doc, 
                              dfSchDef = dfSchDef))
  }

  txt <- c(txt, "<h2>Document Signature</h2>", sig)
  
  txt <- paste(txt, sep = "", collapse = "")
  
  txt <- sprintf(
  '
<html lang="en">
<head>
  <link rel="stylesheet" href="%sdocument-print-formats/style.css">
</head>
<body>
  %s
<body>
</html>

',
  appdir, txt)
  
  #cat(txt, file = sprintf("%stest.html", tmpdir))
  return(txt)
  
}

.docFieldToHtml <- function(title, descr, x) {
  
  # Convert to HTML.
  return(
    sprintf('
  <p>
  <strong>%s</strong><br />
  <em>%s</em><br />
  %s
  </p>
  ',
            title,
            descr,
            x))
}
