
.vcToHtml <- function(docAsList, 
                      dfSchDef,
                      schNm,
                      schV,
                      schDescr) {
  
  idx <- which(names(docAsList) == "credentialSubject")
  
  # Some VCs from other architectures have the credentialSubject portion of 
  # the VC as an array of credential sets, i.e., a list of lists. Even in such 
  # cases, though, the credentialSubject array rarely contains more than one set 
  # of credentials. Our own VCs will (likely) only ever contain one set of 
  # credentials per VC, so we are not going to break our backs (yet) to 
  # accommodate VCs from other architectures. So for now, we'll do the following:
  # if credentialSubject is a named list, we'll assume that the VC contains 
  # only one set of credentials and those credentials were not wrapped in an 
  # array. If the credentialSubject is an unnamed list of length = 1, we'll 
  # also assume that the VC contains only one set of credentials, but that that
  # set was wrapped in an array, so we'll just extract the set as item [[1]]
  # from the list. If, however, it appears that there might be multiple sets of 
  # credentials in the VC, we'll simply issue a warning and then just continue
  # with only the first set anyway.
  if (length(names(docAsList[[idx]])) > 0){
    doc <- docAsList[[idx]]
  } else {
    if (length(docAsList[[idx]]) > 1) {
      message("WARNING: Multiple credential subjects detected. Only the first will be used.")
      doc <- docAsList[[idx]][[1]]
    }
    doc <- docAsList[[idx]][[1]]
  }
  
  docMd <- docAsList[-idx]

  txt <- c(
    sprintf('
<h2>Document Metadata</h2>
<p>
<strong>Document ID: </strong>%s<br />
<strong>Document Type: </strong>%s <br />
<strong>Issuer: </strong>%s <br />
<strong>Date of Issuance: </strong>%s <br />
<strong>Contexts: </strong><br />
%s
</p>',
            docMd$id,
            docMd$type[[1]],
            docMd$issuer,
            docMd$issuanceDate,
            paste(
              sprintf(
                "  [%d] %s", 
                1:length(docMd[["@context"]]), 
                unlist(docMd[["@context"]])), 
              sep = "", 
              collapse = "<br />")),
    
    sprintf('
<h3>VC Proof</h3>
<p>
<strong>Type: </strong>%s <br />
<strong>Created: </strong>%s <br />
<strong>Verification Method: </strong>%s <br />
<strong>Proof Purpose: </strong>%s <br />
<strong>JWS: </strong>%s
</p>',
            docMd$proof$type,
            docMd$proof$created,
            docMd$proof$verificationMethod,
            docMd$proof$proofPurpose,
            docMd$proof$jws
            #sprintf('<span style="width:700px; word-wrap:break-word; display:inline-block;">%s</span>', docMd$proof$jws)
            #shortText(x = docMd$proof$jws, lim = 100)
    ),
    
    sprintf("<h2>Document Content</h2>"))
  
  txt <- c(txt,
           sprintf("<h3>%s (v%s)</h3>", schNm, schV),
           sprintf("%s <br /><br />", schDescr))
  
  txt <- c(txt, 
           .schemaDocToHtml(doc = doc, 
                            dfSchDef = dfSchDef))
  
  txt <- paste(txt, sep = "", collapse = "")
  
  txt <- sprintf('
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







