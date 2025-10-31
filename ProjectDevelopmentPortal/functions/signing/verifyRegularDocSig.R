# Verifies the JWS signature of a JSON document signed via our app.
verifyRegularDocSig <- function(doc, sig) {
  
  parts <- strsplit(sig, "\\.", fixed = FALSE)[[1]]
  stopifnot(length(parts) == 3)
  
  sigHeader <- jsonlite::fromJSON(
    txt = rawToChar(jose::base64url_decode(parts[1])), 
    simplifyVector = TRUE)
  stopifnot(sigHeader$alg == "EdDSA")  

  keys <- getOpenSslDocSigKeyPair(
    did = gsub(
      pattern = "#doc-sign-key-1", 
      replacement = "", 
      x = sigHeader$kid, 
      fixed = TRUE), 
    dbCon = dbCon)

  signingInput <- paste(parts[1], parts[2], sep = ".")
  bOk <- openssl::ed25519_verify(
    data = charToRaw(signingInput),
    sig = jose::base64url_decode(parts[3]),
    pubkey = keys$public$asKey)
  
  return(bOk)
  
}