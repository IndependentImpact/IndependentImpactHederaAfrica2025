#'@description Signs a JSON document. 
#'@param doc List. The JSON document content to be signed.
#
signRegularDoc <- function(doc, idAgent, dbCon) {
  
  # Get the DID and key pair for this agent.
  keys <- getOpenSslDocSigKeyPair(idAgent = idAgent, dbCon = dbCon)
  
  # Construct signature header, as per RFC 7515 + RFC 8037.
  {
    sigHeader <- list(
      alg = "EdDSA", 
      kid = paste0(keys$did, "#doc-sign-key-1")) 
    # < kid here must be exactly the same as used in f 'compileJwksJson'.
    sigHeaderB64 <- jose::base64url_encode(
      charToRaw(
        jsonlite::toJSON(
          x = sigHeader, 
          auto_unbox = TRUE, 
          digits = NA)))
  }
  
  # Prepare payload: base64url(EXACT bytes of your JSON).
  {
    payloadRaw <- charToRaw(
      enc2utf8(
        jsonlite::toJSON(
          x = doc, 
          dataframe = "rows", 
          matrix = "rowmajor", 
          Date = "ISO8601", 
          POSIXt = "string", 
          factor = "string", 
          complex = "list", 
          auto_unbox = TRUE, 
          pretty = FALSE)))
    payloadB64 <- jose::base64url_encode(payloadRaw)
  }
  
  # Sign.
  input <- paste0(sigHeaderB64, ".", payloadB64)
  sigRaw <- openssl::ed25519_sign(
    data = charToRaw(input), 
    key = keys$private$asKey)
  sigB64 <- jose::base64url_encode(sigRaw)
  
  # Compact.
  jws_compact <- paste(sigHeaderB64, payloadB64, sigB64, sep = ".")
  
  return(jws_compact)
}
