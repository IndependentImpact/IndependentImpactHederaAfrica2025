# Compiles a JWKS JSON object for a specific agent or DID, so that users can 
# verify that a document was truly signed with the key belonging to that (agent's) DID.
# NB: This function makes the following assumptions:
# (a) Each agent can only have one DID.
# (b) Each DID is linked to only one 'DID'-labelled Hedera topic.
# (c) The ADMIN key pair of a DID topic is the one that's used for signing 
#       documents.
compileJwksJson <- function(idAgent = NULL, did = NULL, dbCon) {

  # Get the document signing key pair for this DID.
  keys <- getOpenSslDocSigKeyPair(
    idAgent = idAgent, 
    did = did, 
    dbCon = dbCon)

  # Build a JWK (OKP/Ed25519) you can expose via /.well-known/jwks.json
  jwk <- list(
    kty = "OKP", crv = "Ed25519",
    kid = paste0(keys$did, "#doc-sign-key-1"), # # kid here must be exactly
    # the same as used in f 'signRegularDoc'.
    x   = jose::base64url_encode(sodium::hex2bin(keys$public$asTxt)) # public part
    # (Private "d" MUST NOT be in your public JWKS; if you need a local JWK:)
    # d = base64url_encode(hex2bin(privKeyAsStr))
  )
  jwks <- list(keys = list(jwk))
  #cat(jsonlite::toJSON(jwks, auto_unbox = TRUE, digits = NA))
  # -> publish this JSON at your JWKS endpoint 
  
  return(jsonlite::toJSON(jwks, auto_unbox = TRUE, digits = NA))
}
