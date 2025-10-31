getOpenSslDocSigKeyPair <- function(idAgent = NULL, did = NULL, dbCon) {
  
  if (length(idAgent) == 0 & length(did) == 0) {
    stop("Must provide either 'idAgent' or 'did'.")
  }
  
  if (length(did) != 0 & length(idAgent) != 0) {
    
    # Make sure that the provided combination match what we have on record.
    
    dfCheck <- dbGetQuery(
      conn = dbCon, 
      statement = sprintf(
        "SELECT id_agent, did FROM tbl_link_agents_x_dids WHERE id_agent = '%s' OR did = '%s';",
        idAgent, did))
    
    # Check 1.
    bOk <- (nrow(dfCheck) == 1)
    
    # Check 2.
    if (bOk) {
      bOk <- (dfCheck$id_agent == idAgent & dfCheck$did == did)
    }
    
    if (!bOk) {
      stop(
        sprintf(
          "Provided did (%s...%s) does not match what we have on record for agent %s.",
          substr(x = did, start = 1, stop = 10), 
          substr(x = did, start = nchar(did) - 10, stop = nchar(did)),
          idAgent))
    }
    
  }
  
  if (length(did) == 0) {
    did <- dbGetQuery(
      conn = dbCon, 
      statement = sprintf(
        "SELECT did FROM tbl_link_agents_x_dids WHERE id_agent = '%s';",
        idAgent))[["did"]]
  }
  
  if (length(idAgent) == 0) {
    idAgent <- dbGetQuery(
      conn = dbCon, 
      statement = sprintf(
        "SELECT id_agent FROM tbl_link_agents_x_dids WHERE did = '%s';",
        did))[["id_agent"]]
  }
  
  didTopicId <- dbGetQuery(
    conn = dbCon, 
    statement = sprintf(
      "SELECT id_topic_h FROM tbl_link_entities_x_hedera_topics WHERE id_entity = '%s' AND label_topic_h = 'DID';",
      idAgent))[["id_topic_h"]]
  
  keyPairId <- dbGetQuery(
    conn = dbCon, 
    statement = sprintf(
      "SELECT id_key_pair FROM tbl_link_hedera_topics_x_key_pairs WHERE id_topic_h = '%s' AND label_key_pair = 'ADMIN';",
      didTopicId))[["id_key_pair"]]
  
  dfKeyPair <- dbGetQuery(
    con = dbCon, 
    statement = sprintf(
      "SELECT * FROM tbl_key_pairs WHERE id = '%s';",
      keyPairId))
  
  pubKeyAsStr <- dfKeyPair$public_key
  privKeyAsStr <- cyphr::decrypt_string(
    data = openssl::base64_decode(dfKeyPair$private_key_encr),
    key = cyphr::keypair_openssl(
      pub = iwefdj$KEYPTH_CYPHR,
      key = iwefdj$KEYPTH_CYPHR,
      envelope = TRUE,
      password = iwefdj$PW_CYPHR))
  
  # Construct openssl key objects.
  {
    # Note: privKeyAsStr is a DER string, not simple hex, so we need to convert 
    # it to a hiero$PrivateKey object first so that we can call its to_string 
    # function which will give us the simple hex, which we can then use with 
    # sodium and openssl.
    sk <- hiero$PrivateKey$from_string(privKeyAsStr) 
    sk <- openssl::read_ed25519_key(sodium::hex2bin(sk$to_string()))
    pk <- openssl::read_ed25519_pubkey(sodium::hex2bin(pubKeyAsStr))
  }
  
  return(list(
    did = did,
    private = list(asTxt = privKeyAsStr, asKey = sk), 
    public = list(asTxt = pubKeyAsStr, asKey = pk)))
}