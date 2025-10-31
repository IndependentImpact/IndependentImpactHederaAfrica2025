#'@param content List. That which should go into the 'credentialSubject' portion,
#'  of the VC.
createVerifCred <- function(content, idIssuer, idSubject, dbCon, 
                            vcType = c("jws", "jwt")[1]) {
  
  # Banana proofing.
  if (idIssuer == idSubject) {
    stop("Issuer and subject cannot be the same entity.")
  }
  if (vcType != "jwt" & vcType != "jws") {
    stop("vcType must be either 'jwt' or 'jws'.")
  }
  
  # Get the DIDs of the issuer and the subject.
  {
    q <- sprintf("SELECT id_agent, did FROM tbl_link_agents_x_dids WHERE id_agent IN(%s);",
                 paste(sprintf("'%s'", c(idIssuer, idSubject)), 
                       sep = "", collapse = ","))
    dfDids <- dbGetQuery(conn = dbCon, statement = q)
    
    if (!(idIssuer %in% dfDids$id_agent)) {
      stop(sprintf(
        "Failed to retrieve DID for entity %s.",
        idIssuer))
    }
    didIssuer <- dfDids$did[which(dfDids$id_agent == idIssuer)]
    
    if (!(idSubject %in% dfDids$id_agent)) {
      
      # This means we are probably dealing with a project as the subject, so 
      # let's check.
      q <- sprintf("SELECT title FROM tbl_projects WHERE id = '%s';", idSubject)
      if (nrow(dbGetQuery(conn = dbCon, statement = q)) != 1) {
        stop(sprintf(
          "Failed to retrieve DID for entity %s.",
          idSubject))
      }
      
      # If it is a project, use its Hedera topic ID for now.
      dfTopic <- dbGetQuery(
        conn = dbCon, 
        statement = sprintf(
          "SELECT * FROM tbl_link_entities_x_hedera_topics WHERE id_entity = '%s';",
          idSubject))
      didSubject <- dfTopic$id_topic_h
      
    } else {
      didSubject <- dfDids$did[which(dfDids$id_agent == idSubject)]
    }
  }
  
  # Get the signing key pair belonging to the issuer.
  keys <- getOpenSslDocSigKeyPair(
    idAgent = idIssuer, 
    did = didIssuer, 
    dbCon = dbCon)
  
  # Construct the VC payload.
  {
    vcUuid <- paste0("urn:uuid:", uuid::UUIDgenerate())
    ctx <- c("https://www.w3.org/ns/credentials/v2")
    credSubj <- c(list(id = didSubject), content)
    type2 <- c("VerifiableCredential") # c("VerifiableCredential", "AgentProfile")
    dtIssuance <- lubridate::now(tzone = "UTC")  
    
    if (vcType == "jwt") {
      
      now  <- as.integer(as.numeric(dtIssuance))
      exp  <- now + 365*24*3600   # 1-year expiry # TODO.
      
      payload <- list(
        iss = didIssuer,
        sub = didSubject,
        iat = now,
        nbf = now,
        exp = exp,
        jti = vcUuid,
        vc = list(
          `@context` = ctx,
          type = type2,
          credentialSubject = credSubj))
      
    } else {
      
      payload <- list(
        "@context" = ctx,
        id = vcUuid,
        type = type2,
        issuer = didIssuer,
        issuanceDate = strftime(
          x = dtIssuance, 
          format = "%Y-%m-%dT%H:%M:%OSZ", 
          tz = "UTC"),
        credentialSubject = credSubj)
      
      payload <- .canoniseList(payload)
      
    }
    
    payloadJson <- jsonlite::toJSON(
      x = payload, 
      pretty = FALSE,
      auto_unbox = TRUE, 
      digits = NA)
    
    payloadB64 <- jose::base64url_encode(charToRaw(payloadJson))
  }
  
  # Construct the JOSE header.
  {
    if (vcType == "jwt") {
      header <- list(
        alg = "EdDSA", 
        kid =  paste0(keys$did, "#doc-sign-key-1"), 
        typ = "JWT")
    } else {
      header <- list(
        alg = "EdDSA", 
        kid = paste0(keys$did, "#doc-sign-key-1"))
    }
    
    headerJson <- jsonlite::toJSON(
      x = header, 
      pretty = FALSE,
      auto_unbox = TRUE)
    
    headerB64 <- jose::base64url_encode(charToRaw(headerJson))
  }
  
  # Sign.
  {
    sigInput <- charToRaw(paste0(headerB64, ".", payloadB64))
    
    sigRaw <- openssl::ed25519_sign(
      data = sigInput, 
      key = keys$private$asKey) 
    
    sigB64 <- jose::base64url_encode(sigRaw)  
  }
  
  # Finish up.
  if (vcType == "jwt") {
    res <- paste(headerB64, payloadB64, sigB64, sep = ".")
  } else {
    res <- payload
    res$proof <- list(
      type = "Ed25519Signature2018",
      created = strftime(
        x = lubridate::now(tzone = "UTC"), 
        format = "%Y-%m-%dT%H:%M:%OSZ", 
        tz = "UTC"),
      verificationMethod = header$kid,
      proofPurpose = "assertionMethod",
      jws = paste(headerB64, "", sigB64, sep = ".") # header..signature
      )
  }
  
  # Done.
  return(res)
}

.canoniseList <- function(x) {
  
  if (is.list(x) && !is.null(names(x))) {
    x <- x[order(names(x))]
    for (nm in names(x)) x[[nm]] <- .canoniseList(x[[nm]])
    return(x)
  } 
  
  if (is.list(x)) {
    return(lapply(X = x, FUN = .canoniseList))
  } 
  
  return(x)
  
}

