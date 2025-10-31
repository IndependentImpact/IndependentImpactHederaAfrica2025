# This is a run-once script. Only run this script once per Hedera network.

# 1. Set up a Python environment with the following installed:
#   pip install --upgrade pip setuptools wheel "protobuf>=5.27,<5.29" hiero-did-sdk-python hiero-sdk-python requests python-dotenv

# 2. Set Python path variable and Fluree API key in iwefdj or sys env.
PYTHON_PATH <- "" # In iwefdj
API_KEY_FLUREE <- "" # In sys env

# 3. Populate and source global.R. (Create a copy of 'template_globalR.', rename 
# it to 'global.R', and replace all occurrences of 'REPLACE_ME' in your 
# global.R.)
source("global.R")

# Initialise db connection.
dbCon <- getDbCon()

# Create core database tables.
{
  fps <- dir(tbldefdir, full.names = TRUE, recursive = FALSE)
  idxxIgnore <- grep(pattern = "_xxx", x = basename(fps))
  if (length(idxxIgnore) > 0) {
    fps <- fps[-idxxIgnore]
  }
  
  ctch <- sapply(X = fps, FUN = function(fp) {
    
    message(basename(fp))
    
    q <- readLines(con = fp)
    q <- paste(q, collapse = " ")
    res <- dbSendStatement(conn = dbCon, statement = q)
    dbClearResult(res)
    
  }); rm(ctch)
}

# Add the metadata of our workflows to the database.
{
  fps <- dir(
    path = wrkflwdir, 
    pattern = "workflow.json", 
    recursive = TRUE, 
    full.names = TRUE)
  
  ctch <- sapply(X = fps, FUN = function(fp) {
    
    workflow <- jsonlite::read_json(
      path = fp, 
      simplifyVector = TRUE, 
      simplifyDataFrame = FALSE, 
      flatten = FALSE)
    
    message(workflow$name)
    
    dfAdd <- data.frame(
      id = NA_character_,
      title = workflow$name, 
      tag_version = workflow$version,
      description = workflow$description,
      subject = ifelse(
        workflow$name == "Independent Impact - License Application Workflow", 
        "AGENT", 
        "ACTIVITY"),   
      handler_r = switch(
        workflow$name,
        "Independent Impact - License Application Workflow" = "indImpAgntLcnsngWrkflwHndlr1",
        "Independent Impact - Main Workflow" = "indImpMainWrkflwHndlr1",
        "Independent Impact - Monitoring Verification Workflow" = "indImpMrVrfctnWrkflwHndlr1",
        "Independent Impact - PDD Validation Workflow" = "indImpPddVldtnWrkflwHndlr1"), 
      status = "ACTIVE",
      id_topic_h = NA_character_, # TODO
      hash = digest::digest(
        workflow$config, 
        algo = "sha256", 
        serialize = TRUE),
      id_version_prev = NA_character_) 
    
    addToDb(
      dfAdd = dfAdd, 
      tblNm = "tbl_workflows", 
      vnmsChckEx = "hash", 
      dbCon = dbCon, 
      calcIds = TRUE)
    
    return(invisible(0))
    
  }); rm(ctch)
}

# Add the metadata of our schemas to the database.
{
  fps <- dir(
    path = schemadir, 
    recursive = FALSE, 
    full.names = TRUE)
  
  ctch <- sapply(X = fps, FUN = function(fp) {
    
    schema <- jsonlite::read_json(
      path = fp, 
      simplifyVector = TRUE, 
      simplifyDataFrame = FALSE, 
      flatten = FALSE)
    
    message(schema$name)
    
    dfAdd <- data.frame(
      id = NA_character_,
      title = schema$name,
      tag_version = schema$version,
      description = ifelse(nchar(schema$description) > 0, schema$description, NA_character_),
      status = "ACTIVE",
      iri = schema$iri,
      uri_ipfs = schema$documentURL,
      id_topic_h = NA_character_,
      handler_r = NA_character_,
      hash = digest::digest(
        schema$document, 
        algo = "sha256", 
        serialize = TRUE),
      id_version_prev = NA_character_,
      b_encrypt = schema$name %in% c("Agent Details (AD)"))
    
    addToDb(
      dfAdd = dfAdd, 
      tblNm = "tbl_schemas", 
      vnmsChckEx = "hash", 
      dbCon = dbCon, 
      calcIds = TRUE)
    
    return(0)
    
  }); rm(ctch)
}

# Create a Hedera operator account and key pair. This will be the account that 
# pays for all Hedera transactions. Store the account and key details in sys env
# or in iwefdj.
{
  operatorAccId <- hiero$AccountId$from_string(iwefdj$HEDERA_OPERATOR_ACCOUNT_ID_ED25519)
  operatorPrivKey <- hiero$PrivateKey$from_string(iwefdj$HEDERA_OPERATOR_ACCOUNT_PRIVATE_KEY_ED25519)
  operatorPubKey <- operatorPrivKey$public_key()
  
  operatorAccIdStr <- paste(
    operatorAccId$shard, 
    operatorAccId$realm, 
    operatorAccId$num, 
    sep = ".", 
    collapse = "")

  # Add this key pair to tbl_key_pairs.
  {
    encrPrivKey <- cyphr::encrypt_string(
      string = operatorPrivKey$to_string_der(), 
      key = cyphr::keypair_openssl(
        pub = iwefdj$KEYPTH_CYPHR, 
        key = iwefdj$KEYPTH_CYPHR, 
        envelope = TRUE,
        password = iwefdj$PW_CYPHR))
    encrPrivKey <- openssl::base64_encode(encrPrivKey)
    
    # decrRaw <- openssl::base64_decode(encrPrivKey)
    # decrChar <- cyphr::decrypt_string(
    #   data = decrRaw, 
    #   key = cyphr::keypair_openssl(
    #     pub = iwefdj$KEYPTH_CYPHR, 
    #     key = iwefdj$KEYPTH_CYPHR, 
    #     envelope = TRUE,
    #     password = iwefdj$PW_CYPHR))
    
    idUserKeyPair <- addToDb(
      dfAdd = data.frame(
        public_key = operatorPubKey$to_string(),
        private_key_encr = encrPrivKey), 
      tblNm = "tbl_key_pairs", 
      vnmsChckEx = "public_key", 
      dbCon = dbCon, 
      calcIds = TRUE, 
      returnIds = TRUE)
  }
}


# Set up Hedera client (we'll need it for the rest of the setup process)
{
  client <- hiero$Client(network = hiero$Network(hederaNetwork))
  client$set_operator(
    account_id = operatorAccId, 
    private_key = operatorPrivKey)
  
  # Check balance.
  balance_query <- hiero$CryptoGetAccountBalanceQuery(
    account_id = operatorAccId)
  balance <- balance_query$execute(client)
  print(paste("Balance (hbars):", balance$hbars))
}

# Optional: Create a treasury account. This will be the account that owns the
# definitions of our tokens. The Hedera operator account can be used as the
# treasury account if you want to.
treasAccId <- operatorAccId

# Create token definitions on Hedera.
if (FALSE) { # Our workflows no longer issue tokens, just VCs, so this is no 
  # longer need.
  
  # Create tokens.
  {
    # TODO. Perhaps let this flow read from the /tokens dir of each workflow's dir.
    
    lsTokens <- list(
      c(name = "Independent Impact Monitoring Verifier License",
        symbol = "II-L-MV"),
      c(name = "Independent Impact PDD Validator License",
        symbol = "II-L-PV"),
      c(name = "Independent Impact Project Developer License",
        symbol = "II-L-PD"),
      c(name = "Independent Impact Verified Impact Certificate",
        symbol = "II-C-VI"),
      c(name = "Independent Impact Monitoring Verification Certificate",
        symbol = "II-C-MR"),
      c(name = "Independent Impact PDD Validation Certificate",
        symbol = "II-C-PDD"))
    
    sapply(X = lsTokens, FUN = function(tkd) {
      
      message(tkd[["name"]])
      
      # Create and store a supply key for the token.
      {
        supplyKeyPriv <- hiero$PrivateKey$generate_ed25519()
        supplyKeyPub  <- supplyKeyPriv$public_key()
        
        encrPrivKey <- cyphr::encrypt_string(
          string = supplyKeyPriv$to_string_der(), 
          key = cyphr::keypair_openssl(
            pub = iwefdj$KEYPTH_CYPHR, 
            key = iwefdj$KEYPTH_CYPHR, 
            envelope = TRUE,
            password = iwefdj$PW_CYPHR))
        encrPrivKey <- openssl::base64_encode(encrPrivKey)
        
        # decrRaw <- openssl::base64_decode(encrPrivKey)
        # decrChar <- cyphr::decrypt_string(
        #   data = decrRaw, 
        #   key = cyphr::keypair_openssl(
        #     pub = iwefdj$KEYPTH_CYPHR, 
        #     key = iwefdj$KEYPTH_CYPHR, 
        #     envelope = TRUE,
        #     password = iwefdj$PW_CYPHR))
        
        tokenSupplyKeyPairId <- addToDb(
          dfAdd = data.frame(
            public_key = supplyKeyPub$to_string(),
            private_key_encr = encrPrivKey), 
          tblNm = "tbl_key_pairs", 
          vnmsChckEx = "public_key", 
          dbCon = dbCon, 
          calcIds = TRUE, 
          returnIds = TRUE)
      }
      
      # Submit token create tx to Hedera.
      {
        tx <- hiero$TokenCreateTransaction(
          token_name = tkd[["name"]], 
          token_symbol = tkd[["symbol"]], 
          decimals = as.integer(0), 
          initial_supply = as.integer(0), 
          #token_type =  # Must be NFT. TODO. Hiero SDK does not currently provide this option.
          treasury_account_id = treasAccId, 
          supply_key = supplyKeyPriv)
        tx <- tx$freeze_with(client)
        tx <- tx$sign(supplyKeyPriv)
        tx <- tx$sign(operatorPrivKey)
        resp <- tx$execute(client)
        
        tokenId <- paste(resp$tokenId$shard, resp$tokenId$realm, resp$tokenId$num, sep = ".")
      }
      
      # Add tbl_tokens db entry.
      {
        dfAdd <- data.frame(
          id_token_h = tokenId,
          name = tkd[["name"]],
          symbol = tkd[["symbol"]],
          type_token = "NON_FUNGIBLE",
          decimals = 0,
          date_created = lubridate::now(tzone = "UTC"),
          id_account_h_treasury = paste(treasAccId$shard, treasAccId$realm, treasAccId$num, sep = "."))
        
        addToDb(
          dfAdd = dfAdd, 
          tblNm = "tbl_tokens", 
          vnmsChckEx = c("id_token_h"), 
          dbCon = dbCon, 
          calcIds = FALSE)
      }
      
      # Link the token and its key pairs in the db.
      {
        addToDb(
          dfAdd = data.frame(
            id_token_h = tokenId, 
            id_key_pair = tokenSupplyKeyPairId, 
            label_key_pair = "SUPPLY"), 
          tblNm = "tbl_link_hedera_tokens_x_key_pairs", 
          vnmsChckEx = c("id_token_h", "id_key_pair", "label_key_pair"), 
          dbCon = dbCon, 
          calcIds = FALSE)
      }
      
      return(tokenId)
      
    })
  }
  
  # Create links in the db between each token and its issuing workflow(s).
  # TODO: The code below is suboptimal, because it will fail the moment we have
  # multiple versions of a workflow. Improve.
  {
    tknWrkflwlinks <- c(
      "II-L-PD" = "Independent Impact - License Application Workflow",
      "II-L-PV" = "Independent Impact - License Application Workflow",
      "II-L-MV" = "Independent Impact - License Application Workflow",
      "II-C-PDD" = "Independent Impact - PDD Validation Workflow",
      "II-C-MR" = "Independent Impact - Monitoring Verification Workflow",
      "II-C-VI" = "Independent Impact - Main Workflow")
    
    dfTokens <- dbGetQuery(conn = dbCon, statement = "SELECT oidx, id_token_h, symbol FROM tbl_tokens;")
    dfWrkflws <- dbGetQuery(conn = dbCon, statement = "SELECT oidx, id, title FROM tbl_workflows;")
    
    dfTokens$title_workflow <- tknWrkflwlinks[dfTokens$symbol]
    dfTokens$id_workflow <- NA_character_
    idxx <- match(x = dfTokens$title_workflow, table = dfWrkflws$title)  
    dfTokens$id_workflow <- dfWrkflws$id[idxx]
    
    addToDb(
      dfAdd = dfTokens[,c("id_token_h", "id_workflow")], 
      tblNm = "tbl_link_tokens_x_workflows",
      vnmsChckEx = c("id_token_h", "id_workflow"), 
      dbCon = dbCon, 
      calcIds = FALSE)
  }
}

# Populate tbl_map_schemas_identifying_content_vars.
addIdContVarsToDb(dbCon)

# Create log-in credentials for the standards body / registry user.
{
  # 1. Create an entry into tbl_agents.
  {
    dfAdd <- data.frame(
      id = NA_character_,
      date_registered = lubridate::now(tzone = "UTC"),
      type_user = "STANDARD_REGISTRY")
    
    idAgent <- addToDb(
      dfAdd = dfAdd,
      tblNm = "tbl_agents",
      calcIds = TRUE,
      returnIds = TRUE,
      dbCon = dbCon,
      vnmsChckEx = NULL)
  }
  
  # 2. Create an entry into tbl_link_agents_x_ics_passwords.
  {
    dfAdd <- data.frame(
      id_agent = idAgent,
      hash_pw_ics = digest::digest(
        object = "REPLACE_ME", 
        algo = "sha512"))
    
    addToDb(
      dfAdd = dfAdd,
      tblNm = "tbl_link_agents_x_ics_passwords",
      vnmsChckEx = "id_agent",
      dbCon = dbCon,
      calcIds = FALSE)
  }
  
  # 3. Create an entry into tbl_link_agents_x_email_addresses.
  {
    dfAdd <- data.frame(
      id_agent = idAgent,
      email_address = iwefdj$EMAIL_ADDRESS_NovaInstitute)
    
    addToDb(
      dfAdd = dfAdd,
      tblNm = "tbl_link_agents_x_email_addresses",
      vnmsChckEx = names(dfAdd),
      dbCon = dbCon,
      calcIds = FALSE)
  }
  
  # 6. Link our Hedera account with its key pair in the db.
  {
    addToDb(
      dfAdd = data.frame(
        id_account_h = operatorAccIdStr,
        id_key_pair = idUserKeyPair,
        label_key_pair = "PRIMARY"), 
      tblNm = "tbl_link_hedera_accounts_x_key_pairs", 
      vnmsChckEx = c("id_account_h", "id_key_pair", "label_key_pair"), 
      dbCon = dbCon, 
      calcIds = FALSE)
  }
  
  # 7. Create an entry into tbl_link_agents_x_hedera_accounts.
  {
    dfAdd <- data.frame(
      id_agent = idAgent,
      id_acc_h = operatorAccIdStr,
      label_acc_h = "PRIMARY")
    
    addToDb(
      dfAdd = dfAdd,
      tblNm = "tbl_link_agents_x_hedera_accounts",
      vnmsChckEx = names(dfAdd),
      dbCon = dbCon,
      calcIds = FALSE)
  }
  
  # 8. Create or import a Hedera DID (and thus a topic) for ourselves.
  {
    # Register a new identifier.
    {
      did <- hieroDid$HederaDid(
        client = client, 
        private_key_der = operatorPrivKey$to_string_der())
      asyncio$run(did$register())
      sprintf("New DID created with identifier %s", did$identifier)
    }
    
    # Create the DID document.
    {
      didDoc <- list(
        "@context" = list("https://www.w3.org/ns/did/v1"),
        id = did$identifier,
        verificationMethod = list(
          list(
            id = paste0(did$identifier, "#key-1"),
            type = "Ed25519VerificationKey2018",
            controller = did$identifier,
            publicKeyMultibase = hieroDid$utils$encoding$multibase_encode(
              value = operatorPubKey$to_bytes_raw(), 
              encoding = "base58btc"))),
        authentication = list(paste0(did$identifier, "#key-1")),
        assertionMethod = list(paste0(did$identifier, "#key-1")))
    }
    
    # Upload the DID document to IPFS.
    {
      fp <- tempfile(fileext = ".json")
      cat(jsonlite::toJSON(x = didDoc, auto_unbox = TRUE), file = fp)
      
      ipfsCid <- uploadToIpfs(
        df = data.frame(
          datapath = fp, 
          name = "did-doc.json",
          ext = ".json"), 
        encrypt = FALSE, 
        zip = FALSE, 
        cidOnly = TRUE, 
        wrap = FALSE)
      
      file.remove(fp)
    }
    
    # Add a service entry to the DID.
    asyncio$run(
      did$add_service(
        id_ = paste0(did$identifier, "#service-0"), 
        service_type = "DIDDocument", 
        service_endpoint = paste0("ipfs://", ipfsCid)))
    
    # # Test.
    # resolver <- hieroDid$HederaDidResolver(client)
    # result <- asyncio$run(resolver$resolve(did$identifier))
  }
  
  # 9. Link the DID topic and its key pair in the db.
  {
    didTopicId <- strsplit(did$identifier, split = "_", fixed = TRUE)[[1]][2]
    addToDb(
      dfAdd = data.frame(
        id_topic_h = didTopicId,
        id_key_pair = idUserKeyPair,
        label_key_pair = "ADMIN"), 
      tblNm = "tbl_link_hedera_topics_x_key_pairs", 
      vnmsChckEx = c("id_topic_h", "id_key_pair", "label_key_pair"), 
      dbCon = dbCon, 
      calcIds = FALSE)
  }
  
  # 10. Create an entry into tbl_link_agents_x_dids.
  {
    dfAdd <- data.frame(
      id_agent = idAgent,
      did = did$identifier,
      uri_ipfs_doc_did = paste0("ipfs://", ipfsCid))
    
    addToDb(
      dfAdd = dfAdd, 
      tblNm = "tbl_link_agents_x_dids", 
      vnmsChckEx = "did", 
      dbCon = dbCon, 
      calcIds = FALSE)
  }
  
  # 11. Link our agent ID to our DID topic in the db. 
  {
    addToDb(
      dfAdd = data.frame(
        id_topic_h = didTopicId, 
        id_entity = idAgent, 
        label_topic_h = "DID"), 
      tblNm = "tbl_link_entities_x_hedera_topics", 
      vnmsChckEx = c("id_topic_h", "id_entity", "label_topic_h"), 
      dbCon = dbCon, 
      calcIds = FALSE)
  }
}

# Run prepFlureeDataModel.R 
