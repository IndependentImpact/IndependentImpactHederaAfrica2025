# plumber_merged.R

#* @apiTitle Independent Impact API (Merged)
#* @apiDescription Combined REST API covering account, workflow, agent, project, and trust-chain endpoints.

library(plumber)
library(DBI)
library(RPostgres)
library(digest)
library(jsonlite)
library(lubridate)
library(tibble)
library(dplyr)
library(stringr)
library(glue)
library(openssl)
library(cyphr)



# ------------------------------------------------------------------------------
# Shared configuration
# ------------------------------------------------------------------------------

getDbCon <- function() {
  require("RPostgres")
  DBI::dbConnect(
    RPostgres::Postgres(),
    host     = Sys.getenv("II_DB_HOST"),
    dbname   = Sys.getenv("II_DB_NAME"),
    port     = Sys.getenv("II_DB_PORT"),
    user     = Sys.getenv("II_DB_USER"),
    password = Sys.getenv("II_DB_PASS")
  )
}

hederaNetwork <- c("testnet", "previewnet", "mainnet")[1]


# ------------------------------------------------------------------------------
# Helper bootstrap
# ------------------------------------------------------------------------------

maybe_source_helpers <- function() {
  if (exists(".plumber_helpers_loaded", envir = .GlobalEnv)) {
    return(invisible())
  }

  paths_to_try <- c(".", "..", "../..", "../../..")

  for (base in paths_to_try) {
    fn_dir <- file.path(base, "functions")
    if (!dir.exists(fn_dir)) next

    helper_files <- list.files(
      fn_dir,
      pattern = "\\.(R|r)$",
      full.names = TRUE,
      recursive = TRUE,
      include.dirs = FALSE
    )
    if (length(helper_files) > 0) {
      invisible(lapply(helper_files, function(f) source(f, local = FALSE)))
    }

    assign(".plumber_helpers_loaded", TRUE, envir = .GlobalEnv)
    break
  }
}

maybe_source_helpers()

maybe_source_modules <- function() {
  if (exists(".plumber_modules_loaded", envir = .GlobalEnv)) {
    return(invisible())
  }

  paths_to_try <- c(".", "..", "../..", "../../..")

  for (base in paths_to_try) {
    mod_dir <- file.path(base, "modules")
    if (!dir.exists(mod_dir)) next

    module_files <- list.files(
      mod_dir,
      pattern = "\\.(R|r)$",
      full.names = TRUE,
      recursive = TRUE,
      include.dirs = FALSE
    )
    if (length(module_files) > 0) {
      invisible(lapply(module_files, function(fp) source(fp, local = FALSE)))
    }

    assign(".plumber_modules_loaded", TRUE, envir = .GlobalEnv)
    break
  }
}

maybe_source_modules()



# ------------------------------------------------------------------------------
# Utility helpers
# ------------------------------------------------------------------------------

cleanJSONCols <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(df)
  df[] <- lapply(df, function(col) {
    if (inherits(col, "pq_json")) {
      vapply(col, as.character, "")
    } else col
  })
  df
}

split_param <- function(x) {
  if (is.null(x) || length(x) == 0) return(character())
  if (all(is.na(x))) return(character())
  parts <- unlist(strsplit(x, ",", fixed = TRUE))
  parts <- trimws(parts)
  parts[nzchar(parts)]
}

get_agent_did <- function(con, agent_id) {
  res <- dbGetQuery(
    con,
    "SELECT did FROM tbl_link_agents_x_dids WHERE id_agent = $1;",
    params = list(agent_id)
  )
  if (nrow(res) == 0) return(NA_character_)
  res$did[1]
}

safe_symbol_name <- function(x) {
  if (length(x) == 0) return(NA_character_)
  x <- tolower(as.character(x[1]))
  x <- stringr::str_replace_all(x, "[^[:alnum:]]+", "_")
  x <- stringr::str_replace_all(x, "_+", "_")
  stringr::str_replace_all(x, "^_|_$", "")
}

schema_abbreviation <- function(name) {
  if (length(name) == 0 || is.na(name) || !nzchar(name)) return(NA_character_)
  idx_open <- gregexpr("(", name, fixed = TRUE)[[1]]
  if (length(idx_open) == 0 || idx_open[1] == -1) return(NA_character_)
  idx_close <- gregexpr(")", name, fixed = TRUE)[[1]]
  if (length(idx_close) == 0 || idx_close[1] == -1) {
    idx_close <- nchar(name) + 1
  } else {
    idx_close <- idx_close[length(idx_close)]
  }
  idx_open <- idx_open[length(idx_open)]
  if (idx_close <= idx_open) return(NA_character_)
  substr(name, idx_open + 1, idx_close - 1)
}

shorten_did <- function(did) {
  if (length(did) == 0 || is.na(did) || !nzchar(did)) return(NA_character_)
  did <- as.character(did)
  prefix_start <- min(20, nchar(did))
  prefix_end <- min(30, nchar(did))
  suffix_start <- max(1, nchar(did) - 10)
  suffix_end <- nchar(did)
  sprintf(
    "did:%s...%s",
    substr(did, start = prefix_start, stop = prefix_end),
    substr(did, start = suffix_start, stop = suffix_end)
  )
}

shorten_ipfs <- function(url) {
  if (length(url) == 0 || is.na(url) || !nzchar(url)) return(NA_character_)
  url <- as.character(url)
  if (nchar(url) <= 40) return(url)
  sprintf(
    "%s...%s",
    substr(url, start = 1, stop = 15),
    substr(url, start = nchar(url) - 19, stop = nchar(url))
  )
}

parse_json_body <- function(req) {
  if (is.null(req$postBody) || !nzchar(req$postBody)) return(list())
  tryCatch(
    jsonlite::fromJSON(req$postBody, simplifyVector = TRUE),
    error = function(e) list()
  )
}

first_non_empty <- function(...) {
  vals <- list(...)
  for (val in vals) {
    if (is.null(val) || length(val) == 0) next
    candidate <- val[1]
    if (is.null(candidate) || is.na(candidate)) next
    candidate <- as.character(candidate)
    if (!nzchar(candidate)) next
    return(candidate)
  }
  NULL
}

prepare_identifying_content <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0) return(df)
  if (!exists("processIdentifyingContentForDisplay", mode = "function")) return(df)
  tryCatch(
    processIdentifyingContentForDisplay(df),
    error = function(e) df
  )
}

as_utc <- function(x) {
  if (is.null(x)) return(x)
  if (inherits(x, "POSIXct")) {
    return(lubridate::with_tz(x, tzone = "UTC"))
  }
  suppressWarnings({
    parsed <- lubridate::as_datetime(x, tz = "UTC")
  })
  lubridate::with_tz(parsed, tzone = "UTC")
}

get_project_documents <- function(con, id) {
  dbCon <- getDbCon()
  on.exit(dbDisconnect(dbCon), add = TRUE)
  docs <- dbFetch(
    dbSendQuery(
      conn = dbCon, 
      statement = sprintf(
        "SELECT * FROM tbl_document_metadata WHERE id_entity = '%s' ORDER BY date_modified DESC;", 
        id)))
  
  if (nrow(docs) > 0) {
    rownames(docs) <- 1:nrow(docs)
  }

  if (nrow(docs) == 0) {
    return(tibble())
  }

  docs <- cleanJSONCols(tibble::as_tibble(docs))

  time_cols <- intersect(c("date_created", "date_modified"), names(docs))
  for (col in time_cols) {
    docs[[col]] <- as_utc(docs[[col]])
  }

  docs <- prepare_identifying_content(docs)

  if (!"monitoring_period" %in% names(docs)) {
    docs$monitoring_period <- NA_character_
  }
  if (!"additional_info" %in% names(docs)) {
    docs$additional_info <- NA_character_
  }

  docs$additional_info <- dplyr::coalesce(
    as.character(docs$additional_info),
    as.character(docs$identifying_content)
  )
  docs$project_id <- id

  schema_ids <- unique(docs$id_schema)
  schema_ids <- schema_ids[!is.na(schema_ids)]
  if (length(schema_ids) > 0) {
    schema_meta <- dbGetQuery(
      dbCon,
      paste("SELECT id, title, tag_version FROM tbl_schemas WHERE id IN(", paste("'", schema_ids,"'", sep = "", collapse = ","),");", sep = ""),
    )
    schema_meta <- tibble::as_tibble(schema_meta)
    docs <- docs %>%
      left_join(schema_meta, by = c("id_schema" = "id")) %>%
      rename(
        schema_title = title,
        schema_version = tag_version
      )
  } else {
    docs <- docs %>%
      mutate(
        schema_title = NA_character_,
        schema_version = NA_character_
      )
  }

  wf_ids <- unique(docs$id_workflow)
  wf_ids <- wf_ids[!is.na(wf_ids)]
  if (length(wf_ids) > 0) {
    wf_meta <- dbGetQuery(
      dbCon,
      paste("SELECT id, title, tag_version FROM tbl_workflows WHERE id IN (", paste("'", wf_ids,"'", sep = "", collapse = ","),");", sep = "")
    )
    wf_meta <- tibble::as_tibble(wf_meta)
    docs <- docs %>%
      left_join(wf_meta, by = c("id_workflow" = "id")) %>%
      rename(
        workflow_title = title,
        workflow_version = tag_version
      )
  } else {
    docs <- docs %>%
      mutate(
        workflow_title = NA_character_,
        workflow_version = NA_character_
      )
  }

  if ("did_author" %in% names(docs)) {
    did_vec <- unique(docs$did_author)
    did_vec <- did_vec[!is.na(did_vec)]
    if (length(did_vec) > 0) {
      did_map <- dbGetQuery(
        con,
        paste("SELECT did, id_agent FROM tbl_link_agents_x_dids WHERE did IN (", paste("'", did_vec, "'", sep = "", collapse = ","),");")
      )
      did_map <- tibble::as_tibble(did_map)
      docs <- docs %>%
        left_join(did_map, by = c("did_author" = "did"))

      agent_vec <- unique(did_map$id_agent)
      agent_vec <- agent_vec[!is.na(agent_vec)]
      if (length(agent_vec) > 0) {
        email_map <- dbGetQuery(
          con,
          paste("SELECT id_agent, email_address, oidx
           FROM tbl_link_agents_x_email_addresses
           WHERE id_agent IN (",  paste("'", agent_vec, "'", sep = "", collapse = ","),");", sep = "")
        )
        email_map <- tibble::as_tibble(email_map)
        if (nrow(email_map) > 0) {
          email_map <- email_map %>%
            arrange(id_agent, desc(oidx)) %>%
            distinct(id_agent, .keep_all = TRUE)
          docs <- docs %>%
            left_join(email_map %>% select(id_agent, email_address), by = "id_agent")
        } else {
          docs$email_address <- NA_character_
        }
      } else {
        docs$email_address <- NA_character_
      }
    } else {
      docs$email_address <- NA_character_
    }
  }

  docs %>%
    arrange(desc(date_modified)) %>%
    distinct(id_message_h, uri_ipfs, .keep_all = TRUE)
}

list_projects_for_agent <- function(con, agent_id = NA_character_) {
  if (length(agent_id) == 0 || is.null(agent_id) || is.na(agent_id) || !nzchar(agent_id[1])) {
    agent_id <- NA_character_
  } else {
    agent_id <- agent_id[1]
  }

  dfProjs <- dbFetch(
    dbSendQuery(conn = con, statement = "SELECT * FROM tbl_projects;")
  )

  if (nrow(dfProjs) == 0) {
    return(list(my_projects = tibble(), other_projects = tibble()))
  }

  vals <- dbQuoteString(con, unique(dfProjs$created_by))
  sql  <- paste0(
    "SELECT id_agent, email_address
   FROM tbl_link_agents_x_email_addresses
   WHERE id_agent IN (", paste(vals, collapse = ","), ")"
  )
  emails <- dbGetQuery(con, sql)
  
  if (nrow(emails) > 0) {
    idx <- match(dfProjs$created_by, emails$id_agent)
    dfProjs$created_by_email <- NA_character_
    valid <- which(!is.na(idx))
    if (length(valid)) {
      dfProjs$created_by_email[valid] <- emails$email_address[idx[valid]]
    }
  } else {
    dfProjs$created_by_email <- NA_character_
  }

  dfProjs <- dfProjs[order(dfProjs$date_created, decreasing = TRUE), ]
  rownames(dfProjs) <- NULL

  if (is.na(agent_id)) {
    my_projects <- tibble()
    other_projects <- dfProjs
  } else {
    my_projects <- dfProjs[dfProjs$created_by == agent_id, , drop = FALSE]
    other_projects <- dfProjs[dfProjs$created_by != agent_id, , drop = FALSE]
  }

  list(
    my_projects = cleanJSONCols(tibble(my_projects)),
    other_projects = cleanJSONCols(tibble(other_projects))
  )
}

create_project_for_agent <- function(con, agent_id, title) {
  if (!is.character(title) || length(title) == 0) {
    stop("Project title must be a non-empty character string.", call. = FALSE)
  }
  title <- trimws(title[1])
  n_char <- nchar(title)
  if (n_char < 3 || n_char > 100) {
    stop("Project title must be between 3 and 100 characters.", call. = FALSE)
  }

  existing <- dbGetQuery(
    con,
    "SELECT id FROM tbl_projects WHERE title = $1;",
    params = list(title)
  )
  if (nrow(existing) > 0) {
    stop("Title already in use.", call. = FALSE)
  }

  timestamp <- now(tzone = "UTC")
  dbGetQuery(
    con,
    "INSERT INTO tbl_projects (title, created_by, date_created)
     VALUES ($1, $2, $3)
     RETURNING id;",
    params = list(
      title,
      agent_id,
      format(timestamp, "%Y-%m-%d %H:%M:%S")
    )
  )$id
}

authenticate_user_credentials <- function(con, email, password) {
  if (!nzchar(email) || !nzchar(password)) {
    stop("Email and password are required.", call. = FALSE)
  }

  email_is_valid <- FALSE
  if (exists("validateEmail", mode = "function")) {
    email_is_valid <- tryCatch(isTRUE(validateEmail(email)), error = function(e) FALSE)
  } else {
    email_is_valid <- grepl("^[^@\\s]+@[^@\\s]+\\.[^@\\s]+$", email)
  }
  if (!email_is_valid) {
    stop("Invalid email address.", call. = FALSE)
  }

  dfEmail <- dbGetQuery(
    con,
    "SELECT * FROM tbl_link_agents_x_email_addresses WHERE email_address = $1;",
    params = list(email)
  )

  if (nrow(dfEmail) == 0) {
    stop("Account not found.", call. = FALSE)
  }

  if (nrow(dfEmail) > 1) {
    stop("Multiple accounts with this email. Contact support.", call. = FALSE)
  }

  agent_id <- dfEmail$id_agent[1]

  dfPw <- dbGetQuery(
    con,
    "SELECT hash_pw_ics FROM tbl_link_agents_x_ics_passwords WHERE id_agent = $1;",
    params = list(agent_id)
  )
  if (nrow(dfPw) == 0) {
    stop("No password registered for this account.", call. = FALSE)
  }

  hash <- digest::digest(object = password, algo = "sha512")
  if (!identical(hash, dfPw$hash_pw_ics[1])) {
    stop("Incorrect password.", call. = FALSE)
  }

  user_type <- dbGetQuery(
    con,
    "SELECT type_user FROM tbl_agents WHERE id = $1;",
    params = list(agent_id)
  )$type_user
  if (length(user_type) == 0 || is.na(user_type) || !nzchar(user_type[1])) {
    stop("Unable to determine user type.", call. = FALSE)
  }

  did <- dbGetQuery(
    con,
    "SELECT did FROM tbl_link_agents_x_dids WHERE id_agent = $1;",
    params = list(agent_id)
  )$did

  if (length(did) == 0 || is.na(did) || !nzchar(did[1])) {
    stop("Unable to resolve agent DID.", call. = FALSE)
  }

  list(
    email = email,
    agent_id = agent_id,
    user_type = user_type[1],
    did = did[1]
  )
}

fetch_monitoring_periods <- function(con, project_id) {
  df <- dbGetQuery(
    con,
    "SELECT *
     FROM tbl_monitoring_periods
     WHERE id_project = $1
     ORDER BY date_start DESC;",
    params = list(project_id)
  )

  if (nrow(df) == 0) {
    return(tibble())
  }

  df$date_start <- lubridate::force_tz(df$date_start, tzone = "UTC")
  df$date_end <- lubridate::force_tz(df$date_end, tzone = "UTC")

  cleanJSONCols(tibble::as_tibble(df))
}

fetch_monitored_parameters <- function(con, project_id) {
  df <- dbGetQuery(
    con,
    "SELECT *
     FROM tbl_monitored_parameters
     WHERE id_project = $1
     ORDER BY created_at DESC;",
    params = list(project_id)
  )

  if (nrow(df) == 0) {
    return(tibble())
  }

  cleanJSONCols(tibble::as_tibble(df))
}

account_error <- function(message, status = 400) {
  structure(list(message = message, status = status), class = "plumber_account_error")
}

create_agent_account <- function(email, password, accept_terms = FALSE) {
  maybe_source_helpers()

  if (!isTRUE(accept_terms)) {
    stop(account_error("You must accept the Independent Impact Terms and Conditions before creating an account.", status = 400))
  }

  if (is.null(email) || !nzchar(email)) {
    stop(account_error("Email address is required.", status = 400))
  }

  if (is.null(password) || !nzchar(password)) {
    stop(account_error("Password is required.", status = 400))
  }

  email_is_valid <- FALSE
  if (exists("validateEmail", mode = "function")) {
    email_is_valid <- tryCatch(isTRUE(validateEmail(email)), error = function(e) FALSE)
  } else {
    email_is_valid <- grepl("^[^@\\s]+@[^@\\s]+\\.[^@\\s]+$", email)
  }
  if (!email_is_valid) {
    stop(account_error("Invalid email address.", status = 400))
  }

  pwd_validation <- tryCatch(
    validatePassword(pwd = password, emailAddr = email),
    error = function(e) stop(account_error(conditionMessage(e), status = 400))
  )
  if (!isTRUE(pwd_validation$valid)) {
    stop(account_error(pwd_validation$msg, status = 400))
  }

  con <- getDbCon()
  on.exit(dbDisconnect(con), add = TRUE)

  email_exists <- dbGetQuery(
    con,
    "SELECT 1 FROM tbl_link_agents_x_email_addresses WHERE email_address = $1;",
    params = list(email)
  )
  if (nrow(email_exists) > 0) {
    stop(account_error("Email already registered.", status = 409))
  }

  # Initialise Hedera client using operator credentials
  operatorAccId <- hiero$AccountId$from_string(iwefdj[["HEDERA_OPERATOR_ACCOUNT_ID_ED25519"]])
  operatorPrivKey <- hiero$PrivateKey$from_string(iwefdj[["HEDERA_OPERATOR_ACCOUNT_PRIVATE_KEY_ED25519"]])

  client <- hiero$Client(network = hiero$Network(hederaNetwork))
  client$set_operator(
    account_id = operatorAccId,
    private_key = operatorPrivKey
  )

  balance_query <- hiero$CryptoGetAccountBalanceQuery(account_id = operatorAccId)
  balance <- balance_query$execute(client)
  if (balance$hbars$to_hbars() < 100) {
    stop(account_error("Insufficient HBAR balance on the operator account. Please top up before creating new agents.", status = 503))
  }

  result <- tryCatch({
    # 1. Create agent record
    idAgent <- addToDb(
      dfAdd = data.frame(
        id = NA_character_,
        date_registered = lubridate::now(tzone = "UTC"),
        type_user = "USER",
        stringsAsFactors = FALSE
      ),
      tblNm = "tbl_agents",
      vnmsChckEx = NULL,
      dbCon = con,
      calcIds = TRUE,
      returnIds = TRUE
    )
    idAgent <- as.character(idAgent[1])

    # 2. Store password hash
    addToDb(
      dfAdd = data.frame(
        id_agent = idAgent,
        hash_pw_ics = digest::digest(password, algo = "sha512"),
        stringsAsFactors = FALSE
      ),
      tblNm = "tbl_link_agents_x_ics_passwords",
      vnmsChckEx = "id_agent",
      dbCon = con,
      calcIds = FALSE
    )

    # 3. Store primary email address
    addToDb(
      dfAdd = data.frame(
        id_agent = idAgent,
        email_address = email,
        stringsAsFactors = FALSE
      ),
      tblNm = "tbl_link_agents_x_email_addresses",
      vnmsChckEx = c("id_agent", "email_address"),
      dbCon = con,
      calcIds = FALSE
    )

    # 4. Generate Hedera key pair and store encrypted private key
    userPrivKey <- hiero$PrivateKey$generate_ed25519()
    userPubKey <- userPrivKey$public_key()

    encrPrivKey <- cyphr::encrypt_string(
      string = userPrivKey$to_string_der(),
      key = cyphr::keypair_openssl(
        pub = iwefdj$KEYPTH_CYPHR,
        key = iwefdj$KEYPTH_CYPHR,
        envelope = TRUE,
        password = iwefdj$PW_CYPHR
      )
    )
    encrPrivKey <- openssl::base64_encode(encrPrivKey)

    idUserKeyPair <- addToDb(
      dfAdd = data.frame(
        public_key = userPubKey$to_string(),
        private_key_encr = encrPrivKey,
        stringsAsFactors = FALSE
      ),
      tblNm = "tbl_key_pairs",
      vnmsChckEx = "public_key",
      dbCon = con,
      calcIds = TRUE,
      returnIds = TRUE
    )
    idUserKeyPair <- as.character(idUserKeyPair[1])

    # 5. Create Hedera account for the agent
    tx <- hiero$AccountCreateTransaction(
      key = userPubKey,
      initial_balance = hiero$Hbar(20),
      receiver_signature_required = FALSE,
      auto_renew_period = as.integer(90 * 24 * 60 * 60),
      memo = idAgent
    )
    tx <- tx$freeze_with(client)
    tx <- tx$sign(operatorPrivKey)
    resp <- tx$execute(client)
    id_acc_h <- paste(resp$accountId$shard, resp$accountId$realm, resp$accountId$num, sep = ".")

    # 6. Link Hedera account and key pair
    addToDb(
      dfAdd = data.frame(
        id_account_h = id_acc_h,
        id_key_pair = idUserKeyPair,
        label_key_pair = "PRIMARY",
        stringsAsFactors = FALSE
      ),
      tblNm = "tbl_link_hedera_accounts_x_key_pairs",
      vnmsChckEx = c("id_account_h", "id_key_pair", "label_key_pair"),
      dbCon = con,
      calcIds = FALSE
    )

    # 7. Link agent to Hedera account
    addToDb(
      dfAdd = data.frame(
        id_agent = idAgent,
        id_acc_h = id_acc_h,
        label_acc_h = "PRIMARY",
        stringsAsFactors = FALSE
      ),
      tblNm = "tbl_link_agents_x_hedera_accounts",
      vnmsChckEx = c("id_agent", "id_acc_h", "label_acc_h"),
      dbCon = con,
      calcIds = FALSE
    )

    # 8. Create DID for the agent
    did <- hieroDid$HederaDid(
      client = client,
      private_key_der = userPrivKey$to_string_der()
    )
    asyncio$run(did$register())

    did_doc <- list(
      "@context" = list("https://www.w3.org/ns/did/v1"),
      id = did$identifier,
      verificationMethod = list(
        list(
          id = sprintf("%s#key-1", did$identifier),
          type = "Ed25519VerificationKey2018",
          controller = did$identifier,
          publicKeyMultibase = hieroDid$utils$encoding$multibase_encode(
            value = userPubKey$to_bytes_raw(),
            encoding = "base58btc"
          )
        )
      ),
      authentication = list(sprintf("%s#key-1", did$identifier)),
      assertionMethod = list(sprintf("%s#key-1", did$identifier))
    )

    fp <- tempfile(fileext = ".json")
    on.exit(if (file.exists(fp)) unlink(fp), add = TRUE)
    cat(jsonlite::toJSON(did_doc, auto_unbox = TRUE), file = fp)

    ipfsCid <- uploadToIpfs(
      df = data.frame(
        datapath = fp,
        name = "did-doc.json",
        ext = ".json",
        stringsAsFactors = FALSE
      ),
      encrypt = FALSE,
      zip = FALSE,
      cidOnly = TRUE,
      wrap = FALSE
    )
    if (length(ipfsCid) == 0 || is.null(ipfsCid) || !nzchar(ipfsCid)) {
      stop(account_error("Failed to publish DID document to IPFS.", status = 500))
    }

    asyncio$run(
      did$add_service(
        id_ = sprintf("%s#service-0", did$identifier),
        service_type = "DIDDocument",
        service_endpoint = sprintf("ipfs://%s", ipfsCid)
      )
    )

    # 9. Link DID topic and key pair
    did_parts <- strsplit(did$identifier, split = "_", fixed = TRUE)[[1]]
    if (length(did_parts) < 2) {
      stop(account_error("Unexpected DID format returned by Hedera.", status = 500))
    }
    didTopicId <- did_parts[2]
    addToDb(
      dfAdd = data.frame(
        id_topic_h = didTopicId,
        id_key_pair = idUserKeyPair,
        label_key_pair = "ADMIN",
        stringsAsFactors = FALSE
      ),
      tblNm = "tbl_link_hedera_topics_x_key_pairs",
      vnmsChckEx = c("id_topic_h", "id_key_pair", "label_key_pair"),
      dbCon = con,
      calcIds = FALSE
    )

    # 10. Link agent to DID
    addToDb(
      dfAdd = data.frame(
        id_agent = idAgent,
        did = did$identifier,
        uri_ipfs_doc_did = sprintf("ipfs://%s", ipfsCid),
        stringsAsFactors = FALSE
      ),
      tblNm = "tbl_link_agents_x_dids",
      vnmsChckEx = "did",
      dbCon = con,
      calcIds = FALSE
    )

    # 11. Link agent to DID topic
    addToDb(
      dfAdd = data.frame(
        id_topic_h = didTopicId,
        id_entity = idAgent,
        label_topic_h = "DID",
        stringsAsFactors = FALSE
      ),
      tblNm = "tbl_link_entities_x_hedera_topics",
      vnmsChckEx = c("id_topic_h", "id_entity", "label_topic_h"),
      dbCon = con,
      calcIds = FALSE
    )

    list(
      email_address = email,
      id_agent = idAgent,
      did = did$identifier,
      hedera_account = id_acc_h,
      message = "Your Independent Impact account has been created. Welcome!"
    )
  }, error = function(e) {
    if (inherits(e, "plumber_account_error")) {
      stop(e)
    }
    stop(account_error(conditionMessage(e), status = 500))
  })

  result
}

get_agent_overview <- function(con, agent_id) {
  profile <- dbGetQuery(
    con,
    "SELECT * FROM tbl_agents WHERE id = $1;",
    params = list(agent_id)
  )
  if ("oidx" %in% names(profile)) profile$oidx <- NULL
  dids <- dbGetQuery(
    con,
    "SELECT * FROM tbl_link_agents_x_dids WHERE id_agent = $1 ORDER BY oidx DESC;",
    params = list(agent_id)
  )
  if ("oidx" %in% names(dids)) dids$oidx <- NULL
  hedera_accounts <- dbGetQuery(
    con,
    "SELECT * FROM tbl_link_agents_x_hedera_accounts WHERE id_agent = $1 ORDER BY oidx DESC;",
    params = list(agent_id)
  )
  if ("oidx" %in% names(hedera_accounts)) hedera_accounts$oidx <- NULL
  emails <- dbGetQuery(
    con,
    "SELECT * FROM tbl_link_agents_x_email_addresses WHERE id_agent = $1 ORDER BY oidx DESC;",
    params = list(agent_id)
  )
  if ("oidx" %in% names(emails)) emails$oidx <- NULL

  reputation <- dbGetQuery(
    con,
    "SELECT * FROM tbl_agent_reputation WHERE id_agent = $1;",
    params = list(agent_id)
  )
  if ("oidx" %in% names(reputation)) reputation$oidx <- NULL

  all_workflows <- dbGetQuery(
    con,
    "SELECT id, title, tag_version, subject, status
     FROM tbl_workflows
     WHERE subject = 'AGENT';"
  )
  linked_ids <- dbGetQuery(
    con,
    "SELECT DISTINCT id_workflow
     FROM tbl_document_metadata
     WHERE id_entity = $1;",
    params = list(agent_id)
  )$id_workflow

  if (length(linked_ids) == 0 && nrow(all_workflows) > 0) {
    if (exists("orderVersionTags", mode = "function")) {
      latest_tag <- orderVersionTags(unique(all_workflows$tag_version), asc = FALSE)[1]
    } else {
      latest_tag <- tail(sort(unique(all_workflows$tag_version)), 1)
    }
    workflows <- all_workflows[all_workflows$tag_version == latest_tag, , drop = FALSE]
  } else if (length(linked_ids) > 0) {
    workflows <- all_workflows[all_workflows$id %in% linked_ids, , drop = FALSE]
  } else {
    workflows <- tibble()
  }

  primary_did <- if (nrow(dids) > 0) dids$did[1] else NA_character_
  licenses <- tibble()
  if (!is.na(primary_did) && nzchar(primary_did)) {
    licenses <- dbGetQuery(
      con,
      "SELECT * FROM tbl_agent_licenses WHERE did_holder = $1;",
      params = list(primary_did)
    )
    if ("oidx" %in% names(licenses)) licenses$oidx <- NULL
  }

  documents <- dbGetQuery(
    con,
    "SELECT *
     FROM tbl_document_metadata
     WHERE id_entity = $1
     ORDER BY date_modified DESC;",
    params = list(agent_id)
  )
  if ("oidx" %in% names(documents)) documents$oidx <- NULL

  list(
    profile = cleanJSONCols(tibble::as_tibble(profile)),
    dids = cleanJSONCols(tibble::as_tibble(dids)),
    hedera_accounts = cleanJSONCols(tibble::as_tibble(hedera_accounts)),
    emails = cleanJSONCols(tibble::as_tibble(emails)),
    reputation = cleanJSONCols(tibble::as_tibble(reputation)),
    workflows = cleanJSONCols(tibble::as_tibble(workflows)),
    licenses = cleanJSONCols(tibble::as_tibble(licenses)),
    documents = cleanJSONCols(tibble::as_tibble(documents))
  )
}

# ------------------------------------------------------------------------------
# Trust chain helpers
# ------------------------------------------------------------------------------

layout_trust_chain_nodes <- function(con, df_nodes, req_chain = NULL, id_col = "id_message") {
  if (nrow(df_nodes) == 0) {
    return(list(nodes = tibble(), lines = tibble()))
  }

  id_col <- if (id_col %in% names(df_nodes)) id_col else names(df_nodes)[1]

  if (!"name_schema" %in% names(df_nodes) && "title_schema" %in% names(df_nodes)) {
    df_nodes$name_schema <- df_nodes$title_schema
  }
  if (!"ver_schema" %in% names(df_nodes) && "tag_version" %in% names(df_nodes)) {
    df_nodes$ver_schema <- df_nodes$tag_version
  }

  df_nodes <- df_nodes[!duplicated(df_nodes[[id_col]]), , drop = FALSE]
  rownames(df_nodes) <- NULL

  if ("id_g_policy" %in% names(df_nodes)) {
    df_policies <- dbGetQuery(
      con,
      "SELECT id_g, name, version FROM tbl_policies;"
    )
    idx <- match(df_nodes$id_g_policy, df_policies$id_g)
    df_nodes$name_policy <- NA_character_
    df_nodes$ver_policy <- NA_character_
    valid <- which(!is.na(idx))
    if (length(valid)) {
      df_nodes$name_policy[valid] <- df_policies$name[idx[valid]]
      df_nodes$ver_policy[valid] <- df_policies$version[idx[valid]]
    }
  }

  if ("type_vc" %in% names(df_nodes)) {
    df_nodes$type_vc <- sprintf("#%s", df_nodes$type_vc)
  }
  if ("iri_schema" %in% names(df_nodes) || "type_vc" %in% names(df_nodes)) {
    schema_key <- if ("iri_schema" %in% names(df_nodes)) "iri_schema" else "type_vc"
    schema_ids <- df_nodes[[schema_key]]
    schema_ids <- schema_ids[!is.na(schema_ids)]
    if (length(schema_ids) > 0) {
      df_schemas <- dbGetQuery(
        con,
        "SELECT iri, name, version FROM tbl_schemas WHERE iri = ANY($1::text[]);",
        params = list(unique(schema_ids))
      )
      idx <- match(df_nodes[[schema_key]], df_schemas$iri)
      df_nodes$name_schema <- NA_character_
      df_nodes$ver_schema <- NA_character_
      valid <- which(!is.na(idx))
      if (length(valid)) {
        df_nodes$name_schema[valid] <- df_schemas$name[idx[valid]]
        df_nodes$ver_schema[valid] <- df_schemas$version[idx[valid]]
      }
    }
  }

  df_nodes$abbr_schema <- vapply(df_nodes$name_schema, schema_abbreviation, character(1))
  if ("id_msg_pred" %in% names(df_nodes)) {
    match_idx <- match(df_nodes$id_msg_pred, df_nodes[[id_col]])
    df_nodes$abbr_schema_pred <- NA_character_
    valid <- which(!is.na(match_idx))
    if (length(valid)) {
      df_nodes$abbr_schema_pred[valid] <- df_nodes$abbr_schema[match_idx[valid]]
    }
    name_schema_vec <- df_nodes$name_schema
    name_schema_vec[is.na(name_schema_vec)] <- ""
    idx_dr <- grep("Document Review", name_schema_vec, ignore.case = TRUE)
    idx_na <- which(is.na(df_nodes$abbr_schema) | (df_nodes$abbr_schema == "" & !is.na(df_nodes$abbr_schema_pred)))
    idx_fix <- intersect(idx_dr, idx_na)
    if (length(idx_fix)) {
      df_nodes$abbr_schema[idx_fix] <- sprintf("DR-%s", df_nodes$abbr_schema_pred[idx_fix])
    }
  }

  df_nodes$id_node <- seq_len(nrow(df_nodes))

  df_nodes$lvl <- NA_integer_
  if (!is.null(req_chain) && nrow(req_chain) > 0 && "type_document" %in% names(req_chain)) {
    idx <- match(df_nodes$abbr_schema, req_chain$type_document)
    valid <- which(!is.na(idx))
    if (length(valid)) {
      df_nodes$lvl[valid] <- req_chain$level[idx[valid]]
    }
  }

  df_nodes$col <- NA_real_
  lvls <- sort(unique(df_nodes$lvl[!is.na(df_nodes$lvl)]))
  if (length(lvls) == 0) {
    df_nodes$col <- seq_len(nrow(df_nodes))
  } else {
    lvl_counts <- df_nodes %>%
      filter(!is.na(lvl)) %>%
      group_by(lvl) %>%
      summarise(n = dplyr::n(), .groups = "drop")
    n_cols_max <- if (nrow(lvl_counts) == 0) nrow(df_nodes) else max(lvl_counts$n)
    if (n_cols_max %% 2 == 0) n_cols_max <- n_cols_max + 1
    for (lvl in lvls) {
      idx_lvl <- which(df_nodes$lvl == lvl)
      if (length(idx_lvl) == 0) next
      order_idx <- idx_lvl
      if (!is.null(req_chain) && nrow(req_chain) > 0) {
        level_docs <- req_chain$type_document[which(req_chain$level == lvl)]
        order_metric <- match(df_nodes$abbr_schema[idx_lvl], level_docs)
        if (!all(is.na(order_metric))) {
          order_metric[is.na(order_metric)] <- max(order_metric, na.rm = TRUE) + seq_len(sum(is.na(order_metric)))
          order_idx <- idx_lvl[order(order_metric)]
        }
      }
      cols <- seq_along(order_idx)
      cols <- cols + ((n_cols_max - length(cols)) / 2)
      df_nodes$col[order_idx] <- cols
    }
  }

  if (any(!is.na(df_nodes$lvl))) {
    max_lvl <- max(df_nodes$lvl, na.rm = TRUE)
    df_nodes$lvl <- (max_lvl + 1) - df_nodes$lvl
  }

  df_nodes$lbl_schema <- ifelse(
    !is.na(df_nodes$name_schema) & !is.na(df_nodes$ver_schema),
    sprintf("%s (v%s)", df_nodes$name_schema, df_nodes$ver_schema),
    df_nodes$name_schema
  )

  if ("did_issuer" %in% names(df_nodes)) {
    df_nodes$did_issuer_short <- vapply(df_nodes$did_issuer, shorten_did, character(1))
  }
  if ("url_ipfs" %in% names(df_nodes)) {
    df_nodes$url_ipfs_short <- vapply(df_nodes$url_ipfs, shorten_ipfs, character(1))
  }

  if ("did_issuer" %in% names(df_nodes)) {
    did_vec <- unique(df_nodes$did_issuer)
    did_vec <- did_vec[!is.na(did_vec)]
    if (length(did_vec) > 0) {
      did_map <- dbGetQuery(
        con,
        "SELECT did, id_agent FROM tbl_link_agents_x_dids WHERE did = ANY($1::text[]);",
        params = list(did_vec)
      )
      if (nrow(did_map) > 0) {
        df_nodes <- merge(df_nodes, did_map, by.x = "did_issuer", by.y = "did", all.x = TRUE)
        agent_vec <- unique(did_map$id_agent)
        agent_vec <- agent_vec[!is.na(agent_vec)]
        if (length(agent_vec) > 0) {
          email_map <- dbGetQuery(
            con,
            "SELECT id_agent, email_address, oidx FROM tbl_link_agents_x_email_addresses WHERE id_agent = ANY($1::text[]);",
            params = list(agent_vec)
          )
          if (nrow(email_map) > 0) {
            email_map <- email_map[order(email_map$id_agent, email_map$oidx, decreasing = TRUE), ]
            email_map <- email_map[!duplicated(email_map$id_agent), ]
            df_nodes <- merge(df_nodes, email_map[, c("id_agent", "email_address")], by = "id_agent", all.x = TRUE)
          } else {
            df_nodes$email_address <- NA_character_
          }
        } else {
          df_nodes$email_address <- NA_character_
        }
      } else {
        df_nodes$id_agent <- NA_character_
        df_nodes$email_address <- NA_character_
      }
    } else {
      df_nodes$id_agent <- NA_character_
      df_nodes$email_address <- NA_character_
    }
  }

  if ("date_issuance" %in% names(df_nodes)) {
    df_nodes$date_issuance <- suppressWarnings(lubridate::as_datetime(df_nodes$date_issuance))
  }

  nodes_tbl <- cleanJSONCols(tibble::as_tibble(df_nodes))
  list(nodes = nodes_tbl)
}

build_trust_chain_graph <- function(con, token_id_h, msg_id = NULL, ipfs_url = NULL) {
  if (is.null(token_id_h) || !nzchar(token_id_h)) {
    stop("tokenIdH is required", call. = FALSE)
  }

  token <- dbGetQuery(
    con,
    "SELECT * FROM tbl_tokens WHERE id_token_h = $1;",
    params = list(token_id_h)
  )
  if (nrow(token) == 0) {
    stop("Token not found", call. = FALSE)
  }

  real_chain <- getRealDocChain(msgId = msg_id, ipfsUrl = ipfs_url)
  if (is.null(real_chain)) {
    real_chain <- data.frame()
  }
  if (nrow(real_chain) > 0 && "uuid" %in% names(real_chain)) {
    real_chain <- real_chain[!is.na(real_chain$uuid), , drop = FALSE]
  }

  safe_sym <- safe_symbol_name(token$symbol[1])
  req_fun <- sprintf("getReqDocChain.%s", safe_sym)
  if (!exists(req_fun, mode = "function")) {
    stop(sprintf("Required document chain function '%s' not available", req_fun), call. = FALSE)
  }
  req_chain <- do.call(req_fun, list())
  if (is.null(req_chain)) {
    req_chain <- tibble()
  }

  nodes_tbl <- tibble()
  lines_tbl <- tibble()

  if (nrow(real_chain) > 0) {
    layout <- layout_trust_chain_nodes(con, real_chain, req_chain)
    nodes_tbl <- layout$nodes

    if ("url_ipfs" %in% names(real_chain)) {
      real_chain$id_node <- nodes_tbl$id_node[match(real_chain$url_ipfs, nodes_tbl$url_ipfs)]
      line_rows <- lapply(seq_len(nrow(real_chain)), function(r) {
        pred_url <- real_chain$url_ipfs_pred[r]
        if (is.null(pred_url) || is.na(pred_url) || !nzchar(pred_url)) return(NULL)
        from_node <- real_chain$id_node[r]
        if (is.null(from_node) || is.na(from_node)) return(NULL)
        to_node <- nodes_tbl$id_node[match(pred_url, nodes_tbl$url_ipfs)]
        if (length(to_node) == 0 || is.na(to_node)) return(NULL)
        data.frame(
          edge = r,
          x = c(nodes_tbl$col[nodes_tbl$id_node == from_node],
                nodes_tbl$col[nodes_tbl$id_node == to_node],
                NA_real_),
          y = c(nodes_tbl$lvl[nodes_tbl$id_node == from_node],
                nodes_tbl$lvl[nodes_tbl$id_node == to_node],
                NA_real_)
        )
      })
      line_rows <- Filter(function(x) !is.null(x), line_rows)
      if (length(line_rows)) {
        lines_tbl <- plyr::rbind.fill(line_rows)
        if (!is.null(lines_tbl) && nrow(lines_tbl) > 0) {
          lines_tbl <- cleanJSONCols(tibble::as_tibble(lines_tbl))
        } else {
          lines_tbl <- tibble()
        }
      }
    }
  }

  list(
    token = cleanJSONCols(tibble::as_tibble(token)),
    required_chain = cleanJSONCols(tibble::as_tibble(req_chain)),
    real_chain = cleanJSONCols(tibble::as_tibble(real_chain)),
    nodes = nodes_tbl,
    lines = lines_tbl
  )
}

build_vic_trust_chain_graph <- function(con, cert_id = NULL, msg_id = NULL, ipfs_uri = NULL) {
  dfReqDocChain <- getReqDocChain.ii_vic()
  
  # Retrieve the message ID and IPFS URI if only the certId were provided.
  {
    if (length(msgId) == 0 & length(ipfsUri) == 0) {
      q <- sprintf("SELECT * FROM tbl_impact_certificates WHERE id = '%s';",
                   certId)
      dfCertMd <- dbGetQuery(conn = dbCon, statement = q)
      msgId <- dfCertMd$id_message_h
      ipfsUri <- dfCertMd$uri_ipfs
    }
  }
  
  # Get realised document chain.
  dfReDoCh <- getRealDocChain(
    msgId = msgId,
    ipfsUrl = ipfsUri)
  
  dfReDoCh <- dfReDoCh[which(!is.na(dfReDoCh$iri_schema)),]
  
  # Add some metadata from the db.
  {
    dfDocMd <- dbGetQuery(
      conn = dbCon, 
      statement = sprintf(
        "SELECT id, date_modified, did_author, id_workflow, id_message_h, status, outcome, encrypted, type_doc FROM tbl_document_metadata WHERE id_message_h IN(%s);",
        paste(sprintf("'%s'", unique(dfReDoCh$id_message_h)), 
              sep = "", collapse = ",")))
    dfReDoCh <- merge.data.frame(
      x = dfReDoCh, 
      y = dfDocMd, 
      by = "id_message_h", 
      all.x = TRUE)
  }
  dfRealDocChain <- dfReDoCh
  
  #######################################
  {
    dfNodes <- dfRealDocChain
    dfNodes <- dfNodes[which(!duplicated(dfNodes$id_message_h)),]
    
    # Get workflow names and version numbers.
    {
      q <- sprintf(
        "SELECT * FROM tbl_workflows WHERE id IN(%s);",
        paste(sprintf("'%s'", unique(dfNodes$id_workflow)), 
              sep = "", collapse = ","))
      dfWflws <- dbGetQuery(conn = dbCon, statement = q)
      dfNodes$title_wflw <- NA_character_
      dfNodes$ver_wflw <- NA_character_
      idxx <- match(x = dfNodes$id_workflow, table = dfWflws$id)
      dfNodes$title_wflw[which(!is.na(idxx))] <- dfWflws$title[idxx[!is.na(idxx)]]
      dfNodes$ver_wflw[which(!is.na(idxx))] <- dfWflws$tag_version[idxx[!is.na(idxx)]]
    }
    
    # Get schema names and version numbers.
    {
      dfNodes$iri_schema <- sprintf("#%s", dfNodes$iri_schema)
      q <- sprintf(
        "SELECT * FROM tbl_schemas WHERE iri IN(%s);",
        paste(sprintf("'%s'", unique(dfNodes$iri_schema)), 
              sep = "", collapse = ","))
      dfSchemas <- dbGetQuery(conn = dbCon, statement = q)
      dfNodes$title_schema <- NA_character_
      dfNodes$ver_schema <- NA_character_
      idxx <- match(x = dfNodes$iri_schema, table = dfSchemas$iri)
      dfNodes$title_schema[which(!is.na(idxx))] <- dfSchemas$title[idxx[!is.na(idxx)]]
      dfNodes$ver_schema[which(!is.na(idxx))] <- dfSchemas$tag_version[idxx[!is.na(idxx)]]
    }
    
    # Get schema name abbreviations.
    {
      dfNodes$abbr_schema <- sapply(
        X = dfNodes$title_schema, FUN = function(x) {
          
          idxOpen <- as.integer(gregexpr(pattern = "(", text = x, fixed = TRUE))
          if (length(idxOpen) == 0) { return(NA_character_) }
          idxOpen <- idxOpen[length(idxOpen)]
          
          idxClose <- as.integer(gregexpr(pattern = ")", text = x, fixed = TRUE))
          if (length(idxClose) == 0) { idxClose <- nchar(x) }
          idxClose <- idxClose[length(idxClose)]
          
          return(substr(x, idxOpen + 1, idxClose - 1))
        })
      
      # Determine the schema name abbreviations for the generic review 
      # documents.
      {
        dfNodes$abbr_schema_pred <- NA_character_
        idxx <- match(x = dfNodes$id_msg_pred, table = dfNodes$id_message_h)
        dfNodes$abbr_schema_pred[which(!is.na(idxx))] <- dfNodes$abbr_schema[idxx[!is.na(idxx)]]
        
        idxxDR <- grep(pattern = "Document Review", x = dfNodes$title_schema, ignore.case = TRUE)
        idxxNAabbr <- which(is.na(dfNodes$abbr_schema) | nchar(dfNodes$abbr_schema) == 0 & !is.na(dfNodes$abbr_schema_pred))
        idxx <- intersect(idxxDR, idxxNAabbr)
        dfNodes$abbr_schema[idxx] <- sprintf("DR-%s", dfNodes$abbr_schema_pred[idxx])
        
        dfNodes$abbr_schema_pred <- NULL
      }
      
      # Hack.
      dfNodes$abbr_schema[
        which(dfNodes$title_schema == "Independent Impact Verified Impact Certificate")] <- "VIC"
    }
    
    # Assign node IDs.
    dfNodes$id_node <- 1:nrow(dfNodes)
  }
  
  # First pass: Assign a graph row ("level") to each document.
  {
    dfNodes$lvl <- NA_integer_
    idxx <- match(
      x = dfNodes$abbr_schema,
      table = dfReqDocChain$type_document)
    dfNodes$lvl[which(!is.na(idxx))] <- dfReqDocChain$level[idxx[!is.na(idxx)]]
    
    if (any(is.na(dfNodes$lvl))) {
      idxxNA <- which(is.na(dfNodes$lvl))
      message("WARNING: Dropping ", length(idxxNA), " rows from dfNodes because 'lvl' is NA.")
      dfNodes <- dfNodes[which(!is.na(dfNodes$lvl)),]
      dfNodes$id_node <- dfNodes$id_node - min(dfNodes$id_node) + 1
    }
  }
  
  # Second pass: Assign a graph column to each document.
  {
    dfNodes$col <- NA_integer_
    
    # Determine the number of columns we need in our graph.
    {
      df <- data.frame(
        dplyr::summarise(
          dplyr::group_by(
            dfNodes, lvl),
          n = dplyr::n(),
          .groups = "keep"))
      
      nColsMax <- max(df$n)
      
      # If this is an even number, make it odd, so that we can have a nice
      # centre line.
      if (as.numeric(as.integer(nColsMax/2)) == (nColsMax/2)) {
        nColsMax <- nColsMax + 1
      }
    }
    
    for (lvl in (min(dfReqDocChain$level)):(max(dfReqDocChain$level))) {
      
      idxxNodes <- which(dfNodes$lvl == lvl)
      
      # If there are multiple, sort them correctly.
      if (length(idxxNodes) > 1) {
        idxxOrdr <- match(
          table = dfNodes$abbr_schema[idxxNodes], 
          x = dfReqDocChain$type_document[which(dfReqDocChain$level == lvl)])
        idxxNodes <- idxxNodes[idxxOrdr]
      }
      
      idxxCols <- 1:length(idxxNodes)
      idxxCols <- idxxCols + ((nColsMax - length(idxxCols)) / 2)
      dfNodes$col[idxxNodes] <- idxxCols
    }
  }
  
  # Tag levels with problems.
  # Tag individual nodes with problems.
  # Add empty nodes for missing documents.
  {
    # TODO.
  }
  
  dfNodes$id_msg_pred <- NULL
  dfNodes$url_ipfs_pred <- NULL
  
  # Reverse the levels for display purposes.
  dfNodes$lvl <- (max(dfNodes$lvl)+1) - dfNodes$lvl
  
  # Join schema name and version for convenience.
  dfNodes$lbl_schema <- sprintf(
    "%s (v%s)",
    dfNodes$title_schema,
    dfNodes$ver_schema)
  
  # Make a shortened version of did_issuer / did_author for display convenience.
  {
    for (vnm in c("did_issuer", "did_author")) {
      
      dfNodes[[paste0(vnm, "_short")]] <- sprintf(
        "did:%s...%s",
        substr(dfNodes[[vnm]], start = 20, stop = 30),
        substr(dfNodes[[vnm]],
               start = nchar(dfNodes[[vnm]])-10,
               stop = nchar(dfNodes[[vnm]])))
      
    }
    dfNodes$did_issuer_short[which(is.na(dfNodes$did_issuer))] <- NA_character_
    dfNodes$did_author_short[which(is.na(dfNodes$did_author))] <- NA_character_
  }
  
  # Make a shortened version of the IPFS URL for display convenience.
  {
    dfNodes$tmp <- nchar(dfNodes$url_ipfs)
    dfNodes$url_ipfs_short <- sprintf(
      "%s...%s",
      substr(dfNodes$url_ipfs, start = 1, stop = 15),
      substr(dfNodes$url_ipfs,
             start = dfNodes$tmp - 19,
             stop = dfNodes$tmp))
    dfNodes$tmp <- NULL
  }
  
  # Retrieve the issuer's / author's email address.
  {
    # Get the agent IDs from the DIDs.
    q <- sprintf("SELECT * FROM tbl_link_agents_x_dids WHERE did IN(%s);",
                 paste(sprintf("'%s'", unique(dfNodes$did_author)),
                       collapse = ","))
    dfAgDids <- dbGetQuery(conn = dbCon, statement = q)
    dfNodes <- merge.data.frame(
      x = dfNodes,
      y = dfAgDids,
      by.x = "did_author",
      by.y = "did",
      all.x = TRUE,
      all.y = FALSE)
    
    # Get all email addresses for the agents in question.
    q <- sprintf(
      "SELECT * FROM tbl_link_agents_x_email_addresses WHERE id_agent IN(%s);",
      paste(sprintf("'%s'", dfAgDids$id_agent),
            collapse = ","))
    dfEmAddrs <- dbGetQuery(conn = dbCon, statement = q)
    
    # Subset to only the most recent email address for each agent.
    dfEmAddrs <- dfEmAddrs[order(dfEmAddrs$oidx, decreasing = TRUE),]
    rownames(dfEmAddrs) <- 1:nrow(dfEmAddrs)
    dfEmAddrs <- dfEmAddrs[which(!duplicated(dfEmAddrs$id_agent)),]
    rownames(dfEmAddrs) <- 1:nrow(dfEmAddrs)
    
    # Merge with dfNodes.
    dfNodes <- merge.data.frame(
      x = dfNodes,
      y = dfEmAddrs,
      by = "id_agent",
      all.x = TRUE,
      all.y = FALSE)
  }
  
  # # Make y axis a date. Place each level at the minimum timestamp for that level.
  {
    dfNodes$date_modified <- lubridate::as_datetime(dfNodes$date_modified)
    # dfY <- data.frame(
    #   dplyr::summarise(
    #     dplyr::group_by(
    #       dfNodes, lvl),
    #     ts_lvl = min(date_modified),
    #     .groups = "keep"))
    # dfNodes <- merge.data.frame(x = dfNodes, y = dfY, by = "lvl", all.x = TRUE)
  }
  
  
  ################## Build dfLines for plotting document chain.
  
  # Copy id_node to dfRealDocChain.
  idxx <- match(x = dfRealDocChain$url_ipfs,
                table = dfNodes$url_ipfs)
  dfRealDocChain$id_node <- dfNodes$id_node[idxx]
  
  # Build dfLines.
  dfLines <- lapply(X = 1:nrow(dfRealDocChain), FUN = function(r) {
    
    if (is.na(dfRealDocChain$url_ipfs_pred[r])) { return(NULL) }
    if (is.na(dfRealDocChain$id_node[r])) { return(NULL) }
    
    idxNode <- which(dfNodes$id_node == dfRealDocChain$id_node[r])
    xFrom <- dfNodes$col[idxNode]
    yFrom <- dfNodes$lvl[idxNode]
    
    xTo <- NA_real_
    yTo <- NA_real_
    idxNode <- which(dfNodes$url_ipfs == dfRealDocChain$url_ipfs_pred[r])
    if (length(idxNode) == 1) {
      xTo <- dfNodes$col[idxNode]
      yTo <- dfNodes$lvl[idxNode]
    }
    
    df <- data.frame(
      no = c(r, r, r),
      x = c(xFrom, xTo, 1),
      y = c(yFrom, yTo, NA_real_))
    
    return(df)
  })
  dfLines <- do.call("rbind.fill", dfLines)
  
  
  list(
    nodes = dfNodes,
    lines = dfLines)
}

get_workflow_details <- function(con, workflow_id) {
  wf <- dbGetQuery(
    con,
    "SELECT * FROM tbl_workflows WHERE id = $1;",
    params = list(workflow_id)
  )
  if (nrow(wf) == 0) {
    stop("Workflow not found", call. = FALSE)
  }
  wf
}

workflow_primary_agents <- function(con, subject, workflow_id, entity_id = NULL, fallback_agent = NULL) {
  if (identical(subject, "AGENT")) {
    if (is.null(entity_id) || !nzchar(entity_id)) {
      entity_id <- fallback_agent
    }
    return(unique(na.omit(entity_id)))
  }

  if (is.null(entity_id) || !nzchar(entity_id)) {
    stop("entity_id (e.g., project_id) required for non-agent workflows", call. = FALSE)
  }

  prim <- dbGetQuery(
    con,
    "SELECT id_agent FROM tbl_link_projects_x_agents WHERE id_project = $1 AND id_workflow = $2 AND role = 'PRIMARY_AGENT';",
    params = list(entity_id, workflow_id)
  )
  unique(prim$id_agent)
}

workflow_agent_role <- function(con, subject, workflow_id, entity_id = NULL, agent_id = NULL) {
  if (is.null(agent_id) || !nzchar(agent_id)) {
    return("VIEWER")
  }
  if (identical(subject, "AGENT")) {
    return("PRIMARY_AGENT")
  }
  if (is.null(entity_id) || !nzchar(entity_id)) {
    return("VIEWER")
  }
  role_info <- dbGetQuery(
    con,
    "SELECT role FROM tbl_link_projects_x_agents WHERE id_project = $1 AND id_workflow = $2 AND id_agent = $3;",
    params = list(entity_id, workflow_id, agent_id)
  )
  if (nrow(role_info) == 0 || is.na(role_info$role[1]) || !nzchar(role_info$role[1])) {
    return("VIEWER")
  }
  role_info$role[1]
}

workflow_step_map <- function(handler_r, workflow_id, con) {
  if (!exists("getWorkflowStepMap", mode = "function")) {
    return(tibble())
  }
  tryCatch(
    {
      df <- getWorkflowStepMap(
        nmWrkflwHndlr = handler_r,
        idWorkflow = workflow_id,
        dbCon = con
      )
      cleanJSONCols(tibble::as_tibble(df))
    },
    error = function(e) tibble()
  )
}

workflow_state_snapshot <- function(con, workflow_row, agent_id = NULL, entity_id = NULL) {
  handler_r <- workflow_row$handler_r[1]
  state_fun <- sprintf("getState.%s", handler_r)
  if (!exists(state_fun, mode = "function")) {
    stop(sprintf("Workflow state handler '%s' not found", state_fun), call. = FALSE)
  }

  subject <- workflow_row$subject[1]
  entity <- entity_id
  if (identical(subject, "AGENT")) {
    entity <- if (!is.null(entity_id) && nzchar(entity_id)) entity_id else agent_id
  }
  prim_agents <- workflow_primary_agents(
    con = con,
    subject = subject,
    workflow_id = workflow_row$id[1],
    entity_id = entity,
    fallback_agent = agent_id
  )

  if (length(prim_agents) == 0 || all(is.na(prim_agents))) {
    prim_agents <- agent_id
  }

  state_df <- do.call(
    state_fun,
    list(
      idPrimAgents = prim_agents,
      idEntity = entity,
      dbCon = con,
      idWorkflow = workflow_row$id[1]
    )
  )
  cleanJSONCols(tibble::as_tibble(state_df))
}

# ------------------------------------------------------------------------------
# Routes shared across services
# ------------------------------------------------------------------------------

#* @get /health
#* @tag System
#* @description Lightweight probe that returns the service health status.
#* @response 200 A list containing `status = "OK"`.
function() {
  list(status = "OK")
}

#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin",  "*")
  res$setHeader("Access-Control-Allow-Methods", "GET,POST,OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Accept, Authorization")
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  }
  plumber::forward()
}

# ------------------------------------------------------------------------------
# Auth & account endpoints (from plumber.R)
# ------------------------------------------------------------------------------

#* @post /login
#* @tag Auth
#* @description Authenticate an Independent Impact agent and return their Guardian refresh token.
#* @param req Plumber request object containing a JSON body with `email` and `password`.
#* @param res Plumber response object used to set HTTP status codes.
#* @response 200 Authentication succeeded; the response includes the agent id, type, DID, and refresh token.
#* @response 400 The request body is missing or malformed, or the email/password is not provided.
#* @response 401 The supplied password does not match the stored hash.
#* @response 500 An unexpected database inconsistency prevented login.
#* @response 502 Guardian authentication failed for the generated credentials.
#* @serializer json
function(req, res) {
  if (is.null(req$postBody) || nchar(req$postBody) == 0) {
    res$status <- 400
    return(list(error = "Request body is empty"))
  }

  body <- parse_json_body(req)
  if (!is.list(body) || length(body) == 0) {
    res$status <- 400
    return(list(error = "Malformed JSON"))
  }

  #body <- fromJSON('{"email": "abcde12345@nomail.com", "password":"aBcDe_54321"}')
  email <- first_non_empty(body$email)
  password <- first_non_empty(body$password)

  if (is.null(email) || is.null(password)) {
    res$status <- 400
    return(list(error = "Missing email or password"))
  }

  print(paste(as.character(Sys.time()), "Login attempt for email:", email))

  con <- getDbCon()
  on.exit(dbDisconnect(con), add = TRUE)

  auth <- tryCatch(
    authenticate_user_credentials(con, email, password),
    error = function(e) {
      status_code <- if (grepl("Incorrect password", e$message, fixed = TRUE)) 401 else 400
      res$status <- status_code
      list(error = e$message)
    }
  )
  if (is.list(auth) && !is.null(auth$error)) {
    return(auth)
  }

  list(
    email_address = auth$email,
    id_agent = auth$agent_id,
    userType = auth$user_type,
    did = auth$did,
    message = "Welcome."
  )
  
}

#* @parser json
#* @post /register
#* @tag Auth
#* @description Register a new Independent Impact agent using the same workflow as the Shiny module.
#* @param req Plumber request object containing JSON body with `email`, `password`, and `accept_terms`.
#* @param res Plumber response object used to set HTTP status codes.
#* @response 201 Agent created successfully; the response includes the new agent id, DID, and Hedera account.
#* @response 400 Validation failed (missing fields, invalid email/password, or terms not accepted).
#* @response 409 Email address already registered.
#* @response 500 Account creation failed due to an internal error.
#* @serializer json
function(req, res) {
  body <- parse_json_body(req)
  email <- first_non_empty(body$email, body$email_address)
  password <- first_non_empty(body$password)
  accept_terms <- isTRUE(body$accept_terms)

  result <- tryCatch(
    create_agent_account(email = email, password = password, accept_terms = accept_terms),
    plumber_account_error = function(e) {
      res$status <- e$status
      list(error = e$message)
    },
    error = function(e) {
      res$status <- 500
      list(error = conditionMessage(e))
    }
  )

  if (!is.null(result$error)) {
    return(result)
  }

  res$status <- 201
  result
}

# ------------------------------------------------------------------------------
# Agent endpoints (from plumber2)
# ------------------------------------------------------------------------------

#* @get /agents/<agent_id>/profile
#* @tag Agents
#* @description Retrieve the agent's profile record along with linked DIDs, Hedera accounts, and email addresses.
#* @param agent_id:path Unique agent identifier.
#* @param res Plumber response object used to set HTTP status codes.
#* @response 200 Profile attributes grouped by resource type.
#* @response 400 The request did not include an `agent_id`.
#* @response 404 No agent exists for the supplied identifier.
function(agent_id, res) {
  if (missing(agent_id) || is.null(agent_id) || !nzchar(agent_id)) {
    res$status <- 400
    return(list(error = "Missing agent_id"))
  }

  con <- getDbCon()
  on.exit(dbDisconnect(con), add = TRUE)

  overview <- get_agent_overview(con, agent_id)

  if (nrow(overview$profile) == 0) {
    res$status <- 404
    return(list(error = "Agent not found"))
  }

  list(
    agent = overview$profile,
    dids = overview$dids,
    hedera_accounts = overview$hedera_accounts,
    emails = overview$emails
  )
}

#* @get /agents/<agent_id>/reputation
#* @tag Agents
#* @description Return the aggregate reputation metrics stored for a given agent.
#* @param agent_id:path Unique agent identifier.
#* @param res Plumber response object used to set HTTP status codes.
#* @response 200 Reputation metrics returned (empty tibble when no data exists).
#* @response 400 The request did not include an `agent_id`.
function(agent_id, res) {
  if (missing(agent_id) || is.null(agent_id) || !nzchar(agent_id)) {
    res$status <- 400
    return(list(error = "Missing agent_id"))
  }

  con <- getDbCon()
  on.exit(dbDisconnect(con), add = TRUE)

  rep <- get_agent_overview(con, agent_id)$reputation

  list(reputation = rep)
}

#* @get /agents/<agent_id>/credential-workflows
#* @tag Agents
#* @description List credential workflows relevant to the agent, prioritising workflows already linked to their documents.
#* @param agent_id:path Unique agent identifier.
#* @param res Plumber response object used to set HTTP status codes.
#* @response 200 Matching workflow records, or the latest workflow version when none are linked.
#* @response 400 The request did not include an `agent_id`.
function(agent_id, res) {
  if (missing(agent_id) || is.null(agent_id) || !nzchar(agent_id)) {
    res$status <- 400
    return(list(error = "Missing agent_id"))
  }

  con <- getDbCon()
  on.exit(dbDisconnect(con), add = TRUE)

  workflows <- get_agent_overview(con, agent_id)$workflows

  list(workflows = workflows)
}

#* @get /agents/<agent_id>/credentials
#* @tag Agents
#* @description Collate verifiable credential details for the agent by joining Guardian tokens and policy metadata.
#* @param agent_id:path Unique agent identifier.
#* @param res Plumber response object used to set HTTP status codes.
#* @response 200 Credential rows enriched with token metadata (empty when no credentials exist).
#* @response 400 The request did not include an `agent_id`.
function(agent_id, res) {
  require(plyr)
  if (missing(agent_id) || is.null(agent_id) || !nzchar(agent_id)) {
    res$status <- 400
    return(list(error = "Missing agent_id"))
  }

  dbCon <- getDbCon()
  on.exit(dbDisconnect(dbCon), add = TRUE)

  did <- get_agent_did(dbCon, agent_id)
  if (is.na(did)) {
    return(list(credentials = tibble(), message = "Agent has no DID on record"))
  }

  q <- sprintf("SELECT * FROM tbl_policies WHERE subject = 'AGENT';")
  dfCredPolicies <- dbGetQuery(conn = dbCon, statement = q)
  dfCredPolicies$oidx <- NULL

  q <- sprintf("SELECT * FROM tbl_link_tokens_x_policies WHERE id_message_policy IN(%s);",
               paste(sprintf("'%s'", dfCredPolicies$id_message),
                     collapse = ","))
  dfMapTknsPolicies <- dbGetQuery(conn = dbCon, statement = q)
  dfMapTknsPolicies$oidx <- NULL

  q <- sprintf("SELECT * FROM tbl_tokens WHERE id IN(%s);",
               paste(sprintf("'%s'", dfMapTknsPolicies$id_token),
                     collapse = ","))
  dfTokens <- dbGetQuery(conn = dbCon, statement = q)
  dfTokens$oidx <- NULL

  names(dfCredPolicies) <- sprintf("p_%s", names(dfCredPolicies))
  names(dfTokens) <- sprintf("t_%s", names(dfTokens))

  df <- dfMapTknsPolicies; rm(dfMapTknsPolicies)
  df <- merge.data.frame(
    x = dfTokens,
    y = df,
    by.x = "t_id",
    by.y = "id_token",
    all = TRUE)
  df <- merge.data.frame(
    x = df,
    y = dfCredPolicies,
    by.x = "id_message_policy",
    by.y = "p_id_message",
    all = TRUE)
  dfMapAllCredPolsToTkns <- df



  q <- sprintf("SELECT * FROM tbl_agents WHERE id = '%s';", agent_id)
  dfAgent <- dbGetQuery(conn = dbCon, statement = q)
  dfAgent$oidx <- NULL

  q <- sprintf("SELECT * FROM tbl_link_agents_x_dids WHERE id_agent = '%s';", agent_id)
  dfDids <- dbGetQuery(conn = dbCon, statement = q)
  dfDids$oidx <- NULL

  q <- sprintf("SELECT * FROM tbl_link_agents_x_hedera_accounts WHERE id_agent = '%s';", agent_id)
  dfHaccs <- dbGetQuery(conn = dbCon, statement = q)
  dfHaccs$oidx <- NULL

  hedera_accounts <- unique(dfHaccs$id_acc_h)
  hedera_accounts <- hedera_accounts[!is.na(hedera_accounts)]
  if (length(hedera_accounts) == 0) {
    return(list(credentials = tibble(), message = "Agent has no Hedera accounts associated."))
  }


  # Get the list of tokens associated with this agent.
  q <- sprintf("SELECT * FROM tbl_link_agents_x_tokens WHERE id_agent = '%s';",agent_id)
  dfAgentCreds <- dbGetQuery(conn = dbCon, statement = q)
  if (nrow(dfAgentCreds) == 0) {
    dfAgentCreds <- NULL
    return(invisible(0))
  }

  dfTkns <- dfMapAllCredPolsToTkns
  dfTkns <- dfTkns[which(dfTkns$t_id %in% dfAgentCreds$id_token),]
  rm(dfAgentCreds)

  vnms <- grep(pattern = "^t_", x = names(dfTkns), value = TRUE)
  dfTkns <- dfTkns[vnms]
  names(dfTkns) <- gsub(pattern = "^t_", replacement = "", x = names(dfTkns))
  dfTkns <- dfTkns[which(!duplicated(dfTkns$id_topic)),]

  # # Retrieve the VP metadata.
  {
    # Note: Querying Hedera network directly, instead of relying on Guardian.
    lsdfNfts <- sapply(X = dfTkns$id_token_h, FUN = function(idTkn) {
      return(
    lsdf <- lapply(X = hedera_accounts, FUN = function(idAccH) {
          return(
            hedera::getNfts(
              tokenId = idTkn,
              accountId = idAccH,
              network = hederaNetwork,
              asDf = TRUE))
        }))
      df <- do.call("rbind.fill", lsdf); rm(lsdf)
      return(df)
    })
    dfNfts <- do.call("rbind.fill", lsdfNfts); rm(lsdfNfts)
    if (length(dfNfts) == 0) { return(invisible(NULL)) }

    dfNfts <- dfNfts[which(!dfNfts$deleted),]
    if (nrow(dfNfts) == 0) { return(invisible(NULL)) }

    dfNfts[c("deleted",
             "created_timestamp", "modified_timestamp",
             "delegating_spender", "spender")] <- NULL
  }

  # Add message ID, date of issuance, token symbol.
  {
    # id_message
    dfNfts$id_message <- sapply(X = dfNfts$metadata, FUN = function(x) {
      rawToChar(openssl::base64_decode(x))
    })
    dfNfts$metadata <- NULL

    # date_issued
    dfNfts$date_issued <- as.POSIXct(
      x = as.numeric(dfNfts$id_message),
      origin = "1970-01-01 00:00:00.0",
      tz = "UTC")

    # symbol
    idxx <- match(x = dfNfts$token_id, table = dfTkns$id_token_h)
    dfNfts$symbol <- dfTkns$symbol[idxx]
  }

  # Rename some variables.
  dfNfts <- dplyr::rename(
    dfNfts,
    id_acc_h = account_id,
    serial_no = serial_number,
    id_token_h = token_id)

  # Done.
  dfAgentCreds <- dfNfts


  # Add 'View' buttons.
  dfAgentCreds$btn_view <- "View"

  # Subset to display vars.
  vnms <- c("symbol", "id_acc_h", "serial_no", "date_issued", "btn_view")
  vnmsDtoAgentCreds <- vnms
  dfAgentCreds <- dfAgentCreds[vnms]
  names(dfAgentCreds) <- c("Symbol", "Hedera Account ID", "Serial No.",
                           "Date Issued", "Action")

  list(credentials = tibble(dfAgentCreds))
}

#* @get /agents/<agent_id>/documents
#* @tag Agents
#* @description Retrieve document metadata records owned by the specified agent.
#* @param agent_id:path Unique agent identifier.
#* @param res Plumber response object used to set HTTP status codes.
#* @response 200 Document metadata ordered by last modification date.
#* @response 400 The request did not include an `agent_id`.
function(agent_id, res) {
  if (missing(agent_id) || is.null(agent_id) || !nzchar(agent_id)) {
    res$status <- 400
    return(list(error = "Missing agent_id"))
  }

  con <- getDbCon()
  on.exit(dbDisconnect(con), add = TRUE)

  docs <- dbGetQuery(
    con,
    "SELECT *
     FROM tbl_document_metadata
     WHERE id_entity = $1
     ORDER BY date_modified DESC;",
    params = list(agent_id)
  )

  list(documents = cleanJSONCols(tibble::as_tibble(docs)))
}

#* @get /agents/<agent_id>/licenses
#* @tag Agents
#* @description Fetch Guardian license records for an agent filtered by scope and status.
#* @param agent_id:path Unique agent identifier.
#* @param scopes:string Comma separated scope values (e.g. `PROJECT_DEVELOPER,PDD_VALIDATOR`).
#* @param statuses:string Optional comma separated statuses (defaults to `ACTIVE`).
#* @param res Plumber response object used to set HTTP status codes.
#* @response 200 Filtered license rows (empty when none match); may include a note for implicit standard registry access.
#* @response 400 The request omitted the `agent_id` or `scopes` parameters.
#* @response 404 Agent not found in the agents table.
function(agent_id, scopes, statuses = NULL, res) {
  if (missing(agent_id) || is.null(agent_id) || !nzchar(agent_id)) {
    res$status <- 400
    return(list(error = "Missing agent_id"))
  }

  scopes_vec <- split_param(scopes)
  if (length(scopes_vec) == 0) {
    res$status <- 400
    return(list(error = "Parameter 'scopes' is required"))
  }

  statuses_vec <- split_param(statuses)
  if (length(statuses_vec) == 0) {
    statuses_vec <- "ACTIVE"
  }

  con <- getDbCon()
  on.exit(dbDisconnect(con), add = TRUE)

  type_user <- dbGetQuery(
    con,
    "SELECT type_user FROM tbl_agents WHERE id = $1;",
    params = list(agent_id)
  )
  if (nrow(type_user) == 0) {
    res$status <- 404
    return(list(error = "Agent not found"))
  }

  if ("STANDARDS_BODY" %in% scopes_vec && type_user$type_user[1] == "STANDARD_REGISTRY") {
    std_df <- tibble::tibble(
      date_issued = "(STANDARDS_BODY)",
      id_message_h = "(STANDARDS_BODY)",
      uri_ipfs = "(STANDARDS_BODY)",
      scope = "STANDARDS_BODY",
      status = "ACTIVE"
    )
    return(list(licenses = std_df, note = "Standard registry access granted implicitly"))
  }

  overview <- get_agent_overview(con, agent_id)
  agent_did <- first_non_empty(overview$profile$did)
  if (is.null(agent_did) || is.na(agent_did)) {
    return(list(licenses = tibble(), message = "Agent has no DID on record"))
  }

  df <- overview$licenses
  if (nrow(df) > 0) {
    df <- df[df$scope %in% scopes_vec & df$status %in% statuses_vec, , drop = FALSE]
    df <- df[order(df$date_issued, decreasing = TRUE), , drop = FALSE]
  }

  msg <- NULL
  if (nrow(df) == 0) {
    msg <- sprintf(
      "No licenses found for scopes [%s] with statuses [%s].",
      paste(scopes_vec, collapse = ", "),
      paste(statuses_vec, collapse = ", ")
    )
  }

  list(
    licenses = cleanJSONCols(tibble::as_tibble(df)),
    message = msg
  )
}

# ------------------------------------------------------------------------------
# Workflow endpoints (from plumber2)
# ------------------------------------------------------------------------------

#* @get /workflows
#* @tag Workflows
#* @description List workflows filtered by subject and/or status.
#* @param subject:string Optional subject filter (e.g. `AGENT` or `PROJECT`).
#* @param status:string Optional workflow status filter (e.g. `ACTIVE`).
#* @response 200 A tibble of workflow rows matching the provided filters.
function(subject = NULL, status = NULL) {
  con <- getDbCon()
  on.exit(dbDisconnect(con), add = TRUE)

  subject <- first_non_empty(subject)
  status <- first_non_empty(status)

  if (!is.null(subject) && !is.null(status)) {
    wf <- dbGetQuery(
      con,
      "SELECT * FROM tbl_workflows WHERE subject = $1 AND status = $2 ORDER BY title;",
      params = list(subject, status)
    )
  } else if (!is.null(subject)) {
    wf <- dbGetQuery(
      con,
      "SELECT * FROM tbl_workflows WHERE subject = $1 ORDER BY title;",
      params = list(subject)
    )
  } else if (!is.null(status)) {
    wf <- dbGetQuery(
      con,
      "SELECT * FROM tbl_workflows WHERE status = $1 ORDER BY title;",
      params = list(status)
    )
  } else {
    wf <- dbGetQuery(
      con,
      "SELECT * FROM tbl_workflows ORDER BY title;"
    )
  }

  list(workflows = cleanJSONCols(tibble::as_tibble(wf)))
}

#* @get /workflows/<workflow_id>
#* @tag Workflows
#* @description Retrieve workflow metadata and its handler step map.
#* @param workflow_id:path Workflow identifier.
#* @param res Plumber response object used to set HTTP status codes.
#* @response 200 Workflow metadata plus the resolved step map.
#* @response 400 The request did not include a `workflow_id`.
#* @response 404 No workflow matches the supplied identifier.
#* @response 500 Unexpected database errors while fetching workflow details.
function(workflow_id, res) {
  if (missing(workflow_id) || is.null(workflow_id) || !nzchar(workflow_id)) {
    res$status <- 400
    return(list(error = "Missing workflow_id"))
  }

  con <- getDbCon()
  on.exit(dbDisconnect(con), add = TRUE)

  wf <- tryCatch(
    get_workflow_details(con, workflow_id),
    error = function(e) {
      res$status <- if (grepl("not found", e$message, fixed = TRUE)) 404 else 500
      list(.error = TRUE, message = e$message)
    }
  )
  if (is.list(wf) && isTRUE(wf$.error)) {
    return(list(error = wf$message))
  }

  step_map <- workflow_step_map(wf$handler_r[1], workflow_id, con)

  list(
    workflow = cleanJSONCols(tibble::as_tibble(wf)),
    step_map = step_map
  )
}

#* @get /workflows/<workflow_id>/entities
#* @tag Workflows
#* @description List entity identifiers that have submitted documents for a workflow.
#* @param workflow_id:path Workflow identifier.
#* @param res Plumber response object used to set HTTP status codes.
#* @param limit:integer Optional limit for the number of entities returned (defaults to 1000).
#* @response 200 Distinct entity identifiers linked to the workflow.
#* @response 400 The request did not include a `workflow_id`.
#* @response 404 No workflow matches the supplied identifier.
#* @response 500 Unexpected database errors while fetching workflow details.
function(workflow_id, res, limit = 1000) {
  if (missing(workflow_id) || is.null(workflow_id) || !nzchar(workflow_id)) {
    res$status <- 400
    return(list(error = "Missing workflow_id"))
  }

  limit_int <- suppressWarnings(as.integer(limit))
  if (is.na(limit_int) || limit_int <= 0) limit_int <- 1000

  con <- getDbCon()
  on.exit(dbDisconnect(con), add = TRUE)

  wf <- tryCatch(
    get_workflow_details(con, workflow_id),
    error = function(e) {
      res$status <- if (grepl("not found", e$message, fixed = TRUE)) 404 else 500
      list(.error = TRUE, message = e$message)
    }
  )
  if (is.list(wf) && isTRUE(wf$.error)) {
    return(list(error = wf$message))
  }
  subject <- wf$subject[1]

  sql <- sprintf(
    "SELECT DISTINCT id_entity FROM tbl_document_metadata WHERE id_workflow = $1 ORDER BY id_entity LIMIT %d;",
    limit_int
  )
  entities <- dbGetQuery(con, sql, params = list(workflow_id))

  list(
    workflow_subject = subject,
    entities = cleanJSONCols(tibble::as_tibble(entities))
  )
}

#* @get /workflows/<workflow_id>/documents
#* @tag Workflows
#* @description Retrieve document metadata for an entity within a workflow.
#* @param workflow_id:path Workflow identifier.
#* @param res Plumber response object used to set HTTP status codes.
#* @param entity_id:string Entity identifier whose documents should be returned.
#* @param status:string Optional comma-separated list of document statuses to filter by.
#* @param limit:integer Optional limit for the number of documents returned (defaults to 500).
#* @response 200 Document metadata rows filtered by entity and status.
#* @response 400 The request omitted either `workflow_id` or `entity_id`.
#* @response 404 No workflow matches the supplied identifier.
#* @response 500 Unexpected database errors while fetching workflow details.
function(workflow_id, res, entity_id, status = NULL, limit = 500) {
  if (missing(workflow_id) || is.null(workflow_id) || !nzchar(workflow_id)) {
    res$status <- 400
    return(list(error = "Missing workflow_id"))
  }
  entity_id <- first_non_empty(entity_id)
  if (is.null(entity_id)) {
    res$status <- 400
    return(list(error = "entity_id is required"))
  }

  limit_int <- suppressWarnings(as.integer(limit))
  if (is.na(limit_int) || limit_int <= 0) limit_int <- 500

  status_vec <- split_param(status)

  con <- getDbCon()
  on.exit(dbDisconnect(con), add = TRUE)

  wf <- tryCatch(
    get_workflow_details(con, workflow_id),
    error = function(e) {
      res$status <- if (grepl("not found", e$message, fixed = TRUE)) 404 else 500
      list(.error = TRUE, message = e$message)
    }
  )
  if (is.list(wf) && isTRUE(wf$.error)) {
    return(list(error = wf$message))
  }

  params <- list(workflow_id, entity_id)
  sql <- "SELECT * FROM tbl_document_metadata WHERE id_workflow = $1 AND id_entity = $2"
  if (length(status_vec) > 0) {
    sql <- paste(sql, "AND status = ANY($3::text[])")
    params <- c(params, list(status_vec))
  }
  sql <- paste(sql, sprintf("ORDER BY date_created DESC LIMIT %d;", limit_int))

  docs <- dbGetQuery(con, sql, params = params)

  list(documents = cleanJSONCols(tibble::as_tibble(docs)))
}

#* @post /workflows/<workflow_id>/state
#* @tag Workflows
#* @description Return a workflow snapshot for an agent or project entity.
#* @param workflow_id:path Workflow identifier.
#* @param req Plumber request object containing a JSON body with `agentId`/`entityId`.
#* @param res Plumber response object used to set HTTP status codes.
#* @response 200 Workflow metadata, the agent's role, and the current state matrix.
#* @response 400 Required identifiers are missing from the request.
#* @response 500 Snapshot retrieval failed; see error message for details.
#* @serializer json
function(workflow_id, req, res) {
  if (missing(workflow_id) || is.null(workflow_id) || !nzchar(workflow_id)) {
    res$status <- 400
    return(list(error = "Missing workflow_id"))
  }

  body <- parse_json_body(req)
  agent_id <- first_non_empty(body$agentId, body$agent_id, body$id_agent)
  entity_id <- first_non_empty(body$entityId, body$projectId, body$id_project, body$entity_id)

  con <- getDbCon()
  on.exit(dbDisconnect(con), add = TRUE)

  wf <- get_workflow_details(con, workflow_id)
  subject <- wf$subject[1]

  if (!identical(subject, "AGENT") && is.null(entity_id)) {
    res$status <- 400
    return(list(error = "entityId is required for project workflows"))
  }
  if (identical(subject, "AGENT") && is.null(agent_id)) {
    res$status <- 400
    return(list(error = "agentId is required for agent workflows"))
  }

  agent_role <- workflow_agent_role(
    con = con,
    subject = subject,
    workflow_id = workflow_id,
    entity_id = entity_id,
    agent_id = agent_id
  )

  state_res <- tryCatch(
    workflow_state_snapshot(
      con = con,
      workflow_row = wf,
      agent_id = agent_id,
      entity_id = entity_id
    ),
    error = function(e) {
      res$status <- 500
      list(.error = TRUE, message = e$message)
    }
  )
  if (is.list(state_res) && isTRUE(state_res$.error)) {
    return(list(error = state_res$message))
  }
  state_tbl <- cleanJSONCols(tibble::as_tibble(state_res))

  list(
    workflow = cleanJSONCols(tibble::as_tibble(wf)),
    agent_role = agent_role,
    state = state_tbl
  )
}

# ------------------------------------------------------------------------------
# Trust-chain endpoints (from plumber2)
# ------------------------------------------------------------------------------

#* @post /trust-chain
#* @tag TrustChain
#* @description Build a trust chain graph for a Guardian token using optional message and IPFS context.
#* @param req Plumber request object containing JSON fields `tokenIdH`, `msgId`, and `ipfsUrl`.
#* @param res Plumber response object used to set HTTP status codes.
#* @response 200 Graph data describing the trust chain for the supplied token.
#* @response 400 The request body did not include a token identifier.
#* @response 500 Trust chain construction failed due to an internal error.
#* @serializer json
function(req, res) {
  body <- parse_json_body(req)
  token_id <- first_non_empty(body$tokenIdH, body$token_id, body$id_token_h)
  msg_id <- first_non_empty(body$msgId, body$id_message_h, body$message_id)
  ipfs_url <- first_non_empty(body$ipfsUrl, body$uri_ipfs, body$url_ipfs)

  if (is.null(token_id)) {
    res$status <- 400
    return(list(error = "tokenIdH is required"))
  }

  con <- getDbCon()
  on.exit(dbDisconnect(con), add = TRUE)

  result <- tryCatch(
    build_trust_chain_graph(con, token_id_h = token_id, msg_id = msg_id, ipfs_url = ipfs_url),
    error = function(e) {
      res$status <- 500
      list(error = e$message)
    }
  )

  result
}

#* @post /trust-chain/vic
#* @tag TrustChain
#* @description Build a trust chain graph for a Verifiable Integrity Certificate (VIC).
#* @param req Plumber request object containing `certId` or a combination of `msgId` and `ipfsUri`.
#* @param res Plumber response object used to set HTTP status codes.
#* @response 200 Graph data describing the VIC trust chain.
#* @response 400 The request did not include either a certificate id or both message id and IPFS URI.
#* @response 500 Trust chain graph construction failed due to an internal error.
#* @serializer json
function(req, res) {
  body <- parse_json_body(req)
  cert_id <- first_non_empty(body$certId, body$certificateId, body$id_certificate)
  msg_id <- first_non_empty(body$msgId, body$id_message_h)
  ipfs_uri <- first_non_empty(body$ipfsUri, body$ipfsUrl, body$uri_ipfs)

  if (is.null(cert_id) && (is.null(msg_id) || is.null(ipfs_uri))) {
    res$status <- 400
    return(list(error = "Provide certId or msgId/ipfsUri"))
  }

  con <- getDbCon()
  on.exit(dbDisconnect(con), add = TRUE)

  result <- tryCatch(
    build_vic_trust_chain_graph(
      con = con,
      cert_id = cert_id,
      msg_id = msg_id,
      ipfs_uri = ipfs_uri
    ),
    error = function(e) {
      res$status <- 500
      list(error = e$message)
    }
  )

  result
}

# ------------------------------------------------------------------------------
# Project endpoints (from plumber.R plus extensions)
# ------------------------------------------------------------------------------

#* @get /projects
#* @tag Projects
#* @description List projects, splitting into those created by the requesting agent and all others.
#* @param user_id:string Optional agent identifier used to separate owned projects from other projects.
#* @response 200 Two tibble collections: `my_projects` and `other_projects`.
function(user_id = NULL) {
  con <- getDbCon()
  on.exit(dbDisconnect(con), add = TRUE)
  print(paste("Fetching projects for user_id:", user_id))
  agent_id <- first_non_empty(user_id)
  projects <- list_projects_for_agent(con, agent_id)

  list(
    my_projects = projects$my_projects,
    other_projects = projects$other_projects
  )
}

#* @post /projects
#* @tag Projects
#* @description Create a new project owned by the specified agent.
#* @param user_id:string Creator agent ID.
#* @param body:json Request payload containing a `title` field.
#* @param req Plumber request object used to access the raw JSON body.
#* @param res Plumber response object used to set HTTP status codes.
#* @response 201 Project created successfully; the response contains the new project id.
#* @response 400 Validation failed for the provided title or request body.
#* @response 409 A project with the same title already exists.
function(req, res, user_id) {
  body <- parse_json_body(req)
  title <- first_non_empty(body$title)

  if (is.null(user_id) || !nzchar(user_id)) {
    res$status <- 400
    return(list(error = "Missing user_id"))
  }

  if (is.null(title)) {
    res$status <- 400
    return(list(error = "Project title is required"))
  }

  con <- getDbCon()
  on.exit(dbDisconnect(con), add = TRUE)

  idProj <- tryCatch(
    create_project_for_agent(con, user_id, title),
    error = function(e) {
      msg <- e$message
      if (grepl("already in use", msg, fixed = TRUE)) {
        res$status <- 409
      } else {
        res$status <- 400
      }
      return(list(error = msg))
    }
  )

  if (is.list(idProj) && !is.null(idProj$error)) {
    return(idProj)
  }

  res$status <- 201
  list(id = idProj, title = title, created_by = user_id)
}



sql_in <- function(x) {
  # Safely build IN (...) for character vectors
  if (length(x) == 0) return("(NULL)")
  paste0("(", paste(dbQuoteString(getDbCon(), x), collapse = ","), ")")
}

# NOTE: cleanJSONCols() is assumed to already exist in your codebase.

# Core queries ----------------------------------------------------------
fetch_project <- function(id) {
  con <- getDbCon()
  on.exit(dbDisconnect(con), add = TRUE)
  q <- glue_sql("SELECT * FROM tbl_projects WHERE id = {id};", .con = con)
  res <- dbGetQuery(con, q)
  cleanJSONCols(res)
}

fetch_agents <- function(id) {
  con <- getDbCon()
  on.exit(dbDisconnect(con), add = TRUE)
  q <- glue_sql(
    "SELECT
         title, tag_version as version, COALESCE(e.email_address, l.id_agent) AS username,
         l.role
       FROM tbl_link_projects_x_agents AS l
       LEFT JOIN tbl_link_agents_x_email_addresses AS e
         ON l.id_agent = e.id_agent
       LEFT JOIN tbl_workflows AS w
         ON l.id_workflow = w.id
       WHERE l.id_project = {id};", .con = con)
  dbGetQuery(con, q)
}

fetch_policy_msg_ids <- function(id) {
  con <- getDbCon()
  on.exit(dbDisconnect(con), add = TRUE)
  q <- glue_sql(
    "SELECT id_msg_policy FROM tbl_link_projects_x_agents WHERE id_project = {id};",
    .con = con)
  dbGetQuery(con, q)$id_msg_policy
}


fetch_locations <- function(id){
  con <- getDbCon()
  on.exit(dbDisconnect(con), add = TRUE)
  q <- glue_sql("SELECT * FROM tbl_project_locations WHERE id_project = {id};", .con = con)
  cleanJSONCols(dbGetQuery(con, q))
}

fetch_impacts <- function(id){
  con <- getDbCon()
  on.exit(dbDisconnect(con), add = TRUE)
  q <- glue_sql("SELECT * FROM tbl_project_impact_indicators WHERE id_project = {id};", .con = con)
  cleanJSONCols(dbGetQuery(con, q))
}

fetch_documents <- function(id){
  con <- getDbCon()
  on.exit(dbDisconnect(con), add = TRUE)
  tryCatch(
    get_project_documents(con, id),
    error = function(e) tibble()
  )
}

fetch_vics <- function(id){
  dbCon <- getDbCon()
  on.exit(dbDisconnect(dbCon), add = TRUE)
  dfVICs <- dbGetQuery(
    conn = dbCon, 
    statement = sprintf(
      "SELECT * FROM tbl_impact_certificates WHERE id_entity = '%s';",
      id))
  if (length(dfVICs) == 0) { return(NULL) }
  if (nrow(dfVICs) == 0) { return(NULL) }
  
  names(dfVICs)[names(dfVICs) == "id"] <- "id_cert"
  
  # Add the document ID to the df.
  dfDocIds <- dbGetQuery(
    conn = dbCon, 
    statement = sprintf(
      "SELECT id, id_message_h FROM tbl_document_metadata WHERE id_message_h IN(%s);",
      paste(sprintf("'%s'", dfVICs$id_message_h), 
            sep = "", collapse = ",")))
  idxx <- match(x = dfVICs$id_message_h, table = dfDocIds$id_message_h)
  dfVICs$id_doc <- dfDocIds$id[idxx]
  return(dfVICs)
}

fetch_workflows <- function(id_workflow){
  dbCon <- getDbCon()
  on.exit(dbDisconnect(dbCon), add = TRUE)
  q <- sprintf("SELECT * FROM tbl_workflows WHERE id IN(%s);",
               paste(sprintf("'%s'", unique(id_workflow)), 
                     collapse = ","))
  df <- dbGetQuery(conn = dbCon, statement = q)
  
  # Add 'View' buttons.
  df$btn_view <- sprintf('View%d', 1:nrow(df))
  
  vnms <- c(title = "Title", tag_version = "Version", btn_view = "View")
  vnms <- vnms[intersect(names(vnms), names(df))]
  df <- df[,names(vnms)]
  names(df) <- vnms
  df
}


# Endpoints -------------------------------------------------------------

#* @get /projects/<id>
#* @tag Projects
#* @description Retrieve metadata for a single project.
#* @param id:path Project identifier.
#* @param res Plumber response object used to set HTTP status codes.
#* @response 200 Project details for the supplied identifier.
#* @response 404 Project not found.
function(id, res) {
  message(Sys.time(), " Fetching project details for ID: ", id)
  project <- fetch_project(id)
  if (nrow(project) == 0) {
    res$status <- 404
    return(list(error = sprintf("Project %s not found", id)))
  }
  project[1, ]
}

#* @get /projects/<id>/agents
#* @tag Projects
#* @description List agents associated with the project along with their roles and policy links.
#* @param id:path Project identifier.
#* @response 200 Agent assignments for the supplied project.
function(id) {
  fetch_agents(id)
}

#* @get /projects/<id>/policies
#* @tag Projects
#* @description Return policy metadata linked to the project.
#* @param id:path Project identifier.
#* @response 200 Policy rows associated with the project (empty when none exist).
function(id) {
  pol_ids <- fetch_policy_msg_ids(id)
  fetch_policies_by_ids(pol_ids)
}

#* @get /projects/<id>/tokens
#* @tag Projects
#* @description Return Guardian tokens that are linked to the project's policies.
#* @param id:path Project identifier.
#* @response 200 Token rows associated with the project's policies (empty when none exist).
function(id) {
  pol_ids <- fetch_policy_msg_ids(id)
  fetch_tokens_by_policy_ids(pol_ids)
}

#* @get /projects/<id>/locations
#* @tag Projects
#* @description Return the geospatial locations stored for the project.
#* @param id:path Project identifier.
#* @response 200 Location records for the project (empty when none exist).
function(id) {
  fetch_locations(id)
}

#* @get /projects/<id>/impacts
#* @tag Projects
#* @description Return impact indicator data captured for the project.
#* @param id:path Project identifier.
#* @response 200 Impact indicator rows for the project (empty when none exist).
function(id) {
  tibble(fetch_impacts(id))
}

#* @get /projects/<id>/documents
#* @tag Projects
#* @description Retrieve document metadata associated with the project.
#* @param id:path Project identifier.
#* @response 200 Document metadata rows linked to the project (empty when none exist).
function(id) {
  fetch_documents(id)
}

#* @get /projects/<id>/full
#* @tag Projects
#* @description Return a combined view of project metadata, relationships, and related documents.
#* @param id:path Project identifier.
#* @param res Plumber response object used to set HTTP status codes.
#* @response 200 Aggregated project details including agents, policies, tokens, locations, impacts, and documents.
#* @response 404 Project not found.
function(id, res) {
  cat("Fetching full project details for ID: ", id, "\n")
  project <- fetch_project(id)
  if (nrow(project) == 0) {
    res$status <- 404
    return(list(error = sprintf("Project %s not found", id)))
  }
  agents   <- fetch_agents(id)
  vics      <- fetch_vics(id)
  locations <- fetch_locations(id)
  impacts   <- fetch_impacts(id)
  docs      <- fetch_documents(id)
  workflows <- fetch_workflows(docs$id_workflow)

  list(
    project   = project[1, ],
    agents    = agents,
    workflows  = workflows,
    vics    = vics,
    locations = locations,
    impacts   = tibble(impacts),
    documents = docs
  )
}

rvOther <- list()

#* @post /policy/state
#* @tag Policies
#* @description Resolve the current state of a Guardian policy workflow for a given agent and entity.
#* @param req Plumber request object containing JSON fields `id_agent`, `id_msg_policy`, and optional `id_project` and `access_token_gsb`.
#* @param res Plumber response object used to set HTTP status codes.
#* @response 200 Current policy state matrix filtered for the primary agent.
#* @response 400 The request body was missing or invalid.
#* @response 500 Policy state retrieval failed for the supplied identifiers.
#* @serializer json
function(req, res) {
  if (is.null(req$postBody) || nchar(req$postBody) == 0) {
    res$status <- 400
    return(list(error = "Request body is empty"))
  }
  b <- tryCatch(
    jsonlite::fromJSON(req$postBody, simplifyVector = TRUE),
    error = function(e) NULL
  )
  id_agent <- b$id_agent
  idMsgPolicy <- b$id_msg_policy
  idProject <- if (!is.null(b$id_project)) b$id_project else NA
  accTknGsb <- if (!is.null(b$access_token_gsb)) b$access_token_gsb else NULL
  print("Getting policy state...")
  dbCon <- getDbCon(); on.exit(dbDisconnect(dbCon), add = TRUE)

  # Retrieve details of the policy.
  {
    q <- sprintf("SELECT * FROM tbl_policies WHERE id_message = '%s';",
                 idMsgPolicy)
    rvOther$dfPolicyDets <- dbGetQuery(conn = dbCon, statement = q)
  }

  subjPol <- rvOther$dfPolicyDets$subject

  # Get the agent's role in the policy.
  {
    agentRole <- NULL

    if (subjPol == 'AGENT') {

      agentRole <- "PRIMARY_AGENT"

    } else {

      q <- sprintf("SELECT * FROM tbl_link_projects_x_agents WHERE id_project = '%s' AND id_msg_policy = '%s' AND id_agent = '%s';",
                   idProject, idMsgPolicy, id_agent)
      df <- dbGetQuery(conn = dbCon, statement = q)
      if (nrow(df) == 1) {
        if (!is.na(df$role)) {
          if (nchar(gsub(pattern = "[[:blank:]]",
                         replacement = "",
                         x = df$role)) > 0) {
            agentRole <- df$role
          }
        }
      }
    }

    if (length(agentRole) == 0) {
      agentRole <- "VIEWER"
    }

    rvOther$agentRole <- agentRole
  }

  # Update the current state of the policy.
  {
    if (!exists(sprintf("getState.%s", rvOther$dfPolicyDets$handler_r))) {
      showModal(
        modalDialog(
          title = "Coming soon!",
          sprintf("Function 'getState.%s' has not been defined yet.",
                  rvOther$dfPolicyDets$handler_r),
          footer = NULL,
          size = "m",
          easyClose = TRUE))
      return(NULL)
    }

    # Determine who are the primary agents of this policy.
    {
      if (subjPol == "AGENT") {
        idPrimAgents <- id_agent
      } else {
        q <- sprintf("SELECT id_agent FROM tbl_link_projects_x_agents WHERE id_project = '%s' AND id_msg_policy = '%s' AND role = 'PRIMARY_AGENT';",
                     idProject, idMsgPolicy)
        idPrimAgents <- dbGetQuery(conn = dbCon, statement = q)
      }
    }

    # Determine the ID of the entity subject of this policy.
    idEntity <- ifelse(subjPol == "AGENT",
                       id_agent,
                       idProject)

    # Retrieve the current state of the policy.
    df <- do.call(
      what = sprintf("getState.%s", rvOther$dfPolicyDets$handler_r),
      args = list(idPrimAgents = idPrimAgents,
                  idEntity = idEntity,
                  dbCon = dbCon,
                  accTknGsb = accTknGsb,
                  idMsgPolicy = idMsgPolicy))

    df$oidx <- 1:nrow(df)
    dfState <- df
  }


  idxx <- grep(pattern = "PRIMARY_AGENT",
               x = dfState$permissions_block,
               fixed = TRUE)
  dfState <- dfState[idxx,]

  if (nrow(dfState) == 0) {
    warning("Something went wrong. Policy has no blocks for role 'PRIMARY_AGENT'.")
    return(NULL)
  }

  # Replace the words "policy" and "policies" in var 'descr_block' with
  # "workflow" and "workflows" respectively, because "policy" is just a
  # very confusing word to most people.
  {
    subs <- c(policy = "workflow",
              Policy = "Workflow",
              policies = "workflows",
              Policies = "Workflows")
    for (x in names(subs)) {
      dfState$descr_block <- gsub(
        pattern = x,
        replacement = subs[[x]],
        x = dfState$descr_block,
        fixed = TRUE)
    }
  }

  if (rvOther$agentRole != "PRIMARY_AGENT") {

    # Only the primary agent of the policy should be able to take action
    # from within this module; everyone else is just a viewer, so return
    # without adding buttons.

    vnms <- c("title_block", "descr_block", "state")
    dfState <- dfState[vnms]
    names(dfState) <- c("step", "description", "state")
    rvOther$vnmsDtoState <- vnms
    return(dfState)

  }

  # Add buttons according to each block's state.
  {
    # Note: We use 'oidx' here to label the buttons, instead of just
    # 1:nrow(dfState), because dfState here no longer contains all the
    # rows in rvOther$dfPolicyState, so when we check for cell clicks on
    # this DTO, we'll need oidx instead of just idxRow.

    dfState$action <- NA_character_

    idxx <- which(dfState$state == "START")
    dfState$action[idxx] <- "Start"

    idxx <- which(dfState$state == "IN_PROGRESS")
    dfState$action[idxx] <- "Continue"

    idxx <- which(dfState$state == "RETRY")
    dfState$action[idxx] <- "Retry"

    idxx <- which(dfState$state == "READY_FOR_REPEAT")
    dfState$action[idxx] <- "Repeat"

  }


  rvOther$vnmsDtoState <- vnms

  list(#policyDetails = tibble(dfD[1,]),
       state = tibble(dfState))
}

#* @get /projects/<project_id>/monitored-parameters
#* @tag Projects
#* @description Return monitoring parameter records captured for a project.
#* @param project_id:path Project identifier.
#* @param res Plumber response object used to set HTTP status codes.
#* @response 200 Monitoring parameter rows ordered by creation date (empty list with message when none exist).
#* @response 400 The request did not include a `project_id`.
function(project_id, res) {
  if (missing(project_id) || is.null(project_id) || !nzchar(project_id)) {
    res$status <- 400
    return(list(error = "Missing project_id"))
  }

  con <- getDbCon()
  on.exit(dbDisconnect(con), add = TRUE)

  df <- fetch_monitored_parameters(con, project_id)

  if (nrow(df) == 0) {
    return(list(monitored_parameters = tibble(), message = "No monitored parameters recorded"))
  }

  list(monitored_parameters = df)
}

#* @get /projects/<project_id>/monitoring-periods
#* @tag Projects
#* @description Return configured monitoring periods for the project.
#* @param project_id:path Project identifier.
#* @param res Plumber response object used to set HTTP status codes.
#* @response 200 Monitoring periods ordered by start date (empty list with message when none exist).
#* @response 400 The request did not include a `project_id`.
function(project_id, res) {
  if (missing(project_id) || is.null(project_id) || !nzchar(project_id)) {
    res$status <- 400
    return(list(error = "Missing project_id"))
  }

  con <- getDbCon()
  on.exit(dbDisconnect(con), add = TRUE)

  df <- fetch_monitoring_periods(con, project_id)

  if (nrow(df) == 0) {
    return(list(monitoring_periods = tibble(), message = "No monitoring periods recorded"))
  }

  list(monitoring_periods = df)
}
