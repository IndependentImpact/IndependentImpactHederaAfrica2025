# plumber.R

#* @apiTitle Independent Impact API
#* @apiDescription RESTful API for user login and registration via Guardian

library(plumber)
library(DBI)
library(RPostgres)
library(digest)
library(jsonlite)
library(Guardener)
library(independentImpact)
library(lubridate)
library(tibble)

# Load shared utilities (getDbCon, genDbId, assocAgentPolicy, etc.)
# Make sure global.R defines any project-specific helpers you need

# Helper: obtain DB connection
getDbCon <- function() {
  DBI::dbConnect(
    RPostgres::Postgres(),
    host     = Sys.getenv("II_DB_HOST"),
    dbname   = Sys.getenv("II_DB_NAME"),
    port     = Sys.getenv("II_DB_PORT"),
    user     = Sys.getenv("II_DB_USER"),
    password = Sys.getenv("II_DB_PASS")
  )
}

# Guardian configuration
guardianBaseUrl <- switch(
  Sys.info()[["nodename"]],
  'DESKTOP-CDM64JQ' = "http://209.38.98.228:3000/",
  'DESKTOP-8EVDQT7' = c("https://jellyfi3sh.xyz/", "http://167.99.35.174:3000/")[2],
  'guardian-containerised' = "http://localhost:3000/",
  'independent-impact-20250211-1-s-2vcpu-4gb-amd-ams3-01' = "http://localhost:3000/"
)

unIndImpStanReg <- "NovaInstitute"
hederaNetwork <- c("testnet", "previewnet", "mainnet")[1]

#* Health check
#* @get /health
function() {
  list(status = "OK")
}

# 1) CORS filter (must come *before* any @* tag routes)
#    Allow both Content-Type AND Accept headers
#    Handle preflight OPTIONS requests
#────────────────────────────────────────────────────────
#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin",  "*")
  res$setHeader("Access-Control-Allow-Methods", "GET,POST,OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Accept, Authorization")
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())  # stop here for preflight
  }
  plumber::forward()  # otherwise continue to your route
}


#* User login to Independent Impact
#* @post /login
#* @serializer json
function(req, res) {
  # 0a) Reject totally empty bodies
  if (is.null(req$postBody) || nchar(req$postBody) == 0) {
    res$status <- 400
    return(list(error = "Request body is empty"))
  }

  # 0b) Try to parse JSON, catching invalid payloads
  body <- tryCatch(
    jsonlite::fromJSON(req$postBody, simplifyVector = TRUE),
    error = function(e) NULL
  )
  if (is.null(body) || !is.list(body)) {
    res$status <- 400
    return(list(error = "Malformed JSON"))
  }

  email <- body$email
  password <- body$password

  # 1) Basic validation
  if (is.null(email) || is.null(password)) {
    res$status <- 400
    return(list(error = "Missing email or password"))
  }

  print(paste(as.character(Sys.time()), "Login attempt for email:", email))

  # 2) Verify email exists
  con <- getDbCon()
  dfE <- dbGetQuery(con, sprintf(
    "SELECT id_agent FROM tbl_link_agents_x_email_addresses WHERE email_address = '%s';",
    email
  ))
  if (nrow(dfE) == 0) {
    res$status <- 400; dbDisconnect(con)
    return(list(error = "II account not found. Please register."))
  }
  if (nrow(dfE) > 1) {
    res$status <- 500; dbDisconnect(con)
    return(list(error = "Multiple users found with this email. Contact support."))
  }
  id_agent <- dfE$id_agent[1]

  # 3) Verify password
  dfP <- dbGetQuery(con, sprintf(
    "SELECT hash_pw_ics FROM tbl_link_agents_x_ics_passwords WHERE id_agent = '%s';",
    id_agent
  ))
  if (nrow(dfP) == 0) {
    res$status <- 500; dbDisconnect(con)
    return(list(error = "Password not set. Contact support."))
  }
  if (digest::digest(password, algo = "sha512") != dfP$hash_pw_ics[1]) {
    res$status <- 401; dbDisconnect(con)
    return(list(error = "Incorrect password."))
  }

  # 4) Lookup Guardian credentials
  dfAllInsts <- dbGetQuery(con, "SELECT * FROM tbl_guardian_instances;")
  if (Sys.info()[["user"]] == "tvsot") {
    dfCurrInst <- dfAllInsts
  } else {
    dfCurrInst <- subset(dfAllInsts, url == guardianBaseUrl)
  }
  guardianInstId <- dfCurrInst$id[1]

  dfG <- dbGetQuery(con, sprintf(
    "SELECT username_g, type_user
     FROM tbl_link_agents_x_guardian_users
     WHERE id_agent_user = '%s'
       AND id_guardian_instance = '%s';",
    id_agent, guardianInstId
  ))
  dbDisconnect(con)

  if (nrow(dfG) != 1) {
    res$status <- 500
    return(list(error = "Unable to find Guardian user for this agent."))
  }
  un         <- dfG$username_g[1]
  type_user  <- dfG$type_user[1]
  guardian_pw <- ifelse(type_user == "STANDARD_REGISTRY", password, id_agent)

  # 5) Log into Guardian
  if (type_user == "STANDARD_REGISTRY" && un == unIndImpStanReg) {
    gres <- logSRintoGuardian()
  } else {
    gres <- Guardener::Glogin(
      un      = un,
      pw      = guardian_pw,
      baseurl = guardianBaseUrl
    )
  }
  if ("statusCode" %in% names(gres)) {
    res$status <- 502
    return(list(error = paste0("Guardian login failed: ", gres$message)))
  }

  # 6) Return authenticated session details
  list(
    email_address = email,
    id_agent      = id_agent,
    userType    = gres$role,
    did           = gres$did,
    refreshToken  = gres$refreshToken
  )
}


#* Create a new Independent Impact account
#* @parser json
#* @post /register
#* @param email:string Desired email address
#* @param password:string Desired password
#* @param hederaAccountId:string Hedera account ID
#* @param hederaAccountKey:string Hedera account private key
#* @serializer json
function(email, password, hederaAccountId, hederaAccountKey, res) {
  #–– 1) Validate inputs
  if (any(sapply(list(email, password, hederaAccountId, hederaAccountKey), is.null))) {
    res$status <- 400
    return(list(error = "Missing required fields: email, password, hederaAccountId, hederaAccountKey"))
  }
  if (nchar(password) < 8) {
    res$status <- 400
    return(list(error = "Password must be at least 8 characters."))
  }

  #–– 2) Open DB
  con <- getDbCon()
  on.exit(dbDisconnect(con), add = TRUE)

  #–– 3) Ensure email not already in use
  dfE <- dbGetQuery(con,
                    sprintf("SELECT 1 FROM tbl_link_agents_x_email_addresses WHERE email_address = '%s';", email)
  )
  if (nrow(dfE) > 0) {
    res$status <- 400
    return(list(error = "Email already registered."))
  }

  #–– 4) Create agent record
  dbExecute(con,
            "INSERT INTO tbl_agents(id, date_registered) VALUES ($1, now());",
            params = list(genDbId(con, 'tbl_agents', 1)[1])
  )
  idAgent <- dbGetQuery(con,
                        "SELECT currval(pg_get_serial_sequence('tbl_agents','id'));"
  )$currval

  #–– 5) Store password hash
  dbExecute(con,
            "INSERT INTO tbl_link_agents_x_ics_passwords(id_agent,hash_pw_ics) VALUES ($1,$2);",
            params = list(idAgent, digest::digest(password, algo = "sha512"))
  )

  #–– 6) Store email
  dbExecute(con,
            "INSERT INTO tbl_link_agents_x_email_addresses(id_agent,email_address) VALUES ($1,$2);",
            params = list(idAgent, email)
  )

  #–– 7) Store Hedera account
  dbExecute(con,
            "INSERT INTO tbl_link_agents_x_hedera_accounts(id_agent,id_acc_h) VALUES ($1,$2);",
            params = list(idAgent, hederaAccountId)
  )

  #–– 8) Create Guardian user
  dfInst <- dbGetQuery(con,
                       sprintf("SELECT id FROM tbl_guardian_instances WHERE url = '%s';", guardianBaseUrl)
  )
  idGuardInst <- dfInst$id[1]

  dfSR <- dbGetQuery(con,
                     sprintf("SELECT id_agent_user FROM tbl_link_agents_x_guardian_users
             WHERE type_user='STANDARD_REGISTRY' AND id_guardian_instance='%s';",
                             idGuardInst)
  )
  parentDid <- dbGetQuery(con,
                          sprintf("SELECT did FROM tbl_link_agents_x_dids WHERE id_agent='%s';",
                                  dfSR$id_agent_user[1])
  )$did[1]

  gres <- Guardener::GcreateUser(
    un               = email,
    pw               = idAgent,
    role             = "USER",
    parentDid        = parentDid,
    hederaAccountId  = hederaAccountId,
    hederaAccountKey = hederaAccountKey,
    baseurl          = guardianBaseUrl
  )
  if ("statusCode" %in% names(gres)) {
    res$status <- 502
    return(list(error = paste0("Guardian user creation failed: ", gres$message)))
  }

  #–– 9) Store DID
  dbExecute(con,
            "INSERT INTO tbl_link_agents_x_dids(id_agent,did) VALUES ($1,$2);",
            params = list(idAgent, gres$did)
  )

  #–– 10) Link Guardian user record
  dbExecute(con,
            "INSERT INTO tbl_link_agents_x_guardian_users(
       id_agent_user, username_g, hash_pwd_g, type_user,
       id_agent_standards_registry, id_guardian_instance
     ) VALUES ($1,$2,$3,$4,$5,$6);",
            params = list(
              idAgent,
              gres$username,
              gres$hash_password,
              gres$role,
              dfSR$id_agent_user[1],
              idGuardInst
            )
  )

  #–– 11) Attach license-workflow policy
  dfPol <- dbGetQuery(con,
                      "SELECT id_g, id_message FROM tbl_policies
     WHERE name='Independent Impact - License Application Workflow'
       AND status='ACTIVE'
     ORDER BY version DESC LIMIT 1;"
  )
  assocAgentPolicy(
    idAgent     = idAgent,
    role        = "PRIMARY_AGENT",
    idPolicyG   = dfPol$id_g[1],
    idMsgPolicy = dfPol$id_message[1],
    rfrshTknUsr = gres$refreshToken,
    dbCon       = con,
    gBaseUrl    = guardianBaseUrl
  )

  #–– 12) Return success
  res$status <- 201
  list(
    email_address = email,
    id_agent      = idAgent,
    userType    = gres$role,
    did           = gres$did,
    refreshToken  = gres$refreshToken
  )
}

#* List all projects, split by current user vs others
#* @param user_id:string The agent ID of the current user
#* @get /projects
function(user_id) {
  dbCon <- getDbCon()
  on.exit(dbDisconnect(dbCon), add = TRUE)
  print(paste(as.character(Sys.time()), "Fetching projects for user:", user_id))
  # Fetch all projects
  dfProjs <- dbGetQuery(dbCon, "SELECT tbl_projects.*, email_address FROM tbl_projects
                        LEFT JOIN tbl_link_agents_x_email_addresses ON(tbl_projects.created_by = tbl_link_agents_x_email_addresses.id_agent) ;")
  if (nrow(dfProjs) == 0) {
    return(list(my_projects = list(), other_projects = list()))
  }

  # Attach creator email
  emails <- dfProjs$email_address
  dfProjs$created_by_email <- emails

  # Order by date_created desc
  dfProjs <- dfProjs[order(dfProjs$date_created, decreasing = TRUE),]

  # Split
  my_idx <- which(dfProjs$created_by == user_id)
  my_projects <- dfProjs[my_idx, , drop = FALSE]
  other_projects <- dfProjs[-my_idx, , drop = FALSE]

  # Return lists
  list(
    my_projects = tibble(my_projects),
    other_projects = tibble(other_projects)
  )
}

#* Create a new project
#* @post /projects
#* @param user_id:string Creator agent ID
#* @param body:json The JSON body with field 'title'
function(req, res, user_id) {
  dbCon <- getDbCon()
  on.exit(dbDisconnect(dbCon), add = TRUE)

  body <- fromJSON(req$postBody)
  projTitle <- body$title

  # Validate
  if (!is.character(projTitle) || nchar(projTitle) < 3 || nchar(projTitle) > 100) {
    res$status <- 400
    return(list(error = "Project title must be between 3 and 100 characters."))
  }

  # Check uniqueness
  exists <- dbGetQuery(dbCon, sprintf(
    "SELECT 1 FROM tbl_projects WHERE title = %s;", dbQuoteString(dbCon, projTitle)
  ))
  if (nrow(exists) > 0) {
    res$status <- 409
    return(list(error = "Title already in use."))
  }

  # Insert
  timestamp <- now(tzone = "UTC")
  idProj <- dbGetQuery(dbCon, sprintf(
    "INSERT INTO tbl_projects (title, created_by, date_created) VALUES (%s, %s, %s) RETURNING id;",
    dbQuoteString(dbCon, projTitle),
    dbQuoteString(dbCon, user_id),
    dbQuoteString(dbCon, format(timestamp, "%Y-%m-%d %H:%M:%S"))
  ))$id

  res$status <- 201
  list(id = idProj, title = projTitle, created_by = user_id, date_created = timestamp)
}

# Clean out JSON columns for plumber
cleanJSONCols <- function(df) {
  df[] <- lapply(df, function(col) {
    if (inherits(col, "pq_json")) {
      vapply(col, as.character, "")
    } else col
  })
  df
}

#* Get details for a single project
#* @param id The ID of the project
#* @get /projects/<id>
function(id, res) {
  dbCon <- getDbCon()
  on.exit(dbDisconnect(dbCon), add = TRUE)

  # 1. Project metadata
  project <- dbGetQuery(dbCon, sprintf(
    "SELECT * FROM tbl_projects WHERE id = '%s';", id
  ))
  project <- cleanJSONCols(project)

  # Debug log
  message(Sys.time(), " Fetching project details for ID: ", id)

  # 2. Agents
  agents <- dbGetQuery(dbCon, sprintf(
    "SELECT u.username_g AS username, a.role, p.name, version
       FROM tbl_link_projects_x_agents AS a
       JOIN tbl_link_agents_x_guardian_users AS u
         ON a.id_agent = u.id_agent_user
		 LEFT JOIN tbl_policies p ON (p.id_message = a.id_msg_policy )
       WHERE a.id_project = '%s';", id
  ))

  # 3. Policies
  polMsgs <- dbGetQuery(dbCon, sprintf(
    "SELECT id_msg_policy FROM tbl_link_projects_x_agents WHERE id_project = '%s';", id
  ))$id_msg_policy
  if (length(polMsgs) > 0) {
    policies <- dbGetQuery(dbCon, sprintf(
      "SELECT id_message AS id_msg_policy, name, version, status
         FROM tbl_policies WHERE id_message IN(%s);",
      paste("'", polMsgs ,"'", collapse = ",", sep = "")
    ))
    policies <- cleanJSONCols(policies)
  } else {
    policies <- data.frame()
  }

  # 4. Tokens
  tokenMsgs <- policies$id_msg_policy
  if (length(tokenMsgs) > 0) {
    tokens <- dbGetQuery(dbCon, sprintf(
      "SELECT t.* FROM tbl_link_tokens_x_policies AS l
         JOIN tbl_tokens AS t ON l.id_token = t.id
         WHERE l.id_message_policy IN(%s);",
      paste("'", tokenMsgs, "'", collapse = ",")
    ))
    tokens <- cleanJSONCols(tokens)
  } else {
    tokens <- data.frame()
  }

  # 5. Locations
  locations <- dbGetQuery(dbCon, sprintf(
    "SELECT * FROM tbl_project_locations WHERE id_project = '%s';", id
  ))
  locations <- cleanJSONCols(locations)

  # 6. Impacts
  impacts <- dbGetQuery(dbCon, sprintf(
    "SELECT * FROM tbl_project_impact_indicators WHERE id_project = '%s';", id
  ))
  impacts <- cleanJSONCols(impacts)

  # 7. Documents
  docs <- tryCatch(
    dbGetQuery(dbCon, sprintf("SELECT * FROM tbl_document_metadata WHERE id_entity = '%s';", id)),
    error = function(e) data.frame()
  )
  docs <- cleanJSONCols(docs)

  # Aggregate and return
  list(
    project   = project[1, ],
    agents    = agents,
    policies  = policies,
    tokens    = tokens,
    locations = locations,
    impacts   = tibble(impacts),
    documents = docs
  )
}


#* View policy state and next actions
#* @post /policy/state
#* @serializer json
function(req, res) {
  # 0) Validate request body
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

  con <- getDbCon(); on.exit(dbDisconnect(con), add=TRUE)
  # policy details
  dfD <- dbGetQuery(con, sprintf("SELECT * FROM tbl_policies WHERE id_message='%s';", idMsgPolicy))
  if (nrow(dfD)!=1) { res$status<-404; return(list(error="Policy not found.")) }
  subj <- dfD$subject[1]; handler <- sprintf("getState.%s", dfD$handler_r[1])
  if (!exists(handler, mode="function")) {
    res$status<-501; return(list(error=sprintf("Handler %s undefined.", handler))) }

  # determine primary agents
  if (subj=="AGENT") {
    prim <- list(id_agent)
    entity <- id_agent
  } else {
    dfA<-dbGetQuery(con, sprintf(
      "SELECT id_agent FROM tbl_link_projects_x_agents WHERE id_project='%s' AND id_msg_policy='%s' AND role='PRIMARY_AGENT';",
      idProject, idMsgPolicy))
    prim<-as.list(dfA$id_agent); entity<-idProject
  }

  # fetch state
  dfS <- do.call(handler, list(idPrimAgents=prim,idEntity=entity,dbCon=con,idMsgPolicy=idMsgPolicy))
  dfS$oidx<-seq_len(nrow(dfS))

  list(policyDetails=as.list(dfD[1,]),state=dfS)
}

