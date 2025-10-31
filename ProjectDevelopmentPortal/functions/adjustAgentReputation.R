#'@param lsAgents list A role-named list of character vectors providing the IDs
#'  of all the agents for each role for which the reputation must be adjusted.
#'  Example: 
#'    list(PROJECT_DEVELOPER = "fdeca1444e09c46087ac4acc5e866413", 
#'         PDD_VALIDATOR = c("dda69fbd47accc93e00a84443412da44", 
#'                           "9c4a921ba90f7cf0ec78979d60172c9b"), 
#'         MR_VERIFIER = NULL)
#'@param idDocTrigger character The ID of the document that triggered this 
#'  reputation adjustment.
#'@param eventType character One of the following:
#'  agent_details_verification
#'  license_application
#'  project_registration_approved
#'  project_registration_rejected
#'  vic_issuance_approved
#'  vic_issuance_rejected
#'  
adjustAgentReputation <- function(lsAgents, idDocTrigger, eventType, dbCon) {
  
  dfReputGuide <- dbGetQuery(
    conn = dbCon, 
    statement = sprintf(
      "SELECT * FROM tbl_agent_reputation_event_weights WHERE event_type = '%s';",
      eventType))
  
  for (r in 1:nrow(dfReputGuide)) {
    
    q <- sprintf(
      "UPDATE tbl_agent_reputation SET score = score + %d WHERE id_agent IN(%s) AND domain = '%s';",
      dfReputGuide$weight[r], 
      paste(sprintf("'%s'", lsAgents[[dfReputGuide$agent_role[r]]]), 
            sep = "", collapse = ","),
      dfReputGuide$reputation_domain[r])
    dbRes <- dbSendStatement(conn = dbCon, statement = q)
    dbClearResult(dbRes)
    
    addToDb(
      dfAdd = data.frame(
        id_agent = lsAgents[[dfReputGuide$agent_role[r]]],
        event_type = dfReputGuide$event_type[r],
        occurred_at = dbGetQuery(
          conn = dbCon, 
          statement = sprintf(
            "SELECT date_modified FROM tbl_document_metadata WHERE id = '%s';",
            idDocTrigger))[["date_modified"]],
        trigger = idDocTrigger,
        reputation_domain = dfReputGuide$reputation_domain[r],
        reputation_adjustment = dfReputGuide$weight[r]), 
      tblNm = "tbl_agent_reputation_event_log", 
      vnmsChckEx = c("id_agent", "event_type", "occurred_at", "trigger", "reputation_domain"), 
      dbCon = dbCon, 
      calcIds = FALSE)
    
  }
  
  return(invisible(0))
}
