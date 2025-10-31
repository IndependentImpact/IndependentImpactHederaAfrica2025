
#' Associates an agent with a project according to a specific role of a specific 
#' workflow. Does this by updating tbl_link_projects_x_agents.
#'@param role Character. One of 'PRIMARY_AGENT', 'STANDARDS_BODY', 'REVIEWER'.
#'
linkAgentXproject <- function(idAgent, 
                              idProject, 
                              idWorkflow,
                              role, 
                              dbCon) {
  
  #TODO. What if the workflow does not have any non-SR roles defined?
  
  # Input checking.
  if (!(role %in% c('PRIMARY_AGENT', 'STANDARDS_BODY', 'REVIEWER'))) {
    stop("'role' must be one of 'PRIMARY_AGENT', 'STANDARDS_BODY', 'REVIEWER'.")
  }
  
  # Make sure the agent is not associated with another role of this workflow for
  # this specific project.
  {
    dfEx <- dbFetch(
      dbSendQuery(
        conn = dbCon, 
        statement = sprintf(
          "SELECT * FROM tbl_link_projects_x_agents WHERE id_agent = '%s' AND id_project = '%s' AND id_workflow = '%s';",
          idAgent, idProject, idWorkflow)))
    
    if (nrow(dfEx) > 0) {
      
      if (dfEx$role != role) {
        stop(sprintf(
          "Agent already associated with '%s' role of chosen workflow; cannot associate agent with a different ('%s') role.", 
          dfEx$role, role))
      }
      
      return(invisible(0))
    }
  }
  
  # Make an entry into tbl_link_projects_x_agents for this agent as the <role> 
  # of this workflow for this project.
  {
    dfAdd <- data.frame(
      id_project = idProject,
      id_workflow = idWorkflow,
      id_agent = idAgent,
      role = role)
    
    addToDb(dfAdd = dfAdd, 
            tblNm = "tbl_link_projects_x_agents", 
            vnmsChckEx = c("id_project", "id_workflow", "id_agent"), 
            dbCon = dbCon, 
            calcIds = FALSE)
  }
  
  return(invisible(0))
}
