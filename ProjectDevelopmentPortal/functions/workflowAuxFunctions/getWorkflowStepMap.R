# Used by many other auxiliary workflow functions.
#
getWorkflowStepMap <- function(nmWrkflwHndlr = NULL,
                               idWorkflow, # e.g., "e3f1f8222b683608a9ad8845a432604b"
                               dbCon) {
  
  # Determine nmWrkflwHndlr if not provided.
  if (length(nmWrkflwHndlr) == 0) {
    nmWrkflwHndlr <- dbFetch(
      dbSendQuery(
        conn = dbCon, 
        statement = sprintf("SELECT handler_r FROM tbl_workflows WHERE id = '%s';",
                            idWorkflow)))[["handler_r"]]
  }
  
  # Get generic step map.
  dfStepMap <- do.call(what = sprintf("getWorkflowStepMap.%s", nmWrkflwHndlr), 
                        args = list())
  
  # Get the actual workflow and subset to the rvcdbs and complementary idsbs. 
  {
    workflow <- jsonlite::read_json(
      path = sprintf("%s%s/workflow.json", wrkflwdir, nmWrkflwHndlr), 
      simplifyVector = TRUE, 
      simplifyDataFrame = FALSE, 
      flatten = FALSE)
    dfSteps <- getWorkflowStepInfo(workflow$config)
    dfSteps <- dfSteps[which(dfSteps$tag %in% c(dfStepMap$step_workflow, 
                                                   dfStepMap$review_with)),]
    names(dfSteps) <- gsub(pattern = "^block_", 
                            replacement = "", 
                            x = names(dfSteps))
    names(dfSteps) <- sprintf("%s_step", names(dfSteps))
    names(dfSteps)[which(names(dfSteps) == "schema_step")] <- "iri_schema_step"
    names(dfSteps)[which(names(dfSteps) == "tag_step")] <- "step_workflow"
  }
  
  # Replace the words "policy" and "policies" in var 'descr_step' with 
  # "workflow" and "workflows" respectively.
  {
    subs <- c(policy = "workflow",
              Policy = "Workflow",
              policies = "workflows",
              Policies = "Workflows")
    for (x in names(subs)) {
      dfSteps$descr_step <- gsub(
        pattern = x, 
        replacement = subs[[x]], 
        x = dfSteps$descr_step, 
        fixed = TRUE)
    }
  }
  
  # Merge to map and add workflow vars.
  {
    dfStepMap <- merge.data.frame(
      x = dfSteps, 
      y = dfStepMap,
      by.x = "step_workflow",
      by.y = "step_workflow",
      all = TRUE)
    
    dfStepMap$id_workflow <- idWorkflow; rm(dfSteps)
  }
  
  # Translate the workflow role labels to our workflow role classes.
  {
    roleMap <- list(
      PRIMARY_AGENT = c("PROJECT_DEVELOPER", 
                        "PRIMARY_AGENT", 
                        "NO_ROLE", 
                        "AGENT"),
      REVIEWER = c("VALIDATOR", 
                   "VERIFIER"),
      STANDARDS_BODY = c("OWNER"))
    
    for (r in names(roleMap)) {
      for (t in roleMap[[r]]) {
        dfStepMap$permissions_step <- gsub(
          pattern = t, 
          replacement = r, 
          x = dfStepMap$permissions_step, 
          fixed = TRUE)
        # << Using 'gsub' here, because some steps have permissions for multiple 
        # roles concatenated.
      }
    }
  }
  
  # Add schema ID.
  {
    dfSchemas <- dbGetQuery(
      conn = dbCon, 
      statement = sprintf(
        "SELECT id, iri FROM tbl_schemas WHERE iri IN(%s);",
        paste(sprintf("'%s'", dfStepMap$iri_schema_step), sep = "", collapse = ",")))
    
    dfStepMap$id_schema <- NA_character_
    idxx <- match(x = dfStepMap$iri_schema_step, table = dfSchemas$iri)
    dfStepMap$id_schema[which(!is.na(idxx))] <- dfSchemas$id[idxx[!is.na(idxx)]]
    rm(dfSchemas)
  }
  
  # Try some rearrangin'.
  {
    vnmsFirst <- c("id_workflow", "step_workflow")
    dfStepMap <- dfStepMap[,c(vnmsFirst, 
                                setdiff(names(dfStepMap), 
                                        vnmsFirst))]
    
    vnmsLast <- c("nm_module", "review_with")
    dfStepMap <- dfStepMap[,c(setdiff(names(dfStepMap), 
                                        vnmsLast), 
                                vnmsLast)]
  }
  
  return(dfStepMap)
}
