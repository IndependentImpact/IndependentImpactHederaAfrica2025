buildUngroupedDocumentsGridViewDf <- function(df, dbCon, loginInfoUsr, idProject) {
  
  # Do not display draft documents.
  df <- df[which(df$status != "DRAFT"),]
  if (nrow(df) == 0) {
    return(NULL)
  }
  
  # Get schema titles.
  {
    q <- sprintf("SELECT * FROM tbl_schemas WHERE id IN(%s);",
                 paste(sprintf("'%s'", unique(df$id_schema)), 
                       collapse = ","))
    dfSchemas <- dbGetQuery(conn = dbCon, statement = q)
    idxx <- match(x = df$id_schema, table = dfSchemas$id)
    df$title <- dfSchemas$title[idxx]
  }
  
  # Get workflow handler info for each of the documents, because we need it 
  # to determine what permissions the current user has for each document.
  {
    q <- sprintf("SELECT id, handler_r FROM tbl_workflows WHERE id IN(%s);",
                 paste(sprintf("'%s'", unique(df$id_workflow)), collapse = ","))
    dfWflwHndlrs <- dbGetQuery(conn = dbCon, statement = q)
    idxx <- match(x = df$id_workflow, table = dfWflwHndlrs$id)
    df$handler_r_wflw <- dfWflwHndlrs$handler_r[idxx]
  }
  
  # Add buttons.
  {
    df$btn_view <- NA_character_
    df$btn_action <- NA_character_
    
    # 'View' buttons
    idxx <- which(df$status == "PUBLISHED")
    df$btn_view[idxx] <- makeButtonHtmlForDocumentsGrid(
      documentIds = df$oidx[idxx], buttonType = 'view')
    
    # 'Review' buttons
    {
      # A user can review a document if:
      # They are not the creator or issuer of the document; AND
      # <it is not a document from the IIS Main workflow which can only be reviewed by the SB; OR
      # it is a document from the II Main workflow, but the current user is the SB.>
      # Currently they can also not review a document if it is a review document itself. This 
      # will change in the future.
      
      idxx1 <- which(df$status == "PUBLISHED" & 
                       df$did_author != loginInfoUsr$did)
      
      idxx2 <- grep(pattern = "indImpMainWrkflwHndlr", 
                    x = df$handler_r_wflw, 
                    value = FALSE)

      idxx3 <- grep(pattern = "review", x = df$title, ignore.case = TRUE)
      
      if (loginInfoUsr$userType != "STANDARD_REGISTRY") {
        idxx <- setdiff(idxx1, c(idxx2, idxx3))
      } else {
        idxx <- setdiff(idxx1, idxx3)
      }
      
      # TODO. Remove this check once we have implemented the ability for 
      # agents to review review documents that were issued to them.
      q <- sprintf("SELECT created_by FROM tbl_projects WHERE id = '%s';", idProject)
      idCreator <- dbGetQuery(conn = dbCon, statement = q)[["created_by"]]
      if (idCreator == loginInfoUsr$id_agent) {
        idxx <- c()
      }
      
      df$btn_action[idxx] <- makeButtonHtmlForDocumentsGrid(
        documentIds = df$oidx[idxx], buttonType = 'review')
    }
    
    # # 'Edit' buttons
    # idxx <- which(df$status == "DRAFT" & 
    #                 df$did_creator == loginInfoUsr$did)
    # df$btn_action[idxx] <- makeButtonHtmlForDocumentsGrid(
    #   documentIds = df$oidx[idxx], buttonType = 'edit')
  }
  
  # Hide values in 'Outcome' variable for documents NOT created by the 
  # current user.
  # TODO. This is only a temporary measure until we've implemented the 
  # functionality to accept, contest or reject review documents.
  {
    idxx <- which(df$did_author!= loginInfoUsr$did)
    df$outcome[idxx] <- NA_character_
  }
  
  # Parse 'identifying_content' into meaningful variables, where possible.
  #df <- processIdentifyingContentForDisplay(df)
  
  # Prep for display.
  vnms <- c("title" = "Title", 
            #"monitoring_period" = "Monitoring period",
            #"additional_info" = "Additional info", 
            "identifying_content" = "Additional info",
            "date_created" = "Date created", 
            "date_modified" = "Date modified", 
            "status" = "Status", 
            "outcome" = "Outcome", 
            "btn_view" = "View", 
            "btn_action" = "Action")
  df <- df[names(vnms)]
  
  return(list(vnms = vnms, df = df))
}