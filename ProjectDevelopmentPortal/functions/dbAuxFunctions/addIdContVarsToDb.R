addIdContVarsToDb <- function(dbCon) {

  # Construct and populate tbl_map_schemas_identifying_content_vars
  # oidx
  # id_schema
  # field_key
  # field_position_in_id
  
  q <- readLines(con = sprintf("%stbl_map_schemas_identifying_content_vars.sql", tbldefdir))
  q <- paste(q, collapse = " ")
  res <- dbSendStatement(conn = dbCon, statement = q)
  dbClearResult(res)
  
  df <- dbGetQuery(conn = dbCon, statement = "SELECT * FROM tbl_map_schemas_identifying_content_vars;")
  
  lsFields <- list(
    "Document Review: Monitoring Report (DR-MR)" = 
      c(),
    
    "Document Review: Data Lineage Report (DR-DLR)" = 
      c(),
    
    "Dataset" = 
      c("name_dataset"),
    
    "Monitoring Report (MR)" = 
      c("monitoring_period"),
    
    "Document Review: Monitoring Report (DR-MR)" = 
      c(),
    
    "Data Lineage Report (DLR)" = 
      c("monitoring_period",
        "name_dataset"),
    
    "Document Review: PDD Certificate Issuance Request (DR-CIR)" = 
      c(),
    
    "PDD Certificate Issuance Request (CIR)" = 
      c(),
    
    "PDD Section C - Stakeholder Engagement (PDD-XC)" = 
      c(),
    
    "PDD Section B - Methodologies (PDD-XB)" = 
      c(),
    
    "PDD Section A - Description of Project (PDD-XA)" = 
      c(),
    
    "Document Review: Project Registration Request (DR-PRR)" = 
      c(),
    
    "Project Registration Request (PRR)" = 
      c(),
    
    "Document Review: Project Listing Application (DR-PLA)" = 
      c(),
    
    "Project Listing Application (PLA)" = 
      c(),
    
    "License Application (LA)" = 
      c("license"),
    
    "Document Review: License Application" = 
      c("id_msg_drad",
        "license"),
    
    "Document Review: VIC Issuance Request" = c(),
    
    "VIC Issuance Request (VIC-IR)" = 
      c("id_msg_pred_drmr"),
    
    "Generic Document Review" = c())

  lsFields <- lsFields[which(sapply(X = lsFields, FUN = length) > 0)]
  
  dfFields <- do.call("rbind", lapply(X = names(lsFields), FUN = function(schemaTitle) {
    df <- data.frame(title_schema = schemaTitle,
                     field_key = lsFields[[schemaTitle]],
                     field_position_in_id = 1:length(lsFields[[schemaTitle]]))
    return(df)
  }))
  
  dfSchemas <- dbGetQuery(
    conn = dbCon, 
    statement = sprintf("SELECT title, id FROM tbl_schemas WHERE title IN(%s);",
                        paste(sprintf("'%s'", unique(dfFields$title_schema)), 
                              sep = "", collapse = ",")))
  
  lsAdd <- lapply(X = 1:nrow(dfSchemas), FUN = function(r) {
    df <- dfFields[which(dfFields$title_schema == dfSchemas$title[r]),]
    df$id_schema <- dfSchemas$id[r]
    return(df)
  })
  dfAdd <- do.call("rbind", lsAdd); rm(lsAdd)
  dfAdd$title_schema <- NULL
  
  addToDb(
    dfAdd = dfAdd, 
    tblNm = "tbl_map_schemas_identifying_content_vars", 
    vnmsChckEx = c('id_schema', 'field_key'), 
    dbCon = dbCon, 
    calcIds = FALSE, 
    returnIds = FALSE)
  
  return(TRUE)
}