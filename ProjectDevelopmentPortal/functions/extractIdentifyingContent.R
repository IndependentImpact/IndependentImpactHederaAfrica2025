# "Indentifying content fields" are used to determine whether two documents are
# just different versions of each other, or whether they are two "unique"/
# separate documents.

extractIdentifyingContent <- function(doc, schemaId, dbCon) {
  
  # If a schema has no entries in tbl_map_schemas_identifying_content_vars, 
  # each document instance based on the schema should be considered its own 
  # document (i.e., document versioning will be impossible for documents based 
  # on that schema).
  
  dfBodyFldNms <- dbGetQuery(
    conn = dbCon, 
    statement = sprintf("SELECT * FROM tbl_map_schemas_identifying_content_vars WHERE id_schema = '%s';", schemaId))
  if (length(dfBodyFldNms) == 0) { return(NULL) }
  if (nrow(dfBodyFldNms) == 0) { return(NULL) }
  
  dfBodyFldNms <- dfBodyFldNms[order(dfBodyFldNms$field_position_in_id),]
  rownames(dfBodyFldNms) <- 1:nrow(dfBodyFldNms)
  lsRes <- doc[intersect(dfBodyFldNms$field_key, names(doc))]
  
  return(lsRes)
}

