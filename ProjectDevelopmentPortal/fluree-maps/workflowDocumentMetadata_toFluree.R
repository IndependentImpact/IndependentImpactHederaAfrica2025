workflowDocumentMetadata_toFluree <- function(docId = NULL, dfDocMd = NULL, dbCon = NULL) {
  
  if (length(docId) == 0 & length(dfDocMd) == 0) {
    stop("Must provide either docId or dfDocMd.")
  }
  
  if (length(dfDocMd) == 0) {
    dfDocMd <- dbGetQuery(
      conn = dbCon,
      statement = sprintf(
        "SELECT * FROM tbl_document_metadata WHERE id = '%s';",
        docId))
  }
  if (length(docId) == 0) {
    docId <- dfDocMd$id
  }
  
  idAuthor <- dbGetQuery(
    conn = dbCon, 
    statement = sprintf(
      "SELECT id_agent FROM tbl_link_agents_x_dids WHERE did = '%s';",
      dfDocMd$did_author))[["id_agent"]]
  
  res <- list(
    '@context' = list(
      'data' = "http://jellyfiiish.xyz/ns/",
      'indimp' = "https://independentimpact.org/ns/",
      'rdfs' = "http://www.w3.org/2000/01/rdf-schema#"),
    
    '@id' = paste0("indimp:documents/", docId),
    '@type' = "data:Document",
    
    'indimp:resourceIpfsUri' = dfDocMd$uri_ipfs,
    'indimp:documentSchema' = list(
      '@type' = "indimp:DocumentSchema",
      '@id' = paste0("indimp:document-schema/", dfDocMd$id_schema)),
    'indimp:isEncrypted' = dfDocMd$encrypted, 
    'indimp:documentAuthor' = list(
      '@id' = paste0("indimp:agents/", idAuthor)),
    'indimp:authProof' = switch(
      dfDocMd$type_doc,
      REGULAR_UNSIGNED = "NONE",
      REGULAR_SIGNED = "EDDSA",
      VC = "VC"), # TODO. Determine this from the document's signature.
    
    'indimp:hasWorkflowSubmission' = list(
      '@type' = "indimp:WorkflowDocumentSubmission",
      'indimp:workflow' = list(
        '@id' = paste0("indimp:workflows/", dfDocMd$id_workflow)),
      'indimp:workflowStep' = list(
        '@type' = "indimp:WorkflowStep",
        'rdfs:label' = dfDocMd$step_workflow),
      'indimp:workflowSubject' = list(
        '@id' = paste0("indimp:activities/", dfDocMd$id_entity)),
      'indimp:workflowDocumentSubmissionHederaMessageId' = dfDocMd$id_message_h))
  
  return(res)
  
}