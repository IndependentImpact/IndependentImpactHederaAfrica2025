retrieveDraftDoc <- function(idEntity, docId) {
  fp <- sprintf("%s%s_%s.rds", draftdocdir, idEntity, docId)
  if (!file.exists(fp)) { return(NULL) }
  x <- readRDS(file = fp)
  return(x)
}
