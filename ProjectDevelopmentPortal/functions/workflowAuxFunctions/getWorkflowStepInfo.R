# Used by many other auxiliary workflow functions.
# Example:
# 
# workflow <- jsonlite::read_json(
#   path = sprintf("%s%s/workflow.json", wrkflwdir, nmWrkflwHndlr), 
#   simplifyVector = TRUE, 
#   simplifyDataFrame = FALSE, 
#   flatten = FALSE)
# dfStepInfo <- getWorkflowStepInfo(workflow$config)
#
getWorkflowStepInfo <- function(x) {
  
  if (!("blockType" %in% names(x))) { return(NULL) }
  
  rbind.fill <- plyr::rbind.fill
  
  df <- data.frame(
    id = x$id,
    tag = x$tag,
    block_type = x$blockType,
    permissions = paste(unlist(x$permissions), 
                        collapse = ","),
    title = NA_character_,
    descr = NA_character_)
  
  if ("schema" %in% names(x)) {
    df$schema <- x$schema
  }
  
  if ("uiMetaData" %in% names(x)) {
    
    # Try to find a title for the step.
    if ("title" %in% names(x$uiMetaData)) {
      if (!is.na(x$uiMetaData$title)) {
        if (nchar(x$uiMetaData$title) > 0) {
          df$title <- x$uiMetaData$title
        }
      }
    } 
    if (is.na(df$title)){
      if ("content" %in% names(x$uiMetaData)) {
        df$title <- x$uiMetaData$content
      }
    }
    
    # Try to find a description for the step.
    if ("description" %in% names(x$uiMetaData)) {
      if (!is.na(x$uiMetaData$description)) {
        if (nchar(x$uiMetaData$description) > 0) {
          df$descr <- x$uiMetaData$description
        }
      }
    } 
    if (is.na(df$descr)) {
      if ("dialogContent" %in% names(x$uiMetaData)) {
        df$descr <- x$uiMetaData$dialogContent
      }
    }
    
  }
  
  if (!("children" %in% names(x))) {
    return(df)
  }
  if (length(x$children) == 0) {
    return(df)
  }
  
  lsdf <- lapply(X = x$children, FUN = getWorkflowStepInfo)
  df2 <- do.call("rbind.fill", lsdf); rm(lsdf)
  df <- do.call("rbind.fill", list(df, df2))
  
  return(df)
  
}