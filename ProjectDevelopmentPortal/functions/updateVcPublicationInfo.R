
#' @description Tries to retrieve the publication information (message ID and 
#'    IPFS URL) for a set of VC documents. If successful, can also update the 
#'    database with the acquired information.
#' @param df Data frame. A data frame of VC document metadata. Must contain 
#'    variables 'uuid', 'date_modified' and 'id_msg_policy'. 
#' @param bUpdateDbTbl Logical. If TRUE, the function will update the database
#'    table with the new information. If FALSE, the function will only update the
#'    information in the data frame provided.
#' @param tblNm Character. The name of the table in the database to update if 
#'    bUpdateDbTbl is TRUE.
#' @return Data frame. The argument passed to 'df' with variables 'id_message' and
#'    'url_ipfs' added (or updated), if information retrieval were successful.
#'    
updateVcPublicationInfo <- function(df, dbCon, bUpdateDbTbl = TRUE, tblNm = NULL) {
  
  if (bUpdateDbTbl) {
    if (length(tblNm) != 1) {
      stop("'tblNm' must be of length = 1 when 'bUpdateDbTbl' = TRUE.")
    }
    if (is.na(tblNm)) {
      stop("'tblNm' cannot be NA when 'bUpdateDbTbl' = TRUE.")
    }
    if (nchar(tblNm) == 0) {
      stop("nchar(tblNm) must be > 0 when 'bUpdateDbTbl' = TRUE.")
    }
  }
  
  # Input checking.
  {
    if (!('uuid' %in% names(df))) {
      stop("'df' must contain a variable named 'uuid'.")
    }
    if (!('date_modified' %in% names(df))) {
      stop("'df' must contain a variable named 'date_modified'.")
    }
    if (!('id_msg_policy' %in% names(df))) {
      stop("The data frame must containa variable named 'id_msg_policy'.")
    }
  }
  
  # Get all topic IDs for the policies in the data frame.
  {
    q <- sprintf("SELECT id_message, id_topic_h, id_topic_instance_h, id_topic_synchronization_h FROM tbl_policies WHERE id_message IN(%s);",
                 paste(sprintf("'%s'", unique(df$id_msg_policy)), 
                       collapse = ","))
    dfTopics <- dbGetQuery(conn = dbCon, statement = q)
    dfTopics <- reshape2::melt(
      data = dfTopics, 
      id.vars = "id_message", 
      measure.vars = c("id_topic_h", "id_topic_instance_h", 
                       "id_topic_synchronization_h"))
  }

  # Now go through the documents one by one and try to retrieve their message 
  # IDs and IPFS URLs.
  if (!("id_message" %in% names(df))) { df$id_message <- NA_character_ }
  if (!("url_ipfs" %in% names(df))) { df$url_ipfs <- NA_character_ }
  for (r in which((is.na(df$id_message) | is.na(df$url_ipfs)) &
                  !is.na(df$uuid) &
                  !is.na(df$date_modified))) {

    # Try to find the Hedera network publication message for this VC.    
    if (!is.na(df$id_message[r])) {
      msg <- findVCpubMsg(messageId = df$id_message[r])
    } else {
      msg <- findVCpubMsg(
        docUuid = df$uuid[r],
        datePublished = df$date_modified[r],
        topicIds = dfTopics$value[which(dfTopics$id_message == df$id_msg_policy[r])])
    }
    
    if (length(msg) > 0) {
      
      # IPFS URI
      if (!is.na(msg$uri)) {
        if (length(grep(pattern = "ipfs://", x = msg$uri, fixed = TRUE)) != 1) {
          message(sprintf("WARNING: Invalid IPFS URI returned: %s", msg$uri))
          msg$uri <- NA_character_
        }
      }
      
      # Message ID
      if (!is.na(msg$messageId)) {
        if (is.na(as.numeric(msg$messageId))) {
          message(sprintf("WARNING: Invalid message ID returned: %s", msg$messageId))
          msg$messageId <- NA_character_
        }
      }
      
      # Update df and the db table.
      if (!is.na(msg$uri) & !is.na(msg$messageId)) {
        
        # Update 'df'.
        df$id_message[r] <- msg$messageId
        df$url_ipfs[r] <- msg$uri
        
        # Update the db table, if requested.
        if (bUpdateDbTbl) {
          q <- sprintf("UPDATE %s SET id_message = '%s', url_ipfs = '%s' WHERE uuid = '%s';",
                       tblNm,
                       msg$messageId,
                       msg$uri,
                       df$uuid[r])
          dbSendStatement(conn = dbCon, statement = q)
        }

      }
      
    }
  }
  
  return(df)
}