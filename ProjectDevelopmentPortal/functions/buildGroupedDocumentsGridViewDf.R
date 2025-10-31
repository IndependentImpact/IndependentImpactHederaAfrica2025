buildGroupedDocumentsGridViewDf <- function(df, dbCon, loginInfoUsr, idProject) {
  
  # Get schema titles.
  {
    q <- sprintf("SELECT * FROM tbl_schemas WHERE id IN(%s);",
                 paste(sprintf("'%s'", unique(df$id_schema)), 
                       collapse = ","))
    dfSchemas <- dbGetQuery(conn = dbCon, statement = q)
    idxx <- match(x = df$id_schema, table = dfSchemas$id)
    df$title <- dfSchemas$title[idxx]
  }
  
  # Distinguish between original documents and review documents.
  df$id_thread <- NA_integer_
  idxxReview <- grep(pattern = "Document Review", x = df$title)
  idxxOriginal <- setdiff(1:nrow(df), idxxReview)
  dfOriginals <- df[idxxOriginal, ]
  dfReviews <- df[idxxReview,]
  
  # Assign document thread IDs.
  # Note: A document thread starts with the first version of the original 
  # and ends with the last review.
  dfThreads <- as.data.frame(
    dplyr::summarise(
      dplyr::group_by(
        dfOriginals, 
        id_entity, step_workflow, title, identifying_content),
      .groups = "keep"))
  dfThreads$id_thread <- 1:nrow(dfThreads)
  
  dfOriginals$id_thread <- NULL
  dfOriginals <- merge.data.frame(
    x = dfOriginals, 
    y = dfThreads, 
    all.x = TRUE, 
    all.y = FALSE,
    by = c("id_entity", "step_workflow", "title", "identifying_content"))
  
  idxx <- match(x = dfReviews$id_msg_pred, table = dfOriginals$id_message_h)
  dfReviews$id_thread[which(!is.na(idxx))] <- dfOriginals$id_thread[idxx[!is.na(idxx)]]
  
  # Determine the latest version of each "original" (as opposed to "review") document.
  dfOriginals <- dfOriginals[order(dfOriginals$date_modified, decreasing = TRUE),]
  rownames(dfOriginals) <- 1:nrow(dfOriginals)
  dfOriginals <- dfOriginals[which(!duplicated(dfOriginals$id_thread)),]
  names(dfOriginals)[which(names(dfOriginals) == "oidx")] <- "oidx_original"
  
  # Determine the latest version of each "review" document.
  dfReviews <- dfReviews[order(dfReviews$date_modified, decreasing = TRUE),]
  if (nrow(dfReviews) > 0) {
    rownames(dfReviews) <- 1:nrow(dfReviews)
  }
  dfReviews <- dfReviews[which(!duplicated(dfReviews$id_thread)),]
  names(dfReviews) <- sprintf("%s_review", names(dfReviews))
  
  # Merge dfReviews and dfOriginals
  dfGrouped <- merge.data.frame(
    x = dfOriginals, y = dfReviews, 
    by.x = "id_message_h", by.y = "id_msg_pred_review", 
    all.x = TRUE,
    all.y = FALSE)
  
  # Combine the 'status' and 'outcome' vars.
  idxx <- which(dfGrouped$status == "PUBLISHED" & 
                  !(dfGrouped$outcome %in% c('APPROVED', 'REJECTED')))
  dfGrouped$status[idxx] <- "PENDING-REVIEW"
  idxx <- which(dfGrouped$outcome %in% c('APPROVED', 'REJECTED'))
  dfGrouped$status[idxx] <- sprintf("REVIEWED-%s", dfGrouped$outcome[idxx])
  
  # Parse 'identifying_content' into meaningful variables, where possible.
  #dfGrouped <- processIdentifyingContentForDisplay(dfGrouped)
  
  # Turn the "latest_version..." and "latest_review..." vars into action links.
  {
    dfGrouped$latest_version_timestamp <- as.character(dfGrouped$date_modified)
    idxx <- which(dfGrouped$status != "DRAFT") # Cannot view draft documents.
    dfGrouped$latest_version_timestamp[idxx] <- makeLinkHtmlForDocumentsGrid(
      documentIds = dfGrouped$oidx_original[idxx], 
      linkText = dfGrouped$latest_version_timestamp[idxx], 
      linkType = "view")
    
    dfGrouped$latest_review_timestamp <- as.character(dfGrouped$date_modified_review)
    dfGrouped$text_link_view_review <- dfGrouped$latest_review_timestamp
    idxx <- which(dfGrouped$date_modified_review <= dfGrouped$date_modified)
    dfGrouped$text_link_view_review[idxx] <- sprintf("%s (outdated)", dfGrouped$text_link_view_review[idxx])
    idxx <- which(!is.na(dfGrouped$latest_review_timestamp))
    dfGrouped$latest_review_timestamp[idxx] <- makeLinkHtmlForDocumentsGrid(
      documentIds = dfGrouped$oidx_review[idxx], 
      linkText = dfGrouped$text_link_view_review[idxx], 
      linkType = "view")
  }
  
  # Add 'Action' and 'View History' buttons.
  # TODO.
  dfGrouped[c("btn_action", "btn_view_history")] <- NA_character_
  
  # # TODO. Remove after debugging.
  # save(dfGrouped, file = sprintf("%sdfGrouped.Rda", tmpdir))
  
  # Prep for display.
  vnms <- c("title" = "Title",
            #"monitoring_period" = "Monitoring period",
            #"additional_info" = "Additional info", 
            "identifying_content" = "Additional info",
            "latest_version_timestamp" = "Latest version", 
            "latest_review_timestamp" = "Latest review", 
            "status" = "Status" #, 
            #"btn_view_history" = "View history", 
            #"btn_action" = "Action"
  )
  dfGrouped <- dfGrouped[names(vnms)]
  
  return(list(vnms = vnms, df = dfGrouped))
}
