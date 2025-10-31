.reviewDocToHtml <- function(doc) {
  
  txt <- c()
  
  doc <- doc$review
  
  for (k in 1:length(doc)) {
    
    txt <- c(txt, "<div>")
    
    # The original response is sometimes JSON and sometimes not. If it is JSON,
    # convert it to an R list, because it may not have been "prettified".
    tryCatch({
      doc[[k]]$original_response <- jsonlite::fromJSON(doc[[k]]$original_response)
    }, error = function(e) {})
    
    txt <- c(txt,
             
             sprintf('
  <p>
  <strong>%s</strong><br />
  <em>%s</em><br />
  </p>
  ',
                     doc[[k]]$field_title,
                     doc[[k]]$field_prompt),
             
             
             '<div style="margin-left: 25px;">',
             
             '
  <p>
  <strong>Original Response</strong><br />
  </p>
  ',
             
             htmltools::HTML(
               paste0(
                 "<pre>", 
                 jsonlite::toJSON(
                   x = doc[[k]]$original_response, 
                   dataframe = "rows", 
                   Date = "ISO8601", 
                   factor = "string", 
                   null = "list", 
                   na = "string", 
                   auto_unbox = TRUE, 
                   pretty = TRUE), 
                 "</pre>")),
             
             sprintf('
  <p>
  <strong>Reviewer Decision</strong> %s
  </p>
  ', doc[[k]]$reviewer_decision),
             
             sprintf('
  <p>
  <strong>Reviewer Feedback</strong> %s
  </p>
  ', doc[[k]]$reviewer_feedback),
             
             '</div>')
    
    txt <- c(txt, "</div>")
    
  }
  
  txt <- paste(txt, sep = "", collapse = "")
  
  return(txt)
  
}