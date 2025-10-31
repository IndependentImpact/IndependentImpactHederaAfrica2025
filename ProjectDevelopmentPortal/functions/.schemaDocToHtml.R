.schemaDocToHtml <- function(doc, dfSchDef) {
  
  txt <- c()
  
  for (k in 1:nrow(dfSchDef)) {
    
    #message(k)
    
    if (dfSchDef$type[k] != "complex" & !dfSchDef$repeated[k]) {
      
      txt <- c(txt,
               .docFieldToHtml(
                 title = dfSchDef$title[k], 
                 descr = dfSchDef$description[k], 
                 x = doc[[dfSchDef$key[k]]]))
      
      next 
    }
    
    if (dfSchDef$type[k] != "complex" & dfSchDef$repeated[k]) {
      
      x <- sprintf("[%d] %s", 1:length(doc[[dfSchDef$key[k]]]), doc[[dfSchDef$key[k]]])
      x <- paste(x, sep = "", collapse = "<br />")
      
      txt <- c(txt,
               .docFieldToHtml(
                 title = dfSchDef$title[k], 
                 descr = dfSchDef$description[k], 
                 x = x))
      
      next
    }
    
    if (dfSchDef$type[k] == "complex" & !dfSchDef$repeated[k]) {
      
      txt <- c(txt, "<div>")
      
      txt <- c(txt, 
               sprintf('
  <p>
  <strong>%s</strong><br />
  <em>%s</em><br />
  </p>
  ',
                       dfSchDef$title[k], 
                       dfSchDef$description[k]))
      
      txt <- c(txt,
               '<div style="margin-left: 25px;">',
               .schemaDocToHtml(doc = doc[[dfSchDef$key[k]]],
                                dfSchDef = dfSchDef$format[[k]]),
               '</div>')
      
      txt <- c(txt, "</div>")
      
      next
      
    }
    
    if (dfSchDef$type[k] == "complex" & dfSchDef$repeated[k]) {
      
      txt <- c(txt, "<div>")
      
      txt <- c(txt, 
               sprintf('
  <p>
  <strong>%s</strong><br />
  <em>%s</em><br />
  </p>
  ',
                       dfSchDef$title[k], 
                       dfSchDef$description[k]))
      
      for (m in 1:length(doc[[dfSchDef$key[k]]])) {
        txt <- c(txt,
                 '<div style="margin-left: 25px;">',
                 sprintf('[Item %d]', m),
                 .schemaDocToHtml(doc = doc[[dfSchDef$key[k]]][[m]],
                                  dfSchDef = dfSchDef$format[[k]]),
                 '</div>')
      }
      
      txt <- c(txt, "</div>")
      
      next
    }
    
  }
  
  txt <- paste(txt, sep = "", collapse = "")
  
  return(txt)
}





