# Generates markdown from a list-formed document.
#
genMrkdown <- function(x, nm = NULL, lv = 1) {
  
  if (length(x) == 0) {
    if (length(nm) == 1) {
      x <- ""
    } else {
      return("")
    }
  }
  
  if (!is.list(x)) {
    txt <- sprintf("%s %s\n%s", 
                   paste(rep(x = "#", times = lv), 
                         collapse = ""),
                   nm, 
                   ifelse(length(x) == 1,
                          x,
                          paste(sprintf("* %s", x), 
                                collapse = "\n")))
    return(txt)
  }
  
  # Reaching this point means x is a list.
  
  txt <- paste(
    sapply(X = 1:length(x), FUN = function(k) { 
      genMrkdown(x = x[[k]], 
                 nm = names(x)[k], 
                 lv = lv+1) }), 
    collapse = "\n\n")
  
  return(txt)
  
}

# Examples
#
# doc <- do.call(
# what = sprintf("getPubDoc.p%s", idPolicy), 
# args = list(docIdH = rvOther$dfDocs$id_h[idxRow], 
#             idBlockRetr = rvOther$dfDocs$id_block[idxRow],
#             accTknGsb = accTknGsb,
#             dbCon = dbCon,
#             contentOnly = FALSE,
#             idPolicyV = rvOther$dfDocs$id_policy_v[idxRow]))
# 
# cat(genMrkdown(x = doc, nm = "Document Title"))
#