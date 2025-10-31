# The trust chain should be the result of matching the realised document chain
# to the document chain required by the standard. Such chains are prescribed
# for tokens, not for policies per se.

getReqDocChain.ii_l_pd <- function() {
  
  # TODO. Add version requirements.
  
  # level, 
  # document type, 
  # multiple allowed per ISSUANCE, (i.e., multiple parallel nodes allowed at this level of the graph) 
  # successor document type,
  # multiple successors allowed per DOCUMENT,
  # predecessor document type,
  # multiple predecessors allowed per DOCUMENT,
  lvls = list(
    list(1, "DR-RA", NA, "(none)", FALSE, "RA", FALSE),
    list(2, "RA", FALSE, "DR-RA", FALSE, "DR-AD", FALSE),
    list(3, "DR-AD", FALSE, "RA", FALSE, "AD", FALSE),
    list(4, "AD", FALSE, "DR-AD", FALSE, "(none)", NA))
  
  df <- data.frame(
    level = sapply(X = lvls, FUN = function(x) x[[1]]),
    type_document = sapply(X = lvls, FUN = function(x) x[[2]]),
    multiple = sapply(X = lvls, FUN = function(x) x[[3]]),
    type_document_successor = sapply(X = lvls, FUN = function(x) x[[4]]),
    multiple_successors = sapply(X = lvls, FUN = function(x) x[[5]]),
    type_document_predecessor = sapply(X = lvls, FUN = function(x) x[[6]]),
    multiple_predecessors = sapply(X = lvls, FUN = function(x) x[[7]]))
  
  return(df)
}





