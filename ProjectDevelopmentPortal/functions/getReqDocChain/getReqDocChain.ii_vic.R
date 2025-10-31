# The trust chain should be the result of matching the realised document chain
# to the document chain required by the standard. Such chains are prescribed
# for tokens, not for policies per se.

getReqDocChain.ii_vic <- function() {

  # TODO. Add version requirements.

  # level,
  # document type,
  # multiple allowed per ISSUANCE, (i.e., multiple parallel nodes allowed at this level of the graph)
  # successor document type,
  # multiple successors allowed per DOCUMENT,
  # predecessor document type,
  # multiple predecessors allowed per DOCUMENT,
  lvls = list(
    # list(1, "VIC", FALSE, "(none)", FALSE, "DR-VIC-IR", FALSE),
    # list(1, "DR-VIC-IR", FALSE, "(none)", FALSE, "VIC-IR", FALSE),
    # list(2, "VIC-IR", FALSE, "DR-VIC-IR", FALSE, "MR", FALSE),
    # list(3, "DR-MR", FALSE, "VIC-IR", FALSE, "MR", FALSE),
    list(1, "VIC", FALSE, "(none)", FALSE, "DR-MR", FALSE),
    list(2, "DR-MR", FALSE, "DR-VIC-IR", FALSE, "MR", FALSE),
    list(3, "MR", FALSE, "DR-MR", FALSE, "DR-DLR", TRUE),
    list(4, "DR-DLR", TRUE, "MR", FALSE, "DLR", FALSE),
    list(5, "DLR", TRUE, "DR-DLR", FALSE, "DR-PRR", FALSE),
    list(6, "DR-PRR", FALSE, "DLR", TRUE, "PRR", FALSE),
    list(7, "PRR", FALSE, "DR-PRR", FALSE, "DR-CIR", FALSE),
    list(8, "DR-CIR", FALSE, "PRR", FALSE, "CIR", FALSE),
    list(9, "CIR", FALSE, "DR-CIR", FALSE, "(multiple)", TRUE),
    list(10, "DR-PDD-XA", FALSE, "CIR", FALSE, "PDD-XA", FALSE),
    list(10, "DR-PDD-XB", FALSE, "CIR", FALSE, "PDD-XB", FALSE),
    list(10, "DR-PDD-XC", FALSE, "CIR", FALSE, "PDD-XC", FALSE),
    list(11, "PDD-XA", FALSE, "DR-PDD-XA", FALSE, "DR-PLA", FALSE),
    list(11, "PDD-XB", FALSE, "DR-PDD-XB", FALSE, "DR-PLA", FALSE),
    list(11, "PDD-XC", FALSE, "DR-PDD-XC", FALSE, "DR-PLA", FALSE),
    list(12, "DR-PLA", FALSE, "(multiple)", TRUE, "PLA", FALSE),
    list(13, "PLA", FALSE, "DR-PLA", FALSE, "(none)", NA))

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

# getReqDocChain.ii_vic <- function() {
# 
#   # TODO. Add version requirements.
# 
#   # level,
#   # document type,
#   # multiple allowed per ISSUANCE, (i.e., multiple parallel nodes allowed at this level of the graph)
#   # successor document type,
#   # multiple successors allowed per DOCUMENT,
#   # predecessor document type,
#   # multiple predecessors allowed per DOCUMENT,
#   lvls = list(
#     list(1, "DR-TIR", NA, "(none)", FALSE, "TIR", FALSE),
#     list(2, "TIR", FALSE, "DR-TIR", FALSE, "MR", FALSE),
#     list(3, "DR-MR", FALSE, "TIR", FALSE, "MR", FALSE),
#     list(4, "MR", FALSE, "DR-MR", FALSE, "DR-DLR", TRUE),
#     list(5, "DR-DLR", TRUE, "MR", FALSE, "DLR", FALSE),
#     list(6, "DLR", TRUE, "DR-DLR", FALSE, "DR-PRR", FALSE),
#     list(7, "DR-PRR", FALSE, "DLR", TRUE, "PRR", FALSE),
#     list(8, "PRR", FALSE, "DR-PRR", FALSE, "DR-CIR", FALSE),
#     list(9, "DR-CIR", FALSE, "PRR", FALSE, "CIR", FALSE),
#     list(10, "CIR", FALSE, "DR-CIR", FALSE, "(multiple)", TRUE),
#     list(11, "DR-PDD-XA", FALSE, "CIR", FALSE, "PDD-XA", FALSE),
#     list(11, "DR-PDD-XB", FALSE, "CIR", FALSE, "PDD-XB", FALSE),
#     list(11, "DR-PDD-XC", FALSE, "CIR", FALSE, "PDD-XC", FALSE),
#     list(11, "DR-PDD-XD", FALSE, "CIR", FALSE, "PDD-XD", FALSE),
#     list(11, "DR-PDD-XE", FALSE, "CIR", FALSE, "PDD-XE", FALSE),
#     list(12, "PDD-XA", FALSE, "DR-PDD-XA", FALSE, "DR-PLA", FALSE),
#     list(12, "PDD-XB", FALSE, "DR-PDD-XB", FALSE, "DR-PLA", FALSE),
#     list(12, "PDD-XC", FALSE, "DR-PDD-XC", FALSE, "DR-PLA", FALSE),
#     list(12, "PDD-XD", FALSE, "DR-PDD-XD", FALSE, "DR-PLA", FALSE),
#     list(12, "PDD-XE", FALSE, "DR-PDD-XE", FALSE, "DR-PLA", FALSE),
#     list(13, "DR-PLA", FALSE, "(multiple)", TRUE, "PLA", FALSE),
#     list(14, "PLA", FALSE, "DR-PLA", FALSE, "(none)", NA))
# 
#   df <- data.frame(
#     level = sapply(X = lvls, FUN = function(x) x[[1]]),
#     type_document = sapply(X = lvls, FUN = function(x) x[[2]]),
#     multiple = sapply(X = lvls, FUN = function(x) x[[3]]),
#     type_document_successor = sapply(X = lvls, FUN = function(x) x[[4]]),
#     multiple_successors = sapply(X = lvls, FUN = function(x) x[[5]]),
#     type_document_predecessor = sapply(X = lvls, FUN = function(x) x[[6]]),
#     multiple_predecessors = sapply(X = lvls, FUN = function(x) x[[7]]))
# 
#   return(df)
# }

