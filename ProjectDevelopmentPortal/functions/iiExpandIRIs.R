# Replace calls to this function with novaRush::expandIRIs once the latter is working.

iiExpandIRIs <- function(x, keepCtx = FALSE) {
  
  if (!is.list(x)) {
    stop("x must be a list.")
  }
  if (!("@context" %in% names(x))) {
    return(x)
  }
  
  ctx <- x[["@context"]]
  if (!keepCtx) { x[["@context"]] <- NULL }
  ctx[c("@base", "")]
  ctx <- ctx[order(nchar(names(ctx)), decreasing = FALSE)]
  
  for (nsa in names(ctx)) {
    names(x) <- gsub(
      pattern = paste0("^", nsa, ":"),
      replacement = ctx[[nsa]],
      x = names(x))
    x[["@type"]] <- gsub(
      pattern = paste0("^", nsa, ":"),
      replacement = ctx[[nsa]],
      x = x[["@type"]])
  }
  
  return(x)
}

