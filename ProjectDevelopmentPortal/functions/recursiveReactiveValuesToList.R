recursiveReactiveValuesToList <- function(x) {
  
  if (is.reactivevalues(x)) { # MUST BE DONE BEFORE GOING NAME BY NAME THROUGH THE ITEMS.
    x <- isolate(reactiveValuesToList(x))
  }
  
  if (length(names(x)) == 0) {

    if (is.reactive(x)) {
      return(isolate(x))
    }
    
    return(x)
    
  }

  for (nm in names(x)) {
    x[[nm]] <- recursiveReactiveValuesToList(x[[nm]])
  }

  return(x)
}