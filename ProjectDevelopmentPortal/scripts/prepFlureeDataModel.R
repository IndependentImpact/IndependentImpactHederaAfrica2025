


# TODO. Do not read .jsonld files from your local directories. These files are 
# all on the web and should be read from there, versioned and all.



# Helper functions.
{
  combineSkosDefinitionAndRdfsComment <- function(x) {
    
    idx <- which(names(x) == "skos:definition")
    if (length(idx) == 0) { return(x) }
    
    x[[idx]] <- sprintf("Definition: %s", unlist(x[[idx]]))
    
    idx <- which(names(x) == "rdfs:comment")
    if (length(idx) == 1) {
      x[[idx]] <- sprintf("Note: %s", unlist(x[[idx]]))
    }
    
    x[["rdfs:comment"]] <- c(x[["skos:definition"]], x[["rdfs:comment"]])
    x[["skos:definition"]] <- NULL
    
    return(x)
  }
  
  fixCtx <- function(x, fixes) {
    
    for (k in 1:length(fixes)) {
      
      names(x) <- gsub(
        pattern = names(fixes)[k], 
        replacement = fixes[[k]], 
        x = names(x), 
        fixed = TRUE)
      
      x[["@id"]] <- gsub(
        pattern = names(fixes)[k], 
        replacement = fixes[[k]], 
        x = x[["@id"]], 
        fixed = TRUE)
      
      x[["@type"]] <- gsub(
        pattern = names(fixes)[k], 
        replacement = fixes[[k]], 
        x = x[["@type"]], 
        fixed = TRUE)
    }
    
    for (key in c("rdfs:subClassOf", "rdfs:domain", "rdfs:range")) {
      
      if (!(key %in% names(x))) { next }
      
      for (k in 1:length(fixes)) {
        
        x[[key]] <- lapply(X = x[[key]], FUN = function(y) {
          if ("@id" %in% names(y)) {
            y[["@id"]] <- gsub(
              pattern = names(fixes)[k], 
              replacement = fixes[[k]], 
              x = y[["@id"]], 
              fixed = TRUE)
          }
          return(y)
        })
        
      }
      
    }
    
    return(x)
    
  }
  
  shrinkIRIs <- function(x, ctx) {
    
    for (k in 1:length(ctx)) {
      
      names(x) <- gsub(
        pattern = ctx[[k]], 
        replacement = paste0(names(ctx)[k], ":"), 
        x = names(x), 
        fixed = TRUE)
      
      x[["@id"]] <- gsub(
        pattern = ctx[[k]], 
        replacement = paste0(names(ctx)[k], ":"), 
        x = x[["@id"]], 
        fixed = TRUE)
      
      x[["@type"]] <- gsub(
        pattern = ctx[[k]], 
        replacement = paste0(names(ctx)[k], ":"), 
        x = x[["@type"]], 
        fixed = TRUE)
    }
    
    for (key in c("rdfs:subClassOf", "rdfs:domain", "rdfs:range")) {
      
      if (!(key %in% names(x))) { next }
      
      for (k in 1:length(ctx)) {
        
        x[[key]] <- lapply(X = x[[key]], FUN = function(y) {
          if ("@id" %in% names(y)) {
            y[["@id"]] <- gsub(
              pattern = ctx[[k]], 
              replacement = paste0(names(ctx)[k], ":"), 
              x = y[["@id"]], 
              fixed = TRUE)
          }
          return(y)
        })
        
      }
      
    }
    
    return(x)
    
  }
  
  # TODO: Merge ...F1 and ...F2.
  
  addToFlureeDataModelF1 <- function(dmId, fp) {
    
    lsOnt <- jsonlite::fromJSON(
      txt = fp, 
      simplifyDataFrame = FALSE, 
      simplifyMatrix = FALSE, 
      flatten = FALSE, 
      simplifyVector = TRUE)
    
    types <- sapply(X = lsOnt[["@graph"]], FUN = function(x) x[["@type"]])
    #table(types)
    lsClasses <- lsOnt[["@graph"]][types == "rdfs:Class"]
    lsProps <- lsOnt[["@graph"]][types == "rdf:Property"]
    
    # Rename "schema:domainIncludes" to "rdfs:domain", and "schema:rangeIncludes" to
    # "rdfs:range".
    lsProps <- lapply(X = lsProps, FUN = function(x) {
      names(x)[names(x) == "schema:domainIncludes"] <- "rdfs:domain"
      names(x)[names(x) == "schema:rangeIncludes"] <- "rdfs:range"
      return(x)
    })
    
    # Fluree does not understand skos:definition, so we need to replace our
    # skos:definition with rdfs:comment, but still distinguish between comments
    # and definitions. 
    {
      lsClasses <- lapply(
        X = lsClasses, 
        FUN = combineSkosDefinitionAndRdfsComment)
      lsProps <- lapply(
        X = lsProps, 
        FUN = combineSkosDefinitionAndRdfsComment)
    }
    
    ctx <- c(
      list("f" = "https://ns.flur.ee/ledger#"),
      lsOnt[["@context"]])
    
    lsDataModel <- list(
      "@context" = ctx,
      "@type" = "f:DataModel",
      "@id" = dmId,
      "f:classes" = lsClasses,
      #"f:properties" = lsProps
      "@graph" = lsProps)
    
    res <- novaRush::flureeInsert(
      data = lsDataModel, 
      config = defaultFlureeConf, 
      signTransaction = FALSE, 
      apiKey = Sys.getenv("API_KEY_FLUREE"))
    
    return(res)
  }
  
  addToFlureeDataModelF2 <- function(dmId, fp, ctx, ctxFixes) {

    lsOnt <- jsonlite::fromJSON(
      txt = fp, 
      simplifyDataFrame = FALSE, 
      simplifyMatrix = FALSE, 
      flatten = FALSE)
    
    lsOnt <- lapply(X = lsOnt, FUN = fixCtx, fixes = ctxFixes)
    
    types <- sapply(X = lsOnt, FUN = function(x) x[["@type"]])
    table(types)
    
    lsClasses <- lsOnt[types == "http://www.w3.org/2002/07/owl#Class"]
    lsProps <- lsOnt[types == "http://www.w3.org/2002/07/owl#ObjectProperty"]
    
    # Shrink IRIs.
    lsClasses <- lapply(X = lsClasses, FUN = shrinkIRIs, ctx = ctx)
    lsProps <- lapply(X = lsProps, FUN = shrinkIRIs, ctx = ctx)
    
    # Replace "owl:Class" with "rdfs:Class" and 
    # "owl:ObjectProperty" with "rdf:Property".
    {
      lsClasses <- lapply(X = lsClasses, FUN = function(x) {
        x[["@type"]] <- "rdfs:Class"
        return(x)
      })
      
      lsProps <- lapply(X = lsProps, FUN = function(x) {
        x[["@type"]] <- "rdf:Property"
        return(x)
      })
    }
    
    # Fluree does not understand skos:definition, so we need to replace our
    # skos:definition with rdfs:comment, but still distinguish between comments
    # and definitions. 
    {
      lsClasses <- lapply(
        X = lsClasses, 
        FUN = combineSkosDefinitionAndRdfsComment)
      lsProps <- lapply(
        X = lsProps, 
        FUN = combineSkosDefinitionAndRdfsComment)
    }
    
    # Pass into Fluree.
    {
      ctx <- c(
        list("f" = "https://ns.flur.ee/ledger#"),
        ctx)
      
      lsDataModel <- list(
        "@context" = ctx,
        "@type" = "f:DataModel",
        "@id" = dmId,
        "f:classes" = lsClasses,
        #"f:properties" = lsProps
        "@graph" = lsProps)
      
      res <- novaRush::flureeInsert(
        data = lsDataModel, 
        config = defaultFlureeConf, 
        signTransaction = FALSE, 
        apiKey = Sys.getenv("API_KEY_FLUREE"))
    }
    
    return(res)
  }
}

# Script-scoped variables:
dataModelId <- "dm-main"

# Start with Independent Impact core data model.
res <- addToFlureeDataModelF1(
  dmId = dataModelId, 
  fp = paste0(indImpDatModDir, "independent-impact.jsonld"))

# Add the Jellyfish "Data" namespace to the data model.
res <- addToFlureeDataModelF1(
  dmId = dataModelId, 
  fp = paste0(jellyDatModDir, "data.jsonld"))

# Add the (temporary) Hedera namespace to the data model.
res <- addToFlureeDataModelF1(
  dmId = dataModelId, 
  fp = paste0(hederaDatModDir, "hedera.jsonld"))

# Add aiao, impactont and claimont (but not infocomm).
{
  ctxFixes <- list(
    # wrong | right
    "http://w3id.org/aiao#" = "https://w3id.org/aiao#",
    "http://w3id.org/impactont#" = "https://w3id.org/impactont#",
    "http://w3id.org/claimont#" = "https://w3id.org/claimont#")
  
  ctx <- list(
    "rdfs" = "http://www.w3.org/2000/01/rdf-schema#",
    "skos" = "http://www.w3.org/2004/02/skos/core#",
    "owl" = "http://www.w3.org/2002/07/owl#",
    "aiao" = "https://w3id.org/aiao#",
    "impact" = "https://w3id.org/impactont#",
    "claimont" = "https://w3id.org/claimont#",
    "time" = "http://www.w3.org/2006/time#")
  
  res <- addToFlureeDataModelF2(
    dmId = dataModelId, 
    fp = paste0(aiaoDatModDir, "aiao.jsonld"), 
    ctx = ctx, 
    ctxFixes = ctxFixes)

  res <- addToFlureeDataModelF2(
    dmId = dataModelId, 
    fp = paste0(impactontDatModDir, "impactont.jsonld"), 
    ctx = ctx, 
    ctxFixes = ctxFixes)
  
  res <- addToFlureeDataModelF2(
    dmId = dataModelId, 
    fp = paste0(claimontDatModDir, "claimont.jsonld"), 
    ctx = ctx, 
    ctxFixes = ctxFixes)
}

# Add classes and properties from schema.org.
{
  # TODO. Nah, not necessary for hackathon MVP. In fact, it can actually confuse the model.
}

# Add from OWL.
{
  # TODO. Nah, not necessary for hackathon MVP.
}

# Add dcterms and dcelements
{
  # TODO. Nah, not necessary for hackathon MVP.
  
  # res <- httr::GET(
  #   url = "http://purl.org/dc/terms/" #, 
  #   #config = httr::add_headers('Content-type' = "application/json")
  #   )
  # resRaw <- httr::content(x = res, as = "raw")
  # cat(rawToChar(resRaw), file = "tmp/resRaw.ttl")
  # 
  # #TODO.
  # 
  # res <- httr::GET(
  #   url = "http://purl.org/dc/elements/1.1/" #, 
  #   #config = httr::add_headers('Content-type' = "application/json")
  # )
  # resRaw <- httr::content(x = res, as = "raw")
  # cat(rawToChar(resRaw), file = "tmp/resRaw.ttl")
  # 
  # # TODO.
}







