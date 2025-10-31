
#' @title decryptFromIpfs
#' @description Retrieves an encrypted file from IPFS and decrypts it.
#' @param url Character. The full IPFS URL of the file to be decrypted, not just 
#'    the URI.
#' @return Character. The path to the decrypted file.
#' 
decryptFromIpfs <- function(url) {
  
  fpths <- NULL
  
  # Create temp location to download the encrypted file to.
  fpthDwnld <- tempfile(basename(url))
  
  # Download the file.
  res <- httr::GET(url = url, httr::write_disk(path = fpthDwnld))
  if (res$status_code != 200) {
    stCode <- res$status_code
    res <- httr::content(res, as = "parsed")
    stop(sprintf("Failed to download file from IPFS. Error: %s (code %s)",
                 res, stCode))
  }
  
  # If it's a zipped file, unzip it.
  if (grepl("\\.zip$", url)) {
    fpthUnzipped <- tempfile()
    unzip(zipfile = fpthDwnld, exdir = fpthUnzipped)
    fpths <- dir(fpthUnzipped, full.names = TRUE, recursive = TRUE)
  } else {
    fpths <- fpthDwnld
  }
  
  # Build a df for the results.
  dfRes <- data.frame(
    fp_enc = fpths,
    name = NA_character_,
    ext = NA_character_)
  
  # Remove the random chars suffixed to the file names.
  dfRes$name <- basename(dfRes$fp_enc)
  dfRes$name <- gsub(pattern = "_[[:alnum:]]{1,}$", 
                     replacement = "", 
                     x = dfRes$name, 
                     perl = TRUE)
  
  # Extract the original file extension.
  splts <- strsplit(x = dfRes$name, split = "_dot_", fixed = TRUE)
  dfRes$name <- sapply(X = splts, FUN = function(x) paste(x[-length(x)], collapse = ""))
  dfRes$ext <- sapply(X = splts, FUN = function(x) x[length(x)])
  dfRes$ext <- sprintf(".%s", dfRes$ext)
  
  # Decrypt the files.
  {
    # Create our key pair.
    key <- cyphr::keypair_openssl(
      pub = iwefdj$KEYPTH_CYPHR, 
      key = iwefdj$KEYPTH_CYPHR, 
      envelope = TRUE,
      password = iwefdj$PW_CYPHR) 
    
    # Process the files one by one.
    for (k in 1:nrow(dfRes)) {
      # Create a temporary file for the decrypted version of the file.
      dfRes$fp_dec[k] <- tempfile(pattern = dfRes$name[k])
      # Do the actual decryption.
      cyphr::decrypt_file(path = dfRes$fp_enc[k], 
                          key = key, 
                          dest = dfRes$fp_dec[k])
    }
    
    # Rename the decrypted files to include their extensions.
    file.rename(from = dfRes$fp_dec, to = sprintf("%s%s", dfRes$fp_dec, dfRes$ext))
    dfRes$fp_dec <- sprintf("%s%s", dfRes$fp_dec, dfRes$ext)
    
  }
  
  # Done.
  return(dfRes$fp_dec)
}