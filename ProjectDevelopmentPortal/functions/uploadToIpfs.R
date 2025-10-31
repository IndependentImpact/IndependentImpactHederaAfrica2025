#'@description A function to upload a file to IPFS. Must be used within a Shiny
#'  UI context.
#'@param df Data frame. Typically the output from a GfileInput. Must contain the 
#'  following variables:
#'    - datapath: The path to the file to upload.
#'    - name: The original name of the file to upload, including its extension.
#'  Can optionally also contain a variable 'ext' which provides the file extension 
#'  separately, although it is not necessary. Recommended if non-standard file
#'  extensions are present.
#'@param encrypt Logical. Should the file content be encrypted before uploading
#'  it to IPFS?
#'@param zip Logical. Should the file(s) be compressed into a single ZIP archive
#'  before uploading it(/them) to IPFS?
#'@param cidOnly Logical. Should only the CID of the resulting IPFS file be 
#'  returned? If FALSE, the full IPFS URL will be returned (e.g.,
#'  https://w3s.link/ipfs/bafybeih77bjsiqy7...h3hmhoi/filec32032....json)
#'@param wrap Logical. Should the file be wrapped in an IPFS directory? 
#'  Default = TRUE to align with w3's default.
#'  Will be ignored if multiple files are uploaded unzipped.
#'@return Character or NULL. The IPFS URI of the uploaded file or NULL if
#'  the upload failed.
uploadToIpfs <- function(df, encrypt = FALSE, zip = FALSE, cidOnly = FALSE, wrap = TRUE) {
  
  # Clean up all temporary files upon exit - whether a successful exit or not.
  on.exit({
    if(exists("zPth")) {
      if (file.exists(zPth)) {
        file.remove(zPth)
      }
    }
    if("datapath_o" %in% colnames(df)) {
      # This means we have already replaced 'datapath' with temp files, so 
      # remove those temp files, but do not remove the original files.
      file.remove(df$datapath)
    }
  })
  
  # Try to determine the file extension(s), if not provided.
  if (!("ext" %in% names(df))) {
    splts <- strsplit(x = df$name, split = ".", fixed = TRUE)  
    df$ext <- sapply(X = splts, FUN = function(x) x[length(x)])
    df$ext <- sprintf(".%s", df$ext)
    idxx <- grep(pattern = ".", x = df$name, fixed = TRUE)
    df$ext[-idxx] <- NA_character_
  }
  
  # Remove any spaces and special characters from the file name.
  {
    splts <- strsplit(x = df$name, split = ".", fixed = TRUE)  
    df$name_safe <- sapply(X = splts, FUN = function(x) paste(x[-length(x)], sep = "", collapse = ""))
    df$name_safe <- gsub(pattern = "[^[:alnum:]]", replacement = "", x = df$name_safe)
    idxx <- which(!is.na(df$ext))
    df$name_safe[idxx] <- sprintf("%s%s", df$name_safe[idxx], df$ext[idxx])
  }
  
  # encrypt
  if (encrypt) {
    
    # Back up the original file path, because we'll change it soon.
    df$datapath_o <- df$datapath
    
    # Create a new file name to be used for the encrypted file, but which will
    # tell us what the original file extension was.
    df$name2 <- gsub(pattern = ".", 
                     replacement = "_dot_", 
                     x = df$name_safe, 
                     fixed = TRUE)
    df$name2 <- sprintf("%s_", df$name2)
    
    # Create our key pair.
    key <- cyphr::keypair_openssl(
      pub = iwefdj$KEYPTH_CYPHR, 
      key = iwefdj$KEYPTH_CYPHR, 
      envelope = TRUE,
      password = iwefdj$PW_CYPHR) 
    
    # Encrypt each file.
    for (k in 1:nrow(df)) {
      # Create temporary file for the encrypted version of the file, using the
      # extension-preserving new file name.
      df$datapath[k] <- tempfile(pattern = df$name2[k])
      # Do the actual encryption.
      cyphr::encrypt_file(path = df$datapath_o[k], 
                          key = key, 
                          dest = df$datapath[k])
    }
    
  }
  
  # zip if requested
  if (zip) {
    
    zPth <- tempfile()
    
    zip(zipfile = zPth, 
        files = df$datapath, 
        flags = "-j9X")
    
    zPth <- sprintf("%s.zip", zPth)
  }
  
  # upload to IPFS
  {
    if (zip) { 
      fPths <- zPth
    } else { 
      fPths <- as.character(df$datapath) 
    }
    
    res <- system2(
      command = "w3", 
      args = c(
        "up", 
        fPths, 
        ifelse(length(fPths) == 1 & !wrap, "--no-wrap", "--wrap")), 
      stdout = TRUE, 
      stderr = TRUE, 
      wait = TRUE, 
      timeout = 60, 
      env = NULL)
    if (length(res) == 0) {
      stop("length(res) == 0")
    } 
    
    if (zip) {
      # Remove the now-redundant temporary file.
      file.remove(zPth)
    }
  }
  
  # Get the IPFS URI of the uploaded file so that we can return it.
  {
    idxURI <- which(sapply(X = res, FUN = function(x) {
      length(grep(pattern = "https://", x = x, fixed = TRUE)) == 1
    }))
    if (length(idxURI) != 1) {
      stop("length(idxURI) != 1")
    }
    
    ipfsURI <- res[idxURI]
    idxStart <- as.integer(
      gregexpr(pattern = "https://", 
               text = ipfsURI, 
               fixed = TRUE))
    if (idxStart < 0) {
      stop("idxStart < 0")
    }
    
    ipfsURI <- substr(x = ipfsURI, 
                      start = idxStart, 
                      stop = nchar(ipfsURI))
  }
  
  if (cidOnly) {
    splts <- strsplit(x = ipfsURI, split = "/", fixed = TRUE)
    return(tail(splts[[1]], 1))
  }
  
  return(paste0(
    ipfsURI, 
    ifelse(
      length(fPths) == 1 & !wrap, 
      "", 
      paste0("/", basename(fPths)))))
  
}





# key <- cyphr::keypair_openssl(
#   pub = iwefdj$KEYPTH_CYPHR, 
#   key = iwefdj$KEYPTH_CYPHR, 
#   envelope = TRUE,
#   password = iwefdj$PW_CYPHR) # TODO. Possibly set param 'authenticated' here to FALSE.
# 
# dfFiles <- data.frame(
#   datapath = sprintf("%s%s", tmpdir,
#                      c("myFirstPlainTextFile.txt",
#                        "myOtherPlainTextFile.txt")),
#   name = c("myFirstPlainTextFile.txt", "myOtherPlainTextFile.txt"),
#   ext = c(".txt", ".txt"))
# 
# 
# # Test 1: Single file, no encryption, no zipping.
# df <- dfFiles[1,]
# uploadToIpfs(df, encrypt = FALSE, zip = FALSE)
# 
# # Test 2: Single file, no encryption, with zipping.
# df <- dfFiles[1,]
# uploadToIpfs(df, encrypt = FALSE, zip = TRUE)
# 
# Test 3: Single file, with encryption, no zipping.
# df <- dfFiles[1,]
# uploadToIpfs(df, encrypt = TRUE, zip = FALSE)
# tmpOut <- tempfile(fileext = ".txt")
# cyphr::decrypt_file(path = "...Downloads/myFirstPlainTextFile_dot_txt_632053971e06",
#                     key = key,
#                     dest = tmpOut)
# 
# # Test 4: Single file, with encryption, with zipping.
# df <- dfFiles[1,]
# uploadToIpfs(df, encrypt = TRUE, zip = TRUE)
# 
# # Test 5: Multiple files, no encryption, no zipping.
# df <- dfFiles
# uploadToIpfs(df, encrypt = FALSE, zip = FALSE)
# 
# # Test 6: Multiple files, no encryption, with zipping.
# df <- dfFiles
# uploadToIpfs(df, encrypt = FALSE, zip = TRUE)
# 
# # Test 7: Multiple files, with encryption, no zipping.
# df <- dfFiles
# uploadToIpfs(df, encrypt = TRUE, zip = FALSE)
# 
# # Test 8: Multiple files, with encryption, with zipping.
# df <- dfFiles
# uploadToIpfs(df, encrypt = TRUE, zip = TRUE)
# 
# 
# fpth <- "...Downloads/file6320211023bf/myOtherPlainTextFile_dot_txt_6320732327dd"
# fnm <- basename(fpth)
# fnm <- gsub(pattern = "_dot_", replacement = ".", x = fnm, fixed = TRUE)
# fnm <- strsplit(x = fnm, split = "_", fixed = TRUE)[[1]]
# fnm <- fnm[-(length(fnm))]
# fnm <- paste(fnm, collapse = "_")
# fext <- strsplit(x = fnm, split = ".", fixed = TRUE)[[1]]
# fext <- fext[length(fext)]
# fext <- sprintf(".%s", fext)
# 
# tmpOut <- tempfile(fileext = fext)
# cyphr::decrypt_file(path = fpth,
#                     key = key,
#                     dest = tmpOut)





