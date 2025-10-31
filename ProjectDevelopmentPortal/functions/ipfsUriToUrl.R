ipfsUriToUrl <- function(x) {
  
  if (length(grep(pattern = "http", x = x, value = FALSE)) == 0) {
    x <- gsub(pattern = "ipfs://", replacement = "", x = x, fixed = TRUE)
    x <- sprintf("https://%s.ipfs.w3s.link", x)
  }
  
  return(x)
}
