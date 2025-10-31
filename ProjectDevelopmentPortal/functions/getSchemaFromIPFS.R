getSchemaFromIPFS <- function(ipfsUrl, ipfsTkn) {
  
  res <- shell(cmd = sprintf("curl GET 'https://%s.ipfs.w3s.link/' --header 'token=%s'",
                             gsub(pattern = "ipfs://", 
                                  replacement = "", 
                                  x = ipfsUrl, 
                                  fixed = TRUE),
                             ipfsTkn), 
               intern = TRUE)
  idx <- grep(pattern = "$id", x = res, fixed = TRUE)
  schema <- paste(res[idx:length(res)], collapse = "")
  schema <- jsonlite::fromJSON(schema)
  
  return(schema)
}