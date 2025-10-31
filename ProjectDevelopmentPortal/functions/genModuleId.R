
genModuleId <- function(modNm) {
  
  idxxCaps <- as.integer(gregexec(pattern = "[A-Z]", 
                                  text = modNm, 
                                  fixed = FALSE, 
                                  ignore.case = FALSE)[[1]])
  chars <- strsplit(x = modNm, split = "")[[1]]
  
  return(sprintf("%s%s%s%s",
                 tolower(chars[1]), 
                 tolower(paste(chars[idxxCaps], sep = "", collapse = "")),
                 toupper(chars[1]),
                 gsub(pattern = "[aeiou]", replacement = "", 
                      x = substr(x = modNm, start = 2, stop = nchar(modNm)))))
}