# 
# 
# # Read all messages on the topic for our 'Independent Impact - License Application Workflow'.
# {
#   topicID <- "0.0.3891303"
#   msgs <- list()
#   bCont <- TRUE
#   xURL <- sprintf("https://testnet.mirrornode.hedera.com/api/v1/topics/%s/messages",
#                   topicID)
#   
#   while (bCont) {
#     
#     res <- httr::GET(xURL)
#     res <- parsed_content(res)
#     
#     if ("messages" %in% names(res)) {
#       nAdd <- length(res$messages)
#       msgs[length(msgs) + 1:nAdd] <- res$messages
#     }
#     
#     bCont <- FALSE
#     xURL <- NULL
#     
#     if ("links" %in% names(res)) {
#       if ("next" %in% names(res$links)) {
#         if (length(res$links[["next"]]) == 1) {
#           if (nchar(res$links[["next"]]) > 0) {
#             xURL <- sprintf("https://testnet.mirrornode.hedera.com%s", 
#                             res$links[["next"]])
#             bCont <- TRUE
#           }
#         }
#       }
#     }
#   }
#   
#   msgConts <- lapply(X = msgs, FUN = function(x) {
#     xc <- rawToChar(openssl::base64_decode(text = x$message))
#     cat(xc)
#     cat("\n")
#     return(jsonlite::fromJSON(rawToChar(openssl::base64_decode(text = x$message))))
#   })
#   
#   idxxPubPol <- which(sapply(X = msgConts, FUN = function(x) x$action) == "publish-policy")
#   x <- msgConts[[idxxPubPol[1]]]
#   ctch <- sapply(X = msgConts[idxxPubPol], FUN = function(x) {
#     cat(sprintf("v%s (%s)\n", x$version, x$instanceTopicId))
#   }); rm(ctch)
#   
#   idxxCreateTopic <- which(sapply(X = msgConts, FUN = function(x) x$action) == "create-topic")
#   childTopicIDs <- sapply(X = idxxCreateTopic, FUN = function(idx) {
#     if ("childId" %in% names(msgConts[[idx]])) {
#       return(msgConts[[idx]]$childId)
#     }  
#     return(NULL)
#   })
#   
#   idxxTokenIss <- which(sapply(X = msgConts, FUN = function(x) x$action) == "token-issue")
#   x <- msgConts[[idxxTokenIss[5]]]
# }
# 
# # Read all messages related to v3.0.6 (0.0.3974420).
# {
#   topicID <- "0.0.3974420"
#   msgs <- list()
#   bCont <- TRUE
#   xURL <- sprintf("https://testnet.mirrornode.hedera.com/api/v1/topics/%s/messages",
#                   topicID)
#   
#   while (bCont) {
#     
#     res <- httr::GET(xURL)
#     res <- parsed_content(res)
#     
#     if ("messages" %in% names(res)) {
#       nAdd <- length(res$messages)
#       msgs[length(msgs) + 1:nAdd] <- res$messages
#     }
#     
#     bCont <- FALSE
#     xURL <- NULL
#     
#     if ("links" %in% names(res)) {
#       if ("next" %in% names(res$links)) {
#         if (length(res$links[["next"]]) == 1) {
#           if (nchar(res$links[["next"]]) > 0) {
#             xURL <- sprintf("https://testnet.mirrornode.hedera.com%s", 
#                             res$links[["next"]])
#             bCont <- TRUE
#           }
#         }
#       }
#     }
#   }
#   
#   msgConts <- lapply(X = msgs, FUN = function(x) {
#     xc <- rawToChar(openssl::base64_decode(text = x$message))
#     cat(xc)
#     cat("\n")
#     return(jsonlite::fromJSON(rawToChar(openssl::base64_decode(text = x$message))))
#   })
#   
#   names(msgConts) <- sapply(X = msgs, FUN = function(x) x$consensus_timestamp)
# }
# 
# 
