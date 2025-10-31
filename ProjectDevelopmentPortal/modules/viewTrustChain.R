viewTrustChainUI <- function(id) {

  ns <- NS(id)

  fluidPage(
    fluidRow(
      column(
        width = 12,
        htmlOutput(
          outputId = ns("htmloTitle")))),
    br(),
    fluidRow(
      column(
        width = 12,
        textOutput(outputId = ns("toErr")))),
    fluidRow(
      column(
        width = 5,
        htmlOutput(
          outputId = ns("htmloTrustChainDoc"))),
      column(
        width = 7,
        plotlyOutput(
          outputId = ns("plyoTrustChain"), height = "700px"))))
}




# msgId = "1711221401.334254003",
# ipfsUrl = "ipfs://bafkreig7ie5zka7fo226gkg43pqvzufrjoacxqucvgz6tink2i2olljfha")

viewTrustChainServer <- function(id,
                                 msgId = NULL, # must be provided if ipfsUrl is not provided
                                 ipfsUrl = NULL, # must be provided if msgId is not provided
                                 tokenIdH,
                                 serialNos = NULL,
                                 dbCon,
                                 idProject = NULL,
                                 loginInfoUsr = NULL) {

  moduleServer(
    id = id,
    function(input, output, session) {

      ns <- session$ns

      rvToReturn <- reactiveValues()

      rvOther <- reactiveValues()
      rvOther$dfTkn <- NULL
      rvOther$dfReqDocChain <- NULL
      rvOther$dfRealDocChain <- NULL
      rvOther$dfNodes <- NULL
      rvOther$dfLines <- NULL
      rvOther$init <- NULL
      rvOther$contentLeft <- NULL

      outputMsgs <- reactiveValues()
      outputMsgs$toErr <- NULL

      # inputs -----------------------------------------------------------------

      # Populate rvOther$dfReqDocChain and rvOther$dfRealDocChain.
      observeEvent(rvOther$init, handlerExpr = {

        bError <- FALSE

        withProgress({

          # Get more info about the token type.
          tryCatch({
            print(paste("tokenIdH: ", tokenIdH))
            q <- sprintf("SELECT * FROM tbl_tokens WHERE id_token_h = '%s';",
                         tokenIdH)
            rvOther$dfTkn <- dbGetQuery(conn = dbCon, statement = q)

          }, error = function(e) {

            bError <- TRUE
            outputMsgs$toErr <- "Failed to retrieve token information. Please try again later. If this problem persists, please contact the system administrator."

          }, finally = {
            if (bError) { return(invisible(1)) }
          })

          incProgress(amount = 1)

          # Get required/prescribed document chain.
          tryCatch({

            rvOther$dfReqDocChain <- do.call(
              what = sprintf("getReqDocChain.%s",
                             novaUtils::format_char(rvOther$dfTkn$symbol)),
              args = list())

          }, error = function(e) {

            bError <- TRUE
            outputMsgs$toErr <- "Failed to retrieve required document chain. Please try again later. If this problem persists, please contact the system administrator."

          }, finally = {
            if (bError) { return(invisible(2)) }
          })

          incProgress(amount = 1)

          # Get realised document chain.
          tryCatch({

            dfReDoCh <- getRealDocChain(
              msgId = msgId,
              ipfsUrl = ipfsUrl)
            rvOther$dfRealDocChain <- dfReDoCh[which(!is.na(dfReDoCh$uuid)),]

          }, error = function(e) {

            bError <- TRUE
            outputMsgs$toErr <- "Failed to retrieve realised document chain. Please try again later. If this problem persists, please contact the system administrator."

          }, finally = {
            if (bError) { return(invisible(3)) }
          })

          incProgress(amount = 1)

        },
        min = 0,
        max = 3,
        value = 0,
        message = "Tracing document histories...")

      })

      # Construct rvOther$dfNodes.
      observeEvent(rvOther$dfRealDocChain, handlerExpr = {

        validate(need(rvOther$dfReqDocChain, message = FALSE))
        if (length(rvOther$dfReqDocChain) == 0) { return (NULL) }
        if (nrow(rvOther$dfReqDocChain) == 0) { return (NULL) }

        validate(need(rvOther$dfRealDocChain, message = FALSE))
        if (length(rvOther$dfRealDocChain) == 0) { return (NULL) }
        if (nrow(rvOther$dfRealDocChain) == 0) { return (NULL) }

        # Match realised document chain to required document chain.

        # Each document in the document chain will have one and exactly one node
        # in the trust chain graph. Construct the foundation of dfNodes.
        {
          dfNodes <- rvOther$dfRealDocChain
          dfNodes <- dfNodes[which(!duplicated(dfNodes$id_message)),]

          # Get policy names and version numbers.
          {
            q <- "SELECT * FROM tbl_policies;"
            dfPols <- dbGetQuery(conn = dbCon, statement = q)
            dfNodes$name_policy <- NA_character_
            dfNodes$ver_policy <- NA_character_
            idxx <- match(x = dfNodes$id_g_policy, table = dfPols$id_g)
            dfNodes$name_policy[which(!is.na(idxx))] <- dfPols$name[idxx[!is.na(idxx)]]
            dfNodes$ver_policy[which(!is.na(idxx))] <- dfPols$version[idxx[!is.na(idxx)]]
          }

          # Get schema names and version numbers.
          {
            q <- "SELECT * FROM tbl_schemas;"
            dfSchemas <- dbGetQuery(conn = dbCon, statement = q)
            dfNodes$name_schema <- NA_character_
            dfNodes$ver_schema <- NA_character_
            dfNodes$type_vc <- sprintf("#%s", dfNodes$type_vc)
            idxx <- match(x = dfNodes$type_vc, table = dfSchemas$iri)
            dfNodes$name_schema[which(!is.na(idxx))] <- dfSchemas$name[idxx[!is.na(idxx)]]
            dfNodes$ver_schema[which(!is.na(idxx))] <- dfSchemas$version[idxx[!is.na(idxx)]]
          }
          
          # Get schema name abbreviations.
          {
            dfNodes$abbr_schema <- sapply(
              X = dfNodes$name_schema, FUN = function(x) {
                
                idxOpen <- as.integer(gregexpr(pattern = "(", text = x, fixed = TRUE))
                if (length(idxOpen) == 0) { return(NA_character_) }
                idxOpen <- idxOpen[length(idxOpen)]
                
                idxClose <- as.integer(gregexpr(pattern = ")", text = x, fixed = TRUE))
                if (length(idxClose) == 0) { idxClose <- nchar(x) }
                idxClose <- idxClose[length(idxClose)]
                
                return(substr(x, idxOpen + 1, idxClose - 1))
              })
            
            # Determine the schema name abbreviations for the generic review 
            # documents.
            {
              dfNodes$abbr_schema_pred <- NA_character_
              idxx <- match(x = dfNodes$id_msg_pred, table = dfNodes$id_message)
              dfNodes$abbr_schema_pred[which(!is.na(idxx))] <- dfNodes$abbr_schema[idxx[!is.na(idxx)]]
              
              idxxDR <- grep(pattern = "Document Review", x = dfNodes$name_schema, ignore.case = TRUE)
              idxxNAabbr <- which(is.na(dfNodes$abbr_schema) | nchar(dfNodes$abbr_schema) == 0 & !is.na(dfNodes$abbr_schema_pred))
              idxx <- intersect(idxxDR, idxxNAabbr)
              dfNodes$abbr_schema[idxx] <- sprintf("DR-%s", dfNodes$abbr_schema_pred[idxx])
              
              dfNodes$abbr_schema_pred <- NULL
            }
          }

          # Assign node IDs.
          dfNodes$id_node <- 1:nrow(dfNodes)
        }

        # First pass: Assign a graph row ("level") to each document.
        {
          dfNodes$lvl <- NA_integer_
          idxx <- match(
            x = dfNodes$abbr_schema,
            table = rvOther$dfReqDocChain$type_document)
          dfNodes$lvl[which(!is.na(idxx))] <- rvOther$dfReqDocChain$level[idxx[!is.na(idxx)]]
        }

        # Second pass: Assign a graph column to each document.
        {
          dfNodes$col <- NA_integer_

          # Determine the number of columns we need in our graph.
          {
            df <- data.frame(
              dplyr::summarise(
                dplyr::group_by(
                  dfNodes, lvl),
                n = dplyr::n(),
                .groups = "keep"))

            nColsMax <- max(df$n)

            # If this is an even number, make it odd, so that we can have a nice
            # centre line.
            if (as.numeric(as.integer(nColsMax/2)) == (nColsMax/2)) {
              nColsMax <- nColsMax + 1
            }
          }

          for (lvl in (min(rvOther$dfReqDocChain$level)):(max(rvOther$dfReqDocChain$level))) {
            
            idxxNodes <- which(dfNodes$lvl == lvl)
            
            # If there are multiple, sort them correctly.
            if (length(idxxNodes) > 1) {
              idxxOrdr <- match(
                table = dfNodes$abbr_schema[idxxNodes], 
                x = rvOther$dfReqDocChain$type_document[which(rvOther$dfReqDocChain$level == lvl)])
              idxxNodes <- idxxNodes[idxxOrdr]
            }
            
            idxxCols <- 1:length(idxxNodes)
            idxxCols <- idxxCols + ((nColsMax - length(idxxCols)) / 2)
            dfNodes$col[idxxNodes] <- idxxCols
          }
        }
        
        # Tag levels with problems.
        # Tag individual nodes with problems.
        # Add empty nodes for missing documents.
        {
          # TODO.
        }

        dfNodes$id_msg_pred <- NULL
        dfNodes$url_ipfs_pred <- NULL
        
        # Reverse the levels for display purposes.
        dfNodes$lvl <- (max(dfNodes$lvl)+1) - dfNodes$lvl

        # Join schema name and version for convenience.
        dfNodes$lbl_schema <- sprintf("%s (v%s)",
                                      dfNodes$name_schema,
                                      dfNodes$ver_schema)

        # Make a shortened version of did_issuer for display convenience.
        {
          dfNodes$did_issuer_short <- sprintf(
            "did:%s...%s",
            substr(dfNodes$did_issuer, start = 20, stop = 30),
            substr(dfNodes$did_issuer,
                   start = nchar(dfNodes$did_issuer)-10,
                   stop = nchar(dfNodes$did_issuer)))
        }

        # Make a shortened version of the IPFS URL for display convenience.
        {
          dfNodes$tmp <- nchar(dfNodes$url_ipfs)
          dfNodes$url_ipfs_short <- sprintf(
            "%s...%s",
            substr(dfNodes$url_ipfs, start = 1, stop = 15),
            substr(dfNodes$url_ipfs,
                   start = dfNodes$tmp - 19,
                   stop = dfNodes$tmp))
          dfNodes$tmp <- NULL
        }

        # Retrieve the issuer's email address.
        {
          # Get the agent IDs from the DIDs.
          q <- sprintf("SELECT * FROM tbl_link_agents_x_dids WHERE did IN(%s);",
                       paste(sprintf("'%s'", unique(dfNodes$did_issuer)),
                             collapse = ","))
          dfAgDids <- dbGetQuery(conn = dbCon, statement = q)
          dfNodes <- merge.data.frame(
            x = dfNodes,
            y = dfAgDids,
            by.x = "did_issuer",
            by.y = "did",
            all.x = TRUE,
            all.y = FALSE)

          # Get all email addresses for the agents in question.
          q <- sprintf("SELECT * FROM tbl_link_agents_x_email_addresses WHERE id_agent IN(%s);",
                       paste(sprintf("'%s'", dfAgDids$id_agent),
                             collapse = ","))
          #alert(paste("rvOther$dfRealDocChain: ", q))
          dfEmAddrs <- dbGetQuery(conn = dbCon, statement = q)

          # Subset to only the most recent email address for each agent.
          dfEmAddrs <- dfEmAddrs[order(dfEmAddrs$oidx, decreasing = TRUE),]
          rownames(dfEmAddrs) <- 1:nrow(dfEmAddrs)
          dfEmAddrs <- dfEmAddrs[which(!duplicated(dfEmAddrs$id_agent)),]
          rownames(dfEmAddrs) <- 1:nrow(dfEmAddrs)

          # Merge with dfNodes.
          dfNodes <- merge.data.frame(
            x = dfNodes,
            y = dfEmAddrs,
            by = "id_agent",
            all.x = TRUE,
            all.y = FALSE)
        }

        # # Make y axis a date. Place each level at the minimum timestamp for that level.
        {
          dfNodes$date_issuance <- lubridate::as_datetime(dfNodes$date_issuance)
          # dfY <- data.frame(
          #   dplyr::summarise(
          #     dplyr::group_by(
          #       dfNodes, lvl),
          #     ts_lvl = min(date_issuance),
          #     .groups = "keep"))
          # dfNodes <- merge.data.frame(x = dfNodes, y = dfY, by = "lvl", all.x = TRUE)
        }

        # Done.
        rvOther$dfNodes <- dfNodes

      })

      # Construct rvOther$dfLines.
      observeEvent(rvOther$dfNodes, handlerExpr = {

        validate(need(rvOther$dfRealDocChain, message = FALSE))
        if (length(rvOther$dfRealDocChain) == 0) { return (NULL) }
        if (nrow(rvOther$dfRealDocChain) == 0) { return (NULL) }

        validate(need(rvOther$dfNodes, message = FALSE))
        if (length(rvOther$dfNodes) == 0) { return (NULL) }
        if (nrow(rvOther$dfNodes) == 0) { return (NULL) }

        dfRealDocChain <- rvOther$dfRealDocChain

        # Copy id_node to dfRealDocChain.
        idxx <- match(x = dfRealDocChain$url_ipfs,
                      table = rvOther$dfNodes$url_ipfs)
        dfRealDocChain$id_node <- rvOther$dfNodes$id_node[idxx]

        # Build dfLines.
        dfLines <- lapply(X = 1:nrow(dfRealDocChain), FUN = function(r) {

          if (is.na(dfRealDocChain$url_ipfs_pred[r])) { return(NULL) }

          idxNode <- which(rvOther$dfNodes$id_node == dfRealDocChain$id_node[r])
          xFrom <- rvOther$dfNodes$col[idxNode]
          yFrom <- rvOther$dfNodes$lvl[idxNode]

          xTo <- NA_real_
          yTo <- NA_real_
          idxNode <- which(rvOther$dfNodes$url_ipfs == dfRealDocChain$url_ipfs_pred[r])
          if (length(idxNode) == 1) {
            xTo <- rvOther$dfNodes$col[idxNode]
            yTo <- rvOther$dfNodes$lvl[idxNode]
          }

          df <- data.frame(
            no = c(r, r, r),
            x = c(xFrom, xTo, 1),
            y = c(yFrom, yTo, NA_real_))

          return(df)
        })
        dfLines <- do.call("rbind.fill", dfLines)

        # Done.
        rvOther$dfLines <- dfLines

      })

      # plotly_click
      observe({
        dfNodes <- rvOther$dfNodes
        validate(need(rvOther$dfNodes, message = FALSE))
        if (length(rvOther$dfNodes) == 0) { return (NULL) }
        if (nrow(rvOther$dfNodes) == 0) { return (NULL) }

        d <- event_data("plotly_click", "trust_chain")

        nodeId <- rvOther$dfNodes$id_node[
          which(rvOther$dfNodes$lvl == d$y &
                  rvOther$dfNodes$col == d$x)]
        if (length(nodeId) == 0) {
          rvOther$contentLeft <- "<br/><br/><p>Click on a node in the chain to see its details.</p>"
          return(invisible(NULL))
        }

        df <- rvOther$dfNodes[which(rvOther$dfNodes$id_node == nodeId),]

        rvOther$contentLeft <- paste(
          "<br/><br/>",
          "<p><b>Schema</b></p>" ,
          sprintf("<p>%s</p>", df$lbl_schema),
          "<p><b>Policy</b></p>",
          sprintf("<p>%s (v%s)</p>", df$name_policy, df$ver_policy),
          "<p><b>Date</b></p>",
          sprintf("<p>%s</p>", df$date_issuance),
          "<p><b>Issuer</b></p>",
          sprintf("<p>%s</p>", df$email_address),
          sprintf("<p><a href=%s>%s</a></p>", df$did_issuer, df$did_issuer_short),
          "<p><b>UUID</b></p>",
          sprintf("<p>%s</p>", df$uuid),
          "<p><b>Message ID</b></p>",
          sprintf("<p>%s</p>", df$id_message),
          "<p><b>IPFS URL</b></p>",
          sprintf("<p><a href=%s>%s</a></p>", df$url_ipfs, df$url_ipfs_short),
          sep = "", collapse = "")
      })

      # outputs ----------------------------------------------------------------

      output$htmloTitle <- renderPrint({
        x <- sprintf('<h3>Trust Chain: %s</h3>', rvOther$dfTkn$name)
        if (length(serialNos) > 0) {
          x <- sprintf("%sSerial numbers in this issuance: %s", x, serialNos)
        }
        return(HTML(x))
      })

      output$toErr <- renderText({
        outputMsgs$toErr
      })

      output$htmloTrustChainDoc <- renderPrint({
        return(HTML(rvOther$contentLeft))
      })

      output$plyoTrustChain <- renderPlotly({
        # load(sprintf("%sfig.Rda", tmpdir))
        # fig

        validate(need(rvOther$dfNodes, message = FALSE))
        validate(need(rvOther$dfLines, message = FALSE))

        fig <- plot_ly(
          source = "trust_chain",
          data = rvOther$dfLines,
          x = ~x,
          y = ~y,
          type = 'scatter',
          mode = 'lines',
          hoverinfo = 'none',
          showlegend = FALSE)

        fig <- fig %>% add_trace(
          data = rvOther$dfNodes,
          x = ~col,
          y = ~lvl,
          mode = "markers",
          color = ~name_policy,
          marker = list(size = 10),
          hoverinfo = 'text',
          text = ~paste('</br> ', lbl_schema,
                        '</br> Date: ', date_issuance,
                        '</br> Issuer: ', email_address))

        fig <- fig %>% add_trace(
          data = rvOther$dfNodes,
          x = ~col,
          y = ~lvl,
          type = 'scatter',
          mode = 'markers',
          hoverinfo = 'none',
          color = ~name_policy,
          showlegend = TRUE,
          marker = list(size = 30, opacity = 0.7, symbol = "square"))

        fig <- fig %>% add_trace(
          data = rvOther$dfNodes,
          x = ~col,
          y = ~lvl,
          type = 'scatter',
          mode = 'text',
          text = ~abbr_schema,
          textposition = 'middle middle',
          textfont = list(color = '#000000', size = 10),
          hoverinfo = 'none',
          showlegend = FALSE)

        fig <- fig %>% layout(
          # title = list(text="",
          #              y = 0.95, x = 0.5),
          title = NULL,
          xaxis = list(showgrid = FALSE,
                       showticklabels = FALSE,
                       title = "",
                       fixedrange = TRUE),
          yaxis = list(showgrid = FALSE,
                       showticklabels=FALSE,
                       title = "",
                       fixedrange = TRUE),
          legend = list(xanchor = "center", x = 0.5, y = -50))

        fig <- event_register(fig, 'plotly_click')

        # config(fig, displayModeBar = FALSE) # Did not work.
        fig %>% config(displayModeBar = FALSE)

      })

      rvOther$init <- Sys.time()

      return(rvToReturn)
    })
}
