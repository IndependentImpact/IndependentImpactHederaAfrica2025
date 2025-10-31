
locationInput <- function(id, 
                          lsPreset = NULL,
                          title = "Shapefile of location",
                          helpTxt = "Formats accepted: .shp, .shx, .dbf, .kml, .cpg, .zip", 
                          hL = 5,
                          colWidth = 12, 
                          inpWidth = DEFAULT_INP_WIDTH) {
  
  ns <- NS(id)
  
  tagList(
    
    #useShinyjs(),
    
    fluidRow(
      column(
        width = 6,
        shiny::br(),
        GfileInput(id = ns("location"), 
                   title = title, 
                   helpTxt = helpTxt, 
                   hL = hL, 
                   colWidth = colWidth, 
                   inpWidth = inpWidth, 
                   accept = c(".shp", ".shx", ".dbf", ".kml", ".cpg", ".zip"), 
                   placeholder = lsPreset$location$datapath, 
                   multiple = TRUE),
        wellPanel(
          textOutput(outputId = ns("toClickContinue")),
          shiny::br(),
          actionButton(inputId = ns("abUpload"), label = "Continue"),
          shiny::br(),
          shiny::br(),
          textOutput(outputId = ns("toUploadResult")))),
      column(
        width = 6,
        h4("Shapefile Preview"),
        plotOutput(outputId = ns("poLocation"))))
    
    # TODO: Add functionality to open Google Maps and choose a location there.
    )
}

locationServer <- function(id, 
                           lsPreset = NULL,
                           uploadToIPFS = FALSE) {
  
  maxFileSize <- 15*1024^2
  options(shiny.maxRequestSize=maxFileSize)
  
  moduleServer(
    id = id, 
    function(input, output, session) {
      
      ns <- session$ns
      
      rvToReturn <- reactiveValues()
      rvToReturn$goToModule <- NULL
      rvToReturn$allResultsGood <- FALSE
      rvToReturn$results <- reactiveValues(
        location = NULL)
      
      resultsGood <- reactiveValues()
      resultsGood$location <- FALSE
      
      outputMsgs <- reactiveValues()
      outputMsgs$location <- ""
      outputMsgs$toClickContinue <- ""
      outputMsgs$toUploadResult <- ""
      
      rvOther <- reactiveValues()
      rvOther$dfFiles <- NULL
      
      # preset inputs ----------------------------------------------------------
      
      if ("location" %in% names(lsPreset)) {
        restoreInput(id = "location", default = lsPreset$location$datapath)
        # TODO. This does not work. Fix it.
      }
      
      # inputs -----------------------------------------------------------------
      
      observeEvent(input$location, handlerExpr = {
        
        outputMsgs$toClickContinue <- ""
        outputMsgs$toUploadResult <- ""
        
        validate(need(input$location, message = FALSE))
        if (length(input$location) == 0) { return(invisible(NULL)) }
        
        resultsGood$location <- FALSE
        rvToReturn$results$location <- NULL
        rvOther$dfFiles <- NULL
        outputMsgs$location <- ""
        
        if (length(input$location) == 0) {
          rvToReturn$results$location <- NULL
          resultsGood$location <- FALSE
          return(invisible(0))
        }
        
        if (nrow(input$location) == 0) {
          rvToReturn$results$location <- input$location
          resultsGood$location <- FALSE
          return(invisible(0))
        }
        
        # Rename the temporary files according to shapefile protocol.
        {
          dfFiles <- input$location
          newPths <- sprintf("%s/%s",
                             dirname(dfFiles$datapath), 
                             dfFiles$name)
          file.rename(from = dfFiles$datapath, to = newPths)
          dfFiles$datapath <- newPths
        }
        
        # Extract the file extensions.
        lsEls <- strsplit(x = dfFiles$name, split = ".", fixed = TRUE)
        dfFiles$ext <- sapply(X = lsEls, FUN = function(x) x[length(x)])
        dfFiles$ext <- sprintf(".%s", dfFiles$ext)
        
        # Assign the files to rvOther so that the plotting function can start
        # working.
        rvOther$dfFiles <- dfFiles

        # Tell user what to do next.
        outputMsgs$toClickContinue <- "Please check the shapefile preview. If you are sure this is the correct file(s), click 'Continue' below."
        
      })
      
      observeEvent(input$abUpload, handlerExpr = {
        
        validate(need(rvOther$dfFiles, message = FALSE))
        if (length(rvOther$dfFiles) == 0) { return(invisible(NULL)) }
        
        # If we don't have to upload the files to IPFS first, return dfFiles
        # as is.
        if (!uploadToIPFS) {
          rvToReturn$results$location <- rvOther$dfFiles
          resultsGood$location <- TRUE
          outputMsgs$toUploadResult <- "Done."
          return(invisible(0))
        }
        
        # Zip the location files and upload them to IPFS.
        withProgress(expr = {
          tryCatch({
            ipfsURI <- uploadToIpfs(df = rvOther$dfFiles, encrypt = FALSE, zip = TRUE)
            rvToReturn$results$location <- ipfsURI
            resultsGood$location <- TRUE
            outputMsgs$toUploadResult <- sprintf("Done. IPFS URL: %s", ipfsURI)
          }, error = function(e) {
            message(sprintf("Failed to upload the file to IPFS. Error: %s", e))
            return(invisible(0))
          })
        }, min = 0, max = 1, 0, message = "Uploading file...")
        
      })
      
      # outputs ----------------------------------------------------------------
      
      output$poLocation <- renderPlot({

        validate(need(rvOther$dfFiles, message = FALSE))
        if (length(rvOther$dfFiles) == 0) { return(invisible(NULL)) }
        
        dfFiles <- rvOther$dfFiles
        if (nrow(dfFiles) == 0) { return(NULL) }
        
        lsEls <- strsplit(x = dfFiles$name, split = ".", fixed = TRUE)
        dfFiles$nm_loc <- sapply(X = lsEls, FUN = function(x) {
          paste(x[-length(x)], collapse = "")
        })

        # Plot .shp or .kml files.
        idx <- which(dfFiles$ext %in% c(".shp", ".kml"))
        if (length(idx) == 0) { 
          return(NULL) }
        if (length(idx) > 1) {
          # Plot only the first one.
          idx <- idx[1]
        }
        
        x <- sf::st_read(dfFiles$datapath[idx])
        
        return(plot(x$geometry, 
                    main = dfFiles$nm_loc[idx]))
        
      })
      
      output$location_msg <- renderText({
        
        msg <- outputMsgs$location
        if (nchar(msg) == 0) {
          msg <- sprintf("Maximum file size: %s MB", 
                         maxFileSize/(1024^2))
        }
        
        return(msg)
      })
      
      output$toClickContinue <- renderText({
        outputMsgs$toClickContinue
      })
      
      output$toUploadResult <- renderText({
        outputMsgs$toUploadResult
      })
      
      # return logic -----------------------------------------------------------
      
      observe({
        rvToReturn$allResultsGood <- all(unlist(reactiveValuesToList(resultsGood)))
      })
      
      return(rvToReturn)
      
    })
}
