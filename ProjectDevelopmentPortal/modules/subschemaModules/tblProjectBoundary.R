
tblProjectBoundaryUI <- function(id,
                                 hL = 4,
                                 colWidth = 12,
                                 inpWidth = DEFAULT_INP_WIDTH) {
  
  ns <- NS(id)
  
  tagList(
    
    #useShinyjs(),
    
    fluidRow(
      column(
        width = colWidth,
        wellPanel(
          hx(x = "Baseline Sources", lvl = hL),
          helpText("What are the baseline sources?"),
          dynamicMultiInput(id = ns("baseline_sources"))))),
    
    fluidRow(
      column(
        width = colWidth,
        wellPanel(
          hx(x = "Project Sources", lvl = hL),
          helpText("What are the project sources?"),
          dynamicMultiInput(id = ns("project_sources"))))))
}

tblProjectBoundaryServer <- function(id,
                                     hL = 4,
                                     colWidth = 12,
                                     inpWidth = DEFAULT_INP_WIDTH, 
                                     lsPreset = NULL) {
  
  moduleServer(
    id = id,
    function(input, output, session) {
      
      ns <- session$ns
      
      rvToReturn <- reactiveValues()
      rvToReturn$allResultsGood <- FALSE
      rvToReturn$results <- reactiveValues(
        baseline_sources = NULL,
        project_sources = NULL)
      
      resultsGood <- reactiveValues()
      resultsGood$baseline_sources <- FALSE
      resultsGood$project_sources <- FALSE
      
      # preset inputs ----------------------------------------------------------
      # None.
      
      # module servers ---------------------------------------------------------
      
      modSrvrs <- reactiveValues()

      modSrvrs$baseline_sources <- dynamicMultiServer(
        id = "baseline_sources", 
        nmUImod = "tblRowProjBndryUI", 
        nmSrvrMod = "tblRowProjBndryServer", 
        lsArgsModUI = list(hL = hL, colWidth = colWidth, inpWidth = inpWidth), 
        lsArgsModSrvr = list(lsPreset = lsPreset$baseline_sources))
      
      modSrvrs$project_sources <- dynamicMultiServer(
        id = "project_sources", 
        nmUImod = "tblRowProjBndryUI", 
        nmSrvrMod = "tblRowProjBndryServer", 
        lsArgsModUI = list(hL = hL, colWidth = colWidth, inpWidth = inpWidth), 
        lsArgsModSrvr = list(lsPreset = lsPreset$project_sources))
      
      # inputs -----------------------------------------------------------------
      
      observe({
        resultsGood$baseline_sources <- FALSE
        rvToReturn$results$baseline_sources <- modSrvrs$baseline_sources$items
        resultsGood$baseline_sources <- length(rvToReturn$results$baseline_sources) > 0
      })

      observe({
        resultsGood$project_sources <- FALSE
        rvToReturn$results$project_sources <- modSrvrs$project_sources$items
        resultsGood$project_sources <- length(rvToReturn$results$project_sources) > 0
      })
            
      # outputs ----------------------------------------------------------------
      # TODO.
      
      # return logic -----------------------------------------------------------
      
      observe({
        rvToReturn$allResultsGood <- all(unlist(reactiveValuesToList(resultsGood)))
      })
      
      return(rvToReturn)
      
    })
}
