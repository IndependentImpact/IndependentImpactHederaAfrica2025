
# UI
tabWorkflowsUI <- function(id) {

  ns <- NS(id)

  tagList(
    DT::dataTableOutput(outputId = ns("dtoWorkflows")),
    DT::dataTableOutput(outputId = ns("dtoWorkflowVersions"))
  )
}

# Server
tabWorkflowsServer <- function(id, loginInfoUsr) {

  moduleServer(
    id = id,
    function(input, output, session) {
    })
}