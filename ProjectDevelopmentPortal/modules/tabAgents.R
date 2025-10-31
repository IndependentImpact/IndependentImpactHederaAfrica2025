
# UI
tabAgentsUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    DT::dataTableOutput(outputId = ns("dtoAgents"))
  )
}

# Server
tabAgentsServer <- function(id, loginInfoUsr) {
  
  moduleServer(
    id = id,
    function(input, output, session) {
    })
}