
# UI
tabSchemasUI <- function(id) {

  ns <- NS(id)

  tagList(
    DT::dataTableOutput(outputId = ns("dtoSchemas")),
    DT::dataTableOutput(outputId = ns("dtoSchemaVersions"))
  )
}

# Server
tabSchemasServer <- function(id, loginInfoUsr) {

  moduleServer(
    id = id,
    function(input, output, session) {
    })
}