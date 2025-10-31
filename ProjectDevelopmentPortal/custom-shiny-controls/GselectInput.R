GselectInput <- function(id, title, choices,
                         helpTxt = NULL, 
                         selected = NULL, 
                         multiple = FALSE, 
                         selectize = TRUE, 
                         size = NULL, 
                         hL = 4, 
                         colWidth = 12,
                         inpWidth = "500px") {
  HTML(
    sprintf(
      '
<div class="row">
  <div class="col-sm-%d">
    <div class="well">
      <h%d>%s</h%d>
      %s
      %s
      <div id="%s_msg" class="shiny-text-output"></div>
    </div>
  </div>
</div>
',
      colWidth,
      hL, title, hL,
      ifelse (length(helpTxt) > 0, sprintf('<span class="help-block">%s</span>', helpTxt), ""),
      ifelse(
        selectize,
        
        sprintf(
          '<div class="form-group shiny-input-container" style="width:%s;">
  <div>
    <select id="%s" class="shiny-input-select"%s>%s</select>
    <script type="application/json" data-for="%s"%s>{"plugins":["selectize-plugin-a11y"]}</script>
  </div>
</div>', 
          inpWidth, id,
          ifelse(multiple, ' multiple="multiple"', ""),
          .makeChoicesStr(chcs = choices, slctd = selected), 
          id, ifelse(!multiple, ' data-nonempty=""', "")),
        
        sprintf(
          '<div class="form-group shiny-input-container" style="width:%s;">
  <div>
    <select id="%s" class="form-control" size="%d"%s>%s</select>
  </div>
</div>', 
          inpWidth, id, size, 
          ifelse(multiple, ' multiple="multiple"', ""),
          .makeChoicesStr(chcs = choices, slctd = selected))),
      id))
}

.makeChoicesStr <- function(chcs, slctd = NULL) {
  
  if (length(names(chcs)) != length(chcs)) {
    names(chcs) <- chcs
  }
  
  slctnStr <- rep(x = "", times = length(chcs))
  if (length(slctd) == 0) { slctd <- chcs[1] }
  idx <- which(chcs == slctd)
  slctnStr[idx] <- " selected"
  
  return(paste(sprintf('<option value="%s"%s>%s</option>',
                       chcs, slctnStr, names(chcs)), 
               collapse = "\n"))
}    


# 
# res1 <- GselectInput(id = "type_techmeas",
#              title = "Type of technology or measure",
#              choices = c("FACILITY",
#                          "SYSTEM",
#                          "EQUIPMENT",
#                          "OTHER"),
#              helpTxt = "", # TODO.
#              selected = NULL,
#              multiple = FALSE,
#              hL = 5,
#              colWidth = 12,
#              inpWidth = DEFAULT_INP_WIDTH)



# tagList(
#   fluidRow(
#     column(
#       width=12,
#       wellPanel(
#         h5("Type of technology or measure"),
#         helpText(""),
#         selectInput(inputId = "type_techmeas",
#                     label = NULL,
#                     choices = c("FACILITY",
#                                 "SYSTEM",
#                                 "EQUIPMENT",
#                                 "OTHER"),
#                     selected = NULL,
#                     multiple = FALSE,
#                     selectize = TRUE,
#                     width = DEFAULT_INP_WIDTH,
#                     size = NULL),
#         textOutput(outputId = "type_techmeas_msg"))
#     )))
# 
# 
# res2 <- '
# <div class="row">
#   <div class="col-sm-12">
#     <div class="well">
#       <h5>Type of technology or measure</h5>
#       <span class="help-block"></span>
#       <div class="form-group shiny-input-container" style="width:100%;">
#         <div>
#           <select id="type_techmeas" class="shiny-input-select"><option value="FACILITY" selected>FACILITY</option>
# <option value="SYSTEM">SYSTEM</option>
# <option value="EQUIPMENT">EQUIPMENT</option>
# <option value="OTHER">OTHER</option></select>
#           <script type="application/json" data-for="type_techmeas" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
#         </div>
#       </div>
#       <div id="type_techmeas_msg" class="shiny-text-output"></div>
#     </div>
#   </div>
# </div>
# '
# res2 <- HTML(res2)
# 
# identical(res1, res2)
