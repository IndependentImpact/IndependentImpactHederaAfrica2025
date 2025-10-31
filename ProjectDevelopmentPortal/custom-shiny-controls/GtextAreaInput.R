
GtextAreaInput <- function(id, 
                           title, 
                           helpTxt = NULL, 
                           value = "", 
                           hL = 4, 
                           colWidth = 12, 
                           inpWidth = DEFAULT_INP_WIDTH,
                           resize = "vertical") {
  
  if (length(value) == 0) {
    value <- ""
  }
  
  HTML(
    sprintf(  
      '
<div class="row">
  <div class="col-sm-%d">
    <div class="well">
      <h%d>%s</h%d>
      %s
      <div class="form-group shiny-input-container" style="width:%s;">
        <textarea id="%s" class="form-control" style="resize:%s;">%s</textarea>
      </div>
      <div id="%s_msg" class="shiny-text-output"></div>
    </div>
  </div>
</div>
',
      colWidth,
      hL, title, hL,
      ifelse (length(helpTxt) > 0, sprintf('<span class="help-block">%s</span>', helpTxt), ""),
      inpWidth,
      id, resize, value,
      id))
  
}
