GtextInput <- function(id, title, 
                       helpTxt = NULL, 
                       value = "", 
                       hL = 4, 
                       colWidth = 12, 
                       inpWidth = DEFAULT_INP_WIDTH,
                       wellP = TRUE) {
  
  if (length(value) == 1) {
    if (is.na(value)) { value <- NULL }
  }
  
  HTML(
    sprintf(
      '
<div class="row">
  <div class="col-sm-%d">
    %s
      <h%d>%s</h%d>
      %s
      <div class="form-group shiny-input-container" style="width:%s;">
        <input id="%s" type="text" class="form-control"%s/>
      </div>
      <div id="%s_msg" class="shiny-text-output"></div>
    %s
  </div>
</div>
',
      colWidth,
      ifelse (wellP, '<div class="well">', ''),
      hL, title, hL,
      ifelse (length(helpTxt) > 0, sprintf('<span class="help-block">%s</span>', helpTxt), ""),
      inpWidth,
      id, ifelse(length(value) > 0, sprintf(' value="%s"', value), ''),
      id,
      ifelse (wellP, '</div>', '')))
}



