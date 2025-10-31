
# TODO: Implement the 'capture' parameter of Shiny's original fileInput().

GfileInput <- function(id, title, 
                       helpTxt = NULL, 
                       hL = 4, 
                       colWidth = 12, 
                       inpWidth = DEFAULT_INP_WIDTH,
                       buttonLabel = "Browse",
                       accept = NULL, 
                       placeholder = "No file selected",
                       multiple = FALSE) {
  
  if (length(placeholder) == 1) {
    if (is.na(placeholder)) { placeholder <- NULL }
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
        <div class="input-group">
          <label class="input-group-btn input-group-prepend">
            <span class="btn btn-default btn-file">
              %s
              <input id="%s" name="%s" type="file" style="position: absolute !important; top: -99999px !important; left: -99999px !important;"%s%s/>
            </span>
          </label>
          <input type="text" class="form-control" %s readonly="readonly"/>
        </div>
        <div id="%s_progress" class="progress active shiny-file-input-progress">
          <div class="progress-bar"></div>
        </div>
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
buttonLabel,
id, id, .makeAcceptStr(accept), ifelse(multiple, ' multiple="multiple"', ''),
.makePlaceHldrStr(placeholder),
id,
id))
  
}

.makeAcceptStr <- function(accept) {
  
  if (length(accept) == 0) { return("") }
  
  return(sprintf(' accept="%s"',
          paste(accept, collapse = ",")))
}

.makePlaceHldrStr <- function(placeholder) {
  if (length(placeholder) == 0) { return("placeholder") }
  return(sprintf('placeholder="%s"', placeholder))
}


