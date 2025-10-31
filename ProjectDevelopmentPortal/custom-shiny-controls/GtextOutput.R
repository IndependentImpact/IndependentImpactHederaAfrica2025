
# TODO: Implement the 'container' and 'inline' parameters of Shiny's original textOutput().

GtextOutput <- function(id, title = NULL, subTxt = NULL, hL = 4, colWidth = 12) {
  HTML(
    sprintf(
'
<div class="row">
  <div class="col-sm-%d">
    <div class="well">
      %s
      %s
      <div id="%s" class="shiny-text-output"></div>
    </div>
  </div>
</div>
',
colWidth,
ifelse (length(title) > 0, 
       sprintf("<h%d>%s</h%d>", hL, title, hL), 
       ''),
ifelse (length(subTxt) > 0, sprintf('<span class="help-block">%s</span>', subTxt), ""),
id))
}

