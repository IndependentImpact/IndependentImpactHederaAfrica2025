GdynamicMultiInput <- function(id, 
                               title, 
                               helpTxt = NULL, 
                               hL = 4, 
                               inpWidth = "800px",
                               colWidth = 12) {
  
  HTML(
    sprintf('
<div class="row">
  <div class="col-sm-%d">
    <div class="well">
      <h%d>%s</h%d>
      <span class="help-block">%s</span>
      <div id="%s-uioMain" class="shiny-html-output"></div>
      <button id="%s-abAdd" type="button" class="btn btn-default action-button">Add</button>
      <button id="%s-abOK" type="button" class="btn btn-default action-button">OK</button>
      <button id="%s-abCancel" type="button" class="btn btn-default action-button">Cancel</button>
      <div class="datatables html-widget html-widget-output shiny-report-size html-fill-item-overflow-hidden html-fill-item" id="%s-dtoItems" style="width:%s;height:auto;"></div>
      <br>
      <div id="%s_msg" class="shiny-text-output"></div>
    </div>
  </div>
</div>',
            colWidth,
            hL, title, hL,
            helpTxt,
            id,
            id,
            id,
            id,
            id, inpWidth,
            id))
}




