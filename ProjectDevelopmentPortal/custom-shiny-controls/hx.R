
hx <- function(x, lvl) {
  HTML(sprintf("<h%d>%s</h%d>", lvl, x, lvl))
}