#' to_clipboard
#'
#' Sends a formatted corx table to the clipboard so that it can be pasted into excel.
#' @param x a corx object, matrix, or data.frame
#' @param ... additional arguments passed to clipr::write_clip
#' @export to_clipboard

to_clipboard <- function(x, ...) {

  if("corx" %in% class(x)){
  clipr::write_clip(x$apa, ...)
  }else{
    clipr::write_clip(x, ...)
  }

}
