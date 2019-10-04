#' to_clipboard
#'
#' Provides a way to send correlation tables to the clipboard so they can be pasted into excel.
#' @param x the corx object
#' @param ... aditional arguments passed to clipr::write_clip
#' @export to_clipboard

to_clipboard <- function(x, ...) {
  clipr::write_clip(x$apa, ...)
}
