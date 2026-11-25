#' Copy a formatted correlation table to the clipboard
#'
#' Copy a table for pasting into a spreadsheet or another application.
#' This function requires an available system clipboard.
#'
#' @param x A [corx()] object, matrix, or data frame. For a `corx` object, only
#'   the formatted table (`x$apa`) is copied, without the caption or note.
#' @param ... Additional arguments passed to `clipr::write_clip()`.
#' @returns Invisibly returns the value from `clipr::write_clip()` (the
#'   original content by default, or the formatted text if
#'   `return_new = TRUE`).
#' @seealso [to_table()] to extract a table without using the clipboard.
#' @md
#' @export

to_clipboard <- function(x, ...) {

  if("corx" %in% class(x)){
  clipr::write_clip(x$apa, ...)
  }else{
    clipr::write_clip(x, ...)
  }

}
