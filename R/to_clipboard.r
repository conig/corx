#' Copy a formatted correlation table to the clipboard
#'
#' Copy a table for pasting into a spreadsheet or another application.
#' This function requires an interactive R session and an available system
#' clipboard. In scripts, use [to_table()] to get the formatted table instead.
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
  if (!interactive()) {
    stop(
      "to_clipboard() only works in interactive R sessions. Use to_table() in scripts.",
      call. = FALSE
    )
  }
  if (inherits(x, "corx")) {
    x <- x$apa
  }
  clipr::write_clip(x, ...)
}
