#' Format a correlation table with papaja
#'
#' Method for `papaja::apa_table()` that uses the formatted table from a
#' [corx()] object. Pass the object as the first positional argument, for
#' example `papaja::apa_table(fit)`.
#'
#' @param corx A [corx()] object.
#' @param ... Additional arguments passed to `papaja::apa_table()`. Specify
#'   `caption` or `note` here to override the values stored in the object.
#'   The method supplies the table itself.
#' @returns The result of `papaja::apa_table()`, in the format selected by
#'   that function.
#' @seealso [to_table()].
#' @md

apa_table.corx <- function(corx, ...){

  args <- list(...)
  args$x <- corx$apa
  if(is.null(args$caption)) args$caption <- corx$caption
  if(is.null(args$note)) args$note <- corx$note

  do.call(papaja::apa_table, args)

}

.onLoad <- function(...) {
  s3_register("papaja::apa_table", "corx")
  invisible()
}

s3_register <- function (generic, class, method = NULL)
{ # exported from vctrs with thanks
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)
  pieces <- strsplit(generic, "::")[[1]]
  stopifnot(length(pieces) == 2)
  package <- pieces[[1]]
  generic <- pieces[[2]]
  caller <- parent.frame()
  get_method_env <- function() {
    top <- topenv(caller)
    if (isNamespace(top)) {
      asNamespace(environmentName(top))
    }
    else {
      caller
    }
  }
  get_method <- function(method, env) {
    if (is.null(method)) {
      get(paste0(generic, ".", class), envir = get_method_env())
    }
    else {
      method
    }
  }
  register <- function(...) {
    envir <- asNamespace(package)
    method_fn <- get_method(method)
    stopifnot(is.function(method_fn))
    if (exists(generic, envir)) {
      registerS3method(generic, class, method_fn, envir = envir)
    }
    else if (identical(Sys.getenv("NOT_CRAN"), "true")) {
      warning(sprintf("Can't find generic `%s` in package %s to register S3 method.",
                      generic, package))
    }
  }
  setHook(packageEvent(package, "onLoad"), register)
  if (isNamespaceLoaded(package)) {
    register()
  }
  invisible()
}
