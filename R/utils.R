select_columns <- function(data, expr, default, allow_empty = FALSE) {
  if (rlang::quo_is_null(expr)) return(default)
  tidyselect::eval_select(expr, data, allow_empty = allow_empty)
}

# Select from the names of the built-in descriptive statistics.
eval_select_names <- function(vars, expr, strict = TRUE) {
  choices <- stats::setNames(seq_along(vars), vars)
  loc <- tidyselect::eval_select(rlang::enquo(expr), choices, strict = strict)
  stats::setNames(vars[loc], names(loc))
}
