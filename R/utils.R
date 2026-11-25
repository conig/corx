select_columns <- function(data, expr, default, allow_empty = FALSE) {
  if (rlang::quo_is_null(expr)) return(default)
  if (rlang::quo_is_symbol(expr)) {
    name <- rlang::as_name(expr)
    env <- rlang::quo_get_env(expr)
    if (!name %in% names(data) && exists(name, envir = env, inherits = TRUE)) {
      # Read even an active binding only once; leave non-NULL selection to tidyselect.
      cache <- new.env(parent = env)
      cache[[name]] <- get(name, envir = env, inherits = TRUE)
      if (is.null(cache[[name]])) return(default)
      expr <- rlang::quo_set_env(expr, cache)
    }
  }
  tidyselect::eval_select(expr, data, allow_empty = allow_empty)
}

# Select from the names of the built-in descriptive statistics.
eval_select_names <- function(vars, expr, strict = TRUE) {
  choices <- stats::setNames(seq_along(vars), vars)
  loc <- tidyselect::eval_select(rlang::enquo(expr), choices, strict = strict)
  stats::setNames(vars[loc], names(loc))
}
