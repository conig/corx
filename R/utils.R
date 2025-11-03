# replacement for tisyselect::vars_select
eval_select_names <- function(
  vars,
  expr,
  strict = TRUE,
  allow_predicates = TRUE
) {
  quo <- rlang::enquo(expr)

  if (rlang::quo_is_null(quo)) {
    return(character())
  }

  data_mask <- stats::setNames(seq_along(vars), vars)

  eval_call <- function() {
    tidyselect::eval_select(
      quo,
      data = data_mask,
      strict = strict,
      allow_predicates = allow_predicates
    )
  }

  locations <- if (allow_predicates) {
    eval_call()
  } else {
    suppressWarnings(eval_call())
  }

  stats::setNames(vars[locations], names(locations))
}
