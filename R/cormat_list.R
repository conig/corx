#' cormat_list
#' @param data data.frame
#' @param x character vector, row names
#' @param y character vector, column names
#' @param z character vector, partial variable names
#' @param method string, passed to cor.test
#' @param p_adjust string, passed to p.adjust

cormat_list <- function(
  data,
  x,
  y,
  z,
  method,
  p_adjust
) {
  template <- matrix(NA_real_, length(x), length(y), dimnames = list(x, y))
  cors <- list(r = template, n = template, p = template)
  symmetric <- is.null(z) && identical(x, y)
  method <- match.arg(method, c("pearson", "kendall", "spearman"))

  for (i in seq_along(x)) {
    cols <- if (symmetric) i:length(y) else seq_along(y)
    for (j in cols) {
      pair <- flex_cor(x[i], y[j], z, method, data)
      cors$r[i, j] <- pair$r
      cors$n[i, j] <- pair$n
      cors$p[i, j] <- pair$p
    }
  }

  if (symmetric) {
    lower <- lower.tri(template)
    cors <- lapply(cors, function(m) {
      m[lower] <- t(m)[lower]
      m
    })
  }

  if (p_adjust != "none") {
    cors$p <- adjust_pmat(cors$p, p_adjust)
  }

  cors
}

flex_cor <- function(x, y, z = NULL, method, data) {
  if (is.null(z)) {
    a <- data[[x]]
    b <- data[[y]]
    n <- sum(stats::complete.cases(a, b))
    if (n < 2L) return(list(r = NA_real_, n = n, p = NA_real_))
    if (n == 2L && method != "kendall") {
      # The coefficient exists, but the t test has zero degrees of freedom.
      r <- stats::cor(a, b, use = "complete.obs", method = method)
      return(list(r = r, n = n, p = NA_real_))
    }

    test <- stats::cor.test(a, b, method = method, exact = FALSE)
    return(list(r = test$estimate, n = n, p = test$p.value))
  }

  partial_data <- stats::na.omit(data[, unique(c(x, y, z)), drop = FALSE])
  n <- nrow(partial_data)
  # ppcor's t test needs positive residual df; its Kendall test needs two
  # observations after subtracting the number of controls.
  min_n <- length(z) + if (method == "kendall") 2L else 3L
  if (n < min_n) return(list(r = NA_real_, n = n, p = NA_real_))

  if (x != y) {
    cor_ob.partial <-
      ppcor::pcor.test(
        x = partial_data[[x]],
        y = partial_data[[y]],
        z = partial_data[, z, drop = FALSE],
        method = method
      )

    list(
      r = cor_ob.partial$estimate,
      n = cor_ob.partial$n,
      p = cor_ob.partial$p.value
    )
  } else {
    list(
      r = 1,
      n = nrow(partial_data),
      p = 1
    )
  }
}
