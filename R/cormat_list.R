#' cormat_list
#' @param data data.frame
#' @param x named column positions for the row variables
#' @param y named column positions for the column variables
#' @param z column positions of the control variables, or NULL
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
  # Compute and adjust once per source pair, even if it has multiple labels.
  rows <- x[!duplicated(x)]
  cols <- y[!duplicated(y)]
  template <- matrix(NA_real_, length(rows), length(cols),
                     dimnames = list(names(rows), names(cols)))
  cors <- list(r = template, n = template, p = template)
  if (is.null(z) || p_adjust != "none") {
    pairs <- shared_pair_cells(rows, cols)
  }
  reuse <- is.null(z) && length(pairs$mirror) > 0L
  if (reuse) {
    compute <- matrix(TRUE, length(rows), length(cols))
    compute[pairs$mirror] <- FALSE
  }

  for (i in seq_along(rows)) {
    js <- if (reuse) which(compute[i, ]) else seq_along(cols)
    for (j in js) {
      pair <- flex_cor(rows[[i]], cols[[j]], z, method, data)
      cors$r[i, j] <- pair$r
      cors$n[i, j] <- pair$n
      cors$p[i, j] <- pair$p
    }
  }

  if (reuse) {
    cors <- lapply(cors, function(m) {
      m[pairs$mirror] <- m[pairs$first]
      m
    })
  }

  if (p_adjust != "none") {
    cors$p <- adjust_pmat(cors$p, p_adjust, pairs)
  }

  if (length(rows) != length(x) || length(cols) != length(y)) {
    i <- match(x, rows)
    j <- match(y, cols)
    cors <- lapply(cors, function(m) {
      m <- m[i, j, drop = FALSE]
      dimnames(m) <- list(names(x), names(y))
      m
    })
  }
  cors
}

flex_cor <- function(x, y, z = NULL, method, data) {
  if (is.null(z)) {
    a <- data[[x]]
    b <- data[[y]]
    ok <- stats::complete.cases(a, b)
    n <- sum(ok)
    if (n < 2L) return(list(r = NA_real_, n = n, p = NA_real_))
    if (method == "pearson") {
      if (n != length(a)) {
        a <- a[ok]
        b <- b[ok]
      }
      r <- stats::cor(a, b)
      # Same two-sided t test as cor.test(), without building an htest per pair.
      df <- n - 2L
      p <- NA_real_
      if (df > 0L) {
        t <- sqrt(df) * r / sqrt(1 - r^2)
        p <- 2 * stats::pt(-abs(t), df)
      }
      return(list(r = r, n = n, p = p))
    }
    if (n == 2L && method == "spearman") {
      r <- stats::cor(a, b, use = "complete.obs", method = method)
      return(list(r = r, n = n, p = NA_real_))
    }

    test <- stats::cor.test(a, b, method = method, exact = FALSE)
    return(list(r = test$estimate, n = n, p = test$p.value))
  }

  partial_data <- stats::na.omit(data[, c(x, y, z), drop = FALSE])
  n <- nrow(partial_data)
  # ppcor's t test needs positive residual df; its Kendall test needs two
  # observations after subtracting the number of controls.
  min_n <- length(z) + if (method == "kendall") 2L else 3L
  if (n < min_n) return(list(r = NA_real_, n = n, p = NA_real_))

  if (x != y) {
    cor_ob.partial <-
      ppcor::pcor.test(
        x = partial_data[[1L]],
        y = partial_data[[2L]],
        z = partial_data[, -(1:2), drop = FALSE],
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
