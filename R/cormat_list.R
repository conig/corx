#' cormat_list
#' @param data data.frame
#' @param x character vector, row names
#' @param y character vector, column names
#' @param z character vector, partial variable names
#' @param method string, passed to cor.test
#' @param p_adjust string, passed to p.adjust

cormat_list <- function(data, x, y, z, method, p_adjust) {
  cors <- list()

  cormat <- matrix(nrow = length(x), ncol = length(y))
  rownames(cormat) <- x
  colnames(cormat) <- y

  cors$r <- cormat
  cors$n <- cormat
  cors$p <- cormat

  for (r in x) {
    for (c in y) {
      cor_ob <-
        flex_cor(
          x = r,
          y = c,
          z = z,
          method = method,
          data = data
        )

      cors$r[r, c] <- cor_ob$r
      cors$n[r, c] <- cor_ob$n
      cors$p[r, c] <- cor_ob$p

    }
  }

  if (p_adjust != "none") {
    cors$p <- adjust_pmat(cors$p, p_adjust)
  }

  cors

}

flex_cor <- function(x, y, z = NULL, method, data) {
  if (is.null(z)) {
    cor_ob <-
      stats::cor.test(
        x = data[, x],
        y = data[, y],
        method = method,
        exact = FALSE
      )
    return(list(
      r = cor_ob$estimate,
      n = as.numeric(psych::pairwiseCount(data[, x], data[, y])),
      p = cor_ob$p.value
    ))
  }

  partial_data <- stats::na.omit(data[, c(x, y, z)])

  if (x != y) {
    cor_ob.partial <-
      ppcor::pcor.test(
        x = partial_data[, x],
        y = partial_data[, y],
        z = partial_data[, z],
        method = method
      )

    list(r = cor_ob.partial$estimate,
         n = cor_ob.partial$n,
         p = cor_ob.partial$p.value)
  } else{
    list(r = 1,
         n = nrow(partial_data),
         p = 1)
  }

}
