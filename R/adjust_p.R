# Only variables on both axes can form self-comparisons or reversed pairs.
# Align that square block by name, keeping its positions in the original matrix.
shared_pair_cells <- function(x, y) {
  shared <- intersect(x, y)
  rows <- match(shared, x)
  offsets <- (match(shared, y) - 1) * length(x)
  block <- outer(rows, offsets, "+")  # R matrices are stored column by column
  upper <- upper.tri(block)
  list(self = diag(block), first = block[upper], mirror = t(block)[upper])
}

#' adjust_p
#' @param pmat named matrix of p-values to adjust
#' @param p_adjust character describing adjustment to make. See stats::p.adjust
#' @param pairs positions of self-comparisons and reversed pairs in pmat
adjust_pmat <- function(pmat, p_adjust,
                        pairs = shared_pair_cells(rownames(pmat), colnames(pmat))) {
  omit <- c(pairs$self, pairs$mirror)
  if (!length(omit)) {
    pmat[] <- stats::p.adjust(pmat, method = p_adjust)
  } else {
    pmat[-omit] <- stats::p.adjust(pmat[-omit], method = p_adjust)
    pmat[pairs$mirror] <- pmat[pairs$first]
  }
  pmat
}
