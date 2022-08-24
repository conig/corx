#' adjust_p
#' @param pmat matrix of p-values to adjust
#' @param p_adjust character describing adjustment to make. See stats::p.adjust

adjust_pmat <- function(pmat, p_adjust){
  if(isSymmetric(pmat)){
    ps <- pmat[upper.tri(pmat, diag = FALSE)]
    ps_adj <- stats::p.adjust(ps, method = p_adjust)

    pmat[upper.tri(pmat, diag = FALSE)] <- ps_adj

    pmat[lower.tri(pmat)] <-
      t(pmat)[lower.tri(pmat, diag = FALSE)]
     if(!isSymmetric(pmat)) stop("p-matrix no longer symmetric due to error with p.adjust")
  }else{
    if(any(rownames(pmat) %in% colnames(pmat))) warning("At least one row variable was also specified as a column variable in a non-symmetrical matrix and p_adjust != 'none'. P-values have been adjusted assuming that ALL pairs (n = ", length(pmat),") in the matrix are unique and valid.")
        pmat[] <- stats::p.adjust(pmat, method = p_adjust)
      }
pmat
}

