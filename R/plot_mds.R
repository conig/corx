#' Plot correlations using multidimensional scaling
#'
#' Show variables as points in a two-dimensional map, with similar variables
#' placed near one another. Optionally group the points with k-means clustering.
#'
#' @param corx A [corx()] object with at least three variables, matching row
#'   and column names in the same order, and no missing coefficients.
#' @param k Number of clusters, as a positive integer no larger than the number
#'   of variables. The default, `NULL`, skips clustering. Use `"auto"` to count
#'   the principal components that each explain more than 5% of the variance
#'   in a PCA of the correlation matrix used for scaling.
#' @param abs Logical. If `TRUE` (the default), use absolute correlations so
#'   that strongly negative and strongly positive correlations both imply
#'   similarity. If `FALSE`, retain the signs.
#' @param ... Additional arguments passed to `ggpubr::ggscatter()`. Do not
#'   supply arguments already set by this function, such as `x`, `y`, or
#'   `label`.
#' @details This function requires the psych package. Correlations are
#'   converted to distances with `psych::cor2dist()`:
#'   \deqn{d = \sqrt{2(1-r)}}{d = sqrt(2 * (1 - r))}
#'   Here, \eqn{r} is the signed or absolute correlation, as specified by
#'   `abs`. Classical multidimensional scaling with `stats::cmdscale()`
#'   embeds the distances in two dimensions. The function warns if the squared
#'   correlation between the original distances and the distances in the map is
#'   below 0.70. When clustering is requested, `stats::kmeans()` uses the
#'   two-dimensional coordinates.
#' @returns A ggplot object.
#' @seealso [plot.corx()] for a plot of the correlation matrix itself.
#' @references
#' Carlson, D. L. (2017). *Quantitative Methods in Archaeology Using R*.
#' Cambridge University Press.
#' @examples
#' fit <- corx(mtcars[, 1:4])
#' if (requireNamespace("psych", quietly = TRUE)) plot_mds(fit)
#' @md
#' @export

plot_mds = function(corx, k = NULL, abs = TRUE, ...) {
  call = match.call()
  if(methods::is(corx, "corx")){
    corx <- stats::coef(corx)
  }else{
    stop("plot_mds can only be used with corx objects")
  }

  # check for misspecification ----

  if(length(colnames(corx)) != length(rownames(corx))) stop("Different number of rows and columns: only symmetrical correlation matrices will work with plot_mds", call. = FALSE)

  if(!all(colnames(corx) == rownames(corx))){
    stop("Row names not equal to colnames: only symmetrical correlation matrices will work with plot_mds",call. =FALSE)
  }

  if(length(colnames(corx)) < 3) stop("At least three variables are needed to perform MDS", call. = FALSE)

  # ----

  if(abs) corx <- abs(corx)
  distances =  psych::cor2dist(corx)
  cmd = stats::cmdscale(distances, k = 2, eig = TRUE)

  new_dists = dist(cmd$points, diag = TRUE, upper = TRUE)
  r2 = (stats::cor(new_dists, stats::as.dist(distances)) ^2) *100

  df1 = 1
  df2 = NROW(new_dists) - 2

  f_value = r2/ ((1 - r2)/df2)
  p_val = stats::pf(f_value, 1, df2, lower.tail = FALSE)

  dist = data.frame(cmd$points)
  colnames(dist) = c("x", "y")

 if(r2 < 70) warning("Two dimensions explains only ", round(r2,1),"% of variance. MDS might not be appropriate.")

  if(!is.null(k)){

    if(k == "auto"){ # if k = auto, figure out a good value
      pca = stats::princomp(corx)$sdev
      cumprop = pca^2 / sum(pca^2)
      k = as.numeric(length(cumprop[cumprop > .05]))
    } # ---------------------------------------------------

    if(! any(c("numeric","integer") %in% class(k))) { # check k is now a numeric
      stop("k must be a numeric", call. = F)
    } # ---------------------------------------------------

    if(k > nrow(dist)){ # throw error if k is larger that var pool
      stop("k = ",k,". You cannot have more clusters than there are variables (",nrow(dist),").",call. =F)
    } # -----------------------------------------------------------


  dist$group = factor(stats::kmeans(dist, k)$cluster)
  ellipse = TRUE
  }else{
    dist$group = 1
    ellipse = FALSE
  }

  p <- ggpubr::ggscatter(
    dist,
    x = "x",
    y = "y",
    label = rownames(dist),
    size = 2,
    repel = T,
    ellipse = ellipse,
    ellipse.type = "convex",
    color = "black",
    fill = "group",
    show.legend.text = F,
    ...
  ) + ggplot2::labs(x = "", y = "") +
    ggplot2::theme(legend.position = "none")
  p
}


