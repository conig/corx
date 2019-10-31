#' plot_mds
#'
#' plot the Classical multidimensional scaling of a corx object
#' @param corx the corx object, or a matrix of correlation coefficicents
#' @param k the number of clusters. If not provided, a prinipal components analysis is performed. k is set to the number of components which explain more than 5\% of variance
#' @export plot_mds

plot_mds = function(corx, k) {
  call = match.call()
  if("corx" %in% class(corx)) corx <- stats::coef(corx)

  if(is.null(call$k)){
    pca = stats::princomp(corx)$sdev
    cumprop = pca^2 / sum(pca^2)
  k = length(cumprop[cumprop > .05])
  }

  dist = data.frame(stats::cmdscale(stats::dist(corx)))
  colnames(dist) = c("x", "y")
  dist$group = factor(stats::kmeans(dist, k)$cluster)

  ggpubr::ggscatter(
    dist,
    x = "x",
    y = "y",
    label = rownames(dist),
    size = 2,
    repel = T,
    ellipse = TRUE,
    ellipse.type = "convex",
    color = "black",
    fill = "group",
    show.legend.text = F
  ) + ggplot2::labs(x = "", y = "") +
    ggplot2::theme(legend.position = "none")
}


