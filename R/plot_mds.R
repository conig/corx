#' plot_mds
#'
#' plot the Classical multidimensional scaling of a corx object
#' @param corx the corx object, or a matrix of correlation coefficicents
#' @param k the number of clusters.
#' @param ... additional arguments passed to ggpubr::ggscatter
#' @export plot_mds

plot_mds = function(corx, k = NULL, ...) {
  call = match.call()
  if("corx" %in% class(corx)) corx <- stats::coef(corx)

  dist = data.frame(stats::cmdscale(stats::dist(corx)))
  colnames(dist) = c("x", "y")

  if(!is.null(k)){

    if(k > nrow(dist)){
      stop("k = ",k,". You cannot have more clusters than there are variables (",nrow(dist),").",call. =F)
    }

  dist$group = factor(stats::kmeans(dist, k)$cluster)
  ellipse = TRUE
  }else{
    dist$group = 1
    ellipse = FALSE
  }

  ggpubr::ggscatter(
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
}


