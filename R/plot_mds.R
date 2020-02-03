#' plot_mds
#'
#' plot the Classical multidimensional scaling of a corx object
#' @param corx the corx object, or a matrix of correlation coefficicents
#' @param k  a numeric, the number of clusters. If set to "auto" will be equal to the number of principal components that explain more than 5\% of total variance.
#' @param ... additional arguments passed to ggpubr::ggscatter
#' @export plot_mds

plot_mds = function(corx, k = NULL, ...) {
  call = match.call()
  if("corx" %in% class(corx)){
    corx <- abs(stats::coef(corx))
  }else{
    stop("Can only be used with corx objects")
  }

  cmd = stats::cmdscale(sqrt(1 - corx) * 2, eig = T)
  dist = data.frame(cmd$points)
  colnames(dist) = c("x", "y")

 # total_var =  sum(cmd$eig[1:2])/sum(cmd$eig) * 100
#  if(total_var < 70) warning("Two dimentions explains only ", round(total_var,1),"% of variance. MDS might not be appropriate.")

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


