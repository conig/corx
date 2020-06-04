#' plot_mds
#'
#' plot the Classical multidimensional scaling of a corx object
#' @param corx the corx object, or a matrix of correlation coefficients
#' @param k  a numeric, the number of clusters. If set to "auto" will be equal to the number of principal components that explain more than 5\% of total variance.
#' @param abs if true (the default) negative correlations will be turned positive. This means items with high negative correlations will be treated as highly similar.
#' @param ... additional arguments passed to ggpubr::ggscatter
#' @details plot_mds performs classic multidimensional scaling on a correlation matrix. The correlation matrix is first converted to a distance matrix using psych::cor2dist.
#' This function employs the following formula:
#' \deqn{dist = \sqrt(2*(1-r))}
#' These distances are then passed to stats::cmdscale where k = 2. To compute \eqn{latex}{R^2}, distances are predict from the cmdscale output and correlated with input distances. This correlation is squared.
#'  If the value of \eqn{R^2} is less than 70%, a warning will inform users that two-dimensions may not be sufficient to represent item relationships.
#'  The position of variables is then plotted with ggplot2. Clusters of items are identified using stats::kmeans. The number of clusters is determined using principal component analysis unless specified.
#'
#' @references
#' Carlson, D.L., 2017. Quantitative methods in archaeology using R. Cambridge University Press.
#'
#' @export plot_mds

plot_mds = function(corx, k = NULL, abs = TRUE, ...) {
  call = match.call()
  if("corx" %in% class(corx)){
    corx <- stats::coef(corx)
  }else{
    stop("Can only be used with corx objects")
  }

  if(abs) corx <- abs(corx)
  distances =  psych::cor2dist(corx)
  cmd = stats::cmdscale(distances, k = 2, eig = TRUE)
  # - test model appropriateness

  new_dists = dist(cmd$points, diag = TRUE, upper = TRUE)
  r2 = (stats::cor(new_dists, stats::as.dist(distances)) ^2) *100

  df1 = 1
  df2 = NROW(new_dists) - 2

  f_value = r2/ ((1 - r2)/df2)
  p_val = stats::pf(f_value, 1, df2, lower.tail = FALSE)

  dist = data.frame(cmd$points)
  colnames(dist) = c("x", "y")

  #total_var =  sum(cmd$eig[1:2])/sum(cmd$eig) * 100
  #if(r2 < 70) warning("Two dimensions explains only ", round(r2,1),"% of variance. MDS might not be appropriate.")

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
    font.family = "serif",
    ...
  ) + ggplot2::labs(x = "", y = "") +
    ggplot2::theme(legend.position = "none",
                   text = ggplot2::element_text(family = "serif"))
}


