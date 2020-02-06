#' PCAnet
#'
#' Convenience function for converting a qgraph object to a layout determined
#' by principal components analysis
#'
#' @param qgraph_net an object of type \code{qgraph}
#' @param cormat the correlation matrix of the relevant data. If this argument is missing,
#' the function will assume that the adjacency matrix from \code{qgraph_net} is a correlation matrix
#' @param varTxt logical. Print the variance accounted for by the PCA in the lower left corner of the plot
#' @param repulse logical. Add a small repulsion force with wordcloud package to avoid node overlap?
#' @param repulsion scalar for the repulsion force (if repulse=T). Larger values add more repulsion
#' @param principalArgs additional arguments in list format passed to \code{psych::principal}
#' @param ... additional arguments passed to \code{qgraph}
#'
#' @details
#'
#' A network plotted with PCA can be interpreted based on coordinate placement
#' of each node. A node in the top right corner scored high on both the first and second
#' principal components
#'
#' @references
#'
#' Jones, P. J., Mair, P., & McNally, R. J. (2018). Visualizing psychological networks: A tutorial in R. Frontiers in Psychology, 9, 1742. https://doi.org/10.3389/fpsyg.2018.01742
#'
#' @export
PCAnet <- function(qgraph_net, cormat, varTxt=F, repulse=F, repulsion=1,
                   principalArgs=list(),...) {
  if(missing(cormat)){
    op <- options("warn")
    on.exit(options(op))
    options(warn=1)
    warning("Correlation matrix not provided: using network adjacency matrix to perform PCA")
    cormat <- qgraph::getWmat(qgraph_net)
    diag(cormat) <- 1
  }
  principalArgs <- c(principalArgs, list(r = cormat, nfactors = 2))
  PCAfit <- R.utils::withTimeout({do.call(what=psych::principal,args=principalArgs)},
                                timeout=10,onTimeout="silent")
  if(is.null(PCAfit)){
    stop("Timeout: PCA could not be computed. Check correlation matrix.")
  }
  if(!repulse){
    qgraph::qgraph(qgraph_net, layout=PCAfit$loadings,...)
  } else {
    message("When repulsion is used, plot is not exact. Compare to unrepulsed graph before interpreting")
    repulse <- repulseLayout(qgraph_net, layout=PCAfit$loadings, repulsion=repulsion*.5)
    qgraph::qgraph(qgraph_net, layout=repulse,...)
  }
  if(varTxt){
    graphics::text(-1,-1, paste("% var=", round(sum(PCAfit$values[1:2]/length(PCAfit$values)),2)))
  }
  class(PCAfit$loadings) <- "matrix"
  invisible(PCAfit$loadings)
}


