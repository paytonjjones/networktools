#' PCAnet
#'
#' Convenience function for converting a qgraph object to a layout determined
#' by principal components analysis
#'
#' @param qgraph_net an object of type \code{qgraph}
#' @param cormat the correlation matrix of the relevant data. If this argument is missing,
#' the function will assume that the adjacency matrix from \code{qgraph_net} is a correlation matrix
#' @param varTxt logical. Print the variance accounted for by the PCA in the lower left corner of the plot
#' @param ... additional arguments passed to \code{psych::principal}
#'
#' @details
#'
#' A network plotted with PCA can be interpreted based on coordinate placement
#' of each node. A node in the top right corner scored high on both the first and second 
#' principal components
#'
#' @references
#'
#' Jones, P. J., Mair, P., & McNally, R. J. (2017). Scaling networks for two-dimensional visualization: a tutorial. Retrieved from osf.io/eugsz
#'
#' @export
PCAnet <- function(qgraph_net, cormat, varTxt=T,...) {
  if(missing(cormat)){
    op <- options("warn")
    on.exit(options(op))
    options(warn=1)
    warning("Correlation matrix not provided: using network adjacency matrix to perform PCA")
    cormat <- qgraph::getWmat(qgraph_net)
  }
  PCAfit <- psych::principal(cormat, nfactors = 2)
  qgraph::qgraph(qgraph_net, layout=PCAfit$loadings)
  if(varTxt){
    graphics::text(-1,-1, paste("% var=", round(sum(PCAfit$values[1:2]/length(PCAfit$values)),2)))
  }
}


