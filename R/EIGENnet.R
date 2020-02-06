#' EIGENnet
#'
#' Convenience function for converting a qgraph object to an eigenmodel layout
#'
#' @param qgraph_net an object of type \code{qgraph}
#' @param EIGENadj to use a base matrix for the eigenmodel other than the adjacency matrix
#' stored in \code{qgraph_net}, provide it in this argument
#' @param S number of samples from the Markov chain
#' @param burn number of initial scans of the Markov chain to be dropped
#' @param seed a random seed
#' @param repulse logical. Add a small repulsion force with wordcloud package to avoid node overlap?
#' @param repulsion scalar for the repulsion force (if repulse=T). Larger values add more repulsion
#' @param eigenmodelArgs additional arguments in list format passed to \code{eigenmodel::eigenmodel_mcmc}
#' @param ... additional arguments passed to \code{qgraph}
#'
#' @details
#'
#' An eigenmodel can be interpreted based on coordinate placement
#' of each node. A node in the top right corner scored high on both the first and second
#' latent components
#'
#' @references
#'
#' Jones, P. J., Mair, P., & McNally, R. J. (2018). Visualizing psychological networks: A tutorial in R. Frontiers in Psychology, 9, 1742. https://doi.org/10.3389/fpsyg.2018.01742
#'
#' @export
EIGENnet <- function(qgraph_net, EIGENadj=NULL, S=1000, burn=200, seed=1,
                     repulse=F, repulsion=1, eigenmodelArgs = list(), ...) {
  if(is.null(EIGENadj)){
    adj <- qgraph::getWmat(qgraph_net)
  } else {
    adj <- EIGENadj
  }
  diag(adj) <- NA
  eigenmodelArgs <- c(eigenmodelArgs, list(Y = adj, R = 2,S = S, burn = burn, seed = seed))
  fitEM <- do.call(what=eigenmodel::eigenmodel_mcmc,args=eigenmodelArgs)
  EVD <- eigen(fitEM$ULU_postmean)
  evecs <- EVD$vec[, 1:2]
  if(!repulse){
    qgraph::qgraph(qgraph_net, layout=evecs,...)
  } else {
    message("When repulsion is used, plot is not exact. Compare to unrepulsed graph before interpreting")
    repulse <- repulseLayout(qgraph_net, layout=evecs, repulsion=repulsion*.5)
    qgraph::qgraph(qgraph_net, layout=repulse,...)
  }
  invisible(evecs)
}
