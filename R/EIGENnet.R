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
#' @param ... additional arguments passed to \code{eigenmodel::eigenmodel_mcmc}
#'
#' @details
#'
#' An eigenmodel can be interpreted based on coordinate placement
#' of each node. A node in the top right corner scored high on both the first and second 
#' latent components
#'
#' @references 
#' 
#' Jones, P. J., Mair, P., & McNally, R. J. (2017). Scaling networks for two-dimensional visualization: a tutorial. Retrieved from osf.io/eugsz
#' 
#' @export
EIGENnet <- function(qgraph_net, EIGENadj=NULL, S=1000, burn=200,  seed=1, ...) {
  if(is.null(EIGENadj)){
    adj <- qgraph::getWmat(qgraph_net)
  } else {
    adj <- EIGENadj
  }
  diag(adj) <- NA   
  fitEM <- eigenmodel::eigenmodel_mcmc(Y = adj, R = 2, S = S, burn = burn, seed = seed,...)
  EVD <- eigen(fitEM$ULU_postmean) 
  evecs <- EVD$vec[, 1:2]      
  qgraph::qgraph(qgraph_net, layout=evecs)
}
