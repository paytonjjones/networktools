#' Expected Influence
#'
#' Calculates the one-step and two-step expected influence of each node.
#'
#' @param network an object of type \code{qgraph}, \code{igraph}, or an adjacency
#' matrix representing a network. Adjacency matrices should be complete (e.g., not
#' only upper or lower half)
#' @param step compute 1-step expected influence, 2-step expected influence,
#' or both
#' @param directed logical. Specifies if edges are directed, defaults to FALSE
#'
#' @details
#'
#' When a network contains both positive and negative edges, traditional centrality measures
#' such as strength centrality may not accurately predict node influence on the network.
#' Robinaugh, Millner, & McNally (2016) showed that in these cases,
#' expected influence is a more appropriate measure.
#'
#' One-step expected influence is defined as the sum of all edges extending
#' from a given node (where the sign of each edge is maintained).
#'
#' Two-step expected influence, as the name implies, measures connectivity up to two edges away from the node.
#' It is defined as the sum of the (weighted) expected influences of each node connected to the initial node
#' plus the one-step expected influence of the initial node. Weights are determined by the edge strength between
#' the initial node and each "second step" node.
#'
#' See citations in the references section for further details.
#'
#' @examples
#' out1 <- expectedInf(cor(depression))
#'
#' plot(out1)
#' plot(out1, order="value", zscore=TRUE)
#' out1$step1
#' out1$step2
#'
#' igraph_obj <- igraph::graph_from_adjacency_matrix(cor(depression))
#' out_igraph <- expectedInf(igraph_obj)
#'
#' qgraph_obj <- qgraph::qgraph(cor(depression), DoNotPlot=TRUE)
#' out_qgraph <- expectedInf(qgraph_obj)
#'
#' Ising_adj_mat <- IsingFit::IsingFit(social, plot=FALSE)$weiadj
#' out_Ising <- expectedInf(Ising_adj_mat)
#' plot(out_Ising)
#'
#' @references
#' Robinaugh, D. J., Millner, A. J., & McNally, R. J. (2016). Identifying highly influential nodes in the complicated grief network. \emph{Journal of abnormal psychology}, 125, 747.
#'
#' @export
expectedInf <- function(network, step=c("both", 1, 2), directed=FALSE) {
  adjmat <- coerce_to_adjacency(network, directed=directed)
  if(attr(adjmat, "directed")==FALSE){
    diag(adjmat) <- 0
  }
  ei1<-apply(adjmat, 1, sum)
  if(step[1]=="both"|step[1]==2){
  ei2.wns <-sweep(adjmat, MARGIN=2, ei1, "*")
  ei2.aei<- apply(ei2.wns, 1, sum)
  ei2<-ei1+ei2.aei}
  class(ei1) <- class(ei2) <-"expectedInf"
  if(step[1]=="both"){res <- list(step1=ei1, step2=ei2)}
  if(step[1]==1){res <- list(step1=ei1)}
  if(step[1]==2){res <- list(step2=ei2)}
  class(res) <- "expectedInf"
  return(res)
}


