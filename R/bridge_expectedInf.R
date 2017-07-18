#' Bridge Expected Influence
#'
#' Calculates the expected influence of a node on communities other than
#' its own community. Note that currently, 2-step bridge EI only works for graphs
#' with exactly 2 communities
#'
#' @param network an object of type \code{qgraph}, \code{igraph}, or an adjacency
#' matrix representing a network. Adjacency matrices should be complete (e.g., not
#' only upper or lower half)
#' @param communities an object of class "communities" (igraph) OR a characcter vector of
#' community  assignments for each node. The ordering of this vector should correspond
#' to the vector from argument "nodes"
#' @param useCommunities character vector specifying which communities should be included
#' @param directed logical. Directedness is automatically detected if set to "NULL" (the default).
#' Symmetric adjacency matrices will be undirected, unsymmetric matrices will be directed
#' @param nodes a vector containing the names of the nodes. If set to "NULL", this vector will
#' be automatically detected in the order given in "network"
#' @param step compute 1-step expected influence, 2-step expected influence,
#' or both
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
#' @references
#' Robinaugh, D. J., Millner, A. J., & McNally, R. J. (2016). Identifying highly influential nodes in the complicated grief network. \emph{Journal of abnormal psychology}, 125, 747.
#'
#' @export
bridge_expectedInf <- function(network, communities=NULL, useCommunities="all", directed=NULL, nodes=NULL, step=c("both", 1, 2)) {
  adj <- coerce_to_adjacency(network)
  #coerce_to_adjacency includes auto-detection of directedness
  if(is.null(directed)) {directed<-attr(adj,"directed")}
  # get igraph of complete network
  if(directed) {
    g <- igraph::graph_from_adjacency_matrix(adj, mode="directed", diag=FALSE, weighted= TRUE)
  } else {
    g <- igraph::graph_from_adjacency_matrix(adj, mode="upper", diag=FALSE, weighted= TRUE)
  }

  #if communities not supplied, use spinglass default settings to detect
  if(is.null(communities) | class(communities)=="function"){
    communities <- try(igraph::spinglass.community(g, spins=3))
    if(class(communities)=="try-error") {stop("Automatic community detection failed. Please prespecify communities")}
    message("Note: Communities automatically detected with spinglass. Use \'communities\' argument to prespecify community structure")
  }

  if(is.null(nodes)){nodes <- colnames(adj)}
  if(class(communities)=="communities") {communities <- communities$membership}

  # for each node, create an adjacency matrix which contains that node,
  # and all nodes from OTHER communities

  ## Bridge expectedInf (step 1)
  ## takes the given node, and makes an adjacency matrix with that node,
  ## and all nodes of OTHER communities, then computes EI of just that node

  if(directed==FALSE){
    diag(adj) <- 0
  }
  ## get rid of diagonal if undirected

  expectedInfBridge <- function(node_of_interest, network, nodes, communities) {
    names(communities) <- nodes
    comm_int <- communities[match(node_of_interest, nodes)] # finds the community of the node_of_interest
    other_comm <- nodes[communities != comm_int] # creates a vector of all nodes not in the community of node_of_interest
    included_nodes <- c(node_of_interest, other_comm)
    new_net <- network[included_nodes, included_nodes]
    ei1_node <- expectedInf(new_net, step=1, directed=directed)[[1]][node_of_interest]
    return(ei1_node)
  }
  ## apply to all nodes
  ei1 <- sapply(nodes, FUN= expectedInfBridge, network=adj, nodes=nodes, communities=communities)
  names(ei1) <- nodes

  if(step[1]=="both" | step[1]==2){
    ## Bridge expectedInf (step2)
    influence_on_comm1 <- function(node_of_interest, network, nodes, communities) {
      names(communities) <- nodes
      included_nodes <- c(node_of_interest, names(communities[communities==1]))
      new_net <- network[included_nodes, included_nodes]
      ei1_node <- expectedInf(new_net, step=1, directed=directed)[[1]][node_of_interest]
      return(ei1_node)
    }
    infcomm1 <- sapply(nodes, FUN= influence_on_comm1, network=adj, nodes=nodes, communities=communities)
    names(infcomm1) <- nodes

    influence_on_comm2 <- function(node_of_interest, network, nodes, communities) {
      names(communities) <- nodes
      included_nodes <- c(node_of_interest, names(communities[communities==2]))
      new_net <- network[included_nodes, included_nodes]
      ei1_node <- expectedInf(new_net, step=1, directed=directed)[[1]][node_of_interest]
      return(ei1_node)
    }
    infcomm2 <- sapply(nodes, FUN= influence_on_comm2, network=adj, nodes=nodes, communities=communities)
    names(infcomm2) <- nodes

    ei2func <- function(node_of_interest, network, nodes, communities, infcomm1, infcomm2) {
      names(communities) <- nodes
      if(communities[match(node_of_interest, nodes)] == 1) { ## if node_of_interest is in community 1
        ei2.wns <-sweep(network, MARGIN=2, infcomm2, "*")
        ei2.aei<- sum(ei2.wns[1,]) # sum of node of interest row
        ei2<-ei1[node_of_interest]+ei2.aei
      } else {
        ei2.wns <-sweep(network, MARGIN=2, infcomm1, "*")
        ei2.aei<- sum(ei2.wns[node_of_interest,]) # sum of node of interest row
        ei2<-ei1[node_of_interest]+ei2.aei
      }
      return(ei2)
    }
    ei2 <- sapply(nodes, FUN=ei2func, network=adj, nodes=nodes, communities=communities,
                  infcomm1=infcomm1, infcomm2=infcomm2)
  }

  if(step[1]=="both"){res <- list(step1=ei1, step2=ei2)}
  if(step[1]==1){res <- list(step1=ei1)}
  if(step[1]==2){res <- list(step2=ei2)}

  class(res) <- "bridge_expectedInf"
  return(res)
}
