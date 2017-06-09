#' Bridge Strength (inter-community strength)
#'
#' Calculates bridge strength
#' given a network and a prespecified set of communities.
#'
#' @param network a network of class "igraph", "qgraph", or an adjacency matrix representing
#' a network
#' @param communities an object of class "communities" (igraph) OR a characcter vector of
#' community  assignments for each node. The ordering of this vector should correspond
#' to the vector from argument "nodes"
#' @param useCommunities character vector specifying which communities should be included
#' @param directed logical. Directedness is automatically detected if set to "NULL" (the default).
#' Symmetric adjacency matrices will be undirected, unsymmetric matrices will be directed
#' @param nodes a vector containing the names of the nodes. If set to "NULL", this vector will
#' be automatically detected in the order given in "network"
#'
#' @details
#'
#' Centrality metrics (betweenness, closeness, etc.) illuminate how nodes are interconnected
#' among the entire network. However, sometimes we are interested in the connectivity
#' \emph{between specific communities} in a larger network. Nodes that are important in communication
#' between communities can be conceptualized as bridge nodes.
#'
#' Bridge centrality statistics aim to identify bridge nodes. Bridge centralities
#' can be calculated across all communities, or between a specific subset of coumminities (as
#' identified by the \code{useCommunities} argument)
#'
#' Bridge strength is defined as the sum of the absolute value of all edges that exist between a
#' given node and all nodes outside its own community. In a directed network, bridge strength is
#' separated into bridge in-degree and bridge out-degree.
#'
#' Functions to compute bridge betweenness and bridge closeness are in development.
#'
#' @examples
#' graph1 <- qgraph::EBICglasso(cor(depression), n=dim(depression)[1])
#' graph2 <- IsingFit::IsingFit(social)$weiadj
#'
#' bridge(graph1, communities=c(1, 1, 2, 2, 2, 2, 1, 2, 1))
#' bridge(graph2, communities=c(rep(1,8), rep(2,8)))
#'
#'
#' @return \code{\link{bridge}} returns a list of class "\code{bridge}" which contains:
#'
#'
#' See \code{\link{global.impact}}, \code{\link{structure.impact}}, and \code{\link{edge.impact}} for
#' details on each list
#'
#'@export
bridge <- function(network, communities=NULL, useCommunities="all", directed=NULL, nodes=NULL) {
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

  #take inverse of weight (igraph's length functions view small edges as closer)
  igraph::E(g)$weight <- 1/igraph::E(g)$weight

  if(is.null(nodes)){nodes <- colnames(adj)}
  if(class(communities)=="communities") {communities <- communities$membership}

  #create igraph that only includes edges which cross communities
  comm_edgelist <- coerce_to_comm_edgelist(network, communities=communities, directed=directed, nodes=nodes)
  bridge_edgelist <- extract_bridge_edgelist(comm_edgelist)
  bridge_igraph <- comm_edgelist_to_igraph(bridge_edgelist, directed=attr(bridge_edgelist, "directed"))

  # make a copy
  g2 <- g

  # Delete negative edges in the copy (if they exist)
  if(min(igraph::E(g2)$weight)<0) {
    g2 <- igraph::delete.edges(g2, which(igraph::E(g2)$weight < 0))
    warning("Negative edges ignored in calculation of bridge betweenness")
  }

  ## Bridge strength
  igraph::E(bridge_igraph)$weight <- abs(igraph::E(bridge_igraph)$weight)
  in_degree <- igraph::strength(bridge_igraph, vids = igraph::V(bridge_igraph), mode="in")
  out_degree <- igraph::strength(bridge_igraph, mode="out")
  total_strength <- igraph::strength(bridge_igraph, mode="all")
  ## add in nodes with 0 strength (they get removed when converting to edgelist)
  empty_strength_vec <- rep(0, length(nodes)); names(empty_strength_vec) <- nodes
  strength_fill <- function(strength_ob) {
    out <- empty_strength_vec
    for(i in 1:length(empty_strength_vec)) {
      if(names(empty_strength_vec)[i] %in% names(strength_ob)){
        out[i] <- strength_ob[names(empty_strength_vec)[i]]
      }
    }
    return(out)
  }
  in_degree <- strength_fill(in_degree)
  out_degree <- strength_fill(out_degree)
  total_strength <- strength_fill(total_strength)

  ## Bridge betweenness

  delete.ends <- function(x) {return(nodes[tail(head(as.vector(x), -1),-1)])}
  short.bridge.mid.paths <- function(x) {
    # Note: mode argument ignored in undirected graphs
    b <- suppressWarnings(igraph::get.all.shortest.paths(g2, from=nodes[x], to=nodes[communities!=communities[which(nodes==nodes[x])]],mode="out"))
    # turn those paths into vectors, deleting "starts" and "ends", and unify into a single vector
    c <- unlist(sapply(b$res, delete.ends))
    return(c)
  }
  betweenness.df <- as.data.frame(table(factor(unlist(sapply(1:length(nodes), short.bridge.mid.paths)), levels=nodes)))
  betweenness <- betweenness.df[,2]
  ## correct for "duplication" in undirected (e.g. node 1--2 and 2--1 were counted)
  if(!directed) {betweenness <- betweenness/2}
  names(betweenness) <- betweenness.df[,1]

  ## Bridge closeness
  b.close <- function(x) {
    # note: mode="all", so it will take the shortest path either in or out, whichever is closer
    b <- igraph::distances(g, v=nodes[x], to=nodes[communities!=communities[which(nodes==nodes[x])]], mode="all")
    c <- 1/mean(b[is.finite(b)])
    return(c)
  }
  closeness <- try(sapply(1:length(nodes), b.close), silent=TRUE)
  if(class(closeness)=="try-error") {
    b.close2 <- function(x) {
      # note: mode="all", so it will take the shortest path either in or out, whichever is closer
      b <- igraph::distances(g2, v=nodes[x], to=nodes[communities!=communities[which(nodes==nodes[x])]], mode="all")
      c <- 1/mean(b[is.finite(b)])
      return(c)
    }
    closeness <- sapply(1:length(nodes), b.close2)
    warning("Negative cycles present: negative edges ignored in calculation of bridge closeness")
  }
  names(closeness) <- nodes
  if(attr(bridge_edgelist, "directed")){
    res <- list("Bridge Indegree"=in_degree, "Bridge Outdegree"=out_degree, "Bridge Strength"=total_strength,
                "Bridge Betweenness"=betweenness, "Bridge Closeness"=closeness, communities=communities)
  }else{
    res <- list("Bridge Strength"=total_strength, "Bridge Betweenness"=betweenness, "Bridge Closeness"=closeness,
                communities=communities)
  }
  class(res) <- "bridge"
  return(res)
}
