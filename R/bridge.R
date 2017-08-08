#' Bridge Centrality
#'
#' Calculates bridge centrality metrics (bridge strength, bridge betweenness, bridge closeness, and bridge expected influence)
#' given a network and a prespecified set of communities.
#'
#' To plot the results, first save as an object, and then use plot() (see ?plot.bridge)
#'
#' @param network a network of class "igraph", "qgraph", or an adjacency matrix representing
#' a network
#' @param communities an object of class "communities" (igraph) OR a characcter vector of
#' community  assignments for each node (e.g., c("Comm1", "Comm1", "Comm2", "Comm2)).
#' The ordering of this vector should correspond to the vector from argument "nodes"
#' @param useCommunities character vector specifying which communities should be included. Default set to "all"
#' @param directed logical. Directedness is automatically detected if set to "NULL" (the default).
#' Symmetric adjacency matrices will be undirected, unsymmetric matrices will be directed
#' @param nodes a vector containing the names of the nodes. If set to "NULL", this vector will
#' be automatically detected in the order extracted
#'
#' @details
#'
#' Centrality metrics (strength, betweenness, etc.) illuminate how nodes are interconnected
#' among the entire network. However, sometimes we are interested in the connectivity
#' \emph{between specific communities} in a larger network. Nodes that are important in communication
#' between communities can be conceptualized as bridge nodes.
#'
#' Bridge centrality statistics aim to identify bridge nodes. Bridge centralities
#' can be calculated across all communities, or between a specific subset of coumminities (as
#' identified by the \code{useCommunities} argument)
#'
#' The bridge() function currently returns 5 centrality metrics: 1) bridge strength,
#' 2) bridge betweenness, 3) bridge closeness, 4) bridge expected influence (1-step), and
#' 5) bridge expected influence (2-step)
#'
#' Bridge strength is defined as the sum of the absolute value of all edges that exist between a
#' node A and all nodes that are not in the same community as node A. In a directed network, bridge strength can be
#' separated into bridge in-degree and bridge out-degree.
#'
#' Bridge betweenness is defined as the number of times a node B lies on the shortest path between
#' nodes A and C, where nodes A and C come from different communities.
#'
#' Bridge closeness is defined as the average length of the path from a node A to all nodes that are
#' not in the same community as node A
#'
#' Bridge expected influence (1-step) is defined as the sum of the value (+ or -) of all edges that
#' exist between a node A and all nodes that are not in the same community as node A. In a directed network, expected influence
#' only considers edges extending from the given node (e.g., out-degree)
#'
#' Bridge expected influence (2-step) is similar to 1-step, but also considers the indirect effect that a node A may have
#' through other nodes (e.g, an indirect effect on node C as in A -> B -> C).
#' Indirect effects are weighted by the first edge weight (e.g., A -> B), and then added to the 1-step expected influence
#'
#' If negative edges exist, bridge expected influence should be used. Any negative edges are currently ignored in the
#' algorithms to compute bridge betweenness and bridge closeness.
#'
#' @examples
#' \donttest{
#' graph1 <- qgraph::EBICglasso(cor(depression), n=dim(depression)[1])
#' graph2 <- IsingFit::IsingFit(social)$weiadj
#'
#' b <- bridge(graph1, communities=c('1','1','2','2','2','2','1','2','1'))
#' b
#' b2 <- bridge(graph2, communities=c(rep('1',8), rep('2',8)))
#' b2
#'
#' plot(b)
#' plot(b2, order="value", zscore=TRUE, include=c("Bridge Strength", "Bridge Betweenness"))
#'
#'}
#' @return \code{\link{bridge}} returns a list of class "\code{bridge}" which contains:
#'
#'
#' See \code{\link{global.impact}}, \code{\link{structure.impact}}, and \code{\link{edge.impact}} for
#' details on each list
#'
#'@export
bridge <- function(network, communities=NULL, useCommunities="all", directed=NULL, nodes=NULL) {
  adj <- coerce_to_adjacency(network)
  adjmat <- adj
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

  #take inverse of weight for igraph object only (igraph's length functions view small edges as closer)
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

  delete.ends <- function(x) {return(nodes[utils::tail(utils::head(as.vector(x), -1),-1)])}
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

  ## Bridge expected influence (1 step)
  if(directed==FALSE){
    diag(adjmat) <- 0
  }
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
  ei1 <- sapply(nodes, FUN= expectedInfBridge, network=adjmat, nodes=nodes, communities=communities)
  names(ei1) <- nodes

  ## Bridge expected influence (2 step)
    ## This function finds the influence of a node on the jth unique community
    influence_on_comm_j <- function(node_of_interest, network, nodes, communities, j) {
      names(communities) <- nodes
      included_nodes <- unique(c(node_of_interest, names(communities[communities==unique(communities)[j]])))
      new_net <- network[included_nodes, included_nodes]
      ei1_node <- expectedInf(new_net, step=1, directed=directed)[[1]][node_of_interest]
      return(ei1_node)
    }
    ## This loop creates a list of j vectors
    ## Each vector contains the influence of each node on the jth community
    infcomm <- list()
    for(j in 1:length(unique(communities))){
      infcomm[[j]] <- sapply(nodes, FUN=influence_on_comm_j, network=adjmat, nodes=nodes,
                             communities=communities, j=j)
      names(infcomm[[j]]) <- nodes
    }
    ei2func <- function(node_of_interest, network, nodes, communities) {
      names(communities) <- nodes
      comm_int <- communities[match(node_of_interest, nodes)] # finds the community of the node of interest
      non_comm_vec <- unique(communities)[unique(communities) != comm_int] #vector of all communities OTHER than comm_int
      ei2_vec <- vector()
      for(i in 1:length(non_comm_vec)) {
        ei2.wns <-sweep(adjmat, MARGIN=2, infcomm[[which(unique(communities)==non_comm_vec[i])]], "*") ## sweep by influence on community i
        ei2_vec[i]<- sum(ei2.wns[node_of_interest,]) # influence of node of interest on non_comm[i]
      }
      ei2 <- sum(ei2_vec) + ei1[node_of_interest] # sums the influence on all OTHER communities, plus ei1
      return(ei2)
    }
    ei2 <- sapply(nodes, FUN=ei2func, network=adjmat, nodes=nodes, communities=communities)
    names(ei2) <- nodes


  if(attr(bridge_edgelist, "directed")){
    res <- list("Bridge Indegree"=in_degree, "Bridge Outdegree"=out_degree, "Bridge Strength"=total_strength,
                "Bridge Betweenness"=betweenness, "Bridge Closeness"=closeness,
                "Bridge Expected Influence (1-step)"=ei1, "Bridge Expected Influence (2-step)"=ei2,
                communities=communities)
  }else{
    res <- list("Bridge Strength"=total_strength, "Bridge Betweenness"=betweenness, "Bridge Closeness"=closeness,
                "Bridge Expected Influence (1-step)"=ei1, "Bridge Expected Influence (2-step)"=ei2,
                communities=communities)
  }
  class(res) <- "bridge"
  return(res)
}
