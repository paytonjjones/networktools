## This function takes an "igraph", "qgraph", or adjacency matrix as input
## and a vector of community names that matches the vector of nodes, and
## returns an edgelist with "from_comm" and "to_comm" columns
coerce_to_comm_edgelist <- function(input, communities=NULL, directed=NULL, nodes=NULL) {
  if(class(input)=="igraph"){
    mat <- as.matrix(igraph::get.adjacency(input, type="lower"))
    edgelist <- reshape2::melt(mat)
    edgelist <- edgelist[edgelist$value != 0,]
    if(is.null(directed)){
      directed <- igraph::is_directed(input)
    }
    if(is.null(nodes)){
      nodes <- names(igraph::V(input))
    }
  } else if(class(input)=="qgraph"){
    if(is.null(nodes)){
      if(is.null(input$graphAttributes$Nodes$names)) {
        nodes <- 1:length(unique(c(input$Edgelist$from, input$Edgelist$to)))
      } else {nodes <- input$graphAttributes$Nodes$names}
    }
    col1 <- nodes[input$Edgelist$from]
    col3 <- nodes[input$Edgelist$to]
    edgelist <- as.data.frame(cbind(col1, col3, input$Edgelist$weight))
    if(is.null(directed)){
      if(TRUE %in% input$Edgelist$directed){
        directed <-TRUE} else {directed <- FALSE}
    }
  } else if (class(input)=="matrix") {
    if(is.null(directed)){
      directed <- !isSymmetric.matrix(unname(input))
    }
    if(!directed){
      input[upper.tri(input, diag=TRUE)] <- 0
    }
    if(is.null(nodes)){
      nodes <- colnames(input)
    }
    colnames(input) <- rownames(input) <- nodes
    edgelist <- reshape2::melt(input)
    edgelist <- edgelist[edgelist$value != 0,]
  }
  attr(edgelist, "directed") <- directed
  attr(edgelist, "nodes") <- nodes
  colnames(edgelist) <- c("from", "to", "value")
  if(is.null(communities)) {
    edgelist$comm_from <- NA
    edgelist$comm_to <- NA
  } else if(class(communities)=="communities") {
    edgelist$comm_from <- communities$membership[match(edgelist$from, nodes)]
    edgelist$comm_to <- communities$membership[match(edgelist$to, nodes)]
  } else {
    edgelist$comm_from <- communities[match(edgelist$from, nodes)]
    edgelist$comm_to <- communities[match(edgelist$to, nodes)]
  }
  return(edgelist)
}

#' @export
coerce_to_adjacency <- function(input, directed=NULL) {
  if(class(input)=="igraph"){
    mat <- as.matrix(igraph::get.adjacency(input, type="both"))
    if(is.null(directed)){
      directed <- igraph::is_directed(input)
    }
    } else if(class(input)=="qgraph"){
    col1 <- input$Edgelist$from
    col3 <- input$Edgelist$to
    edgelist <- as.data.frame(cbind(col1, col3, input$Edgelist$weight))
    if(is.null(input$graphAttributes$Nodes$names)) {
      nodes <- 1:length(unique(c(col1, col3)))
    } else {nodes <- input$graphAttributes$Nodes$names}
    mat <- matrix(0, length(nodes), length(nodes))
    colnames(mat) <- rownames(mat) <- nodes
    mat[as.matrix(edgelist)[,1:2]] <- edgelist[,3]
    if(is.null(directed)){
    if(TRUE %in% input$Edgelist$directed){
      directed <-TRUE} else {directed <- FALSE}
    }
    if(!directed){
     mat <- mat + t(mat) }
    } else {
    mat <- matrix(input)
    if(is.null(directed)) {
      directed <- !isSymmetric(unname(input))
    }
    }
    attr(mat, "directed") <- directed
    return(mat)
  }

## This function extracts the relevant "bridge" edgelist
## from a complete edgelist
extract_bridge_edgelist <- function(edgelist, useCommunities="all"){
  if(useCommunities=="all"){
  edgelist_bridge <- edgelist[edgelist$comm_from != edgelist$comm_to, ]
  } else {
  edgelist_bridge <- edgelist[edgelist$comm_from %in% useCommunities & edgelist$comm_to %in% useCommunities & edgelist$comm_from != edgelist$comm_to, ]
  }
  return(edgelist_bridge)
}

## In a similar fashion, this function extracts the relevant "intra" edgelist
## from a complete edgelist
extract_intra_edgelist <- function(edgelist, useCommunities="all"){
  if(useCommunities=="all"){
    edgelist_bridge <- edgelist[edgelist$comm_from == edgelist$comm_to, ]
  } else {
    edgelist_bridge <- edgelist[edgelist$comm_from %in% useCommunities & edgelist$comm_to %in% useCommunities & edgelist$comm_from == edgelist$comm_to, ]
  }
  return(edgelist_bridge)
}

## Turns any edgelist into an igraph object. Needs direction in input
comm_edgelist_to_igraph <- function(edgelist, directed) {
  edgelist[,1]<-as.character(edgelist[,1])
  edgelist[,2]<-as.character(edgelist[,2])
  edgelist<-as.matrix(edgelist)
  g<-igraph::graph_from_edgelist(edgelist[,1:2],directed=directed)
  igraph::E(g)$weight <- as.numeric(edgelist[,3])
  return(g)
}
