## This function takes an "igraph", "qgraph", or adjacency matrix as input
## and a vector of community names that matches the vector of nodes, and
## returns an edgelist with "from_comm" and "to_comm" columns
coerce_to_comm_edgelist <- function(input, communities=NULL, directed=NULL, nodes=NULL) {
  if(class(input)=="igraph"){
    if(is.null(E(input)$weight)){
      mat <- as.matrix(igraph::get.adjacency(input, type="lower"))
    } else {
      mat <- as.matrix(igraph::get.adjacency(input, type="lower", attr="weight"))
    }
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
      directed <- !isSymmetric(unname(as.matrix(input)))
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
  } else {
    input <- as.matrix(input)
    if(is.null(directed)){
      directed <- !isSymmetric(unname(as.matrix(input)))
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

#' Coerce to adjacency matrix
#'
#' Takes an object of type "qgraph", "igraph", or an adjacency matrix (or data.frame) and outputs an adjacency matrix
#'
#' @param input a network of class "igraph", "qgraph", or an adjacency matrix representing
#' a network
#' @param directed logical. is the network directed? If set to NULL, auto-detection is used
#'
#' @export
coerce_to_adjacency <- function(input, directed=NULL) {
  ## igraph input
  if(class(input)=="igraph"){
    if(is.null(E(input)$weight)){
      mat <- as.matrix(igraph::get.adjacency(input, type="both"))
    } else {
      mat <- as.matrix(igraph::get.adjacency(input, type="both", attr="weight"))
    }
    if(is.null(directed)){
      directed <- igraph::is_directed(input)
    }

    ## qgraph input
    } else if(class(input)=="qgraph"){
    col1 <- input$Edgelist$from
    col3 <- input$Edgelist$to
    edgelist <- as.data.frame(cbind(col1, col3, input$Edgelist$weight))
    if(is.null(input$graphAttributes$Nodes$names) | input$graphAttributes$Nodes$names[1] == TRUE) {
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

    ## Matrix/data.frame of adjacency matrix input
    } else if(dim(input)[1]==dim(input)[2]){
    mat <- as.matrix(input)
    if(is.null(directed)) {
      ## if upper or lower half is missing, flip the matrix and make symmetric
      if(sum(mat[upper.tri(mat)], na.rm=T)==0 | sum(mat[lower.tri(mat)], na.rm=T)==0){
        directed <- FALSE
        mat[is.na(mat)] <- 0
        mat_t <- t(mat)
        diag(mat_t) <- 0
        mat <- mat + mat_t
      } else {
        directed <- !isSymmetric(unname(mat))
      }
    }
    # fix discrepancies in row/column names
    if(is.null(rownames(mat)) & is.null(colnames(mat))){
      colnames(mat)<-rownames(mat)<- paste("node",1:dim(mat)[2], sep="")
    } else if (is.null(rownames(mat))) {
      rownames(mat)<-colnames(mat)
    } else if (is.null(colnames(mat))) {
      colnames(mat)<-rownames(mat)
    } else if(TRUE %in% c(rownames(mat)!=colnames(mat))){
      rownames(mat) <- colnames(mat)
    }

    ## Wrong input
    } else {
      stop("Invalid input: please use an object of class qgraph, igraph, or an adjacency matrix")
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
  edgelist_1_2 <- as.matrix(edgelist[,1:2])
  #if(dim(edgelist)[1]==1) {edgelist_1_2 <- t(edgelist_1_2)}
  g<-igraph::graph_from_edgelist(edgelist_1_2,directed=directed)
  igraph::E(g)$weight <- as.numeric(edgelist[,3])
  return(g)
}

## Repulses nodes (wraps wordcloud) to avoid overlap in MCAnet, PCAnet, EIGENnet
repulseLayout <- function(qgraph, layout=NULL, repulsion=1){
  vsize <- qgraph$graphAttributes$Nodes$width[1]
  if(is.null(layout)){
    layout <- qgraph$layout
  }
  plotSize <- dev.size("in")
  constant <- .025 *(plotSize[1]*plotSize[2]) ## approximately maps size of node size to @ symbol
  cexRepulseFull <- vsize * constant * repulsion
  newlayout <- try(wordcloud::wordlayout(layout[,1],layout[,2], words=rep('@', nrow(layout)),
                          cex=cexRepulseFull), silent=T)
  if(class(newlayout)=="try-error"){
    plot.new()
    newlayout <- wordcloud::wordlayout(layout[,1],layout[,2], words=rep('@', nrow(layout)),
                                           cex=cexRepulseFull)
  }
  return(newlayout[,1:2])
}


