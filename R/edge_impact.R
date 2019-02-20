#' Edge Impact
#'
#' This function is DEPRECATED and will be replaced by impact().
#'
#' Generates a matrix of edge impacts for each specified node. Each scalar in a given
#' matrix represents the degree to which the level of a node impacts the strength of
#' a specified edge in the network
#'
#' @param input a matrix or data frame of observations (not a network/edgelist).
#' See included example datasets \code{\link{depression}} and \code{\link{social}}.
#' @param gamma the sparsity parameter used in generating networks. Defaults to 0.5
#' for interval data and 0.25 for binary data
#' @param nodes indicates which nodes should be tested. Can be given
#' as a character string of desired nodes (e.g., c("node1","node2")) or as a
#' numeric vector of column numbers (e.g., c(1,2)).
#' @param binary.data logical. Indicates whether the input data is binary
#' @param weighted logical. Indicates whether resultant networks preserve
#' edge weights or binarize edges.
#' @param split method by which to split network given non-binary data. "median": median split (excluding the median),
#' "mean": mean split, "forceEqual": creates equally sized groups by partitioning random median observations
#'  to the smaller group, "cutEqual": creates equally sized groups by deleting random values
#'  from the bigger group,"quartile": uses the top and bottom quartile as groups
#'
#' @details
#'
#' For an explanation of impact functions in general, see \code{\link{impact}}.
#'
#' Edge impact is the change in an edge's value as a function of a given node.
#' A separate edge impact value is calculated for each edge in the network.
#'
#' It is highly useful to plot the edge impacts as if they were a network. Positive edges
#' in the resultant graph can be interpreted as edges that were made more positive by the
#' given node, and negative edges can be interpreted as edges that were made more negative
#' by the given node.
#'
#' The $hi and $lo output of \code{edge.impact} can also be used to quickly visualize
#' the difference in network structure depending on node level (see examples).
#'
#'
#' @return \code{edge.impact()} returns a list of class "\code{edge.impact}" which contains:
#'  \item{impact}{a list of matrices. Each symmetric matrix contains the edge
#'  impacts for the given node}
#'  \item{lo}{a list of matrices. Each symmetric matrix contains the edge
#'  estimates for the given node's lower half}
#'  \item{hi}{a list of matrices. Each symmetric matrix contains the edge
#'  estimates for the given node's upper half}
#'  \item{edgelist}{a list of dataframes. Each dataframe contains an edgelist of
#'  edge impacts}
#' @export
edge.impact <- function(input, gamma, nodes = c("all"), binary.data = FALSE, weighted = TRUE,
                        split=c("median","mean", "forceEqual", "cutEqual", "quartiles")) {
  message("Note: This function is DEPRECATED and will be replaced by impact()")
  if (missing(gamma)){
    if (binary.data){
      gamma <- 0.25
    } else {
      gamma <- 0.5
    }
  }

  ## Set up empty vectors
  nwinv.lo<-nwinv.hi<-nwinv.real<-glstr.lo <- glstr.hi <- glstrinv.real <- vector()
  edges.hi<-edges.lo<-nwinv.edge<-diffedges.edgelist<-diffedges.real<-list()

  ## Check to make sure input is not a network object
  if("qgraph" %in% class(input)| "igraph" %in% class(input) | "bootnetResult" %in% class(input)) {
    stop("Network object detected as input. Input must contain observational data. See included datasets ?depression and ?social for examples")
  }

  ## Put data into dataframe format
  input <- data.frame(input)

  ## Determine which nodes to test
  if(nodes[1] == "all") {
    nodesTested <- 1:dim(input)[2]
  } else if(is.numeric(nodes)){
    nodesTested <- nodes
  } else if(is.character(nodes)){
    nodesTested <- match(nodes,names(input))
  }

  ## This is running a for loop to calculate the impact for each node
  for(i in 1:length(nodesTested)) {
    j <- nodesTested[i] ## This specifies the exact node in terms of what row it is in the input dataframe

    if(binary.data==FALSE) { ## If the data is non-binary, a graphical lasso with a median split is used
      if(match.arg(split)=="median"){
        hi <- input[input[,j]> stats::median(input[,j]),][,-j] ## This takes the "upper" half of the participants, on column i. It also cuts out column i itself
        lo <- input[input[,j] < stats::median(input[,j]),][,-j] ## This takes the "lower" half of the participants, on column i. It also cuts out column i itself
      }
      if(match.arg(split)=="mean"){
        hi <- input[input[,j]> mean(input[,j]),][,-j]
        lo <- input[input[,j] < mean(input[,j]),][,-j]
      }
      if(match.arg(split)=="forceEqual"){
        hi <- input[input[,j] > stats::median(input[,j]),]
        lo <- input[input[,j] < stats::median(input[,j]),]
        med <- input[input[,j] == stats::median(input[,j]),]
        random_sample_med <- med[sample(1:dim(med)[1], abs(dim(hi)[1]-dim(lo)[1])),]
        if(dim(hi)[1]<dim(lo)[1]){hi <- rbind(hi, random_sample_med)}
        if(dim(hi)[1]>dim(lo)[1]){lo <- rbind(lo, random_sample_med)}
        hi <- hi[,-j]
        lo <- lo[,-j]
      }
      if(match.arg(split)=="cutEqual"){
        hi <- input[input[,j] > stats::median(input[,j]),]
        lo <- input[input[,j] < stats::median(input[,j]),]
        if(dim(hi)[1]<dim(lo)[1]){lo <- lo[-sample(1:dim(lo)[1], abs(dim(hi)[1]-dim(lo)[1])),-j]}
        if(dim(hi)[1]>dim(lo)[1]){hi <- hi[-sample(1:dim(hi)[1], abs(dim(hi)[1]-dim(lo)[1])),-j]}
      }
      if(match.arg(split)=="quartile"){
        hi <- input[input[,j]>= stats::quantile(input[,j],probs=.75),][,-j]
        lo <- input[input[,j]<= stats::quantile(input[,j],probs=.25),][,-j]
      }
      if((abs(dim(hi)[1]-dim(lo)[1])/dim(input)[1]) > 0.1) {message(colnames(input)[nodesTested[i]], ": Sample size difference after median split is >10% of total sample, try using split=\"forceEqual\"")}
      catch1 <- try(qgraph::EBICglasso(stats::cor(hi),nrow(hi),gamma=gamma), silent = FALSE)
      catch2 <- try(qgraph::EBICglasso(stats::cor(lo),nrow(lo),gamma=gamma), silent = FALSE)
      if(inherits(catch1, "try-error") | inherits(catch2, "try-error")) { }
      if(!inherits(catch1, "try-error") & !inherits(catch2, "try-error")) {
        nw1 <- catch1 ## This calculates the network for the upper half
        ## If the sample is too small or has too little variance, an error will occur. Try helps to manage the errors
        nw2 <- catch2 ## This caluclates the network for the lower half
        if(weighted==FALSE){ ## If the network is unweighted, the edges will be driven down to 0 or up to 1
          nw1=(nw1!=0)*1
          nw2=(nw2!=0)*1
        }
      }
    }

    if(binary.data==TRUE)  { ## If the data is binary, an Ising model is used. Data is split by 0s and 1s
      hi <- input[input[,j]==1,][,-j] ## This takes the "upper" half of the participants, on column i. It also cuts out column i itself
      lo <- input[input[,j]==0,][,-j] ## This takes the "lower" half of the participants, on column i. It also cuts out column i itself
      if(match.arg(split)=="cutEqual"){
        if(dim(hi)[1]<dim(lo)[1]){lo <- lo[-sample(1:dim(lo)[1], abs(dim(hi)[1]-dim(lo)[1])),]}
        if(dim(hi)[1]>dim(lo)[1]){hi <- hi[-sample(1:dim(hi)[1], abs(dim(hi)[1]-dim(lo)[1])),]}
      }
      if((abs(dim(hi)[1]-dim(lo)[1])/dim(input)[1]) > 0.1) {message(colnames(input)[nodesTested[i]], ": Sample size difference after split is >10% of total sample, try using split=\"cutEqual\"")}
      catch1 <- try({IsingFit::IsingFit(hi, gamma=gamma, plot=FALSE, progressbar=FALSE)}, silent = FALSE)
      catch2 <- try({IsingFit::IsingFit(lo, gamma=gamma, plot=FALSE, progressbar=FALSE)}, silent= FALSE)
      if(inherits(catch1, "try-error") | inherits(catch2, "try-error")) { }
      if(!inherits(catch1, "try-error") & !inherits(catch2, "try-error")) {
        nw1 <- catch1$weiadj ## This calculates the network for the upper half
        ## If the sample is too small or has too little variance, an error will occur. Try helps to manage the errors
        nw2 <- catch2$weiadj ## This caluclates the network for the lower half
        if(weighted==FALSE){
          nw1=(nw1!=0)*1
          nw2=(nw2!=0)*1
        }
      }
    }
    if(inherits(catch1, "try-error") | inherits(catch2, "try-error")) {
      glstr.lo[i]<-glstr.hi[i]<-glstrinv.real[i]<-edges.lo[[i]]<-edges.hi[[i]]<-diffedges.real[[i]]<-diffedges.edgelist[[i]]<-nwinv.lo[[i]]<-nwinv.hi[[i]]<-nwinv.edge[[i]] <-nwinv.real[i]<-NA
    }
    if(!inherits(catch1, "try-error") & !inherits(catch2, "try-error")) {
######################## Specific Output ###############################
      ##### Invariance measures #####

      ## Individual edge invariance
      diffedges.real[[i]] <- (nw1-nw2) ## This calculates the differences by edge
      diffedges.edgelist[[i]] <- reshape2::melt(diffedges.real[[i]]) ## This turns the invariances into an edgelist
      edges.lo[[i]] <- nw2
      edges.hi[[i]] <- nw1
    }
  }

  ## What to return

  Edge <- list()
  Edge$impact <- diffedges.real
  Edge$lo <- edges.lo
  Edge$hi <- edges.hi
  Edge$edgelist <- diffedges.edgelist
  names(Edge$impact)<-names(Edge$edgelist)<-names(Edge$lo)<-names(Edge$hi)<-names(input)[nodesTested]
  class(Edge) <- "edge.impact"
  return(Edge)
}
