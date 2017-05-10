#' Network Structure Impact
#'
#' Generates the network structure impact of each specified node. Network structure
#' impact can be interpreted as the degree to which the level of a node causes change
#' in the network structure
#'
#' @param input a matrix or data frame of observations (not a network/edgelist).
#' See included example datasets \code{\link{depression}} and \code{\link{social}}.
#' @param gamma the sparsity parameter used in generating networks. Defaults to 0.5
#' for interval data and 0.25 for binary data
#' @param nodes indicates which nodes should be tested. Can be given
#' as a character string of desired nodes (e.g., c("node1","node2")) or as a numeric
#' vector of column numbers (e.g., c(1,2)).
#' @param binary.data logical. Indicates whether the input data is binary
#' @param weighted logical. Indicates whether resultant networks preserve edge weights
#' or binarize edges. Note: unweighted networks will always result in a network
#' structure impact of 0 or 1.
#' @param split method by which to split network given non-binary data. "median": median split (excluding the median),
#' "mean": mean split, "forceEqual": creates equally sized groups by partitioning median observations
#'  to the smaller group, "quartile": uses the top and bottom quartile as groups
#'
#' @details
#'
#' For an explanation of impact functions in general, see \code{\link{impact}}.
#'
#' Network structure impact computes network structure invariance as a
#' function of node level. Network structure invariance is defined as the absolute value of
#' the single largest edge invariance between two networks. A large network structure invariance
#' is generally used to indicate the \emph{instability} of network structure across groups. For instance, a low global
#' strength invariance coupled with a high network structure invariance would mean that although the
#' overall connectivity remains stable, the actual structure of those edges is unstable across groups.
#'
#' @examples
#' out <- structure.impact(depression[,1:3])
#' \donttest{
#' out1 <- structure.impact(depression)
#' out2 <- structure.impact(depression, gamma=0.65,
#'     nodes=c("sleep_disturbance", "psychomotor_retardation"))
#' out3 <- structure.impact(social, binary.data=TRUE)
#' out4 <- structure.impact(social, nodes=c(1:6, 9), binary.data=TRUE)
#'
#' summary(out1)
#' plot(out1)
#'
#' #Determine which edge drove network structure impact of "sadness"
#' out1$edge$sadness
#'}
#' @return \code{structure.impact()} returns a list of class "\code{structure.impact}" which contains:
#'  \item{impact}{a named vector containing the network structure impact for each node tested. Network structure impacts are given as absolute values}
#'  \item{edge}{a list of vectors. Each vector contains a the edge impact of the most strongly impacted edge (e.g., the network structure impact)}
#'  \item{lo}{a named vector containing the edge estimate for the lower half of the most strongly impacted edge}
#'  \item{hi}{a named vector containing the edge estimate for the upper half of the most strongly impacted edge}
#' @export
structure.impact <- function(input, gamma, nodes = c("all"), binary.data = FALSE, weighted = TRUE,
                             split=c("median","mean", "forceEqual", "quartiles")) {

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
        if(dim(hi)[1]<dim(lo)[1]){
          hi <- input[input[,j]>= stats::median(input[,j]),]
          hi <- hi[order(-hi[,j]),]
          hi <- utils::head(hi, -(dim(hi)[1]-dim(lo)[1]))
        }
        if(dim(hi)[1]>dim(lo)[1]){
          lo <- input[input[,j]<= stats::median(input[,j]),]
          lo <- lo[order(lo[,j]),]
          lo <- utils::head(lo, -(dim(lo)[1]-dim(hi)[1]))
        }
        hi <- hi[,-j]
        lo <- lo[,-j]
      }
      if(match.arg(split)=="quartile"){
        hi <- input[input[,j]>= stats::quantile(input[,j],probs=.75),][,-j]
        lo <- input[input[,j]<= stats::quantile(input[,j],probs=.25),][,-j]
      }
      if((abs(dim(hi)[1]-dim(lo)[1])/dim(input)[1]) > 0.1) {message(colnames(input)[nodesTested[i]], ": Sample size difference after median split is >10% of total sample")}
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
      if((abs(dim(hi)[1]-dim(lo)[1])/dim(input)[1]) > 0.1) {message(colnames(input)[nodesTested[i]], ": Sample size difference after split is >10% of total sample")}
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

      ## Network structure invariance
      nwinv.lo[i] <- nw2[upper.tri(nw2)][nnet::which.is.max(diffedges.real[[i]][upper.tri(diffedges.real[[i]])])]
      nwinv.hi[i] <- nw1[upper.tri(nw1)][nnet::which.is.max(diffedges.real[[i]][upper.tri(diffedges.real[[i]])])]
      nwinv.edge[[i]] <- diffedges.edgelist[[i]][nnet::which.is.max(abs(diffedges.edgelist[[i]][,3])),]
      nwinv.real[i] <- as.numeric(abs(nwinv.edge[[i]][3]))
    }
  }
  nodenames <- colnames(input)[nodesTested]
  names(nwinv.real)<-nodenames

  ## What to return

  Network.Structure <- list()
  Network.Structure$impact <- nwinv.real
  Network.Structure$edge <- nwinv.edge
  Network.Structure$lo <- nwinv.lo
  Network.Structure$hi <- nwinv.hi
  names(Network.Structure$hi)<-names(Network.Structure$lo)<-names(Network.Structure$edge)<-names(input)[nodesTested]
  class(Network.Structure) <- "structure.impact"
  return(Network.Structure)
}
