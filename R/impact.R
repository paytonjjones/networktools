#' Network Impact (combined function)
#'
#' Generates the global strength impact, network structure
#' impact, and edge impact simultaneously for a given set of nodes.
#'
#' @param input a matrix or data frame of observations (not a network/edgelist).
#' See included example datasets \code{\link{depression}} and \code{\link{social}}.
#' @param gamma the sparsity parameter used in generating networks. Defaults to 0.5 for interval data
#' and 0.25 for binary data
#' @param nodes indicates which nodes should be tested. Can be given
#' as a character string of desired nodes (e.g., c("node1","node2")) or as a numeric vector of
#' column numbers (e.g., c(1,2)).
#' @param binary.data logical. Indicates whether the input data is binary
#' @param weighted logical. Indicates whether resultant networks preserve edge weights or binarize edges.
#' @param split method by which to split network given non-binary data. "median": median split (excluding the median),
#' "mean": mean split, "forceEqual": creates equally sized groups by partitioning random median observations
#'  to the smaller group, "cutEqual": creates equally sized groups by deleting random values
#'  from the bigger group,"quartile": uses the top and bottom quartile as groups
#'
#' @details
#'
#' The structures of networks sometimes vary as a function of certain external variables.
#' For instance, Pe et al. (2015) found that the structure of mood networks varied
#' as a function of whether or not individuals had been diagnosed with major depression.
#'
#' The structures of networks may also vary as a function of \emph{internal} variables;
#' that is to say, as a function of each node. \strong{\emph{Impact statistics measure the degree to which
#' node levels impact network structure.}} Impact statistics are similar to centrality statistics
#' in the sense that they are a property of each node in a network.
#'
#' Three relevant impact statistics are included
#' in the \code{networktools} package: global strength impact, network structure
#' impact, and edge impact. To ease computational burden, all three statistics are calculated
#' simultaneously in the \code{\link{impact}} function.
#'
#' Impact statistics are calculated by temporarily regarding a node as an \emph{external} variable
#' to the network. The remaining data are then divided into two networks according to a median
#' split (default) on the external node. Network invariance measures are then computed on the two networks.
#' While median splits are not advisable when continuous analyses are possible, it is not possible
#' to compute networks in a continuous fashion.
#' The median split excludes observations that fall exactly on the median. In the case of binary data, data are split
#' by level rather than by median.
#'
#' @examples
#' out <- impact(depression[,1:3])
#' \donttest{
#' out1 <- impact(depression)
#' out2 <- impact(depression, gamma=0.65, nodes=c("sleep_disturbance", "psychomotor_retardation"))
#' out3 <- impact(social, binary.data=TRUE)
#' out4 <- impact(social, nodes=c(1:6, 9), binary.data=TRUE)
#'
#' summary(out1)
#' plot(out1)
#'
#'# Extract the impact of psychomotor_retardation on the
#'# edge that runs between worthlessness and fatigue
#' out1$Edge$impact[["psychomotor_retardation"]]["worthlessness", "fatigue"]
#'
#' # Extract edge impacts of node Dan in edgelist format
#' out3$Edge$edgelist$Dan
#'
#' # Visualize edge impacts of psychomotor_retardation
#' # as a single network
#' plot(out1$Edge, nodes="psychomotor_retardation", type.edgeplot="single")
#'
#' # Visualize the edge impacts of psychomotor_retardation
#' # as contrast between high and low
#' plot(out1$Edge, nodes="psychomotor_retardation", type.edgeplot="contrast")
#'
#'}
#' @return \code{\link{impact}} returns a list of class "\code{all.impact}" which contains:
#'
#' 1. A list of class "global.impact"
#'
#' 2. A list of class "structure.impact"
#'
#' 3. A list of class "edge.impact"
#'
#' @export
impact <- function(input, gamma, nodes = c("all"), binary.data = FALSE, weighted = TRUE,
                   split=c("median","mean", "forceEqual", "cutEqual", "quartiles")) {
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

  if(binary.data==FALSE) { ## If the data is non-binary, a graphical lasso is used
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
    catch1 <- try(suppressMessages(suppressWarnings(qgraph::EBICglasso(stats::cor(hi),nrow(hi),gamma=gamma, verbose=F))), silent = FALSE)
    catch2 <- try(suppressMessages(suppressWarnings(qgraph::EBICglasso(stats::cor(lo),nrow(lo),gamma=gamma, verbose=F))), silent = FALSE)
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

    ##### Invariance measures #####
    ## Global strength invariance
    glstr.lo[i] <- sum(abs(nw2[upper.tri(nw2)]))
    glstr.hi[i] <- sum(abs(nw1[upper.tri(nw1)]))
    glstrinv.real[i] <- sum(abs(nw1[upper.tri(nw1)]))-sum(abs(nw2[upper.tri(nw2)])) ## This is the global strength difference between the two (hi - lo)

    ## Individual edge invariance <-
    diffedges.real[[i]] <- (nw1-nw2) ## This calculates the differences by edge
    diffedges.edgelist[[i]] <- reshape2::melt(diffedges.real[[i]]) ## This turns the invariances into an edgelist
    edges.lo[[i]] <- nw2
    edges.hi[[i]] <- nw1

    ## Network structure invariance
    nwinv.lo[i] <- nw2[upper.tri(nw2)][nnet::which.is.max(diffedges.real[[i]][upper.tri(diffedges.real[[i]])])]
    nwinv.hi[i] <- nw1[upper.tri(nw1)][nnet::which.is.max(diffedges.real[[i]][upper.tri(diffedges.real[[i]])])]
    nwinv.edge[[i]] <- diffedges.edgelist[[i]][nnet::which.is.max(abs(diffedges.edgelist[[i]][,3])),]
    nwinv.real[i] <- as.numeric(abs(nwinv.edge[[i]][3]))
  }
}
  nodenames <- colnames(input)[nodesTested]
  names(nwinv.real)<-names(glstrinv.real)<-nodenames

  ## What to return

  res <- list()
  res$Global.Strength <- list()
  res$Global.Strength$impact <- glstrinv.real
  res$Global.Strength$lo <- glstr.lo
  res$Global.Strength$hi <- glstr.hi
  names(res$Global.Strength$lo)<-names(res$Global.Strength$hi) <- names(input)[nodesTested]
  class(res$Global.Strength) <- "global.impact"

  res$Network.Structure <- list()
  res$Network.Structure$impact <- nwinv.real
  res$Network.Structure$edge <- nwinv.edge
  res$Network.Structure$lo <- nwinv.lo
  res$Network.Structure$hi <- nwinv.hi
  names(res$Network.Structure$hi)<-names(res$Network.Structure$lo)<-names(res$Network.Structure$edge)<-names(input)[nodesTested]
  class(res$Network.Structure) <- "structure.impact"

  res$Edge <- list()
  res$Edge$impact <- diffedges.real
  res$Edge$lo <- edges.lo
  res$Edge$hi <- edges.hi
  res$Edge$edgelist <- diffedges.edgelist
  names(res$Edge$impact)<-names(res$Edge$edgelist)<-names(res$Edge$lo)<-names(res$Edge$hi)<-names(input)[nodesTested]
  class(res$Edge) <- "edge.impact"

  class(res) <- "all.impact"
  return(res)
}
