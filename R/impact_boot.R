#' Bootstrapping convenience function for impact statistics
#'
#' impact.boot is DEPRECATED. The function will be removed in the next update. Use impact.NCT instead.
#'
#' This function wraps the function \code{impact} and bootstraps to
#' provide confidence intervals of node impacts.
#'
#' This method is computationally intensive. It is recommended that users test a subset of nodes
#' at a time using the \code{nodes} argument, rather than testing all nodes simultaneously.
#'
#' @param input a matrix or data frame of observations (not a network/edgelist).
#' See included example datasets \code{\link{depression}} and \code{\link{social}}.
#' @param boots the number of times to bootstrap the impact function
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
#' @param progressbar Logical. Should the pbar be plotted in order to see the progress of the estimation procedure? Defaults to TRUE.
#'
#' @details
#'
#' \code{impact.boot} returns an object of class \code{impact.boot}, which includes confidence intervals.
#'
#' @return \code{\link{impact.boot}} returns a list of class "impact.boot"
#'
#' @export
impact.boot <-function(input, boots, gamma, nodes = c("all"), binary.data = FALSE, weighted = TRUE,
                       split=c("median","mean", "forceEqual", "cutEqual", "quartiles"), progressbar=TRUE) {
  if (missing(gamma)){
    if (binary.data){
      gamma <- 0.25
    } else {
      gamma <- 0.5
    }
  }
  ## Determine # of nodes tested
  if(nodes[1] == "all") {
    nodesTested <- 1:dim(input)[2]
  } else if(is.numeric(nodes)){
    nodesTested <- nodes
  } else if(is.character(nodes)){
    nodesTested <- match(nodes,names(input))
  }
  numNodesTested <- length(nodesTested)
  numNodes <- dim(input)[[2]]

  # create empty list
  edge <- list()
  glob <- matrix(rep(NA, boots*numNodesTested), boots, numNodesTested)
  struc <- matrix(rep(NA, boots*numNodesTested), boots, numNodesTested)

  # create progress bar
  pb <- utils::txtProgressBar(min = 0, max = boots, style = 3)

  # resample
  for(i in 1:boots){
    resamp <- input[sample(1:dim(input)[1], dim(input)[1], replace=TRUE),]
    imp <- impact(input=resamp, gamma=gamma, nodes=nodes, binary.data=binary.data,
                           weighted=weighted, split=split)
    edge[[i]] <- imp$Edge$impact
    glob[i,] <- imp$Global.Strength$impact
    struc[i,] <- imp$Network.Structure$impact
    if(progressbar){
      utils::setTxtProgressBar(pb, i)
    }
  }
  if(progressbar){close(pb)}

  ## Extract medians and confidence intervals

  ## Global strength
  glstr.est <- apply(glob, 2, stats::median)
  glstr.conf.lo <- apply(glob, 2, stats::quantile, probs=0.025)
  glstr.conf.hi <- apply(glob, 2, stats::quantile, probs=0.975)

  ## Network structure
  nwinv.est <- apply(struc, 2, stats::median)
  nwinv.conf.lo <- apply(struc, 2, stats::quantile, probs=0.025)
  nwinv.conf.hi <- apply(struc, 2, stats::quantile, probs=0.975)

  ## Edge
  ## edge.est: list, contains a matrix of medians for each node tested (much like edge.impact)
  ## edge.conf.lo: list, contains a matrix of low conf int for each node tested
  ## edge.conf.hi: list, contains a matrix of high conf int for each node tested
  edge.est <- edge.conf.lo <- edge.conf.hi <- replicate(numNodesTested, list(matrix(rep(NA, (numNodes-1)^2), (numNodes-1), (numNodes-1))))
  for(node in 1:numNodesTested){
  for(rowmat in 1:(numNodes-1)) {
  for(colmat in 1:(numNodes-1)) {
      sampl_dist <- as.numeric(lapply(edge, function(x) x[[node]][rowmat,colmat]))
      edge.est[[node]][rowmat,colmat] <- stats::median(sampl_dist)
      edge.conf.lo[[node]][rowmat,colmat] <- stats::quantile(sampl_dist, probs=0.025)
      edge.conf.hi[[node]][rowmat,colmat] <- stats::quantile(sampl_dist, probs=0.975)
      colnames(edge.est[[node]]) <- rownames(edge.est[[node]]) <- colnames(edge.conf.lo[[node]]) <- rownames(edge.conf.lo[[node]]) <- colnames(edge.conf.hi[[node]]) <- rownames(edge.conf.hi[[node]]) <- colnames(edge[[1]][[node]])
    }}}
  names(edge.est) <- names(edge.conf.lo) <- names(edge.conf.hi) <- names(edge[[1]])

  ## What to return

  res <- list()
  res$Global.Strength <- list()
  res$Global.Strength$estimate <- glstr.est
  res$Global.Strength$conf.lo <- glstr.conf.lo
  res$Global.Strength$conf.hi <- glstr.conf.hi
  names(res$Global.Strength$estimate)<-names(res$Global.Strength$conf.lo) <-names(res$Global.Strength$conf.hi)<- names(input)[nodesTested]

  res$Network.Structure <- list()
  res$Network.Structure$estimate <- nwinv.est
  res$Network.Structure$conf.lo <- nwinv.conf.lo
  res$Network.Structure$conf.hi <- nwinv.conf.hi
  names(res$Network.Structure$estimate)<-names(res$Network.Structure$conf.lo)<-names(res$Network.Structure$conf.hi)<-names(input)[nodesTested]

  res$Edge <- list()
  res$Edge$estimate <- edge.est
  res$Edge$conf.lo <- edge.conf.lo
  res$Edge$conf.hi <- edge.conf.hi

  class(res) <- "impact.boot"
  return(res)

}
