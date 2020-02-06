#' Network Comparison Test for Impact Statistics
#'
#' This function wraps the function \code{NCT} from the \link{NetworkComparisonTest} package
#' to provide an explicit test for the significance of node impacts.
#'
#' The NCT method is computationally intensive. It is recommended that users test a subset of nodes
#' at a time using the \code{nodes} argument, rather than testing all nodes simultaneously.
#'
#' @param input a matrix or data frame of observations (not a network/edgelist).
#' See included example datasets \code{\link{depression}} and \code{\link{social}}.
#' @param it the number of iterations (permutations) in each network comparison test
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
#' (paired, AND, test.edges, edges, progressbar)
#' @param paired Logical. Can be TRUE of FALSE to indicate whether the samples are dependent or not. If paired is TRUE, relabeling is performed within each pair of observations. If paired is FALSE, relabeling is not restricted to pairs of observations. Note that, currently, dependent data is assumed to entail one group measured twice.
#' @param AND Logical. Can be TRUE of FALSE to indicate whether the AND-rule or the OR-rule should be used to define the edges in the network. Defaults to TRUE. Only necessary for binary data.
#' @param test.edges Logical. Can be TRUE of FALSE to indicate whether or not differences in individual edges should be tested.
#' @param edges Character or list. When 'all', differences between all individual edges are tested. When provided a list with one or more pairs of indices referring to variables, the provided edges are tested. A Holm-Bonferroni correction is applied to control for multiple testing.
#' @param progressbar Logical. Should the pbar be plotted in order to see the progress of the estimation procedure? Defaults to TRUE.
#'
#' @details
#'
#' In order to be interpreted in a meaningful way, the significance of impact statistics should be explicitly tested.
#'
#' The \link{NCT} function from the \link{NetworkComparisonTest} uses a permutation test to determine the significance
#' of structure invariances between two networks. Because impact statistics are mathematically defined as structural invariance between
#' two networks, \code{NCT} is an appropriate method to test the significance of impact statistics.
#'
#' \code{impact.NCT} returns an object of class \code{NCT}, which includes p-values for invariances.
#'
#' @examples
#' out <- impact.NCT(depression[,1:5], it=5, nodes="psychomotor_retardation")
#' \donttest{
#' NCT1 <- impact.NCT(depression, it=25, nodes="psychomotor_retardation")
#' NCT1$psychomotor_retardation$glstrinv.pval
#' NCT1$psychomotor_retardation$nwinv.pval
#' ## Both significant
#'
#' NCT2 <- impact.NCT(social, it=25, nodes="Kim", binary.data=TRUE)
#' NCT2$Kim$glstrinv.pval
#' NCT2$Kim$nwinv.pval
#' ## Only global strength impact is significant
#'
#' ##Note: for speed, 25 permutations are iterated here; more permutations are necessary in practice
#' }
#' @return \code{\link{impact}} returns a list where each element is an object of class \code{NCT}
#'
#' @export
impact.NCT <- function(input, it, gamma, nodes = c("all"), binary.data = FALSE, weighted = TRUE,
                   split=c("median","mean", "forceEqual", "cutEqual", "quartiles"),paired=FALSE,
                   AND=TRUE, test.edges=FALSE,edges,progressbar=TRUE) {
  if (missing(gamma)){
    if (binary.data){
      gamma <- 0.25
    } else {
      gamma <- 0.5
    }
  }

  ## Set up empty list
res <- list()

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
  }
    if(binary.data==TRUE)  {
      hi <- input[input[,j]==1,][,-j]
      lo <- input[input[,j]==0,][,-j]
      if(match.arg(split)=="cutEqual"){
        if(dim(hi)[1]<dim(lo)[1]){lo <- lo[-sample(1:dim(lo)[1], abs(dim(hi)[1]-dim(lo)[1])),]}
        if(dim(hi)[1]>dim(lo)[1]){hi <- hi[-sample(1:dim(hi)[1], abs(dim(hi)[1]-dim(lo)[1])),]}
      }
    }
  res[[i]] <- NetworkComparisonTest::NCT(hi, lo, gamma=gamma, it=it, binary.data=binary.data,
                                       paired=paired, weighted=weighted,AND=AND, test.edges=test.edges,
                                       edges=edges, progressbar=progressbar)
}
  names(res) <- names(input)[nodesTested]
  return(res)
}
