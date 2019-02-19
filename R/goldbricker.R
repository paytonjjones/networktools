#' Goldbricker - Identifying redundant nodes in networks using compared correlations
#'
#' This function compares correlations in a psychometric network in order to identify nodes
#' which most likely measure the same underlying construct (i.e., are colinear)
#'
#' @param data a data frame consisting of n rows (participants) and j columns (variables)
#' @param p a p-value threshold for determining if correlation pairs are "significantly different"
#' @param method method for comparing correlations. See ?cocor.dep.groups.overlap for a full list
#' @param threshold variable pairs which have less than the threshold proportion of significantly different
#' correlations will be considered "bad pairs"
#' @param corMin the minimum zero-order correlation between two items to be considered "bad pairs". Items
#' that are uncorrelated are unlikely to represent the same underlying construct
#' @param progressbar logical. prints a progress bar in the console
#'
#' @details
#'
#' In a given psychometric network, two nodes may be redundantly measuring the same underlying construct. If this is the case,
#' the correlations between those two variables and all other variables should be highly similar. That is, they should correlate
#' to the same degree with other variables.
#'
#' The cocor package uses a p-value threshold to determine whether a pair of correlations to a third variable are
#' significantly different from each other. Goldbricker wraps the cocor package to compare every possible
#' combination of correlations in a psychometric network. It calculates the proportion of correlations which are
#' significantly different for each different pair of nodes.
#'
#' Using the threshold argument, one can set the proportion of correlations which is deemed "too low". All pairs of nodes
#' which fall below this threshold are returned as defined "bad pairs".
#'
#' Pairs can then be combined using the reduce_net function
#'
#' Note: to quickly change the threshold, one may simply enter an object of class "goldbricker" in the data argument, and change the threshold.
#' The p-value cannot be modified in the same fashion, as re-computation is necessary.
#'
#' @examples
#' \donttest{
#' gb_depression <- goldbricker(depression, threshold=0.5)
#'
#' reduced_depression <- net_reduce(data=depression, badpairs=gb_depression)
#'
#' ## Set a new threshold quickly
#' gb_depression_60 <- goldbricker(data=gb_depression, threshold=0.6)
#'
#'}
#' @return \code{\link{goldbricker}} returns a list of class "\code{goldbricker}" which contains:
#'
#' $proportion_matrix - a j x j matrix of proportions. Each proportion signifies the amount of significantly
#' different correlations between the given node pair (j x j)
#' $suggested_reductions - a vector of "bad pairs" (names) and their proportions (values)
#' $p - p value from input
#' $threshold - threshold from input
#'
#'
#'@export
goldbricker <- function(data, p=0.05, method="hittner2003", threshold=0.25, corMin=0.5, progressbar=TRUE) {
  if(method=="zou2007"){warning("zou2007 uses a confidence interval, argument \"p\" is ignored")}
  if(class(data)!="goldbricker"){
    cormat <- qgraph::cor_auto(data)
    n <- nrow(data)
    d <- dim(cormat)[1]
    if (progressbar==TRUE) pb <- txtProgressBar(max=d, style = 3)
    boolarray <- array(dim=c(d,d,d))
    combnames<- perc_reject <- matrix(as.numeric(),d,d)
    rownames(perc_reject) <- colnames(perc_reject) <- colnames(cormat)
    for(i in 1:d){
      if (progressbar==TRUE) setTxtProgressBar(pb, i)
      for(j in 1:d){
        for(k in 1:d){
          test <- switch(method,
                         hittner2003 = suppressWarnings(cocor::cocor.dep.groups.overlap(cormat[k,i], cormat[k,j], cormat[i, j], n, test="hittner2003"))@hittner2003$p.value,
                         zou2007 = ifelse(prod(suppressWarnings(cocor::cocor.dep.groups.overlap(.3, .4, .4, 30, test="zou2007"))@zou2007$'conf.int'[1:2])<0,1,0),
                         pearson1898 = suppressWarnings(cocor::cocor.dep.groups.overlap(cormat[k,i], cormat[k,j], cormat[i, j], n, test="pearson1898"))@pearson1898$p.value,
                         hotelling1940 = suppressWarnings(cocor::cocor.dep.groups.overlap(cormat[k,i], cormat[k,j], cormat[i, j], n, test="hotelling1940"))@hotelling1940$p.value,
                         williams1959 = suppressWarnings(cocor::cocor.dep.groups.overlap(cormat[k,i], cormat[k,j], cormat[i, j], n, test="williams1959"))@williams1959$p.value,
                         hendrickson1970 = suppressWarnings(cocor::cocor.dep.groups.overlap(cormat[k,i], cormat[k,j], cormat[i, j], n, test="hendrickson1970"))@hendrickson1970$p.value,
                         olkin1967 = suppressWarnings(cocor::cocor.dep.groups.overlap(cormat[k,i], cormat[k,j], cormat[i, j], n, test="olkin1967"))@olkin1967$p.value,
                         dunn1969 = suppressWarnings(cocor::cocor.dep.groups.overlap(cormat[k,i], cormat[k,j], cormat[i, j], n, test="dunn1969"))@dunn1969$p.value,
                         steiger1980= suppressWarnings(cocor::cocor.dep.groups.overlap(cormat[k,i], cormat[k,j], cormat[i, j], n, test="steiger1980"))@steiger1980$p.value,
                         meng1992 = suppressWarnings(cocor::cocor.dep.groups.overlap(cormat[k,i], cormat[k,j], cormat[i, j], n, test="meng1992"))@meng1992$p.value)
          if(is.na(test)){test_bool<-NA} else if(test < p){test_bool <- 1}else{test_bool <- 0}
          boolarray[i,j,k] <- as.numeric(test_bool)
        }
        rejectvec <- na.omit(boolarray[i,j,])
        perc_reject[i,j] <- sum(rejectvec)/length(rejectvec)
      }
    }
    diag(perc_reject) <- NA
  } else{
    perc_reject <- data$proportion_matrix
    cormat <- perc_reject
  }
  combnames <- matrix(as.numeric(),dim(perc_reject)[1],dim(perc_reject)[1])
  for(i in 1:dim(perc_reject)[1]){for(j in 1:dim(perc_reject)[1]){
    combnames[i,j] <- paste(colnames(perc_reject)[i], colnames(perc_reject)[j], sep=" & ")
  }}
  lower <- perc_reject[lower.tri(perc_reject)]
  lower_cor <- cormat[lower.tri(cormat)]
  names(lower) <- names(lower_cor) <- combnames[lower.tri(combnames)]
  suggested_reductions <- lower[lower<threshold&lower_cor>corMin][order(lower[lower<threshold&lower_cor>corMin])]
  if(length(suggested_reductions)==0){suggested_reductions <- "No suggested reductions"}
  res <- list(proportion_matrix=perc_reject,
              suggested_reductions=suggested_reductions,
              p=p,
              threshold=threshold)
  class(res) <- "goldbricker"
  cat("\n")
  return(res)
}


