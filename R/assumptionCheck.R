#' Assumption Checking Function
#'
#' Checks some basic assumptions about the suitability of network analysis on
#' your data
#'
#' This function is in BETA. Please report any errors.
#'
#' @param data dataframe or matrix of observational data (rows: observations, columns: nodes)
#' @param type which assumptions to check? "network" tests the suitability for
#' network analysis in general. "impact" tests the suitability for analyzing impact
#' @param percent percent difference from grand mean that is acceptable when comparing variances.
#' @param split if type="impact", specifies the type of split to utilize
#'
#' @details
#'
#' Network analysis rests on several assumptions. Among these:
#' - Variance of each node is (roughly) equal
#' - Distributions are (roughly) normal
#'
#' Comparing networks in impact rests on additional assumptions including:
#' - Overall variances are (roughly) equal in each half
#'
#' This function checks these assumptions and notifies any violations.
#' This function is not intended as a substitute for careful data visualization
#' and independent assumption checks.
#'
#' See citations in the references section for further details.
#'
#' @references
#'
#' Terluin, B., de Boer, M. R., & de Vet, H. C. W. (2016). Differences in Connection Strength between Mental Symptoms Might Be Explained by Differences in Variance: Reanalysis of Network Data Did Not Confirm Staging. PLOS ONE, 11(11), e0155205. Retrieved from https://doi.org/10.1371/journal.pone.0155205
#'
#' @export
assumptionCheck <- function(data, type=c("network", "impact"), percent=20,
                            split=c("median","mean", "forceEqual", "cutEqual", "quartiles"),
                            plot=FALSE, binary.data=FALSE, na.rm=TRUE) {
if(match.arg(type)=="network"){
  varNodes <- apply(data, 2, stats::var, na.rm=na.rm)
  normNodes <- apply(data, 2, stats::shapiro.test)
  normNodes <- as.numeric(sapply(normNodes,`[`,2))
  varviolNodes <- vector()
  normviolNodes <- vector()
  if(is.null(colnames(data))) {colnames(data) <- 1:dim(data)[2]}
  names(normNodes) <- names(varNodes)<-colnames(data)
  grandMean <- mean(varNodes)
  for(i in 1:length(varNodes)) {
    if(abs((varNodes[i]/grandMean)-1)>(percent/100)) {varviolNodes[i] <- names(varNodes[i])}
    if(normNodes[i]<0.01) {normviolNodes[i] <- names(varNodes[i])}
  }
  varviolNodes <- stats::na.omit(varviolNodes)
  normviolNodes <- stats::na.omit(normviolNodes)
  if(length(varviolNodes != 0)) {message("Equal node variance violation: variance of the following nodes differs more than ",
                                percent, "% from grand mean: \n", paste0(varviolNodes, sep=" "), "\n")}
  if(length(normviolNodes != 0)) {message("Shapiro-Wilk violation (p<0.01): Visually check normality of nodes: \n",
                                          paste0(normviolNodes, sep=" "), "\n")}
  cat("Grand mean of variances = ", grandMean, "\n")
  cat("Node variances \n")
  print(varNodes)
  cat("\n Shapiro-Wilk Normality (p) \n")
  print(normNodes)
  if(plot){
  op <- graphics::par(mfrow=c(round((dim(data)[2]/3)+.5), 3))
  for(i in 1:dim(data)[2]) {graphics::hist(data[,i], main=NULL, xlab=colnames(data)[i], ylab=NULL)}
  graphics::par(op)}
}
if(match.arg(type)=="impact"){
  input <- na.omit(data)
  table <- as.data.frame(matrix(rep(NA, 2*dim(data)[2]), dim(data)[2], 2))
  for(j in 1:dim(data)[2]) {
    if(binary.data==FALSE) {
      if(match.arg(split)=="median"){
        hi <- input[input[,j]> stats::median(input[,j]),][,-j]
        lo <- input[input[,j] < stats::median(input[,j]),][,-j]
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
    table[j,1] <- sum(diag(stats::var(lo)))
    table[j,2] <- sum(diag(stats::var(hi)))
    if(abs((table[j,1]/table[j,2])-1)>(percent/100)) {message("Equal network variances violation: difference in variance between halves on node \"",
                                                         colnames(data)[j], "\" is greater than ", percent)}
  }
  colnames(table) <- c("Low Half (variance)", "High Half (variance)")
  rownames(table) <- colnames(data)
  cat("Total network variances \n")
  table
}
}


