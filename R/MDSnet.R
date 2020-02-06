#' MDSnet
#'
#' Convenience function for converting a qgraph object to a layout determined
#' by multidimensional scaling
#'
#' @param qgraph_net an object of type \code{qgraph}
#' @param type transformation function for MDS, defaults to "ordinal"
#' @param MDSadj to use a proximities matrix other than the adjacency matrix
#' stored in \code{qgraph_net}, provide it in this argument
#' @param stressTxt logical. Print the stress value in the lower left corner of the plot?
#' @param repulse logical. Add a small repulsion force with wordcloud package to avoid node overlap?
#' @param repulsion scalar for the repulsion force. Larger values add more repulsion
#' @param mdsArgs additional arguments in list format passed to \code{smacof::mds}
#' @param ... additional arguments passed to \code{qgraph}
#'
#' @details
#'
#' A network plotted with multidimensional scaling can be interpreted based on the distances
#' between nodes. Nodes close together represent closely associated nodes, whereas nodes that are far
#' apart represent unassociated or negatively associated nodes.
#'
#' @references
#'
#' Jones, P. J., Mair, P., & McNally, R. J. (2018). Visualizing psychological networks: A tutorial in R. Frontiers in Psychology, 9, 1742. https://doi.org/10.3389/fpsyg.2018.01742
#'
#' @export
MDSnet <- function(qgraph_net, type=c("ordinal", "interval", "ratio", "mspline"),
                   MDSadj=NULL, stressTxt=F, repulse=F, repulsion=1,
                   mdsArgs=list(), ...) {
  if(is.null(MDSadj)){
    adj <- qgraph::getWmat(qgraph_net)
  } else {
    adj <- MDSadj
  }
  mdsArgs <- c(mdsArgs, list(delta = smacof::sim2diss(adj), type=match.arg(type)))
  MDSfit <- do.call(what=smacof::mds,args=mdsArgs)
  if(!repulse){
    qgraph::qgraph(qgraph_net, layout=MDSfit$conf,...)
  } else {
    message("When repulsion is used, stress is underestimated. Compare to unrepulsed graph before interpreting")
    repulse <- repulseLayout(qgraph_net, layout=MDSfit$conf, repulsion=repulsion)
    qgraph::qgraph(qgraph_net, layout=repulse,...)
  }
  if(stressTxt){
    graphics::text(-1,-1, paste("Stress=", round(MDSfit$stress,2)))
  }
  invisible(MDSfit$conf)
}


#' PROCRUSTESnet
#'
#' Convenience function for simultaneously plotting two networks containing the same nodes.
#'
#' Each network's layout is determined by multidimensional scaling, and then the layouts
#' are brought into a similar space by using the Procrustes algorithm.
#'
#' @param qgraph_net1 an object of type \code{qgraph}
#' @param qgraph_net2 an object of type \code{qgraph}. Contains the same nodes as \code{qgraph_net2}
#' @param type1 transformation function for first MDS, defaults to "ordinal"
#' @param type2 transformation function for second MDS, defaults to the same as \code{type1}
#' @param MDSadj1 to use a proximities matrix other than the adjacency matrix
#' stored in \code{qgraph_net1}, provide it in this argument
#' @param MDSadj2 to use a proximities matrix other than the adjacency matrix
#' stored in \code{qgraph_net2}, provide it in this argument
#' @param stressTxt logical. Print the stress value in the lower left corner of the plots?
#' @param congCoef logical. Print the congruence coefficient fo the two layouts?
#' @param repulse logical. Add a small repulsion force with wordcloud package to avoid node overlap?
#' @param repulsion scalar for the repulsion force. Larger values add more repulsion
#' @param mdsArgs additional arguments in list format passed to \code{smacof::mds}
#' @param ... additional arguments passed to \code{qgraph}
#'
#' @details
#'
#' A network plotted with multidimensional scaling can be interpreted based on the distances
#' between nodes. Nodes close together represent closely associated nodes, whereas nodes that are far
#' apart represent unassociated or negatively associated nodes.
#'
#' The Procrustes algorithm brings the two layouts into a similar space through rotations and dilations
#' that do not impact the fit of the MDS solutions. In this implementation, the second network is rotated
#' and dilated to fit the first.
#'
#' @references
#'
#' Jones, P. J., Mair, P., & McNally, R. J. (2018). Visualizing psychological networks: A tutorial in R. Frontiers in Psychology, 9, 1742. https://doi.org/10.3389/fpsyg.2018.01742
#'
#' @export
PROCRUSTESnet <- function(qgraph_net1, qgraph_net2, type1=c("ordinal", "interval", "ratio", "mspline"), type2 = type1,
                       MDSadj1=NULL, MDSadj2=NULL, stressTxt=F, congCoef = F, repulse=F, repulsion=1,
                       mdsArgs=list(), ...){
  # Determine if MDSadj is used
  if(is.null(MDSadj1)){
    adj1 <- qgraph::getWmat(qgraph_net1)
  } else {
    adj1 <- MDSadj1
  }
  if(is.null(MDSadj2)){
    adj2 <- qgraph::getWmat(qgraph_net2)
  } else {
    adj2 <- MDSadj2
  }
  # Fit first MDS solution
  mdsArgs1 <- c(mdsArgs, list(delta = smacof::sim2diss(adj1), type=match.arg(type1)))
  MDSfit1 <- do.call(what=smacof::mds,args=mdsArgs1)
  # Fit second MDS solution
  mdsArgs2 <- c(mdsArgs, list(delta = smacof::sim2diss(adj2), type=match.arg(type2)))
  MDSfit2 <- do.call(what=smacof::mds,args=mdsArgs2)
  # Fit Procrustes
  proc_fit <- smacof::Procrustes(MDSfit1$conf, MDSfit2$conf)
  # Repulse
  if(repulse){
    layout1 <- repulseLayout(qgraph_net1, layout=proc_fit$X, repulsion=repulsion)
    layout2 <- repulseLayout(qgraph_net2, layout=proc_fit$Yhat, repulsion=repulsion)
  } else{
    layout1 <- proc_fit$X
    layout2 <- proc_fit$Yhat
  }
  # Plot
  op <- graphics::par(mfrow = c(1,2))
  qgraph::qgraph(qgraph_net1, layout=layout1,...)
  if(stressTxt){
    graphics::text(-1,-1, paste("Stress=", round(MDSfit1$stress,2)))
  }
  qgraph::qgraph(qgraph_net2, layout=layout2,...)
  if(stressTxt){
    graphics::text(-1,-1, paste("Stress=", round(MDSfit2$stress,2)))
  }
  if(congCoef){
    graphics::text(1,-1, paste("Congruence=", round(proc_fit$congcoef,2)))
  }
  graphics::par(op)

}


