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
#' Jones, P. J., Mair, P., & McNally, R. J. (2017). Scaling networks for two-dimensional visualization: a tutorial. Retrieved from osf.io/eugsz
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


