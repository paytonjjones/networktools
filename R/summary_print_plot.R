#' @export
summary.global.impact<- function(object,...){
    cat("GLOBAL STRENGTH IMPACT OF EACH NODE \n\n")
  print(object$impact)
}

#' @export
summary.structure.impact <- function(object,...){
  cat("NETWORK STRUCTURE IMPACT OF EACH NODE \n\n")
  print(object$impact)
}

#' @export
summary.edge.impact <- function(object,...){
  cat("EDGE IMPACTS OF EACH NODE (HEAD) \n\n")
  cat("Edge impacts of", names(object$impact)[1], "\n\n")
  print(utils::head(object$impact[[1]]))
}

#' @export
summary.all.impact <- function(object,...){
  summary(object$Global.Strength)
  cat("\n")
  summary(object$Network.Structure)
  cat("\n")
  cat("EDGE IMPACTS COMPUTED? (Y/N): Y")
}

#' @export
summary.expectedInf <- function(object,...){
  if(is.list(object)){
    class(object$step1) <- class(object$step2) <- NULL
    cat("One-step Expected Influence \n\n")
    print(object$step1)
    cat("\nTwo-step Expected Influence \n\n")
    print(object$step2)
  } else{
    class(object) <- NULL
    cat("Expected Influence \n\n")
    print(object)
  }
}

#' @export
print.global.impact <- function(x,...){
  print(x[1:3])
}

#' @export
print.structure.impact <- function(x,...){
  new <- x[-2]
  print(new)
}

#' @export
print.edge.impact <- function(x,...){
  new <- x[[1]]
  print(new)
}

#' @export
print.all.impact <- function(x,...){
  cat("GLOBAL STRENGTH IMPACT \n")
  print(x$Global.Strength)
  cat("NETWORK STRUCTURE IMPACT \n")
  print(x$Network.Structure)
  cat("EDGE IMPACT \n")
  print(x$Edge)
}

#' @export
print.expectedInf <- function(x,...){
  if(is.list(x)){
  class(x$step1) <- class(x$step2) <- NULL
  cat("$out1 \n")
  print(x$step1)
  cat("$out2 \n")
  print(x$step2)
  } else{
    class(x) <- NULL
    print(x)
  }
}

#' Plot "all.impact" objects
#'
#' Convenience function for generating impact plots
#'
#' @param x an output object from an impact function (class \code{all.impact})
#' @param order "alphabetical" orders nodes alphabetically, "value" orders nodes from
#' highest to lowest impact value
#' @param zscore logical. Converts raw impact statistics to z-scores for plotting
#' @param abs_val logical. Plot absolute values of global strength impacts
#' @param ... other plotting specifications (ggplot2)
#'
#' @details
#'
#' Inputting an object of class \code{global.impact} or \code{structure.impact}
#' will return a line plot that shows the relative impacts of each node.
#' Inputting a \code{all.impact} object will return both of these plots simultaneously
#'
#' @examples
#' out1 <- impact(depression)
#' plot(out1)
#' plot(out1, order="value", zscore=TRUE)
#' @method plot all.impact
#' @export
plot.all.impact <- function(x, order=c("alphabetical", "value"), zscore = FALSE, abs_val=FALSE,...){
  df <- data.frame(names(x$Global.Strength$impact), x$Global.Strength$impact, x$Network.Structure$impact)
  colnames(df) <- c("NodeName", "Global Strength Impact", "Network Structure Impact")
  if(zscore) {df$`Global Strength Impact` <- scale(df$`Global Strength Impact`)
  df$`Network Structure Impact` <- scale(df$`Network Structure Impact`)}
  if(abs_val) {df$`Global Strength Impact` <- abs(df$`Global Strength Impact`)}
  longdf <- suppressWarnings(suppressMessages(reshape2::melt(df)))
  colnames(longdf) <- c("NodeName", "Impact.Type", "ImpValue")
  NodeName <- longdf[,1]; ImpValue <- longdf[,3]
  if(order[1]=="value") {
  gl <- longdf[longdf$Impact.Type== "Global Strength Impact",]
  ns <- longdf[longdf$Impact.Type== "Network Structure Impact",]
  gl <- gl[with(gl, order(gl$ImpValue)),]
  ns <- ns[with(ns, order(ns$ImpValue)),]
  gl$NodeName <- factor(as.character(gl$NodeName), levels = unique(as.character(gl$NodeName)[order(gl$ImpValue)]))
  ns$NodeName <- factor(as.character(ns$NodeName), levels = unique(as.character(ns$NodeName)[order(ns$ImpValue)]))
  g1 <- ggplot2::ggplot(gl, ggplot2::aes(x=ImpValue, y=NodeName, group=NA),...)
  g1 <- g1 + ggplot2::geom_path() + ggplot2::xlab("") + ggplot2::ylab("") + ggplot2::geom_point() +
    ggplot2::facet_grid(~Impact.Type, scales="free")
  g2 <- ggplot2::ggplot(ns, ggplot2::aes(x=ImpValue, y=NodeName, group=NA))
  g2 <- g2 + ggplot2::geom_path() + ggplot2::xlab("") + ggplot2::ylab("") + ggplot2::geom_point() +
    ggplot2::facet_grid(~Impact.Type, scales="free")
  gridExtra::grid.arrange(g1,g2, ncol=2)
  } else {
    longdf <- longdf[with(longdf, order(longdf$NodeName)),]
    g <- ggplot2::ggplot(longdf, ggplot2::aes(x=ImpValue, y=NodeName, group=NA))
    g <- g + ggplot2::geom_path() + ggplot2::xlab("") + ggplot2::ylab("") + ggplot2::geom_point() +
      ggplot2::facet_grid(~Impact.Type, scales="free") + ggplot2::scale_y_discrete(limits = rev(levels(longdf$NodeName)))
    return(g)
  }
}


#' Plot "global.impact" objects
#'
#' Convenience function for generating global strength impact plots
#'
#' @param x an output object from an impact function (class \code{global.impact})
#' @param order "alphabetical" orders nodes alphabetically, "value" orders nodes from
#' highest to lowest impact value
#' @param zscore logical. Converts raw impact statistics to z-scores for plotting
#' @param abs_val logical. Plot absolute values of global strength impacts

#' @param ... other plotting specifications (ggplot2)
#'
#' @details
#'
#' Inputting an object of class \code{global.impact}
#' will return a line plot that shows the relative global impacts of each node.
#'
#' @examples
#' out1 <- global.impact(depression)
#' plot(out1)
#' plot(out1, order="value", zscore=TRUE)
#' out2 <- impact(depression)
#' plot(out2$Global.Strength)
#' @method plot global.impact
#' @export
plot.global.impact <- function(x, order=c("alphabetical", "value"), zscore = FALSE,abs_val=FALSE,...) {
  df <- data.frame(names(x$impact), x$impact)
  colnames(df) <- c("NodeName", "ImpValue")
  NodeName <- df[,1]; ImpValue <- df[,2]
  if(zscore) {df$ImpValue <- scale(df$ImpValue)}
  if(abs_val) {df$ImpValue <- abs(df$ImpValue)}
  if(order[1]=="value") {
    df <- df[with(df, order(df$ImpValue)),]
    df$NodeName <- factor(as.character(df$NodeName), levels = unique(as.character(df$NodeName)[order(df$ImpValue)]))
    g <- ggplot2::ggplot(df, ggplot2::aes(x=ImpValue, y=NodeName, group=NA),...) + ggplot2::geom_path() +
      ggplot2::xlab("") + ggplot2::ylab("") + ggplot2::geom_point() +
      ggplot2::ggtitle("Global Strength Impact") + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    return(g)
  } else {
    df <- df[with(df, order(df$NodeName)),]
    df$NodeName <- factor(as.character(df$NodeName), levels = unique(as.character(df$NodeName)[order(df$NodeName)]))
    g <- ggplot2::ggplot(df, ggplot2::aes(x=ImpValue, y=NodeName, group=NA))
    g <- g + ggplot2::geom_path() + ggplot2::xlab("") + ggplot2::ylab("") +
      ggplot2::geom_point() + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::ggtitle("Global Strength Impact") + ggplot2::scale_y_discrete(limits = rev(levels(df$NodeName)))
    return(g)
  }

}


#' Plot "network.impact" objects
#'
#' Convenience function for generating network structure impact plots
#'
#' @param x an output object from an impact function (class \code{network.impact})
#' @param order "alphabetical" orders nodes alphabetically, "value" orders nodes from
#' highest to lowest impact value
#' @param zscore logical. Converts raw impact statistics to z-scores for plotting
#' @param ... other plotting specifications (ggplot2)
#'
#' @details
#'
#' Inputting an object of class \code{network.impact}
#' will return a line plot that shows the relative network impacts of each node.
#'
#' @examples
#' out1 <- global.impact(depression)
#' plot(out1)
#' plot(out1, order="value", zscore=TRUE)
#' out2 <- impact(depression)
#' plot(out2$Network.Structure)
#' @method plot structure.impact
#' @export
plot.structure.impact <- function(x, order=c("alphabetical", "value"), zscore = FALSE,...) {
  df <- data.frame(names(x$impact), x$impact)
  colnames(df) <- c("NodeName", "ImpValue")
  NodeName <- df[,1]; ImpValue <- df[,2]
  if(zscore) {df$ImpValue <- scale(df$ImpValue)}
  if(order[1]=="value") {
    df <- df[with(df, order(df$ImpValue)),]
    df$NodeName <- factor(as.character(df$NodeName), levels = unique(as.character(df$NodeName)[order(df$ImpValue)]))
    g <- ggplot2::ggplot(df, ggplot2::aes(x=ImpValue, y=NodeName, group=NA),...) + ggplot2::geom_path() +
      ggplot2::xlab("") + ggplot2::ylab("") + ggplot2::geom_point() +
      ggplot2::ggtitle("Network Structure Impact") + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    return(g)
  } else {
    df <- df[with(df, order(df$NodeName)),]
    df$NodeName <- factor(as.character(df$NodeName), levels = unique(as.character(df$NodeName)[order(df$NodeName)]))
    g <- ggplot2::ggplot(df, ggplot2::aes(x=ImpValue, y=NodeName, group=NA))
    g <- g + ggplot2::geom_path() + ggplot2::xlab("") + ggplot2::ylab("") +
      ggplot2::geom_point() + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::ggtitle("Network Structure Impact") + ggplot2::scale_y_discrete(limits = rev(levels(df$NodeName)))
    return(g)
  }

}


#' Plot "edge.impact" objects
#'
#' Convenience function for generating edge impact plots
#'
#' @param x an output object from an impact function (\code{edge.impact})
#' @param nodes specifies which impact graph(s) to be plotted.
#' Can be given as a character string of desired node(s) (e.g., c("node1","node2"))
#' or as a numeric vector of column numbers (e.g., c(1,2)).
#' @param type.edgeplot "contrast" returns two separate networks: one for low values of the
#' given node, and one for high values. "single" returns a network where edges represent
#' the edge impact of the given node.
#' @param title if not otherwise specified, title is automatically generated
#' @param ... other plotting specifications (qgraph)
#'
#' @details
#'
#' Inputting a \code{edge.impact} object will return network plots. Depending on the
#' \code{type.edgeplot} argument, two types of networks are possible. Using "contrast"
#' will return "true" estimated networks from the data, separated by a median split on the
#' selected node. Using "single" will return a network where the edges represent the edge
#' impacts for the selected node (e.g., thick positive edges represent a strong positive
#' edge impact)
#'
#' @examples
#' out1 <- edge.impact(depression)
#' plot(out1, nodes="concentration_problems")
#' plot(out1, nodes="psychomotor_retardation",
#'     type.edgeplot="single")
#'
#' out2 <- impact(depression)
#' plot(out2$Edge, nodes="concentration_problems")
#'
#' @method plot edge.impact
#' @export
plot.edge.impact <- function(x, nodes=c("first", "all"), type.edgeplot=c("contrast","single"), title=NULL,...) {
  if(nodes[1]=="first"){
    nodesTested <- 1
  } else if(nodes[1]=="all") {
    nodesTested <- 1:dim(x$impact)[2]
  } else if(is.numeric(nodes)){
    nodesTested <- nodes
  } else if(is.character(nodes)){
    nodesTested <- match(nodes,names(x$impact))
  }
  if(type.edgeplot[1]=="contrast") {
    for(i in nodesTested) {
      if(is.null(title)){
        title1 <- paste("Low values of", names(x$lo)[i])
        title2 <- paste("High values of", names(x$hi)[i])
      } else{title1 <- title[1]; title2 <- title[2]}
      op <- graphics::par(mfrow=c(length(nodesTested),2))
      qgraph::qgraph(x$lo[[i]], title = title1,...)
      qgraph::qgraph(x$hi[[i]], title = title2,...)
      graphics::par(op)
    }
  }
  if(type.edgeplot[1]=="single") {
    for(i in nodesTested) {
      if(is.null(title)){
        title <- paste("Edge impacts of", names(x$impact)[i])
      } else{title <- title}
      qgraph::qgraph(x$impact[[i]], title= title,...)
    }
  }

}


#' Plot "expectedInf" objects
#'
#' Convenience function for plotting expected influence
#'
#' @param x an output object from an \code{expectedInf} (class \code{expectedInf})
#' @param order "alphabetical" orders nodes alphabetically, "value" orders nodes from
#' highest to lowest impact value
#' @param zscore logical. Converts raw impact statistics to z-scores for plotting
#' @param ... other plotting specifications (ggplot2)
#'
#' @details
#'
#' Inputting an object of class \code{expectedInf}
#' will return a line plot that shows the relative one-step and/or two-step
#' expected influence of each node.
#'
#' @examples
#' myNetwork <- cor(depression)
#' out1 <- expectedInf(myNetwork)
#' summary(out1)
#' plot(out1)
#' plot(out1, order="value", zscore=TRUE)
#'
#' igraph_obj <- igraph::graph_from_adjacency_matrix(cor(depression))
#' ei_igraph <- expectedInf(igraph_obj)
#'
#' qgraph_obj <- qgraph::qgraph(cor(depression), plot=FALSE)
#' ei_qgraph <- expectedInf(qgraph_obj)
#'
#' Ising_adj_mat <- IsingFit::IsingFit(social, plot=FALSE)$weiadj
#' ei_Ising <- expectedInf(Ising_adj_mat)
#' plot(ei_Ising)
#'
#' @method plot expectedInf
#' @export
plot.expectedInf <- function(x, order=c("alphabetical", "value"), zscore = FALSE,...){
  if(is.list(x)){
  class(x$step1) <- class(x$step2) <- NULL
  df <- data.frame(names(x$step1), x$step1, x$step2)
  colnames(df) <- c("NodeName", "One-step Expected Influence", "Two-step Expected Influence")
  if(zscore) {df$`One-step Expected Influence` <- scale(df$`One-step Expected Influence`)
  df$`Two-step Expected Influence` <- scale(df$`Two-step Expected Influence`)}
  longdf <- suppressWarnings(suppressMessages(reshape2::melt(df)))
  colnames(longdf) <- c("NodeName", "Impact.Type", "ImpValue")
  NodeName <- longdf[,1]; ImpValue <- longdf[,3]
  if(order[1]=="value") {
    gl <- longdf[longdf$Impact.Type== "One-step Expected Influence",]
    ns <- longdf[longdf$Impact.Type== "Two-step Expected Influence",]
    gl <- gl[with(gl, order(gl$ImpValue)),]
    ns <- ns[with(ns, order(ns$ImpValue)),]
    gl$NodeName <- factor(as.character(gl$NodeName), levels = unique(as.character(gl$NodeName)[order(gl$ImpValue)]))
    ns$NodeName <- factor(as.character(ns$NodeName), levels = unique(as.character(ns$NodeName)[order(ns$ImpValue)]))
    g1 <- ggplot2::ggplot(gl, ggplot2::aes(x=ImpValue, y=NodeName, group=NA),...)
    g1 <- g1 + ggplot2::geom_path() + ggplot2::xlab("") + ggplot2::ylab("") + ggplot2::geom_point() +
      ggplot2::facet_grid(~Impact.Type, scales="free")
    g2 <- ggplot2::ggplot(ns, ggplot2::aes(x=ImpValue, y=NodeName, group=NA))
    g2 <- g2 + ggplot2::geom_path() + ggplot2::xlab("") + ggplot2::ylab("") + ggplot2::geom_point() +
      ggplot2::facet_grid(~Impact.Type, scales="free")
    gridExtra::grid.arrange(g1,g2, ncol=2)
  } else {
    longdf <- longdf[with(longdf, order(longdf$NodeName)),]
    g <- ggplot2::ggplot(longdf, ggplot2::aes(x=ImpValue, y=NodeName, group=NA))
    g <- g + ggplot2::geom_path() + ggplot2::xlab("") + ggplot2::ylab("") + ggplot2::geom_point() +
      ggplot2::facet_grid(~Impact.Type, scales="free") + ggplot2::scale_y_discrete(limits = rev(levels(longdf$NodeName)))
    return(g)
  }
  } else {
    class(x) <- NULL
    df <- data.frame(names(x), x)
    colnames(df) <- c("NodeName", "ImpValue")
    NodeName <- df[,1]; ImpValue <- df[,2]
    if(zscore) {df$ImpValue <- scale(df$ImpValue)}
    if(order[1]=="value") {
      df <- df[with(df, order(df$ImpValue)),]
      df$NodeName <- factor(as.character(df$NodeName), levels = unique(as.character(df$NodeName)[order(df$ImpValue)]))
      g <- ggplot2::ggplot(df, ggplot2::aes(x=ImpValue, y=NodeName, group=NA),...) + ggplot2::geom_path() +
        ggplot2::xlab("") + ggplot2::ylab("") + ggplot2::geom_point() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
      return(g)
    } else {
      df <- df[with(df, order(df$NodeName)),]
      df$NodeName <- factor(as.character(df$NodeName), levels = unique(as.character(df$NodeName)[order(df$NodeName)]))
      g <- ggplot2::ggplot(df, ggplot2::aes(x=ImpValue, y=NodeName, group=NA))
      g <- g + ggplot2::geom_path() + ggplot2::xlab("") + ggplot2::ylab("") +
        ggplot2::geom_point() + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
        ggplot2::scale_y_discrete(limits = rev(levels(df$NodeName)))
      return(g)
  }}
}

