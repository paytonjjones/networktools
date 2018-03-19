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
  if(length(object)==2){
    class(object$step1) <- class(object$step2) <- NULL
    cat("One-step Expected Influence \n\n")
    print(object$step1)
    cat("\nTwo-step Expected Influence \n\n")
    print(object$step2)
  } else if (names(object)[1]=="step1") {
    class(object$step1) <- NULL
    cat("One-step Expected Influence \n\n")
    print(object$step1)
  } else{
    class(object$step2) <- NULL
    cat("Two-step Expected Influence \n\n")
    print(object$step2)
  }
}

#' @export
summary.bridge <- function(object,...){
    class(object) <- NULL
    print(data.frame(object))
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
  x1 <- x
  class(x1) <- NULL
  print(x1)
}

#' @export
print.bridge <- function(x,...){
  class(x) <- NULL
  print(x)
}

#' @export
print.goldbricker <- function(x,...){
  cat("Suggested reductions: Less than", x$threshold*100, "% of correlations are significantly different for the following pairs: \n")
  print(x$suggested_reductions)
}

#' Plot "all.impact" objects
#'
#' Convenience function for generating impact plots
#'
#' @param x an output object from an impact function (class \code{all.impact})
#' @param order "alphabetical" orders nodes alphabetically, "value" orders nodes from
#' highest to lowest impact value
#' @param zscore logical. Converts raw impact statistics to z-scores for plotting
#' @param abs_val logical. Plot absolute values of global strength impacts.
#' If both abs_val=TRUE and zscore=TRUE, plots the absolute value of the z-scores.
#' @param ... other plotting specifications (ggplot2)
#'
#' @details
#'
#' Inputting an object of class \code{global.impact} or \code{structure.impact}
#' will return a line plot that shows the relative impacts of each node.
#' Inputting a \code{all.impact} object will return both of these plots simultaneously
#'
#' @examples
#' out <- impact(depression[,1:5])
#' plot(out)
#' \donttest{
#' out1 <- impact(depression)
#' plot(out1)
#' plot(out1, order="value", zscore=TRUE)
#' }
#' @method plot all.impact
#' @export
plot.all.impact <- function(x, order=c("given","value", "alphabetical"), zscore=FALSE, abs_val=FALSE,...){
  df <- data.frame(names(x$Global.Strength$impact), x$Global.Strength$impact, x$Network.Structure$impact)
  colnames(df) <- c("NodeName", "Global Strength Impact", "Network Structure Impact")
  if(zscore) {df$`Global Strength Impact` <- scale(df$`Global Strength Impact`)
  df$`Network Structure Impact` <- scale(df$`Network Structure Impact`)}
  if(abs_val) {df$`Global Strength Impact` <- abs(df$`Global Strength Impact`)
  df$`Network Structure Impact` <- abs(df$`Network Structure Impact`)}
  longdf <- suppressWarnings(suppressMessages(reshape2::melt(df)))
  colnames(longdf) <- c("NodeName", "Impact.Type", "ImpValue")
  NodeName <- longdf[,1]; ImpValue <- longdf[,3]
  if(order[1]=="given"){
    gl <- longdf[longdf$Impact.Type== "Global Strength Impact",]
    ns <- longdf[longdf$Impact.Type== "Network Structure Impact",]
    gl$NodeName <- factor(as.character(gl$NodeName), levels = unique(as.character(gl$NodeName)))
    ns$NodeName <- factor(as.character(ns$NodeName), levels = unique(as.character(ns$NodeName)))
    gl$NodeName <- factor(gl$NodeName, levels=rev(levels(gl$NodeName)))
    ns$NodeName <- factor(ns$NodeName, levels=rev(levels(ns$NodeName)))
    g1 <- ggplot2::ggplot(gl, ggplot2::aes(x=ImpValue, y=NodeName, group=NA),...)
    g1 <- g1 + ggplot2::geom_path() + ggplot2::xlab("") + ggplot2::ylab("") + ggplot2::geom_point() +
      ggplot2::facet_grid(~Impact.Type, scales="free")
    g2 <- ggplot2::ggplot(ns, ggplot2::aes(x=ImpValue, y=NodeName, group=NA))
    g2 <- g2 + ggplot2::geom_path() + ggplot2::xlab("") + ggplot2::ylab("") + ggplot2::geom_point() +
      ggplot2::facet_grid(~Impact.Type, scales="free")
    gridExtra::grid.arrange(g1,g2, ncol=2)
  } else if(order[1]=="value") {
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
#' @param abs_val logical. Plot absolute values of global strength impacts.
#' If both abs_val=TRUE and zscore=TRUE, plots the absolute value of the z-scores.

#' @param ... other plotting specifications (ggplot2)
#'
#' @details
#'
#' Inputting an object of class \code{global.impact}
#' will return a line plot that shows the relative global impacts of each node.
#'
#' @examples
#' out <- global.impact(depression[,1:5])
#' plot(out)
#' \donttest{
#' out1 <- global.impact(depression)
#' plot(out1)
#' plot(out1, order="value", zscore=TRUE)
#' out2 <- impact(depression)
#' plot(out2$Global.Strength)
#' }
#' @method plot global.impact
#' @export
plot.global.impact <- function(x, order=c("given","value","alphabetical"), zscore=FALSE,abs_val=FALSE,...) {
  df <- data.frame(names(x$impact), x$impact)
  colnames(df) <- c("NodeName", "ImpValue")
  NodeName <- df[,1]; ImpValue <- df[,2]
  if(zscore) {df$ImpValue <- scale(df$ImpValue)}
  if(abs_val) {df$ImpValue <- abs(df$ImpValue)}
  if(order[1]=="given") {
    df$NodeName <- factor(as.character(df$NodeName), levels = unique(as.character(df$NodeName)))
    df$NodeName <- factor(df$NodeName, levels=rev(levels(df$NodeName)))
    g <- ggplot2::ggplot(df, ggplot2::aes(x=ImpValue, y=NodeName, group=NA),...) + ggplot2::geom_path() +
      ggplot2::xlab("") + ggplot2::ylab("") + ggplot2::geom_point() +
      ggplot2::ggtitle("Global Strength Impact") + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    return(g)
  } else if(order[1]=="value") {
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


#' Plot "structure.impact" objects
#'
#' Convenience function for generating network structure impact plots
#'
#' @param x an output object from an impact function (class \code{structure.impact})
#' @param order "alphabetical" orders nodes alphabetically, "value" orders nodes from
#' highest to lowest impact value
#' @param zscore logical. Converts raw impact statistics to z-scores for plotting
#' @param abs_val logical. Plot absolute values of network structure impacts.
#' If both abs_val=TRUE and zscore=TRUE, plots the absolute value of the z-scores.

#' @param ... other plotting specifications (ggplot2)
#'
#' @details
#'
#' Inputting an object of class \code{network.impact}
#' will return a line plot that shows the relative network impacts of each node.
#'
#' @examples
#' out <- structure.impact(depression[,1:5])
#' plot(out)
#' \donttest{
#' out1 <- structure.impact(depression)
#' plot(out1)
#' plot(out1, order="value", zscore=TRUE)
#' out2 <- impact(depression)
#' plot(out2$Network.Structure)
#' }
#' @method plot structure.impact
#' @export
plot.structure.impact <- function(x, order=c("given","alphabetical", "value"),zscore=FALSE,abs_val=FALSE,...) {
  df <- data.frame(names(x$impact), x$impact)
  colnames(df) <- c("NodeName", "ImpValue")
  NodeName <- df[,1]; ImpValue <- df[,2]
  if(zscore) {df$ImpValue <- scale(df$ImpValue)}
  if(abs_val) {df$ImpValue <- abs(df$ImpValue)}
  if(order[1]=="given") {
    df$NodeName <- factor(as.character(df$NodeName), levels = unique(as.character(df$NodeName)))
    df$NodeName <- factor(df$NodeName, levels=rev(levels(df$NodeName)))
    g <- ggplot2::ggplot(df, ggplot2::aes(x=ImpValue, y=NodeName, group=NA),...) + ggplot2::geom_path() +
      ggplot2::xlab("") + ggplot2::ylab("") + ggplot2::geom_point() +
      ggplot2::ggtitle("Network Structure Impact") + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    return(g)
  } else if(order[1]=="value") {
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
#' @param maximum sets a maximum for edge thickness (see maximum argument in ?qgraph).
#' "auto" uses the maximum edge from the collective two networks.
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
#' out <- edge.impact(depression[450:550,1:3])
#' plot(out, nodes="anhedonia", type.edgeplot="single")
#' \donttest{
#' out1 <- edge.impact(depression)
#' plot(out1, nodes="concentration_problems")
#' plot(out1, nodes="psychomotor_retardation",
#'     type.edgeplot="single")
#'
#' out2 <- impact(depression)
#' plot(out2$Edge, nodes="concentration_problems")
#'}
#' @method plot edge.impact
#' @export
plot.edge.impact <- function(x, nodes=c("first", "all"), type.edgeplot=c("contrast","single"), title=NULL, maximum="auto",...) {
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
      if(maximum=="auto"){max <- max(max(x$lo[[i]]), max(x$hi[[i]]))
      }else {max <- maximum}
      op <- graphics::par(mfrow=c(length(nodesTested),2))
      qgraph::qgraph(x$lo[[i]], title = title1,maximum=max,...)
      qgraph::qgraph(x$hi[[i]], title = title2,maximum=max,...)
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
#' \dontshow{myNetwork <- cor(depression[,1:5])}
#' out1 <- expectedInf(myNetwork)
#' plot(out1$step1)
#' \donttest{
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
#'}
#' @method plot expectedInf
#' @export
plot.expectedInf <- function(x, order=c("given","alphabetical", "value"), zscore=TRUE,...){
  if(is.list(x)){
  class(x$step1) <- class(x$step2) <- NULL
  if(is.null(names(x$step1))) {names(x$step1) <- 1:length(x$step1)}
  df <- data.frame(names(x$step1), x$step1, x$step2)
  colnames(df) <- c("NodeName", "One-step Expected Influence", "Two-step Expected Influence")
  if(zscore) {df$`One-step Expected Influence` <- scale(df$`One-step Expected Influence`)
  df$`Two-step Expected Influence` <- scale(df$`Two-step Expected Influence`)}
  longdf <- suppressWarnings(suppressMessages(reshape2::melt(df)))
  colnames(longdf) <- c("NodeName", "Impact.Type", "ImpValue")
  NodeName <- longdf[,1]; ImpValue <- longdf[,3]
  if(order[1]=="given"){
    gl <- longdf[longdf$Impact.Type== "One-step Expected Influence",]
    ns <- longdf[longdf$Impact.Type== "Two-step Expected Influence",]
    gl$NodeName <- factor(as.character(gl$NodeName), levels = unique(as.character(gl$NodeName)))
    ns$NodeName <- factor(as.character(ns$NodeName), levels = unique(as.character(ns$NodeName)))
    gl$NodeName <- factor(gl$NodeName, levels=rev(levels(gl$NodeName)))
    ns$NodeName <- factor(ns$NodeName, levels=rev(levels(ns$NodeName)))
    g1 <- ggplot2::ggplot(gl, ggplot2::aes(x=ImpValue, y=NodeName, group=NA),...)
    g1 <- g1 + ggplot2::geom_path() + ggplot2::xlab("") + ggplot2::ylab("") + ggplot2::geom_point() +
      ggplot2::facet_grid(~Impact.Type, scales="free")
    g2 <- ggplot2::ggplot(ns, ggplot2::aes(x=ImpValue, y=NodeName, group=NA))
    g2 <- g2 + ggplot2::geom_path() + ggplot2::xlab("") + ggplot2::ylab("") + ggplot2::geom_point() +
      ggplot2::facet_grid(~Impact.Type, scales="free")
    gridExtra::grid.arrange(g1,g2, ncol=2)
  } else if(order[1]=="value") {
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
    if(order[1]=="given"){
      df$NodeName <- factor(as.character(df$NodeName), levels = unique(as.character(df$NodeName)))
      df$NodeName <- factor(df$NodeName, levels=rev(levels(df$NodeName)))
      g <- ggplot2::ggplot(df, ggplot2::aes(x=ImpValue, y=NodeName, group=NA),...) + ggplot2::geom_path() +
        ggplot2::xlab("") + ggplot2::ylab("") + ggplot2::geom_point() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
      return(g)
    } else if(order[1]=="value") {
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

#' Plot "bridge" objects
#'
#' Convenience function for plotting bridge centrality
#'
#' @param x an output object from \code{bridge} (class \code{bridge})
#' @param order "alphabetical" orders nodes alphabetically, "value" orders nodes from
#' highest to lowest centrality values
#' @param zscore logical. Converts raw impact statistics to z-scores for plotting
#' @param include a vector of centrality measures to include ("Bridge Strength", "Bridge Betweenness", "Bridge Closeness",
#' "Bridge Expected Influence (1-step)", "Bridge Expected Influence (2-step)"),
#' if missing all available measures will be plotted
#' @param RColorBrewer A palette name from RColorBrewer, for coloring of axis labels
#' @param ... other plotting specifications in ggplot2 (aes)
#'
#' @details
#'
#' Inputting an object of class \code{bridge}
#' will return a line plot that shows the bridge centrality
#' values of each node
#'
#' @examples
#'\donttest{
#' b <- bridge(cor(depression))
#' plot(b)
#' plot(b, order="value", zscore=TRUE)
#' plot(b, include=c("Bridge Strength", "Bridge Betweenness"))
#'}
#' @method plot bridge
#' @export
plot.bridge <- function(x, order=c("given","alphabetical", "value"), zscore=FALSE, include, RColorBrewer="Dark2", ...){
  attr(x, "class") <- NULL
  nodes <- names(x[[1]])
  comm <- x$communities; commcol <- vector(); pal <- brewer.pal(max(length(unique(comm)), 3), RColorBrewer)
  for(i in 1:length(unique(comm))){commcol[i] <- pal[i]}
  cols <- commcol[match(comm, unique(comm))]
  x$communities <- NULL
  if(zscore) {
    scalenoatt <- function(y){
      y <- scale(y)
      attr(y, "scaled:center") <- NULL
      attr(y, "scaled:scale") <- NULL
      return(y)
    }
    x <- sapply(x, scalenoatt)
  }
  Long <- reshape2::melt(x); colnames(Long)[2] <- "measure"
  Long$type <- rep(NA, nrow(Long))
  Long$node <- rep(nodes, length(unique(Long$measure)))
  if (missing(include)) {
    include <- unique(Long$measure[Long$measure != "communities"])
  }
  Long <- subset(Long, Long$measure %in% include)
  if(order[1]=="given"){
    Long$node <- factor(as.character(Long$node), levels = rev(unique(as.character(Long$node))))
    g <- ggplot2::ggplot(Long, ggplot2::aes_string(x = 'value', y = 'node', group = 'type', ...))
    g <- g + ggplot2::geom_path() + ggplot2::xlab("") + ggplot2::ylab("") + ggplot2::geom_point()
    g <- g + ggplot2::facet_grid('~measure', scales = "free") +
      ggplot2::theme(axis.text.y = ggplot2::element_text(colour=rev(cols)))
  } else if(order[1]=="alphabetical"){
    Long <- Long[with(Long, order(Long$node)),]
    Long$node <- factor(as.character(Long$node), levels = unique(as.character(Long$node)[order(Long$node)]))
    g <- ggplot2::ggplot(Long, ggplot2::aes_string(x='value', y='node', group='type', ...))
    g <- g + ggplot2::geom_path() + ggplot2::geom_point() + ggplot2::xlab("") + ggplot2::ylab("") +
      ggplot2::facet_grid('~measure', scales="free") + ggplot2::scale_y_discrete(limits = rev(levels(Long$node))) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(colour=cols[order(nodes, decreasing=T)]))
  } else if(order[1]=="value") {
    glist <- list()
    for(i in 1:length(include)) {
      temp_Long <- Long[Long$measure==include[i],]
      temp_Long <- temp_Long[with(temp_Long, order(temp_Long$value)),]
      temp_Long$node <- factor(as.character(temp_Long$node), levels = unique(as.character(temp_Long$node)[order(temp_Long$value)]))
      glist[[i]] <- ggplot2::ggplot(temp_Long, ggplot2::aes_string(x='value', y='node', group='type',...)) +
        ggplot2::geom_path() + ggplot2::geom_point() + ggplot2::xlab("") + ggplot2::ylab("") +
        ggplot2::facet_grid('~measure', scales="free") +
        ggplot2::theme(axis.text.y = ggplot2::element_text(colour=cols[order(temp_Long$value, decreasing=T)]))
    }
    if(length(include)==1){g <- gridExtra::grid.arrange(glist[[1]])
    } else if(length(include)==2){gridExtra::grid.arrange(glist[[1]],glist[[2]], ncol=2)
    } else if(length(include)==3){gridExtra::grid.arrange(glist[[1]],glist[[2]],glist[[3]], ncol=3)
    } else if(length(include)==4){gridExtra::grid.arrange(glist[[1]],glist[[2]],glist[[3]],glist[[4]], ncol=4)
    } else if(length(include)==5){gridExtra::grid.arrange(glist[[1]],glist[[2]],glist[[3]],glist[[4]],glist[[5]], ncol=5)
    }
  }
  if(order[1]!="value") { # if "value", plotting is already done with gridExtra (552-557)
      return(g)
    }
}

#' @export
plot.goldbricker <- function(x,...){
  X <- Y <- Proportion_Unique <- NULL
  mat <- x$proportion_matrix
  xd <- yd <-  colnames(mat)
  data <- expand.grid(X=xd, Y=yd)
  data$Proportion_Unique <- as.vector(mat)
  ggplot2::ggplot(data, ggplot2::aes(X, Y, z= Proportion_Unique)) + ggplot2::geom_tile(ggplot2::aes(fill = Proportion_Unique)) +
    ggplot2::theme_bw() +
    ggplot2::scale_fill_gradient(low="red", high="white") +
    ggplot2::ggtitle("Proportion of Unique Correlations")
}


