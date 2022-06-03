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
#' myNetwork <- cor(depression[,1:5])
#' out1 <- expectedInf(myNetwork)
#' plot(out1$step1)
#' \donttest{
#' plot(out1, order="value", zscore=TRUE)
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
#' @param color logical. Color each community separately in the plot?
#' @param colpalette A palette name from RColorBrewer, for coloring of axis labels
#' @param plotNA should nodes with NA values be included on the y axis?
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
#' plot(b, order="value", zscore=TRUE,include=c("Bridge Strength", "Bridge Betweenness"))
#'}
#' @method plot bridge
#' @export
plot.bridge <- function(x, order=c("given","alphabetical", "value"), zscore=FALSE,
                        include, color=FALSE, colpalette="Dark2", plotNA=FALSE, ...){
  attr(x, "class") <- NULL
  if(!plotNA){
    if(TRUE%in%is.na(x[[1]])){
      message("Missing values have been removed from the plot, set plotNA = TRUE to show missing values")
      x<-lapply(x, function(x){x[!is.na(x)]})
      if(length(x[[1]])==0){warning("No values remain after removing missing values")}
    }
  }
  nodes <- names(x[[1]])
  comm <- x$communities; commcol <- vector()
  if(color) {pal <- RColorBrewer::brewer.pal(max(length(unique(comm)), 3), colpalette)
    for(i in 1:length(unique(comm))){commcol[i] <- pal[i]}
    cols <- commcol[match(comm, unique(comm))]
  } else {
    cols <- rep("black", length(comm))
  }
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
      temp_Long_orig <- Long[Long$measure==include[i],]
      temp_Long <- temp_Long_orig[with(temp_Long_orig, order(temp_Long_orig$value)),]
      temp_Long$node <- factor(as.character(temp_Long$node), levels = unique(as.character(temp_Long$node)[order(temp_Long$value)]))
      glist[[i]] <- ggplot2::ggplot(temp_Long, ggplot2::aes_string(x='value', y='node', group='type',...)) +
        ggplot2::geom_path() + ggplot2::geom_point() + ggplot2::xlab("") + ggplot2::ylab("") +
        ggplot2::facet_grid('~measure', scales="free") +
        ggplot2::theme(axis.text.y = ggplot2::element_text(colour=cols[order(temp_Long_orig$value)]))
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


