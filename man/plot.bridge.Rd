% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary_print_plot.R
\name{plot.bridge}
\alias{plot.bridge}
\title{Plot "bridge" objects}
\usage{
\method{plot}{bridge}(
  x,
  order = c("given", "alphabetical", "value"),
  zscore = FALSE,
  include,
  color = FALSE,
  colpalette = "Dark2",
  plotNA = FALSE,
  ...
)
}
\arguments{
\item{x}{an output object from \code{bridge} (class \code{bridge})}

\item{order}{"alphabetical" orders nodes alphabetically, "value" orders nodes from
highest to lowest centrality values}

\item{zscore}{logical. Converts raw impact statistics to z-scores for plotting}

\item{include}{a vector of centrality measures to include ("Bridge Strength", "Bridge Betweenness", "Bridge Closeness",
"Bridge Expected Influence (1-step)", "Bridge Expected Influence (2-step)"),
if missing all available measures will be plotted}

\item{color}{logical. Color each community separately in the plot?}

\item{colpalette}{A palette name from RColorBrewer, for coloring of axis labels}

\item{plotNA}{should nodes with NA values be included on the y axis?}

\item{...}{other plotting specifications in ggplot2 (aes)}
}
\description{
Convenience function for plotting bridge centrality
}
\details{
Inputting an object of class \code{bridge}
will return a line plot that shows the bridge centrality
values of each node
}
\examples{
\donttest{
b <- bridge(cor(depression))
plot(b)
plot(b, order="value", zscore=TRUE,include=c("Bridge Strength", "Bridge Betweenness"))
}
}
