---
output:
  pdf_document: default
  html_document: default
---
# networktools 1.0.0

* Added a `NEWS.md` file to track changes to the package.

# networktools 1.1.0

* The bridge() function is now available. Can be used to calculate and plot "bridge statistics", 
  including bridge strength, bridge betweenness, bridge closeness, and bridge expected influence.
  
# networktools 1.1.1

* Bug fixes for bridge strength in the bridge() function
* The goldbricker() and net_reduce() functions are now available. Useful for comparing correlations 
  in order to eliminate nodes in networks which ostensibly measure the same construct

# networktools 1.1.2

* Additions to bridge() function: can use list input for communities, improved error handling
* New argument "corMin" in goldbricker function

# networktools 1.2.0

* Three new functions for plotting networks using MDS, PCA, and eigenmodels (MDSnet, PCAnet, EIGENnet)
* Added an argument in bridge() to average bridge centrality by number of nodes in communities

# networktools 1.2.1

* MDSnet, EIGENnet, & PCAnet now include an optional repulsion parameter. This allows the user to avoid any node overlap in plots. Additional arguments can now be passed to qgraph via ...
* The "average" argument in bridge has been changed to "normalize" & normalizes bridge centralities

# networktools 1.2.2

* Added PROCRUSTESnet function for plotting two networks with Procrustes-aligned MDS
* In plot.bridge, argument plotNA (default FALSE) now controls whether nodes with NA values are plotted
* Removed previously deprecated functions edge.impact, global.impact, and structure.impact
* Fixed a bug in impact.NCT where multiple booleans were passed to if statement

# networktools 1.2.3

* Fixed deprecated /donttest examples
