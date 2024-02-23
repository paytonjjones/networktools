## Test environments

-   Windows Server (R-hub)
-   Fedora Linux (R-hub)
-   Ubuntu Linux (R-hub)
-   macOS Ventura 13.6 (local)

## R CMD Check results

On Windows, there were two NOTEs: \* checking for detritus in the temp
directory … NOTE Found the following files/directories:
‘lastMiKTeXException’ \* checking for detritus in the temp directory …
NOTE Found the following files/directories: ‘’NULL’’

On Ubuntu Linux, there was one NOTE: \* Skipping checking HTML
validation: no command ‘tidy’ found

These are known Rhub issues (<https://github.com/r-hub/rhub/issues/503>)

The package fails to install on R-hub Fedora Linux \* byte-compile and
prepare package for lazy loading ERROR: lazy loading failed for package
networktools

There is no further information about this failure in any of the logs. I
have not made any changes to the package that would conceivably affect a
Fedora installation, and am unsure how to debug further.

## Downstream dependencies

There are 3 downstream dependencies for this package (bootnet,
NetworkComparr, NetworkComparisonTest), which passed checks.
