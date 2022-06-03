## Test environments

-   Windows Server 2022, R-devel, 64 bit (R-hub)
-   Ubuntu Linux 20.04.1 LTS, R-release, GCC (R-hub)
-   Fedora Linux, R-devel, clang, gfortran (R-hub)
-   macOS Big Sur 11.6.5 (local)

## R CMD Check results

There were no ERRORS or WARNINGS

On Windows, there was one NOTE: \* checking for detritus in the temp
directory … NOTE Found the following files/directories:
‘lastMiKTeXException’

This is a known Rhub issue (<https://github.com/r-hub/rhub/issues/503>)

On Ubuntu Linux, Rhub classified the build as a PREPERROR, though this
may be a mistake. No apparent build error could be found in the logs and
an OK status was returned with no errors or warnings.

## Downstream dependencies

There are 3 downstream dependencies for this package (bootnet, BGGM,
NeworkComparisonTest), which passed checks.

Please note that prior to the present release, there was a circular
dependency with NetworkComparisonTest. This release removes that
circularity by removing NetworkComparisonTest from the networktools
imports.
