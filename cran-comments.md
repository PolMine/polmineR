## General remarks

This is a minor release with many new small features and bug fixes. The general architecture of the package is unchanged. I release the package earlier than originally planned to bring it back to CRAN: The RcppCWB, a major dependency of polmineR, was archived for several hours, provoking polmineR to be removed, too.

## Test environments

* local OS X install, R 4.0.2
* Ubuntu 14.04 (on travis-ci), R 4.0.0
* Ubuntu 16.04 (project server), R 3.6.3
* win-builder (devel and release), R. 3.6.1
* Windows/AppVeyor, R 3.6.1 Patched
* Debian Linux, R-devel, GCC


## R CMD check results

There were no ERRORs, WARNINGs or NOTEs on the Linux / macOS / Windows environments I used. 


## Downstream dependencies

I have also checked downstream dependencies using devtools::revdep(),
without seeing ERRORs, WARNINGs, or NOTEs.

