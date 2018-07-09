## General remarks

This release addresses an issue raised by Kurt Hornik concerning 'RcppCWB' that occurs with 'polmineR', too: A required path within a 'registry'-file within the package is not adjusted by the configure/configure.win when installing a binary package. The previous version did the adjustment during .onLoad() mechanism, changing a file in the package directory -- thus violating CRAN requirements. The new version of polmineR uses a temporary session registry .

The 'RcppCWB' dependency does not yet compile Solaris, so 'polmineR' will not work on Solaris. Once I have resolved the issue with 'RcppCWB' on Solaris, this issue should disappear.


## Test environments

* local OS X install, R 3.5.0
* Ubuntu 14.04 (on travis-ci), R 3.5.0
* Ubuntu 14.04 (project server), R 3.4.3
* win-builder (devel and release), R. 3.5.0


## R CMD check results

There were no ERRORs, WARNINGs or NOTEs on the Linux / macOS / Windows environments I used. 
## Downstream dependencies

I have also checked downstream dependencies using devtools::revdep(),
without seeing ERRORs, WARNINGs, or NOTEs.

