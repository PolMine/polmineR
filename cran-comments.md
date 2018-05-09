## General remarks

After having published 'RcppCWB' as a portable backend to 'polmineR', this is the first release of the 'polmineR' package that can installed without complications on Windows machines.

The package includes some test data, and basic package tests. The dependency on a data package I hosted at a CRAN-like repository on one of my project servers ('europarl.en') could be dropped. In the next upcoming release, I plan to further reduce the number of dependencies.

The RcppCWB package I heavily rely on does not yet compile Solaris, so propably 'polmineR' will not work on Solaris either. Once I have resolved the issue with RcppCWB on Solaris, this issue should disappear.


## Test environments

* local OS X install, R 3.5.0
* Ubuntu 14.04 (on travis-ci), R 3.5.0
* Ubuntu 14.04 (project server), R 3.4.3
* win-builder (devel and release), R. 3.5.0


## R CMD check results

When building on Windows, you may see the following NOTE: "Package suggested but not available for checking: 'rcqp'"
The RcppCWB provides the main interface for querying corpora, so this is no impediment for Windows users. If 

Apart from this, there were no ERRORs, WARNINGs or NOTEs on the Linux / macOS / Windows environments I used. 




## Downstream dependencies

I have also checked downstream dependencies using devtools::revdep(),
without seeing ERRORs, WARNINGs, or NOTEs.

