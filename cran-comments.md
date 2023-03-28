## General remarks

This release catches up with markdown >= 1.3 such that markdown maintainers
will not have to rely on workarounds to ensure that markdown improvements 
to not break polmineR reverse dependency tests.


## Test environments

* local OS X install, R 4.2.2
* GithubActions (Windows, macOS, Linux), R 4.2.3
* win-builder (devel, release, oldrel)


## R CMD check results

There is a NOTE "installed size is  5.2Mb", resulting from 1.9Mb in the extdata
directory. I plan to externalize sample data in a future release, but this has
not yet been a priority.

There were no ERRORs or WARNINGs on the Linux / macOS / Windows environments I
used. 


## Downstream dependencies

Not applicable.

