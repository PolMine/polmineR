## General remarks

Many changes and performance improvements, but nothing that would change the
setup of the package and that would require your particular attention - as I 
conceive it.


## Test environments

* local OS X install, R 4.1.3
* GithubActions (Windows, macOS, Linux), R 4.2.0
* win-builder (devel and release)


## R CMD check results

There is a NOTE "installed size is  5.2Mb", resulting from 1.9Mb in the extdata
directory. I plan to externalize sample data in a future release, but this has
not yet been a priority.

There were no ERRORs or WARNINGs on the Linux / macOS / Windows environments I used. 


## Downstream dependencies

Not applicable.

