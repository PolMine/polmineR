## General remarks

Fedora R-devel checks indicated an encoding issue in a plot. I have resolved 
the underlying issue and checked all plots.

- I have also cleaned up warnings from tidy for HTML 
- moved examples into 'donttest'-blocks to avoid long check times
- I call setDTthreads(2L) in zzz.R to limit usage of cores to 2 on CRAN

To convey to users how to get full performance, I introduced a startup message.

## Test environments

* local OS X installation, R 4.3.1
* GithubActions (Windows, macOS, Linux), R 4.3.1
* win-builder (devel, release, oldrel)


## R CMD check results

There is a NOTE "installed size is  5.2Mb", resulting from 1.9Mb in the extdata
directory. I plan to externalize sample data in a future release, but this has
not yet been a priority.

There were no ERRORs or WARNINGs on the Linux / macOS / Windows environments I
used. 


## Downstream dependencies

Not applicable.

