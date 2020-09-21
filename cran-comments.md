## General remarks

This release includes a few bug fixes to address issues encountered by users.


## Test environments

* local OS X install, R 4.0.2
* Ubuntu Ubuntu 16.04.6 LTS (on travis-ci), R 4.0.2
* win-builder (devel and release), R. 4.0.2
* Windows/AppVeyor, R 4.0.2 Patched


## R CMD check results

I see the NOTE "checking for future file timestamps ... unable to verify current time". I gather it is an known issue with the availability of worldclockapi.com (see https://stackoverflow.com/questions/63613301/r-cmd-check-note-unable-to-verify-current-time).

There is a NOTE "installed size is  5.2Mb", resulting from 1.9Mb in the extdata directory. I plan to externalize sample data in a future release, but this has not yet been a priority.

There were no ERRORs or WARNINGs on the Linux / macOS / Windows environments I used. 


## Downstream dependencies

I have also checked downstream dependencies using devtools::revdep(),
without seeing ERRORs, WARNINGs, or NOTEs.

