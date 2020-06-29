## General remarks

The 'polmineR' package has been archived because a required update of the 'RcppCWB'
dependency failed  to pass tests I had not anticipated. Upon resubmission, 
Swetlana Herbrandt identified the following issues that had gone unnoticed before:

(1.) Avoid calling the package a "library" in Description: Fixed.
(2.) Avoid cat(): Fixed - I have replaced any remaining call by using message().
(3.) Avoid changing options: Fixed - using on.exit() right after changing an option if necessary.
(4.) Avoid changing the user's home filespace: Actually I was not aware any
function might still do that. To be sure, I declared the (outdated) methods "mail" and 
"store" as defunct. File operations are limited to the temporary session directory.
(5.) Avoid using installed.packages(): Fixed by using system.file(package = pkg).
(6.) No installation of packages without user's permission: There is an auxiliary 
function .conditional_install() within the polmineR() function that asks for the
user's consent before installing anything and that aborts of not in interactive 
session.

I would be very relieved if polmineR could make it back to CRAN soon. The package
is used in a few university courses and I am getting requests where it has gone...

Anyway: I do understand very well that CRAN has standards that my package violated and 
that you are investing a lot of effort for quality control. So thank you for your 
enduring effort to make CRAN what it is.


## Test environments

* local OS X install, R 4.0.2
* Ubuntu 14.04 (on travis-ci), R 4.0.0
* Ubuntu 16.04 (project server), R 3.6.3
* win-builder (devel and release), R. 4.0.2
* Windows/AppVeyor, R 4.0.2 Patched
* Debian Linux, R-devel, GCC


## R CMD check results

There were no ERRORs, WARNINGs or NOTEs on the Linux / macOS / Windows environments I used. 


## Downstream dependencies

I have also checked downstream dependencies using devtools::revdep(),
without seeing ERRORs, WARNINGs, or NOTEs.

