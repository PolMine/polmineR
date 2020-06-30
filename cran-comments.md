## General remarks

The 'polmineR' package has been archived because a required update of the 'RcppCWB'
dependency failed  to pass tests I had not anticipated. As a response to my last re-submission, I received some final instructions about do's and dont's from Martina Schmirl. I address the issues with this re-submission.

(1.) Avoid using foo:::f - remaining usage has been removed, fixed.

(2.) Avoid using installed.packages(): One remaining usage has been replaced by using system.file(package = pkg).

(3.) No installation of packages without user's permission: To avoid confusion, I
removed installation instructions in the vignette where you may have seen some 
install.packages() calls (though in Rmarkdown chunks with eval = FALSE). There is a 
remaining usage in the polmineR() function. If a sugggested package 
required to run the shiny app within the package is missing, a user dialogue will ask
for the user's explicit consent to install a package.

I would be very relieved if polmineR could make it back to CRAN soon. The package
is used in a few university courses and I am getting requests where it has gone.-

Yet I do understand very well that CRAN has standards that my package violated and 
that you are investing a lot of effort for quality control. So thank you for your 
enduring effort to make CRAN what it is and for helping me to make polmineR
a better package.


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

