## General remarks

Having reworked the class systems thoroughly, this is a major release. But as the changes are pure R changes documented in the NEWS file, nothing noteworthy for the CRAN team, I think.

Yet this is what I should address here: An issue that may arise with R 4.0 has been fixed. Kurt Hornik notified me that the in the upcoming R version (4.0), the 'matrix' class will inherit from the 'array' class, thus causing a error when running for instance: class(matrix(1 : 4, 2, 2)) == "matrix". I modified the code such that polmineR will comply with the upcoming changes in R-devel / R 4.0. 

I am grateful for the foresight of the CRAN team and the precise advice I received!

P.S.: This is the second take to submit polmineR v0.8.0. Flaws in the URLs linked
pointed out by Uwe Ligges have been removed.

## Test environments

* local OS X install, R 4.0.0
* Ubuntu 14.04 (on travis-ci), R 3.6.1
* Ubuntu 16.04 (project server), R 3.6.3
* win-builder (devel and release), R. 3.6.1
* Windows/AppVeyor, R 3.6.1 Patched
* Debian Linux, R-devel, GCC


## R CMD check results

There were no ERRORs, WARNINGs or NOTEs on the Linux / macOS / Windows environments I used. 


## Downstream dependencies

I have also checked downstream dependencies using devtools::revdep(),
without seeing ERRORs, WARNINGs, or NOTEs.

