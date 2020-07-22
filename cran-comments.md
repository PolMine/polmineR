## General remarks

This is a quick follow-up to polmineR v0.8.4 to address remaining issues with portability:

- Internal usage of iconv will now allow that a Linux distribution might use something alse than UTF-8 (the Debian CRAN machine with clan / R-devel uses ISO-8859-15). An ERROR on this machine should be addressed.
- All remaining Unicode characters have been removed from the documentation. Hopefully, the warning on Solaris "it is not known that wchar_t is Unicode on this platform" will not occurr any more.
- Building the vignette will not require pandoc to be present. Respective warnings on Solaris and macOS-oldrel will hopefully not occur any more.


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

