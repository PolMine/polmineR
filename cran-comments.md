## General remarks

This release catches up with markdown >= 1.3 such that markdown maintainers
will not have to rely on workarounds to ensure that markdown improvements 
to not break polmineR reverse dependency tests.

The first submission of this update was rejected because some examples
ran too long (> 2.5 secs). The report I received was:

Examples with CPU time > 2.5 times elapsed time
                         user system elapsed ratio
polmineR-package        1.003  0.012   0.338 3.003
all-cooccurrences-class 1.646  0.068   0.593 2.890
bundle                  1.599  0.052   0.588 2.808
features                1.611  0.032   0.593 2.771
as.DocumentTermMatrix   2.277  0.088   0.933 2.535

I have somewhat minimized the respective examples and replaced the (bigger) 
test data "GERMAPARLMINI" with the (smaller) "REUTERS" data, which reduces 
time for executing examples by 1/3.

I also put one long-running example (in "all-cooccurrences-class") into 
donttest{}.

If examples are still too slow, I will add further donttest{} statements. 
Tests with GitHub Actions would still ensure that code remains tested. 


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

