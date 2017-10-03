
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![License](https://img.shields.io/aur/license/yaourt.svg)](http://www.gnu.org/licenses/gpl-3.0.html) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/polmineR)](https://cran.r-project.org/package=polmineR) [![Downloads](http://cranlogs.r-pkg.org/badges/polmineR)](https://cran.r-project.org/package=polmineR) [![Travis-CI Build Status](https://api.travis-ci.org/PolMine/polmineR.svg?branch=master)](https://travis-ci.org/PolMine/polmineR) [![codecov](https://codecov.io/gh/PolMine/polmineR/branch/master/graph/badge.svg)](https://codecov.io/gh/PolMine/polmineR/branch/master)

polmineR
========

**Purpose**: The focus of the package 'polmineR' is the interactive analysis of corpora using R. Core objectives for the development of the package are performance, usability, and a modular design.

**Aims**: Key aims for developing the package are:

-   To keep the original text accessible. A seamless integration of qualitative and quantitative steps in corpus analysis supports validation, based on inspecting the text behind the numbers.

-   To provide a library with standard tasks. It is an open source platform that will make text mining more productive, avoiding prohibitive costs to reimplement basics, or to run many lines of code to perform a basic tasks.

-   To create a package that makes the creation and analysis of subcorpora ('partitions') easy. A particular strength of the package is to support contrastive/comparative research.

-   To offer performance for users with an ordinary infrastructure. The package picks up the idea of a three-tier software design. Corpus data are managed and indexed by using the *Open Corpus Workbench* (CWB). The CWB is particularly efficient for storing large corpora and offers a powerful language for querying corpora, the Corpus Query Processor (CQP).

-   To support sharing consolidated and documented data, following the ideas of reproducible research.

**Background**: The polmineR-package was specifically developed to make full use of the XML annotation structure of the corpora created in the PolMine project (see polmine.sowi.uni-due.de). The core PolMine corpora are corpora of plenary protocols. In these corpora, speakers, parties etc. are structurally annotated. The polmineR-package is meant to help making full use of the rich annotation structure.

Core Functions
--------------

-   *partition*: Set up a partition (i.e. subcorpus);
-   *count*: Count features
-   *dispersion*: Analyse the dispersion of a query across one or two dimensions (absolute and relative frequencies);
-   *cooccurrences*: Analyse the context of a query (including some statistics);
-   *features*: Compare partitions to identify features / keywords (using statistical tests such as chi square).

[![kwic()](http://polmine.sowi.uni-due.de/gallery/kwic.png)](http://polmine.sowi.uni-due.de/gallery/kwic.png)

Installation
------------

### Windows (32 bit / i386)

At this stage, an easy way to install polmineR is available only for 32bit R. Usually, an R installation will include both 32bit and 64bit R. So if you want to keep things simple, make sure that you work with 32bit version. If you work with RStudio (highly recommended), the menu Tools &gt; Global Options will open a dialogue where you can choose 32bit R.

Before installing polmineR, the package 'rcqp' needs to be installed. In turn, rcqp requires plyr, which should be installed first.

``` r
install.packages("plyr")
```

To avoid compiling C code in a package, packages with compiled binaries are very handy. Windows binaries for the rcqp package are not available at CRAN, but can be installed from a repository of packages entertained at the server of the PolMine project:

``` r
install.packages("rcqp", repos = "http://polmine.sowi.uni-due.de/packages", type = "win.binary")
```

To explain: Compiling the C code in the rcqp package on a windows machine is not yet possible. The package we offer uses a cross-compilation of these C libraries, i.e. binaries that have been prepared for windows on a MacOS/Linux machine.

Before proceeding to install polmineR, we install dependencies that are not installed automatically.

``` r
install.packages(pkgs = c("htmltools", "htmlwidgets", "magrittr", "iterators", "NLP"))
```

The latest stable version of polmineR can now be installed from CRAN. Several other packages that polmineR depends on, or that dependencies depend on may be installed automatically.

``` r
install.packages("polmineR")
```

The development version of the package, which may include the most recent updates and features, can be installed from GitHub. The easiest way to do this is to use a mechanism offered by the package devtools.

``` r
install.packages("devtools")
devtools::install_github("PolMine/polmineR", ref = "dev")
```

The installation may throw warnings. There are three warnings you can ignore at this stage:

-   "WARNING: this package has a configure script / It probably needs manual configuration".
-   The environment variable CORPUS\_REGISTRY is not defined.
-   package 'rcqp' is not installed for 'arch = x64'.

The configure script is for Linux/MacOS installation, its sole purpose is to pass tests for uploading the package to CRAN. As mentioned, windows binaries are not yet available for 64bit R at present, so that can be ignored. The environment variable "CORPUS\_REGISTRY" can be set as follows in R:

``` r
Sys.setenv(CORPUS_REGISTRY = "C:/PATH/TO/YOUR/REGISTRY")
```

To set the environment variable CORPUS\_REGISTRY permanently, see the instructions R offer how to find the file '.Renviron' or '.Renviron.site' when calling the help for the startup process(`?Startup`).

Two important notes concerning problems with the CORPUS\_REGISTRY environment variable that may cause serious headaches:

-   The path can not be processed, if there is any whitespace in the path pointing to the registry. Whitespace may occur in the user name ("C:/Users/Donald Duck/Documents"), for instance. We do not yet know any workaround to make rcqp/CWB process whitespace. The recommendation is to create a directory at a path without whitespace to keep the registry and the indexed\_corpora (a directory such as "C:/cwb").

-   If you keep data on another volume than your system files, your R packages etc. (eg. volume 'C:' for system files, and 'D:' for data and user files), make sure to set the working directory (`setwd()`) is set to any directory on the volume with the directory defined via CORPUS\_REGISTRY. CWB/rcqp will assume that the CORPUS\_REGISTRY directory is on the same volume as the current working directory (which can be identified by calling `getwd()`).

Finally: polmineR if optimized for working with RStudio. It you work with 32bit R, you may have to check in the settings of RStudio that it will call 32bit R. To be sure, check the startup message.

If everything works, check whether polmineR can be loaded.

``` r
library(polmineR)
corpus() # to see corpora available at your system
```

### Windows (64 bit / x86)

At this stage, 64 bit support is still experimental. Apart from an installation of 64 bit R, you will need to install Rtools, available [here](https://cran.r-project.org/bin/windows/Rtools/). Rtools is a collection of tools necessary to build and compile R packages on a Windows machine.

To interface to a core C library of the Corpus Workbench (CWB), you will need an installation of a 64 bit AND a 32 bit version of the CWB.

The "official" 32 bit version of the CWB is available [here](https://sourceforge.net/projects/cwb/files/cwb/cwb-3.4-beta/). Installation instructions are available at the [CWB Website](http://cwb.sourceforge.net/beta.php). The 32 bit version should be installed in the directory "C:Files", with admin rights.

The 64 bit version, prepared by Andreas Blaette, is available [here](http://polmine.sowi.uni-due.de/public/?dir=CWB). Install this 64 bit CWB version to "C:Files (x86)". In the unzipped downloaded zip file, you will find a bat file that will do the installation. Take care that you run the file with administrator rights. Without these rights, no files will be copied.

The interface to the Corpus Workbench is the package polmineR.Rcpp, [available at GitHub](https://www.github.com/PolMine/polminer.Rcpp). If you use git, you can clone that repository, otherwise, you can download a zip file.

The downloaded zip file needs to be unzipped again. Then, in the directory with the 'polmineR.Rcpp'-directory, run:

``` sh
R CMD build polmineR.Rcpp
R CMD INSTALL polmineR.Rcpp_0.1.0.tar.gz
```

If you read closely what is going on during the compilation, you will see a few warnings that libraries are not found. If creating the package is not aborted, nothing is wrong. R CMD build will look for the 64 bit files in the directory with the 32 bit dlls first and discover that they do not work for 64 bit, only then will it move to the correct location.

One polmineR.Rcpp is installed, proceed with the instructions for installing polmineR in a 32 bit context. Future binary releases of the polmineR.Rcpp package may make things easier. Anyway, the proof of concept is there that polmineR will work on a 64 bit Windows machine too.

Finally, you need to make sure that polmineR will interface to CWB indexed corpora using polmineR.Rcpp, and not with rcqp (the default). To set the interface accordingly:

``` r
setCorpusWorkbenchInterface("Rcpp")
```

To test whether corpora are available:

``` r
corpus()
```

### MacOS

The following instructions for Mac users assume that R is installed on your system. Binaries are available from the [Homepage of the R Project](https://cran.r-project.org/bin/macosx/). An installation of RStudio is highly recommended. The Open Source License version of [RStudio Desktop](https://www.rstudio.com/products/rstudio/download/) is what you need.

#### Installing 'polmineR'

The latest release of polmineR can be installed from CRAN using the usual `install.packages`-function.

``` r
install.packages("polmineR")
```

The development version of *polmineR* can be installed using devtools:

``` r
install.packages("devtools") # unless devtools is already installed
devtools::install_github("PolMine/polmineR", ref = "dev")
```

#### Installing 'rcqp'

The default interface of the polmineR package to access CWB indexed corpora is the package 'rcqp'. Accessing corpora will not work before you have installed the interface.

##### Installing precompiled binary of rcqp from the PolMine server

The easiest way to get rcqp for Mac is install a precompiled binary that is available at the PolMine server:

``` r
install.packages(
  "rcqp",
  repos = "http://polmine.sowi.uni-due.de/packages",
  type = "mac.binary"
  )
```

##### Building rcqp from source

If you want to get rcqp from CRAN and/or if you want to to compile the C code yourself, the procedure is as follows.

First, you will need an installation of Xcode, which you can get it via the Mac App Store. You will also need the Command Line Tools for Xcode. It can be installed from a terminal with:

``` sh
xcode-select --install
```

To compile the C code in the rcqp package, there are system requirements that need to be fulfilled. Using a package manager such as Homebrew or Macports makes things considerably easier.

*Option 1: Using Homebrew*

We recommend to use 'Homebrew'. To install Homebrew, follow the instructions on the [Homebrew Homepage](https://brew.sh/index_de.html). The following commands will install the C libraries the rcqp package relies on:

``` sh
brew -v install pkg-config
brew -v install glib --universal
brew -v install pcre --universal
brew -v install readline
```

*Option 2: Using Macports*

If you prefer using Macports, get it from <https://www.macports.org/>. After installing Macports, it is necessary to restart the computer. Next, an update of Macports is necessary.

``` sh
sudo port -v selfupdate
```

Now we can install the libraries rcqp will require. Again, from the terminal.

``` sh
sudo port install glib2
sudo port install pkgconfig
sudo port install pcre
```

*Install dependencies and rcqp*

Once the system requirements are there, the next steps can be done from R. Before installing rcqp, and then polmineR, we install a few packages. In the R console:

``` r
install.packages(pkgs = c("RUnit", "devtools", "plyr", "tm"))
```

Now rcqp can be installed, and then polmineR:

``` r
install.packages("rcqp")
install.packages("polmineR")
```

If you like to work with the development version, that can be installed from GitHub.

``` r
devtools::install_github("PolMine/polmineR", ref = "dev")
```

### Linux

The pcre, glib and pkg-config libraries can be installed using apt-get.

``` sh
sudo apt-get install libglib2.0-dev
sudo apt-get install libssl-dev
sudo apt-get install libcurl4-openssl-dev
```

The system requirements will now be fulfilled. From R, install dependencies for rcqp/polmineR first, and then rcqp and polmineR.

``` r
install.packages("RUnit", "devtools", "plyr", "tm")
install.packages("rcqp")
install.packages("polmineR")
```
