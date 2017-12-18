
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

``` r
library(polmineR)
#> polmineR 0.7.6
#> registry:  /Users/blaette/Data/cwb/registry
#> interface: CQI.rcqp
```

### install.corpus (and use packaged corpus)

Indexed corpora wrapped into R data packages can be installed from a (private) package repository.

``` r
install.corpus("GermaParl")
install.corpus("europarl.en")
```

Calling the use function is necessary activate a corpus included in a data package.

``` r
use("europarl.en") # activate the corpus in the europarl-en package
#> ... resetting CORPUS_REGISTRY environment variable:
#> ... setting registry: /Library/Frameworks/R.framework/Versions/3.4/Resources/library/europarl.en/extdata/cwb/registry
#> ... unloading rcqp library
#> ... reloading rcqp library
#> ... ... status: OK
```

An advantage of keeping corpora in data packages are the versioning and documentation mechanisms that are the hallmark of packages. Of course, polmineR will work with the library of CWB indexed corpora stored on your machine. To assist the preparation of corpora, consider the ctk package (corpus toolkit) as a companion package to polmineR.

### partition (and partitionBundle)

All methods can be applied to a whole corpus, are to partitions (i.e. subcorpora). Use the metadata of a corpus (so-called s-attributes) to define a subcorpus.

``` r
ep2005 <- partition("EUROPARL-EN", text_year = "2006")
#> ... Setting up partition
#> ... get encoding: latin1
#> ... get cpos and strucs
#> ... get partition size
size(ep2005)
#> [1] 3100529
```

``` r
barroso <- partition("EUROPARL-EN", speaker_name = "Barroso", regex = TRUE)
#> ... Setting up partition
#> ... get encoding: latin1
#> ... get cpos and strucs
#> ... get partition size
size(barroso)
#> [1] 98142
```

Partitions can be bundled into partitionBundle objects, and most methods can be applied to a whole corpus, a partition, or a partitionBundle object alike. Consult the package vignette to learn more.

### count (using CQP syntax)

Counting occurrences of a feature in a corpus, a partition or in the partitions of a partitionBundle is a basic operation. By offering access to the query syntax of the Corpus Query Processor (CQP), the polmineR package makes accessible a query syntax that goes far beyond regular expressions. See the [CQP documentation](http://www.ims.uni-stuttgart.de/forschung/projekte/CorpusWorkbench/CQPTutorial/cqp-tutorial.2up.pdf) to learn more.

``` r
count("EUROPARL-EN", "France")
#>     query count         freq
#> 1: France  5517 0.0001399122
count("EUROPARL-EN", c("France", "Germany", "Britain", "Spain", "Italy", "Denmark", "Poland"))
#>      query count         freq
#> 1:  France  5517 1.399122e-04
#> 2: Germany  4196 1.064114e-04
#> 3: Britain  1708 4.331523e-05
#> 4:   Spain  3378 8.566676e-05
#> 5:   Italy  3209 8.138089e-05
#> 6: Denmark  1615 4.095673e-05
#> 7:  Poland  1820 4.615557e-05
count("EUROPARL-EN", '"[pP]opulism"')
#>            query count         freq
#> 1: "[pP]opulism"   107 2.713542e-06
```

### dispersion (across one or two dimensions)

The dispersion method is there to analyse the dispersion of a query, or a set of queries across one or two dimensions (absolute and relative frequencies). The CQP syntax can be used.

``` r
populism <- dispersion("EUROPARL-EN", "populism", sAttribute = "text_year", progress = FALSE)
popRegex <- dispersion("EUROPARL-EN", '"[pP]opulism"', sAttribute = "text_year", cqp = TRUE, progress = FALSE)
```

### cooccurrences (to analyse collocations)

The cooccurrences method is used to analyse the context of a query (including some statistics).

``` r
islam <- cooccurrences("EUROPARL-EN", query = 'Islam', left = 10, right = 10)
islam <- subset(islam, rank_ll <= 100)
dotplot(islam)
islam
```

[![cooccurrences()](http://polmine.sowi.uni-due.de/gallery/cooccurrences.png)](http://polmine.sowi.uni-due.de/gallery/cooccurrences.png)

### features (keyword extraction)

Compare partitions to identify features / keywords (using statistical tests such as chi square).

``` r
ep2002 <- partition("EUROPARL-EN", text_year = "2002", pAttribute = "word")
epPre911 <- partition("EUROPARL-EN", text_year = as.character(1997:2001), pAttribute = "word")
y <- features(ep2002, epPre911, included = FALSE)
```

### kwic (also known as concordances)

So what happens in the context of a word, or a CQP query? To attain valid research results, reading will often be necessary. The kwic method will help, and uses the conveniences of DataTables, outputted in the Viewer pane of RStudio.

``` r
kwic("EUROPARL-EN", "Islam", meta = c("text_date", "speaker_name"))
```

[![kwic()](http://polmine.sowi.uni-due.de/gallery/kwic.png)](http://polmine.sowi.uni-due.de/gallery/kwic.png)

### read (the full text)

Corpus analysis involves moving from text to numbers, and back again. Use the read method, to inspect the full text of a partition (a speech given by chancellor Angela Merkel in this case).

``` r
use("GermaParl")
merkel <- partition("GERMAPARL", text_speaker = "Angela Merkel", text_date = "2013-09-03")
read(merkel)
```

[![read()](http://polmine.sowi.uni-due.de/gallery/read.png)](http://polmine.sowi.uni-due.de/gallery/read.png)

### as.TermDocumentMatrix (for text mining purposes)

Many advanced methods in text mining require term document matrices as input. Based on the metadata of a corpus, these data structures can be obtained in a fast and flexible manner, for performing topic modelling, machine learning etc.

``` r
use("europarl.en")
speakers <- partitionBundle(
  "EUROPARL-EN", sAttribute = "speaker_id",
  progress = FALSE, verbose = FALSE
)
speakers <- enrich(speakers, pAttribute = "word")
tdm <- as.TermDocumentMatrix(speakers, col = "count")
dim(tdm)
```

Installation
------------

### Windows

The following instructions assume that you have installed R. If not, install it from[CRAN](https://cran.r-project.org/bin/windows/base/). An installation of [RStudio](https://www.rstudio.com/products/rstudio/download/#download) is highly recommended.

#### Windows (64 bit)

For 64bit Windows, an interface included in the package polmineR.Rcpp is offered. It can be installed from the R package repository on the Webserver of the PolMine project as follows.

``` r
install.packages(
  "polmineR.Rcpp",
  repos = "http://polmine.sowi.uni-due.de/packages",
  type = "win.binary"
  )
```

Now the official CRAN version of polmineR can be installed.

``` r
install.packages("polmineR")
```

To install the most recent development version that is hosted in a GitHub repository, use the convenient installation mechanism offered by the devtools package.

``` r
install.packages("devtools")
devtools::install_github("PolMine/polmineR", ref = "dev")
```

To have access to the syntax of CQP when forming queries, an installation of the CWB needs to be on your system. The install.cwb-function of polmineR will download the CWB binaries and puts it into a subfolder of the polmineR package.

``` r
library(polmineR)
install.cwb()
```

During the download and installation, you may see the following message: "Please make sure that 'D:' is in your 'path' so you can call CWB programs from the command prompt!" Adding this path to the PATH environment variable is actually not necessary, because functions in the polmineR package will know where to look for the CWB.

Finally, as a basic test whether the REUTERS corpus included in the polmineR package (for testing and demonstration purposes) is available, run:

``` r
corpus()
```

#### Windows (32 bit / i386)

If you work with a 32bit Windows machine, the rcqp package can be used to access CWB indexed corpora. rcqp requires plyr, which should be installed first.

``` r
install.packages("plyr")
```

Windows binaries for the rcqp package are not available at CRAN. A cross-compilation can be installed from a repository of packages hosted at the server of the PolMine project:

``` r
install.packages("rcqp", repos = "http://polmine.sowi.uni-due.de/packages", type = "win.binary")
```

Before proceeding to install polmineR, we install dependencies that are not installed automatically.

``` r
install.packages(pkgs = c("htmltools", "htmlwidgets", "magrittr", "iterators", "NLP"))
```

The latest stable version of polmineR can now be installed from CRAN. Several other packages that polmineR depends on will be installed automatically.

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

If everything works, check whether polmineR can be loaded.

``` r
library(polmineR)
corpus() # to see corpora available at your system
```

#### Setting the CORPUS\_REGISTRY environment variable

The environment variable "CORPUS\_REGISTRY" can be set as follows in R:

``` r
Sys.setenv(CORPUS_REGISTRY = "C:/PATH/TO/YOUR/REGISTRY")
```

To set the environment variable CORPUS\_REGISTRY permanently, see the instructions R offer how to find the file '.Renviron' or '.Renviron.site' when calling the help for the startup process(`?Startup`).

Two important notes concerning problems with the CORPUS\_REGISTRY environment variable that may cause serious headaches on Windows machines:

-   The path can not be processed, if there is any whitespace in the path pointing to the registry. Whitespace may occur in the user name ("C:/Users/Donald Duck/Documents"), for instance. There is no known workaround to make rcqp/CWB process whitespace. The recommendation is to create a directory at a path without whitespace to keep the registry and the indexed\_corpora (a directory such as "C:/cwb").

-   If you keep data on another volume than your system files, your R packages etc. (eg. volume 'C:' for system files, and 'D:' for data and user files), make sure to set the working directory (`setwd()`) is set to any directory on the volume with the directory defined via CORPUS\_REGISTRY. CWB/rcqp will assume that the CORPUS\_REGISTRY directory is on the same volume as the current working directory (which can be identified by calling `getwd()`).

### MacOS

The following instructions for Mac users assume that R is installed on your system. Binaries are available from the [Homepage of the R Project](https://cran.r-project.org/bin/macosx/). An installation of RStudio is highly recommended. Get the Open Source License version of [RStudio Desktop](https://www.rstudio.com/products/rstudio/download/).

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

The easiest way to get rcqp for Mac is install a precompiled binary that is available at the PolMine server. The precompiled package includes the C library 'rcqp.so' and compilations of all dependencies.

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

Please make sure that you agree to the license.

Second, an installation of XQuartz is required, it can be obtained from [www.xquartz.org](https://www.xquartz.org/).

Third, to compile the C code in the rcqp package, there are system requirements that need to be fulfilled. Using a package manager makes things considerably easier. We recommend using 'Homebrew'. To install Homebrew, follow the instructions on the [Homebrew Homepage](https://brew.sh/index_de.html). The following commands then need to be executed from a terminal window. They will install the C libraries that the rcqp package relies on:

``` sh
brew -v install pkg-config
brew -v install glib --universal
brew -v install pcre --universal
brew -v install readline
```

Fourth, install dependencies of rcqp, and finally rcqp. That can be done from within R.

``` r
install.packages(pkgs = c("RUnit", "devtools", "plyr", "tm"))
install.packages("rcqp")
```

A quick check that polmineR is installed correctly is to load the library, and to check which corpora are available.

``` r
library(polmineR)
corpus()
```

You should see a message that rcqp is the interface used, and that the REUTERS corpus is on your system.

### Linux (Ubuntu)

#### Installing R

If you have not yet installed R on your Ubuntu machine, there is a good instruction at [ubuntuuser](https://wiki.ubuntuusers.de/R/). To install base R, enter in the terminal.

``` sh
sudo apt-get install r-base r-recommended
```

Make sure that you have installed the latest version of R. The following commands will add the R repository to the package sources and run an update. The second line assumes that you are using Ubuntu 16.04.

``` sh
sudo apt-key adv --recv-keys --keyserver keyserver.ubuntu.com E084DAB9
sudo add-apt-repository 'deb http://ftp5.gwdg.de/pub/misc/cran/bin/linux/ubuntu xenial/'
sudo apt-get update
sudo apt-get upgrade
```

#### Installing RStudio

It is highly recommended to install [RStudio](https://www.rstudio.com/products/rstudio/download/#download), a powerful IDE for R. Output of polmineR methods is generally optimized to be displayed using RStudio facilities. If you are working on a remote server, running RStudio Server may be an interesting option to consider.

#### Base Installation of polmineR

The Corpus Workbench will require the pcre, glib and pkg-config libraries. They can be installed as follows. In addition libxml2 is installed, a dependency of the R package xml2 that is used for manipulating html output.

``` sh
sudo apt-get install libglib2.0-dev libssl-dev libcurl4-openssl-dev
sudo apt-get install libxml2-dev
sudo apt-get install libprotobuf-dev
```

The system requirements will now be fulfilled. From R, install dependencies for rcqp/polmineR first, and then rcqp and polmineR.

``` r
install.packages(pkgs = c("RUnit", "devtools", "plyr", "tm"))
install.packages("rcqp")
install.packages("polmineR")
```

Use devtools to install the development version of polmineR from GitHub.

``` r
install.packages("devtools")
devtools::install_github("PolMine/polmineR", ref = "dev")
```

You may want to install packaged corpora to run examples in the vignette, and the man packages.

``` r
library(polmineR)
install.corpus("polmineR.sampleCorpus")
install.corpus("europarl.en")
```

#### polmineR - Full installation

To have access to all package functions and to run all package tests, the installation of further system requirements and packages is required. The xlsx dependency requires that rJava is installed and configured for R. That is done on the shell:

``` sh
sudo apt-get install openjdk-8-jre
sudo R CMD javareconf
```

To run package tests including (re-)building the manual and vignettes, a working installation of Latex is required, too. Be aware that this may be a time-consuming operation.

``` sh
sudo apt-get install texlive-full texlive-xetex 
```

Now install the remaining packages from within R.

``` r
devtools::install_github("PolMine/polmineR.Rcpp")
install.packages(pkgs = c("rJava", "xlsx", "tidytext"))
```
