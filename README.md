
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
#> polmineR 0.7.5
#> registry:  /Library/Frameworks/R.framework/Versions/3.4/Resources/library/polmineR.sampleCorpus/extdata/cwb/registry
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
#> ... reseting CORPUS_REGISTRY environment variable:
#>     /Library/Frameworks/R.framework/Versions/3.4/Resources/library/europarl.en/extdata/cwb/registry
#> ... unloading rcqp library
#> ... reloading rcqp library
#> ... status: OK
```

An advantage of keeping corpora in data packages are the versioning and documentation mechanisms that are the hallmark of packages. Of course, polmineR will work with the library of CWB indexed corpora stored on your machine. To assist the preparation of corpora, consider the ctk package (corpus toolkit) as a companion package to polmineR.

### partition (and partitionBundle)

All methods can be applied to a whole corpus, are to partitions (i.e. subcorpora). Use the metadata of a corpus (so-called s-attributes) to define a subcorpus.

``` r
ep2005 <- partition("EUROPARL-EN", text_year = "2006")
#> Setting up partition
#> ... get encoding: latin1
#> ... get cpos and strucs
#> ... get partition size
size(ep2005)
#> [1] 3100529
```

``` r
barroso <- partition("EUROPARL-EN", speaker_name = "Barroso", regex = TRUE)
#> Setting up partition
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
