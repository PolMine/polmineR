[![License](https://img.shields.io/aur/license/yaourt.svg)](http://www.gnu.org/licenses/gpl-3.0.html)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/polmineR)](https://cran.r-project.org/package=polmineR)
[![Downloads](http://cranlogs.r-pkg.org/badges/polmineR)](https://cran.r-project.org/package=polmineR)
[![Travis-CI Build Status](https://api.travis-ci.org/PolMine/polmineR.svg?branch=master)](https://travis-ci.org/PolMine/polmineR)
[![codecov](https://codecov.io/gh/PolMine/polmineR/branch/master/graph/badge.svg)](https://codecov.io/gh/PolMine/polmineR/branch/master)

# R-package 'polmineR'

### Purpose
The focus of the package 'polmineR' is the interactive analysis of corpora using R. Core objectives for the development of the package are performance, usability, and a modular design.

### Aims
Key aims for developing the package are:
- To keep the original text accessible. A seamless integration of qualitative and quantitative steps in corpus analysis supports validation, based on inspecting the text behind the numbers.
- To provide a library with standard tasks.  It is  an open source platform that will make text mining more productive, avoiding prohibitive costs to reimplement basics, or to run many lines of code to perform a basic tasks.
- To create a package that makes the creation and analysis of subcorpora ('partitions') easy. A particular strength of the package is to support contrastive/comparative research.
- To offer performance for users with an ordinary infrastructure. The package picks up the idea of a three-tier software design. Corpus data are managed and indexed by using the Corpus Workbench (CWB).
- To support sharing consolidated and documented data, following the ideas of reproducible research.

### Backend
The polmineR relies on the _Open Corpus Workbench_ (CWB) as a backend and uses the _rcqp_ package as an interface. The CWB is particularly efficient for storing large corpora and offers a powerful language for querying corpora, the Corpus Query Processor (CQP). The architecture may be overengineered if you work with smaller corpora. It is meant to make working with larger corpora efficient, both locally, or on a server.

### Background
The polmineR-package was specifically developed to make full use of the XML annotation structure of the corpora created in the PolMine project (see polmine.sowi.uni-due.de). The core PolMine corpora are corpora of plenary protocols. In these corpora, speakers, parties etc. are structurally annotated. The polmineR-package is meant to help making full use of the rich annotation structure.

### Installation

### Core Functions
- *partition*: Set up a partition (i.e. subcorpus);
- *count*: Count features
- *dispersion*: Analyse the dispersion of a query across one or two dimensions (absolute and relative frequencies);
- *cooccurrences*: Analyse the context of a query (including some statistics);
- *features*: Compare partitions to identify features / keywords (using statistical tests such as chi square).

[![kwic()](http://polmine.sowi.uni-due.de/gallery/kwic.png)](http://polmine.sowi.uni-due.de/gallery/kwic.png)


### Feedback
Getting feedback is most welcome! Please do get in touch: Andreas Blaette, University of Duisburg-Essen (andreas.blaette@uni-due.de).
