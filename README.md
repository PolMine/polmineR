R-package 'polmineR'
===================
Base package of the PolMine-Toolkit
-----------------------------------

### Purpose
The purpose of the package 'polmineR' is to facilitate the interactive analysis of corpora using R. Core objectives for the development of the package are performance, usability, and a modular design.

There are many tools already for text mining. Why yet another one? Important incentives for developing the package were:
- to create a package that makes the creation and analysis of subcorpora (called 'partitions' here) as easy as possible. A particular strength of the package should be to support contrastive/comparative research.
- to keep the original text accessible. The polmineR is based on the conviction that statistical analysis alone may be blind and deaf.
- to provide an open source platform that will make text mining more productive, avoiding prohibitive costs of any kind. Well, some familiarity with R is still necessary.

### Design
The polmineR relies on the _Open Corpus Workbench_ (CWB) as a backend and uses the _rcqp_ package as an interface. The CWB is particularly efficient for storing large corpora and offers a powerful language for querying corpora, the Corpus Query Processor (CQP). The architecture may be overengineered if you work with smaller corpora. It is meant to make working with larger corpora efficient, both locally, or on a server.

### Background
The polmineR-package was specifically developed to make full use of the XML annotation structure of the corpora created in the PolMine project (see polmine.sowi.uni-due.de). The core PolMine corpora are corpora of plenary protocols. In these corpora, speakers, parties etc. are structurally annotated. The polmineR-package is meant to help making full use of the rich annotation structure.

### Core functions
- *partition*: Set up a partition (i.e. subcorpus);
- *context*: Analyse the context of a query (including some statistics);
- *dispersion*: Analyse the dispersion of a query across one or two dimensions (absolute and relative frequencies);
- *compare*: Compare two partitions to identify specific vocabulary (using a chi-square test).
- *count*: Count features

### State of affairs
There are quite a few further functions, some of which are experimental. The publication of the polmineR-package on CRAN is planned as soon as the portability of the package is ensured. Most recent developments will be available here on GitHub.

### Installation
Theoretically, it sould be easy to install the package with the _devtools_ mechanism. It has been checked on a preliminary basis that the package is portable, but feedback is most welcome.
The tricky part of the installation will usually be the _rcqp_ package. See the package vignette for some advice.

### Feedback
Getting feedback is most welcome! I want this to be a useful package not just for me. Please do get in touch: Andreas Blaette, University of Duisburg-Essen (andreas.blaette@uni-due.de).
