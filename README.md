R-package 'driller'
===================
Machinery for drilling CWB corpora
----------------------------------

### Purpose
The purpose of the package 'driller' is to facilitate the interactive analysis of corpora using R. Core objectives for the development of the package are performance, usability, and a modular design.

There are many tools already for text mining. Why yet another one? Important incentives for developing the package were:
- to create a package that makes the creation and analysis of subcorpora (called 'partitions' here) as easy as possible. A particular strength of the package should be to support contrastive/comparative research.
- to keep the original text accessible. The driller is based on the conviction that statistical analysis alone may be blind and deaf.
- to provide an open source platform that will make text mining more productive, avoiding prohibitive costs of any kind. Getting into R will help, however.

### Design
The driller relies on the Open Corpus Workbench (CWB) as a backend and uses the rcqp package as an interface. The CWB is particularly efficient for storing large corpora and offers a powerful language for querying corpora, the Corpus Query Processor (CQP). The architecture may be overengineered if you work with smaller corpora. It is meant to make working with larger corpora efficient, both locally, or on a server.

### Background
The driller was specifically developed to make full use of the XML annotation structure of the corpora created in the PolMine project (see polmine.sowi.uni-due.de). The core PolMine corpora are corpora of plenary protocols. In these corpora, speakers, parties etc. are structurally annotated. The driller is meant to help making full use of the rich annotation structure.

### Core functions
- partition: Set up a partition (i.e. subcorpus);
- context: Analyse the context of a query (including some statistics);
- distribution: Analyse the distribution of a query across one or two dimensions (absolute and relative frequencies);
- keyness: Compare two partitions to identify specific vocabulary (using a chi-square test).

### State of affairs
There are quite a few further functions, some of which are experimental. The publication of the Driller on CRAN is planned will be a next step. Most recent developments will be available here on GitHub.

### Installation
Theoretically, it sould be easy to install the package with the devtools mechanism. It has been checked that the driller is portable, but feedback is most welcome.
The tricky part of the installation will usually be the rcqp package. See the package vignette of the driller for some preliminary advice.

### Feedback
Getting feedback is most welcome! I want this to be a useful package not just for me. Please do get in touch: Andreas Blaette, University of Duisburg-Essen (andreas.blaette@uni-due.de).