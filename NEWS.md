# v0.7.7.9001
* upon loading the package, check that data directories are set correctly in registry files to make sure that sample data in pre-compiled packages can be used
* startup messages adjust slightly

# v0.7.7 
* removed depracated classes: dispersion, Textstat (reference class), Partition (reference class)
* divide-methode moved to package polmineR.misc
* bug removed: size of ngrams object was always 1
* dotplot-method added for featuresNgrams
* sample corpus GermaParlMini added to the package (replacing suggested package polmineR.sampleCorpus)
* configuration mechanism added to set path to data directory in registry file upon installation 
* class hits now inherits from class 'textstat', exposing a set of generic functions (such as dim, nrow etc.); slot 'dt' changed to 'stat' for this purpose
* count,partitionBundle and hits,partitionBundle: cqp parameter added
* RegistryFile class replaced by a set of leightweight-functions (corpus_...)
* encode-method moved to cwbtools package
* getTerms,character-method and terms,partition-method merged
* examples using EUROPARL corpus have been replaced by REUTERS corpus (including vignette)
* param id2str has been renamed to decode in all functions to avoid unwanted behavior
* robust indexing of bundle objects for subsetting
* optional settings have been cleaned
* reliance on cwb command line tools removed
* encoding issue with names of partitionBundle solved

# v0.7.6
* functionality of matches-method (breakdown of frequencies of matches) integrated
into count-method (new param breakdown)
* corpus REUTERS included (as data for testsuite)
* adjust data directory of REUTERS corpus upon loading package
* a pkgdown-generated website is included in the docs directory
* consistent use of .message helper function to make shiny app work
* bug removed for count-method when options("polmineR.cwb-lexdecode") is TRUE and
options("polmineR.Rcpp") is FALSE
* if CORPUS_REGISTRY is not defined, the registry directory in the package will
be used, making REUTERS corpus available
* getSettings-function removed, was not sufficiently useful, and was superseded by
template mechanism
* new class 'count' introduced to organize results from count operations
* at startup, default template is assigned for corpora without explicitly defined templates to make read() work in a basic fashion
* new cpos,hits-method to support highlight method
* tooltips-method to reorder functionality of html/highlight/tooltip-methods
* param charoffset added to html-method 
* coerce-method from partition to json and vice versa, potentially useful for storing partitions
* sAttributes2cpos to work properly with nested xml
* partition,partition-method reworked to work properly with nested XML
* encoding of return value of sAttributes will be locale
* references added to methods count, kwic, cooccurrences, features.
* as.DocumentTermMatrix,character-method reworked to allow for subsetting and divergence of
strucs and struc_str
* html,partition-method has new option beautify, to remove whitespace before interpunctuation
* output error removed in html,partition-method (that misinterprets `` as code block)
* the class Corpus now has a slot sAttribute to keep/manage a data.table with corpus positions
and struc values, and there is a new partition,Corpus-method. In compbination, it will be a lot
faster to derive a partition, particularly if you need to do that repeatedly
* a new function install.cwb() provides a convenient way to install CWB in the package 
* added a missing encoding conversion for the count method

# v0.7.5
* class 'Regions' renamed to class 'regions' as a matter of consistency
* data type of slot cpos of class 'regions' is a matrix now
* rework and improved documentation for decode- and encode-methods
* new functions copy.corpus and rename.corpus
* as.DocumentTermMatrix-method checks for strucs with value -1 
* improved as.speeches-method: reordering of speeches, default values
* blapply-method: verbose output will be suppressed of progress is TRUE

# v0.7.4
* applying stoplists and positivelists working again for context-method
* matches-method to learn about matches for CQP queries replacing frequencies-method
* Rework of enrich-method, including documentation.
* param 'neighbor' dropped from kwic,context-method; params positivelist and negativelist offer equivalent functionality
* highlight-method for (newly exported) kwic-method (for validation purposes)
* performance improvement for partitionBundle,character-method
* a new Labels class and label method for generating test data
* bug removed for partitionBundle,character-class, and performance improved
* Improved explanation of the installation procedure for Mac in the package vignette
* for context-method: param sAttribute working again to check boundaries of match regions
* sample-method for objects of class kwic and context
* kwic, cpos, and context method will accept queries of length > 1
* use-function and resetRegistry-function reworked
* more explicit startup message to get info about version, registry and interface
* encoding issues solved for size-method, hits-method and dispersion-method
* use-function will now work for users working with polmineR.Rcpp as interface

# v0.7.3
* new installed.corpora() convenience function to list all data packages with corpora
* view-method and show-method for cooccurrences-objects now successfully redirect
output to RStudio viewer
* data.table-style indexing of objects inheriting from textstat-class
* for windows compatibility, as.corpusEnc/as.nativeEnc for encoding conversion
* performance gain for size-method by using polmineR.Rcpp
* dissect-method dropped (replaced by size)
* improved documentation of size-method
* labels for cooccurrences-output
* cooccurrencesBundle-class and cooccurrence-method for bundle restored
* as.data.table for cooccurrencesBundle-class
* count-method for whole corpus for pAttribute > 1
* functionality of meta-method merged into sAttributes-method (meta-method dropped)
* speed improvements for generating html output for reading
* previously unexported highlight-method now exported, and more robust than before (using xml2)
* progress bars for multicore operations now generated by pbapply package
* starting to use testthat for unit testing

# v0.7.2
* updated documentation of partition-method.
* documentation of hits-method improved
* use-methode: default value for pkg ist NULL (return to default registry), function more robust
* Rework for parsing the registry
* rework of templates, are part of options now (see ?setTemplate, ?getTemplate)
* experimental use of polmineR.Rcpp-package for fast counts for whole corpus
* new convenience function install.corpus to install CWB corpus wrapped into R data package
* adjustments to make package compatible with polmineR.shiny
* cpos-method to get hits more robust if there are not matches for string
* hits-method removes NAs
* compare-method renamed to features-method
* warnings caused by startup on windows removed

# v0.7.1
* size-method now allows for a param 'sAttribute'
* hits-method reworked, allows for names query vectors
* first version that can be installed on windows

# v0.7.0
* rcqp package moved to suggests, to facilitate installation


# v0.6.3
* more generic implementation of as.markdown-method to prepare use of templates
* LICENSE file updated
* getTokenStream,character-method: new default behavior for params left and right
* use of templates for as.markdown-method
* Regions and TokenStream class (not for frontend use, so far)
* getTermFrequencies-method merged into count-method
* Corpus class introduced
* decode- and encode-methods introduced

# v0.6.2
* refactoring of context-method to prepare more consistent usage
* progress bar for context-method (using blapply)
* progress bar for partitionBundle (using blapply)
* more coherent naming of parameters in partitionBundle-method
* partitionBundle,character-method debugged and more robust
* usage of blapply in as.speeches-method
* hits-method: paramter cqp defaults to FALSE for hits-method, size defaults to FALSE
* new parameter cqp for dispersion-method
* aggregation for dispersion-method when length(sAttribute) == 1
* bugfix for ngrams-method, sample code for the method

# v0.6.1
* configure file removed to avoid unwanted bugs

# v0.6.0
* this is the first version that passes all CRAN tests and that is available via CRAN
* the 'rcqp' remains the interface to the CWB, but usage of rcqp functions is wrapped into an new new CQI.rcqp (R6) class. CQI.perl and CQI.cqpserver are introduced as alternative interfaces to prepare portability to Windows systems
* code in the vignette and method examples will be executed conditionally, if rcqp and the polmineR.sampleCorpus are available
* the polmineR.sampleCorpus package is available in a drat repo at www.github.com/PolMine
* a series of bug fixes

# v0.5.6
  * slot tf renamed to stat, class is data.table now
  * keyness_method moved to data.tables


# v0.5.3
  * renamed collocations to cooccurrences, seems more appropriate
  
# v0.5.2
  * multicore for term frequency counts (param for partition)

# v0.5.0
  * renamed xxxCluster to xxxBundle, bundle-superclass introduced
  * slot label/labels renamed to name/names
  * name/names-method instead of label/labels

# v0.4.57
  * debugging of tf,partition-method: There was an error, if all hits for a query in a corpus
    were obtained outside of the partition
  * tf,context-method to provide quick access to number of query results
  * view-method as a wrapper for View
  * multicore for tf-method if cqp=TRUE

# v0.4.56
  * pAttributes-method for character, and partition objects
    for easy access to inspect available p-attributes
  * (hidden) helper functions .parseRegistry and .parseInfoFile
  * automatic detection of corpus type, if .info file is available
  * def parameter of partition-method may be NULL, and anchor element
    will be read from .info-file

# v0.4.49
  * cooccurences method for partition objects

# v0.46
  * multicore for keyness,partitionCluster-method

# v0.45
 * bubblegraph-method removed, turned into an independent package
   (available at github.com/ablaette/bubblegraph)
 * tag-method using the treetag function from the koRpus package

# v0.41
 * DataTables-functionality moved to DataTablesR-package which is imported

# v0.40
 * browse-method for textstat objects: use DataTables.js

# v0.39
 * reshape collocations-class-objects with trim(object, reshape=TRUE)
 * collocationsReshaped-class
 * keynessCollocations-class

# v0.37
 * turned partition in a character-method
 * verbose=FALSE really implemented for 

# v0.36
 * context,collocations-method
 * call-slot in most objects
 * as.TermDocumentMatrix,collocations-method finalized

# v0.35
 * textstat-class introduced to serve as superclass for keyness- and context- classes
 * chisquare-, ll-, pmi-methods for statistics
 * collocations-method introduced
 * multicore for partition-constructor (parallel preparation of tf-lists)
 * parallelisation of as.sparseMatrix,collocations-method
 * introduction of keyness,collocations-method

# v0.4.34
 * plot-method for partitionCluster
 * rm.blank-functionality extended

# v0.4.33
 * code re-ordered


# v0.4.32
 * meta,character-method to learn about a corpus without first generating a partition
 * rudimentary barplot-method for partitionCluster
 * functionality to remove empty rows in DocumentTermMatrix upon construction
 * tf/idf-weighting included in as.TermDocumentMatrix
 * summary-method for keynessCluster-class
 * [[- method for keynessCluster-class


# v0.4.31
 * some changes to context method:
    - whether to use multicore can be stated explicitly
    - stopwords renamed to stoplist, and a positivelist is introduced
 * individual documentation for partitionCluster enrich-method
 * NULL object returned for partition call if s-attribute/value-combination not available
 * call to dispersion (2dim): metadata are set up if not available
 * as.data.frame-method for context and keyness class to access statistics table more easily

# v0.4.30
 * minor bug fixes

# v0.4.29
 * controls() function for setting drillingControls
 * mail concordances
 * rework of kwic as a S4 method for partition and context class
 * speeches method
 * tf for partitionCluster improved

# v0.4.28
 * reorganization of the code in files so that shift to S4 methods is reflected
 * documentation for trim method
 * documentation for enrich method
 * addPos method integrated into enrich method [to be checked]
 * addPos is kept as a method, but not exported into namespace
 * set up of missing metadata for dispersion
 * warning if labels are missing in tf method for partitionCluster
 * Encoding of partition labels adjusted to encoding of console
 * adjust encoding for input to partitionCluster
 * speeches method is drafted at end of partition.R [final development, debugging]
 * context method: no explicit statement of posFilter required [to be checked]
 * methods for adjusting crosstab objects fused into trim,crosstab-method [BUG, needs to be checked]


# v0.4.27
 * context and contextCluster functions turned into methods

# v0.4.26
 * extended export functionality: mail statistics
 * parameters for partition and partitionCluster call simplified (tf, meta)
 * sAttributes method for character vectors to get sAttributes of corpus

# v0.4.25
 * html method to inspect partitions

# v0.4.24
 * tf method for partition and partitionCluster
 * summary for partitionCluster
 * selective setup of metadata for speeding up things

# v0.4.23
 * stopwords option introduced in context function for filtering and brute disambiguation
 * cqpQuery class thrown out again - it does not improve usability
 * partitionCluster and contextCluster are now S4 classes, with some more methods
 * adjust function is now trim method for contextCluster objects

# v0.4.22
 * zoom function added to specify partitions
 * partitionCluster faster as it relies on zoom function
 * context function will work on character vectors and cqpQuery object
 * some modifications of backend functions

# v0.4.21
 * technical update: automatic generation of NAMESPACE file with roxygen

# v0.4.20
 * new cqpQuery-class introduced
 * distribution-function renamed as dispersion
 * trim function for sorting tables

# v0.4.19
 * documentation streamlined, package fully roxygenized

# v0.4.18
 * using options (not exported list drillingControls)
 * getting Encoding of corpus from registry

# v0.4.17
 * method 'addPos' added for partition object, and keyness objects 

# v0.4.16
 * inclusion of sample Data

# v0.4.15
 * reduction of dependencies for publication of the package on CRAN

# v0.4.14
 * adapting partition and distribution functions to cope with nested xml
 
# v0.4.13 
 * wordCloud visualization
 * usability of concordances improved

# v0.4.12
 * partitionMerge changed so that it will use full functionality of partition-function
 * helper functions for distribution on two dimensions improved, tremendous gain in speed
 * started to fill in sample code into vignette

# v0.4.12
 * new function for frequency counts at partition setup

# v0.4.11
 * inclusion of shiny apps
 * function 'distribution' as wrapper for functions to inspect distribution

# v0.4.10
 * improved usability of the package, started to use lowerCamelCase

# v0.4.9
 * partition can now be called without explicitly stating a label
 * partition does not require sAttributes to be set, function .sattributes2cpos streamlined
 * no labels in partition.cluster
 * in context, the pos.filter can also be set as an exclusion   

# v0.4.8
 * started to make functions more usable by shortening function names, 'partition.init' is now 'partition'
 * xterm highlighting for collocates
 * context can be called with parameters given by drillingControls

# v0.4.7
 * xtermStyle used for kwic output on console
 * partition object can be indexed

# v0.4.6
 * partition.init expanded to allow for the generation of partitions with specified start and end dates
 * wordscore analysis adapted so that it can be really used for performing wordscore analysis

# v0.4.5
 * export functionality to tm introduced with as.TermDocumentMatrix.partitioncluster

# v0.4.4
 * combine.collocates improved (new columns for plotting)

# v0.4.3
 * bug-fix for partition.merge

# v0.4.1
 *  partition.init can be used without setting up frequency lists and metadata. This may be useful, if a quick partition.init is desired and term frequencies and/or metadata are not needed.
 *  partition.init can handle sattribute-lists with length == 1. partition.init will still not work, if no sattributes are given.
 *  query.crosstab has been renamed to crosstab
 *  crosstab will accept special characters (transforming them to .)
