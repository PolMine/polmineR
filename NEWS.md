polmineR 0.7.11
===============

## NEW FEATURES

  * A `Cooccurrences()`-method and a `Cooccurrences`-class have been migrated from the (experimental) polmineR.graph package to polmineR to generate and manage all cooccurrences in a corpus/`partition`. A `cooccurrenes()`-method produces a subset of `Cooccurrences`-class object and is the basis for ensuring that results are identical.
  * New functionality to make using corpora more robust when paths include special characters: There is now a temporary data directory which is a subdirectory of the per-session temporary directory. A new function `data_dir()` will return this temporary data directory. The `use()`-function will now check for non-ASCII characters in the path to binary corpus data and move the corpus data to the temporary data directory (a subdirectory of the directory returned  by `data_dir()`), if necessary. An argument `tmp` added to `use()` will force using a temporary directory. The  temporary files are removed when the package is detached. 
  * Experimental functionality for a non-standard evaluation approach to create subcorpora via a `zoom()`-method. See documentation for (new) `corpus`-class (`?"corpus-class"`) and extended documentation for `partition`-class (`?"partition-class"`). A new `corpus()`-method for character vector serves as a constructor. This is a beginning of somewhat re-arranging the class structure: The `regions`-class now inherits from the new `corpus`-class, and a new `subcorpus`-class inherits from the `regions`-class.
  * A new function `check_cqp_query()` offers a preliminary check whether a CQP query may be faulty. It is used by the `cpos()`-method, if the new argument `check` is TRUE. All higher-level functions calling `cpos()` also include this new argument. Faulty queries may still cause a crash of the R session, but the most common source is prevent now, hopefully.
  * A `format()`-method is defined for `textstat`, `cooccurrences`, and `features`, moving the formatting of tables out of the `view()`, and `print()`-methods. This will be useful  when including tables in Rmarkdown documents.


## MINOR IMPROVEMENTS

  * Startup messages reporting the package version of polmineR and the registry path are omitted now.
  * The functions `registry()` and `data_dir()` now accept an argument `pkg`. The functions will return the path to the registry directory / the data directory within a package, if the argument is used.
  * The `data.table`-package used to be imported entirely, now the package is imported selectively. To avoid namespace conflicts, the former S4 method `as.data.table()` is now a S3 method. Warnings appearing if the `data.table` package is loaded after polmineR are now omitted.
  * The `coerce()`-methodes to turn `textstat`, `cooccurrences`, `features` and `kwic` objects into htmlwidgets now set a `pageLength`.
  * New methods for `partition_bundle` objects: `[[<-`, `$`, `$<-`
  * Rework of indexing `textstat` objects.
  * A slot `p_attribute` has been added to the `kwic`-class; `kwic()`-methods and methods to process `kwic`-objects are now able to use the attribute thus indicated, and not just the p-attribute "word".
  * A new `size()`-method for `context`-objects will return the size of the corpus of interest (coi) and the reference corpus (ref).
  * New `encoding()`-method for character vector.
  * New `name()`-method for character vector.
  * A new `count()`-method for `context`-objects will return the `data.table` in the `stat`-slot with the counts for the tokens in the window.
  * The `decode()`-function replaces a `decode()`-method and can be applied to partitions. The return value is a `data.table` which can be coerced to a `tibble`, serving as an interface to tidytext (#37).
  * The `ngrams()`-method will work for corpora, and a new `show()`-method for `textstat`-object generates a proper output (#27).


## BUG FIXES

  * Any usage of `tempdir()` is wrapped into normalizePath(..., winslash = "/"), to avoid mixture of file separators in a path, which may cause problems on Windows systems.
  * In the calculation of cooccurrences, the node has previously been included in the window size. This has been corrected.
  * The `kwic()`-method for corpora returned one surplus token to the left and to the right of the query. The excess tokens are not removed.
  * The object returned by the `kwic()`-method for `character`-objects method did not include the correct position of matches in the `cpos` slot. Corrected.
  * Bug removed that occurrs when context window reaches beyond beginning or end of a corpus (#48).
  * When generating a `partition_bundle` using the `as.speeches()`-method, an error could occur when an empty partition has been generated accidentaly. Has been removed. (#50)
  * The `as.VCorpus()`-method is not available if the `tm`-package has been loaded previously. A coerce method (`as(OBJECT, "VCorpus")) solves the issue. The `as.VCorpus()`-method is still around, but serves as a wrapper for the formal coerce-method (#55).
  * The argument `verbose` as used by the `use()`-method did not have any effect. Now, messages are not reported as would be expected, if `verbose` is `FALSE`. On this occasion, we took care that corpora that are activated are now reported in capital letters, which is consistent with the uppercase logic you need to follow when using corpora. (#47)
  * A new check prevents an error that has occurred when a token queried by the `context()`-method would occurr at the very beginning or very end of a corpus and the window would transgress the beginning / end of the corpus without being checked (#44).
  * The `as.speeches()`-function caused an error when the type of the partition was not defined. Solved (#57).
  * To deal with issues resulting from an unset locale, there is a check during startup whether the locale is unset (i.e. 'C') (#39).
  * There was a difficulty to generate a `TermDocumentMatrix` from a `partition_bundle` if the partitions in the `partition_bundle` were not named. The fix is to assign integer numbers as names to the partitions (#58).


## DOCUMENTATION FIXES

  * Substantial rework of the documentation of the `ll()`, and `chisquare()`-methods to make the statistical procedure used transparent.
  * Expanded documentation for `cooccurrences()`-method to explain subsetting results vs applying positivelist/negativelist (#28).
  * Wrote some documentation for the `round()`-method for `textstat`-objects that will show up in documentation of `textstat` class.
  * Improved documentation of the `mail()`-method (#31).
  * In the examples for the `decode()`-function, using the REUTERS corpus replaces the usage
    of the GERMAPARLMINI corpus, to reduce time consumed when checking the package.
  


polmineR 0.7.10
===============

### NEW FEATURES

  * The package now offers a simplified and seamless workflow for dictionary-based sentiment analysis: The `weigh()`-method has been implemented for the classes `count` and `count_bundle`. Via inheritance, it will also be available for the `partition`- and `partition_bundle`-classes. Then, a new `summary()`-method for `partition`-class objects is introduced. If the object has been weighed, the list that is returned will include a report on weights. There is an example that explains the workflow.
  * The `partition_bundle`-method for `context`-objects has been reworked entirely (and is working again);
a new `partition`-method for `context`-objects has been introduced. Buth steps are intended for workflows for dictionary-based sentiment analysis.
  * The `highlight()`-method is now implemented for class `kwic`. You can highlight words in the neighborhood of a node that are part of a dictionaty.
  * A new `knit_print()`-method for `textstat`- and `kwic`-objects offers a seamless inclusion of analyses in  Rmarkdown documents.
  * A `coerce()`-method to turn a `kwic`-object into a htmlwidget has been singled out from the `show()`-method for `kwic`-objects. Now it is possible to generate a htmlwidget from a kwic object, and to include the widget into a Rmarkdown document.
  * A new `coerce()`-method to turn `textstat`-objects into an htmlwidget (DataTable), very useful for  Rmarkdown documents such as slides.
  * A new argument height for the `html()`-method will allow to define a scroll box. Useful to embed a fulltext output to a Rmarkdown document.


### MINOR IMPROVEMENTS

  * The `partition_bundle`-class, rather than inheriting from `bundle`-class directly, will now inherit from the `count_bundle`-class
  * The `use()`-function is limited now to activating the corpus in data packages. Having introduced the session registry, switching registry directories is not needed any more.
  * The `as.regions()`-function has been turned into a `as.regions()`-method to have a more generic tool.
  * Some refactoring of the `context`-method, so that full use of `data.table` speeds up things.
  * The `highlight()`-method allows definitions of terms to be highlighted to be passed in via three dots (...);
no explicit list necessary.
  * A new `as.character()`-method for kwic-class objects is introduced.


### BUG FIXES

  * The `size_coi`-slot (coi for corpus of interest) of the `context`-object included the node; the node (i.e. matches for queries) is excluded now from the count of size_coi.
  * When calling `use()`, the registry directory is reset for CQP, so that the corpora in the package that have been  activated can be used with CQP syntax.
  * The script configure.win has been removed so that installation works on Windows without an installation of Rtools.
  * Bug removed from `s_attributes()`-method for `partition`-objects: "fast track" was activated without preconditions.
  * Bug removed that would swallow metadata/s-attributes to be displayed in `kwic`-output after highlighting.
  * As a matter of consistency, the argument `meta` has been renamed to `s_attributes` for the `kwic()`-method for `context`-objects, and for the `enrich()`-method for `kwic`-objects.
  * To avoid confusion (with argument s_attributes), the argument `s_attribute` to check for integrity within 
a struc has been renamed into `boundary`.


### DOCUMENTATION FIXES

  * Documentation for `kwic`-objects has been reworked thoroughly.


polmineR 0.7.9
==============

  * new as.list,bundle-method for convenience, to access slot objects
  * as.bundle is more generic now, so that any kind of object can be coerced to a bundle now
  * as.speeches-method turned into function that allows partition and corpus as input
  * is.partition-function introduced
  * sAttributes,partition-method in line with RcppCWB requirements (no negative values of  strucs)
  * count repaired for muliple p-attributes
  * bug removed causing a crash for as.markdown-method when cutoff is larger than number of tokens
  * polmineR will now work with a temporary registry in the temporary session directory
  * a (new) registry_move() function is used to copy files to the tmp registry
  * the (new) registry() function will get the temporary registry directory
  * the use() function will add the registry file of a package to the tmp registry
  * a bug removed that has prevented the name<- method to work properly for bundle objects
  * new partition_bundle,partition_bundle-method introduced
  * naming of methods and functions, classes and most arguments moved to snake_case, maintaining backwards compatibility
  * utility function getObjects not exported any more
  * for count,partition_bundle-method, column 'partition' will be a character vector now (not factor)
  * new argument 'type' added to partition_bundle
  * new method 'get_type' introduced to make getting corpus type more robust
  * bug removed that has caused a crash when cutoff is larger than number of tokens in a partition when calling get_token_stream
  * count-method will now return count-object if query is NULL, making it easier to write pipes


polmineR 0.7.8
==============

  * upon loading the package, check that data directories are set correctly in registry files to make sure that sample data in pre-compiled packages can be used
  * startup messages adjusted slightly


polmineR 0.7.7 
==============

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


polmineR 0.7.6
==============

  * functionality of matches-method (breakdown of frequencies of matches) integrated
into count-method (new param breakdown)
  * corpus REUTERS included (as data for testsuite)
  * adjust data directory of REUTERS corpus upon loading package
  * a pkgdown-generated website is included in the docs directory
  * consistent use of .message helper function to make shiny app work
  * bug removed for count-method when options("polmineR.cwb-lexdecode") is TRUE and options("polmineR.Rcpp") is FALSE
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
  * the class Corpus now has a slot sAttribute to keep/manage a data.table with corpus positions and struc values, and there is a new partition,Corpus-method. In compbination, it will be a lot faster to derive a partition, particularly if you need to do that repeatedly
  * a new function install.cwb() provides a convenient way to install CWB in the package 
  * added a missing encoding conversion for the count method


polmineR 0.7.5
==============

  * class 'Regions' renamed to class 'regions' as a matter of consistency
  * data type of slot cpos of class 'regions' is a matrix now
  * rework and improved documentation for decode- and encode-methods
  * new functions copy.corpus and rename.corpus
  * as.DocumentTermMatrix-method checks for strucs with value -1 
  * improved as.speeches-method: reordering of speeches, default values
  * blapply-method: verbose output will be suppressed of progress is TRUE


polmineR 0.7.4
==============

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


polmineR 0.7.3
==============

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


polmineR 0.7.2
==============

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


polmineR 0.7.1
==============

  * size-method now allows for a param 'sAttribute'
  * hits-method reworked, allows for names query vectors
  * first version that can be installed on windows

polmineR 0.7.0
==============

  * rcqp package moved to suggests, to facilitate installation


polmineR 0.6.3
==============

  * more generic implementation of as.markdown-method to prepare use of templates
  * LICENSE file updated
  * getTokenStream,character-method: new default behavior for params left and right
  * use of templates for as.markdown-method
  * Regions and TokenStream class (not for frontend use, so far)
  * getTermFrequencies-method merged into count-method
  * Corpus class introduced
  * decode- and encode-methods introduced


polmineR 0.6.2
==============

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


polmineR 0.6.1
==============

  * configure file removed to avoid unwanted bugs


polmineR 0.6.0
==============

  * this is the first version that passes all CRAN tests and that is available via CRAN
  * the 'rcqp' remains the interface to the CWB, but usage of rcqp functions is wrapped into an new new CQI.rcqp (R6) class. CQI.perl and CQI.cqpserver are introduced as alternative interfaces to prepare portability to Windows systems
  * code in the vignette and method examples will be executed conditionally, if rcqp and the polmineR.sampleCorpus are available
  * the polmineR.sampleCorpus package is available in a drat repo at www.github.com/PolMine
  * a series of bug fixes


polmineR 0.5.6
==============

  * slot tf renamed to stat, class is data.table now
  * keyness_method moved to data.tables


polmineR 0.5.3
==============

  * renamed collocations to cooccurrences, seems more appropriate
  
polmineR 0.5.2
==============

  * multicore for term frequency counts (param for partition)

polmineR 0.5.0
==============

  * renamed xxxCluster to xxxBundle, bundle-superclass introduced
  * slot label/labels renamed to name/names
  * name/names-method instead of label/labels


