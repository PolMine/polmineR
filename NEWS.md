polmineR 0.8.1
==============

## New Features

- The `decode()`-method now entails the possibility to decode structural and positional
attributes selectively, via the new arguments `p_attributes` and `s_attributes` (#116).
Internally, the reliance on `coerce()`-methods has been replaced by a simpler 
if-else-syntax. The `as(from, "Annotation")` option persists, however.
- A new argument `phrases` was added to the `count()`-method for `partition_bundle` objects.
- The slots "user" and "password" of the `remote_corpus` and the `remote_subcorpus` class are replaced by a single slot `restricted` (values `TRUE`/`FALSE`) to indicate if a user name and a password are necessary to access a corpus. A file following the conventions of CWB files is assumed to include the credentials for corpus access. This approach avoids the accessibility of the password. 
- Using the temporary registry file can be suppressed by setting the environment variable POLMINER_USE_TMP_REGISTRY as 'false'. (Background: Necessary to deal with changing temporary directories when polmineR is preloaded in an OpenCPU context.)
- The Dockerfile included in the package (./inst/docker/debian_polminer_min) prepares a Debian image with a minimal installation of polmineR that will be available at the 'polmine' repository at dockerhub (see `https://hub.docker.com/r/polmine/debian_polminer_min`). 
- The `corpus()`-method that serves as a constructor either for the `corpus` or the `remote_corpus` class does not flag default values for the arguments `user` and `password` any more. If the argument `server` is stated explicitly (not `NULL`, default), these variables will get the value `character()`. This way, a set of if/else statements can be omitted and it is much easier to implement methods for the `remote_corpus` class for corpora that are password-protected, or not.
- There is now a definition of an S3 `as.list.bundle()`-method (previously, there has only been the S4 method). The beautiful effect is that `lapply()` and `sapply()` can be used on `bundle` objects now (a `subcorpus_bundle`, for instance)
- The performance of the `count()`-method for `partition_bundle` objects has been improved, it is twice as fast now (#137).
- The `p_attributes` method now accepts an argument `decode`.
- The `p_attributes`-method has been implemented for `partition_bundle` objects.
- In the shiny app you can launch via `polmineR()`, the mail-button has been dropped in the kwic, and code can be displayed (using code highlighting)
- The settings have been dropped from the shiny app altogether, as we have the buttons now
the `phrases` argument is used are now also available when a `phrases` object is not passed in.
- Code buttons have been added to the shiny app experimentally.
- The `get_token_stream()`-method for `partition_bundle` objects will now accept an argument `phrases`(#128).
- The `merge()`-method for `partition_bundle`-objects has been reworked: Substantial performance improvement by relying on `RcppCWB::get_region_matrix`. Internally, the method performs a check whether the `partition`/`subcorpus` objects to be merged are non-overlapping. The default value for the argument `verbose` is now `FALSE`, as waiting time is much shorter.


## Minor Improvements

- A new option `polmineR.warn.size` can be used to control the issuing of warnings
for large `kwic` objects.
- Indexing `Cooccurrences` objects had not been possible, now at least using integer
  indices is possible (#114).
- Introduced experimentally a feature to count phrases in the `count()`-method for 
  `slice` class objects.
- The `corpus()` method for a character vector will not abort gracefully with a 
  message if more than one corpus is offered as `.Object`.
- The `Cooccurrences()`-method will now accept zero values (0) for the arguments
  `left` and `right`. Relevant for detecting bigrams / phrases.
- When sorting the results `data.table` of a `Cooccurrences` object, the NA values are
  pushed to the end of the table now.
- A new `concatenate()` method is a worker to collapse tokens into phrases.
- Implemented pointwise mutual information (PMI) for `Cooccurrences` class objects, see
  `pmi()`-method.
- Implemented a `ngrams()`-method for class `data.table` - useful if you need to work
  with decoded corpora.
- Implemented the `pmi()`-method for the `ngrams()`-method, to provide a workflow for
  phrase detection.
- A new method `enrich()` for object of class `Cooccurrences` will add columns with counts
  for the co-occurring tokens to the `data.table` in the slot 'stat'.
- Removed an inconsistency with the naming of the columns of the `data.table` in the `stat`
  slot of an `ngrams` object: Column names will now be "word_1" , "word_2" etc.
- Defined an explicit method `count()` for `subcorpus_bundle` objects (just callling `callNextMethod()` internally) -  useful to see the availability of the method in the documentation object.
- The `as.speeches()`-method for `corpus` objects now supports parallelization
- A unit test checks different methods for generating a `DocumentTermMatrix` against each other, as a safeguard that different approaches might lead to different results (#139).
- New class `phrases` and `as.phrases()`-method for `ngrams` and `matrix` objects. The
  `count()`-method now accepts an argument `phrases`. See the documentation (`?phrases`).
- The `s_attributes()`-method is now consistent with the usage of the `unique` argument (#133).
- The `hits()`-method for `partition_bundle` objects now accepts an argument `s_attribute` to include metadata in results (#74).
- The `check_cqp_query()` function now has a further argument `warn`. If `TRUE` (default), a warning is issued, if the query is buggy. The `as.phrases()`-method will use the function to avoid that buggy CQP queries may be generated.
- If no template is set, no reliance on a plain and simple template, and telling error messages, if no template is available (#123).
- The `Corpus` class has been re-introduced (temporarily), to avoid an issue with the GermaParl package if the class is not available (#127).
- The `get_template()`-method is now defined for the `corpus` class.
- The `count()`-method with arguments `breakdown` is `TRUE` and `cqp` is `TRUE` has been awfully slow. Fast now.
- Decoding a p-attribute has seen a substantial performance improvement (#130). A new argument `boost` allows user to opt for the improvement, which will involve decoding the lexicon directly.
- The `merge()`-method is implemented for `subcorpus_bundle` objects now, and has been implemented for `subcorpus` objects (#76).
- Generating a `kwic` view from a `cooccurrences` object based on more than one p-attribute will work now (#119).
- The `decode()`-method has been defined for `integer` vectors. Internally it will decide whether decoding token ids is speeded up by reading in the lexicon file directly. The behavior can be triggered explicitly by setting the argument `boost` as `TRUE`.
- The `get_token_stream()`-method will use the new `decode()`-method for integer values internally. The argument `boost` is used by the `get_token_stream()` to control the approach.
- Improvements of performance initially implemented for `get_token_stream` for `partition_bundle` when 
- Internally, the `partition_bundle()`-methods defined for `character`, `corpus` and `partition` objects now call the `split()`-methods for `corpus` and `subcorpus` objects, resulting in a huge performance gain (#112).
- Zero values can be processed by `Cooccurrences()`-method (#117).
- The `corpus` class includes a (new) slot `size`, just as the `regions` and the `subcorpus` classes.
- The `split()`-method for `corpus` objects now accepts the argument `xml`, to indicate whether the annotation structure of the corpus is flat or nested.
- The definition of the S4 class `partition` now includes a prototype defining default values for the slots 'stat' (a `data.table`) and the slot 'size' (`NA_integer_`). This avoids that an incomplete initialization of a `partition` object will result in an error.
- The `kwic()`-method is now available for `partition_bundle`/`subcorpus_bundle`-objects (#73).
- To make the `kwic()`-method work correctly for `partition` objects that result from a `merge()`
  operation (and that were not created by ), the `cpos()`-method for `slice` objects will extract
  strucs based on the s-attribute defined in the slot `s_attr_strucs` rather than the last s-attribute
  in the list of the slot `s-attributes`.
- Class `subcorpus` is exported for usage in other packages.
- The default value of the argument `progress` of the `count()`-method for `partition_bundle` objects is now FALSE.
- The `get_type()`-method is now defined for the `corpus` class.
- Upon starting the shiny app included in the package, the presence of packages "shiny" 
and "shinythemes" is checked. If the packages are not yet present, an optional install
is offered (#110).
- A coerce method has been defined to turn a `corpus` object into a `subcorpus` object, to recover
functionality used (internally) that relied on the former `Corpus` reference class.
- The `Cooccurrences()`-method is now defined for the `corpus`-class, too. The `Cooccurrences()`-method 
for the `character` class now relies on this method.
- The deprecated `Corpus` reference class has been dropped from the code altogether: As `roxygen::roxygenize()` started to check the documentation of R6 classes and reference classes, the poor documentation of this class started to provoke many errors. Rather than starting to write documentation for a deprecated class, getting rid of an outdated and poorly documented class appeared to be the better solution.
- New coerce method to derive a `kwic` object from a `cooccurrences` object. Introduced to
  serve as a basis for quantitative/qualitative workflows, e.g. integrated in a flexdashboard.
- There is now a telling error message for the `s_attributes()` method for `corpus` objects when 
values are requested for an s-attribute that does not exist (#122).
- In the `decode()`-method for `subcorpus` objects, s-attributes were not decoded appropriately (#120). Fixed. When decoding a corpus/subcorpus, the struc column is kept (again).
- A new check in `.onLoad()` whether polmineR is loaded from the repository directory will ensure that temporary registry files will not be gone when calling `devtools::document()` (#68).


## Bug Fixes

- In the `as.speeches()`-method for `corpus` objects, setting `progress` as `FALSE` did not
  suppress the display of a progress bar. Solved.
- Removed a bug that occurred when counting matches for CQP queries over a `subcorpus_bundle`
  that resulted from CQP queries being turned into invalid column names.
- Solved: No longer an error when calling polmineR commands after having worked in the shiny app context (#111).
- A bug caused when the name of an object in a `partition_bundle` was an empty string and calling `count()` on this object has been removed (#121).
- A bug was addressed that occurs when unfolding the region matrix where all regions have the same length (#124).


## Documentation

- A skeleton documentation of package options is included in the documentation of the package as a whole (`?polmineR`)


polmineR 0.8.0
==============

## New Features

- The `corpus` class has been put in a shape to become the default point of 
  departure of most workflows. All core methods are now available for the 
  `corpus` class, and have been implemented newly if necessary, e.g. `show()` 
  and `size()`-method. The constructor method for a  `corpus` object, the 
  `corpus()` method, will now check whether the character vector with the corpus 
  ID refers to an available corpus, whether all letters are upper case and
  issue informative warnings and error messages.
- The `s_attributes()`-method for `corpus` objects has been reworked: It will decode 
  binary files directly, without reliance on the corpus library functions, which is
  significantly faster.
- The `Corpus` reference class is now obsolete after the introduction of the
  S4 `corpus` class. To maintain the functionality not covered otherwise, 
  new generics `get_info` and `show_info` have been introduced and defined 
  for the `corpus` class.
- Methods available for the `subcorpus` class have been expanded so that this
  class can supersede the `partition` class: Methods newly available are 
  `cpos()`, `count()`, `p_attributes()`,  `s_attributes()` `get_token_stream()`,
  and `size()`. Technically, there is virtual `slice`-class, from which
  `subcorpus` inherits (methods called via `callNextMethod()`). 
- A new `subset()`-method for the `corpus` and `subcorpus` classes to generate subcorpora
  (i.e. `subcorpus` objects) has been introduced. It outperforms the
  `partition()` method. The `subset()`-method for `corpus` and `subcorpus` objects
  will be the default way to work with non standard evaluation in a manner that
  feels "R-ish" (#40).
- The `zoom()`-method that has been introduced experimentally has 
  been dropped again in favor of the `subset()`-method to get `subcorpus` objects 
  from `corpus` and `subcorpus` objects. A set of experimental methods for an
  initial check of the feasibility of a non-standard evaluation approach to
  the generation of subcorpora has been dropped (methods `$`, `==`, `!=`,
  `zoom` for `corpus`-class). 
- To facilitate the transition from the `partition` class (inheriting from 
  the `textstat` class) to the `subcorpus` class (inheriting from the `textstat`
  class), there is a new `coerce()`-method to turn a `partition` object into 
  a `subcorpus` object.
- A new `remote_corpus`-class is the basis for accessing remote
  corpora. A `remote_subcorpus` can be derived from a `remote_corpus`. Methods
  available for remote corpora und subcorpora remain limited at this stage.
- Consolidation of the class system: For all the S4 classes in the package, multiple
  contains have been checked, and multiple contains have been removed.
- The `subcorpus_bundle` class now inherits from `partition_bundle`. This is not
  intended to be a long-term solution, but facilitates the implementation of new
  workflows based on the `subcorpus` class rather than the `partition` class.
- Calling the polmineR shiny app via `polmineR` did not have safeguards if
  the suggested packages [shiny](https://CRAN.R-project.org/package=shiny) and [shinythemes](https://CRAN.R-project.org/package=shinythemes) were not installed. Now
  there will be a conditional installation of the packages required for running
  the shiny app.
- The somewhat odd class `CorpusOrSubcorpus` has been removed. The `ngrams`-method
  now applies for `corpus` and `subcorpus` objects.
- The pipe operator of the [magrittr](https://CRAN.R-project.org/package=magrittr) package is imported now, and magrittr has moved
  from a suggested package to a required package.
- The `label()`-method, present for a while, is superseded by a `edit()`-method now.
  It will call a shiny gadget either using DataTables or Handsontable. The former 
  `Labels` reference class has been turned into a S4 class, because the
  desired reference logic can also be achieved with a `data.table` in a slot of
  the labels class.
- The `table`-slot of the `kwic` class has been renamed as `stat` slot (a `data.table`),
  so that the `kwic` class can now inherit from the `textstat` class. The
  `enrich()`-method for objects of class `kwic` now includes a new argument 
  `extra` that will add extra tokens to the left of the windows for concordances so
  that qualitative inspections for query hits can work with more context.
- The `as.TermDocumentMatrix()` and the `as.DocumentTermMatrix()`-methods are now
  also defined for `kwic` objects. They work exactly the same as for the `context`
  class. To avoid having to write new methods, a new `neighborhood` virtual class has
  been introduced. The aforementioned methods are defined for the virtual class and
  are available for context and kwic class objects.
- Added CQP functionality to count tab in shiny app, and to the dispersion tab.
- There is now a basic implementation of `get_token_stream()` for a `partition_bundle`
  object.
- The `Cooccurrences()`-method is now available for `subcorpus`-objects (#88).
- There is a new coerce method to turn a `kwic`-object into a `context`-object.
  The `neighborhood` virtual class could be discarded again, and a bug could be removed
  that left an `enrich()`-operation for `kwic` objects (argument `p_attribute`)
  ineffectual (#103).
- A potential error resulting from setting argument `cpos` to `FALSE` in the `kwic()`-method
has been solved (#106), and the documentation of the argument has been rewritten so that
  includes a warning to use the argument falsely.
- If the properties "version" and "build_date" are available in a registry file, the information
will be shown when calling `use()` (#72).


## Minor changes

- Added a new argument `regex` to the `cpos()`-method (for `corpus` objects), which
  will interpret argument `query` as a regular expression. This may be faster than
  taking `query` as an outright CQP query.
- The configure-script in the package that would adjust paths in the registry files
  for the corpora included in the package for documentation and testing purposes has
  been removed. Having switched to a temporary registry directory, it has lost 
  its function.
- The version of the data.table package now required is 1.12.2, because previous
  versions did not allow adding columns to a new data.table.
- Implemented the possibility to use multiple queries in `dispersion`-method (#92).
- To keep up with the renaming of functions and arguments in the package, "sAttributes"
  and "pAttributes" in the polmineR shiny app have been renamed ("s_attributes",
  and "p_attributes", respectively).
- The shiny app module for kwic output will not show `p_attribute` and `positivelist`
  by default.
- The `format()`-method is used to create proper output in the cooccurrences of the
  shiny app.
- User names that include non-ASCII characters were a persistent problem on Windows
  machines (#66). The solution now is to check for non-ASCII characters in the path
  to the data directory, and to use the "old" short DOS path if necessary. The worker is 
  a modified `registry()`-function.
- The ordering of the table for `ll`-method had been somewhat mixed up, which is repaired
  now. Tokens with NA values for the ll-test will show up at the end of the table.
- The `registry_move()`-function, used only internally at this stage, is exported now 
  so that it can be used by other packages.
- The return value of `the get_token_stream()`-method for `regions` objects was a
  `data.table`. The behavior is now in line with the other `get_token_stream()` methods
- The `tempcorpus()`-method and the `tempcorpus` class have been removed from the package,
  having become utterly deprecated.
- The `summary()`-method for `partition`-class objects has been turned into a method
  for the `count`-class, to eliminate an inconsistency. The example of a workflow has been
  moved to the documentation object for the `count`-class.
- The `browse()`-method has not proven to be useful and has been removed from the package.
  A new `browse()`-function is introduced to throw a warning, if browse should be
  called nevertheless.
- A refactoring of the `split()`-method for `partition`-objects improved the readability
  of the code, but the performance gain is minimal.
- A new `kwic_bundle`-class has been introduced, a list of `kwic` objects can be turned
  into this new class using `as.bundle`.
- The `context()`-method will now take again as input character vectors for the arguments
  `left` and `right` to expand to the left and right boundaries of the designated
  region (#87).
- Rework of the way messages are printed to make it easy to implement notifications in
  the shiny environment.
- Default highlighting when a positivelist is supplied has been removed from the
  `kwic()`-method. This ensures that subsequent highlighting operations can assign
  new colors (#38).
- Implemented feature request for `dispersion()` that results are reported for all
  values of structural attributes, including those with zero matches. (#104)
- Performance improved for the `cpos`-method for `matrix` which unfolds a matrix with regions 
  of corpus positions, useful for operations that require many calls.
- The `count`-method for `partition_bundle` has been reworked and is much faster and more
  memory efficient. 
- `as.TermDocumentMatrix()` for `partition_bundle` optimized to work efficiently
  with large corpora.
- Introduction of a context,matrix-method to have a unified auxiliary function
  to create contexts.
- The `as.corpusEnc()`-function uses the `localeToCharset()`-function from the utils
  package to determine the charset of input strings. On RStudio Server, we have seen
  cases when the return value is NA. Then it will be assumed that the locale is UTF-8.
- Functionality to highlight terms in kwic display has been restored for the shiny app.


## Bug fixes

- Removed a bug in the `context()`/`kwic()` method that led to superfluous words in the
  right context.
- Removed a bug that occurred with the `as.data.frame()`-method for `kwic`-objects
  when no metadata were added.
- The `count()`-method for `partition_bundle`-objects did not perform `iconv()` if
  necessary - this has been corrected.
- Indexing the concordances of a `kwic` object did not reduce the `cpos` table
  concurringly. This has been corrected.
- The `as.speeches()`-method failed to handle situations correctly, when one speaker
  occurring in the corpus only contributed one single region to the entire corpus (#86). 
  This behavior has been debugged.
- Counting over a `partition_bundle` started to throw a warning that an argument arrives
  at the `cpos()`-method that is not used. The cause for the warning message is removed,
  an additional unit test has been introduced to recognize issues with the
  `count`-method (#90).
- The `kwic()`-method threw an error when trimming the matches by using a positivelist 
  or a stoplist resulted in no remaining matches. The method will now return a NULL 
  object and keep issuing a warning if no matches remain after filtering (#91).
- Chaining subsetting calls on a corpus/subcorpus omitted filling the s_attribute slot
  of the `subcorpus` object, resulting in false results when counting over 
  subcorpora. Fixed.
- Started to remove bugs in the shiny app: kwic starts to work again (bug: slot table
  has been replaced by stat).
- The part of the shiny app for dispersions did not work at all - has been repaired,
  exposing more functionality of `dispersion()` (#62).
- In the `as.speeches()`-method, the argument `verbose` was not used (#64) - this had
  been addressed when solving issue #86.
- Telling messages when sending out emails - on success and error - have been added (#61).
- A shortcoming in coerce method to turn a `subcorpus` into a `String` was removed: 
  A semicolon was not recognized as a punctuation mark. This makes decoding subcorpora
  as `Annotation` more robust. The respective unit test has been updated.
- Calling `read()` on a `kwic` object works again (#84).
- Checks for the `as.VCorpus()` method that failed are now ok (#77). The reason was
  that `get_token_stream()` assumed implicitly that a p-attribute "pos" is present,
  which is not the case for the REUTERS test corpus.
- A minor bug in the `s_attributes`-method was removed that would make retrieving the
  metadata for the first strucs (index 0) of a s-attribute impossible.
- Fixed an issue for `as.DocumentTermMatrix` that started to occur with the introduction
  of the `subcorpus_bundle` class (#100).
- Removed a bug in the `kwic`-method for `character` that prevented using different values for
  right and left context (#101).
- Removed a bug that occurred when using `as.DocumentTermMatrix()` on a corpus stated
  by corpus ID / length-one character vector (#105).
- Removed a bug from the kwic,character-method, and the context,corpus-method that
  would result in odd behavior when either the left or right context is 0.
- An endemic encoding issue for full text output on Windows machines (latin1 encoding)
  has been solved by replacing internally `markdown::markdownToHTML` by a direct
  call to `markdown::renderMarkdown`. On this occasion, some overhead preparing
  fulltext output has been removed.
- A bug that prevented getting extra left and right context for `kwic` objects has
  been removed (#102).
- The `as.TermDocumentMatrix()`-method for `neighborhood`-objects returned a 
  DocumentTermMatrix (unintendedly), this bug is removed now.


## Documentation

- Extended documentation for `pmi()`-method and `t_test()`-method.
- New `s_attributes()`-method for `corpus`-class.
- The documentation for the `corpus`-class has been rewritten entirely, and the
  documentation for the `remote_corpus`-class has been integrated, whereas methods
  applicable to the `remote_corpous`-class were integrated into the documentation
  objects for the respective methods.
- The documentation for the `get_token_stream()`-method has been reworked and expanded
  thoroughly (#65). On this occasion, test coverage for the method has been improved
  significantly. (Everything is tested now apart from parallelization.)

  
polmineR 0.7.11
===============

## NEW FEATURES

- A `Cooccurrences()`-method and a `Cooccurrences`-class have been migrated from the (experimental) polmineR.graph package to polmineR to generate and manage all cooccurrences in a corpus/`partition`. A `cooccurrenes()`-method produces a subset of `Cooccurrences`-class object and is the basis for ensuring that results are identical.
- New functionality to make using corpora more robust when paths include special characters: There is now a temporary data directory which is a subdirectory of the per-session temporary directory. A new function `data_dir()` will return this temporary data directory. The `use()`-function will now check for non-ASCII characters in the path to binary corpus data and move the corpus data to the temporary data directory (a subdirectory of the directory returned  by `data_dir()`), if necessary. An argument `tmp` added to `use()` will force using a temporary directory. The  temporary files are removed when the package is detached. 
- Experimental functionality for a non-standard evaluation approach to create subcorpora via a `zoom()`-method. See documentation for (new) `corpus`-class (`?"corpus-class"`) and extended documentation for `partition`-class (`?"partition-class"`). A new `corpus()`-method for character vector serves as a constructor. This is a beginning of somewhat re-arranging the class structure: The `regions`-class now inherits from the new `corpus`-class, and a new `subcorpus`-class inherits from the `regions`-class.
- A new function `check_cqp_query()` offers a preliminary check whether a CQP query may be faulty. It is used by the `cpos()`-method, if the new argument `check` is TRUE. All higher-level functions calling `cpos()` also include this new argument. Faulty queries may still cause a crash of the R session, but the most common source is prevent now, hopefully.
- A `format()`-method is defined for `textstat`, `cooccurrences`, and `features`, moving the formatting of tables out of the `view()`, and `print()`-methods. This will be useful when including tables in R Markdown documents.
- The `highlight()`-method for `character` and `html` objects now has the arguments `regex` and `perl`, so that regular expressions can be used for highlighting (#99).
- The `as.data.frame()`-method for `kwic`-objects has seen a small performance improvement, and is more robust now if the order of columns changes unexpectedly.


## MINOR IMPROVEMENTS

- Startup messages reporting the package version of polmineR and the registry path are omitted now.
- The functions `registry()` and `data_dir()` now accept an argument `pkg`. The functions will return the path to the registry directory / the data directory within a package, if the argument is used.
- The `data.table`-package used to be imported entirely, now the package is imported selectively. To avoid namespace conflicts, the former S4 method `as.data.table()` is now a S3 method. Warnings appearing if the `data.table` package is loaded after polmineR are now omitted.
- The `coerce()`-methodes to turn `textstat`, `cooccurrences`, `features` and `kwic` objects into htmlwidgets now set a `pageLength`.
- New methods for `partition_bundle` objects: `[[<-`, `$`, `$<-`
- Rework of indexing `textstat` objects.
- A slot `p_attribute` has been added to the `kwic`-class; `kwic()`-methods and methods to process `kwic`-objects are now able to use the attribute thus indicated, and not just the p-attribute "word".
- A new `size()`-method for `context`-objects will return the size of the corpus of interest (coi) and the reference corpus (ref).
- New `encoding()`-method for character vector.
- New `name()`-method for character vector.
- A new `count()`-method for `context`-objects will return the `data.table` in the `stat`-slot with the counts for the tokens in the window.
- The `decode()`-function replaces a `decode()`-method and can be applied to partitions. The return value is a `data.table` which can be coerced to a `tibble`, serving as an interface to tidytext (#37).
- The `ngrams()`-method will work for corpora, and a new `show()`-method for `textstat`-object generates a proper output (#27).


## BUG FIXES

- Any usage of `tempdir()` is wrapped into normalizePath(..., winslash = "/"), to avoid mixture of file separators in a path, which may cause problems on Windows systems.
- In the calculation of cooccurrences, the node has previously been included in the window size. This has been corrected.
- The `kwic()`-method for corpora returned one surplus token to the left and to the right of the query. The excess tokens are not removed.
- The object returned by the `kwic()`-method for `character`-objects method did not include the correct position of matches in the `cpos` slot. Corrected.
- Bug removed that occurrs when context window reaches beyond beginning or end of a corpus (#48).
- When generating a `partition_bundle` using the `as.speeches()`-method, an error could occur when an empty partition has been generated accidentaly. Has been removed. (#50)
- The `as.VCorpus()`-method is not available if the `tm`-package has been loaded previously. A coerce method (`as(OBJECT, "VCorpus")) solves the issue. The `as.VCorpus()`-method is still around, but serves as a wrapper for the formal coerce-method (#55).
- The argument `verbose` as used by the `use()`-method did not have any effect. Now, messages are not reported as would be expected, if `verbose` is `FALSE`. On this occasion, we took care that corpora that are activated are now reported in capital letters, which is consistent with the uppercase logic you need to follow when using corpora. (#47)
- A new check prevents an error that has occurred when a token queried by the `context()`-method would occurr at the very beginning or very end of a corpus and the window would transgress the beginning / end of the corpus without being checked (#44).
- The `as.speeches()`-function caused an error when the type of the partition was not defined. Solved (#57).
- To deal with issues resulting from an unset locale, there is a check during startup whether the locale is unset (i.e. 'C') (#39).
- There was a difficulty to generate a `TermDocumentMatrix` from a `partition_bundle` if the partitions in the `partition_bundle` were not named. The fix is to assign integer numbers as names to the partitions (#58).


## DOCUMENTATION FIXES

- Substantial rework of the documentation of the `ll()`, and `chisquare()`-methods to make the statistical procedure used transparent.
- Expanded documentation for `cooccurrences()`-method to explain subsetting results vs applying positivelist/negativelist (#28).
- Wrote some documentation for the `round()`-method for `textstat`-objects that will show up in documentation of `textstat` class.
- Improved documentation of the `mail()`-method (#31).
- In the examples for the `decode()`-function, using the REUTERS corpus replaces the usage
    of the GERMAPARLMINI corpus, to reduce time consumed when checking the package.
  


polmineR 0.7.10
===============

### NEW FEATURES

- The package now offers a simplified and seamless workflow for dictionary-based sentiment analysis: The `weigh()`-method has been implemented for the classes `count` and `count_bundle`. Via inheritance, it will also be available for the `partition`- and `partition_bundle`-classes. Then, a new `summary()`-method for `partition`-class objects is introduced. If the object has been weighed, the list that is returned will include a report on weights. There is an example that explains the workflow.
- The `partition_bundle`-method for `context`-objects has been reworked entirely (and is working again);
a new `partition`-method for `context`-objects has been introduced. Buth steps are intended for workflows for dictionary-based sentiment analysis.
- The `highlight()`-method is now implemented for class `kwic`. You can highlight words in the neighborhood of a node that are part of a dictionaty.
- A new `knit_print()`-method for `textstat`- and `kwic`-objects offers a seamless inclusion of analyses in Rmarkdown documents.
- A `coerce()`-method to turn a `kwic`-object into a htmlwidget has been singled out from the `show()`-method for `kwic`-objects. Now it is possible to generate a htmlwidget from a kwic object, and to include the widget into a Rmarkdown document.
- A new `coerce()`-method to turn `textstat`-objects into an htmlwidget (DataTable), very useful for  Rmarkdown documents such as slides.
- A new argument height for the `html()`-method will allow to define a scroll box. Useful to embed a fulltext output to a Rmarkdown document.


### MINOR IMPROVEMENTS

- The `partition_bundle`-class, rather than inheriting from `bundle`-class directly, will now inherit from the `count_bundle`-class
- The `use()`-function is limited now to activating the corpus in data packages. Having introduced the session registry, switching registry directories is not needed any more.
- The `as.regions()`-function has been turned into a `as.regions()`-method to have a more generic tool.
- Some refactoring of the `context`-method, so that full use of `data.table` speeds up things.
- The `highlight()`-method allows definitions of terms to be highlighted to be passed in via three dots (...);
no explicit list necessary.
- A new `as.character()`-method for kwic-class objects is introduced.


### BUG FIXES

- The `size_coi`-slot (coi for corpus of interest) of the `context`-object included the node; the node (i.e. matches for queries) is excluded now from the count of size_coi.
- When calling `use()`, the registry directory is reset for CQP, so that the corpora in the package that have been  activated can be used with CQP syntax.
- The script configure.win has been removed so that installation works on Windows without an installation of Rtools.
- Bug removed from `s_attributes()`-method for `partition`-objects: "fast track" was activated without preconditions.
- Bug removed that would swallow metadata/s-attributes to be displayed in `kwic`-output after highlighting.
- As a matter of consistency, the argument `meta` has been renamed to `s_attributes` for the `kwic()`-method for `context`-objects, and for the `enrich()`-method for `kwic`-objects.
- To avoid confusion (with argument s_attributes), the argument `s_attribute` to check for integrity within 
a struc has been renamed into `boundary`.


### DOCUMENTATION FIXES

- Documentation for `kwic`-objects has been reworked thoroughly.


polmineR 0.7.9
==============

- new as.list,bundle-method for convenience, to access slot objects
- as.bundle is more generic now, so that any kind of object can be coerced to a bundle now
- as.speeches-method turned into function that allows partition and corpus as input
- is.partition-function introduced
- sAttributes,partition-method in line with RcppCWB requirements (no negative values of  strucs)
- count repaired for muliple p-attributes
- bug removed causing a crash for as.markdown-method when cutoff is larger than number of tokens
- polmineR will now work with a temporary registry in the temporary session directory
- a (new) registry_move() function is used to copy files to the tmp registry
- the (new) registry() function will get the temporary registry directory
- the use() function will add the registry file of a package to the tmp registry
- a bug removed that has prevented the name<- method to work properly for bundle objects
- new partition_bundle,partition_bundle-method introduced
- naming of methods and functions, classes and most arguments moved to snake_case, maintaining backwards compatibility
- utility function getObjects not exported any more
- for count,partition_bundle-method, column 'partition' will be a character vector now (not factor)
- new argument 'type' added to partition_bundle
- new method 'get_type' introduced to make getting corpus type more robust
- bug removed that has caused a crash when cutoff is larger than number of tokens in a partition when calling get_token_stream
- count-method will now return count-object if query is NULL, making it easier to write pipes


polmineR 0.7.8
==============

- upon loading the package, check that data directories are set correctly in registry files to make sure that sample data in pre-compiled packages can be used
- startup messages adjusted slightly


polmineR 0.7.7 
==============

- removed depracated classes: dispersion, Textstat (reference class), Partition (reference class)
- divide-methode moved to package polmineR.misc
- bug removed: size of ngrams object was always 1
- dotplot-method added for featuresNgrams
- sample corpus GermaParlMini added to the package (replacing suggested package polmineR.sampleCorpus)
- configuration mechanism added to set path to data directory in registry file upon installation 
- class hits now inherits from class 'textstat', exposing a set of generic functions (such as dim, nrow etc.); slot 'dt' changed to 'stat' for this purpose
- count,partitionBundle and hits,partitionBundle: cqp parameter added
- RegistryFile class replaced by a set of leightweight-functions (corpus_...)
- encode-method moved to cwbtools package
- getTerms,character-method and terms,partition-method merged
- examples using EUROPARL corpus have been replaced by REUTERS corpus (including vignette)
- param id2str has been renamed to decode in all functions to avoid unwanted behavior
- robust indexing of bundle objects for subsetting
- optional settings have been cleaned
- reliance on cwb command line tools removed
- encoding issue with names of partitionBundle solved


polmineR 0.7.6
==============

- functionality of matches-method (breakdown of frequencies of matches) integrated
into count-method (new param breakdown)
- corpus REUTERS included (as data for testsuite)
- adjust data directory of REUTERS corpus upon loading package
- a pkgdown-generated website is included in the docs directory
- consistent use of .message helper function to make shiny app work
- bug removed for count-method when options("polmineR.cwb-lexdecode") is TRUE and options("polmineR.Rcpp") is FALSE
- if CORPUS_REGISTRY is not defined, the registry directory in the package will
be used, making REUTERS corpus available
- getSettings-function removed, was not sufficiently useful, and was superseded by
template mechanism
- new class 'count' introduced to organize results from count operations
- at startup, default template is assigned for corpora without explicitly defined templates to make read() work in a basic fashion
- new cpos,hits-method to support highlight method
- tooltips-method to reorder functionality of html/highlight/tooltip-methods
- param charoffset added to html-method 
- coerce-method from partition to json and vice versa, potentially useful for storing partitions
- sAttributes2cpos to work properly with nested xml
- partition,partition-method reworked to work properly with nested XML
- encoding of return value of sAttributes will be locale
- references added to methods count, kwic, cooccurrences, features.
- as.DocumentTermMatrix,character-method reworked to allow for subsetting and divergence of
strucs and struc_str
- html,partition-method has new option beautify, to remove whitespace before interpunctuation
- output error removed in html,partition-method (that misinterprets `` as code block)
- the class Corpus now has a slot sAttribute to keep/manage a data.table with corpus positions and struc values, and there is a new partition,Corpus-method. In compbination, it will be a lot faster to derive a partition, particularly if you need to do that repeatedly
- a new function install.cwb() provides a convenient way to install CWB in the package 
- added a missing encoding conversion for the count method


polmineR 0.7.5
==============

- class 'Regions' renamed to class 'regions' as a matter of consistency
- data type of slot cpos of class 'regions' is a matrix now
- rework and improved documentation for decode- and encode-methods
- new functions copy.corpus and rename.corpus
- as.DocumentTermMatrix-method checks for strucs with value -1 
- improved as.speeches-method: reordering of speeches, default values
- blapply-method: verbose output will be suppressed of progress is TRUE


polmineR 0.7.4
==============

- applying stoplists and positivelists working again for context-method
- matches-method to learn about matches for CQP queries replacing frequencies-method
- Rework of enrich-method, including documentation.
- param 'neighbor' dropped from kwic,context-method; params positivelist and negativelist offer equivalent functionality
- highlight-method for (newly exported) kwic-method (for validation purposes)
- performance improvement for partitionBundle,character-method
- a new Labels class and label method for generating test data
- bug removed for partitionBundle,character-class, and performance improved
- Improved explanation of the installation procedure for Mac in the package vignette
- for context-method: param sAttribute working again to check boundaries of match regions
- sample-method for objects of class kwic and context
- kwic, cpos, and context method will accept queries of length > 1
- use-function and resetRegistry-function reworked
- more explicit startup message to get info about version, registry and interface
- encoding issues solved for size-method, hits-method and dispersion-method
- use-function will now work for users working with polmineR.Rcpp as interface


polmineR 0.7.3
==============

- new installed.corpora() convenience function to list all data packages with corpora
- view-method and show-method for cooccurrences-objects now successfully redirect
output to RStudio viewer
- data.table-style indexing of objects inheriting from textstat-class
- for windows compatibility, as.corpusEnc/as.nativeEnc for encoding conversion
- performance gain for size-method by using polmineR.Rcpp
- dissect-method dropped (replaced by size)
- improved documentation of size-method
- labels for cooccurrences-output
- cooccurrencesBundle-class and cooccurrence-method for bundle restored
- as.data.table for cooccurrencesBundle-class
- count-method for whole corpus for pAttribute > 1
- functionality of meta-method merged into sAttributes-method (meta-method dropped)
- speed improvements for generating html output for reading
- previously unexported highlight-method now exported, and more robust than before (using xml2)
- progress bars for multicore operations now generated by pbapply package
- starting to use testthat for unit testing


polmineR 0.7.2
==============

- updated documentation of partition-method.
- documentation of hits-method improved
- use-methode: default value for pkg ist NULL (return to default registry), function more robust
- Rework for parsing the registry
- rework of templates, are part of options now (see ?setTemplate, ?getTemplate)
- experimental use of polmineR.Rcpp-package for fast counts for whole corpus
- new convenience function install.corpus to install CWB corpus wrapped into R data package
- adjustments to make package compatible with polmineR.shiny
- cpos-method to get hits more robust if there are not matches for string
- hits-method removes NAs
- compare-method renamed to features-method
- warnings caused by startup on windows removed


polmineR 0.7.1
==============

- size-method now allows for a param 'sAttribute'
- hits-method reworked, allows for names query vectors
- first version that can be installed on windows

polmineR 0.7.0
==============

- rcqp package moved to suggests, to facilitate installation


polmineR 0.6.3
==============

- more generic implementation of as.markdown-method to prepare use of templates
- LICENSE file updated
- getTokenStream,character-method: new default behavior for params left and right
- use of templates for as.markdown-method
- Regions and TokenStream class (not for frontend use, so far)
- getTermFrequencies-method merged into count-method
- Corpus class introduced
- decode- and encode-methods introduced


polmineR 0.6.2
==============

- refactoring of context-method to prepare more consistent usage
- progress bar for context-method (using blapply)
- progress bar for partitionBundle (using blapply)
- more coherent naming of parameters in partitionBundle-method
- partitionBundle,character-method debugged and more robust
- usage of blapply in as.speeches-method
- hits-method: paramter cqp defaults to FALSE for hits-method, size defaults to FALSE
- new parameter cqp for dispersion-method
- aggregation for dispersion-method when length(sAttribute) == 1
- bugfix for ngrams-method, sample code for the method


polmineR 0.6.1
==============

- configure file removed to avoid unwanted bugs


polmineR 0.6.0
==============

- this is the first version that passes all CRAN tests and that is available via CRAN
- the 'rcqp' remains the interface to the CWB, but usage of rcqp functions is wrapped into an new new CQI.rcqp (R6) class. CQI.perl and CQI.cqpserver are introduced as alternative interfaces to prepare portability to Windows systems
- code in the vignette and method examples will be executed conditionally, if rcqp and the polmineR.sampleCorpus are available
- the polmineR.sampleCorpus package is available in a drat repo at www.github.com/PolMine
- a series of bug fixes


polmineR 0.5.6
==============

- slot tf renamed to stat, class is data.table now
- keyness_method moved to data.tables


polmineR 0.5.3
==============

- renamed collocations to cooccurrences, seems more appropriate
  
polmineR 0.5.2
==============

- multicore for term frequency counts (param for partition)

polmineR 0.5.0
==============

- renamed xxxCluster to xxxBundle, bundle-superclass introduced
- slot label/labels renamed to name/names
- name/names-method instead of label/labels


