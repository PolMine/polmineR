#' @include polmineR.R
NULL

#' Corpus class initialization
#' 
#' Corpora indexed using the Corpus Workbench (CWB) offer an efficient data
#' structure for large, linguistically annotated corpora. The `corpus`-class
#' keeps basic information on a CWB corpus. Corresponding to the name of the
#' class, the `corpus`-method is the initializer for objects of the `corpus`
#' class. A CWB corpus can also be hosted remotely on an
#' \href{https://www.opencpu.org}{OpenCPU} server. The `remote_corpus` class
#' (which inherits from the `corpus` class) will handle respective information.
#' A (limited) set of polmineR functions and methods can be executed on the
#' corpus on the remote machine from the local R session by calling them on the
#' `remote_corpus` object. Calling the `corpus`-method without an argument will
#' return a `data.frame` with basic information on the corpora that are
#' available.
#'
#' @details Calling `corpus()` will return a `data.frame` listing the corpora
#'   available locally and described in the active registry directory, and some
#'   basic information on the corpora.
#' @details A `corpus` object is instantiated by passing a corpus ID as argument
#'   `.Object`. Following the conventions of the Corpus Workbench (CWB), Corpus
#'   IDs are written in upper case. If `.Object` includes lower case letters,
#'   the `corpus` object is instantiated nevertheless, but a warning is issued
#'   to prevent bad practice. If `.Object` is not a known corpus, the error
#'   message will include a suggestion if there is a potential candidate that
#'   can be identified by `agrep`.
#' @details A limited set of methods of the `polmineR` package is exposed to be
#'   executed on a remote OpenCPU server. As a matter of convenience, the
#'   whereabouts of an OpenCPU server hosting a CWB corpus can be stated in an
#'   environment variable "OPENCPU_SERVER". Environment variables for R sessions
#'   can be set easily in the `.Renviron` file. A convenient way to do this is
#'   to call `usethis::edit_r_environ()`.
#'     
#' @slot corpus A length-one `character` vector, the upper-case ID of a CWB
#'   corpus.
#' @slot registry_dir Registry directory with registry file describing the
#'   corpus.
#' @slot data_dir The directory where binary files of the indexed corpus reside.
#' @slot info_file If available, the info file indicated in the registry file
#'   (typically a file named `.info` `info.md` in the data directory), or `NA`
#'   if not.
#' @slot template Full path to the template containing formatting instructions
#'   when showing full text output (`fs_path` object or `NA`). 
#' @slot type If available, the type of the corpus (e.g. "plpr" for a corpus of
#'   plenary protocols), or `NA`.
#' @slot name Full name of the corpus that may be more expressive than
#'   the corpus ID.
#' @slot encoding The encoding of the corpus, given as a length-one
#'   `character` vector (usually 'utf8' or 'latin1').
#' @slot size Number of tokens (size) of the corpus, a length-one `integer`
#'   vector.
#' @slot server The URL (can be IP address) of the OpenCPU server. The slot is
#'   available only with the `remote_corpus` class inheriting from the `corpus`
#'   class.
#' @slot user If the corpus on the server requires authentication, the username.
#' @slot password If the corpus on the server requires authentication, the
#'   password.
#' @exportClass corpus
#' @aliases zoom corpus get_corpus remote_corpus remote_corpus-class
#' @name corpus-class
#' @seealso Methods to extract basic information from a `corpus` object are
#'   covered by the `corpus-methods` documentation object. Use the
#'   \code{\link{s_attributes}} method to get information on structural
#'   attributes. Analytical methods available for `corpus` objects are
#'   \code{\link{size}}, \code{\link{count}}, \code{\link{dispersion}},
#'   \code{\link{kwic}}, \code{\link{cooccurrences}},
#'   \code{\link{as.TermDocumentMatrix}}.
#' @family classes to manage corpora
#' @examples
#' use("polmineR")
#' 
#' # get corpora present locally
#' y <- corpus()
#' 
#' # initialize corpus object
#' r <- corpus("REUTERS")
#' r <- corpus ("reuters") # will work, but will result in a warning
#' 
#' 
#' # apply core polmineR methods
#' a <- size(r)
#' b <- s_attributes(r)
#' c <- count(r, query = "oil")
#' d <- dispersion(r, query = "oil", s_attribute = "id")
#' e <- kwic(r, query = "oil")
#' f <- cooccurrences(r, query = "oil")
#' 
#' # used corpus initialization in a pipe
#' y <- corpus("REUTERS") %>% s_attributes()
#' y <- corpus("REUTERS") %>% count(query = "oil")
#' 
#' # working with a remote corpus
#' \dontrun{
#' REUTERS <- corpus("REUTERS", server = Sys.getenv("OPENCPU_SERVER"))
#' count(REUTERS, query = "oil")
#' size(REUTERS)
#' kwic(REUTERS, query = "oil")
#' 
#' GERMAPARL <- corpus("GERMAPARL", server = Sys.getenv("OPENCPU_SERVER"))
#' s_attributes(GERMAPARL)
#' size(x = GERMAPARL)
#' count(GERMAPARL, query = "Integration")
#' kwic(GERMAPARL, query = "Islam")
#' 
#' p <- partition(GERMAPARL, year = 2000)
#' s_attributes(p, s_attribute = "year")
#' size(p)
#' kwic(p, query = "Islam", meta = "date")
#' 
#' GERMAPARL <- corpus("GERMAPARLMINI", server = Sys.getenv("OPENCPU_SERVER"))
#' s_attrs <- s_attributes(GERMAPARL, s_attribute = "date")
#' sc <- subset(GERMAPARL, date == "2009-11-10")
#' }
setClass(
  "corpus",
  slots = c(
    corpus = "character",
    registry_dir = "fs_path",
    data_dir = "fs_path",
    template = "fs_path",
    info_file = "fs_path",
    type = "character",
    encoding = "character",
    name = "character",
    size = "integer"
  )
)



#' Bundle Class
#' 
#' A `bundle` is used to combine several objects (`partition`, `context`,
#' `features`, `cooccurrences` objects) into one S4 class object. Typically, a
#' class inheriting from the `bundle` superclass will be used. When working with
#' a `context_bundle`, a `features_bundle`, a `cooccurrences_bundle`, or a
#' `context_bundle`, a similar set of standard methods is available to perform
#' transformations.
#' 
#' @slot corpus The CWB corpus the xobjects in the `bundle` are based on, a
#'   length 1 `character` vector.
#' @slot objects An object of class `list`.
#' @slot p_attribute Object of class `character`.
#' @slot encoding The encoding of the corpus.
#' 
#' @param x a bundle object
#' @param i An `integer` value to index a bundle object.
#' @param object A `bundle` object.
#' @param size number of items to choose to generate a sample
#' @param ... Further parameters
#' @param col columns of the `data.table` to use to generate an object.
#' @param value character string with a name to be assigned
#' @rdname bundle
#' @name bundle-class
#' @aliases bundle [[,bundle-method [[<-,bundle-method
#' @exportClass bundle
#' @docType class
#' @author Andreas Blaette
#' @examples
#' parties <- s_attributes("GERMAPARLMINI", "party")
#' parties <- parties[-which(parties == "NA")]
#' party_bundle <- partition_bundle("GERMAPARLMINI", s_attribute = "party")
#' length(party_bundle)
#' names(party_bundle)
#' get_corpus(party_bundle)
#' party_bundle <- enrich(party_bundle, p_attribute = "word")
#' summary(party_bundle)
#' parties_big <- party_bundle[[c("CDU_CSU", "SPD")]]
#' summary(parties_big)
setClass(
  "bundle",
  slots = c(
    objects = "list",
    p_attribute = "character"
  ),
  contains = "corpus"
)


#' @exportMethod name<-
#' @rdname bundle
setReplaceMethod("name", signature = "bundle", function(x, value) {
  names(x@objects) <- value
  for (i in 1L:length(x)) x@objects[[i]]@name <- value[[i]]
  x
})





#' S4 textstat superclass.
#' 
#' The `textstat` S4 class is the superclass for the classes `features`,
#' `context`, and `partition`. Usually, these subclasses, which are designed to
#' serve a specified analytical purpose, will be used . Common standard generic
#' methods such as `head`, `tail`, `dim`, `nrow`, `colnames` are defined for the
#' `textstat` class and are available for subclasses by inheritence. The core of
#' `textstat` and its childs is a `data.table` in the slot `stat` for keeping
#' data on text statistics of a `corpus`, or a `partition`. The `textstat` class
#' inherits from the `corpus` class, keeping information on the corpus
#' available.
#' 
#' A `head`-method will return the first rows of the `data.table` in
#' the `stat`-slot. Use argument `n` to specify the number of rows.
#'
#' A `tail`-method will return the last rows of the `data.table` in
#' the `stat`-slot. Use argument `n` to specify the number of rows.
#'
#' The methods `dim`, `nrow` and `ncol` will return information
#' on the dimensions, the number of rows, or the number of columns of the
#' `data.table` in the `stat`-slot, respectively.
#' 
#' Objects derived from the `textstat` class can be indexed with simple
#' square brackets ("[") to get rows specified by an numeric/integer vector,
#' and with double square brackets ("[[") to get specific columns from the 
#' `data.table` in the slot `stat`.
#' 
#' The `colnames`-method will return the column names of the `data-table`
#' in the slot `stat`.
#' 
#' The methods `as.data.table`, and `as.data.frame` will extract the
#' `data.table` in the slot `stat` as a `data.table`, or `data.frame`,
#' respectively.
#' 
#' @slot p_attribute Object of class `character`, p-attribute of the query.
#' @slot corpus A corpus specified by a length-one `character` vector. 
#' @slot stat A `data.table` with statistical information.
#' @slot name The name of the object.
#' @slot annotation_cols A `character` vector, column names of
#'   `data.table` in slot `stat` that are annotations.
#' @slot encoding A length-one `character` vector, the encoding of the corpus.
#' @param .Object A `textstat` object.
#' @param x A `textstat` object.
#' @param by Column that will serve as the key for sorting.
#' @param decreasing Logical, whether to return decreasing order.
#' @param e1 A `texstat` object.
#' @param e2 Another `texstat` object.
#' @param ... Further arguments.
#' @aliases as.data.frame,textstat-method show,textstat-method
#'   dim,textstat-method
#'   colnames,textstat-method rownames,textstat-method names,textstat-method
#'   as.DataTables,textstat-method head,textstat-method tail,textstat-method
#'   dim,textstat-method nrow,textstat-method ncol,textstat-method
#'   colnames,textstat-method as.data.frame,textstat-method
#'   round,textstat-method sort,textstat-method [,textstat,ANY,ANY,ANY-method [[,textstat-method
#'   name name<-
#' @docType class
#' @exportClass textstat
#' @examples
#' use("polmineR")
#' P <- partition("GERMAPARLMINI", date = ".*", p_attribute = "word", regex = TRUE)
#' y <- cooccurrences(P, query = "Arbeit")
#' 
#' # generics defined in the polmineR package
#' x <- count("REUTERS", p_attribute = "word")
#' name(x) <- "count_reuters"
#' name(x)
#' get_corpus(x)
#' 
#' # Standard generic methods known from data.frames work for objects inheriting
#' # from the textstat class
#' 
#' head(y)
#' tail(y)
#' nrow(y)
#' ncol(y)
#' dim(y)
#' colnames(y)
#' 
#' # Use brackets for indexing 
#' 
#' \dontrun{
#' y[1:25]
#' y[,c("word", "ll")]
#' y[1:25, "word"]
#' y[1:25][["word"]]
#' y[which(y[["word"]] %in% c("Arbeit", "Sozial"))]
#' y[ y[["word"]] %in% c("Arbeit", "Sozial") ]
#' }
setClass(
  "textstat",
  slots = c(
    p_attribute = "character",
    stat = "data.table",
    annotation_cols = "character"
  ),
  contains = "corpus",
  prototype = list(
    stat = data.table()
  )
)

# setValidity("textstat", function(object){
#   if (
#     identical(attr(object@stat, ".internal.selfref"), new("externalptr"))
#   ){
#     return(paste(
#       "data.table pointer is 0x0 - copy object using cp() to create",
#       "object with valid pointer"
#     ))
#   } else {
#     TRUE
#   }
# })

#' @details `textstat` objects can have a name, which can be retrieved, and set using
#' the `name`-method and `name<-`, respectively.
#' @rdname textstat-class
setMethod("name", "textstat", function(x) x@name)

#' @rdname textstat-class
setMethod("name", "character", function(x) x)

#' @param value A `character` vector to assign as name to slot `name`
#'   of a `textstat` class object.
#' @rdname textstat-class
#' @exportMethod name<-
setReplaceMethod("name", signature = "textstat", function(x, value) {
  x@name <- value
  x
})



#' Feature selection by comparison.
#' 
#' The  `features`-method returns a `features`-object. Several
#' `features`-objects can be combined into a `features_bundle`-object.
#' 
#' @slot corpus The CWB corpus the features are derived from, a `character` vector of length 1.
#' @slot p_attribute Object of class `character`.
#' @slot encoding Object of class `character`.
#' @slot corpus Object of class `character`.
#' @slot stat Object of class `data.frame`.
#' @slot size_coi Object of class `integer`. 
#' @slot size_ref Object of class `integer`.
#' @slot included Object of class `logical` whether corpus of interest is included in reference corpus
#' @slot method Object of class `character` statisticalTest used
#' @slot call Object of class `character` the call that generated the object
#' 
#' @param object A `features` or `features_bundle` object.
#' @param .Object a `features` object.
#' 
#' @rdname features-class
#' @name features-class
#' @docType class
#' @exportClass features
#' @author Andreas Blaette
setClass("features",
         slots = c(
           size_coi = "integer",
           size_ref = "integer",
           method = "character",
           included = "logical",
           call = "character"
         ),
         contains = "textstat" # inherited slots: corpus, p_attribute, encoding, stat
)


#' Count class.
#' 
#' S4 class to organize counts. The classes `polmineR` and
#' `ngrams` inherit from the class.
#' 
#' @slot stat Object of class `data.table`.
#' @slot corpus Object of class `character` the CWB corpus the partition is based on .
#' @slot encoding Object of class `character`, the encoding of the corpus.
#' @slot name Object of class `character`, a name for the object.
#' @slot size Object of class `integer`, the size of the partition or
#'   corpus the count is based upon.
#' @rdname count_class
#' @param ... Further parameters.
#' @name count_class
#' @exportClass count
#' @docType class
#' @author Andreas Blaette
#' @aliases count-class
#' @seealso The `count`-class inherits from the \code{\link{textstat-class}}.
setClass("count",
         representation = list(
           size = "integer"
           ),
         contains = "textstat"
         )


#' @param object A `count` object.
#' @details The `summary`-method in combination with a weighed
#'   `count`-object can be used to perform a dictionary-based sentiment
#'   analysis (see examples).
#' @rdname count_class
#' @examples
#' # sample for dictionary-based sentiment analysis
#' weights <- data.table::data.table(
#'   word = c("gut", "super", "herrlich", "schlecht", "grob", "mies"),
#'   weight = c(1,1,1,-1,-1,-1)
#' )
#' corp <- corpus("GERMAPARLMINI")
#' sc <- subset(corp, date == "2009-11-11")
#' cnt <- count(sc, p_attribute = "word")
#' cnt <- weigh(cnt, with = weights)
#' y <- summary(cnt)
#' 
#' # old, partition-based workflow
#' p <- partition("GERMAPARLMINI", date = "2009-11-11")
#' p <- enrich(p, p_attribute = "word")
#' weights <- data.table::data.table(
#'   word = c("gut", "super", "herrlich", "schlecht", "grob", "mies"),
#'   weight = c(1,1,1,-1,-1,-1)
#' )
#' p <- weigh(p, with = weights)
#' summary(p)
setMethod("summary", "count", function(object){
  y <- list(
    name = if (length(name(object)) > 0) name(object) else NA,
    size = object@size
  )
  if (nrow(object@stat) > 0){
    y[["p_attribute"]] <- paste(object@p_attribute, collapse = "|")
    y[["unique"]] <- nrow(object@stat)
    if ("weight" %in% colnames(object@stat)){
      dt_positive <- subset(object@stat, object@stat[["weight"]] > 0)
      if (nrow(dt_positive) > 0){
        y[["positive_n"]] <- sum(dt_positive[["count"]])
        y[["positive_share"]] <- y[["positive_n"]] / y[["size"]]
        y[["positive_weighed"]] <- sum(dt_positive[["count"]] * dt_positive[["weight"]])
      } else {
        y <- c(y, list(positive_n = 0, positive_share = 0, positive_weighed = 0))
      }
      
      dt_negative <- subset(object@stat, object@stat[["weight"]] < 0)
      if (nrow(dt_negative) > 0){
        y[["negative_n"]] <- sum(dt_negative[["count"]])
        y[["negative_share"]] <- y[["negative_n"]] / y[["size"]]
        y[["negative_weighed"]] <- sum(dt_negative[["count"]] * dt_negative[["weight"]])
      } else {
        y <- c(y, list(negative_n = 0, negative_share = 0, negative_weighed = 0))
      }
    }
  }
  y
})



#' @docType class
#' @exportClass count_bundle
#' @rdname count_class
setClass("count_bundle", contains = "bundle")



#' @details The `length`-method is synonymous with the `size`-method
#' and will return the size of the `corpus` or `partition` a count
#' has been derived from.
#' @param x A `count` object, or a class inheriting from `count`.
#' @exportMethod length
#' @rdname count_class
setMethod("length", "count", function(x) x@size)




#' Partition class and methods.
#' 
#' The `partition` class is used to manage subcorpora. It is an S4 class, and
#' a set of methods is defined for the class. The class inherits
#' from the classes `count` and `textstat`.
#' 
#' @details As `partition` objects inherit from `count` and
#'   `textstat` class, methods available are `view` to inspect the
#'   table in the `stat` slot, `name` and `name<-` to
#'   retrieve/set the name of an object, and more.
#' 
#' @slot name A name to identify the object (`character` vector with length 1); useful when multiple
#' `partition` objects are combined to a `partition_bundle`.
#' @slot corpus The CWB indexed corpus the partition is derived from (`character` vector with length 1).
#' @slot encoding Encoding of the corpus (`character` vector with length 1).
#' @slot s_attributes A named `list` with the s-attributes specifying the partition.
#' @slot explanation Object of class `character`, an explanation of the partition.
#' @slot cpos A `matrix` with left and right corpus positions defining regions (two columns).
#' @slot annotations Object of class `list`.
#' @slot size Total size of the partition (`integer` vector, length 1).
#' @slot stat An (optional) `data.table` with counts. If present, speeds up computation of cooccurrences,
#' as count is already present.
#' @slot metadata Object of class `data.frame`, metadata information.
#' @slot strucs Object of class `integer`, the strucs defining the partition.
#' @slot p_attribute Object of class `character` indicating the p_attribute of the
#' count in slot `stat`.
#' @slot xml Object of class `character`, whether the xml is flat or nested.
#' @slot s_attribute_strucs Object of class `character` the base node 
#' @slot key Experimental, an s-attribute that is used as a key.
#' @slot call Object of class `character` the call that generated the partition 
#' @param .Object A `partition` object.
#' @param p_attribute a p-attribute (for enriching) / performing count.
#' @param verbose `logical` value, whether to output messages
#' @param ... further parameters passed into `count` when calling `enrich`, and ...
#' @aliases partition-class show,partition-method [,partition,ANY,ANY,ANY-method 
#'   [,partition-method as.partition_bundle 
#'   as.partition_bundle,partition-method export export,partition-method split
#' @rdname partition_class
#' @name partition_class
#' @exportClass partition
#' @docType class
#' @author Andreas Blaette
#' @seealso The `partition`-class inherits from the
#'   \code{\link{textstat-class}}, see respective documentation to learn more.
setClass(
  "partition",
  slots = c(
    s_attributes = "list",
    explanation = "character",
    cpos = "matrix",
    annotations = "list",
    size = "integer",
    metadata = "data.frame",
    strucs = "integer",
    xml = "character",
    s_attribute_strucs = "character",
    call = "character",
    key = "character"
  ),
  prototype = list(
    size = NA_integer_,
    stat = data.table()
  ),
  contains = "count"
)

#' @exportClass remote_partition
#' @rdname partition_class
setClass(
  "remote_partition",
  slots = c(
    server = "character",
    restricted = "logical"
  ),
  contains = "partition"
)


setAs(from = "partition", to = "remote_partition", def = function(from){
  y <- new("remote_partition")
  for (x in slotNames(from)) slot(y, x) <- slot(from, x)
  y
})

setAs(from = "remote_partition", to = "partition", def = function(from){
  y <- new("partition")
  for (x in slotNames(y)) slot(y, x) <- slot(from, x)
  y
})



#' Context class.
#' 
#' Class to organize information of context analysis.
#' 
#' @details Objects of the class `context` include a `data.table` in the
#' slot `cpos`. The `data.table` will at least include the columns "match_id",
#' "cpos" and "position".
#' 
#' @slot query The query examined (`character`).
#' @slot count An `integer` value, the number of hits for the query.
#' @slot partition The `partition` the `context` object is based on.
#' @slot size_partition The size of the partition, a length-one `integer` vector.
#' @slot left A length-one `integer` value, the number of tokens to the left of the query match.
#' @slot right An `integer` value, the number of tokens to the right of the query match.
#' @slot size A length-one `integer` value, the number of tokens covered by
#'   the `context`-object, i.e. the number of tokens in the right and left context
#'   of the node as well as query matches.
#' @slot size_match A length-one `integer` value, the number of tokens
#'   matches by the query. Identical with the value in slot `count` if the query
#'   is \emph{not} a CQP query.
#' @slot size_coi A length-one `integer` value, the number of tokens in the
#'   right and left context of the node (excluding query matches).
#' @slot size_ref A length-one `integer` value, the number of tokens in the
#'   partition, without tokens matched and the tokens in the left and right
#'   context.
#' @slot boundary An s-attribute (`character`).
#' @slot p_attribute The p-attribute of the query (`character`).
#' @slot corpus The CWB corpus used (`character`).
#' @slot stat A `data.table`, the statistics of the analysis.
#' @slot encoding Object of class `character`, encoding of the corpus.
#' @slot cpos A `data.table`, with the columns match_id, cpos, position, word_id.
#' @slot method A `character`-vector, statistical test used.
#' @slot call Object of class `character`, call that generated the object.
#'     
#' @param .Object object
#' @param x a context object
#' @param size integer indicating sample size
#' @param object a context object
#' @param positivelist tokens that are required to be present to keep a match
#' @param stoplist tokens that are used to exclude a match
#' @param regex logical, whether positivlist / stoplist is interpreted as regular expressions
#' @param progress logical, whether to show progress bar
#' @param ... to maintain backwards compatibility if argument `pAttribute` is still used
#' @aliases show,context-method [,context-method [,context,ANY,ANY,ANY-method
#'   [[,context-method summary,context-method head,context-method
#'   as.DataTables,context-method
#' @docType class
#' @rdname context-class
#' @exportClass context
setClass(
  "context",
  slots = c(
    query = "character",
    count = "integer",
    partition = "partition",
    size_partition = "integer",
    size_match = "integer",
    left = "integer",
    right = "integer",
    size = "integer",
    boundary = "character",
    cpos = "data.table",
    call = "character"
  ),
  contains = "features"
)

#' @details The `length`-method will return the number of hits that were achieved.
#' @rdname context-class
#' @exportMethod length
setMethod("length", "context", function(x) as.integer(x@count))

setAs(from = "textstat", to = "data.table", def = function(from) from@stat)
setAs(from = "partition", to = "data.table", def = function(from) from@stat)

#' Cooccurrences class.
#' 
#' S4 class to organize information of context analysis
#' 
#' @param .Object object to work with
#' @param object object to work with
#' @param x object to work with
#' @slot call Object of class `character` the call that generated the object
#' @slot partition Object of class `character` the partition the analysis is based on
#' @slot size_partition  Object of class `integer` the size of the partition
#' @slot left  Object of class `integer` number of tokens to the left.
#' @slot right  Object of class `integer` number of tokens to the right.
#' @slot p_attribute  Object of class `character` p-attribute of the query
#' @slot corpus  Object of class `character` the CWB corpus used
#' @slot stat  Object of class `data.table` statistics of the analysis
#' @slot encoding  Object of class `character` encoding of the corpus
#' @slot method  Object of class `character` statistical test(s) used
#' @aliases cooccurrences-class
#' @docType class
#' @exportClass cooccurrences
#' @rdname cooccurrences-class
setClass("cooccurrences", contains = "context")




#' S4 kwic class
#' 
#' S4 class for organizing information for kwic/concordance output. A set of
#' standard generics (`show`, `as.character`, `as.data.frame`,
#' `length`, `sample`, `subset`) as well as indexing is implemented to process
#' kwic class objects (see 'Usage'). See section 'Details' for the
#' `enrich`, `view` and `knit_print` methods.
#' 
#' @slot metadata A `character` vector with s-attributes of the metadata
#'   that are to be displayed.
#' @slot p_attribute The p-attribute for which the context has been generated.
#' @slot left An `integer` value, words to the left of the query match. 
#' @slot right An `integer` value, words to the right of the query match.
#' @slot corpus Length-one `character` vector, the CWB corpus.
#' @slot cpos A `data.table` with the columns "match_id", "cpos", "position",
#'   "word_id", "word" and "direction".
#' @slot stat A `data.table`, a table with columns "left", "node",
#'   "right", and metadata, if the object has been enriched.
#' @slot encoding A length-one `character` vector with the encoding of the
#'   corpus.
#' @slot name A length-one `character` vector naming the object.
#' @slot annotation_cols A `character` vector designating the columns of
#'   the `data.table` in the slot `table` that are annotations.
#' 
#' @param x A `kwic` class object.
#' @param object A `kwic` class object.
#' @param s_attributes Character vector of s-attributes with metainformation.
#' @param table Logical, whether to turn cpos `data.table` into
#'   `data.table` in slot `stat` for output.
#' @param p_attribute A length-one `character` vector supplying a p-attribute.
#' @param verbose A `logical` value, whether to output debugging messages.
#' @param extra An `integer` value, number of extra tokens to the left and
#'   to the right of the windows of tokens to the left and right of a query
#'   match that are decoded to be displayed in a kwic output to facilitate
#'   interpretation.
#' @param size An `integer`, subset size for sampling.
#' @param i Single integer value, the kwic line for which the fulltext shall be
#'   inspected.
#' @param ... Used for backwards compatibility.
#'   
#' @name kwic-class
#' @docType class
#' @aliases kwic-class [,kwic,ANY,ANY,ANY-method [,kwic-method
#' @exportClass kwic
#' @seealso The constructor for generating kwic objects is the
#'   \code{\link{kwic}} method.
#' @examples
#' use("polmineR")
#' K <- kwic("GERMAPARLMINI", "Integration")
#' get_corpus(K)
#' length(K)
#' K_min <- K[1]
#' K_min <- K[1:5]
#' 
#' # using kwic_bundle class
#' queries <- c("oil", "prices", "barrel")
#' li <- lapply(queries, function(q) kwic("REUTERS", query = q))
#' kb <- as.bundle(li)
#' 
#' @rdname kwic-class
setClass(
  "kwic",
  slots = c(
    cpos = "data.table",
    metadata = "character",
    left = "integer",
    right = "integer"
  ),
  contains = "textstat" # inherited: corpus, encoding, p_attribute, name, annotation_cols, stat
)


#' @rdname features-class
setClass("kwic_bundle", contains = "bundle")



setAs(from = "corpus", to = "partition", def = function(from){
  new(
    "partition",
    corpus = from@corpus,
    encoding = from@encoding,
    cpos = matrix(data = c(0L, (size(from) - 1L)), nrow = 1L),
    stat = data.table(),
    info_file = from@info_file,
    data_dir = from@data_dir,
    registry_dir = from@registry_dir,
    template = from@template,
    size = size(from),
    p_attribute = character()
  )
})


#' @exportClass remote_corpus
#' @docType class
#' @rdname corpus-class
setClass(
  "remote_corpus",
  slots = c(
    server = "character",
    restricted = "logical"
  ),
  contains = "corpus"
)

setAs(from = "corpus", to = "remote_corpus", def = function(from){
  y <- new("remote_corpus")
  for (x in slotNames(from)) slot(y, x) <- slot(from, x)
  y
})

setAs(from = "remote_corpus", to = "corpus", def = function(from){
  y <- new("corpus")
  for (x in slotNames(y)) slot(y, x) <- slot(from, x)
  y
})


#' Corpus class methods
#' 
#' A set of generic methods is available to extract basic information from
#' objects of the `corpus` class.
#' @param object An object of class `corpus`, or inheriting from it.
#' @aliases name,corpus-method
#' @name corpus-methods
#' @rdname corpus_methods
#' @details A `corpus` object can have a name, which can be retrieved using
#' the `name`-method.
#' @examples
#' # get/show information on corpora
#' corpus("REUTERS") %>% get_info()
#' corpus("REUTERS") %>% show_info()
#' corpus("GERMAPARLMINI") %>% get_info()
#' corpus("GERMAPARLMINI") %>% show_info()
#'
setMethod("name", "corpus", function(x) x@name)


#' Regions of a CWB corpus.
#' 
#' Class to store and process the regions of a corpus. Regions are defined by
#' start and end corpus positions and correspond to a set of tokens surrounded
#' by start and end XML tags.
#' 
#' The `regions` class is a minimal representation of regions and does not
#' include information on the "strucs" (region IDs) that are used internally to
#' obtain values of s-attributes or information, which combination of conditions
#' on s-attributes has been used to obtain regions. This is left to the
#' `subcorpus` corpus class. Whereas the `subcorpus` class is associated with
#' the assumption, that a set of regions is a meaningful sub-unit of a corpus,
#' the `regions` class has a focus on the individual sequences of tokens defined
#' by a structural attribute (such as paragraphs, sentences, named entities).
#' 
#' Information on regions is maintained in the `cpos` slot of the `regions` S4
#' class: A two-column `matrix` with begin and end corpus positions (first and
#' second column, respectively). All other slots are inherited from the `corpus`
#' class.
#' 
#' The understanding of "regions" is modelled on the usage of terms by CWB
#' developers. As it is put in the
#' \href{https://cwb.sourceforge.io/files/CQP_Manual.pdf}{CQP Interface and
#' Query Language Manual}: "Matching pairs of XML start and end tags are encoded
#' as token regions, identified by the corpus positions of the first token
#' (immediately following the start tag) and the last token (immediately
#' preceding the end tag) of the region." (p. 6)
#' 
#' @slot cpos A two-column `matrix` with start and end corpus positions (first
#'   and second column, respectively).
#' @param x object of class `regions`
#' @param values values to assign to a column that will be added
#' @exportClass regions
#' @rdname regions_class
#' @name regions
#' @examples
#' use("polmineR")
#' P <- partition("GERMAPARLMINI", date = "2009-11-12", speaker = "Jens Spahn")
#' R <- as.regions(P)
#' @aliases regions-class
#' @family classes to manage corpora
setClass("regions", slots = c(cpos = "matrix"), contains = "corpus")




#' The S4 subcorpus class.
#' 
#' @description Class to manage subcorpora derived from a CWB corpus.
#' 
#' @slot s_attributes A named `list` with the structural attributes
#'   defining the subcorpus.
#' @slot cpos A `matrix` with left and right corpus positions defining
#'   regions (two column matrix with `integer` values).
#' @slot annotations Object of class `list`.
#' @slot size Total size (number of tokens) of the `subcorpus` object (a
#'   length-one `integer` vector). The value is accessible by calling 
#'   the `size`-method on the `subcorpus`-object (see examples).
#' @slot metadata Object of class `data.frame`, metadata information.
#' @slot strucs Object of class `integer`, the strucs defining the
#'   subcorpus.
#' @slot xml Object of class `character`, whether the xml is "flat" or
#'   "nested".
#' @slot s_attribute_strucs Object of class `character`, the base node.
#' @slot user If the corpus on the server requires authentication, the username.
#' @slot password If the corpus on the server requires authentication, the password.
#' @param object A `subcorpus` object.
#' @param x A `subcorpus` object.
#' @param ... Arguments passed into `size`-method. Used only to maintain
#'   backwards compatibility.
#' @inheritParams size
#' @seealso Most commonly, a `subcorpus` is derived from a `corpus` or
#'   a `subcorpus` using the \code{\link{subset}} method. See
#'   \code{\link{size}} for detailed documentation on how to use the
#'   `size`-method. The `subcorpus` class shares many features with
#'   the `partition` class, but it is more parsimonious and does not
#'   include information on statistical properties of the subcorpus (i.e. a
#'   count table). In line with this logic, the `subcorpus` class inherits
#'   from the `corpus` class, whereas the `partition` class inherits
#'   from the `textstat` class.
#' @family classes to manage corpora
#' @name subcorpus
#' @rdname subcorpus-class
#' @aliases subcorpus-class
#' @examples
#' use("polmineR")
#' 
#' # basic example 
#' r <- corpus("REUTERS")
#' k <- subset(r, grepl("kuwait", places))
#' name(k) <- "kuwait"
#' y <- summary(k)
#' s <- size(k)
#' 
#' # the same with a magrittr pipe
#' corpus("REUTERS") %>%
#'   subset(grepl("kuwait", places)) %>%
#'   summary()
#'   
#' # subsetting a subcorpus in a pipe
#' stone <- corpus("GERMAPARLMINI") %>%
#'   subset(date == "2009-11-10") %>%
#'   subset(speaker == "Frank-Walter Steinmeier")
#' 
#' # perform count for subcorpus
#' n <- corpus("REUTERS") %>% subset(grep("kuwait", places)) %>% count(p_attribute = "word")
#' n <- corpus("REUTERS") %>% subset(grep("saudi-arabia", places)) %>% count('"Saudi" "Arabia"')
#'   
#' # keyword-in-context analysis (kwic)   
#' k <- corpus("REUTERS") %>% subset(grep("kuwait", places)) %>% kwic("oil")
#' 
#' @exportClass subcorpus
setClass(
  "subcorpus",
  slots = c(
    s_attributes = "list",
    annotations = "list",
    metadata = "data.frame",
    strucs = "integer",
    xml = "character",
    s_attribute_strucs = "character"
  ),
  contains = "regions"
)

#' @exportClass remote_subcorpus
#' @rdname subcorpus-class
setClass(
  "remote_subcorpus",
  slots = c(
    server = "character",
    restricted = "logical"
  ),
  contains = "subcorpus"
)


setAs(from = "subcorpus", to = "remote_subcorpus", def = function(from){
  y <- new("remote_subcorpus")
  for (x in slotNames(from)) slot(y, x) <- slot(from, x)
  y
})




setAs(from = "remote_subcorpus", to = "subcorpus", def = function(from){
  y <- new("subcorpus")
  for (x in slotNames(y)) slot(y, x) <- slot(from, x)
  y
})



#' @describeIn subcorpus Get named list with basic information for
#'   `subcorpus` object.
#' @export
setMethod("summary", "subcorpus", function(object){
  list(
    name = if (length(name(object)) > 0L) name(object) else NA,
    size = object@size
  )
})


#' @param value A `character` vector to assign as name to slot `name`
#'   of a `subcorpus` class object.
#' @describeIn subcorpus Assign name to a `subcorpus` object.
#' @exportMethod name<-
setReplaceMethod("name", "subcorpus", function(x, value) {
  x@name <- value
  x
})


#' @rdname subcorpus-class
setClass("plpr_subcorpus", contains = "subcorpus")

#' @rdname subcorpus-class
setClass("press_subcorpus", contains = "subcorpus")



#' Manage and use phrases
#' 
#' @description Class, methods and functionality for processing phrases (lexical
#'   units, lexical items, multi-word expressions) beyond the token level. The
#'   envisaged workflow at this stage is to detect phrases using the
#'   `ngrams`-method and to generate a `phrases` class object from the
#'   `ngrams` object using the `as.phrases` method. This object can be
#'   passed into a call of `count`, see examples. Further methods and
#'   functions documented here are used internally, but may be useful.
#' @details The `phrases` considers a phrase as sequence as tokens that can
#'   be defined by region, i.e. a left and a right corpus position. This
#'   information is kept in a region matrix in the slot "cpos" of the
#'   `phrases` class. The `phrases` class inherits from the
#'   \code{\link{regions}} class (which inherits from the and the
#'   \code{\link{corpus}} class), without adding further slots.
#' @family classes to manage corpora
#' @name phrases
#' @rdname phrases-class
#' @aliases phrases-class
#' @examples
#' # Workflow to create document-term-matrix with phrases
#' 
#' obs <- corpus("GERMAPARLMINI") %>%
#'   count(p_attribute = "word")
#' 
#' phrases <- corpus("GERMAPARLMINI") %>%
#'   ngrams(n = 2L, p_attribute = "word") %>%
#'   pmi(observed = obs) %>% 
#'   subset(ngram_count > 5L) %>%
#'   subset(1:100) %>%
#'   as.phrases()
#' 
#' dtm <- corpus("GERMAPARLMINI") %>%
#'   as.speeches(s_attribute_name = "speaker", progress = TRUE) %>%
#'   count(phrases = phrases, p_attribute = "word", progress = TRUE, verbose = TRUE) %>%
#'   as.DocumentTermMatrix(col = "count", verbose = FALSE)
#'   
#' grep("erneuerbaren_Energien", colnames(dtm))
#' grep("verpasste_Chancen", colnames(dtm))
#' 
setClass(
  "phrases",
  contains = "regions"
)




#' @rdname features-class
#' @exportClass features_cooccurrences
setClass("features_cooccurrences", contains = "features")


#' @rdname features-class
#' @exportClass features_ngrams
setClass("features_ngrams", representation(n = "integer"), contains = "features")


#' @details A set of `features` objects can be combined into a `features_bundle`.
#' Typically, a `features_bundle` will result from applying the `features`-method
#' on a `partition_bundle`. See the documentation for `bundle` to learn about
#' the methods for `bundle` objects that are available for a `features_bundle`.
#' @rdname features-class
setClass("features_bundle", contains = "bundle")




#' Ngrams class.
#' 
#' @exportClass ngrams
#' @rdname ngrams_class
#' @name ngrams_class
#' @aliases ngrams-class
setClass("ngrams", representation(n = "integer"), contains = "count")


#' S4 class to represent hits for queries.
#' 
#' @slot stat A `data.table` with the following columns: 
#' \describe{
#'   \item{query}{The query (optionally using CQP syntax) that evoked
#'   a hit.}
#'   \item{count}{Number of matches in corpus/subcorpus.}
#'   \item{freq}{Relative frequency of matches in corpus/subcorpus (optional,
#'   presence depends on usage of argument `freq` of the `hits` method).}
#'   \item{size}{Total number of tokens in corpus/subcorpus (optional, presence
#'   depends on usage of argument `size` of the `hits` method).}
#' }
#' If argument `s_attribute` has been used in the call of the `hits`
#' method, the `data.table` will include additional columns with the
#' s-attributes. The values in the columns will be the values these s-attributes
#' assume. Columns `count`, `freq` and `size` will be based on
#' subcorpora defined by (combinations of) s-attributes.
#' @slot corpus A length-one `"character"` vector, ID of the corpus with
#'   hits for query or queries.
#' @slot query Object of class `"character"`, query or queries for 
#' @slot p_attribute The p-attribute that has been queried, a length-one
#'   `character` vector.
#' @slot encoding Length-one `character` vector, the encoding of the
#'   corpus.
#' @slot name Length-one `characte` vector, name of the object.
#' @exportClass hits
#' @rdname hits_class
#' @name hits_class
#' @aliases hits-class
setClass(
  "hits",
  representation(query = "character"),
  contains = "textstat"
)


#' S4 context_bundle class
#' 
#' class to organize information of multiple context analyses
#' 
#' @slot objects Object of class `"list"` a list of context objects
#'
#' @section Methods:
#'   \describe{
#'     \item{show}{output of core information}
#'     \item{summary}{core statistical information}
#'     \item{[}{specific cooccurrences}
#'     \item{[[}{specific cooccurrences}
#'    }
#'     
#' @name context_bundle-class
#' @aliases show,context_bundle-method summary,context_bundle-method [,context_bundle-method [,context_bundle,ANY,ANY,ANY-method [[,context_bundle-method
#' @docType class
#' @exportClass kwic
#' @rdname context_bundle-class
setClass(
  "context_bundle",
  slots = c(
    query = "character"
  ),
  contains = "bundle"
)


#' @rdname partition_class
setClass(
  "plpr_partition",
  contains = "partition",
  prototype = list(
    stat = data.table()
  )
)

#' @rdname partition_class
setClass(
  "press_partition",
  contains = "partition",
  prototype = list(
    stat = data.table()
  )
)


#' Bundle of partitions (partition_bundle class).
#' 
#' Class and methods to manage bundles of partitions. 
#' 
#' @slot objects Object of class `list` the partitions making up the bundle
#' @slot corpus Object of class `character` the CWB corpus the partition is based on
#' @slot s_attributes_fixed Object of class `list` fixed s-attributes
#' @slot encoding Object of class `character` encoding of the corpus
#' @slot explanation Object of class `character` an explanation of the partition
#' @slot xml Object of class `character` whether the xml is flat or nested
#' @slot call Object of class `character` the call that generated the `partition_bundle`
#' @aliases partition_bundle-class
#'   [,partition_bundle-method [[,partition_bundle-method
#'   as.matrix,partition_bundle-method 
#'   merge,partition_bundle-method
#'   +,partition_bundle-method names,partition_bundle-method 
#'   summary,partition_bundle-method +,partition_bundle,ANY-method
#'   [,partition_bundle,ANY,ANY,ANY-method +,partition_bundle,partition-method 
#'   +,partition_bundle,partition_bundle-method as.partition_bundle,list-method 
#'   barplot,partition_bundle-method
#' @param x a `partition_bundle` object
#' @param .Object a `partition_bundle` object
#' @param object a `partition_bundle` object
#' @param i integer index
#' @param s_attribute the s-attribute to use
#' @param height height
#' @param ... further parameters
#' @rdname partition_bundle-class
#' @name partition_bundle-class
#' @exportClass partition_bundle
#' @author Andreas Blaette
setClass("partition_bundle",
         slots = c(
           s_attributes_fixed = "list",
           explanation = "character",
           xml = "character",
           call = "character"
         ),
         contains = "count_bundle"
)


#' @title Bundled subcorpora
#' @description A `subcorpus_bundle` object combines a set of
#'   `subcorpus` objects in a `list` in the the slot `objects`.
#'   The class inherits from the `partition_bundle` and the `bundle`
#'   class. Typically, a `subcorpus_bundle` is generated by applying the
#'   `split`-method on a `corpus` or `subcorpus`.
#' @exportClass subcorpus_bundle
#' @rdname subcorpus_bundle
#' @examples 
#' corpus("REUTERS") %>% split(s_attribute = "id") %>% summary()
setClass("subcorpus_bundle",
         slots = c(
           s_attributes_fixed = "list",
           xml = "character"
         ),
         contains = "partition_bundle"
)


#' @export
setAs(from = "partition_bundle", to = "subcorpus_bundle", def = function(from){
  type <- get_type(from)
  
  dest_class <- if (is.null(type))
    "subcorpus"
  else
    paste(type, "subcorpus", sep = "_")
  
  y <- as(as(from, "corpus"), "subcorpus_bundle")
  y@objects <- lapply(from@objects, function(x) as(x, dest_class))
  y@s_attributes_fixed <- from@s_attributes_fixed
  y@xml <- from@xml
  
  y
})


#' @rdname cooccurrences-class
setClass("cooccurrences_reshaped", contains = "cooccurrences")


#' @name cooccurrences_bundle-class
#' @aliases cooccurrences_bundle
#' @docType class
#' @exportClass cooccurrences_bundle
#' @rdname cooccurrences-class
setClass("cooccurrences_bundle", contains = "bundle")


#' Virtual class slice.
#' 
#' The classes `subcorpus` and `partition` can be used to define
#' subcorpora. Unlike the `subcorpus` class, the `partition` class may
#' include statistical evaluations. The virtual class `slice` is a
#' mechanism to define methods for these classes without making `subcorpus`
#' the superclass of `partition`.
#' 
#' @rdname slice
#' @name slice
#' @docType class
#' @aliases slice-class
#' @exportClass slice
setClassUnion(
  name = "slice",
  members = c("subcorpus", "regions", "partition")
)


setAs(from = "corpus", to = "subcorpus", def = function(from){
  new(
    "subcorpus",
    corpus = from@corpus,
    data_dir = from@data_dir,
    template = from@template,
    registry_dir = from@registry_dir,
    info_file = from@info_file,
    encoding = from@encoding,
    cpos = matrix(data = c(0L, (size(from) - 1L)), nrow = 1L),
    size = size(from)
  )
})






#' Ranges of query matches.
#' 
#' S4 class to manage ranges of corpus positions for query matches. The class
#' inherits from the classes `regions` and `corpus`.
#' 
#' @slot query A length-one `character` string, query used for query
#'   matches.
#' @exportClass ranges
#' @rdname ranges_class
#' @family classes to manage corpora
setClass("ranges", slots = c(query = "character"), contains = "regions")


#' @details The method `aggregate` will deflate the matrix in the slot `cpos`,
#' i.e. it checks for each new row in the matrix whether it increments the end
#' of the previous region (by 1), and ensure that the cpos matrix defines
#' disjoined regions.
#' 
#' @param x An object of a class belonging to the virtual class `slice`, i.e. a 
#' `partition` or `regions` object.
#' @exportMethod aggregate
#' @rdname slice
#' @examples 
#' P <- new(
#'   "partition",
#'   cpos = matrix(data = c(1:10, 20:29), ncol = 2, byrow = TRUE),
#'   stat = data.table::data.table()
#' )
#' P2 <- aggregate(P)
#' P2@cpos
setMethod("aggregate", "slice", function(x){
  if (nrow(x@cpos) == 1L){
    message("NOTE: Only one region, returning the partition unchanged")
    return(x)
  }
  jumps <- x@cpos[2L:nrow(x@cpos), 1L] - x@cpos[1L:(nrow(x@cpos) - 1L), 2L]
  jumpsWhere <- c(0L, which(jumps > 1L), nrow(x@cpos)) + 1L
  rework <- lapply(
    1L:(length(jumpsWhere) - 1L),
    function(i) c(x@cpos[jumpsWhere[i], 1L], x@cpos[jumpsWhere[i + 1L] - 1L, 2L])
  )
  x@cpos <- do.call(rbind, rework)
  x
})

