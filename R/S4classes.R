#' @include polmineR.R
NULL


#' Bundle Class
#' 
#' A \code{bundle} is used to combine several objects (\code{partition}, \code{context},
#' \code{features}, \code{cooccurrences} objects) into one S4 class object. Typically,
#' a class inheriting from the \code{bundle} superclass will be used. When working with a
#' \code{context_bundle}, a \code{features_bundle}, a \code{cooccurrences_bundle}, or a
#' \code{context_bundle}, a similar set of standard methods is available to perform 
#' transformations.
#' 
#' @slot corpus The CWB corpus the objects in the \code{bundle} are based on, a length 1 
#' \code{character} vector.
#' @slot objects An object of class \code{list}.
#' @slot p_attribute Object of class \code{character}.
#' @slot encoding The encoding of the corpus.
#' 
#' @param x a bundle object
#' @param i An \code{integer} value to index a bundle object.
#' @param object A \code{bundle} object.
#' @param size number of items to choose to generate a sample
#' @param ... Further parameters
#' @param col columns of the \code{data.table} to use to generate an object.
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
  representation(
    objects = "list",
    p_attribute = "character",
    corpus = "character",
    encoding = "character"
  )
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
#' The \code{textstat}-class (technically an S4 class) serves as a superclass
#' for the classes \code{features}, \code{context}, and \code{partition}.
#' Usually, the class will not be used directly. It offers a set of standard
#' generic methods (such as \code{head}, \code{tail}, \code{dim}, \code{nrow},
#' \code{colnames}) its childs inherit. The core feature of \code{textstat} and
#' its childs is a \code{data.table} in the slot \code{stat} for keeping data on
#' text statistics of a corpus, or a \code{partition}.
#' 
#' A \code{head}-method will return the first rows of the \code{data.table} in
#' the \code{stat}-slot. Use argument \code{n} to specify the number of rows.
#'
#' A \code{tail}-method will return the last rows of the \code{data.table} in
#' the \code{stat}-slot. Use argument \code{n} to specify the number of rows.
#'
#' The methods \code{dim}, \code{nrow} and \code{ncol} will return information
#' on the dimensions, the number of rows, or the number of columns of the
#' \code{data.table} in the \code{stat}-slot, respectively.
#' 
#' Objects derived from the \code{textstat} class can be indexed with simple
#' square brackets ("[") to get rows specified by an numeric/integer vector,
#' and with double square brackets ("[[") to get specific columns from the 
#' \code{data.table} in the slot \code{stat}.
#' 
#' The \code{colnames}-method will return the column names of the \code{data-table}
#' in the slot \code{stat}.
#' 
#' The methods \code{as.data.table}, and \code{as.data.frame} will extract the
#' \code{data.table} in the slot \code{stat} as a \code{data.table}, or
#' \code{data.frame}, respectively.
#' 
#' @slot p_attribute Object of class \code{character}, p-attribute of the query.
#' @slot corpus A corpus specified by a length-one \code{character} vector. 
#' @slot stat A \code{data.table} with statistical information.
#' @slot name The name of the object.
#' @slot annotation_cols A \code{character} vector, column names of
#'   \code{data.table} in slot \code{stat} that are annotations.
#' @slot encoding A length-one \code{character} vector, the encoding of the corpus.
#' @param .Object A \code{textstat} object.
#' @param x A \code{textstat} object.
#' @param by Column that will serve as the key for sorting.
#' @param decreasing Logical, whether to return decreasing order.
#' @param e1 A \code{texstat} object.
#' @param e2 Another \code{texstat} object.
#' @param ... Further arguments.
#' @aliases as.data.frame,textstat-method show,textstat-method
#'   dim,textstat-method
#'   colnames,textstat-method rownames,textstat-method names,textstat-method
#'   as.DataTables,textstat-method head,textstat-method tail,textstat-method
#'   dim,textstat-method nrow,textstat-method ncol,textstat-method
#'   colnames,textstat-method as.data.table,textstat-method as.data.frame,textstat-method
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
#' @aliases as.data.table
setClass("textstat",
         representation(
           corpus = "character",
           p_attribute = "character",
           encoding = "character",
           stat = "data.table",
           annotation_cols = "character",
           name = "character"
         )
)


#' @details \code{textstat} objects can have a name, which can be retrieved, and set using
#' the \code{name}-method and \code{name<-}, respectively.
#' @rdname textstat-class
setMethod("name", "textstat", function(x) x@name)

#' @rdname textstat-class
setMethod("name", "character", function(x) x)

#' @param value A \code{character} vector to assign as name to slot \code{name}
#'   of a \code{textstat} class object.
#' @rdname textstat-class
#' @exportMethod name<-
setReplaceMethod("name", signature = "textstat", function(x, value) {
  x@name <- value
  x
})



#' Feature selection by comparison.
#' 
#' The  \code{features}-method returns a \code{features}-object. Several
#' \code{features}-objects can be combined into a \code{features_bundle}-object.
#' 
#' @slot corpus The CWB corpus the features are derived from, a \code{character} vector of length 1.
#' @slot p_attribute Object of class \code{character}.
#' @slot encoding Object of class \code{character}.
#' @slot corpus Object of class \code{character}.
#' @slot stat Object of class \code{data.frame}.
#' @slot size_coi Object of class \code{integer}. 
#' @slot size_ref Object of class \code{integer}.
#' @slot included Object of class \code{logical} whether corpus of interest is included in reference corpus
#' @slot method Object of class \code{character} statisticalTest used
#' @slot call Object of class \code{character} the call that generated the object
#' 
#' @param object A \code{features} or \code{features_bundle} object.
#' @param .Object a \code{features} object.
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
#' S4 class to organize counts. The classes \code{polmineR} and
#' \code{ngrams} inherit from the class.
#' 
#' @slot stat Object of class \code{data.table}.
#' @slot corpus Object of class \code{character} the CWB corpus the partition is based on .
#' @slot encoding Object of class \code{character}, the encoding of the corpus.
#' @slot name Object of class \code{character}, a name for the object.
#' @slot size Object of class \code{integer}, the size of the partition or
#'   corpus the count is based upon.
#' @rdname count_class
#' @param ... Further parameters.
#' @name count_class
#' @exportClass count
#' @docType class
#' @author Andreas Blaette
#' @aliases count-class
#' @seealso The \code{count}-class inherits from the \code{\link{textstat-class}}
setClass("count",
         representation = list(
           size = "integer"
           ),
         contains = "textstat"
         )


#' @param object A \code{count} object.
#' @details The \code{summary}-method in combination with a weighed
#'   \code{count}-object can be used to perform a dictionary-based sentiment
#'   analysis (see examples).
#' @rdname count_class
#' @examples
#' # sample for dictionary-based sentiment analysis
#'     weights <- data.table::data.table(
#'     word = c("gut", "schön", "herrlich", "schlecht", "hässlich", "mies"),
#'     weight = c(1,1,1,-1,-1,-1)
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
#'   word = c("gut", "schön", "herrlich", "schlecht", "hässlich", "mies"),
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



#' @details The \code{length}-method is synonymous with the \code{size}-method
#' and will return the size of the \code{corpus} or \code{partition} a count
#' has been derived from.
#' @param x A \code{count} object, or a class inheriting from \code{count}.
#' @exportMethod length
#' @rdname count_class
setMethod("length", "count", function(x) x@size)




#' Partition class and methods.
#' 
#' The \code{partition} class is used to manage subcorpora. It is an S4 class, and
#' a set of methods is defined for the class. The class inherits
#' from the classes \code{count} and \code{textstat}.
#' 
#' @details As \code{partition} objects inherit from \code{count} and
#'   \code{textstat} class, methods available are \code{view} to inspect the
#'   table in the \code{stat} slot, \code{name} and \code{name<-} to
#'   retrieve/set the name of an object, and more.
#' 
#' @slot name A name to identify the object (\code{character} vector with length 1); useful when multiple
#' \code{partition} objects are combined to a \code{partition_bundle}.
#' @slot corpus The CWB indexed corpus the partition is derived from (\code{character} vector with length 1).
#' @slot encoding Encoding of the corpus (\code{character} vector with length 1).
#' @slot s_attributes A named \code{list} with the s-attributes specifying the partition.
#' @slot explanation Object of class \code{character}, an explanation of the partition.
#' @slot cpos A \code{matrix} with left and right corpus positions defining regions (two columns).
#' @slot annotations Object of class \code{list}.
#' @slot size Total size of the partition (\code{integer} vector, length 1).
#' @slot stat An (optional) \code{data.table} with counts. If present, speeds up computation of cooccurrences,
#' as count is already present.
#' @slot metadata Object of class \code{data.frame}, metadata information.
#' @slot strucs Object of class \code{intger}, the strucs defining the partition.
#' @slot p_attribute Object of class \code{character} indicating the p_attribute of the
#' count in slot \code{stat}.
#' @slot xml Object of class \code{character}, whether the xml is flat or nested.
#' @slot s_attribute_strucs Object of class \code{character} the base node 
#' @slot key Experimental, an s-attribute that is used as a key.
#' @slot call Object of class \code{character} the call that generated the partition 
#' @param .Object A \code{partition} object.
#' @param p_attribute a p-attribute (for enriching) / performing count.
#' @param verbose \code{logical} value, whether to output messages
#' @param ... further parameters passed into \code{count} when calling \code{enrich}, and ...
#' @aliases partition-class show,partition-method [,partition,ANY,ANY,ANY-method 
#'   [,partition-method as.partition_bundle 
#'   as.partition_bundle,partition-method export export,partition-method split
#' @rdname partition_class
#' @name partition_class
#' @exportClass partition
#' @docType class
#' @author Andreas Blaette
#' @seealso The \code{partition}-class inherits from the \code{\link{textstat-class}}, see
#' respective documentation to learn more.
setClass(
  "partition",
  representation(
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
  contains = "count"
)

#' @exportClass remote_partition
#' @rdname partition_class
setClass(
  "remote_partition",
  slots = c(server = "character"),
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
#' @details Objects of the class \code{context} include a \code{data.table} in the
#' slot \code{cpos}. The \code{data.table} will at least include the columns "match_id",
#' "cpos" and "position".
#' 
#' @slot query The query examined (\code{character}).
#' @slot count An \code{integer} value, the number of hits for the query.
#' @slot partition The \code{partition} the \code{context} object is based on.
#' @slot size_partition The size of the partition, a length-one \code{integer} vector.
#' @slot left A length-one \code{integer} value, the number of tokens to the left of the query match.
#' @slot right An \code{integer} value, the number of tokens to the right of the query match.
#' @slot size A length-one \code{integer} value, the number of tokens covered by
#'   the \code{context}-object, i.e. the number of tokens in the right and left context
#'   of the node as well as query matches.
#' @slot size_match A length-one \code{integer} value, the number of tokens
#'   matches by the query. Identical with the value in slot \code{count} if the query
#'   is \emph{not} a CQP query.
#' @slot size_coi A length-one \code{integer} value, the number of tokens in the
#'   right and left context of the node (excluding query matches).
#' @slot size_ref A length-one \code{integer} value, the number of tokens in the
#'   partition, without tokens matched and the tokens in the left and right
#'   context.
#' @slot boundary An s-attribute (\code{character}).
#' @slot p_attribute The p-attribute of the query (\code{character}).
#' @slot corpus The CWB corpus used (\code{character}).
#' @slot stat A \code{data.table}, the statistics of the analysis.
#' @slot encoding Object of class \code{character}, encoding of the corpus.
#' @slot cpos A \code{data.table}, with the columns match_id, cpos, position, word_id.
#' @slot method A \code{character}-vector, statistical test used.
#' @slot call Object of class \code{character}, call that generated the object.
#'     
#' @param .Object object
#' @param x a context object
#' @param size integer indicating sample size
#' @param object a context object
#' @param positivelist tokens that are required to be present to keep a match
#' @param stoplist tokens that are used to exclude a match
#' @param regex logical, whether positivlist / stoplist is interpreted as regular expressions
#' @param progress logical, whether to show progress bar
#' @param ... to maintain backwards compatibility if argument \code{pAttribute} is still used
#' @aliases show,context-method [,context-method [,context,ANY,ANY,ANY-method
#'   [[,context-method summary,context-method head,context-method
#'   as.DataTables,context-method
#' @docType class
#' @rdname context-class
#' @exportClass context
setClass("context",
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

#' @details The \code{length}-method will return the number of hits that were achieved.
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
#' @slot call Object of class \code{character} the call that generated the object
#' @slot partition Object of class \code{character} the partition the analysis is based on
#' @slot size_partition  Object of class \code{integer} the size of the partition
#' @slot left  Object of class \code{integer} number of tokens to the left.
#' @slot right  Object of class \code{integer} number of tokens to the right.
#' @slot p_attribute  Object of class \code{character} p-attribute of the query
#' @slot corpus  Object of class \code{character} the CWB corpus used
#' @slot stat  Object of class \code{data.table} statistics of the analysis
#' @slot encoding  Object of class \code{character} encoding of the corpus
#' @slot method  Object of class \code{character} statistical test(s) used
#' @aliases cooccurrences-class
#' @docType class
#' @exportClass cooccurrences
#' @rdname cooccurrences-class
setClass("cooccurrences", contains = "context")




#' S4 kwic class
#' 
#' S4 class for organizing information for kwic/concordance output. A set of
#' standard generics (\code{show}, \code{as.character}, \code{as.data.frame},
#' \code{length}, \code{sample}, \code{subset}) as well as indexing is implemented to process
#' kwic class objects (see 'Usage'). See section 'Details' for the
#' \code{enrich}, \code{view} and \code{knit_print} methods.
#' 
#' @slot metadata A \code{character} vector with s-attributes of the metadata
#'   that are to be displayed.
#' @slot p_attribute The p-attribute for which the context has been generated.
#' @slot left An \code{integer} value, words to the left of the query match. 
#' @slot right An \code{integer} value, words to the right of the query match.
#' @slot corpus Length-one \code{character} vector, the CWB corpus.
#' @slot cpos A \code{data.table} with the columns "match_id", "cpos", "position",
#'   "word_id", "word" and "direction".
#' @slot stat A \code{data.table}, a table with columns "left", "node",
#'   "right", and metadata, if the object has been enriched.
#' @slot encoding A length-one \code{character} vector with the encoding of the
#'   corpus.
#' @slot name A length-one \code{character} vector naming the object.
#' @slot annotation_cols A \code{character} vector designating the columns of
#'   the \code{data.table} in the slot \code{table} that are annotations.
#' 
#' @param x A \code{kwic} class object.
#' @param object A \code{kwic} class object.
#' @param s_attributes Character vector of s-attributes with metainformation.
#' @param table Logical, whether to turn cpos \code{data.table} into
#'   \code{data.table} in slot \code{stat} for output.
#' @param p_attribute A length-one \code{character} vector supplying a p-attribute.
#' @param verbose A \code{logical} value, whether to output debugging messages.
#' @param extra An \code{integer} value, number of extra tokens to the left and
#'   to the right of the windows of tokens to the left and right of a query
#'   match that are decoded to be displayed in a kwic output to facilitate
#'   interpretation.
#' @param size An \code{integer}, subset size for sampling.
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



#' Corpus class initialization
#' 
#' Corpora indexed using the Corpus Workbench (CWB) offer an efficient data
#' structure for large, linguistically annotated corpora. The
#' \code{corpus}-class keeps basic information on a CWB corpus. Corresponding to
#' the name of the class, the \code{corpus}-method is the initializer for
#' objects of the \code{corpus} class. A CWB corpus can also be hosted remotely
#' on an \href{https://www.opencpu.org}{OpenCPU} server. The \code{remote_corpus}
#' class (which inherits from the \code{corpus} class) will handle respective
#' information. A (limited) set of polmineR functions and methods can be
#' executed on the corpus on the remote machine from the local R session by
#' calling them on the \code{remote_corpus} object. Calling the
#' \code{corpus}-method without an argument will return a \code{data.frame} with
#' basic information on the corpora that are available.
#'
#' @details Calling \code{corpus()} will return a \code{data.frame} listing the corpora
#' available locally and described in the active registry directory, and some
#' basic information on the corpora.
#' @details A \code{corpus} object is instantiated by passing a corpus ID as
#'   argument \code{.Object}. Following the conventions of the Corpus Workbench
#'   (CWB), Corpus IDs are written in upper case. If \code{.Object} includes
#'   lower case letters, the \code{corpus} object is instantiated nevertheless,
#'   but a warning is issued to prevent bad practice. If \code{.Object} is not a
#'   known corpus, the error message will include a suggestion if there is a
#'   potential candidate that can be identified by \code{agrep}.
#' @details A limited set of methods of the \code{polmineR} package is exposed
#'   to be executed on a remote OpenCPU server. As a matter of convenience, the
#'   whereabouts of an OpenCPU server hosting a CWB corpus can be stated in an
#'   environment variable "OPENCPU_SERVER". Environment variables for R sessions
#'   can be set easily in the \code{.Renviron} file. A convenient way to do this
#'   is to call \code{usethis::edit_r_environ()}.
#'     
#' @slot corpus A length-one \code{character} vector, the upper-case ID of a CWB
#'   corpus.
#' @slot data_dir The directory where the files for the indexed corpus are.
#' @slot type The type of the corpus (e.g. "plpr" for a corpus of plenary protocols).
#' @slot name An additional name for the object that may be more telling than the corpus ID.
#' @slot encoding The encoding of the corpus, given as a length-one \code{character} vector.
#' @slot server The URL (can be IP address) of the OpenCPU server. The slot is
#'   available only with the \code{remote_corpus} class inheriting from the
#'   \code{corpus} class.
#' @slot user If the corpus on the server requires authentication, the username.
#' @slot password If the corpus on the server requires authentication, the password.
#' @exportClass corpus
#' @aliases zoom corpus get_corpus remote_corpus remote_corpus-class
#' @name corpus-class
#' @seealso Methods to extract basic information from a \code{corpus} object are
#'   covered by the \link{corpus-methods} documentation object. Use the
#'   \code{\link{s_attributes}} method to get information on structural
#'   attributes. Analytical methods available for \code{corpus} objects are
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
    data_dir = "character",
    type = "character",
    encoding = "character",
    name = "character"
  )
)

setAs(from = "partition", to = "corpus", def = function(from){
  new(
    "corpus",
    corpus = from@corpus,
    data_dir = registry_get_home(corpus = from@corpus, registry = registry()),
    type = if (class(from) == "partition") character() else gsub("^(.*?)_.*$", "\\1", class(from)),
    encoding = from@encoding,
    name = from@name
  )
})

setAs(from = "corpus", to = "partition", def = function(from){
  new(
    "partition",
    corpus = from@corpus,
    encoding = from@encoding,
    cpos = matrix(data = c(0L, (size(from) - 1L)), nrow = 1L),
    stat = data.table(),
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
    user = "character",
    password = "character"
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
#' objects of the \code{corpus} class.
#' @param object An object of class \code{corpus}, or inheriting from it.
#' @aliases name,corpus-method
#' @name corpus-methods
#' @rdname corpus_methods
#' @details A \code{corpus} object can have a name, which can be retrieved using
#' the \code{name}-method.
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
#' A coerce-method is available to coerce a \code{partition} object to a
#' \code{regions} object.
#' 
#' The virtual class \code{CorpusOrSubcorpus} is a way to handle corpora specified
#' by a character vector, \code{region} objects, and \code{partition} objects
#' in a uniform manner.
#' 
#' @slot cpos a two-column \code{data.table} that will include a "cpos_left" and "cpos_right" column
#' @slot corpus the CWB corpus (character vector length 1)
#' @slot encoding the encoding of the CWB corpus (character vector length 1)
#' @param x object of class \code{regions}
#' @param values values to assign to a column that will be added
#' @exportClass regions
#' @rdname regions_class
#' @name regions
#' @aliases CorpusOrSubcorpus-class regions-class CorpusOrSubcorpus
#' @examples
#' use("polmineR")
#' P <- partition("GERMAPARLMINI", date = "2009-11-12", speaker = "Jens Spahn")
#' R <- as.regions(P)
#' @aliases regions-class
#' @family classes to manage corpora
setClass(
  "regions",
  slots = c(
    cpos = "matrix",
    size = "integer"
  ),
  contains = "corpus"
)


#' The S4 subcorpus class.
#' 
#' @description Class to manage subcorpora derived from a CWB corpus.
#' 
#' @slot s_attributes A named \code{list} with the structural attributes
#'   defining the subcorpus.
#' @slot cpos A \code{matrix} with left and right corpus positions defining
#'   regions (two column matrix with \code{integer} values).
#' @slot annotations Object of class \code{list}.
#' @slot size Total size (number of tokens) of the \code{subcorpus} object (a
#'   length-one \code{integer} vector). The value is accessible by calling 
#'   the \code{size}-method on the \code{subcorpus}-object (see examples).
#' @slot metadata Object of class \code{data.frame}, metadata information.
#' @slot strucs Object of class \code{integer}, the strucs defining the
#'   subcorpus.
#' @slot xml Object of class \code{character}, whether the xml is "flat" or
#'   "nested".
#' @slot s_attribute_strucs Object of class \code{character}, the base node.
#' @slot user If the corpus on the server requires authentication, the username.
#' @slot password If the corpus on the server requires authentication, the password.
#' @param object A \code{subcorpus} object.
#' @param x A \code{subcorpus} object.
#' @param ... Arguments passed into \code{size}-method. Used only to maintain
#'   backwards compatibility.
#' @inheritParams size
#' @seealso Most commonly, a \code{subcorpus} is derived from a \code{corpus} or
#'   a \code{subcorpus} using the \code{\link{subset}} method. See
#'   \code{\link{size}} for detailed documentation on how to use the
#'   \code{size}-method. The \code{subcorpus} class shares many features with
#'   the \code{partition} class, but it is more parsimonious and does not
#'   include information on statistical properties of the subcorpus (i.e. a
#'   count table). In line with this logic, the \code{subcorpus} class inherits
#'   from the \code{corpus} class, whereas the \code{partition} class inherits
#'   from the \code{textstat} class.
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
    user = "character",
    password = "character"
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
#'   \code{subcorpus} object.
#' @export
setMethod("summary", "subcorpus", function(object){
  list(
    name = if (length(name(object)) > 0L) name(object) else NA,
    size = object@size
  )
})


#' @param value A \code{character} vector to assign as name to slot \code{name}
#'   of a \code{subcorpus} class object.
#' @describeIn subcorpus Assign name to a \code{subcorpus} object.
#' @exportMethod name<-
setReplaceMethod("name", "subcorpus", function(x, value) {
  x@name <- value
  x
})


#' @rdname subcorpus-class
setClass("plpr_subcorpus", contains = "subcorpus")

#' @rdname subcorpus-class
setClass("press_subcorpus", contains = "subcorpus")



#' @rdname features-class
#' @exportClass features_cooccurrences
setClass("features_cooccurrences", contains = "features")


#' @rdname features-class
#' @exportClass features_ngrams
setClass("features_ngrams", representation(n = "integer"), contains = "features")


#' @details A set of \code{features} objects can be combined into a \code{features_bundle}.
#' Typically, a \code{features_bundle} will result from applying the \code{features}-method
#' on a \code{partition_bundle}. See the documentation for \code{bundle} to learn about
#' the methods for \code{bundle} objects that are available for a \code{features_bundle}.
#' @rdname features-class
setClass("features_bundle", contains = "bundle")




#' Ngrams class.
#' 
#' @exportClass ngrams
#' @rdname ngrams_class
#' @name ngrams_class
#' @aliases ngrams-class
setClass("ngrams", representation(n = "integer"), contains = "count")


#' Hits class.
#' @slot stat a \code{"data.table"}
#' @slot corpus a \code{"character"} vector
#' @slot query Object of class \code{"character"}
#' @slot p_attribute p-attribute that has been queried
#' @slot encoding encoding of the corpus
#' @slot name name of the object
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
#' @slot objects Object of class \code{"list"} a list of context objects
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
setClass("context_bundle",
         slots = c(query = "character"),
         contains = "bundle"
)


#' @rdname partition_class
setClass("plpr_partition", contains = "partition")

#' @rdname partition_class
setClass("press_partition", contains = "partition")


#' Bundle of partitions (partition_bundle class).
#' 
#' Class and methods to manage bundles of partitions. 
#' 
#' @slot objects Object of class \code{list} the partitions making up the bundle
#' @slot corpus Object of class \code{character} the CWB corpus the partition is based on
#' @slot s_attributes_fixed Object of class \code{list} fixed s-attributes
#' @slot encoding Object of class \code{character} encoding of the corpus
#' @slot explanation Object of class \code{character} an explanation of the partition
#' @slot xml Object of class \code{character} whether the xml is flat or nested
#' @slot call Object of class \code{character} the call that generated the \code{partition_bundle}
#' @aliases partition_bundle-class
#'   [,partition_bundle-method [[,partition_bundle-method
#'   as.matrix,partition_bundle-method 
#'   merge,partition_bundle-method
#'   +,partition_bundle-method names,partition_bundle-method 
#'   summary,partition_bundle-method +,partition_bundle,ANY-method
#'   [,partition_bundle,ANY,ANY,ANY-method +,partition_bundle,partition-method 
#'   +,partition_bundle,partition_bundle-method as.partition_bundle,list-method 
#'   barplot,partition_bundle-method
#' @param x a \code{partition_bundle} object
#' @param .Object a \code{partition_bundle} object
#' @param object a \code{partition_bundle} object
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
#' @description A \code{subcorpus_bundle} object combines a set of
#'   \code{subcorpus} objects in a \code{list} in the the slot \code{objects}.
#'   The class inherits from the \code{partition_bundle} and the \code{bundle}
#'   class. Typically, a \code{subcorpus_bundle} is generated by applying the
#'   \code{split}-method on a \code{corpus} or \code{subcorpus}.
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
  dest_class <- if (is.null(type)) "subcorpus" else paste(type, "subcorpus", sep = "_")
  new(
    "subcorpus_bundle",
    objects = lapply(from@objects, function(x) as(x, dest_class)),
    p_attribute = character(), # unlike a partition_bundle, a subcorpus_bundle will never include a count
    corpus = from@corpus,
    encoding = from@encoding,
    s_attributes_fixed = from@s_attributes_fixed,
    xml = from@xml
  )
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
#' The classes \code{subcorpus} and \code{partition} can be used to define
#' subcorpora. Unlike the \code{subcorpus} class, the \code{partition} class may
#' include statistical evaluations. The virtual class \code{slice} is a
#' mechanism to define methods for these classes without making \code{subcorpus}
#' the superclass of \code{partition}.
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


#' @details The method \code{aggregate} will deflate the matrix in the slot \code{cpos},
#' i.e. it checks for each new row in the matrix whether it increments the end
#' of the previous region (by 1), and ensure that the cpos matrix defines
#' disjoined regions.
#' 
#' @param x An object of a class belonging to the virtual class \code{slice}, i.e. a 
#' \code{partition} or \code{regions} object.
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

