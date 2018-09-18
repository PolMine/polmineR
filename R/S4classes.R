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
#' @slot objects An object of class \code{"list"}
#' @slot p_attribute Object of class \code{"character"}
#' @slot encoding The encoding of the corpus.
#' 
#' @param x a bundle object
#' @param i integer to index a bundle object
#' @param object a bundle object
#' @param size number of items to choose to generate a sample
#' @param ... further parameters
#' @param col columns of the data.table to use to generate an object
#' @param value character string with a name to be assigned
#' @rdname bundle
#' @name bundle-class
#' @aliases bundle
#' @exportClass bundle
#' @docType class
#' @author Andreas Blaette
#' @examples
#' parties <- s_attributes("GERMAPARLMINI", "party")
#' parties <- parties[-which(parties == "NA")]
#' party_bundle <- partition_bundle("GERMAPARLMINI", s_attribute = "party")
#' length(party_bundle)
#' names(party_bundle)
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
setReplaceMethod("name", signature = c(x = "bundle", value = "character"), function(x, value) {
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
#' y[1:25]
#' y[,c("word", "ll")]
#' y[1:25, "word"]
#' y[1:25][["word"]]
#' y[which(y[["word"]] %in% c("Arbeit", "Sozial"))]
#' y[ y[["word"]] %in% c("Arbeit", "Sozial") ]
setClass("textstat",
         representation(
           corpus = "character",
           p_attribute = "character",
           encoding = "character",
           stat = "data.table",
           name = "character"
         )
)


#' @details \code{textstat} objects can have a name, which can be retrieved, and set using
#' the \code{name}-method and \code{name<-}, respectively.
#' @rdname textstat-class
setMethod("name", "textstat", function(x) x@name)

#' @param value A \code{character} vector to assign as name to slot \code{name}
#'   of a \code{textstat} class object.
#' @rdname textstat-class
#' @exportMethod name<-
setReplaceMethod("name", signature = c(x = "textstat", value = "character"), function(x, value) {
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
         representation(
           corpus = "character",
           p_attribute = "character",
           encoding = "character",
           stat = "data.table",
           size_coi = "integer",
           size_ref = "integer",
           method = "character",
           included = "logical",
           call = "character"
         ),
         contains = "textstat"
)


#' Count class.
#' 
#' S4 class to organize counts. The classes \code{polmineR} and
#' \code{ngrams} inherit from the class.
#' 
#' @slot stat Object of class \code{data.table}
#' @slot corpus Object of class \code{character} the CWB corpus the partition is based on 
#' @slot encoding Object of class \code{character} encoding of the corpus 
#' @slot name Object of class \code{character}, a name for the object
#' @slot size Object of class \code{integer}, the size of the partition or
#'   corpus the count is based upon
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
#' @details As \code{partition} objects inherit from \code{count} and \code{textstat} class, methods
#' available are \code{view} to inspect the table in the \code{stat} slot, \code{name} and 
#' \code{name<-} to retrieve/set the name of an object, and more.
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
#' @slot call Object of class \code{character} the call that generated the partition 
#' @param object A \code{partition} object.
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
    call = "character"
  ),
  contains = "count"
)


#' @rdname partition_class
setMethod("summary", "partition", function(object){
  y <- list(
    name = if (length(name(object)) > 0) name(object) else NA,
    size = size(object)
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



#' Context class.
#' 
#' Class to organize information of context analysis.
#' 
#' @details Objects of the class \code{context} include a \code{data.table} in the
#' slot \code{cpos}. The \code{data.table} will at least include the columns "hit_no",
#' "cpos" and "position".
#' 
#' @slot query The query used/node examined (\code{character}).
#' @slot count An \code{integer} value, the number of hits.
#' @slot partition The \code{partition} the \code{context} object is based on.
#' @slot size_partition A length-one \code{integer}, the size of the partition.
#' @slot left An \code{integer} value, the number of tokens to the left.
#' @slot right An \code{integer} value, the number of tokens to the right.
#' @slot size An \code{integer} value, number of tokens in the right and left context of the node.
#' @slot boundary An s-attribute (\code{character}).
#' @slot p_attribute The p-attribute of the query (\code{character}).
#' @slot corpus The CWB corpus used (\code{character}).
#' @slot stat A \code{data.table}, the statistics of the analysis.
#' @slot encoding Object of class \code{character}, encoding of the corpus.
#' @slot cpos A \code{data.table}, with the columns hit_no, cpos, position, word_id.
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
           left = "integer",
           right = "integer",
           size = "integer",
           boundary = "character",
           cpos = "data.table",
           call = "character"
         ),
         contains = c("features", "textstat")
)

#' @details The \code{length}-method will return the number of hits that were achieved.
#' @rdname context-class
#' @exportMethod length
setMethod("length", "context", function(x) as.integer(x@count))


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
#' @slot left  Object of class \code{numeric} number of tokens to the right
#' @slot right  Object of class \code{numeric} number of tokens to the left
#' @slot p_attribute  Object of class \code{character} p-attribute of the query
#' @slot corpus  Object of class \code{character} the CWB corpus used
#' @slot stat  Object of class \code{data.table} statistics of the analysis
#' @slot encoding  Object of class \code{character} encoding of the corpus
#' @slot pos  Object of class \code{character} part-of-speech tags filtered
#' @slot method  Object of class \code{character} statistical test(s) used
#' @slot cutoff  Object of class \code{list} cutoff levels that have been applied
#' @aliases cooccurrences-class
#' @docType class
#' @exportClass cooccurrences
#' @rdname cooccurrences-class
setClass(
  "cooccurrences",
  contains = c("context", "features", "textstat")
)




#' kwic (S4 class)
#' 
#' S4 class for organizing information for kwic/concordance output. A set of
#' standard generics (\code{show}, \code{as.character}, \code{as.data.frame},
#' \code{length}, \code{sample}, \code{subset}) as well as indexing is implemented to process
#' kwic class objects (see 'Usage'). See section 'Details' for the
#' \code{enrich}, \code{view} and \code{knit_print} methods.
#' 
#' @slot metadata A \code{character} vector with s-attributes of the metadata
#'   that are to be displayed.
#' @slot left An \code{integer} value, words to the left of the query match. 
#' @slot right An \code{integer} value, words to the right of the query match.
#' @slot corpus Length-one \code{character} vector, the CWB corpus.
#' @slot cpos A \code{data.table} with the columns "hit_no", "cpos", "position", "word_id", "word" and "direction".
#' @slot table A \code{data.frame}, a table with columns "left", "node", "right", and metadata, if the object has been enriched.
#' @slot encoding A length-one \code{character} vector with the encoding of the corpus.
#' @slot labels A \code{character} vector with labels.
#' @slot categories A \code{character} vector.
#' 
#' @param x A \code{kwic} class object.
#' @param object A \code{kwic} class object.
#' @param s_attributes Character vector of s-attributes with metainformation.
#' @param table Logical, whether to turn cpos \code{data.table} into \code{data.frame} for output.
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
#' length(K)
#' K[1]
#' K[1:5]
#' @rdname kwic-class
#' @include Labels.R
setClass(
  "kwic",
  slots = c(
    corpus = "character",
    cpos = "data.table",
    metadata = "character",
    left = "integer",
    right = "integer",
    table = "data.frame",
    encoding = "character",
    labels = "Labels",
    categories = "character"
  )
)


#' S4 class to capture core information on a temporary CWB corpus 
#' 
#' @slot cpos matrix with start/end corpus positions
#' @slot dir directory where the tempcorpus is stored
#' @slot registry directory of the registry dir (subdirectory of dir)
#' @slot indexed directory of the dir with the indexed files
#' @exportClass tempcorpus
#' @rdname tempcorpus_class
#' @name tempcorpus_class
#' @aliases tempcorpus-class
setClass(
  "tempcorpus",
  slots = c(
    cpos = "matrix",
    dir = "character",
    registry = "character",
    indexed = "character"
  ))


#' Regions of a CWB corpus.
#' 
#' A coerce-method is available to coerce a \code{partition} object to a
#' \code{regions} object.
#' 
#' @slot cpos a two-column \code{data.table} that will include a "cpos_left" and "cpos_right" column
#' @slot corpus the CWB corpus (character vector length 1)
#' @slot encoding the encoding of the CWB corpus (character vector length 1)
#' @param x object of class \code{regions}
#' @param values values to assign to a column that will be added
#' @exportClass regions
#' @rdname regions_class
#' @name regions
#' @examples
#' use("polmineR")
#' P <- partition("GERMAPARLMINI", date = "2009-11-12", speaker = "Jens Spahn")
#' R <- as.regions(P)
#' @aliases regions-class
setClass(
  "regions",
  representation = list(
    cpos = "matrix",
    corpus = "character",
    encoding = "character"
  )
)





#' @rdname features-class
#' @exportClass features_cooccurrences
setClass("features_cooccurrences", contains = c("features", "textstat"))


#' @rdname features-class
#' @exportClass features_ngrams
setClass("features_ngrams", representation(n = "integer"), contains = c("features", "textstat"))


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
setClass(
  "ngrams",
  representation(
    n = "integer"
    ),
  contains = c("count", "textstat")
)


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
         slots = c(
           query = "character"
         ),
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
         representation(
           s_attributes_fixed = "list",
           explanation = "character",
           xml = "character",
           call = "character"
         ),
         contains = "count_bundle"
)




#' @rdname cooccurrences-class
setClass("cooccurrences_reshaped", contains = "cooccurrences")


#' @name cooccurrences_bundle-class
#' @aliases cooccurrences_bundle
#' @docType class
#' @exportClass cooccurrences_bundle
#' @rdname cooccurrences-class
setClass("cooccurrences_bundle", contains = "bundle")


#' Virtual class subcorpus
#' 
#' The classes \code{regions} and \code{partition} can be used to define
#' subcorpora. Unlike the \code{regions} class, the \code{partition} class may include
#' statistical evaluations. The virtual class \code{subcorpora} is a mechanism to define
#' methods for these classes without making \code{regions} the superclass of \code{partition}.
#' 
#' @rdname subcorpus
#' @name subcorpus
#' @docType class
#' @aliases subcorpus-class
#' @exportClass subcorpus
setClassUnion(
  name = "subcorpus",
  members = c("regions", "partition")
)


#' @details The method \code{aggregate} will deflate the matrix in the slot \code{cpos},
#' i.e. it checks for each new row in the matrix whether it increments the end
#' of the previous region (by 1), and ensure that the cpos matrix defines
#' disjoined regions.
#' 
#' @param x An object of a class belonging to the virtual class \code{subcorpus}, i.e. a 
#' \code{partition} or \code{regions} object.
#' @exportMethod aggregate
#' @rdname subcorpus
#' @examples 
#' P <- new(
#'   "partition",
#'   cpos = matrix(data = c(1:10, 20:29), ncol = 2, byrow = TRUE),
#'   stat = data.table::data.table()
#' )
#' P2 <- aggregate(P)
#' P2@cpos
setMethod("aggregate", "subcorpus", function(x){
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
