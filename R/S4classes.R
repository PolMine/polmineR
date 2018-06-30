
#' Bundle Class
#' 
#' A class to bundle several objects (partition, context, comp, cooccurrences objects)
#' in one S4 object.
#' 
#' @slot corpus the CWB corpus the objects in the bundle are based on
#' @slot objects Object of class \code{"list"}
#' @slot pAttribute Object of class \code{"character"}
#' @slot encoding encoding of objects
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
#' party_bundle <- partitionBundle("GERMAPARLMINI", sAttribute = "party")
#' length(party_bundle)
#' names(party_bundle)
#' party_bundle <- enrich(party_bundle, pAttribute = "word")
#' summary(party_bundle)
#' parties_big <- party_bundle[[c("CDU_CSU", "SPD")]]
#' summary(parties_big)
setClass(
  "bundle",
  representation(
    objects = "list",
    pAttribute = "character",
    corpus = "character",
    encoding = "character"
  )
)





#' S4 textstat class
#' 
#' Superclass for features, context, and partition class.
#' 
#' Objects derived from the \code{textstat} class can be indexed with simple
#' square brackets ("[") to get rows specified by an numeric/integer vector,
#' and with double square brackets ("[[") to get specific columns from the 
#' \code{data.table} in the slot \code{stat}.
#' 
#' @slot pAttribute Object of class \code{"character"} p-attribute of the query
#' @slot corpus Object of class \code{"character"}
#' @slot stat Object of class \code{"data.table"} statistics of the analysis
#' @slot name name of the object
#' @slot encoding Object of class \code{"character"} encoding of the corpus
#' @param .Object an object
#' @param by by
#' @param decreasing logical
#' @param e1 object 1
#' @param e2 object 2
#' @param i vector to index data.table in stat-slot
#' @param j vector to index data.table in stat-slot
#' @param ... further parameters
#' @aliases as.data.frame,textstat-method show,textstat-method
#'   dim,textstat-method
#'   colnames,textstat-method rownames,textstat-method names,textstat-method
#'   as.DataTables,textstat-method
#' @docType class
#' @exportClass textstat
#' @examples
#' use("polmineR")
#' P <- partition("GERMAPARLMINI", date = ".*", pAttribute = "word", regex = TRUE)
#' y <- cooccurrences(P, query = "Arbeit")
#' y[1:25]
#' y[,c("word", "ll")]
#' y[1:25, "word"]
#' y[1:25][["word"]]
#' y[which(y[["word"]] %in% c("Arbeit", "Sozial"))]
#' y[ y[["word"]] %in% c("Arbeit", "Sozial") ]
setClass("textstat",
         representation(
           corpus = "character",
           pAttribute = "character",
           encoding = "character",
           stat = "data.table",
           name = "character"
         )
)

#' Feature selection by comparison (S4 class).
#' 
#' object resulting from features-method
#' 
#' @slot corpus Object of class \code{"character"} 
#' @slot pAttribute Object of class \code{"character"} 
#' @slot encoding Object of class \code{"character"}  
#' @slot corpus Object of class \code{"character"}  
#' @slot stat Object of class \code{"data.frame"} 
#' @slot sizeCoi Object of class \code{"numeric"} 
#' @slot sizeRef Object of class \code{"numeric"} 
#' @slot included Object of class \code{"logical"} whether corpus of interest is included in reference corpus
#' @slot method Object of class \code{"character"} statisticalTest used
#' @slot call Object of class \code{"character"} the call that generated the object
#' 
#' @param .Object an object
#' @param object an object
#' @rdname features-class
#' @name features-class
#' @docType class
#' @exportClass features
#' @author Andreas Blaette
setClass("features",
         representation(
           corpus = "character",
           pAttribute = "character",
           encoding = "character",
           stat = "data.table",
           sizeCoi = "numeric",
           sizeRef = "numeric",
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
#' @name count_class
#' @exportClass count
#' @docType class
#' @author Andreas Blaette
#' @aliases count-class
#' @seealso The \code{count}-class inherits from the \code{\link{textstat-class}}
setClass("count", representation = list(size = "integer"), contains = "textstat")






#' Partition class and methods.
#' 
#' S4 partition class and methods for instances of class partition.
#' 
#' @slot name Object of class \code{"character"} a name that may be useful 
#' @slot corpus Object of class \code{"character"} the CWB corpus the partition is based on 
#' @slot encoding Object of class \code{"character"} encoding of the corpus 
#' @slot sAttributes Object of class \code{"list"} s-attributes specifying the partition 
#' @slot explanation Object of class \code{"character"} an explanation of the partition 
#' @slot cpos Object of class \code{"matrix"} corpus positions
#' @slot annotations Object of class \code{"list"}
#' @slot pos Object of class \code{"list"} with tables "abs", "rel" and "max"
#' @slot size Object of class \code{"numeric"} total size of the partition 
#' @slot metadata Object of class \code{"data.frame"} metadata information 
#' @slot strucs Object of class \code{"numeric"} the strucs defining the partition 
#' @slot pAttribute Object of class \code{"character"} indicating the pAttribute of the
#' count in slot tf
#' @slot xml Object of class \code{"character"} whether the xml is flat or nested 
#' @slot sAttributeStrucs Object of class \code{"character"} the base node 
#' @slot call Object of class \code{"character"} the call that generated the partition 
#' @param .Object a partition object
#' @param p_attribute a p-attribute (for enriching)
#' @param x a partition object
#' @param verbose logical
#' @param cpos ...
#' @param meta ...
#' @param cutoff maximum number of tokens to decode
#' @param ... further parameters
#' @param value value
#' @param template template to use
#' @aliases partition-class show,partition-method [,partition,ANY,ANY,ANY-method 
#'   [,partition-method as.partitionBundle 
#'   as.partitionBundle,partition-method export export,partition-method split
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
    sAttributes = "list",
    explanation = "character",
    cpos = "matrix",
    pos = "list",
    annotations = "list",
    size = "integer",
    metadata = "data.frame",
    strucs = "numeric",
    xml = "character",
    sAttributeStrucs = "character",
    call = "character"
  ),
  contains = c("count", "textstat")
)






#' Context class.
#' 
#' Class to organize information of context analysis.
#' 
#' @details Objects of the class \code{context} include a \code{data.table} in the
#' slot \code{cpos}. The \code{data.table} will at least include the columns "hit_no",
#' "cpos" and "position".
#' 
#' @slot query Object of class \code{"character"}, the query/node examined
#' @slot count Object of class \code{"numeric"} number of hits
#' @slot partition Object of class \code{"partition"}, the partition the context object is based on
#' @slot partitionSize Object of class \code{"numeric"} the size of the partition
#' @slot left Object of class \code{"numeric"} number of tokens to the left
#' @slot right Object of class \code{"numeric"} number of tokens to the right
#' @slot size Object of class \code{"numeric"} number of tokens in the right and left context
#' @slot sAttribute Object of class \code{"character"} s-attribute
#' @slot pAttribute Object of class \code{"character"} p-attribute of the query
#' @slot corpus Object of class \code{"character"} the CWB corpus used
#' @slot stat Object of class \code{"data.table"} statistics of the analysis
#' @slot encoding Object of class \code{"character"} encoding of the corpus
#' @slot cpos Object of class \code{"list"} corpus positions of the hits
#' @slot method Object of class \code{"character"} statistical test used
#' @slot call Object of class \code{"character"} call that generated the object
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
           count = "numeric",
           partition = "partition",
           partitionSize = "numeric",
           left = "numeric",
           right = "numeric",
           size = "numeric",
           sAttribute = "character",
           cpos = "data.table",
           call = "character"
         ),
         contains = c("features", "textstat")
)




#' Cooccurrences class.
#' 
#' S4 class to organize information of context analysis
#' 
#' @param .Object object to work with
#' @param object object to work with
#' @param x object to work with
#' @slot call Object of class \code{"character"} the call that generated the object
#' @slot partition Object of class \code{"character"} the partition the analysis is based on
#' @slot partitionSize  Object of class \code{"numeric"} the size of the partition
#' @slot left  Object of class \code{"numeric"} number of tokens to the right
#' @slot right  Object of class \code{"numeric"} number of tokens to the left
#' @slot pAttribute  Object of class \code{"character"} p-attribute of the query
#' @slot corpus  Object of class \code{"character"} the CWB corpus used
#' @slot stat  Object of class \code{"data.frame"} statistics of the analysis
#' @slot encoding  Object of class \code{"character"} encoding of the corpus
#' @slot pos  Object of class \code{"character"} part-of-speech tags filtered
#' @slot method  Object of class \code{"character"} statistical test(s) used
#' @slot cutoff  Object of class \code{"list"} cutoff levels that have been applied
#' @slot svg Object of class \code{"character"} - valid XML with svg representation
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
#' S4 class for organizing information for concordance output
#' 
#' @details The \code{enrich} method is used to generate the actual output for
#' the kwic method. If param \code{table} is \code{TRUE}, corpus positions will
#' be turned into a data.frame with the concordance lines. If param \code{meta}
#' is a character vector with s-attributes, the respective s-attributes will be
#' added as columns to the table with concordance lines.
#' 
#' @slot metadata Object of class \code{"character"} keeping the sAttributes of the metadata that are to be displayed
#' @slot left words to the left
#' @slot right words to the right
#' @slot corpus the CWB corpus
#' @slot cpos the corpus positions
#' @slot table Object of class \code{"data.frame"} a table with the relevant information for kwic output
#' @slot encoding Object of class \code{"character"} encoding of the corpus
#' @slot labels Object of class \code{"character"}
#' @slot categories Object of class \code{"character"}
#' 
#' @param x a kwic-class object
#' @param object an object of class \code{kwic}
#' @param meta sAttributes (character vector) with metainformation
#' @param table logical, whether to turn cpos data.table into data.frame for output
#' @param size integer, the subset size for sampling
#' @section Methods:
#'   \describe{
#'    \item{[}{indexing for seeing only some concordances}
#'    \item{show}{get kwic output}
#'   }
#'   
#' @name kwic-class
#' @docType class
#' @aliases kwic-class [,kwic,ANY,ANY,ANY-method [,kwic-method
#' @exportClass kwic
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
    left = "numeric",
    right = "numeric",
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
#' @rdname tempcorpus
setClass(
  "tempcorpus",
  slots=c(
    cpos="matrix",
    dir="character",
    registry="character",
    indexed="character"
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
#' \donttest{
#' use("polmineR")
#' P <- partition("GERMAPARLMINI", date = "2009-11-12", speaker = "Jens Spahn")
#' R <- as.regions(P)
#' encode(R, sAttribute = "foo", values = "Jens")
#' }
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
#' @exportClass featuresCooccurrences
setClass("featuresCooccurrences", contains=c("features", "textstat"))


#' @rdname features-class
#' @exportClass featuresNgrams
setClass("featuresNgrams", representation(n="integer"), contains=c("features", "textstat"))


#' @slot objects an object of class \code{list}
#' @rdname features-class
setClass("featuresBundle", slots = c(objects = "list"), contains = "bundle")




#' @exportClass ngrams
#' @rdname ngrams
setClass(
  "ngrams",
  representation(n = "integer"),
  contains = c("count", "textstat")
)


#' Get Hits.
#' 
#' Get hits for a (set of) queries, optionally with s-attribute values.
#' 
#' If the query character vector is named, the names of the query occurr in
#' the data.table that is returned rather than the queries.
#' 
#' If freq is TRUE, the data.table returned in the DT-slot will deliberately
#' include the subsets of the partition/corpus with no hits (query is NA,
#' count is 0).
#' 
#' @slot stat a \code{"data.table"}
#' @slot corpus a \code{"character"} vector
#' @slot query Object of class \code{"character"}
#' @slot pAttribute p-attribute that has been queried
#' @slot encoding encoding of the corpus
#' @slot name name of the object
#' @param query a (optionally named, see datails) character vector with one or more queries
#' @param cqp either logical (TRUE if query is a CQP query), or a
#'   function to check whether query is a CQP query or not
#' @param s_attribute s-attributes
#' @param p_attribute p-attribute
#' @param size logical - return size of subcorpus
#' @param freq logcial - return relative frequencies
#' @param x a hits object
#' @param .Object a character, partition or partitionBundle object
#' @param mc logical, whether to use multicore
#' @param progress logical, whether to show progress bar
#' @param verbose logical
#' @param ... further parameters
#' @exportClass hits
#' @rdname hits
setClass(
  "hits",
  representation(query = "character"),
  contains = "textstat"
)


#' S4 contextBundle class
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
#' @name contextBundle-class
#' @aliases show,contextBundle-method summary,contextBundle-method [,contextBundle-method [,contextBundle,ANY,ANY,ANY-method [[,contextBundle-method as.TermContextBundle,contextBundle-method
#' @docType class
#' @exportClass kwic
#' @rdname contextBundle-class
setClass("contextBundle",
         representation(
           objects = "list",
           query = "character",
           pAttribute = "character"
         ),
         contains = "bundle"
)


#' @rdname partition_class
setClass("plprPartition", contains = "partition")

#' @rdname partition_class
setClass("pressPartition", contains = "partition")


#' Bundle of partitions (partitionBundle class).
#' 
#' Class and methods to manage bundles of partitions. 
#' 
#' @slot objects Object of class \code{"list"} the partitions making up the bundle
#' @slot corpus Object of class \code{"character"} the CWB corpus the partition is based on
#' @slot sAttributesFixed Object of class \code{"list"} fixed sAttributes
#' @slot encoding Object of class \code{"character"} encoding of the corpus
#' @slot explanation Object of class \code{"character"} an explanation of the partition
#' @slot xml Object of class \code{"character"} whether the xml is flat or nested
#' @slot call Object of class \code{"character"} the call that generated the partitionBundle
#' @aliases partitionBundle-class
#'   [,partitionBundle-method [[,partitionBundle-method
#'   as.matrix,partitionBundle-method 
#'   merge,partitionBundle-method
#'   +,partitionBundle-method names,partitionBundle-method 
#'   summary,partitionBundle-method +,partitionBundle,ANY-method
#'   [,partitionBundle,ANY,ANY,ANY-method +,partitionBundle,partition-method 
#'   +,partitionBundle,partitionBundle-method as.partitionBundle,list-method 
#'   barplot,partitionBundle-method
#' @param x a partitionBundle object
#' @param .Object a partitionBundle object
#' @param object a partitionBundle object
#' @param i integer index
#' @param sAttribute the s-attribute to use
#' @param height height
#' @param ... further parameters
#' @rdname partitionBundle-class
#' @name partitionBundle-class
#' @exportClass partitionBundle
#' @author Andreas Blaette
setClass("partitionBundle",
         representation(
           sAttributesFixed = "list",
           explanation = "character",
           xml = "character",
           call = "character"
         ),
         contains = "bundle"
)




#' @rdname cooccurrences-class
setClass("cooccurrencesReshaped", contains = "cooccurrences")


#' @name cooccurrencesBundle-class
#' @aliases cooccurrencesBundle
#' @docType class
#' @exportClass cooccurrencesBundle
#' @rdname cooccurrences-class
setClass("cooccurrencesBundle", contains = "bundle")
