#' @include textstat_class.R comp_class.R
NULL

#' S4 context class
#' 
#' class to organize information of context analysis
#' 
#' @slot query Object of class \code{"character"} node examined
#' @slot count Object of class \code{"numeric"} number of hits
#' @slot partition Object of class \code{"character"} the partition the analysis is based on
#' @slot partitionSize Object of class \code{"numeric"} the size of the partition
#' @slot leftContext Object of class \code{"numeric"} number of tokens to the right
#' @slot rightContext Object of class \code{"numeric"} number of tokens to the left
#' @slot size Object of class \code{"numeric"} number of tokens in the right and left context
#' @slot pAttribute Object of class \code{"character"} p-attribute of the query
#' @slot sAttribute Object of ...
#' @slot corpus Object of class \code{"character"} the CWB corpus used
#' @slot stat Object of class \code{"data.table"} statistics of the analysis
#' @slot encoding Object of class \code{"character"} encoding of the corpus
#' @slot cpos Object of class \code{"list"} corpus positions of the hits
#' @slot method Object of class \code{"character"} statistical test used
#' @slot call Object of class \code{"character"} call that generated the object
#'     
#' @param x object
#' @param object object
#' @param mc logical, whether to use multicore
#' @param i for indexing
#' @param n for head and tail
#' @param sAttribute the s-attribute to use
#' @aliases show,context-method [,context-method [,context,ANY,ANY,ANY-method
#'   [[,context-method summary,context-method head,context-method ll,context-method
#'   as.DataTables,context-method
#' @docType class
#' @exportClass context
setClass("context",
         representation(query="character",
                        count="numeric",
                        partition="character",
                        partitionSize="numeric",
                        leftContext="numeric",
                        rightContext="numeric",
                        size="numeric",
                        sAttribute="character",
                        cpos="list",
                        call="character"
         ),
         contains=c("comp", "textstat")
)
