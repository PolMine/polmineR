#' @include context_class.R textstat_class.R partition_class.R polmineR_package.R cooccurrences_class.R bundle_class.R
NULL

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

#' @rdname cooccurrences-class
setClass("cooccurrencesReshaped", contains = "cooccurrences")

#' Methods for manipulating cooccurrencesReshaped-class-objects
#' 
#' @param x cooccurrences for a corpus of interest
#' @param y cooccurrences for a reference corpus
#' @rdname cooccurrencesReshaped
#' @aliases cooccurrencesReshaped merge,cooccurrencesReshaped-method
#' @name cooccurrencesReshaped
NULL


#' @docType methods
#' @rdname cooccurrences-class
setMethod('summary', 'cooccurrences', function(object) {
  cat("\n** Context object: **\n")
  cat(sprintf("%-20s", "CWB-Korpus:"), object@corpus, "\n")
  cat(sprintf("%-20s", "Partition:"), object@partition, "\n")
  cat(sprintf("%-20s", "Node:"), object@query, "\n")
  cat(sprintf("%-20s", "P-Attribute:"), object@pAttribute, "\n")
  cat(sprintf("%-20s", "Node count:"), object@count, "\n")
  cat(sprintf("%-20s", "Stat table length:"), nrow(object@stat), "\n\n")
})



#' @docType methods
#' @rdname cooccurrences-class
setMethod("show", "cooccurrences", function(object) {
  object <- round(object)
  if (Sys.getenv("RSTUDIO") == "1"){
    view(object)
  } else {
    if (getOption("polmineR.browse") == TRUE){
      browse(object@stat)  
    } else {
      return(object@stat) 
    }
  }
})

#' @name cooccurrencesBundle-class
#' @aliases cooccurrencesBundle
#' @docType class
#' @exportClass cooccurrencesBundle
#' @rdname cooccurrences-class
setClass("cooccurrencesBundle", contains = "bundle")


#' @importFrom data.table copy
#' @rdname cooccurrences-class
setMethod("as.data.frame", "cooccurrencesBundle", function(x){
  dts <- lapply(
    x@objects,
    function(object) copy(object@stat)[, "a" := object@query, with = TRUE]
  )
  dt <- rbindlist(dts)
  pAttr <- unique(unlist(lapply(x@objects, function(C) C@pAttribute)))
  if (length(pAttr) > 1){
    b <- dt[[ pAttr[1] ]]
    for (i in 2:length(pAttr)) b <- paste(b, dt[[pAttr[i]]], sep = "//")
    dt[, "b":= b, with = TRUE]
    for (i in 1:length(pAttr)) dt[, eval(pAttr[i]) := NULL, with = TRUE]
  } else if (length(pAttr) == 1){
    setnames(dt, old = pAttr, new = "b")
  }
  setcolorder(dt, c("a", "b", colnames(dt)[-which(colnames(dt) %in% c("a", "b"))]))
  as.data.frame(dt)
})
