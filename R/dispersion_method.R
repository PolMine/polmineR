#' @include partition_class.R hits_class.R
NULL

setGeneric("dispersion", function(.Object, ...){standardGeneric("dispersion")})


#' Dispersion of a query or multiple queries
#' 
#' The function returns the frequencies of a query or a multiple queries
#' in sub-partitions defined by one or two dimensions. This is a wrapper function, so the output will depend
#' on the number of queries and dimensions provided.
#' 
#' @param .Object a partition object
#' @param query a character vector containing one or multiple queries
#' @param sAttribute a character vector of length 1 or 2 providing the sAttributes 
#' @param pAttribute the p-attribute that will be looked up, typically 'word'
#' or 'lemma'
#' @param freq logical, whether to calculate normalized frequencies
#' @param mc logical, whether to use multicore
#' @param verbose logical, whether to be verbose
#' @return depends on the input, as this is a wrapper function
#' @seealso \code{crosstab-class}
#' @exportMethod dispersion
#' @examples
#' test <- partition("PLPRBTTXT", def=list(text_year="2009"), pAttribute=NULL)
#' dispersion(test, query="Integration", pAttribute="word", dim=c("text_date"))
#' foo <- dispersion(test, "Integration", c("text_date", "text_party"))
#' dispersion(test, '"Integration.*"', c("text_year")) # note the brackets when using regex!
#' @seealso count
#' @author Andreas Blaette
#' @docType methods
#' @exportMethod dispersion
#' @rdname dispersion-method
#' @name dispersion
setMethod("dispersion", "partition", function(.Object, query, sAttribute, pAttribute=getOption("polmineR.pAttribute"), freq=TRUE, mc=FALSE, progress=TRUE, verbose=FALSE){
  dispersion(
    hits(
      .Object=.Object, query=query, sAttribute=sAttribute, pAttribute=pAttribute, freq=freq,
      mc=mc, verbose=verbose, progress=progress
      ),
    sAttribute=sAttribute, freq=freq
  )
})

setMethod("dispersion", "character", function(.Object, query, sAttribute, pAttribute=getOption("polmineR.pAttribute"), freq=FALSE, mc=FALSE, progress=TRUE, verbose=TRUE){
  dispersion(
    hits(
      .Object, query=query, sAttribute=sAttribute, pAttribute=pAttribute, freq=freq,
      mc=mc, verbose=verbose, progress=progress
    ),
    sAttribute=sAttribute, freq=freq
  )
})


setMethod("dispersion", "hits", function(.Object, sAttribute, freq=FALSE){
  if (length(sAttribute) == 2){
    retval <- dcast.data.table(
      .Object@dt, formula(paste(sAttribute, collapse="~")),
      value.var=ifelse(freq == TRUE, "freq", "count"), fun.aggregate=sum, fill=0
      )  
  } else if (length(sAttribute) == 1){
    retval <- .Object@dt[, query := NULL]
  } else {
    warning("length(sAttribute) needs to be 1 or 2")
  }
  retval
})
