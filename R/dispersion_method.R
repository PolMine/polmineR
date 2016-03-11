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
#' @param dim a character vector of length 1 or 2 providing the sAttributes 
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
setMethod("dispersion", "partition", function(.Object, query, dim, pAttribute=NULL, freq=TRUE, mc=FALSE, progress=TRUE, verbose=FALSE){
  if ( is.null(pAttribute) ) pAttribute <- slot(get("session", ".GlobalEnv"), "pAttribute")
  dispersion(
    hits(
      .Object=.Object, query=query, sAttribute=dim, pAttribute=pAttribute, freq=freq,
      mc=mc, verbose=verbose, progress=progress
      ),
    dim=dim, freq=freq
  )
})

setMethod("dispersion", "character", function(.Object, query, dim, pAttribute=NULL, freq=FALSE, mc=FALSE, progress=TRUE, verbose=TRUE){
  if ( is.null(pAttribute) ) pAttribute <- slot(get("session", ".GlobalEnv"), "pAttribute")
  dispersion(
    hits(
      .Object, query=query, sAttribute=dim, pAttribute=pAttribute, freq=freq,
      mc=mc, verbose=verbose, progress=progress
    ),
    dim=dim, freq=freq
  )
})


setMethod("dispersion", "hits", function(.Object, dim, freq=FALSE){
  if (length(dim) == 2){
    dcast.data.table(
      .Object@dt, formula(paste(dim, collapse="~")),
      value.var=ifelse(freq == TRUE, "freq", "count"), fun.aggregate=sum, fill=0
      )  
  }
})
