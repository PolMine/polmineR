#' @include dispersion_class.R
NULL

#' get counts
#' 
#' Count number of occurrences of a query. The CQP syntax can be used to formulate 
#' the query.
#' 
#' @param .Object a \code{"partition"} or \code{"partitionBundle"} object, or a character vector (length 1) providing the name of a corpus
#' @param query a character vector (one or multiple terms to be looked up)
#' @param pAttribute the p-attribute(s) to use
#' @param mc logical, whether to use multicore (defaults to FALSE)
#' @param verbose logical, whether to be verbose
#' @param freq logical, if FALSE, counts will be reported, if TRUE, frequencies
#' @param ... further parameters
#' @return a \code{"data.table"}
#' @exportMethod count
#' @docType methods
#' @rdname count-method
#' @name count
#' @aliases count-method
#' @seealso count
#' @examples
#' use("polmineR.sampleCorpus")
#' debates <- partition("PLPRBTTXT", list(text_id=".*"), regex=TRUE)
#' count(debates, "Arbeit") # get frequencies for one token
#' count(debates, c("Arbeit", "Freizeit", "Zukunft")) # get frequencies for multiple tokens
#' count("PLPRBTTXT", c("Migration", "Integration"), "word")
#' 
#' debates <- partitionBundle(
#'   object="PLPRBTTXT",
#'   def=list(text_id=".*"),
#'   var=list(text_date=sAttributes("PLPRBTTXT", "text_date")),
#'   regex=TRUE, mc=FALSE, verbose=FALSE
#' )
#' count(debates, c("Arbeit", "Integration", "Umwelt"))
#' }
setGeneric("count", function(.Object, ...){standardGeneric("count")})

#' @rdname count-method
setMethod("count", "partition", function(.Object, query, pAttribute=NULL, mc=F, verbose=T){
  pAttr <- ifelse(
    is.null(pAttribute),
    slot(get("session", ".GlobalEnv"), "pAttribute"), 
    pAttribute
  )
  .getNumberOfHits <- function(query) {
    if (verbose == TRUE) message("... processing query ", query)
    cposResult <- cpos(.Object=.Object, query=query, pAttribute=pAttr, verbose=FALSE)
    if (is.null(cposResult)){
      retval <- 0
    } else {
      retval <- nrow(cposResult)
    }
    retval
  }
  if (mc == FALSE){
    no <- vapply(query, .getNumberOfHits, FUN.VALUE=1)
  } else if (mc == TRUE){
    no <- unlist(mclapply(
      query,
      .getNumberOfHits, 
      mc.cores=slot(get("session", ".GlobalEnv"), "cores")
    ))
  }
  DT <- data.table(query=query, count=no, freq=no/.Object@size)
  return(DT)
  DT
})


#' @rdname count-method
#' @docType methods
setMethod("count", "partitionBundle", function(.Object, query, pAttribute=NULL, freq=FALSE, mc=F, verbose=T){
  # check whether all partitions in the bundle have a proper name
  if (is.null(names(.Object@objects)) || any(is.na(names(.Object@objects)))) {
    warning("all partitions in the bundle need to have a name (at least some missing)")
  }
  countAvailable <- unique(unlist(lapply(.Object@objects, function(x) x@pAttribute)))
  bag <- lapply(
    names(.Object@objects),
    function(x) {
      data.table(partition=x, query=query, count(.Object@objects[[x]], query))
    }
  )
  tabRaw <- do.call(rbind, bag)
  if(!is.null(tabRaw)){
    tab <- tabRaw[,c("partition", "query", ifelse(freq==FALSE, "count", "freq")), with=FALSE]
    if (freq == FALSE){
      tab <- xtabs(count~partition+query, data=tab)  
    } else {
      tab <- xtabs(freq~partition+query, data=tab)  
    }
    tab <- data.table(partition=names(.Object), as.data.table(as.matrix(unclass(tab))))
  }
  tab
})

#' @rdname count-method
setMethod("count", "character", function(.Object, query, pAttribute=NULL, verbose=TRUE){
  stopifnot(.Object %in% cqi_list_corpora())
  if (is.null(pAttribute)) {
    pAttribute <- slot(get("session", '.GlobalEnv'), 'pAttribute')
    if (verbose == TRUE) message("... using pAttribute ", pAttribute, " from session settings")
  }  
  pAttr <- paste(.Object, ".", pAttribute, sep="")
  total <- cqi_attribute_size(pAttr)
  count <- sapply(query, function(query) cqi_id2freq(pAttr, cqi_str2id(pAttr, query)))
  freq <- count/total
  data.table(query=query, count=count, freq=freq)
})

#' @rdname context-class
setMethod("count", "context", function(.Object) {
  .Object@count
})

#' @rdname dispersion-class
setMethod("count", "dispersion", function(.Object) .Object@count)
