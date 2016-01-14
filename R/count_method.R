#' @include dispersion_class.R
NULL

#' get counts
#' 
#' Count number of occurrences of a query (CQP syntax can be used).
#' 
#' @param .Object either a partition or a partitionBundle object
#' @param query a character vector (one or multiple terms to be looked up)
#' @param pAttribute the p-attribute(s) to use
#' @param mc logical, whether to use multicore (defaults to FALSE)
#' @param verbose logical, whether to be verbose
#' @param ... further parameters
#' @exportMethod count
#' @docType methods
#' @rdname count-method
#' @name count
#' @aliases count-method
#' @seealso count
#' @examples
#' \dontrun{
#' # generate a partition for testing 
#' use("polmineR.sampleCorpus")
#' test <- partition("PLPRBTTXT", list(text_id=".*"), regex=TRUE)
#' count(test, "Wir") # get frequencies for one token
#' count(test, c("Wir", "lassen", "uns")) # get frequencies for multiple tokens
#' count("PLPRBTTXT", c("machen", "Integration"), "word")
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
setMethod("count", "partitionBundle", function(.Object, query, pAttribute=NULL, mc=F, verbose=T){
  # check whether all partitions in the bundle have a proper name
  if (is.null(names(.Object@objects)) || any(is.na(names(.Object@objects)))) {
    warning("all partitions in the bundle need to have a name (at least some missing)")
  }
  what <- paste(pAttribute, ifelse(freq==FALSE, "count", "freq"), sep="")
  countAvailable <- unique(unlist(lapply(.Object@objects, function(x) x@pAttribute)))
  bag <- lapply(
    names(.Object@objects),
    function(x) {
      data.table(partition=x, query=query, count(.Object@objects[[x]], query))
    }
  )
  tab <- do.call(rbind, bag)
  if(!is.null(tab)){
    tab <- data.table(tab[,c("partition", "query", what)])
    colnames(tab) <- c("partition", "query", "count")
    tab <- xtabs(count~partition+query, data=tab)
    tab <- as.data.frame(as.matrix(unclass(tab)))
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
