#' @include dispersion_class.R
NULL

#' get counts
#' 
#' Count number of occurrences of a query (CQP syntax may be used) in a
#' partition, or a partitionBundle.
#' 
#' @seealso  For a metadata-based breakdown of counts
#' (i.e. a differentiation by s-attributes), see \code{"dispersion"}.
#' 
#' @param .Object a \code{"partition"} or \code{"partitionBundle"} object, or a character vector (length 1) providing the name of a corpus
#' @param query a character vector (one or multiple terms to be looked up), CQP syntax can be used.
#' @param pAttribute the p-attribute(s) to use
#' @param mc logical, whether to use multicore (defaults to FALSE)
#' @param verbose logical, whether to be verbose
#' @param freq logical, if FALSE, counts will be reported, if TRUE, frequencies
#' @param total defaults to FALSE, if TRUE, the added value of counts (column: TOTAL) will be amended to the data.table that is returned
#' @param progress logical, whether to show progress
#' @param ... further parameters
#' @return a \code{"data.table"}
#' @exportMethod count
#' @docType methods
#' @rdname count-method
#' @name count
#' @aliases count-method
#' @seealso count
#' @examples
#' \dontrun{
#' if (require(polmineR.sampleCorpus) && require(rcqp)){
#'   use("polmineR.sampleCorpus")
#'   debates <- partition("PLPRBTTXT", list(text_id=".*"), regex=TRUE)
#'   x <- count(debates, "Arbeit") # get frequencies for one token
#'   x <- count(debates, c("Arbeit", "Freizeit", "Zukunft")) # get frequencies for multiple tokens
#'   x <- count("PLPRBTTXT", c("Migration", "Integration"), "word")
#' 
#'   debates <- partitionBundle(
#'     .Object="PLPRBTTXT",
#'     def=list(text_date=sAttributes("PLPRBTTXT", "text_date")),
#'     regex=TRUE, mc=FALSE, verbose=FALSE
#'   )
#'   aiu <- count(debates, c("Arbeit", "Integration", "Umwelt"))
#' }
#' }
setGeneric("count", function(.Object, ...){standardGeneric("count")})

#' @rdname count-method
setMethod("count", "partition", function(.Object, query, pAttribute=getOption("polmineR.pAttribute"), mc=F, verbose=T, progress=F){
  .getNumberOfHits <- function(query) {
    if (verbose == TRUE) message("... processing query ", query)
    cposResult <- cpos(.Object=.Object, query=query, pAttribute=pAttribute, verbose=FALSE)
    ifelse(is.null(cposResult), 0, nrow(cposResult))
  }
  if (mc == FALSE){
    no <- vapply(query, .getNumberOfHits, FUN.VALUE=1)
  } else if (mc == TRUE){
    no <- unlist(mclapply(
      query,
      .getNumberOfHits, 
      mc.cores=getOption("polmineR.cores")
    ))
  }
  data.table(query=query, count=no, freq=no/.Object@size)
})


#' @rdname count-method
#' @docType methods
setMethod("count", "partitionBundle", function(.Object, query, pAttribute=NULL, freq=FALSE, total=T, mc=F, progress=T, verbose=FALSE){
  if (verbose == TRUE) message("... preparatory work")
  DT <- hits(.Object, query=query, pAttribute=pAttribute, mc=mc, progress=progress, verbose=verbose)@dt
  if (verbose == TRUE) message("... wrapping things up")
  DT_cast <- dcast.data.table(DT, partition~query, value.var="count")
  DT_cast2 <- DT_cast[is.na(DT_cast[["partition"]]) == FALSE] # remove counts that are not in one of the partitions
  for (q in query){
    DT_cast2[, eval(q) := sapply(DT_cast2[[q]], function(x) ifelse(is.na(x), 0, x)), with=FALSE]
  }
  if (total == TRUE) DT_cast2[, "TOTAL" := rowSums(.SD), by=partition, with=TRUE]
  DT_cast2
})

#' @rdname count-method
setMethod("count", "character", function(.Object, query, pAttribute=getOption("polmineR.pAttribute"), verbose=TRUE){
  stopifnot(.Object %in% CQI$list_corpora())
  # pAttr <- paste(.Object, ".", pAttribute, sep="")
  total <- CQI$attribute_size(.Object, pAttribute)
  count <- sapply(
    query,
    function(query)
      CQI$id2freq(
        .Object,
        pAttribute,
        CQI$str2id(.Object, pAttribute, query)
        )
    )
  freq <- count/total
  data.table(query=query, count=count, freq=freq)
})

#' @rdname context-class
setMethod("count", "context", function(.Object) {
  .Object@count
})

#' @rdname dispersion-class
setMethod("count", "dispersion", function(.Object) .Object@count)
