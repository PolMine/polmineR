#' @include dispersion_class.R
NULL

#' @exportClass count
setOldClass("count")

#' get counts
#' 
#' Count number of occurrences of a query in a partition, or a partitionBundle.
#' The CQP syntax can be used to formulate the query.
#' 
#' @param .Object a \code{"partition"} or \code{"partitionBundle"} object, or a character vector (length 1) providing the name of a corpus
#' @param query a character vector (one or multiple terms to be looked up)
#' @param pAttribute the p-attribute(s) to use
#' @param mc logical, whether to use multicore (defaults to FALSE)
#' @param verbose logical, whether to be verbose
#' @param freq logical, if FALSE, counts will be reported, if TRUE, frequencies
#' @param total defaults to FALSE, if TRUE, the added value of counts (column: TOTAL) will be amended to the data.table that is returned
#' @param method either 1 or 2, for working with 
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
setGeneric("count", function(.Object, ...){standardGeneric("count")})

#' @rdname count-method
setMethod("count", "partition", function(.Object, query, pAttribute=NULL, mc=F, verbose=T, progress=F){
  pAttr <- ifelse(
    is.null(pAttribute),
    slot(get("session", ".GlobalEnv"), "pAttribute"), 
    pAttribute
  )
  .getNumberOfHits <- function(query) {
    if (verbose == TRUE) message("... processing query ", query)
    cposResult <- cpos(.Object=.Object, query=query, pAttribute=pAttr, verbose=FALSE)
    ifelse(is.null(cposResult), 0, nrow(cposResult))
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
  data.table(query=query, count=no, freq=no/.Object@size)
})


#' @rdname count-method
#' @docType methods
setMethod("count", "partitionBundle", function(.Object, query, pAttribute=NULL, freq=FALSE, total=T, method=2, mc=F, progress=T, verbose=FALSE){
  if (method == 1){
    # check whether all partitions in the bundle have a proper name
    if (is.null(names(.Object@objects)) || any(is.na(names(.Object@objects)))) {
      warning("all partitions in the bundle need to have a name (at least some missing)")
    }
    countAvailable <- unique(unlist(lapply(.Object@objects, function(x) x@pAttribute)))
    bag <- blapply(.Object@objects, f=count, mc=mc, progress=progress, verbose=verbose, query=query, pAttribute=pAttribute)
    lapply(c(1:length(bag)), function(i) bag[[i]][, partition := names(.Object)[i]])
    colToGet <- ifelse(freq == TRUE, "freq", "count")
    tabRaw <- do.call(rbind, lapply(bag, function(x) x[[colToGet]]))
    colnames(tabRaw) <- query
    DT <- data.table(partition=names(.Object), data.table(tabRaw))
    if (total == TRUE) DT[, TOTAL := rowSums(.SD), by=partition]
    return(DT)
  } else if (method == 2){
    if (verbose == TRUE) message("... preparatory work")
    corpus <- unique(unlist(lapply(.Object@objects, function(x) x@corpus)))
    stopifnot(length(corpus) == 1)
    corpusEncoding <- .Object@objects[[1]]@encoding
    sAttributeStrucs <- unique(unlist(lapply(.Object@objects, function(x) x@sAttributeStrucs)))
    stopifnot(length(sAttributeStrucs) == 1)
    # combine strucs and partition names into an overall data.table
    if (verbose == TRUE) message("... preparing struc table")
    strucDT <- data.table(
      struc=unlist(lapply(.Object@objects, function(x) x@strucs)),
      partition=unlist(lapply(.Object@objects, function(x) rep(x@name, times=length(x@strucs))))
    )
    setkey(strucDT, cols="struc")
    # perform counts
    if (verbose == TRUE) message("... performing counts")
    
    if (mc == FALSE){
      if (progress == TRUE) pb <- txtProgressBar(max=length(query), style=3)
      countDTlist <- lapply(
        c(1:length(query)),
        function(i) {
          if (progress == TRUE) pb <- setTxtProgressBar(pb, i)
          queryToPerform <- query[i]
          cposMatrix <- cpos(
            corpus, query=queryToPerform, encoding=corpusEncoding,
            verbose=ifelse(progress == TRUE, FALSE, TRUE)
            )
          if (!is.null(cposMatrix)){
            dt <- data.table(cposMatrix)
            dt[, query := queryToPerform]
            return(dt)
          } else {
            return(NULL)
          }
        }
      )
    } else if (mc == TRUE){
      countDTlist <- mclapply(
        query,
        function(queryToPerform) {
          cposMatrix <- cpos(corpus, query=queryToPerform, encoding=corpusEncoding)
          if (!is.null(cposMatrix)){
            dt <- data.table()
            dt[, query := queryToPerform]
            return(dt)
          } else {
            return(NULL)
          }
        }, mc.cores=slot(get("session", ".GlobalEnv"), "cores")
      )
    }
    countDT <- rbindlist(countDTlist)
    if (verbose == TRUE) message("... matching data.tables")
    countDT[, struc := cqi_cpos2struc(paste(corpus, sAttributeStrucs, sep="."), countDT[["V1"]]) ]
    setkeyv(countDT, cols="struc")
    DT <- strucDT[countDT] # merge
    if (verbose == TRUE) message("... wrapping things up")
    DT[, dummy := 1]
    DT_cast <- dcast.data.table(DT, partition~query, value.var="dummy", fun.aggregate=length)
    DT_cast2 <- DT_cast[is.na(DT_cast[["partition"]]) == FALSE] # remove counts that are not in one of the partitions
    if (total == TRUE) DT_cast2[, TOTAL := rowSums(.SD), by=partition]
    class(DT_cast2) <- c("count", is(DT_cast2))
    return(DT_cast2)
  }
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
