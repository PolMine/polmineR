#' @include tempcorpus.R
NULL

#' get corpus positions
#' 
#' If the cpos-method isapplied on \code{"character"}, \code{"partition"},
#' or \code{"tempcorpus"} object, the result is a two-column matrix with the
#' start end end corpus positions of the matches for a query (CQP syntax can
#' be used.) If the cpos-method is called on a \code{"matrix"} object,  the cpos
#' matrix is unfolded. 
#' 
#' @param .Object a \code{"character"} vector indicating a CWB corpus, a \code{"partition"}
#' object, a \code{"tempcorpus"} object, or a \code{"matrix"} with corpus positions
#' @param query a character vector (length 1) providing a (single) query: either a single token to look up, or a CQP query. 
#' @param cqp either logical (TRUE if query is a CQP query), or a
#'   function to check whether query is a CQP query or not (defaults to is.query
#'   auxiliary function)
#' @param pAttribute the p-attribute to search. Needs to be stated only if query is not a CQP query. Defaults to NULL.
#' @param encoding the encoding of the corpus (if NULL, the
#'  encoding provided in the registry file of the corpus will be used)
#' @param verbose logical, whether to be talkative
#' @param ... further arguments
#' @return Unless .Object is a \code{"matrix"}, you get a matrix with two columns, the first column giving the start cpos of the hits obtained,
#' the second column giving the end cpos of the respective hit. The number of rows is the number of hits.
#' If there are no hits, a NULL object will be returned.
#' @exportMethod cpos
#' @rdname cpos-method
#' @name cpos
setGeneric("cpos", function(.Object, ... ) standardGeneric("cpos"))

#' @rdname cpos-method
setMethod("cpos", "character", function(.Object, query, pAttribute=getOption("polmineR.pAttribute"), cqp=is.cqp, encoding=NULL, verbose=TRUE, ...){
  if (length(query) > 1) warning("query needs to be a character vector with length 1")
  if (is.null(encoding)) encoding <- getEncoding(.Object) 
  query <- adjustEncoding(query, encoding)
  if (class(cqp) == "function") cqp <- cqp(query)
  if (cqp == FALSE) {
    cpos <- try({
      id <- CQI$str2id(.Object, pAttribute, query)
      CQI$id2cpos(.Object, pAttribute, id)
    })
    if (is(cpos)[1] != "try-error"){
      hits <- matrix(c(cpos, cpos), ncol=2)
    } else {
      if (verbose == TRUE) message("no hits for query: ", query)
      hits = NULL
    }
  } else if (cqp == TRUE) {
    CQI$query(.Object, query)
    cpos <- try(CQI$dump_subcorpus(.Object))
    if (is(cpos)[1] == "try-error"){
      if (verbose == TRUE) message("no hits for query: ", query)
      hits <- NULL
    } else if (!is.null(cpos)) {
      hits <- matrix(cpos[,1:2], ncol=2)
    } else {
      if (verbose == TRUE) message("no hits for query: ", query)
      hits <- NULL
    }
  }
  hits
})
  
#' @rdname cpos-method
setMethod("cpos", "partition", function(.Object, query, cqp=is.cqp, pAttribute=NULL, verbose=TRUE, ...){
  hits <- cpos(.Object@corpus, query=query, cqp=cqp, pAttribute, encoding=.Object@encoding, verbose=verbose)
  if (!is.null(hits) && length(.Object@sAttributes) > 0){
    sAttribute <- names(.Object@sAttributes)[length(.Object@sAttributes)]
    # corpus.sAttribute <- paste(.Object@corpus, ".", sAttribute, sep="")
    strucHits <- CQI$cpos2struc(.Object@corpus, sAttribute, hits[,1])
    hits <- hits[which(strucHits %in% .Object@strucs),]
    if (is(hits)[1] == "integer") hits <- matrix(data=hits, ncol=2)
    if (nrow(hits) == 0) hits <- NULL
  }
  if (is(hits)[1] == "integer") hits <- matrix(hits, ncol=2)
  return(hits)
})

#' @param shift logical, if true, the cpos resulting from the query performed on
#'   the tempcorpus will be shifted so that they match the positions of the
#'   corpus from which the tempcorpus was generated
#' @rdname cpos-method
setMethod("cpos", "tempcorpus", function(.Object, query, shift=TRUE){
  cqpBatchCmds <- paste(paste(c(
    "TMPCORPUS",
    paste("FOO = ", query, sep=""),
    "dump FOO"
  ), collapse=";"), ";", sep="")
  cqpBatchFile=file.path(.Object@dir, "cqpBatchFile.txt")
  cat(cqpBatchCmds, file=cqpBatchFile)
  cqpCmd <- paste(c(
    "cqp", "-r", .Object@registry, "-f", cqpBatchFile
  ), collapse=" ")
  cpos <- system(cqpCmd, intern=TRUE)
  cposMatrix <- do.call(rbind, lapply(strsplit(cpos, "\t"), as.integer))
  cposMatrix <- cposMatrix[,c(1:2)]
  if (shift == TRUE) cposMatrix <- apply(cposMatrix, 2, function(x) x + .Object@cpos[1,1])
  cposMatrix
})

#' @rdname cpos-method
setMethod("cpos", "matrix", function(.Object){
  as.vector(unlist(apply(.Object, 1, function(row) c(row[1]:row[2]))))  
})
