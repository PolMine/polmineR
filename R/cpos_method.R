#' @include tempcorpus.R hits_class.R
NULL

#' Get corpus positions for a query or queries.
#' 
#' Get matches for a query in a CQP corpus, optionally using the CQP syntax of the
#' Corpus Workbench (CWB).
#' 
#' If the cpos-method is applied on \code{"character"}, \code{"partition"}, or
#' \code{"tempcorpus"} object, the result is a two-column matrix with the 
#' regions (start end end corpus positions of the matches) for a query. CQP
#' syntax can be used. The encoding of the query is adjusted to conform to the
#' encoding of the CWB corpus.
#' 
#' If the cpos-method is called on a \code{matrix} object,  the cpos
#' matrix is unfolded, the return value is an integer vector with the individual
#' corpus positions. Equally, if \code{.Object} is a \code{hits} object,
#' an integer vector is returned with the individual corpus positions.
#' 
#' @param .Object a \code{"character"} vector indicating a CWB corpus, a
#'   \code{"partition"} object, a \code{"tempcorpus"} object, or a
#'   \code{"matrix"} with corpus positions
#' @param query a character vector providing one or multiple queries (token or CQP query)
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
#' @importFrom data.table fread
setGeneric("cpos", function(.Object, ... ) standardGeneric("cpos"))

#' @rdname cpos-method
setMethod("cpos", "character", function(.Object, query, pAttribute = getOption("polmineR.pAttribute"), cqp = is.cqp, encoding = NULL, verbose = TRUE, ...){
  if (is.null(encoding)) encoding <- getEncoding(.Object) # get encoding of the corpus
  query <- as.corpusEnc(query, corpusEnc = encoding)
  if (class(cqp) == "function") cqp <- cqp(query)
  if (length(cqp) > 1) stop("length of cqp is larger than 1, but needs to be 1")
  if (cqp == FALSE) {
    if (any(grepl("[\\|]", query))) warning("Special character that may cause problems in query!")
    hitList <- lapply(
      query,
      function(Q){
        cpos <- try({
          id <- CQI$str2id(.Object, pAttribute, Q)
          if (id == -1){ # CQP will return -1 if there are no matches
            .message("no hits for query: ", Q, verbose = verbose)
            cpos <- NULL
          } else {
            cpos <- CQI$id2cpos(.Object, pAttribute, id)
          }
        })
        if (!is.null(cpos)) matrix(c(cpos, cpos), ncol = 2) else NULL
      }
    )
    hits <- do.call(rbind, hitList)
  } else if (cqp) {
    hitList <- lapply(
      query,
      function(Q){
        CQI$query(.Object, Q)
        cpos <- try(CQI$dump_subcorpus(.Object), silent = TRUE)
        if (is(cpos)[1] == "try-error"){
          .message("no hits for query: ", Q, verbose = verbose)
          hits <- NULL
        } else if (!is.null(cpos)) {
          hits <- matrix(cpos[,1:2], ncol = 2)
        } else {
          .message("no hits for query: ", Q, verbose = verbose)
          hits <- NULL
        }
        return(hits)
      }
    )
    hits <- do.call(rbind, hitList)
  }
  if (nrow(hits) == 0) invisible(NULL) else hits
})
  
#' @rdname cpos-method
setMethod("cpos", "partition", function(.Object, query, cqp = is.cqp, pAttribute = NULL, verbose = TRUE, ...){
  hits <- cpos(.Object@corpus, query = query, cqp = cqp, pAttribute, encoding = .Object@encoding, verbose = verbose)
  if (!is.null(hits) && length(.Object@sAttributes) > 0){
    sAttribute <- names(.Object@sAttributes)[length(.Object@sAttributes)]
    strucHits <- CQI$cpos2struc(.Object@corpus, sAttribute, hits[,1])
    hits <- hits[which(strucHits %in% .Object@strucs),]
    if (is(hits)[1] == "integer") hits <- matrix(data = hits, ncol = 2)
    if (nrow(hits) == 0) hits <- NULL
  }
  if (is(hits)[1] == "integer") hits <- matrix(hits, ncol = 2)
  hits
})

#' @param shift logical, if true, the cpos resulting from the query performed on
#'   the tempcorpus will be shifted so that they match the positions of the
#'   corpus from which the tempcorpus was generated
#' @rdname cpos-method
setMethod("cpos", "tempcorpus", function(.Object, query, shift = TRUE){
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
  cposMatrix <- cposMatrix[,1:2]
  if (shift) cposMatrix <- apply(cposMatrix, 2, function(x) x + .Object@cpos[1,1])
  cposMatrix
})

#' @rdname cpos-method
setMethod("cpos", "matrix", function(.Object)
  as.vector(unlist(apply(.Object, 1, function(row) row[1]:row[2])))  
)

#' @rdname cpos-method
setMethod("cpos", "hits", function(.Object)
  cpos(as.matrix(.Object@stat[, c("cpos_left", "cpos_right")]))
)


