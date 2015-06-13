#' corpus positions for a query
#' 
#' Get the corpus positions (cpos) for a query. The query may also be a 
#' CQP query. 
#' 
#' @param .Object a character vector (length 1) giving the name of a corpus, or a partition object
#' @param query a query, either a single term, or a CQP query
#' @param pAttribute the p-attribute to search
#' @param encoding the encoding of the corpus (if NULL, the
#'  encoding provided in the registry file of the corpus will be used)
#' @param verbose logical, whether to be talkative
#' @param ... further arguments
#' @return You get a matrix with two columns, the first column giving the start cpos of the hits obtained,
#' the second column giving the end cpos of the respective hit. The number of rows is the number of hits.
#' If there are no hits, a NULL object will be returned.
#' @exportMethod cpos
#' @rdname cpos-method
#' @name cpos
setGeneric("cpos", function(.Object, ... ) standardGeneric("cpos"))

#' @rdname cpos-method
setMethod("cpos", "character", function(.Object, query, pAttribute=NULL, encoding=NULL, verbose=TRUE){
  if (is.null(pAttribute)) pAttribute <- slot(get("session", ".GlobalEnv"), "pAttribute")
  if (length(query) > 1) warning("query needs to be a character vector with length 1")
  if (is.null(encoding)) encoding <- getEncoding(.Object) 
  query <- adjustEncoding(query, encoding)
  if (grepl('"', query) == FALSE) {
    pAttr <- paste(.Object, '.', pAttribute, sep='')
    cpos <- try(cqi_id2cpos(pAttr, cqi_str2id(pAttr, query)), silent=TRUE)
    if (is(cpos)[1] != "try-error"){
      hits <- matrix(c(cpos, cpos), ncol=2)
    } else {
      if (verbose == TRUE) message("no hits for query -> ", query)
      hits = NULL
    }
  } else if (grepl('"', query) == TRUE) {
    cqi_query(.Object, "Hits", query)
    cpos <- try(cqi_dump_subcorpus(paste(.Object, ":Hits", sep="")), silent=TRUE)
    if (!is.null(cpos)){
      hits <- matrix(cpos[,1:2], ncol=2)
    } else {
      if (verbose == TRUE) message("no hits for query -> ", query)
      hits = NULL
    }
  }
  hits
})
  
#' @rdname cpos-method
setMethod("cpos", "partition", function(.Object, query, pAttribute=NULL, verbose=TRUE){
  hits <- cpos(.Object@corpus, query=query, pAttribute, encoding=.Object@encoding, verbose=verbose)
  if (!is.null(hits)){
    sAttribute <- names(.Object@sAttributes)[length(.Object@sAttributes)]
    corpus.sAttribute <- paste(.Object@corpus, ".", sAttribute, sep="")
    strucHits <- cqi_cpos2struc(corpus.sAttribute, hits[,1])
    hits <- hits[which(strucHits %in% .Object@strucs),]
    if (nrow(hits) == 0){
      hits <- NULL
    }
  }
  if (is(hits)[1] == "integer") {
    hits <- matrix(hits, ncol=2)
  }
  return(hits)
})