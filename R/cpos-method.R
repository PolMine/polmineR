#' get the corpus positions for a query
#' 
#' @param .Object an Object
#' @param query a query, may be CQP
#' @param pAttribute which attribute to search
#' @param ... further arguments
#' @exportMethod cpos
#' @rdname cpos-method
#' @name cpos
setGeneric("cpos", function(.Object, ... ) standardGeneric("cpos"))

#' @rdname cpos-method
setMethod("cpos", "character", function(.Object, query, pAttribute=NULL, encoding="latin-1", verbose=TRUE){
  if (is.null(pAttribute)) pAttribute <- slot(get("session", ".GlobalEnv"), "pAttribute")
  if (length(query) > 1) warning("query needs to be a character vector with length 1")
  query <- adjustEncoding(query, encoding)
  if (grepl('"', query) == FALSE) {
    pAttr <- paste(.Object, '.', pAttribute, sep='')
    cpos <- try(cqi_id2cpos(pAttr, cqi_str2id(pAttr, query)), silent=TRUE)
    if (is(cpos)[1] != "try-error"){
      hits <- matrix(c(cpos, cpos), ncol=2)
    }
  } else if (grepl('"', query) == TRUE) {
    cqi_query(.Object, "Hits", query)
    cpos <- try(cqi_dump_subcorpus(paste(.Object, ":Hits", sep="")), silent=TRUE)
    if(is(cpos)[1] != "try-error"){
      hits <- matrix(cpos[,1:2], ncol=2)
    }
  }
  if (is(cpos)[1] != "try-error") {
    if (is(hits)[1] == "integer") {
      hits <- matrix(hits, nrow=1)
    }
  } else {
    if (verbose == TRUE) warning("no hits for query -> ", query)
    hits = NULL
  }
  hits
})
  
setMethod("cpos", "partition", function(.Object, query, pAttribute=NULL, verbose=TRUE){
  # if (is.null(encoding)) encoding <- 
  hits <- cpos(
    .Object@corpus, query=query, pAttribute,
    encoding=.Object@encoding, verbose=verbose
    )
  if (!is.null(hits)){
    sAttribute <- names(.Object@sAttributes)[length(.Object@sAttributes)]
    corpus.sAttribute <- paste(.Object@corpus, ".", sAttribute, sep="")
    strucHits <- cqi_cpos2struc(corpus.sAttribute, hits[,1])
    hits <- hits[which(strucHits %in% .Object@strucs),]
  }
  if (nrow(hits) == 0) hits <- NULL
  return(hits)
})