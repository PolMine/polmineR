#' find cooccurrences 
#' 
#' a story to be told
#' 
#' @exportMethod cooccurrence
#' @param object the partition
#' @param x cqp query one
#' @param y cqp query two
#' @param dist distance between queries
#' @param as.character logical, defaults to TRUE
#' @param ... further parameters
#' @rdname cooccurrence
setGeneric("cooccurrence", function(object, ...){standardGeneric("cooccurrence")})

#' @rdname cooccurrence
setMethod("cooccurrence", "partition", function(object, x, y, dist=5, as.character=TRUE){
  query_xy <- paste(x, sprintf('[]{0,%d}', dist), y, sep='')
  query_yx <- paste(y, sprintf('[]{0,%d}', dist), x, sep='')
  cpos_xy <- polmineR:::.queryCpos(query_xy, object, pAttribute=NULL, verbose=FALSE)
  cpos_yx <- polmineR:::.queryCpos(query_yx, object, pAttribute=NULL, verbose=FALSE)
  cpos <- rbind(cpos_xy, cpos_yx)
  if (as.character == FALSE){
    retval <- nrow(cpos)
  } else {
    retval <- apply(cpos, 1, function(cposPair) {
      paste(CQI$cpos2str(object@corpus, 'word', c(cposPair[1]:cposPair[2])), collapse=" ")
    })
    retval <- iconv(retval, from=object@encoding, to="UTF-8")
  }
  retval
})
