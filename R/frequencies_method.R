setGeneric("frequencies", function(object, ...){standardGeneric("frequencies")})


#' Frequency breakdown of the variation of query results 
#'
#' If one query is supplied, the function returns a frequency breakdown of the results of the query.
#' If several queries are supplied, frequencies for the individual queries are retrieved.
#' 
#' @param query a query, CQP syntax may be used
#' @param object a partition object
#' @param pAttribute pAttribute
#' @return a matrix
#' @exportMethod frequencies
#' @docType methods
#' @rdname frequencies
#' @name frequencies
#' @aliases frequencies frequencies-method frequencies,partition-method frequencies,partitionBundle-method
setMethod("frequencies", "partition", function(object, query, pAttribute=getOption("polmineR.pAttribute")) {
  if (length(query)==1) {
    cpos <- cpos(query, object, pAttribute)
    # attr <- paste(object@corpus, '.', pAttribute, sep='')
    explicit <- apply(cpos, 1, function(x) paste(CQI$cpos2str(object@corpus, pAttribute, x[1]:x[2]), collapse=' '))
    result <- table(explicit)
    bag <- data.frame(
      queryResult=names(result),
      freq=as.vector(result),
      stringsAsFactors=FALSE
    )
    if (nrow(bag) > 0){
      Encoding(bag[,1]) <- object@encoding
      bag <- bag[order(bag[,2], decreasing=TRUE),]
      row.names(bag) <- c(1:nrow(bag))
    } else {
      bag <- NULL
    }
  } else if (length(query>1)){
    bag <- sapply(
      query,
      function(x)(nrow(cpos(x, object, pAttribute)))
    )
    bag <- unlist(bag)
    bag <- bag[order(bag, decreasing=TRUE)]
  }
  return(bag)
})

#' @docType methods
#' @noRd
setMethod("frequencies", "partitionBundle", function(object, query, pAttribute=NULL){
  bag <- lapply(object@objects, function(x) frequencies(x, query, pAttribute=pAttribute))
  tmp <- do.call(rbind, lapply(names(bag), function(x) {
    if (!is.null(bag[[x]])) data.frame(partition=rep(x, nrow(bag[[x]])), bag[[x]])
    }))
  tab <- as.matrix(ftable(xtabs(freq~queryResult+partition, data=tmp)))
  return(tab)
})
