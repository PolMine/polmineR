#' Obtain frequencies
#' 
#' Get term frequencies for a partition object. This is a helper function
#' for \code{partition}.
#'
#' @param part a partition object
#' @param pAttribute either 'word' or 'lemma'
#' @noRd
.cpos2tf <- function(part, pAttribute){
  cpos <- unlist(apply(part@cpos, 1, function(x) x[1]:x[2]))
  ids <- cqi_cpos2id(paste(part@corpus, '.', pAttribute, sep=''), cpos)
  tfRaw <- tabulate(ids)
  tf <- data.frame(
    id=c(0:length(tfRaw)),
    tf=c(length(ids[which(ids==0)]), tfRaw),
    row.names=cqi_id2str(paste(part@corpus,'.',pAttribute, sep=''), c(0:length(tfRaw)))
  )
  tf <- subset(tf, tf > 0)
  Encoding(rownames(tf)) <- part@encoding
  tf
}

#' Frequency breakdown of the variation of query results 
#'
#' If one query is supplied, the function returns a frequency breakdown of the results of the query.
#' If several queries are supplied, frequencies for the individual queries are retrieved.
#' 
#' @param query a query, CQP syntax may be used
#' @param Partition a partition object
#' @param pAttribute pAttribute
#' @export
#' @rdname frequencies
#' @name frequencies
frequencies <- function(query, Partition, pAttribute=NULL) {
  if (is.null(pAttribute)) pAttribute <- get("drillingControls", '.GlobalEnv')[['pAttribute']]
  if (length(query)==1) {
    cpos <- .queryCpos(query, Partition, pAttribute)
    attr <- paste(Partition@corpus, '.', pAttribute, sep='')
    explicit <- apply(cpos, 1, function(x) paste(cqi_cpos2str(attr, x[1]:x[2]), collapse=' '))
    result <- table(explicit)
    bag <- data.frame(
      queryResult=names(result),
      freq=as.vector(result),
      stringsAsFactors=FALSE
    )
    Encoding(bag[,1]) <- Partition@encoding
    bag <- bag[order(bag[,2], decreasing=TRUE),]
    row.names(bag) <- c(1:nrow(bag))
  } else if (length(query>1)){
    bag <- sapply(
      query,
      function(x)(nrow(.queryCpos(x, Partition,pAttribute)))
    )
    bag <- unlist(bag)
    bag <- bag[order(bag, decreasing=TRUE)]
  }
  return(bag)
}