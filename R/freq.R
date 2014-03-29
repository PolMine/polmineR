#' Frequency breakdown of the variation of query results 
#'
#' Returns a frequency breakdown of the results of a query
#' 
#' @param query a query, CQP syntax may be used
#' @param Partition a partition object
#' @param pAttribute pAttribute
#' @export
#' @rdname frequencies
#' @name frequencies
frequencies <- function(query, Partition, pAttribute=NULL) {
  if (is.null(pAttribute)) pAttribute <- get("drillingControls", '.GlobalEnv')[['pAttribute']]
  cpos <- .queryCpos(query, Partition, pAttribute)
  attr <- paste(Partition@corpus, '.', pAttribute, sep='')
  explicit <- apply(cpos, 1, function(x) paste(cqi_cpos2str(attr, x[1]:x[2]), collapse=' '))
  Encoding(explicit) <- Partition@encoding
  result <- table(explicit)
  bag <- as.vector(result)
  names(bag) <- names(result)
  bag <- bag[order(bag, decreasing=TRUE)]
  return(bag)
}