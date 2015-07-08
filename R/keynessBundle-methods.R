#' @include keynessBundle-class.R
NULL


#' @rdname keynessBundle-class
setMethod("as.TermDocumentMatrix", signature(x="keynessBundle"), function(x, col="chiSquare", rmBlank=TRUE, verbose=TRUE){
  object <- x
  encoding <- unique(unlist(lapply(object@objects, function(o) o@encoding)))
  corpus <- unique(unlist(lapply(object@objects, function(o) o@corpus)))
  pAttribute <- unique(unlist(lapply(object@objects, function(o) o@pAttribute)))
  pAttr <- paste(corpus, '.', pAttribute, sep='')
  i <- unlist(lapply(object@objects, function(o) (cqi_str2id(pAttr, rownames(o@stat))+1)))
  j <- unlist(lapply(c(1:length(object@objects)), function(m) {rep(m,times=nrow(x[[m]]@stat))}))
  v <- unlist(lapply(object@objects, function(c) c@stat[,col]))
  lexiconSize <- cqi_lexicon_size(pAttr)
  mat <- simple_triplet_matrix(
    i=i, j=j, v=v,
    ncol=length(x@objects),
    nrow=lexiconSize+1,
    dimnames=list(
      Terms=cqi_id2str(pAttr, c(0:lexiconSize)),
      Docs=names(object@objects)
    )
  )
  mat$dimnames$Terms <- iconv(mat$dimnames$Terms, from=encoding, to="UTF-8")
  class(mat) <- c("TermDocumentMatrix", "simple_triplet_matrix")
  if (rmBlank==TRUE) mat <- .rmBlank(mat, verbose=verbose)
  mat
})


#' @docType methods
#' @noRd
setMethod("as.matrix", signature(x="keynessBundle"), function(x, col="chiSquare", rmBlank=TRUE, verbose=TRUE){
  mat <- as.TermDocumentMatrix(x, col=col, rmBlank=rmBlank, verbose=verbose)
  mat <- as.matrix(mat)
  mat
})


setMethod("summary", "keynessBundle", function(object){
  tab <- do.call(rbind, lapply(object@objects, function(x) summary(x)$no))
  colnames(tab) <- c("0.001", "0.005", "0.010", "0.050")
  tab
})

#' @rdname keynessBundle-class
setMethod("dim", "keynessBundle", function(x) length(x@objects))
