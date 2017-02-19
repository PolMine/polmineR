#' @include partition_class.R TermDocumentMatrix_methods.R
NULL

#' Get number of tokens.
#' 
#' @param x object to get size(s) for
#' @param sAttribute character vector with s-attributes (one or more)
#' @param verbose logical, whether to print messages
#' @param ... further arguments
#' @rdname size-method
setGeneric("size", function(x, ...) UseMethod("size"))

#' @rdname size-method
setMethod("size", "character", function(x, sAttribute = NULL, verbose = TRUE){
  if (is.null(sAttribute)){
    return(CQI$attribute_size(x, "word"))
  } else {
    stopifnot(all(sAttribute %in% sAttributes(x)))
    dt <- as.data.table(
      lapply(
        setNames(sAttribute, sAttribute),
        function(sAttr) CQI$struc2str(x, sAttr, c(0:(CQI$attribute_size(x, sAttr) - 1))))
    )
    if (system("cwb-s-decode -h", intern = FALSE, ignore.stderr =  TRUE) == 1){
      if (verbose) message ("... cwb-s-decode utility found, going to use it")
      cmd <- c("cwb-s-decode", "-v", "-r", Sys.getenv("CORPUS_REGISTRY"), x, "-S", sAttribute[1])
      decode_result <- system(paste(cmd, collapse = " "), intern = TRUE)
      cpos_matrix <- do.call(rbind, lapply(strsplit(decode_result, "\\t"), as.integer))
    } else {
      cpos_matrix <- do.call(
        rbind,
        lapply(
          c(0:(CQI$attribute_size(x, sAttribute[1]) - 1)),
          function(x) CQI$struc2cpos(x, sAttribute[1], x))
      )
    }
  
    dt[, size := cpos_matrix[,2] - cpos_matrix[,1] + 1]
    y <- dt[, sum(size), by = eval(sAttribute), with = TRUE]
    setnames(y, old = "V1", new = "size")
    setkeyv(y, cols = sAttribute)
    return(y)
  }
})

#' @rdname size-method
#' @exportMethod size
setMethod("size", "partition", function(x, sAttribute = NULL){
  if (is.null(sAttribute)){
    return( sum(x@cpos[,2] - x@cpos[,1] + 1) )
  } else {
    dt <- as.data.table(
      lapply(
        setNames(sAttribute, sAttribute),
        function(sAttr) as.utf8(CQI$struc2str(x@corpus, sAttr, x@strucs), from = x@encoding)
      )
    )
    dt[, size := x@cpos[,2] - x@cpos[,1] + 1]
    y <- dt[, sum(size), by = eval(sAttribute), with = TRUE]
    setnames(y, old = "V1", new = "size")
    setkeyv(y, cols = sAttribute)
    return(y)
  }
  })


#' @rdname size-method
setMethod("size", "DocumentTermMatrix", function(x){
  setNames(tapply(x$v, INDEX = x$i, sum), x[["dimnames"]][["Docs"]])
})

#' @rdname TermDocumentMatrix
setMethod("size", "TermDocumentMatrix", function(x){
  setNames(tapply(x$v, INDEX=x$j, sum), x[["dimnames"]][["Docs"]])
})

setGeneric("reindex", function(x) standardGeneric("reindex"))

setMethod("reindex", "DocumentTermMatrix", function(x){
  i_uniqueValues <- unique(x$i)
  i_uniqueValuesOrdered <- i_uniqueValues[order(i_uniqueValues)]
  i_newIndex <- rep(0, times=i_uniqueValuesOrdered[length(i_uniqueValuesOrdered)])
  i_newIndex[i_uniqueValuesOrdered] <- c(1:length(i_uniqueValues))
  x$i <- i_newIndex[x$i]
  x$nrow <- length(i_uniqueValues)

  j_uniqueValues <- unique(x$j)
  j_uniqueValuesOrdered <- j_uniqueValues[order(j_uniqueValues)]
  j_newIndex <- rep(0, times=j_uniqueValuesOrdered[length(j_uniqueValuesOrdered)])
  j_newIndex[j_uniqueValuesOrdered] <- c(1:length(j_uniqueValues))
  x$j <- j_newIndex[x$j]
  x$j <- length(j_uniqueValues)
  
  x
})

setMethod("reindex", "TermDocumentMatrix", function(x){
  t(reindex(t(x)))
})