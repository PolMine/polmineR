#' @include partition_class.R TermDocumentMatrix_methods.R
NULL

#' Get number of tokens.
#' 
#' The method will get the number of tokens in a corpus or partition,
#' or the dispersion across one or more s-attributes.
#' 
#' One or more s-attributes can be provided to get the dispersion of
#' tokens across one or more dimensions. Two or more s-attributes
#' can lead to reasonable results only if the corpus XML is flat.
#' 
#' @param x object to get size(s) for
#' @param sAttribute character vector with s-attributes (one or more)
#' @param verbose logical, whether to print messages
#' @param ... further arguments
#' @rdname size-method
#' @return an integer vector if sAttribute is NULL, a \code{data.table} otherweise
#' @seealso See \code{\link{dispersion}}-method for counts of hits. The \code{\link{hits}}
#' method calls the \code{size}-method to get sizes of subcorpora.
#' @examples
#' \dontrun{
#' use("polmineR.sampleCorpus")
#' size("PLPRBTTXT")
#' size("PLPRBTTXT", sAttribute = "text_date")
#' size("PLPRBTTXT", sAttribute = c("text_date", "text_party"))
#' 
#' P <- partition("PLPRBTTXT", text_date = "2009-11-11")
#' size(P, sAttribute = "text_name")
#' size(P, sAttribute = "text_party")
#' size(P, sAttribute = c("text_name", "text_party"))
#' }
setGeneric("size", function(x, ...) UseMethod("size"))

#' @rdname size-method
setMethod("size", "character", function(x, sAttribute = NULL, verbose = TRUE){
  if (is.null(sAttribute)){
    return(CQI$attribute_size(x, "word", type = "p"))
  } else {
    stopifnot(all(sAttribute %in% sAttributes(x)))
    dt <- as.data.table(
      lapply(
        setNames(sAttribute, sAttribute),
        function(sAttr) CQI$struc2str(x, sAttr, c(0:(CQI$attribute_size(x, sAttr, type = "s") - 1))))
    )
    if (requireNamespace("polmineR.Rcpp", quietly = TRUE) && (getOption("polmineR.Rcpp") == TRUE)){
      if (verbose) message ("... polmineR.Rcpp available, going to use it")
      cpos_matrix <- polmineR.Rcpp::getRegionMatrix(
        corpus = x, sAttribute = sAttribute[1],
        strucs = 0:(CQI$attribute_size(x, sAttribute[1], "s") - 1),
        registry = Sys.getenv("CORPUS_REGISTRY")
        )
    } else if (system("cwb-s-decode -h", intern = FALSE, ignore.stderr =  TRUE) == 1){
      if (verbose) message ("... cwb-s-decode utility available, going to use it")
      cmd <- c("cwb-s-decode", "-v", "-r", Sys.getenv("CORPUS_REGISTRY"), x, "-S", sAttribute[1])
      decode_result <- system(paste(cmd, collapse = " "), intern = TRUE)
      cpos_matrix <- do.call(rbind, lapply(strsplit(decode_result, "\\t"), as.integer))
    } else {
      cpos_matrix <- do.call(
        rbind,
        lapply(
          c(0:(CQI$attribute_size(x, sAttribute[1], type = "s") - 1)),
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
    stopifnot(all(sAttribute %in% sAttributes(x)))
    dt <- as.data.table(
      lapply(
        setNames(sAttribute, sAttribute),
        function(sAttr) as.nativeEnc(CQI$struc2str(x@corpus, sAttr, x@strucs), from = x@encoding)
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
