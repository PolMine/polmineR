#' @include partition.R TermDocumentMatrix.R S4classes.R
NULL

#' Get Number of Tokens.
#' 
#' The method will get the number of tokens in a corpus or partition,
#' or the dispersion across one or more s-attributes.
#' 
#' One or more s-attributes can be provided to get the dispersion of
#' tokens across one or more dimensions. Two or more s-attributes
#' can lead to reasonable results only if the corpus XML is flat.
#' 
#' @param x object to get size(s) for
#' @param s_attribute character vector with s-attributes (one or more)
#' @param verbose logical, whether to print messages
#' @param ... further arguments
#' @rdname size-method
#' @return an integer vector if s_attribute is NULL, a \code{data.table} otherweise
#' @seealso See \code{\link{dispersion}}-method for counts of hits. The \code{\link{hits}}
#' method calls the \code{size}-method to get sizes of subcorpora.
#' @examples
#' use("polmineR")
#' size("GERMAPARLMINI")
#' size("GERMAPARLMINI", s_attribute = "date")
#' size("GERMAPARLMINI", s_attribute = c("date", "party"))
#' 
#' P <- partition("GERMAPARLMINI", date = "2009-11-11")
#' size(P, s_attribute = "speaker")
#' size(P, s_attribute = "party")
#' size(P, s_attribute = c("speaker", "party"))
setGeneric("size", function(x, ...) UseMethod("size"))

#' @rdname size-method
setMethod("size", "character", function(x, s_attribute = NULL, verbose = TRUE, ...){
  
  if ("sAttribute" %in% names(list(...))) s_attribute <- list(...)[["sAttribute"]]
  
  if (is.null(s_attribute)){
    return( CQI$attribute_size(x, "word", type = "p") )
  } else {
    stopifnot(all(s_attribute %in% s_attributes(x)))
    dt <- data.table::as.data.table(
      lapply(
        setNames(s_attribute, s_attribute),
        function(sAttr){
          sAttrDecoded <- CQI$struc2str(x, sAttr, 0:(CQI$attribute_size(x, sAttr, type = "s") - 1))
          as.nativeEnc(sAttrDecoded, from = registry_get_encoding(x))
        }
      )
    )
    cpos_matrix <- RcppCWB::get_region_matrix(
      corpus = x, s_attribute = s_attribute[1],
      strucs = 0L:(CQI$attribute_size(x, s_attribute[1], "s") - 1L),
      registry = Sys.getenv("CORPUS_REGISTRY")
    )
    
    dt[, size := cpos_matrix[,2] - cpos_matrix[,1] + 1L]
    y <- dt[, sum(size), by = eval(s_attribute), with = TRUE]
    setnames(y, old = "V1", new = "size")
    setkeyv(y, cols = s_attribute)
    return(y)
  }
})

#' @rdname size-method
#' @exportMethod size
setMethod("size", "partition", function(x, s_attribute = NULL, ...){
  
  if ("sAttribute" %in% names(list(...))) s_attribute <- list(...)[["sAttribute"]]
  
  if (is.null(s_attribute)){
    return( sum(as.integer(x@cpos[,2L]) - as.integer(x@cpos[,1L]) + 1L) )
  } else {
    stopifnot(all(s_attribute %in% s_attributes(x)))
    dt <- data.table::as.data.table(
      lapply(
        setNames(s_attribute, s_attribute),
        function(sAttr) as.nativeEnc(CQI$struc2str(x@corpus, sAttr, x@strucs), from = x@encoding)
      )
    )
    dt[, size := x@cpos[,2] - x@cpos[,1] + 1L]
    y <- dt[, sum(size), by = eval(s_attribute), with = TRUE]
    setnames(y, old = "V1", new = "size")
    setkeyv(y, cols = s_attribute)
    return( y )
  }
  })


#' @rdname size-method
setMethod("size", "DocumentTermMatrix", function(x){
  setNames(tapply(x$v, INDEX = x$i, sum), x[["dimnames"]][["Docs"]])
})

#' @rdname size-method
setMethod("size", "TermDocumentMatrix", function(x){
  setNames(tapply(x$v, INDEX = x$j, sum), x[["dimnames"]][["Docs"]])
})

#' @details The \code{size}-method for \code{features} objects will return a
#'   named list with the size of the corpus of interest ("coi"), i.e. the number
#'   of tokens in the window, and the reference corpus ("ref"), i.e. the number
#'   of tokens that are not matched by the query and that are outside the
#'   window.
#' @rdname size-method
setMethod("size", "features", function(x) list(coi = x@size_coi, ref = x@size_ref) )
