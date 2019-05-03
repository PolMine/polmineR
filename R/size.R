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
#' @param x An object to get size(s) for.
#' @param s_attribute A \code{character} vector with s-attributes (one or more).
#' @param verbose A \code{logical} value, whether to output messages.
#' @param ... Further arguments (used only for backwards compatibility).
#' @rdname size-method
#' @return An \code{integer} vector if s_attribute is \code{NULL}, a \code{data.table} otherwise.
#' @seealso See \code{\link{dispersion}}-method for counts of hits. The \code{\link{hits}}
#' method calls the \code{size}-method to get sizes of subcorpora.
#' @aliases size,slice-method
#' @examples
#' use("polmineR")
#' 
#' # for corpus object
#' corpus("REUTERS") %>% size()
#' corpus("REUTERS") %>% size(s_attribute = "id")
#' corpus("GERMAPARLMINI") %>% size(s_attribute = c("date", "party"))
#' 
#' # for corpus specified by ID
#' size("GERMAPARLMINI")
#' size("GERMAPARLMINI", s_attribute = "date")
#' size("GERMAPARLMINI", s_attribute = c("date", "party"))
#' 
#' # for partition object
#' P <- partition("GERMAPARLMINI", date = "2009-11-11")
#' size(P, s_attribute = "speaker")
#' size(P, s_attribute = "party")
#' size(P, s_attribute = c("speaker", "party"))
#' 
#' # for subcorpus
#' sc <- corpus("GERMAPARLMINI") %>% subset(date == "2009-11-11")
#' size(sc, s_attribute = "speaker")
#' size(sc, s_attribute = "party")
#' size(sc, s_attribute = c("speaker", "party"))
setGeneric("size", function(x, ...) UseMethod("size"))

#' @rdname size-method
setMethod("size", "corpus", function(x, s_attribute = NULL, verbose = TRUE, ...){
  
  if ("sAttribute" %in% names(list(...))) s_attribute <- list(...)[["sAttribute"]]
  
  if (is.null(s_attribute)){
    return( cl_attribute_size(corpus = x@corpus, attribute = "word", attribute_type = "p", registry = registry()) )
  } else {
    stopifnot(all(s_attribute %in% s_attributes(x)))
    dt <- data.table::as.data.table(
      lapply(
        setNames(s_attribute, s_attribute),
        function(s_attr){
          s_attr_max <- cl_attribute_size(corpus = x@corpus, attribute = s_attr, attribute_type = "s", registry = registry())
          s_attr_vals <- cl_struc2str(corpus = x@corpus, s_attribute = s_attr, struc = 0L:(s_attr_max - 1L), registry = registry())
          as.nativeEnc(s_attr_vals, from = x@encoding)
        }
      )
    )
    cpos_matrix <- RcppCWB::get_region_matrix(
      corpus = x@corpus, s_attribute = s_attribute[1],
      strucs = 0L:(cl_attribute_size(corpus = x@corpus, attribute = s_attribute[1], attribute_type = "s", registry = registry()) - 1L),
      registry = Sys.getenv("CORPUS_REGISTRY")
    )
    
    dt[, size := cpos_matrix[,2] - cpos_matrix[,1] + 1L]
    y <- dt[, sum(size), by = eval(s_attribute), with = TRUE]
    setnames(y, old = "V1", new = "size")
    setkeyv(y, cols = s_attribute)
    return(y)
  }
})

#' @exportMethod size
#' @rdname size-method
setMethod("size", "character", function(x, s_attribute = NULL, verbose = TRUE, ...){
  size(corpus(x), s_attribute = s_attribute, verbose = verbose, ...)
})


#' @noRd
setMethod("size", "slice", function(x, s_attribute = NULL, ...){
  
  if ("sAttribute" %in% names(list(...))) s_attribute <- list(...)[["sAttribute"]]
  
  if (is.null(s_attribute)){
    return( sum(as.integer(x@cpos[,2L]) - as.integer(x@cpos[,1L]) + 1L) )
  } else {
    stopifnot(all(s_attribute %in% s_attributes(x)))
    dt <- data.table::as.data.table(
      lapply(
        setNames(s_attribute, s_attribute),
        function(sAttr) as.nativeEnc(cl_struc2str(corpus = x@corpus, s_attribute = sAttr, struc = x@strucs, registry = registry()), from = x@encoding)
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
setMethod("size", "partition", function(x, s_attribute = NULL, ...){
  callNextMethod(x = x, s_attribute = s_attribute, ...)
})

#' @describeIn subcorpus Get the size of a \code{subcorpus} object from the
#'   respective slot of the object.
setMethod("size", "subcorpus", function(x, s_attribute = NULL, ...){
  callNextMethod()
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


#' @rdname size-method
setMethod("size", "remote_corpus", function(x){
  ocpu_exec(fn = "size", server = x@server, do.call = FALSE, x = x@corpus)
})

#' @rdname size-method
setMethod("size", "remote_partition", function(x){
  ocpu_exec(fn = "size", server = x@server, x = as(x, "partition"))
})


