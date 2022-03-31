#' @include S4classes.R
NULL

#' Get template for reconstructing full text.
#' 
#' Templates are used to format the markdown/html output of partitions.
#' 
#' @param .Object A \code{corpus}, \code{subcorpus} or \code{partition} object,
#'   or a length-one \code{character} vector with a corpus ID.
#' @param warn A \code{logical} value, whether to issue a warning if template 
#'   is not available. Defaults to \code{FALSE}.
#' @param ... Further arguments to be defined.
#' @exportMethod get_template
#' @rdname templates
#' @aliases get_template,subcorpus-method
setGeneric("get_template", function(.Object, ...) standardGeneric("get_template"))

#' @rdname templates
setMethod("get_template", "character", function(.Object, warn = FALSE){
  get_template(corpus(.Object), warn = warn)
})

#' @rdname templates
setMethod("get_template", "corpus", function(.Object, warn = FALSE){
  if (is.na(.Object@template)){
    if (warn) warning(
      sprintf("No template available for corpus '%s'.", .Object@corpus)
    )
    return(NULL)
  } else {
    y <- jsonlite::fromJSON(txt = .Object@template) 
    if ("metadata" %in% names(y)) y[["metadata"]] <- unlist(y[["metadata"]])
    return(y)
  }
})


#' @rdname templates
setMethod("get_template", "subcorpus", function(.Object, warn = FALSE){
  callNextMethod()
})
