#' run treetagger over a character vector
#' 
#' treetagger is used as a backend
#' 
#' @param object a character vector
#' @param ... further parameters
#' @rdname tag
#' @name tag
#' @exportMethod tag
setGeneric("tag", function(object, ...){standardGeneric("tag")})

#' @rdname tag
setMethod("tag", "character", function(object){
  if (requireNamespace("koRpus", quietly = TRUE)) {
    # requireNamespace("koRpus", quietly = TRUE)
    retval <- koRpus::treetag(
      object,
      treetagger="/opt/treetagger/cmd/tree-tagger-german",
      lang="de-utf8",
      format="obj"
    )  
  } else {
    warning("the koRpus package is not available, but necessary for tagging")
    stop()
  }
  retval
})
