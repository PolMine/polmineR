#' @include polmineR-package.R
NULL

#' @exportMethod browse
setGeneric("browse", function(object, ...){standardGeneric("browse")})

setMethod("browse", "html", function(object){
  tmpFile <- tempfile(fileext=".html")
  cat(object, file=tmpFile)
  browseURL(tmpFile)
})

#' @rdname partition
setMethod("browse", "html", function(object){browse(html(object))})