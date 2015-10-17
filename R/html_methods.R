#' @include html_method.R generics.R
NULL

setOldClass("html")


setMethod("print", "html", function(x) {
  if (requireNamespace("htmltools", quietly=T)){
    htmltools::html_print(x)
  } else {
    warning("package 'htmltools' needs to be installed, but is not available")
  }
})

setMethod("show", "html", function(object) {
  if (requireNamespace("htmltools", quietly=T)){
    htmltools::html_print(object)
  } else {
    warning("package 'htmltools' needs to be installed, but is not available")
  }  
})

