#' @include html_method.R generics.R
NULL

setOldClass("html")


#' @export print.html
#' @rdname html-method
print.html <- function(x, ...){
  if (requireNamespace("htmltools", quietly = TRUE)){
    htmltools::html_print(x)
  } else {
    warning("package 'htmltools' needs to be installed, but is not available")
  }  
}
