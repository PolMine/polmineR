#' @include html_method.R generics.R
NULL

setOldClass("html")


#' @export print.html
#' @rdname html-method
#' @S3method print html
print.html <- function(x, ...){
  if (requireNamespace("htmltools", quietly = TRUE)){
    if (interactive()) htmltools::html_print(x)
  } else {
    warning("package 'htmltools' needs to be installed, but is not available")
  }  
}
