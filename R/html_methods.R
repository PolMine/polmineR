#' @include html_method.R generics.R
NULL

setOldClass("html")


#' @export print.html
print.html <- function(object){
  if (requireNamespace("htmltools", quietly = TRUE)){
    htmltools::html_print(object)
  } else {
    warning("package 'htmltools' needs to be installed, but is not available")
  }  
}
