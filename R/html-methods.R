#' @include html-method.R generics.R
NULL

setOldClass("html")


#' @importFrom htmltools html_print
setMethod("print", "html", function(x) html_print(x))

setMethod("show", "html", function(object) html_print(object))

