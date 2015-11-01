#' @include cooccurrences_class.R
NULL

#' @rdname cooccurrencesReshaped
setMethod("merge", "cooccurrencesReshaped", function(x,y){
  if (all(c(class(x), class(y)) == "cooccurrencesReshaped") == FALSE) warning("x and y need to be cooccurrences objects")
  dfRet <- merge(x@stat, y@stat, by.x=0, by.y=0)
  dfRet
})
