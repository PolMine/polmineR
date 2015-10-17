#' @include collocations_class.R
NULL

#' @rdname collocationsReshaped
setMethod("merge", "collocationsReshaped", function(x,y){
  if (all(c(class(x), class(y)) == "collocationsReshaped") == FALSE) warning("x and y need to be collocations objects")
  dfRet <- merge(x@stat, y@stat, by.x=0, by.y=0)
  dfRet
})
