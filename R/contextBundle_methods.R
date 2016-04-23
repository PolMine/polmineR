#' @include contextBundle_class.R
NULL




#' @docType methods
#' @noRd
setMethod("summary", "contextBundle", function(object, top=3){
  partitionSizes <- unlist(lapply(object@objects, function(x) x@partitionSize))
  counts <- unlist(lapply(object@objects, function(x) x@frequency))
  overview <- data.frame(
    count=counts,
    freq=round(counts/partitionSizes*100000,2)
    )
  overview <- cbind(overview, t(data.frame(lapply(object@objects, function(x) .statisticalSummary(x)$no))))
  colnames(overview)[3:6] <- criticalValue <- c(">10.83", ">7.88", ">6.63", ">3.84")
  overview <- cbind(overview, t(data.frame(lapply(object@objects, function(x) rownames(x@stat)[1:top]))))
  overview
})

#' @docType methods
#' @noRd
setMethod("show", "contextBundle", function(object){
  summary(object)
})
