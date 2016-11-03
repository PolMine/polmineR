#' @include partition_class.R
NULL

#' @exportMethod aggregate
#' @rdname partition-class
setMethod("aggregate", "partition", function(x){
  if (nrow(x@cpos) == 1){
    message("NOTE: Only one region, returning the partition unchanged")
  } else {
    jumps <- x@cpos[2:nrow(x@cpos), 1] - x@cpos[1:(nrow(x@cpos) - 1), 2]
    jumpsWhere <- c(0, which(jumps > 1), nrow(x@cpos)) + 1
    rework <- lapply(
      c(1:(length(jumpsWhere) - 1)),
      function(i){c(x@cpos[jumpsWhere[i], 1], x@cpos[jumpsWhere[i + 1] - 1, 2])}
    )
    x@cpos <- do.call(rbind, rework)
  }
  x
})