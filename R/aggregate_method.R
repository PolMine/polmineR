#' @include partition_class.R
NULL

#' The method \code{aggregate} will deflate the matrix in the slot \code{cpos},
#' i.e. it checks for each new row in the matrix whether it increments the end
#' of the previous region (by 1), and ensure that the cpos matrix defines
#' disjoined regions.
#' 
#' @exportMethod aggregate
#' @rdname partition_class
#' @examples 
#' P <- new(
#'   "partition",
#'   cpos = matrix(data = c(1:10, 20:29), ncol = 2, byrow = TRUE),
#'   stat = data.table::data.table()
#' )
#' P2 <- aggregate(P)
#' P2@cpos
setMethod("aggregate", "partition", function(x){
  if (nrow(x@cpos) == 1){
    message("NOTE: Only one region, returning the partition unchanged")
  } else {
    jumps <- x@cpos[2:nrow(x@cpos), 1] - x@cpos[1:(nrow(x@cpos) - 1), 2]
    jumpsWhere <- c(0, which(jumps > 1), nrow(x@cpos)) + 1
    rework <- lapply(
      1:(length(jumpsWhere) - 1),
      function(i) c(x@cpos[jumpsWhere[i], 1], x@cpos[jumpsWhere[i + 1] - 1, 2])
    )
    x@cpos <- do.call(rbind, rework)
  }
  x
})