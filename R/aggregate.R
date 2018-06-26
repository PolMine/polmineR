#' @include partition.R S4classes.R
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
  if (nrow(x@cpos) == 1L){
    message("NOTE: Only one region, returning the partition unchanged")
    return(x)
  }
  jumps <- x@cpos[2L:nrow(x@cpos), 1L] - x@cpos[1L:(nrow(x@cpos) - 1L), 2L]
  jumpsWhere <- c(0L, which(jumps > 1L), nrow(x@cpos)) + 1L
  rework <- lapply(
    1L:(length(jumpsWhere) - 1L),
    function(i) c(x@cpos[jumpsWhere[i], 1L], x@cpos[jumpsWhere[i + 1L] - 1L, 2L])
  )
  x@cpos <- do.call(rbind, rework)
  x
})