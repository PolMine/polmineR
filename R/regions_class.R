#' @include partition_class.R
NULL


#' Regions of a CWB corpus.
#' 
#' A coerce-method is available to coerce a \code{partition} object to a
#' \code{regions} object.
#' 
#' @slot cpos a two-column \code{data.table} that will include a "cpos_left" and "cpos_right" column
#' @slot corpus the CWB corpus (character vector length 1)
#' @slot encoding the encoding of the CWB corpus (character vector length 1)
#' @param x object of class \code{regions}
#' @param values values to assign to a column that will be added
#' @exportClass regions
#' @rdname regions_class
#' @name regions
#' @examples
#' \donttest{
#' use("polmineR.sampleCorpus")
#' P <- partition("PLPRBTTXT", text_date = "2009-11-12", text_name = "Jens Spahn")
#' R <- as.regions(P)
#' encode(R, sAttribute = "text_foo", values = "Jens")
#' }
#' @aliases regions-class
setClass(
  "regions",
  representation = list(
    cpos = "matrix",
    corpus = "character",
    encoding = "character"
  )
)

setAs(from = "partition", to = "regions", function(from, to){
  new(
    "regions",
    cpos = from@cpos,
    encoding = from@encoding,
    corpus = from@corpus
    )
})

#' @rdname regions_class
#' @export as.regions
as.regions <- function(x){
  stopifnot("partition" %in% is(x))
  as(x, "regions")
}

#' @rdname regions_class
#' @exportMethod as.data.table
setMethod("as.data.table", "regions", function(x, values = NULL){
  dt <- as.data.table(x@cpos)
  if (!is.null(values)){
    stopifnot(length(values) == nrow(dt) || length(values) == 1)
    dt[[3]] <- values
  }
  dt
})